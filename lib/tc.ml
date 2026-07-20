(* TODO: Refactor this whole module to replace manual match/error with Result.bind + projection helper. *)

[@@@warning "-27"]

exception TypeError of string

exception Defeq_failure of string

exception Not_well_posed of string

(* Raised when the kernel encounters a construct it does not yet support checking
   (e.g. an unimplemented reduction/computation rule). This is the "decline"
   signal: it is NOT a claim that the declaration is invalid -- it means we cannot
   verify it either way. The arena maps this to exit code 2 (declined), which is
   ignored for soundness/completeness scoring. Nothing raises it yet; it is the
   hook for deciding, decl-kind by decl-kind, what nyaya declines vs. checks. *)
exception Unsupported of string

module Logger = Nyaya_parser.Util.MakeLogger (struct
  let header = "Checker"
end)

(** Inference mode, mirroring the kernel's [infer_type_core]'s [infer_only]
    flag (`type_checker.cpp`) and nanoda_lib's [InferFlag] (`tc.rs`).

    [Check] re-verifies as it goes: every [App] argument's type is defeq-
    checked against the binder type, every [Let]/[Lam] binder type is
    checked to be a sort, etc. This is what a declaration's own type and
    value get, exactly once, at the top of [check]/[well_posed].

    [InferOnly] trusts that its input is already well-typed and only
    *computes* the type -- no defeq checks, no sort checks. Everything
    inference does in service of reduction or definitional equality (proof
    irrelevance, iota's K-check, struct-eta, unit-like defeq, projection
    typing) runs on terms the checker itself produced by reducing already-
    checked terms, so re-verifying them is pure overhead -- previously the
    dominant work multiplier, since every [isDefEq] inferred both sides in
    Check mode and every App node inside re-ran defeq on its argument. *)
type infer_flag =
  | Check
  | InferOnly

let truncate ?(max_len = 400) s =
  if String.length s <= max_len then
    s
  else
    String.sub s 0 (max_len - 3) ^ "..."

let expr_summary expr = CCFormat.asprintf "%a" Expr.pp expr |> truncate

let whnf_memo : (int, Expr.t) Hashtbl.t = Hashtbl.create 4096

(** Never-reset cache for [whnf] on closed (no free vars) terms, backed by
    [Expr.GrowArray] for the same O(1)-regardless-of-population reason
    [Expr.num_loose_bvars_memo] is. See {!whnf}'s comment for the
    measurement that justified this (92.6% cross-declaration reuse on a
    full init.export sweep). *)
let whnf_closed_memo : Expr.t Expr.GrowArray.t = Expr.GrowArray.create 65536

(** Memo table for {!whnf_core}, the delta-free weak-head-normal-form used by
    the lazy-delta loop in [isDefEq]. Separate from [whnf_memo] since the two
    functions compute different things for the same input (whnf_core leaves a
    delta-eligible [Const] head stuck; whnf unfolds it). *)
let whnf_core_memo : (int, Expr.t) Hashtbl.t = Hashtbl.create 4096

(** Memo tables for {!infer}, one per {!infer_flag}, following nanoda_lib's
    [infer_cache_check]/[infer_cache_no_check] split: a [Check] result is
    strictly stronger than an [InferOnly] one (same computed type, more
    verification), so [Check] hits satisfy both modes and its table is
    consulted first for every query; an [InferOnly] result must never
    satisfy a [Check] query, so the no-check table is only consulted in
    [InferOnly] mode.

    Keyed by [(Expr.tag expr, env_id)], not just [Expr.tag expr]: since
    {!infer_open} resolves [BoundVar]s against an explicit environment
    ([benv]) instead of eagerly substituting them away, the same raw,
    still-open subterm can in general need a different result under a
    different enclosing environment, so the hashcons tag alone is no longer
    a sound cache key. [env_id] is a fresh id minted every time a binder is
    pushed onto [benv] (see [fresh_env_id]/[infer_open]'s [Lam]/[Forall]/
    [Let] cases) -- two [env_id]s are only ever equal if they name the
    literal same push, so this can never conflate two different
    environments. [env_id = 0] is reserved for the empty (top-level) [benv],
    so ordinary closed terms -- the overwhelming majority -- get exactly the
    same [(tag, 0)] caching behavior this table always had. *)
let infer_memo_check : (int * int, Expr.t) Hashtbl.t = Hashtbl.create 4096

let infer_memo_no_check : (int * int, Expr.t) Hashtbl.t = Hashtbl.create 4096

(** Fresh ids for {!infer_memo_check}/{!infer_memo_no_check}'s environment
    component. [0] is reserved for the empty [benv] (see above); real pushes
    start from [1]. Never reset -- a stale id from an earlier declaration
    simply won't be found in the (per-declaration-reset) memo tables, so
    reuse across declarations is harmless, and there's no correctness reason
    to reset the counter itself. *)
let env_id_counter = ref 0

let fresh_env_id () =
  incr env_id_counter;
  !env_id_counter

(** Whether diagnostic counters are collected ([NYAYA_STATS=1]). When off, the
    only cost is a handful of [if stats_on then incr] guards on hot paths. See
    {!Nyaya_parser.Util.Stats} for aggregation and reporting. *)
let stats_on = Nyaya_parser.Util.Stats.on

(** Successful one-step delta unfolds (a definition's head replaced by its
    value) since the last per-declaration reset. *)
let delta_unfolds = ref 0

(** [whnf]/[infer] memo hits since the last per-declaration reset. *)
let whnf_memo_hits = ref 0

let infer_memo_hits = ref 0

module InferTrace = Nyaya_parser.Util.MakeTrace (struct
  type env = Env.t

  type input = Expr.t

  type output = Expr.t

  let env_logger env = env.Env.logger

  let kind = "i"

  let elide_ok_env = "NYAYA_INFER_TRACE_ELIDE_OK"

  let max_depth_env = "NYAYA_INFER_MAX_DEPTH"

  (* [infer_open]'s environment-threaded recursion (see its doc) makes each
     level of a deep binder chain cheap, so a legitimate deeply-nested term
     can genuinely need tens of thousands of levels, not thousands --
     measured directly: `app-lam.ndjson`'s adversarial-but-valid term needs
     ~18-20k. 25k gives headroom over that measurement while still catching
     genuinely unbounded/buggy recursion (which would run away far past
     this regardless of the exact cutoff). *)
  let default_max_depth = 25000

  let input_summary = expr_summary

  let output_summary = expr_summary
end)

module WhnfTrace = Nyaya_parser.Util.MakeTrace (struct
  type env = Env.t

  type input = Expr.t

  type output = Expr.t

  let env_logger env = env.Env.logger

  let kind = "w"

  let elide_ok_env = "NYAYA_WHNF_TRACE_ELIDE_OK"

  let max_depth_env = "NYAYA_WHNF_MAX_DEPTH"

  let default_max_depth = 2000

  let input_summary = expr_summary

  let output_summary = expr_summary
end)

module WhnfCoreTrace = Nyaya_parser.Util.MakeTrace (struct
  type env = Env.t

  type input = Expr.t

  type output = Expr.t

  let env_logger env = env.Env.logger

  let kind = "c"

  let elide_ok_env = "NYAYA_WHNF_CORE_TRACE_ELIDE_OK"

  let max_depth_env = "NYAYA_WHNF_CORE_MAX_DEPTH"

  let default_max_depth = 2000

  let input_summary = expr_summary

  let output_summary = expr_summary
end)

module DefEqTrace = Nyaya_parser.Util.MakeTrace (struct
  type env = Env.t

  type input = Expr.t * Expr.t

  type output = bool

  let env_logger env = env.Env.logger

  let kind = "d"

  let elide_ok_env = "NYAYA_DEFEQ_TRACE_ELIDE_OK"

  let max_depth_env = "NYAYA_DEFEQ_MAX_DEPTH"

  let default_max_depth = 2000

  let input_summary (lhs, rhs) =
    truncate (CCFormat.asprintf "%a =?= %a" Expr.pp lhs Expr.pp rhs)

  let output_summary = string_of_bool
end)

module Pp = struct
  let pp_check fpf (e, ty) =
    let pp_value fpf e = CCFormat.fprintf fpf "value:@ %a" Expr.pp e in
    let pp_type fpf ty = CCFormat.fprintf fpf "type :@ %a" Expr.pp ty in

    CCFormat.fprintf fpf "@[<v 0>@[check:@]@,@[<hv 2>%a@]@,@[<hv 2>%a@]@]"
      pp_value e pp_type ty

  let pp_check_name fpf (n, ty) =
    let pp_name fpf e = CCFormat.fprintf fpf "name:@ %a" Name.pp e in
    let pp_type fpf ty = CCFormat.fprintf fpf "type :@ %a" Expr.pp ty in

    CCFormat.fprintf fpf "@[<v 0>@[check:@]@,@[<hv 2>%a@]@,@[<hv 2>%a@]@]"
      pp_name n pp_type ty

  let pp_defeq fpf (lhs, rhs) =
    CCFormat.fprintf fpf
      "@[<hov 0>@[defeq:@]@;@[<hv 2>expected:@; %a@]@;@[<hv 2>actual:@; %a@]@]"
      Expr.pp lhs Expr.pp rhs

  let pp_inferring fpf expr =
    CCFormat.fprintf fpf "@[<hov 0>Now inferring @[<hov 2>%a@]@]" Expr.pp expr

  let pp_failed_inferring fpf expr =
    CCFormat.fprintf fpf "@[<v 0>failed inferring:@, @[<hov 2> %a@]@]" Expr.pp
      expr
end

module Reduce = struct
  open Expr

  let beta e =
    (* Generalized beta: peel as many leading [Lam]s off [f] as there are
       available [args] in one pass (cheap -- walks the Lam/body spine, not
       the bodies' own sizes), then substitute all of them into the
       innermost remaining body in a single traversal via
       [Expr.instantiate_many], instead of the old one-[instantiate]-per-
       argument loop (each of which re-walked the whole, only-shrinking-by-
       one-binder body). *)
    let rec count_peelable f args k =
      match node f, args with
      | Lam { body; _ }, _ :: vs -> count_peelable body vs (k + 1)
      | _, _ -> f, k
    in
    let f, args = get_apps e in
    let inner, k = count_peelable f args 0 in
    if k = 0 then
      e
    else (
      let consumed = CCList.take k args in
      let remaining = CCList.drop k args in
      mk_app (instantiate_many ~free_vars:consumed ~expr:inner ()) remaining
    )

  let delta_at_head (env : Env.t) f =
    (* One-step delta reduction of the head. *)
    match node f with
    | Const { name; uparams } ->
      let decl = Hashtbl.find env.tbl name in
      let decl_value = decl |> Decl.get_value in
      (* TODO: add a note about this in the notebook. *)
      (match decl_value with
      | Some v ->
        if stats_on then incr delta_unfolds;
        let decl_uparams = CCList.map Level.param (decl |> Decl.get_uparams) in
        Expr.subst_levels v decl_uparams uparams
      | None -> f)
    | _ -> f

  (* Convert a string literal to its constructor form, e.g. StrLit "ok" -> String.mk (List.cons ...). *)
  let string_lit_to_ctor (s : string) : Expr.t =
    let mk_name base field = Name.Str (Name.Str (Name.Anon, base), field) in
    let char_type = Expr.const (Name.Str (Name.Anon, "Char")) in
    (* [List] is universe-polymorphic ([List.{u} : Type u -> Type u]); [Char :
       Type 0] fixes [u] to [Level.Zero]. Omitting [~ups] here defaults to an
       empty uparams list, an arity mismatch against every other [List.nil]/
       [List.cons] in the kernel (which all carry one level), even though the
       pretty-printer hides zero-valued levels and makes the two look
       identical in traces. *)
    let list_nil =
      Expr.mk_app
        (Expr.const ~ups:[ Level.Zero ] (mk_name "List" "nil"))
        [ char_type ]
    in
    let list_cons_char =
      Expr.mk_app
        (Expr.const ~ups:[ Level.Zero ] (mk_name "List" "cons"))
        [ char_type ]
    in
    let char_of_nat = Expr.const (mk_name "Char" "ofNat") in
    let string_mk = Expr.const (mk_name "String" "mk") in
    (* Decode UTF-8 string to list of Unicode code points *)
    let code_points = ref [] in
    let len = String.length s in
    let i = ref 0 in
    while !i < len do
      let b0 = Char.code (String.get s !i) in
      let cp, advance =
        if b0 land 0x80 = 0 then
          b0, 1
        else if b0 land 0xe0 = 0xc0 then (
          let b1 = Char.code (String.get s (!i + 1)) in
          ((b0 land 0x1f) lsl 6) lor (b1 land 0x3f), 2
        ) else if b0 land 0xf0 = 0xe0 then (
          let b1 = Char.code (String.get s (!i + 1)) in
          let b2 = Char.code (String.get s (!i + 2)) in
          ( ((b0 land 0x0f) lsl 12) lor ((b1 land 0x3f) lsl 6) lor (b2 land 0x3f),
            3 )
        ) else (
          let b1 = Char.code (String.get s (!i + 1)) in
          let b2 = Char.code (String.get s (!i + 2)) in
          let b3 = Char.code (String.get s (!i + 3)) in
          ( ((b0 land 0x07) lsl 18)
            lor ((b1 land 0x3f) lsl 12)
            lor ((b2 land 0x3f) lsl 6)
            lor (b3 land 0x3f),
            4 )
        )
      in
      code_points := cp :: !code_points;
      i := !i + advance
    done;
    (* Build list from right to left (code_points is reversed) *)
    let char_list =
      List.fold_left
        (fun acc cp ->
          let ch = Expr.mk_app char_of_nat [ Expr.natlit (Z.of_int cp) ] in
          Expr.mk_app list_cons_char [ ch; acc ])
        list_nil !code_points
    in
    Expr.mk_app string_mk [ char_list ]

  let proj_field_at_head (env : Env.t) (proj_expr : Expr.t) whnf =
    match Expr.node proj_expr with
    | Expr.Proj { name; nat; expr = inner } ->
      let inner' = whnf env inner in
      let inner' =
        match Expr.node inner' with
        | Expr.Literal (Expr.StrLit s) -> string_lit_to_ctor s
        | _ -> inner'
      in
      let hd, args = Expr.get_apps inner' in
      (match Expr.node hd with
      | Expr.Const { name = cname; _ } ->
        let ind_decl = Hashtbl.find env.tbl name in
        let ctor_names = Decl.get_inductive_ctors ind_decl in
        if List.mem cname ctor_names then (
          let num_params = Decl.get_inductive_num_params ind_decl in
          CCList.get_at_idx (nat + num_params) args
        ) else
          None
      | _ -> None)
    | _ -> None

  (* One-step iota reduction at the head of [e]: a recursor applied to a constructor-headed major premise. *)
  let iota_at_head (env : Env.t) (e : Expr.t) whnf infer isDefEq : Expr.t =
    let module Logger = (val env.logger) in
    let hd, args = Expr.get_apps e in
    match Expr.node hd with
    | Expr.Const { name = rec_name; uparams = rec_levels } ->
      (* Look up decl for the head constant *)
      let decl =
        try Hashtbl.find env.tbl rec_name
        with Not_found -> (* unknown constant *) raise Not_found
      in
      (match decl with
      | Decl.Rec
          { num_params; num_idx; num_motives; num_minors; rules; is_K; _ } ->
        (* The rule value's universe params get substituted with the actual application's levels. *)
        let decl_uparams = CCList.map Level.param (Decl.get_uparams decl) in
        let major_idx = num_params + num_idx + num_motives + num_minors in
        if List.length args <= major_idx then
          e
        else (
          let major0 = List.nth args major_idx in
          (* K-like reduction: replace a stuck major with its type's nullary constructor, guarded by defeq. *)
          let major =
            if not is_K then
              major0
            else (
              let app_type = whnf env (infer env major0) in
              let ind_hd, ind_args = Expr.get_apps app_type in
              match Expr.node ind_hd with
              | Expr.Const { name = ind_name; uparams = ind_levels } ->
                (match Hashtbl.find_opt env.tbl ind_name with
                | Some (Decl.Inductive { ctor_names = [ ctor_name ]; _ })
                  when List.exists
                         (fun (r : Decl.Rec_rule.t) -> r.ctor_name = ctor_name)
                         rules ->
                  let params = CCList.take num_params ind_args in
                  let cnstr_app =
                    Expr.mk_app (Expr.const ~ups:ind_levels ctor_name) params
                  in
                  let new_type = infer env cnstr_app in
                  let prev = Logs.level () in
                  Logs.set_level None;
                  let deq =
                    Fun.protect
                      ~finally:(fun () -> Logs.set_level prev)
                      (fun () ->
                        try isDefEq env app_type new_type
                        with Defeq_failure _ -> false)
                  in
                  if deq then
                    cnstr_app
                  else
                    major0
                | _ -> major0)
              | _ -> major0
            )
          in
          let major_whnf = whnf env major in
          Logger.debug "iota_at_head: rec=%a major_idx=%d major_whnf=@[%a@]"
            Name.pp rec_name major_idx Expr.pp major_whnf;
          let maj_hd, maj_args = Expr.get_apps major_whnf in
          (* Structure eta: [major_whnf] isn't headed by one of this recursor's
             own constructors (it may be Const-headed by something else
             entirely, e.g. a stuck recursor on an unrelated symbolic
             argument -- not just non-Const/non-NatLit) but the recursor's
             inductive is a single-ctor, non-recursive, no-index structure, so
             [major] is definitionally [Ctor (major.0) (major.1) ...]
             regardless of its own head. Shared by both the "Const-headed but
             not one of our own ctors" case and the general "not
             ctor/NatLit-headed" case below. *)
          let try_struct_eta_reduce () =
            match rec_name with
            | Name.Str (ind_name, _) ->
              (match Hashtbl.find_opt env.tbl ind_name with
              | Some (Decl.Inductive { ctor_names; num_idx; is_recursive; _ })
                when List.length ctor_names = 1
                     && num_idx = 0 && not is_recursive ->
                let rule = List.hd rules in
                let rule_val =
                  Expr.subst_levels rule.value decl_uparams rec_levels
                in
                let prefix_len = num_params + num_motives + num_minors in
                let prefix = CCList.take prefix_len args in
                let suffix = CCList.drop (major_idx + 1) args in
                let field_args =
                  List.init rule.ctor_num_args (fun i ->
                      Expr.proj ind_name i major)
                in
                let new_args = prefix @ field_args @ suffix in
                let red = Expr.mk_app rule_val new_args in
                Logger.debug "iota (struct-eta): %a" Expr.pp red;
                Some red
              | _ -> None)
            | _ -> None
          in
          match node maj_hd with
          | Expr.Const { name = ctor_name; _ } ->
            (* Find matching reduction rule *)
            let rule_opt =
              List.find_opt
                (fun (r : Decl.Rec_rule.t) -> r.ctor_name = ctor_name)
                rules
            in
            (match rule_opt with
            | None ->
              (* Not a constructor this recursor's rules know about -- may
                 still be a stuck term of a struct-eta-eligible type (e.g. a
                 [Poly.cancel] application stuck on a symbolic list, being
                 destructured by [Prod.rec]). *)
              (match try_struct_eta_reduce () with
              | Some red -> red
              | None -> e)
            | Some rule ->
              (* Append only the constructor's own field args (not its params, already in prefix), or params get passed twice. *)
              let prefix_len = num_params + num_motives + num_minors in
              let prefix = CCList.take prefix_len args in
              (* Args after the major premise aren't part of the rule RHS; re-apply them to the result. *)
              let suffix = CCList.drop (major_idx + 1) args in
              let maj_num_args = List.length maj_args in
              if maj_num_args < rule.ctor_num_args then
                (* Malformed constructor application: don't reduce. *)
                e
              else (
                let field_args =
                  maj_args |> List.rev
                  |> CCList.take rule.ctor_num_args
                  |> List.rev
                in
                let rule_val =
                  Expr.subst_levels rule.value decl_uparams rec_levels
                in
                let new_args = prefix @ field_args @ suffix in
                let red = Expr.mk_app rule_val new_args in
                Logger.debug "iota: %a" Expr.pp red;
                red
              ))
          | Expr.Literal (Expr.NatLit n) ->
            (* NatLit major: treat as Nat.zero/Nat.succ and apply the matching rule directly. *)
            let mk_nat s = Name.Str (Name.Str (Name.Anon, "Nat"), s) in
            let ctor_name, field_args =
              if Z.equal n Z.zero then
                mk_nat "zero", []
              else
                mk_nat "succ", [ Expr.natlit (Z.pred n) ]
            in
            let rule_opt =
              List.find_opt
                (fun (r : Decl.Rec_rule.t) -> r.ctor_name = ctor_name)
                rules
            in
            (match rule_opt with
            | None -> e
            | Some rule ->
              let rule_val =
                Expr.subst_levels rule.value decl_uparams rec_levels
              in
              let prefix_len = num_params + num_motives + num_minors in
              let prefix = CCList.take prefix_len args in
              let suffix = CCList.drop (major_idx + 1) args in
              let new_args = prefix @ field_args @ suffix in
              let red = Expr.mk_app rule_val new_args in
              Logger.debug "iota (natlit): %a" Expr.pp red;
              red)
          | _ ->
            (* major isn't constructor/NatLit-headed: try structure eta,
               projecting out each field. *)
            (match try_struct_eta_reduce () with
            | Some red -> red
            | None -> e)
        )
      | Decl.Quot _ ->
        (* Quot.ind/Quot.lift computation rules. *)
        let quot_ind_name = Name.Str (Name.Str (Name.Anon, "Quot"), "ind") in
        let quot_lift_name = Name.Str (Name.Str (Name.Anon, "Quot"), "lift") in
        let quot_mk_name = Name.Str (Name.Str (Name.Anon, "Quot"), "mk") in
        let major_idx =
          if rec_name = quot_ind_name then
            Some 4
          else if rec_name = quot_lift_name then
            Some 5
          else
            None
        in
        (match major_idx with
        | None -> e
        | Some idx ->
          if List.length args <= idx then
            e
          else (
            let major = whnf env (List.nth args idx) in
            let maj_hd, maj_args = Expr.get_apps major in
            match Expr.node maj_hd with
            | Expr.Const { name; _ }
              when name = quot_mk_name && List.length maj_args = 3 ->
              let a = List.nth maj_args 2 in
              let f = List.nth args 3 in
              let suffix = CCList.drop (idx + 1) args in
              let red = Expr.mk_app f (a :: suffix) in
              Logger.debug "iota (quot): %a" Expr.pp red;
              red
            | _ -> e
          ))
      | _ -> e)
    | _ -> e

  (* Builtin reductions for Nat literal ops; returns [Some result] if a reduction fired. *)
  (* [native_only] restricts reduction to the fully-concrete case (both args
     already literal), matching the kernel's [reduce_nat] exactly -- no
     partial-iota extensions. Used by [isDefEq]'s lazy-delta loop, which wants
     the same narrow, magnitude-independent check the kernel runs inline
     (see [type_checker.cpp]'s [reduce_nat]/[reduce_bin_nat_op]); the
     partial-iota branches below are a nyaya-specific extension for whnf
     outside of defeq and stay off in that path. *)
  (* Precomputed [Name.t]/[Expr.t] constants for [nat_lit_reduce] below --
     nyaya's [Name.t] isn't hash-consed/interned (unlike [Expr.t]), so these
     can't become O(1)-comparable pointers the way nanoda_lib's [NameCache]
     is; but building each one exactly once here (rather than re-allocating
     a fresh 2-node [Name.Str] tree, or a fresh [Expr.const] hashcons
     lookup, on every single comparison inside [nat_lit_reduce], which runs
     on every App whnf) removes real, measured allocation/lookup volume from
     the hottest path in the checker with zero behavior change -- structural
     comparison against a stable value is exactly as correct as comparison
     against a freshly-built equal one. *)
  module NameCache = struct
    let mk s1 s2 = Name.Str (Name.Str (Name.Anon, s1), s2)
    let nat_succ = mk "Nat" "succ"
    let nat_pred = mk "Nat" "pred"
    let nat_add = mk "Nat" "add"
    let nat_sub = mk "Nat" "sub"
    let nat_mul = mk "Nat" "mul"
    let nat_pow = mk "Nat" "pow"
    let nat_div = mk "Nat" "div"
    let nat_mod = mk "Nat" "mod"
    let nat_beq = mk "Nat" "beq"
    let nat_ble = mk "Nat" "ble"
    let nat_land = mk "Nat" "land"
    let nat_lor = mk "Nat" "lor"
    let nat_xor = mk "Nat" "xor"
    let nat_shift_left = mk "Nat" "shiftLeft"
    let nat_shift_right = mk "Nat" "shiftRight"
    let nat_test_bit = mk "Nat" "testBit"
    let bool_true = mk "Bool" "true"
    let bool_false = mk "Bool" "false"
    let const_nat_succ = Expr.const nat_succ
    let const_nat_pred = Expr.const nat_pred
    let const_nat_add = Expr.const nat_add
    let const_nat_mul = Expr.const nat_mul
    let const_nat_ble = Expr.const nat_ble
    let expr_bool_true = Expr.const bool_true
    let expr_bool_false = Expr.const bool_false
  end

  let nat_lit_reduce ?(native_only = false) (env : Env.t) (e : Expr.t) whnf :
      Expr.t option =
    let hd, args = Expr.get_apps e in
    match Expr.node hd with
    | Expr.Const { name; _ } ->
      let open NameCache in
      (* Cheap, whnf-free test that an argument can never compute to a literal,
         used to bail out of a Nat op before forcing the other argument. *)
      let obviously_not_lit e =
        match Expr.node e with
        | Expr.FreeVar _ | Expr.Lam _ | Expr.Forall _ | Expr.Sort _ -> true
        | _ -> false
      in
      (* Recover a concrete literal, looking through [Nat.succ] chains; returns
         [None] on a symbolic argument rather than forcing it. *)
      let rec as_nat_lit e =
        if obviously_not_lit e then
          None
        else (
          let e = whnf env e in
          match Expr.node e with
          | Expr.Literal (Expr.NatLit n) -> Some n
          | Expr.App _ ->
            (match Expr.get_apps e with
            | shd, [ y ] ->
              (match Expr.node shd with
              | Expr.Const { name = sn; _ } when sn = nat_succ ->
                (match as_nat_lit y with
                | Some k -> Some (Z.succ k)
                | None -> None)
              | _ -> None)
            | _ -> None)
          | _ -> None
        )
      in
      let bool_const b =
        if b then
          expr_bool_true
        else
          expr_bool_false
      in
      let binary f =
        match args with
        (* Bail before forcing if either argument obviously isn't a literal. *)
        | [ a; b ] when obviously_not_lit a || obviously_not_lit b -> None
        | [ a; b ] ->
          (match as_nat_lit a, as_nat_lit b with
          | Some m, Some n -> Some (f m n)
          | _ -> None)
        | _ -> None
      in
      (match name with
      | n when n = nat_succ ->
        (* Fold into a literal when the argument reduces to one; [succ x] with
           symbolic [x] is still stuck through [as_nat_lit]'s cheap bail-out. *)
        (match args with
        | [ a ] ->
          (match as_nat_lit a with
          | Some n -> Some (Expr.natlit (Z.succ n))
          | _ -> None)
        | _ -> None)
      | n when n = nat_add ->
        (match binary (fun m n -> Expr.natlit (Z.add m n)) with
        | Some _ as r -> r
        | None when native_only -> None
        | None ->
          (* Partial iota for Nat.add with a symbolic/successor-headed second arg (NatLit case bounded to n <= 64). *)
          (match args with
          | [ a; b ] ->
            (match as_nat_lit b with
            | Some n when Z.leq n (Z.of_int 64) ->
              if Z.equal n Z.zero then
                Some a
              else (
                let inner = Expr.mk_app hd [ a; Expr.natlit (Z.pred n) ] in
                Some (Expr.mk_app const_nat_succ [ inner ])
              )
            | _ ->
              (* Symbolic successor: Nat.add x (Nat.succ y) -> Nat.succ (Nat.add x y). *)
              let b' = whnf env b in
              let bhd, bargs = Expr.get_apps b' in
              (match Expr.node bhd, bargs with
              | Expr.Const { name = sn; _ }, [ y ] when sn = nat_succ ->
                let inner = Expr.mk_app hd [ a; y ] in
                Some (Expr.mk_app const_nat_succ [ inner ])
              | _ -> None))
          | _ -> None))
      | n when n = nat_sub ->
        (match binary (fun m n -> Expr.natlit (Z.max (Z.sub m n) Z.zero)) with
        | Some _ as r -> r
        | None when native_only -> None
        | None ->
          (* Partial iota for Nat.sub: recurses only on the subtrahend, matching
             Nat.sub's own equations. The literal-subtrahend case is bounded to
             <= 64, like add/mul/pow's partial iota -- for a large literal
             subtrahend with a symbolic minuend this would otherwise peel one
             predecessor at a time (e.g. ~4.3e9 times for a 2^32-scale mask
             literal). Beyond the bound, decline: whnf's Nat.sub delta guard
             keeps the term stuck instead of forcing the same blowup via
             Nat.sub's own definition. *)
          let is_zero e =
            match Expr.node (whnf env e) with
            | Expr.Literal (Expr.NatLit n) -> Z.equal n Z.zero
            | _ -> false
          in
          let as_succ e =
            let e' = whnf env e in
            let shd, sargs = Expr.get_apps e' in
            match Expr.node shd, sargs with
            | Expr.Const { name = sn; _ }, [ x ] when sn = nat_succ -> Some x
            | _ ->
              (match Expr.node e' with
              | Expr.Literal (Expr.NatLit k)
                when Z.gt k Z.zero && Z.leq k (Z.of_int 64) ->
                Some (Expr.natlit (Z.pred k))
              | _ -> None)
          in
          (match args with
          | [ a; b ] ->
            if is_zero b then
              Some (whnf env a)
            else (
              match as_succ b with
              | Some m ->
                let inner = Expr.mk_app hd [ a; m ] in
                Some (Expr.mk_app const_nat_pred [ inner ])
              | None -> None
            )
          | _ -> None))
      | n when n = nat_mul ->
        (match binary (fun m n -> Expr.natlit (Z.mul m n)) with
        | Some _ as r -> r
        | None when native_only -> None
        | None ->
          (* Partial iota for Nat.mul with a symbolic/successor-headed second arg (NatLit case bounded to n <= 64). *)
          (match args with
          | [ a; b ] ->
            (match as_nat_lit b with
            | Some n when Z.leq n (Z.of_int 64) ->
              if Z.equal n Z.zero then
                Some (Expr.natlit Z.zero)
              else (
                let inner = Expr.mk_app hd [ a; Expr.natlit (Z.pred n) ] in
                Some (Expr.mk_app const_nat_add [ inner; a ])
              )
            | _ ->
              (* Symbolic successor: Nat.mul x (Nat.succ y) -> Nat.add (Nat.mul x y) x. *)
              let b' = whnf env b in
              let bhd, bargs = Expr.get_apps b' in
              (match Expr.node bhd, bargs with
              | Expr.Const { name = sn; _ }, [ y ] when sn = nat_succ ->
                let inner = Expr.mk_app hd [ a; y ] in
                Some (Expr.mk_app const_nat_add [ inner; a ])
              | _ -> None))
          | _ -> None))
      | n when n = nat_pow ->
        (match binary (fun m n -> Expr.natlit (Z.pow m (Z.to_int n))) with
        | Some _ as r -> r
        | None when native_only -> None
        | None ->
          (* Partial iota for Nat.pow: recurses only on the exponent (NatLit case bounded to n <= 64). *)
          (match args with
          | [ a; b ] ->
            (match as_nat_lit b with
            | Some n when Z.leq n (Z.of_int 64) ->
              if Z.equal n Z.zero then
                Some (Expr.natlit Z.one)
              else (
                let inner = Expr.mk_app hd [ a; Expr.natlit (Z.pred n) ] in
                Some (Expr.mk_app const_nat_mul [ inner; a ])
              )
            | _ ->
              (* Symbolic successor: Nat.pow m (Nat.succ y) -> Nat.mul (Nat.pow m y) m. *)
              let b' = whnf env b in
              let bhd, bargs = Expr.get_apps b' in
              (match Expr.node bhd, bargs with
              | Expr.Const { name = sn; _ }, [ y ] when sn = nat_succ ->
                let inner = Expr.mk_app hd [ a; y ] in
                Some (Expr.mk_app const_nat_mul [ inner; a ])
              | _ -> None))
          | _ -> None))
      | n when n = nat_div ->
        (match
           binary (fun m n ->
               if Z.equal n Z.zero then
                 Expr.natlit Z.zero
               else
                 Expr.natlit (Z.div m n))
         with
        | Some _ as r -> r
        | None when native_only -> None
        | None ->
          (* Partial iota for Nat.div: 0 with either symbolic arg zero. *)
          let is_zero e =
            match Expr.node (whnf env e) with
            | Expr.Literal (Expr.NatLit n) -> Z.equal n Z.zero
            | _ -> false
          in
          (match args with
          | [ a; _ ] when is_zero a -> Some (Expr.natlit Z.zero)
          | [ _; b ] when is_zero b -> Some (Expr.natlit Z.zero)
          | _ -> None))
      | n when n = nat_mod ->
        (match
           binary (fun m n ->
               (* Divisor 0 returns the dividend, not 0 (Nat.mod's own doc comment: "5 % 0 = 5"). *)
               if Z.equal n Z.zero then
                 Expr.natlit m
               else
                 Expr.natlit (Z.rem m n))
         with
        | Some _ as r -> r
        | None when native_only -> None
        | None ->
          (* Partial iota for Nat.mod, including the Fin-literal case (Nat.ble proves m > n). *)
          let is_zero e =
            match Expr.node (whnf env e) with
            | Expr.Literal (Expr.NatLit n) -> Z.equal n Z.zero
            | _ -> false
          in
          (match args with
          | [ a; _ ] when is_zero a -> Some (Expr.natlit Z.zero)
          | [ a; _b ] when is_zero _b -> Some (whnf env a)
          | [ a; b ] ->
            (match as_nat_lit a with
            | Some k when Z.gt k Z.zero ->
              let ble_expr = Expr.mk_app const_nat_ble [ b; a ] in
              (match Expr.node (whnf env ble_expr) with
              | Expr.Const { name = bn; _ } when bn = bool_false ->
                Some (Expr.natlit k)
              | _ -> None)
            | _ ->
              (* Succ-headed dividend: one delta step exposes the ite that this whnf's further. *)
              let a' = whnf env a in
              let ahd, aargs = Expr.get_apps a' in
              (match Expr.node ahd, aargs with
              | Expr.Const { name = sn; _ }, [ _ ] when sn = nat_succ ->
                let unfolded = delta_at_head env hd in
                if unfolded != hd then
                  Some (Expr.mk_app unfolded args)
                else
                  None
              | _ -> None))
          | _ -> None))
      | n when n = nat_beq ->
        (match binary (fun m n -> bool_const (Z.equal m n)) with
        | Some _ as r -> r
        | None when native_only -> None
        | None ->
          (* Partial iota for Nat.beq: peel matching Nat.succ off both sides, else zero-vs-succ is false. *)
          let is_zero e =
            match Expr.node (whnf env e) with
            | Expr.Literal (Expr.NatLit n) -> Z.equal n Z.zero
            | _ -> false
          in
          let as_succ e =
            let e' = whnf env e in
            let shd, sargs = Expr.get_apps e' in
            match Expr.node shd, sargs with
            | Expr.Const { name = sn; _ }, [ x ] when sn = nat_succ -> Some x
            | _ ->
              (match Expr.node e' with
              | Expr.Literal (Expr.NatLit k) when Z.gt k Z.zero ->
                Some (Expr.natlit (Z.pred k))
              | _ -> None)
          in
          (match args with
          | [ a; b ] ->
            (match as_succ a, as_succ b with
            | Some n, Some m -> Some (Expr.mk_app hd [ n; m ])
            | Some _, _ when is_zero b -> Some (bool_const false)
            | _, Some _ when is_zero a -> Some (bool_const false)
            | _ -> None)
          | _ -> None))
      | n when n = nat_ble ->
        (match binary (fun m n -> bool_const (Z.leq m n)) with
        | Some _ as r -> r
        | None when native_only -> None
        | None ->
          (* Partial iota for Nat.ble: peel matching Nat.succ off both sides, else zero-vs-succ decides it. *)
          let is_zero e =
            match Expr.node (whnf env e) with
            | Expr.Literal (Expr.NatLit n) -> Z.equal n Z.zero
            | _ -> false
          in
          let as_succ e =
            let shd, sargs = Expr.get_apps (whnf env e) in
            match Expr.node shd, sargs with
            | Expr.Const { name = sn; _ }, [ x ] when sn = nat_succ -> Some x
            | _ ->
              (match Expr.node (whnf env e) with
              | Expr.Literal (Expr.NatLit k) when Z.gt k Z.zero ->
                Some (Expr.natlit (Z.pred k))
              | _ -> None)
          in
          (match args with
          | [ a; b ] ->
            if is_zero a then
              Some (bool_const true)
            else (
              match as_succ a with
              | Some _ when is_zero b -> Some (bool_const false)
              | Some n ->
                (match as_succ b with
                | Some m -> Some (Expr.mk_app hd [ n; m ])
                | None -> None)
              | None -> None
            )
          | _ -> None))
      (* Bitwise operations: Nat.land, Nat.lor, Nat.xor *)
      | n when n = nat_land -> binary (fun m n -> Expr.natlit (Z.logand m n))
      | n when n = nat_lor -> binary (fun m n -> Expr.natlit (Z.logor m n))
      | n when n = nat_xor -> binary (fun m n -> Expr.natlit (Z.logxor m n))
      (* Bit shift operations: Nat.shiftLeft, Nat.shiftRight *)
      | n when n = nat_shift_left ->
        binary (fun m n -> Expr.natlit (Z.shift_left m (Z.to_int n)))
      | n when n = nat_shift_right ->
        binary (fun m n -> Expr.natlit (Z.shift_right m (Z.to_int n)))
      (* Bit test: Nat.testBit *)
      | n when n = nat_test_bit ->
        binary (fun m n -> bool_const (Z.testbit m (Z.to_int n)))
      | _ -> None)
    | _ -> None
end

type delta_target = {
  head: Expr.t;
  args: Expr.t list;
  name: Name.t;
  hint: Decl.hint;
}
(** A delta-unfoldable application: an app (or bare constant) whose head is a
    [Const] naming a declaration with a value, at matching universe arity.
    Mirrors the kernel's [is_delta] (`type_checker.cpp`) -- the predicate that
    gates one step of [lazy_delta_reduction]. *)

let find_delta_target (env : Env.t) (expr : Expr.t) : delta_target option =
  let head, args = Expr.get_apps expr in
  match Expr.node head with
  | Expr.Const { name; uparams } ->
    (match Hashtbl.find_opt env.tbl name with
    | Some decl
      when Option.is_some (Decl.get_value decl)
           && List.length uparams = List.length (Decl.get_uparams decl) ->
      (* Matches the kernel's [constant_info::get_hints]: only an actual
         [Definition] (nyaya's [Decl.Def]) carries its own hint; every other
         kind that can still reach here with a value (Thm, Opaque) defaults to
         Opaque, so it's never preferred over a Regular-hinted peer and is
         only unfolded once the other side has nothing left to offer. *)
      let hint =
        match decl with
        | Decl.Def { red_hint; _ } -> red_hint
        | Decl.Thm _ | Decl.Opaque _ -> Decl.Opaque
        | _ -> assert false
      in
      Some { head; args; name; hint }
    | _ -> None)
  | _ -> None

let unfold_delta_target (env : Env.t) (target : delta_target) : Expr.t =
  Expr.mk_app (Reduce.delta_at_head env target.head) target.args

(** Safety net for [isDefEq]'s lazy-delta loop: it is a plain local [let rec],
    outside the [MakeTrace] depth-tracked recursion the rest of the checker
    uses, so without this it could spin unboundedly (e.g. on two terms that
    never converge) with no diagnostic. Generous relative to the 2000 default
    elsewhere: each lazy-delta step is one bounded unfold + a cheap
    [whnf_core], far cheaper than a full [whnf] recursion level. *)
let max_lazy_delta_depth =
  match Sys.getenv_opt "NYAYA_LAZY_DELTA_MAX_DEPTH" with
  | Some s -> (try int_of_string s with _ -> 8192)
  | None -> 8192

(** Mirrors the kernel's [failed_before]/[cache_failure]
    (`type_checker.cpp`): a set of expr-tag pairs whose same-head
    arg-by-arg congruence attempt has already failed once at this exact
    pair, so the lazy-delta loop's [c == 0] branch doesn't keep re-paying
    for the same doomed comparison every time an enclosing term revisits it
    (e.g. a repeated subterm pattern across a ring-normalization proof).
    Reset per-declaration alongside the other memo tables. *)
let congruence_failure_memo : (int * int, unit) Hashtbl.t = Hashtbl.create 4096

let congruence_failure_key (t : Expr.t) (s : Expr.t) : int * int =
  let a = Expr.tag t and b = Expr.tag s in
  if a <= b then
    a, b
  else
    b, a

let failed_congruence_before (t : Expr.t) (s : Expr.t) : bool =
  Hashtbl.mem congruence_failure_memo (congruence_failure_key t s)

let cache_congruence_failure (t : Expr.t) (s : Expr.t) : unit =
  Hashtbl.replace congruence_failure_memo (congruence_failure_key t s) ()

(** A per-declaration union-find over expr hash-cons tags, caching *positive*
    [isDefEq] verdicts -- the complement of [congruence_failure_memo] above,
    which only remembers failures. Modeled directly on nanoda_lib's
    [UnionFind]/[TcCache.eq_cache] (`union_find.rs`, `tc.rs`): every successful
    [isDefEq env e1 e2] unions [tag e1] and [tag e2] (see [isDefEq] below), and
    every call first checks whether the two tags are already in the same
    class -- via path compression, transitively, so if [e1] was separately
    proven equal to some [e3] which was proven equal to [e2], this catches
    [e1 =?= e2] for free without re-deriving it through congruence/delta/eta
    again. This is a different, complementary gap from the failure cache:
    that one only avoids repeating one doomed same-head congruence attempt;
    this one avoids repeating an entire successful derivation (by whatever
    rule) for a pair -- or an equivalent pair -- that recurs elsewhere in the
    same proof term (e.g. a repeated sub-hypothesis across a well-founded-
    recursion decreasing-measure proof). Reset per-declaration alongside the
    other memo tables. *)
module Uf = struct
  type node = {
    mutable parent: int;
    mutable rank: int;
  }

  let table : (int, node) Hashtbl.t = Hashtbl.create 4096

  let reset () = Hashtbl.reset table

  let node_of tag =
    match Hashtbl.find_opt table tag with
    | Some n -> n
    | None ->
      let n = { parent = tag; rank = 0 } in
      Hashtbl.replace table tag n;
      n

  let rec find tag =
    let n = node_of tag in
    if n.parent = tag then
      tag
    else (
      let root = find n.parent in
      n.parent <- root;
      root
    )

  let union a b =
    let ra = find a and rb = find b in
    if ra <> rb then (
      let na = node_of ra and nb = node_of rb in
      if na.rank < nb.rank then
        na.parent <- rb
      else if na.rank > nb.rank then
        nb.parent <- ra
      else (
        nb.parent <- ra;
        na.rank <- na.rank + 1
      )
    )

  let check_eq a b = find a = find b
end

(** Reset every per-declaration trace and memo table, so one declaration's
    caches don't bleed into the next. (WhnfCoreTrace is deliberately not
    reset, matching the previous inline reset blocks this consolidates.)

    [Expr.num_loose_bvars_memo]/[Expr.has_free_vars_memo] are deliberately
    NOT reset here, unlike every table below: both are pure structural
    properties of a hashconsed node, so a stale entry can never be wrong,
    and both are backed by [Expr.GrowArray] (an array indexed directly by
    hash-cons tag), whose access cost is O(1) regardless of how many
    entries are populated -- unlike a [Hashtbl], which is why every other
    table here (keyed by tag the same way, but genuinely needing the reset
    below: they cache results for terms *constructed during checking* via
    [instantiate]/[whnf]/etc., most of which embed a declaration's own
    fresh free variables and become permanent garbage once that
    declaration's check finishes) can't just switch to the same trick --
    their *keys* are unbounded across a sweep, not just their storage
    mechanism. A first attempt at "cache forever" for these two used a
    plain [Hashtbl] left unreset instead of [GrowArray] and regressed a
    full [init.export] sweep by ~20% (cpu 272.6s vs 218.6s), because a
    [Hashtbl]'s lookup cost grows with population -- millions of entries
    accumulated across ~36k declarations made every lookup slower, even
    though every individual entry was correct. [GrowArray] doesn't have
    that failure mode, so it's the version that actually keeps this cache
    forever cheaply. *)
let reset_decl_caches () =
  InferTrace.reset ();
  WhnfTrace.reset ();
  DefEqTrace.reset ();
  Hashtbl.reset whnf_memo;
  Hashtbl.reset whnf_core_memo;
  Hashtbl.reset congruence_failure_memo;
  Uf.reset ();
  Hashtbl.reset infer_memo_check;
  Hashtbl.reset infer_memo_no_check

(** Match Lean's reducibility-hint priority
    (`src/kernel/declaration.cpp`'s [compare]): negative unfolds [h1]'s side,
    positive unfolds [h2]'s side, zero unfolds both. Equal-height Regular
    hints unfold both; a higher Regular height unfolds first; Opaque always
    yields to a non-Opaque peer; Abbrev always goes first. *)
let compare_reducibility_hints (h1 : Decl.hint) (h2 : Decl.hint) : int =
  match h1, h2 with
  | Decl.Reg n1, Decl.Reg n2 -> Int.compare n2 n1
  | Decl.Opaque, Decl.Opaque | Decl.Abbrev, Decl.Abbrev -> 0
  | Decl.Opaque, _ -> 1
  | _, Decl.Opaque -> -1
  | Decl.Abbrev, _ -> -1
  | _, Decl.Abbrev -> 1

(** Infer the type of the given [expr]. See {!infer_flag} for what [flag]
    controls; declaration-level entry points pass [Check], everything in
    service of reduction/defeq passes [InferOnly]. Thin wrapper over
    {!infer_open} with an empty environment -- see that function's doc for
    why the eager "instantiate a binder, then recurse" pattern was replaced
    with environment threading. *)
let rec infer (flag : infer_flag) (env : Env.t) (expr : Expr.t) : Expr.t =
  infer_open flag env Containers_pvec.empty 0 expr

(** Like {!infer}, but resolves [BoundVar]s against an explicit environment
    [benv] (a persistent vector, pushed at the end as binders open, so the
    most-recently-opened/innermost binder sits at index [length benv - 1] --
    see {!infer_open_impl}'s [BoundVar] case) instead of requiring [expr] to
    already be fully substituted. [env_id] identifies this specific [benv]
    for memoization purposes only -- see {!infer_memo_check}'s doc.

    [benv] is a {!Containers_pvec.t}, not a plain list: [BoundVar] lookups
    need O(1)-ish (O(log n) with a large branching factor) random access,
    not [List.nth]'s O(n) linear scan. On an adversarial term with binder
    depth and bound-variable indices both in the thousands (the exact shape
    this whole rewrite targets), an O(n) lookup at every occurrence
    re-introduces an O(n^2) cost by a different route than the one this
    rewrite set out to eliminate -- caught by direct measurement on
    `app-lam.ndjson` (still memory-blowing-up with a plain-list [benv], even
    after every other fix in this rewrite landed).

    This exists to fix a memory-quadratic blowup on adversarial terms with
    many nested binders whose bodies reference far-outer binders (so
    [Expr.instantiate]'s [num_loose_bvars] short-circuit never fires):
    the old [infer_impl] opened one binder at a time by calling
    [Expr.instantiate], which eagerly rebuilds the *entire* remaining
    subtree before recursing -- for [n] such binders that's O(n) rebuilds
    each doing O(remaining size) work, and (worse than just slow) every
    rebuilt copy is retained live through the recursive [infer] call stack
    while it recurses deeper, so peak live memory tracks cumulative
    substitution work. [infer_open] never rebuilds a value just to open a
    binder: [Lam]/[Forall]/[Let] push the binder's replacement onto [benv]
    and recurse directly on the raw, untouched body; [BoundVar] resolves
    against [benv] instead of assuming it can never occur.

    Invariant maintained throughout: every [benv] entry, and every value
    [infer_open] returns, is fully closed (benv-independent) by
    construction. The one thing every case must do itself is pass any *raw*
    AST field it's about to use concretely (a binder's [btype], a [Let]'s
    [value], a [Proj]'s struct expr) through {!resolve} first, since those
    can still carry loose bvars referencing already-pushed outer binders.

    Each [benv] entry is a [(replacement, ty)] pair, not just the
    replacement value: [BoundVar] resolution needs [ty] (its whole purpose
    is to feed [infer]), and storing it precomputed makes that a pure O(1)
    lookup instead of a nested [infer] call. That distinction matters
    because a nested call would add a genuine extra stack frame at *every*
    bound-variable occurrence (not merely extra work -- real recursion
    depth), on top of whatever depth the surrounding term structure already
    contributes; on deeply-nested terms that was enough on its own to trip
    {!InferTrace}'s depth-limit safety net well short of any actual runaway
    recursion. [Lam]/[Forall] get [ty] for free (it's [btype], already
    resolved before the fresh [FreeVar] is built); [Let] computes it once at
    push time via a single plain [infer] call, reused by however many
    occurrences of the bound variable [body] contains. *)
and infer_open (flag : infer_flag) (env : Env.t)
    (benv : (Expr.t * Expr.t) Containers_pvec.t) (env_id : int)
    (expr : Expr.t) : Expr.t =
  (* Normalize a closed [expr] to the canonical empty environment before
     doing anything else. Without this, a subterm that happens not to
     reference [benv] at all (the common case: most of any real term is
     closed relative to its own immediately-enclosing binders) would still
     get memoized under whatever [env_id] happened to be ambient at the
     point it was reached -- defeating cache sharing across every other
     occurrence of that same subterm anywhere else in the declaration (under
     a different, unrelated ambient environment), even though its inferred
     type can only ever be one thing. Measured directly: without this,
     `shift-cascade.ndjson` (deeply-nested lets, each level's own body
     largely closed) drove [InferTrace]'s recursion depth to 2x what the old
     eager-substitution code needed for the exact same file, tripping the
     depth-limit safety net -- not because the new recursion is inherently
     deeper, but because closed sub-computations that used to hit the
     (tag-only-keyed) cache on repeat were instead being fully re-walked
     under a fresh [env_id] every time. This also means [infer_open_impl]
     never has to [resolve] anything inside an already-closed subterm. *)
  let benv, env_id =
    if Containers_pvec.is_empty benv || Expr.num_loose_bvars expr = 0 then
      Containers_pvec.empty, 0
    else
      benv, env_id
  in
  let key = Expr.tag expr, env_id in
  match Hashtbl.find_opt infer_memo_check key with
  | Some ty ->
    if stats_on then incr infer_memo_hits;
    ty
  | None ->
    let no_check_hit =
      match flag with
      | InferOnly -> Hashtbl.find_opt infer_memo_no_check key
      | Check -> None
    in
    (match no_check_hit with
    | Some ty ->
      if stats_on then incr infer_memo_hits;
      ty
    | None ->
      let frame = InferTrace.enter env expr in
      (match infer_open_impl flag env benv env_id expr with
      | ty ->
        InferTrace.leave_success env frame ty;
        (match flag with
        | Check -> Hashtbl.replace infer_memo_check key ty
        | InferOnly -> Hashtbl.replace infer_memo_no_check key ty);
        ty
      | exception exn ->
        let backtrace = Printexc.get_raw_backtrace () in
        InferTrace.leave_failure env frame exn;
        Printexc.raise_with_backtrace exn backtrace))

(** Resolve [expr] (which may carry loose [BoundVar]s referencing entries of
    [benv]) into a fully closed term, by substituting the whole of [benv] in
    one pass via {!Expr.instantiate_many}. O(1) short-circuit (matching
    [Expr.instantiate]'s own [num_loose_bvars] check) when [expr] has no
    loose bvars at all -- the common case. [Expr.instantiate_many]'s
    [free_vars] convention is "first element fills the outermost binder",
    which is exactly [benv]'s own natural (push-at-the-end) order, so no
    reversal is needed -- just an in-order fold. Raises if a bvar escapes
    past the whole of [benv]: for a well-scoped term reachable from a
    declaration's own (closed) value/type, every loose bvar at any point
    must be bound by some ambient (already-pushed) binder, so an escape
    means malformed input, not a case to silently paper over -- matches how
    {!infer_open_impl}'s own [BoundVar] case treats it. *)
and resolve (benv : (Expr.t * Expr.t) Containers_pvec.t) (expr : Expr.t) :
    Expr.t =
  if Containers_pvec.is_empty benv || Expr.num_loose_bvars expr = 0 then
    expr
  else
    let free_vars =
      Containers_pvec.fold_rev (fun acc (v, _) -> v :: acc) [] benv
    in
    let result = Expr.instantiate_many ~free_vars ~expr () in
    if Expr.num_loose_bvars result > 0 then
      raise (TypeError "infer: bound variable escaped its scope")
    else
      result

and infer_open_impl (flag : infer_flag) (env : Env.t)
    (benv : (Expr.t * Expr.t) Containers_pvec.t) (env_id : int)
    (expr : Expr.t) : Expr.t =
  let module Logger = (val env.logger) in
  match Expr.node expr with
  | Expr.Sort u -> Expr.sort (Level.Succ u)
  | Expr.FreeVar { name; expr; info; fvarId } -> expr
  | Expr.BoundVar i ->
    (* [benv] is pushed at the end as binders open, so the innermost
       (most-recently-opened, index 0 in de Bruijn terms) binder is the
       *last* entry, at [length benv - 1]. *)
    (match Containers_pvec.get_opt benv (Containers_pvec.length benv - 1 - i) with
    | Some (_replacement, ty) -> ty
    | None ->
      Logger.err
        "@[<v 0>@[infer BoundVar: bound variable escaped its scope (index \
         %d, %d in scope):@,\
         @[<hv 2> %a@]@]@]"
        (TypeError "infer: bound variable escaped its scope") i
        (Containers_pvec.length benv) Expr.pp expr)
  | Expr.Lam { name; btype; binfo; body } ->
    (* infer Lam: Pi binder (abstract (infer (open body with binderFvar)) binderFvar). *)
    let btype = resolve benv btype in
    (* The binder-type-is-a-sort check is [Check]-only, like the kernel's
       [infer_lambda] (which only [ensure_sort]s under [!infer_only]). *)
    if flag = Check then (
      match whnf env (infer flag env btype) |> Expr.node with
      | Expr.Sort _ -> ()
      | _ ->
        Logger.err "infer Lam: binder type is not a sort: %a"
          (TypeError "infer Lam: binder type not a sort") Expr.pp expr
    );
    let binder_free_var =
      Expr.fvar name btype binfo (Nyaya_parser.Util.Uid.mk ())
    in
    let new_env_id = fresh_env_id () in
    let body_type =
      infer_open flag env
        (Containers_pvec.push benv (binder_free_var, btype))
        new_env_id body
    in
    let target_id = Expr.get_fvar_id binder_free_var in
    Expr.pi name btype binfo (Expr.abstract_fvar ~target_id ~k:0 body_type)
  | Expr.Forall { name; btype; binfo; body } ->
    (* infer Forall: imax(sortOf binder, sortOf (open body with binderFvar)). *)
    let btype = resolve benv btype in
    let l = infer_sort_of flag env btype in
    let free_var = Expr.fvar name btype binfo (Nyaya_parser.Util.Uid.mk ()) in
    let new_env_id = fresh_env_id () in
    let r =
      infer_sort_of_open flag env
        (Containers_pvec.push benv (free_var, btype))
        new_env_id body
    in
    Expr.sort (Level.IMax (l, r) |> Level.simplify)
  | Expr.Const { name; uparams } ->
    (* infer Const: the declared type with its own uparams substituted by this application's levels. *)
    let known_type : Decl.t =
      match Hashtbl.find_opt env.tbl name with
      | Some d -> d
      | None ->
        (* Referencing a constant absent from the environment is a type error,
           not a nyaya bug. Raise a [TypeError] (so the verdict is [Reject])
           rather than let a [Not_found] escape as a checker error. *)
        Logger.err "infer Const: unknown constant %a"
          (TypeError "infer Const: unknown constant") Name.pp name
    in
    let known_type_uparams =
      CCList.map Level.param (Decl.get_uparams known_type)
    in
    Expr.subst_levels (known_type |> Decl.get_type) known_type_uparams uparams
  | Expr.App (f, arg) ->
    (* infer App: whnf(infer f) must be a Pi; check binder.type =?= infer arg, instantiate body with arg. *)
    (match whnf env (infer_open flag env benv env_id f) |> Expr.node with
    | Expr.Forall { btype; body; _ } ->
      (* The argument's type must be defeq to the parameter type -- in
         [Check] mode only, matching the kernel's [infer_app] under
         [infer_only]. (Proof irrelevance lives in [isDefEq], not here: it
         never licenses skipping this check in Check mode.) *)
      if flag = Check then (
        let arg_type = infer_open flag env benv env_id arg in
        if not (isDefEq env btype arg_type) then
          if Logs.level () = Some Logs.Debug then
            Logger.err
              "@[<v 0>[infer App] defeq failed for@,\
              \ expr = %a@,\
              \ btype = %a@,\n\
              \              arg_type = %a@]"
              (Defeq_failure "infer App: btype vs arg type") Expr.pp expr
              Expr.pp btype Expr.pp arg_type
          else
            (* Formatting the message pretty-prints three full terms; this
               failure is routinely caught and recovered from (speculative
               congruence, the lazy-delta fallback), so only pay for the
               diagnostics when Debug logging is actually on. *)
            raise (Defeq_failure "infer App: btype vs arg type")
      );
      (* Non-dependent Pi (the common case, and the shape that makes the
         classic app-lam adversarial term cheap): [body] doesn't reference
         the bound var at all, so it's returned unchanged without ever
         needing to resolve [arg] -- which can itself be an arbitrarily
         large, benv-dependent value (a shared DAG argument reused across
         many App nodes) that would be wasted work to resolve if the result
         is discarded immediately anyway. Only a genuinely dependent Pi pays
         the resolve cost, same as today's unconditional cost. *)
      if Expr.num_loose_bvars body = 0 then
        body
      else
        let arg = resolve benv arg in
        Expr.instantiate ~logger:env.logger ~free_var:arg ~expr:body ()
    | e ->
      Logger.err "infer App: expected forall, got @[%a@]"
        (TypeError "infer App: whnf of fn type not a forall") Expr.pp expr)
  | Let { name; btype; value; body } ->
    (* infer Let: check binder.type is a sort and infer(val) =?= binder.type, then infer (open body with value). *)
    let btype = resolve benv btype in
    let value = resolve benv value in
    (* Computed unconditionally (not just under [flag = Check] like the
       check below): every [BoundVar] occurrence of this let-binding in
       [body] needs [value]'s type as an O(1) [benv] lookup (see
       [infer_open]'s doc), so it has to exist before we recurse. Memoized,
       so this is cheap on any occurrence beyond the first regardless. *)
    let value_ty = infer flag env value in
    (* Both checks are [Check]-only, like the kernel's [infer_let]. *)
    if flag = Check then (
      match infer flag env btype |> Expr.node with
      | Sort _ ->
        if not (isDefEq env btype value_ty) then
          if Logs.level () = Some Logs.Debug then
            Logger.err
              "@[<v 0>[infer Let] defeq failed for@,\
              \ btype = %a@,\
              \ value_type = %a@]"
              (Defeq_failure "infer Let: btype vs value type") Expr.pp btype
              Expr.pp value_ty
          else
            (* Same rationale as the infer App failure above: don't
               pretty-print full terms for a routinely-caught failure. *)
            raise (Defeq_failure "infer Let: btype vs value type")
      | _ ->
        Logger.err "infer Let: binder type is not a sort: %a"
          (TypeError "infer Let: binder type not a sort") Expr.pp expr
    );
    let new_env_id = fresh_env_id () in
    infer_open flag env
      (Containers_pvec.push benv (value, value_ty))
      new_env_id body
  | Proj { name; nat; expr } ->
    (* infer Proj: instantiate the sole constructor's type with the struct's params, then with each prior projection, up to nat. *)
    let expr = resolve benv expr in
    let struct_type = infer flag env expr |> whnf env in
    let const, ty_args = Expr.get_apps struct_type in
    (match const |> Expr.node with
    | Const { name; uparams } ->
      let inductive_info = Hashtbl.find env.tbl name in
      let ctor_names = Decl.get_inductive_ctors inductive_info in
      let ctor_num_params = Decl.get_inductive_num_params inductive_info in
      (* A projection is only valid when its type is a structure: an inductive
         with exactly one constructor. Raise a [TypeError] (verdict [Reject])
         otherwise, rather than crashing on a multi-constructor inductive. *)
      if CCList.length ctor_names <> 1 then
        Logger.err
          "infer Proj: type %a is not a single-constructor structure (%d \
           constructors)"
          (TypeError "infer Proj: type is not a single-constructor structure")
          Name.pp name (CCList.length ctor_names);
      let ctor_info = ctor_names |> CCList.hd |> Hashtbl.find env.tbl in
      let ctor_info_type = ctor_info |> Decl.get_type in
      let ctor_uparams = CCList.map Level.param (Decl.get_uparams ctor_info) in
      let ctor_type =
        ref (Expr.subst_levels ctor_info_type ctor_uparams uparams)
      in
      let ty_param_args = CCList.take ctor_num_params ty_args in
      for i = 0 to CCList.length ty_param_args - 1 do
        let for_ty = whnf env !ctor_type in
        match for_ty |> Expr.node with
        | Forall { body; _ } ->
          let ty_arg = CCList.nth ty_param_args i in
          ctor_type :=
            Expr.instantiate ~logger:env.logger ~free_var:ty_arg ~expr:body ()
        | _ ->
          Logger.err
            "infer Proj: ctor type instantiation expected Forall, got %a"
            (TypeError "infer Proj: ctor type not a forall") Expr.pp for_ty
      done;
      (* Projecting out of a proposition is only sound for proof (Prop-typed)
         fields: proof irrelevance equates all inhabitants of the structure, so
         a data projection could equate distinct data. A field is also rejected
         when an earlier depended-upon field is data. Non-Prop structures are
         unrestricted. *)
      let is_prop t =
        match Expr.node (whnf env (infer InferOnly env t)) with
        | Expr.Sort u -> Level.is_zero u
        | _ -> false
      in
      let is_prop_type = is_prop struct_type in
      (* Now, instantiate the projections *)
      for i = 0 to nat - 1 do
        let for_ty = whnf env !ctor_type in
        match for_ty |> Expr.node with
        | Forall { btype; body; _ } ->
          (* A field that a later field depends on must itself be a proof. *)
          if
            is_prop_type && Expr.num_loose_bvars body > 0 && not (is_prop btype)
          then
            Logger.err
              "infer Proj: projection out of a proposition depends on data \
               field %d"
              (TypeError
                 "infer Proj: projection out of a proposition depends on data")
              i;
          let proj_expr = Expr.proj name i expr in
          ctor_type :=
            Expr.instantiate ~logger:env.logger ~free_var:proj_expr ~expr:body
              ()
        | _ ->
          Logger.err
            "infer Proj: projection instantiation expected Forall, got %a"
            (TypeError "infer Proj: projection type not a forall") Expr.pp
            for_ty
      done;
      (* Now, the next binder's type is the projection type *)
      let final_ty = whnf env !ctor_type in
      (match final_ty |> Expr.node with
      | Forall { btype; _ } ->
        (* The projected field itself must be a proof when the structure is. *)
        if is_prop_type && not (is_prop btype) then
          Logger.err "infer Proj: data projection out of a proposition"
            (TypeError "infer Proj: data projection out of a proposition");
        btype
      | _ ->
        Logger.err "infer Proj: final type expected Forall, got %a"
          (TypeError "infer Proj: final type not a forall") Expr.pp final_ty)
    | _ ->
      Logger.err "infer Proj: expected a const, got @[%a@] for @[%a@]"
        (TypeError "infer Proj: struct type not a const") Expr.pp struct_type
        Expr.pp expr)
  | Literal lit ->
    (match lit with
    | Expr.NatLit _ -> Expr.const (Name.of_string "Nat")
    | Expr.StrLit _ -> Expr.const (Name.of_string "String"))

and infer_sort_of flag env (expr : Expr.t) =
  infer_sort_of_open flag env Containers_pvec.empty 0 expr

and infer_sort_of_open flag env (benv : (Expr.t * Expr.t) Containers_pvec.t)
    (env_id : int) (expr : Expr.t) =
  let module Logger = (val env.logger) in
  match whnf env (infer_open flag env benv env_id expr) |> Expr.node with
  | Sort lvl -> lvl
  | _ ->
    Logger.err "infer_sort_of: expr %a is not a sort"
      (TypeError "infer_sort_of: not a sort") Expr.pp
      (infer_open flag env benv env_id expr)

and whnf (env : Env.t) (expr : Expr.t) : Expr.t =
  (* Closed terms (no free vars) go through [whnf_closed_memo] instead of the
     per-declaration [whnf_memo]: a closed expr's whnf is a pure function of
     (expr, env), and env is static for the whole run, so the result is
     globally valid -- not just within the declaration that first computed
     it. Measured directly (throwaway instrumentation, since reverted):
     across a full init.export sweep, 92.6% of closed-term whnf-misses
     (7.49M of 8.09M) were re-deriving a result some earlier declaration in
     the sweep had already computed -- shared library terms like [Nat.add 2
     3] recur constantly across unrelated declarations' proofs. Safe to
     never reset for the same reason [Expr.num_loose_bvars_memo] is:
     [Expr.GrowArray]-backed, so lookup cost is O(1) regardless of
     population, avoiding the Hashtbl-population regression documented on
     [reset_decl_caches]. Terms with free vars keep using [whnf_memo]
     (reset per declaration): those routinely embed a declaration's own
     fresh free variables and are not safe to treat as globally valid. *)
  if not (Expr.has_free_vars expr) then (
    match Expr.GrowArray.get whnf_closed_memo (Expr.tag expr) with
    | Some result ->
      if stats_on then incr whnf_memo_hits;
      result
    | None ->
      let frame = WhnfTrace.enter env expr in
      (match whnf_impl env expr with
      | e ->
        WhnfTrace.leave_success env frame e;
        Expr.GrowArray.set whnf_closed_memo (Expr.tag expr) e;
        e
      | exception exn ->
        let backtrace = Printexc.get_raw_backtrace () in
        WhnfTrace.leave_failure env frame exn;
        Printexc.raise_with_backtrace exn backtrace)
  ) else (
    match Hashtbl.find_opt whnf_memo (Expr.tag expr) with
    | Some result ->
      if stats_on then incr whnf_memo_hits;
      result
    | None ->
      let frame = WhnfTrace.enter env expr in
      (match whnf_impl env expr with
      | e ->
        WhnfTrace.leave_success env frame e;
        Hashtbl.replace whnf_memo (Expr.tag expr) e;
        e
      | exception exn ->
        let backtrace = Printexc.get_raw_backtrace () in
        WhnfTrace.leave_failure env frame exn;
        Printexc.raise_with_backtrace exn backtrace)
  )

and whnf_impl (env : Env.t) (expr : Expr.t) : Expr.t =
  let module Logger = (val env.logger) in
  match expr |> Expr.node with
  | Expr.Sort u -> Expr.sort (Level.simplify u)
  | Expr.App (f, arg) ->
    let hd, args = Expr.get_apps expr in
    (* Try Nat literal builtins before delta, so e.g. Nat.add on two NatLits computes in O(1) instead of O(n) iota. *)
    (match Reduce.nat_lit_reduce env expr whnf with
    | Some r ->
      Logger.debug "whnf: nat-builtin";
      whnf env r
    | None ->
      let hd' =
        match hd |> Expr.node with
        | Expr.Const { name; _ }
          when name = Name.Str (Name.Str (Name.Anon, "Nat"), "sub") ->
          (* nat_lit_reduce just declined for this Nat.sub application (a
             large/symbolic subtrahend past its bound); delta-unfolding
             Nat.sub's own brecOn definition here would force the identical
             blowup via a different path, so stay stuck instead. Scoped to
             Nat.sub only -- guarding mod/div/modCore too breaks
             Nat.modCore_lt, which needs modCore delta-unfolded. *)
          hd
        | Expr.Const _ -> Reduce.delta_at_head env hd
        | Expr.Proj _ ->
          (match Reduce.proj_field_at_head env hd whnf with
          | Some field -> field
          | None -> whnf env hd)
        | _ -> whnf env hd
      in
      let e1 = Expr.mk_app hd' args in
      let e2 = Reduce.beta e1 in

      (* Now attempt iota at head *)
      let e3 =
        Reduce.iota_at_head env e2 whnf (infer InferOnly) isDefEq |> Reduce.beta
      in
      if e3 == expr then
        e3
      else (
        (* Log which reductions fired *)
        if hd' != hd then Logger.debug "whnf App: delta";
        if e2 != e1 then Logger.debug "whnf App: beta";
        if e3 != e2 then Logger.debug "whnf App: iota";
        whnf env e3
      ))
  | Expr.Let { name; btype; value; body } ->
    Logger.debug "whnf: zeta";
    whnf env (Expr.instantiate ~logger:env.logger ~free_var:value ~expr:body ())
  | Expr.Const { name; uparams } ->
    (* Normalize Nat.zero to NatLit 0 so isDefEq doesn't need a Const-vs-Literal case. *)
    if name = Name.Str (Name.Str (Name.Anon, "Nat"), "zero") then
      Expr.natlit Z.zero
    else (
      let e' = Reduce.delta_at_head env expr in
      if e' == expr then
        expr
      else (
        Logger.debug "whnf Const: delta";
        whnf env e'
      )
    )
  | Expr.Forall _ ->
    (* Already in whnf: the head is a Forall constructor, don't reduce under binders. *)
    expr
  | Expr.Proj { name; nat; expr = inner } ->
    let inner' = whnf env inner in
    (* String literals aren't constructor-headed; expand to String.mk first so proj-reduce can fire. *)
    let inner' =
      match Expr.node inner' with
      | Expr.Literal (Expr.StrLit s) -> Reduce.string_lit_to_ctor s
      | _ -> inner'
    in
    let hd, args = Expr.get_apps inner' in
    (match Expr.node hd with
    | Expr.Const { name = cname; _ } ->
      (* Only reduce if the head is actually a constructor of the projected type, not a stuck recursor. *)
      let ind_decl = Hashtbl.find env.tbl name in
      let ctor_names = Decl.get_inductive_ctors ind_decl in
      if List.mem cname ctor_names then (
        let num_params = Decl.get_inductive_num_params ind_decl in
        let idx = nat + num_params in
        match CCList.get_at_idx idx args with
        | Some field ->
          Logger.debug "whnf: proj-reduce (#%d)" nat;
          whnf env field
        | None -> Expr.proj name nat inner'
      ) else
        Expr.proj name nat inner'
    | _ -> Expr.proj name nat inner')
  | _ -> expr

(** Weak head normal form, but never delta-unfolding a [Const] head -- matching
    the kernel's [whnf_core] (`type_checker.cpp`). Beta, iota, zeta, projection
    and Nat-literal reduction all still fire (none of those unfold a named
    definition); only [delta_at_head] is skipped, leaving a delta-eligible
    [Const] application stuck. This is the building block [isDefEq]'s
    lazy-delta loop uses to compare terms without eagerly unfolding either
    side -- see {!isDefEq_impl}. *)
and whnf_core (env : Env.t) (expr : Expr.t) : Expr.t =
  match Hashtbl.find_opt whnf_core_memo (Expr.tag expr) with
  | Some result -> result
  | None ->
    let frame = WhnfCoreTrace.enter env expr in
    (match whnf_core_impl env expr with
    | e ->
      WhnfCoreTrace.leave_success env frame e;
      Hashtbl.replace whnf_core_memo (Expr.tag expr) e;
      e
    | exception exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      WhnfCoreTrace.leave_failure env frame exn;
      Printexc.raise_with_backtrace exn backtrace)

and whnf_core_impl (env : Env.t) (expr : Expr.t) : Expr.t =
  let module Logger = (val env.logger) in
  match expr |> Expr.node with
  | Expr.Sort u -> Expr.sort (Level.simplify u)
  | Expr.App (f, arg) ->
    let hd, args = Expr.get_apps expr in
    (match Reduce.nat_lit_reduce env expr whnf_core with
    | Some r ->
      Logger.debug "whnf_core: nat-builtin";
      whnf_core env r
    | None ->
      let hd' =
        match hd |> Expr.node with
        (* No delta, ever -- this is the one difference from whnf_impl. A
           delta-eligible Const head is left exactly as it is. *)
        | Expr.Const _ -> hd
        | Expr.Proj _ ->
          (match Reduce.proj_field_at_head env hd whnf_core with
          | Some field -> field
          | None -> whnf_core env hd)
        | _ -> whnf_core env hd
      in
      let e1 = Expr.mk_app hd' args in
      let e2 = Reduce.beta e1 in
      (* Recursor iota reduction needs its major premise fully reduced (with
         delta) to see through typeclass wrappers like [OfNat.ofNat] down to
         [Nat.zero]/[Nat.succ] -- this is not delta on the recursor's own
         head (recursors are never delta targets, they have no [value]), it's
         evaluating a strict subterm. The kernel does the same: `whnf_core`'s
         cheap_rec is false even inside `is_def_eq_core`/lazy-delta, only
         cheap_proj is true there. So [whnf], not [whnf_core], here. *)
      let e3 =
        Reduce.iota_at_head env e2 whnf (infer InferOnly) isDefEq |> Reduce.beta
      in
      if e3 == expr then
        e3
      else (
        if e2 != e1 then Logger.debug "whnf_core App: beta";
        if e3 != e2 then Logger.debug "whnf_core App: iota";
        whnf_core env e3
      ))
  | Expr.Let { name; btype; value; body } ->
    Logger.debug "whnf_core: zeta";
    whnf_core env
      (Expr.instantiate ~logger:env.logger ~free_var:value ~expr:body ())
  | Expr.Const { name; uparams = _ } ->
    (* Kept even here (matching whnf_impl): free, doesn't unfold a definition,
       and lets isDefEq's Literal/Const cases line up without a special case. *)
    if name = Name.Str (Name.Str (Name.Anon, "Nat"), "zero") then
      Expr.natlit Z.zero
    else
      expr
  | Expr.Forall _ -> expr
  | Expr.Proj { name; nat; expr = inner } ->
    let inner' = whnf_core env inner in
    let inner' =
      match Expr.node inner' with
      | Expr.Literal (Expr.StrLit s) -> Reduce.string_lit_to_ctor s
      | _ -> inner'
    in
    let hd, args = Expr.get_apps inner' in
    (match Expr.node hd with
    | Expr.Const { name = cname; _ } ->
      let ind_decl = Hashtbl.find env.tbl name in
      let ctor_names = Decl.get_inductive_ctors ind_decl in
      if List.mem cname ctor_names then (
        let num_params = Decl.get_inductive_num_params ind_decl in
        let idx = nat + num_params in
        match CCList.get_at_idx idx args with
        | Some field ->
          Logger.debug "whnf_core: proj-reduce (#%d)" nat;
          whnf_core env field
        | None -> Expr.proj name nat inner'
      ) else
        Expr.proj name nat inner'
    | _ -> Expr.proj name nat inner')
  | _ -> expr

(* TODO: audit for other wasteful whnfs before def eq check. *)
and isDefEq env e1 e2 =
  let frame = DefEqTrace.enter env (e1, e2) in
  match isDefEq_impl env e1 e2 with
  | ans ->
    DefEqTrace.leave_success env frame ans;
    if ans then Uf.union (Expr.tag e1) (Expr.tag e2);
    ans
  | exception exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    DefEqTrace.leave_failure env frame exn;
    Printexc.raise_with_backtrace exn backtrace

and isDefEq_impl env e1 e2 =
  let module Logger = (val env.logger) in
  if e1 == e2 then
    true
  else if Uf.check_eq (Expr.tag e1) (Expr.tag e2) then
    true
  else (
    (* Cheap structural verdicts needing no inference and no reduction --
       the kernel's [quick_is_def_eq] (`type_checker.cpp`): two Sorts
       compare levels; two manifest binders of the same kind compare
       domain plus instantiated bodies. Both are definitive either way.
       Running these before anything else means Sort-vs-Sort and
       binder-vs-binder comparisons -- the bulk of recursive defeq
       traffic -- never pay for inference or whnf at all. *)
    let quick_check l r : bool option =
      match Expr.node l, Expr.node r with
      | Expr.Sort u1, Expr.Sort u2 ->
        let ans = Level.(u1 === u2) in
        if not ans then
          Logger.debug "defeq: Sort level mismatch: %a vs %a" Level.pp u1
            Level.pp u2;
        Some ans
      | ( Expr.Forall { name = n; btype = s; body = a; binfo },
          Expr.Forall { btype = t; body = b; _ } )
      | ( Expr.Lam { name = n; btype = s; body = a; binfo },
          Expr.Lam { btype = t; body = b; _ } ) ->
        Some
          (isDefEq env s t
          &&
          let free_var = Expr.fvar n s binfo (Nyaya_parser.Util.Uid.mk ()) in
          isDefEq env
            (Expr.instantiate ~logger:env.logger ~free_var ~expr:a ())
            (Expr.instantiate ~logger:env.logger ~free_var ~expr:b ()))
      | _ -> None
    in
    match quick_check e1 e2 with
    | Some ans -> ans
    | None ->
      let e1_core = whnf_core env e1 in
      let e2_core = whnf_core env e2 in
      (* If whnf_core made progress on either side, retry the O(1) and
         quick verdicts on the cores before anything expensive. *)
      let quick_again =
        if e1_core != e1 || e2_core != e2 then
          if e1_core == e2_core then
            Some true
          else if Uf.check_eq (Expr.tag e1_core) (Expr.tag e2_core) then
            Some true
          else
            quick_check e1_core e2_core
        else
          None
      in
      (match quick_again with
      | Some ans -> ans
      | None ->
        (* Proof irrelevance -- after the quick checks and whnf_core,
           matching the kernel's [is_def_eq_core] order (and nanoda_lib's
           [def_eq]), not before them as nyaya used to: running it first
           meant *every* defeq call inferred both sides up front. Left-
           short-circuiting: the right side's type is never inferred unless
           the left side is actually a proof. [InferOnly] throughout --
           these are terms the checker already produced/checked, so
           re-verifying them (the old behavior, with checking inference)
           only multiplied work. *)
        let is_prop ty =
          match Expr.node (whnf env (infer InferOnly env ty)) with
          | Expr.Sort u -> Level.is_zero u
          | _ -> false
        in
        let proof_irrelevant () =
          let t1 = infer InferOnly env e1_core in
          is_prop t1
          &&
          let t2 = infer InferOnly env e2_core in
          is_prop t2 && isDefEq env t1 t2
        in
        if proof_irrelevant () then
          true
        else (
          (* Same-head-args shortcut (on the cores; whnf_core never
             delta-unfolds, so a same-Const head survives it): if both
             sides are the same Const applied to equal-arity args, compare
             args first. Placed after proof irrelevance so proof arguments
             are not deep-compared before irrelevance has had its chance. *)
          let same_head_args_shortcut () =
            let h1, args1 = Expr.get_apps e1_core in
            let h2, args2 = Expr.get_apps e2_core in
            match Expr.node h1, Expr.node h2 with
            | ( Expr.Const { name = n1; uparams = us },
                Expr.Const { name = n2; uparams = vs } )
              when args1 <> [] && n1 = n2
                   && List.length args1 = List.length args2
                   && List.length us = List.length vs
                   && CCList.fold_left2
                        (fun acc u v -> acc && Level.(u === v))
                        true us vs ->
              (* Speculative: suppress logging and treat a failed/raising comparison as "shortcut doesn't apply", not a real failure. *)
              let prev_level = Logs.level () in
              Logs.set_level None;
              Fun.protect
                ~finally:(fun () -> Logs.set_level prev_level)
                (fun () ->
                  try List.for_all2 (isDefEq env) args1 args2
                  with Defeq_failure _ -> false)
            | _ -> false
          in
          if same_head_args_shortcut () then
            true
          else (
            (* Lazy-delta reduction (mirrors `type_checker.cpp`'s
               lazy_delta_reduction): compare via whnf_core -- never delta -- and
               only delta-unfold one side at a time, the one with lower
               reducibility height (Def.red_hint), re-checking after every step.
               This is what keeps a comparison like `grind-ring-5`'s deep proof
               term or `UInt16.toUInt32_toUInt64`'s Nat.sub-on-a-2^32-mask
               shallow: most real comparisons resolve by congruence or a
               one-level Nat.succ offset long before either side is fully
               reduced, which is exactly what eagerly whnf-ing both sides (the
               old behavior below, now only the fallback once neither side is
               delta-eligible) defeats. *)
            let nat_offset_bridge t_n s_n =
              let is_nat_zero e =
                match Expr.node e with
                | Expr.Literal (Expr.NatLit n) -> Z.equal n Z.zero
                | _ -> false
              in
              (* Two concrete literals: compare the big-int values directly.
                 Without this, [as_nat_succ] below (needed to bridge a literal
                 against a symbolic [Nat.succ] chain) would peel one unit off
                 *both* sides per step via a genuine recursive [isDefEq] call --
                 each peel is cheap, but it's one more [DefEqTrace]-counted
                 stack frame, so for two large literals (e.g. two Nat.emod
                 results 2^16 apart, as in [Int16.ofBitVec_intMax]) the tens of
                 thousands of peels needed blow the depth cap long before
                 reaching a verdict, even though the total work is trivial.
                 Same root shape as the [Nat.sub]-on-a-2^32-literal fix
                 (`135fd76`), but there the fix was also a real time-complexity
                 win; here it's purely about not exhausting the recursion-depth
                 budget on a lot of individually-cheap steps. *)
              match Expr.node t_n, Expr.node s_n with
              | Expr.Literal (Expr.NatLit n), Expr.Literal (Expr.NatLit m) ->
                Some (Z.equal n m)
              | _ ->
                let as_nat_succ e =
                  match Expr.node e with
                  | Expr.Literal (Expr.NatLit n) when Z.gt n Z.zero ->
                    Some (Expr.natlit (Z.pred n))
                  | _ ->
                    (match Expr.get_apps e with
                    | shd, [ x ] ->
                      (match Expr.node shd with
                      | Expr.Const { name; _ }
                        when name
                             = Name.Str (Name.Str (Name.Anon, "Nat"), "succ") ->
                        Some x
                      | _ -> None)
                    | _ -> None)
                in
                if is_nat_zero t_n && is_nat_zero s_n then
                  Some true
                else (
                  match as_nat_succ t_n, as_nat_succ s_n with
                  | Some p, Some q -> Some (isDefEq env p q)
                  | _ -> None
                )
            in
            let rec lazy_delta depth t_n s_n =
              if depth > max_lazy_delta_depth then
                raise
                  (Nyaya_parser.Util.Depth_limit
                     (Printf.sprintf "[lazy-delta] depth %d exceeds max %d"
                        depth max_lazy_delta_depth))
              else if t_n == s_n then
                `Equal
              else (
                match nat_offset_bridge t_n s_n with
                | Some b ->
                  if b then
                    `Equal
                  else
                    `NotEqual
                | None ->
                  (match
                     Reduce.nat_lit_reduce ~native_only:true env t_n whnf_core
                   with
                  | Some t_v ->
                    if isDefEq env t_v s_n then
                      `Equal
                    else
                      `NotEqual
                  | None ->
                    (match
                       Reduce.nat_lit_reduce ~native_only:true env s_n whnf_core
                     with
                    | Some s_v ->
                      if isDefEq env t_n s_v then
                        `Equal
                      else
                        `NotEqual
                    | None ->
                      (match
                         find_delta_target env t_n, find_delta_target env s_n
                       with
                      | None, None -> `Stuck (t_n, s_n)
                      | Some dt, None ->
                        lazy_delta (depth + 1)
                          (whnf_core env (unfold_delta_target env dt))
                          s_n
                      | None, Some ds ->
                        lazy_delta (depth + 1) t_n
                          (whnf_core env (unfold_delta_target env ds))
                      | Some dt, Some ds ->
                        let c = compare_reducibility_hints dt.hint ds.hint in
                        if c < 0 then
                          lazy_delta (depth + 1)
                            (whnf_core env (unfold_delta_target env dt))
                            s_n
                        else if c > 0 then
                          lazy_delta (depth + 1) t_n
                            (whnf_core env (unfold_delta_target env ds))
                        else (
                          (* Equal delta priority: same declaration, same arity,
                             same universe instantiation, Regular hint -- try
                             arg-by-arg congruence before paying for a delta step
                             on both sides (`type_checker.cpp`'s
                             lazy_delta_reduction_step, the [c == 0] branch). *)
                          let same_head =
                            dt.name = ds.name
                            &&
                            match Expr.node dt.head, Expr.node ds.head with
                            | ( Expr.Const { uparams = us; _ },
                                Expr.Const { uparams = vs; _ } ) ->
                              List.length us = List.length vs
                              && CCList.fold_left2
                                   (fun acc u v -> acc && Level.(u === v))
                                   true us vs
                            | _ -> false
                          in
                          let congruent =
                            same_head
                            && List.length dt.args = List.length ds.args
                            && (match dt.hint with
                               | Decl.Reg _ -> true
                               | _ -> false)
                            && (not (failed_congruence_before t_n s_n))
                            &&
                            let prev = Logs.level () in
                            Logs.set_level None;
                            let ok =
                              Fun.protect
                                ~finally:(fun () -> Logs.set_level prev)
                                (fun () ->
                                  try
                                    List.for_all2 (isDefEq env) dt.args ds.args
                                  with Defeq_failure _ -> false)
                            in
                            if not ok then cache_congruence_failure t_n s_n;
                            ok
                          in
                          if congruent then
                            `Equal
                          else
                            lazy_delta (depth + 1)
                              (whnf_core env (unfold_delta_target env dt))
                              (whnf_core env (unfold_delta_target env ds))
                        ))))
              )
            in
            match lazy_delta 0 e1_core e2_core with
            | `Equal -> true
            | `NotEqual -> false
            | `Stuck (e1_lazy, e2_lazy) ->
              (* Both sides are confirmed non-delta-eligible at the head (the
                 lazy-delta loop above only stops here once [find_delta_target]
                 says so for both) -- but a Proj-headed side may still be stuck
                 behind a delta-eligible instance, e.g. [(instOfNatNat 0).1]:
                 whnf_core deliberately never unfolds [instOfNatNat] to reach the
                 constructor the projection needs. The kernel's [is_def_eq_core]
                 gives projections exactly one more chance here with full [whnf]
                 (`type_checker.cpp`, the "invoke whnf_core again, but now using
                 whnf to reduce projections" pass) before falling to
                 congruence/eta. Since the head is already known non-delta, this
                 can only do more work on a projection's structure -- bounded by
                 instance-resolution depth, not an arbitrary computation -- so it
                 doesn't reintroduce the eager-full-whnf blowup lazy-delta exists
                 to avoid. *)
              let e1' = whnf env e1_lazy in
              let e2' = whnf env e2_lazy in
              if e1' != e1_lazy || e2' != e2_lazy then
                isDefEq env e1' e2'
              else if e1' == e2' then
                true
              else (
                try
                  (* Structure eta: y is constructor-headed for a structure type T; compare Proj(i, x) against each of y's fields. *)
                  let try_struct_eta x y =
                    let hd, y_args = Expr.get_apps y in
                    match Expr.node hd with
                    | Expr.Const { name = ctor_name; _ } ->
                      (match Hashtbl.find_opt env.tbl ctor_name with
                      | Some
                          (Decl.Ctor
                            { inductive_name; num_params; num_fields; _ }) ->
                        (match Hashtbl.find_opt env.tbl inductive_name with
                        | Some
                            (Decl.Inductive
                              { ctor_names; num_idx; is_recursive; _ })
                          when List.length ctor_names = 1
                               && num_idx = 0 && (not is_recursive)
                               && List.length y_args = num_params + num_fields
                          ->
                          if
                            not
                              (isDefEq env (infer InferOnly env x)
                                 (infer InferOnly env y))
                          then
                            false
                          else (
                            Logger.debug "defeq: struct-eta for %a" Name.pp
                              inductive_name;
                            let rec check i =
                              if i >= num_fields then
                                true
                              else
                                isDefEq env
                                  (Expr.proj inductive_name i x)
                                  (CCList.nth y_args (num_params + i))
                                && check (i + 1)
                            in
                            check 0
                          )
                        | _ -> false)
                      | _ -> false)
                    | _ -> false
                  in
                  (* Unit-like defeq: any two terms of a non-recursive, single-ctor, zero-field structure type are defeq once their types agree. *)
                  let is_def_eq_unit_like x y =
                    let x_type = whnf env (infer InferOnly env x) in
                    let hd, _ = Expr.get_apps x_type in
                    match Expr.node hd with
                    | Expr.Const { name = ind_name; _ } ->
                      (match Hashtbl.find_opt env.tbl ind_name with
                      | Some
                          (Decl.Inductive
                            {
                              ctor_names = [ ctor_name ];
                              num_idx = 0;
                              is_recursive = false;
                              _;
                            }) ->
                        (match Hashtbl.find_opt env.tbl ctor_name with
                        | Some (Decl.Ctor { num_fields = 0; _ }) ->
                          isDefEq env x_type (infer InferOnly env y)
                        | _ -> false)
                      | _ -> false)
                    | _ -> false
                  in
                  (* Lambda eta: fun x => body  =?=  e   iff   body =?= e x *)
                  let try_lam_eta lam other =
                    match Expr.node lam with
                    | Expr.Lam { name; btype; body; binfo } ->
                      let fv =
                        Expr.fvar name btype binfo (Nyaya_parser.Util.Uid.mk ())
                      in
                      isDefEq env
                        (Expr.instantiate ~logger:env.logger ~free_var:fv
                           ~expr:body ())
                        (Expr.app other fv)
                    | _ -> false
                  in
                  (* Nat.xor is delta-guarded; as a last resort try one manual delta step of it. *)
                  let try_xor_one_step other side =
                    match Expr.node side with
                    | Expr.Const { name; _ }
                      when name = Name.Str (Name.Str (Name.Anon, "Nat"), "xor")
                      ->
                      let side' = Reduce.delta_at_head env side in
                      if side' == side then
                        false
                      else
                        isDefEq env side' other
                    | _ -> false
                  in
                  (* Bridge [NatLit n] against a [Nat.succ] chain: peel leading [succ]s in a flat
                     loop, counting, then match [n - count] against the base. *)
                  let try_natlit_succ lit_side other =
                    let succ_arg e =
                      match Expr.get_apps e with
                      | shd, [ y ]
                        when match Expr.node shd with
                             | Expr.Const { name; _ } ->
                               name
                               = Name.Str (Name.Str (Name.Anon, "Nat"), "succ")
                             | _ -> false ->
                        Some y
                      | _ -> None
                    in
                    match Expr.node lit_side with
                    | Expr.Literal (Expr.NatLit n) ->
                      let rec loop count cur =
                        if Z.gt count n then
                          false
                        (* more [succ]s than [n]: cannot be equal *)
                        else (
                          match succ_arg cur with
                          | Some y -> loop (Z.succ count) (whnf env y)
                          | None ->
                            isDefEq env (Expr.natlit (Z.sub n count)) cur
                        )
                      in
                      (match succ_arg other with
                      | Some _ -> loop Z.zero other
                      | None -> false)
                    | _ -> false
                  in
                  (* Shared final fallback, reached from the catch-all below and from a FreeVar/FreeVar id mismatch. *)
                  let final_fallback e1' e2' =
                    if try_struct_eta e1' e2' || try_struct_eta e2' e1' then
                      true
                    else if
                      is_def_eq_unit_like e1' e2' || is_def_eq_unit_like e2' e1'
                    then
                      true
                    else if try_lam_eta e1' e2' || try_lam_eta e2' e1' then
                      true
                    else if try_xor_one_step e1' e2' || try_xor_one_step e2' e1'
                    then
                      true
                    else if try_natlit_succ e1' e2' || try_natlit_succ e2' e1'
                    then
                      true
                    else if Logs.level () = Some Logs.Debug then
                      Logger.err
                        "@[<v 0>[isDefEq] structural mismatch@,\
                        \ lhs = %a@,\
                        \ rhs = %a@]"
                        (Defeq_failure "isDefEq: structural mismatch") Expr.pp
                        e1 Expr.pp e2
                    else
                      (* This failure is speculative more often than not (caught
                         by congruence attempts, the safety-net retry, callers'
                         recovery); formatting it pretty-prints both full terms,
                         so only pay for that when Debug logging is on. *)
                      raise (Defeq_failure "isDefEq: structural mismatch")
                  in
                  let result =
                    match Expr.node e1', Expr.node e2' with
                    | Expr.Sort u1, Expr.Sort u2 ->
                      if Level.(u1 === u2) then
                        true
                      else (
                        Logger.debug "defeq: Sort level mismatch: %a vs %a"
                          Level.pp u1 Level.pp u2;
                        false
                      )
                    | ( Expr.FreeVar { fvarId = f1; _ },
                        Expr.FreeVar { fvarId = f2; _ } ) ->
                      if f1 = f2 then
                        true
                      else (
                        Logger.debug
                          "defeq: FreeVar id mismatch, trying final fallback";
                        final_fallback e1' e2'
                      )
                    | ( Expr.Forall { name = n; btype = s; body = a; binfo },
                        Expr.Forall { btype = t; body = b; _ } ) ->
                      if isDefEq env s t then (
                        let free_var =
                          Expr.fvar n s binfo (Nyaya_parser.Util.Uid.mk ())
                        in
                        let r =
                          isDefEq env
                            (Expr.instantiate ~logger:env.logger ~free_var
                               ~expr:a ())
                            (Expr.instantiate ~logger:env.logger ~free_var
                               ~expr:b ())
                        in
                        if not r then Logger.debug "defeq: Forall body mismatch";
                        r
                      ) else (
                        Logger.debug "defeq: Forall btype mismatch";
                        false
                      )
                    | Expr.App (f, a), Expr.App (g, b) ->
                      (* Try per-argument congruence first (suppressing a nested raise); only then fall back to struct-eta on the whole terms. *)
                      let try_cong () =
                        let prev = Logs.level () in
                        Logs.set_level None;
                        Fun.protect
                          ~finally:(fun () -> Logs.set_level prev)
                          (fun () ->
                            try
                              isDefEq env
                                (Reduce.delta_at_head env f)
                                (Reduce.delta_at_head env g)
                              && isDefEq env a b
                            with Defeq_failure _ -> false)
                      in
                      if try_cong () then
                        true
                      else if try_struct_eta e1' e2' || try_struct_eta e2' e1'
                      then
                        true
                      else (
                        Logger.debug
                          "defeq: App fn/arg mismatch (and struct-eta failed)";
                        false
                      )
                    | ( Expr.Const { name = n1; uparams = us },
                        Expr.Const { name = n2; uparams = vs } ) ->
                      if
                        n1 = n2
                        && List.length us = List.length vs
                        && CCList.fold_left2
                             (fun acc u v -> acc && Level.(u === v))
                             true us vs
                      then
                        true
                      else (
                        Logger.debug "defeq: Const mismatch: %a vs %a" Name.pp
                          n1 Name.pp n2;
                        false
                      )
                    | ( Expr.Lam { name = n; btype = s; body = a; binfo },
                        Expr.Lam { btype = t; body = b; _ } ) ->
                      if isDefEq env s t then (
                        let free_var =
                          Expr.fvar n s binfo (Nyaya_parser.Util.Uid.mk ())
                        in
                        let r =
                          isDefEq env
                            (Expr.instantiate ~logger:env.logger ~free_var
                               ~expr:a ())
                            (Expr.instantiate ~logger:env.logger ~free_var
                               ~expr:b ())
                        in
                        if not r then Logger.debug "defeq: Lam body mismatch";
                        r
                      ) else (
                        Logger.debug "defeq: Lam btype mismatch";
                        false
                      )
                    | Literal (Expr.NatLit n1), Literal (Expr.NatLit n2) ->
                      if Z.equal n1 n2 then
                        true
                      else (
                        Logger.debug "defeq: NatLit mismatch: %s vs %s"
                          (Z.to_string n1) (Z.to_string n2);
                        false
                      )
                    | ( Proj { nat = n1; expr = e1; _ },
                        Proj { nat = n2; expr = e2; _ } ) ->
                      if n1 == n2 && isDefEq env e1 e2 then
                        true
                      else (
                        Logger.debug "defeq: Proj mismatch";
                        false
                      )
                    | ( Expr.Let { name = n1; btype = s1; value = v1; body = a },
                        Expr.Let { name = n2; btype = s2; value = v2; body = b }
                      ) ->
                      if isDefEq env s1 s2 && isDefEq env v1 v2 then (
                        let free_var =
                          Expr.fvar n1 s1 Expr.Default
                            (Nyaya_parser.Util.Uid.mk ())
                        in
                        let r =
                          isDefEq env
                            (Expr.instantiate ~logger:env.logger ~free_var
                               ~expr:a ())
                            (Expr.instantiate ~logger:env.logger ~free_var
                               ~expr:b ())
                        in
                        if not r then Logger.debug "defeq: Let body mismatch";
                        r
                      ) else (
                        Logger.debug "defeq: Let btype/value mismatch";
                        false
                      )
                    | _ -> final_fallback e1' e2'
                  in
                  result
                with Defeq_failure _ as exn ->
                  (* Safety net: lazy-delta's height-ordered, one-side-at-a-time
                     unfolding is not always equivalent to unfolding each side
                     independently to a full, unconditional whnf fixpoint --
                     empirically (Int.fdiv_eq_ediv in init.export), continuing a
                     recursor's major-premise resolution from a partially-lazy-
                     unfolded term can take a different path than starting fresh
                     from the original expr. Retry once against fully-whnf'd
                     originals -- this exactly reproduces the proven-correct
                     pre-lazy-delta behavior as an ultimate fallback, so it can
                     only ever rescue a case lazy-delta's strategy mishandled,
                     never mask a real inequality. If that ALSO doesn't change
                     anything, re-raise the original failure.

                     The "doesn't change anything" check must compare against
                     [e1]/[e2] (this call's own original arguments), not
                     [e1']/[e2'] (lazy-delta's already-unfolded stuck output):
                     a whnf-level guard that leaves a term stuck earlier than
                     lazy-delta's own unguarded unfolding does (e.g. [Nat.sub],
                     guarded in [whnf_impl] but not in [find_delta_target]) means
                     [whnf env e1]/[whnf env e2] can permanently differ from
                     [e1']/[e2'] on every retry without ever converging -- so
                     comparing against [e1']/[e2'] never detects "no progress"
                     and retries forever with the exact same [e1_full]/[e2_full]
                     pair (observed: [Nat.sub_succ'], `n =?= Nat.sub n m` looping
                     to the 2000-deep [DefEqTrace] cap). Comparing against the
                     true originals guarantees the retry can never be called
                     with the same two arguments this frame started with. *)
                  let e1_full = whnf env e1 in
                  let e2_full = whnf env e2 in
                  if e1_full == e1 && e2_full == e2 then
                    raise exn
                  else
                    isDefEq env e1_full e2_full
              )
          )
        ))
  )

(* Validate a constructor: its parameters must match the inductive's, each field
   must live in a universe no larger than the inductive's (unless the inductive
   is a Prop), and its result must apply the inductive to those parameters and
   its indices. Strict positivity of the inductive's occurrences is not yet
   checked -- see doc/soundness-risks.md #3. *)
let check_ctor (decl : Decl.t) (env : Env.t) =
  let module Logger = (val env.logger) in
  match decl with
  | Decl.Ctor { info = ctor_info; inductive_name; num_params; _ } ->
    let inductive = Hashtbl.find env.tbl inductive_name in
    let ind_info = Decl.get_decl_info inductive in
    let num_idx =
      match inductive with
      | Decl.Inductive { num_idx; _ } -> num_idx
      | _ -> 0
    in
    let num_nested =
      match inductive with
      | Decl.Inductive { num_nested; _ } -> num_nested
      | _ -> 0
    in
    assert (num_params = Decl.get_inductive_num_params inductive);
    (* Peel the inductive's arity to recover the shared parameter free variables
       and its resultant universe level. *)
    let param_fvars = ref [] in
    let rec peel_ind k ty =
      if k <= 0 then
        ty
      else (
        match Expr.node (whnf env ty) with
        | Expr.Forall { name; btype; binfo; body } ->
          let fv = Expr.fvar name btype binfo (Nyaya_parser.Util.Uid.mk ()) in
          if List.length !param_fvars < num_params then
            param_fvars := !param_fvars @ [ fv ];
          peel_ind (k - 1)
            (Expr.instantiate ~logger:env.logger ~free_var:fv ~expr:body ())
        | _ -> ty
      )
    in
    let ind_result = peel_ind (num_params + num_idx) ind_info.ty in
    let result_level =
      match Expr.node (whnf env ind_result) with
      | Expr.Sort l -> l
      | _ -> Level.Zero
    in
    let params_arr = Array.of_list !param_fvars in
    let param_type i =
      match Expr.node params_arr.(i) with
      | Expr.FreeVar { expr; _ } -> expr
      | _ -> assert false
    in
    (* Whether [e] mentions the inductive being defined; free variables and
       other atoms are leaves. *)
    let rec has_ind_occ (e : Expr.t) : bool =
      match Expr.node e with
      | Expr.Const { name; _ } -> name = inductive_name
      | Expr.App (f, a) -> has_ind_occ f || has_ind_occ a
      | Expr.Lam { btype; body; _ } | Expr.Forall { btype; body; _ } ->
        has_ind_occ btype || has_ind_occ body
      | Expr.Let { btype; value; body; _ } ->
        has_ind_occ btype || has_ind_occ value || has_ind_occ body
      | Expr.Proj { expr; _ } -> has_ind_occ expr
      | Expr.BoundVar _ | Expr.FreeVar _ | Expr.Sort _ | Expr.Literal _ -> false
    in
    (* Whether [t] applies the inductive (with its own universe parameters) to
       exactly the shared parameter fvars and its indices, with no index
       mentioning the inductive. Universe parameters are compared structurally by
       name, since [Name.t] is not interned. *)
    let is_valid_ind_app t =
      let head, args = Expr.get_apps t in
      let head_ok =
        match Expr.node head with
        | Expr.Const { name; uparams } ->
          name = inductive_name
          &&
          (try
             List.for_all2
               (fun lvl nm ->
                 match lvl with
                 | Level.Param p -> p = nm
                 | _ -> false)
               uparams ind_info.uparams
           with Invalid_argument _ -> false)
        | _ -> false
      in
      head_ok
      && List.length args = num_params + num_idx
      &&
      let params_args, index_args = CCList.take_drop num_params args in
      (try List.for_all2 (fun a p -> a == p) params_args !param_fvars
       with Invalid_argument _ -> false)
      && not (List.exists has_ind_occ index_args)
    in
    (* Strict positivity of a field's type: the inductive may occur only in the
       result of a function type (never to the left of an arrow) and only as a
       plain recursive application. Reduces to head-normal form but no further, so
       an occurrence that could be reduced away still counts. Only sound for
       inductives with no nested occurrences: a nested one (e.g. a field of type
       [List I]) is rejected by the final case here, but the real kernel un-nests
       it into a fresh parameter first, which nyaya does not do -- so the check is
       gated on [num_nested = 0] below. *)
    let rec check_positivity t =
      let t = whnf env t in
      if not (has_ind_occ t) then
        ()
      else (
        match Expr.node t with
        | Expr.Forall { name; btype = dom; binfo; body } ->
          if has_ind_occ dom then
            Logger.err
              "check_ctor: non-positive occurrence of %a in a field of %a"
              (TypeError "constructor has a non-positive occurrence") Name.pp
              inductive_name Name.pp ctor_info.name;
          let fv = Expr.fvar name dom binfo (Nyaya_parser.Util.Uid.mk ()) in
          check_positivity
            (Expr.instantiate ~logger:env.logger ~free_var:fv ~expr:body ())
        | _ ->
          if not (is_valid_ind_app t) then
            Logger.err "check_ctor: invalid occurrence of %a in a field of %a"
              (TypeError "constructor has an invalid recursive occurrence")
              Name.pp inductive_name Name.pp ctor_info.name
      )
    in
    (* Peel the constructor's telescope without reducing it -- the type must be
       manifest foralls, so a type that merely reduces to the inductive is still
       rejected. Parameters are def-eq-checked against the inductive's; fields
       are universe-checked. *)
    let rec loop i t =
      match Expr.node t with
      | Expr.Forall { name; btype = dom; binfo; body } ->
        let fv =
          if i < num_params then (
            if not (isDefEq env dom (param_type i)) then
              Logger.err
                "check_ctor: parameter %d of %a does not match the inductive's"
                (TypeError "constructor parameter does not match inductive")
                (i + 1) Name.pp ctor_info.name;
            params_arr.(i)
          ) else (
            (* field sort <= inductive level, or the inductive is a Prop. *)
            let s = infer_sort_of Check env dom in
            if not (Level.(s <= result_level) || Level.is_zero result_level)
            then
              Logger.err
                "check_ctor: field %d of %a has a universe too big for the \
                 inductive"
                (TypeError "constructor field universe too big") (i + 1) Name.pp
                ctor_info.name;
            if num_nested = 0 then check_positivity dom;
            Expr.fvar name dom binfo (Nyaya_parser.Util.Uid.mk ())
          )
        in
        loop (i + 1)
          (Expr.instantiate ~logger:env.logger ~free_var:fv ~expr:body ())
      | _ ->
        if is_valid_ind_app t then
          true
        else
          Logger.err "check_ctor: invalid return type for %a"
            (TypeError "constructor has an invalid return type") Name.pp
            ctor_info.name
    in
    loop 0 ctor_info.ty
  | _ -> Logger.err "Ctor check called on non-ctor declaration" (Failure "")

let check (env : Env.t) (decl : Decl.t) : bool =
  let module Logger = (val env.logger) in
  match (decl : Decl.t) with
  | Def { info; value; red_hint = _red_hint } ->
    (* TODO: definitions should be unfolded according to reducibility hints. *)
    Logger.debugf Pp.pp_check (value, info.ty);
    let inf = infer Check env value in
    Logger.app "Inference complete";
    let ans = isDefEq env inf info.ty in
    Logger.success "@[Successfully type-checked @[%a@].@]" Name.pp info.name;
    ans
  | Thm { info; value } ->
    Logger.debugf Pp.pp_check (value, info.ty);
    (* A theorem's type must be a proposition (unlike a definition's). *)
    (match Expr.node (whnf env (infer Check env info.ty)) with
    | Expr.Sort u when Level.is_zero u -> ()
    | _ ->
      Logger.err "check Thm: theorem type %a is not a proposition"
        (TypeError "theorem type is not a proposition") Expr.pp info.ty);
    let inf = infer Check env value in
    Logger.app "Inference complete";
    let ans = isDefEq env inf info.ty in
    Logger.success "@[Successfully type-checked @[%a@].@]" Name.pp info.name;
    ans
  | Axiom { name; uparams; ty } ->
    Logger.debugf Pp.pp_check_name (name, ty);
    true
  | Quot { info } ->
    (* Primitive, like Axiom: no value to check. *)
    Logger.debugf Pp.pp_check_name (info.name, info.ty);
    true
  | Opaque { info; value } ->
    Logger.debugf Pp.pp_check (value, info.ty);
    let inf = infer Check env value in
    Logger.app "Inference complete";
    let ans = isDefEq env inf info.ty in
    Logger.success "@[Successfully type-checked @[%a@].@]" Name.pp info.name;
    ans
  | Ctor { info; inductive_name; _ } as d -> check_ctor d env
  | Rec _ -> (* TODO: what goes here? *) true
  | Inductive { info; num_params; num_idx; _ } ->
    (* An inductive's type must be an arity: a Pi-telescope of exactly
       [num_params] parameters followed by [num_idx] indices, ending in a sort. *)
    let rec peel k ty =
      match Expr.node (whnf env ty) with
      | Expr.Forall { name; btype; binfo; body } when k > 0 ->
        let fv = Expr.fvar name btype binfo (Nyaya_parser.Util.Uid.mk ()) in
        peel (k - 1)
          (Expr.instantiate ~logger:env.logger ~free_var:fv ~expr:body ())
      | Expr.Sort _ when k = 0 -> true
      | _ -> false
    in
    if peel (num_params + num_idx) info.ty then
      true
    else
      Logger.err
        "check Inductive: type of %a is not an arity of %d parameter(s) + %d \
         index(es) ending in a sort"
        (TypeError "inductive type is not a valid arity") Name.pp info.name
        num_params num_idx

(* Well-posed: no duplicate uparams, no free vars in the type, and the type's type is a sort. *)
let well_posed (env : Env.t) (info : Decl.decl_info) : bool =
  let module Logger = (val env.logger) in
  let rec dup_exist = function
    | [] -> false
    | hd :: tl -> List.exists (( = ) hd) tl || dup_exist tl
  in
  let no_dup_uparams = dup_exist info.uparams |> not in
  let no_free_vars = Expr.has_free_vars info.ty |> not in
  let type_is_sort =
    try
      match infer Check env info.ty |> Expr.node with
      | Expr.Sort _ -> true
      | _ ->
        Logger.debug "info.ty : %a@." Expr.pp (infer Check env info.ty);
        false
    with TypeError origin ->
      Logger.err "well_posed: while inferring %a, type error: %s"
        (TypeError ("well_posed: " ^ origin))
        Name.pp info.name origin
  in

  no_dup_uparams && no_free_vars && type_is_sort

(* The three verdicts the Lean Kernel Arena understands, one per export file.
   [Accept]  -> exit 0: every declaration checked out.
   [Reject]  -> exit 1: at least one declaration is provably invalid.
   [Decline] -> exit 2: we hit a construct we don't support and can't judge the
                file either way (ignored by the arena for scoring).
   The [string] carries the offending declaration + reason, for diagnostics. *)
type verdict =
  | Accept
  | Reject of string
  | Decline of string

(* Produce a single arena verdict for a whole environment (one export file).

   Precedence is [Reject] > [Decline] > [Accept], and it is sound in the sense
   that matters: we never return [Accept] unless *every* declaration fully
   checked. A concretely-detected invalidity (a failed [check], a type/defeq
   error, or a malformed declaration) dominates -- it makes the file invalid
   regardless of any unsupported declarations elsewhere -- so we short-circuit
   on the first reject. An [Unsupported] declaration only downgrades an
   otherwise-clean file to [Decline]; it never turns into a false accept.

   [Stack_overflow]/[Out_of_memory] and any other unexpected exception are
   deliberately *not* caught here: they propagate so the entry point can map
   them to a checker-error exit code (not 0/1/2), matching the arena's
   "anything else = a bug in the checker" rule. *)

(** Reset the per-declaration stat counters and start its timer. Call after the
    per-declaration trace/memo reset, before checking the declaration. *)
let stats_begin_decl () =
  if stats_on then (
    delta_unfolds := 0;
    whnf_memo_hits := 0;
    infer_memo_hits := 0
  );
  Nyaya_parser.Util.Stats.begin_decl ()

(** Snapshot the just-checked declaration's counters into the stats aggregator.
    Reads the per-declaration trace counts and the local counters, so it must
    run before the next per-declaration reset. *)
let stats_end_decl name =
  Nyaya_parser.Util.Stats.end_decl ~name ~whnf_misses:(WhnfTrace.calls ())
    ~infer_misses:(InferTrace.calls ()) ~defeq_calls:(DefEqTrace.calls ())
    ~delta:!delta_unfolds ~peak_whnf:(WhnfTrace.peak_depth ())
    ~peak_infer:(InferTrace.peak_depth ())
    ~peak_defeq:(DefEqTrace.peak_depth ()) ~whnf_hit:!whnf_memo_hits ()

let check_env_verdict (env : Env.t) : verdict =
  let exception Found_reject of string in
  let declined = ref None in
  let iter = env.tbl |> Iter.of_hashtbl in
  (* A name declared more than once makes the file invalid. *)
  match env.duplicates with
  | dup :: _ ->
    Reject (CCFormat.sprintf "%a: duplicate declaration name" Name.pp dup)
  | [] ->
    (try
       Iter.iter2
         (fun n d ->
           (* Same per-declaration memo reset the discovery sweep does, so one
              declaration's traces/caches don't bleed into the next. *)
           reset_decl_caches ();
           let name = CCFormat.to_string Name.pp n in
           stats_begin_decl ();
           Fun.protect
             ~finally:(fun () -> stats_end_decl name)
             (fun () ->
               try
                 let info = Decl.get_decl_info d in
                 if not (well_posed env info) then
                   raise (Found_reject (name ^ ": declaration not well-posed"))
                 else if not (check env d) then
                   raise (Found_reject (name ^ ": type mismatch"))
               with
               | Unsupported msg ->
                 if !declined = None then declined := Some (name ^ ": " ^ msg)
               | TypeError m | Defeq_failure m | Not_well_posed m ->
                 raise (Found_reject (name ^ ": " ^ m))
               | Nyaya_parser.Util.Depth_limit m ->
                 raise (Found_reject (name ^ ": depth limit: " ^ m))))
         iter;
       Nyaya_parser.Util.Stats.report ();
       match !declined with
       | Some m -> Decline m
       | None -> Accept
     with Found_reject m ->
       Nyaya_parser.Util.Stats.report ();
       Reject m)

let typecheck (env : Env.t) =
  let iter = env.tbl |> Iter.of_hashtbl in
  let total = Hashtbl.length env.tbl in
  let success = ref 0 in
  let skipped = ref 0 in
  (* NYAYA_ONLY_DECL restricts the sweep to one named declaration, for fast iterate-fix-verify cycles. *)
  let only_decl = Sys.getenv_opt "NYAYA_ONLY_DECL" in
  (* NYAYA_SKIP_PREFIX excludes declarations by namespace prefix (comma-separated) from a discovery walk. *)
  let skip_prefixes =
    match Sys.getenv_opt "NYAYA_SKIP_PREFIX" with
    | Some s ->
      String.split_on_char ',' s |> List.map String.trim
      |> List.filter (fun s -> s <> "")
    | None -> []
  in
  let is_skipped decl_name_str =
    List.exists
      (fun prefix ->
        String.equal decl_name_str prefix
        ||
        let plen = String.length prefix in
        String.length decl_name_str > plen
        && String.equal (String.sub decl_name_str 0 (plen + 1)) (prefix ^ "."))
      skip_prefixes
  in
  (* NYAYA_SWEEP_ALL checks every declaration and reports the full pass/fail set, instead of stopping at the first failure. *)
  let sweep_all =
    match Sys.getenv_opt "NYAYA_SWEEP_ALL" with
    | Some ("1" | "true" | "TRUE") -> true
    | _ -> false
  in
  let failures = ref [] in
  Iter.iter2
    (fun n d ->
      let decl_name_str = CCFormat.to_string Name.pp n in
      if
        match only_decl with
        | Some target -> not (String.equal target decl_name_str)
        | None -> false
      then
        ()
      else if only_decl = None && is_skipped decl_name_str then (
        skipped := !skipped + 1;
        Logger.info "SKIPPED (NYAYA_SKIP_PREFIX): %s" decl_name_str
      ) else if sweep_all then (
        reset_decl_caches ();
        let record_failure () =
          failures := decl_name_str :: !failures;
          (* Stream failures as they happen, so a killed sweep still leaves a partial record. *)
          Logger.info "SWEEP_ALL_FAIL: %s" decl_name_str
        in
        stats_begin_decl ();
        Fun.protect
          ~finally:(fun () -> stats_end_decl decl_name_str)
          (fun () ->
            try
              let info = Decl.get_decl_info d in
              if not (well_posed env info) then
                record_failure ()
              else if check env d then
                success := !success + 1
              else
                record_failure ()
            with
            | (Stack_overflow | Out_of_memory) as exn -> raise exn
            | _ -> record_failure ())
      ) else
        let module DeclLogger : Env.LOGGER =
        Nyaya_parser.Util.MakeLogger (struct
          let header = CCFormat.to_string Name.pp n
        end) in
        let env = Env.with_logger env (module DeclLogger) in
        let prev_level = Logs.level () in
        (match Sys.getenv_opt "NYAYA_DECL_DEBUG" with
        | Some target when String.equal target decl_name_str ->
          Logs.set_level (Some Logs.Debug)
        | _ -> ());
        reset_decl_caches ();
        let info = Decl.get_decl_info d in
        (* Check well-posedness. *)
        let is_well_posed = well_posed env info in
        if not is_well_posed then
          DeclLogger.err "Declaration %a is not well-posed"
            (Not_well_posed "declaration not well-posed") Name.pp n
        else
          DeclLogger.success "Declaration %a is well-posed." Name.pp n;
        (* Decl is well-posed, so perform typechecking. *)
        (try
           if check env d then (
             DeclLogger.success "Type checked decl %a." Name.pp
               (Decl.get_name d);
             success := !success + 1
           ) else
             ()
         with
         | exn
         when match exn with
              | TypeError _ | Defeq_failure _ | Not_well_posed _
              | Nyaya_parser.Util.Depth_limit _ ->
                true
              | _ -> false
         ->
           let remaining = total - !success - !skipped - 1 in
           Logger.success
             "Failed after checking %d declarations (%d skipped) in \
              environment; %d declarations remaining."
             !success !skipped remaining;
           (* Auto-debug: re-run the failing declaration with debug logging to a file. *)
           let sanitized =
             String.map
               (fun c ->
                 if c = '.' || c = ' ' || c = '/' then
                   '_'
                 else
                   c)
               decl_name_str
           in
           let debug_file = "debug_" ^ sanitized ^ ".txt" in
           let oc = open_out debug_file in
           let debug_ppf = Format.formatter_of_out_channel oc in
           let saved_reporter = Logs.reporter () in
           Logs.set_reporter (DeclLogger.reporter debug_ppf);
           Logs.set_level (Some Logs.Debug);
           reset_decl_caches ();
           (try ignore (check env d) with _ -> ());
           Format.pp_print_flush debug_ppf ();
           close_out oc;
           Logs.set_reporter saved_reporter;
           Logs.set_level prev_level;
           Logger.info "Debug trace written to %s" debug_file;
           Logger.err "typecheck: failed on %a: %s"
             (TypeError ("typecheck: " ^ Printexc.to_string exn))
             Name.pp n (Printexc.to_string exn));
        Logs.set_level prev_level)
    iter;
  Nyaya_parser.Util.Stats.report ();
  if sweep_all then (
    let total_checked = !success + List.length !failures in
    Logger.info "SWEEP_ALL: %d/%d declarations passed (%d skipped)." !success
      total_checked !skipped;
    Logger.info "SWEEP_ALL: %d failing declarations:" (List.length !failures);
    List.iter
      (fun name -> Logger.info "SWEEP_ALL_FAIL: %s" name)
      (List.sort String.compare !failures)
  ) else
    Logger.success "Successfully checked %d declarations in environment."
      !success
