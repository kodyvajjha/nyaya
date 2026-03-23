(* TODO: Refactor this whole module to replace manual match/error with Result.bind + projection helper. *)

[@@@warning "-27"]

exception TypeError of Expr.t

exception Defeq_failure

exception Not_well_posed

module Logger = Nyaya_parser.Util.MakeLogger (struct
  let header = "Checker"
end)

let truncate ?(max_len = 400) s =
  if String.length s <= max_len then
    s
  else String.sub s 0 (max_len - 3) ^ "..."

let expr_summary expr = CCFormat.asprintf "%a" Expr.pp expr |> truncate

let whnf_memo : (int, Expr.t) Hashtbl.t = Hashtbl.create 4096
let infer_memo : (int, Expr.t) Hashtbl.t = Hashtbl.create 4096

module InferTrace = Nyaya_parser.Util.MakeTrace (struct
  type env = Env.t

  type input = Expr.t

  type output = Expr.t

  let env_logger env = env.Env.logger

  let kind = "i"

  let elide_ok_env = "NYAYA_INFER_TRACE_ELIDE_OK"

  let max_depth_env = "NYAYA_INFER_MAX_DEPTH"

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
    let rec aux f args =
      match node f, args with
      | Lam { body; _ }, v :: vs ->
        aux (instantiate ~free_var:v ~expr:body ()) vs
      | _, _ -> mk_app f args
    in
    let f, args = get_apps e in
    aux f args

  let delta_at_head (env : Env.t) f =
    (* One-step delta reduction of the head. *)
    match node f with
    | Const { name; uparams }  ->
      let decl = Hashtbl.find env.tbl name in
      let decl_value = decl |> Decl.get_value in
      (* TODO: add a note about this in the notebook. *)
      (match decl_value with
      | Some v ->
        let decl_uparams =
          CCList.map Level.param (decl |> Decl.get_uparams)
        in
        Expr.subst_levels v decl_uparams uparams
      | None -> f)
    | _ -> f

  (**
     One-step iota reduction at the head of [e].

     Iota reduction fires when [e] is a recursor applied to a
     constructor-headed major premise.  The general shape of a reducible
     application is:

       R  p₁…pₙ  motive  minor₁…minorₖ  (C  q₁…qₘ  f₁…fⱼ)  s₁…sₜ

     where:
       - R          is the recursor constant
       - p₁…pₙ     are the recursor parameters
       - motive     is the motive (one or more)
       - minor₁…   are the minor premises (one per constructor)
       - C q… f…   is the major premise, already whnf'd to a constructor
                   application; q… are the constructor's own type parameters
                   and f… are its fields (= the ctor_num_args trailing args)
       - s₁…sₜ     are any *extra* arguments that appear after the major
                   in the full spine (e.g. motive indices that trail the
                   major, or additional arguments to the overall type such
                   as the [List.below] proof in brecOn-based recursors)

     The iota rule value [rule.value] is a closed term lambda-bound over
     (params, motives, minors, ctor_fields), i.e. it expects exactly
     [prefix @ field_args].  The extra suffix args [s₁…sₜ] are NOT part
     of the rule RHS; they must be passed on to the result:

       result = rule.value  p₁…pₙ  motive  minor₁…  f₁…fⱼ  s₁…sₜ

     Omitting [suffix] was the original bug: specialised recursors such as
     [List.lengthTRAux.match_1] take a [List.below] proof after the major
     premise.  Without [suffix], that proof was silently dropped, leaving
     the minor result partially applied (e.g. [fun (_ : List.below …) => x]
     instead of [x]).
  *)
  let iota_at_head (env : Env.t) (e : Expr.t) whnf : Expr.t =
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
      | Decl.Rec { num_params; num_idx; num_motives; num_minors; rules; _ } ->
        (* The rule value is a template with the recursor's own universe
           param names.  Substitute them with the actual universe levels
           from the const application. *)
        let decl_uparams =
          CCList.map Level.param (Decl.get_uparams decl)
        in
        let major_idx = num_params + num_idx + num_motives + num_minors in
        if List.length args <= major_idx then
          e
        else (
          let major = List.nth args major_idx in
          let major_whnf = whnf env major in
          Logger.debug "iota_at_head: rec=%a major_idx=%d major_whnf=@[%a@]"
            Name.pp rec_name major_idx Expr.pp major_whnf;
          let maj_hd, maj_args = Expr.get_apps major_whnf in
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
              (* Not a constructor the recursor knows about *)
              e
            | Some rule ->
              (**
                 The major premise is constructor-headed at this point, so
                 [maj_args] contains *all* arguments of that constructor
                 application — including constructor parameters (e.g. the
                 implicit type parameter [α] in [MyList.nil α]).

                 But the recursor's [prefix] already contains the recursor
                 parameters/indices/motives/minors, which themselves include
                 those constructor parameters.

                 If we append all of [maj_args], params are passed twice and we
                 over-apply the rule RHS (example bug: [MyList.nil α α]), which
                 later shows up as confusing defeq failures.

                 So for iota reduction we must append only the constructor
                 *field* arguments (the final [rule.ctor_num_args] arguments),
                 not all constructor arguments.
              *)
              (* The rule value expects: params, motives, minors, then
                 ctor fields.  Indices sit between minors and major in
                 the recursor spine but are NOT passed to the rule — they
                 are determined by the constructor. *)
              let prefix_len = num_params + num_motives + num_minors in
              let prefix = CCList.take prefix_len args in
              (* Args in the spine that come after the major premise.
                 These are not part of the rule RHS and must be re-applied
                 to the result.  See the docstring on [iota_at_head] for
                 why this matters. *)
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
            (* NatLit iota: the major premise is a Nat literal, not a
               constructor application.  Convert to constructor form and
               apply the matching rule directly.
                 NatLit 0   → Nat.zero rule, field_args = []
                 NatLit n+1 → Nat.succ rule, field_args = [NatLit (n-1)]
               We apply the rule directly rather than re-calling
               iota_at_head to avoid looping with the Nat.succ kernel
               builtin (which would whnf Nat.succ(NatLit n) back to
               NatLit(n+1)). *)
            let mk_nat s = Name.Str (Name.Str (Name.Anon, "Nat"), s) in
            let ctor_name, field_args =
              if Z.equal n Z.zero then (mk_nat "zero", [])
              else (mk_nat "succ", [Expr.natlit (Z.pred n)])
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
            (* major isn't constructor-headed *)
            e
        )
      | _ -> e)
    | _ -> e

  (** Kernel builtin reductions for Nat literals.

      When the head constant is a known Nat builtin and the relevant
      arguments (after whnf) are NatLit values, compute the result
      directly.  Returns [Some result] if a reduction fired, [None]
      otherwise.

      Unary:  Nat.succ
      Binary arithmetic: Nat.add, Nat.sub, Nat.mul, Nat.pow, Nat.div, Nat.mod
      Binary comparisons: Nat.beq, Nat.ble  (produce Bool constructors)

      Reference: "Type Checking in Lean 4", §3.5 (Literals). *)
  let nat_lit_reduce (env : Env.t) (e : Expr.t) whnf : Expr.t option =
    let hd, args = Expr.get_apps e in
    match Expr.node hd with
    | Expr.Const { name; _ } ->
      let mk_name s1 s2 = Name.Str (Name.Str (Name.Anon, s1), s2) in
      let as_nat_lit e =
        match Expr.node (whnf env e) with
        | Expr.Literal (Expr.NatLit n) -> Some n
        | _ -> None
      in
      let bool_const b =
        Expr.const (mk_name "Bool" (if b then "true" else "false"))
      in
      let unary f =
        match args with
        | [a] -> (match as_nat_lit a with Some n -> Some (f n) | None -> None)
        | _ -> None
      in
      let binary f =
        match args with
        | [a; b] ->
          (match as_nat_lit a, as_nat_lit b with
          | Some m, Some n -> Some (f m n)
          | _ -> None)
        | _ -> None
      in
      (match name with
      | n when n = mk_name "Nat" "succ" ->
        unary (fun n -> Expr.natlit (Z.succ n))
      | n when n = mk_name "Nat" "add" ->
        binary (fun m n -> Expr.natlit (Z.add m n))
      | n when n = mk_name "Nat" "sub" ->
        binary (fun m n -> Expr.natlit (Z.max (Z.sub m n) Z.zero))
      | n when n = mk_name "Nat" "mul" ->
        binary (fun m n -> Expr.natlit (Z.mul m n))
      | n when n = mk_name "Nat" "pow" ->
        binary (fun m n -> Expr.natlit (Z.pow m (Z.to_int n)))
      | n when n = mk_name "Nat" "div" ->
        binary (fun m n ->
          if Z.equal n Z.zero then Expr.natlit Z.zero
          else Expr.natlit (Z.div m n))
      | n when n = mk_name "Nat" "mod" ->
        binary (fun m n ->
          if Z.equal n Z.zero then Expr.natlit Z.zero
          else Expr.natlit (Z.rem m n))
      | n when n = mk_name "Nat" "beq" ->
        binary (fun m n -> bool_const (Z.equal m n))
      | n when n = mk_name "Nat" "ble" ->
        binary (fun m n -> bool_const (Z.leq m n))
      | _ -> None)
    | _ -> None

  (** Returns [true] when [name] is a known Nat kernel builtin.
      Used to prevent delta-unfolding of builtins when [nat_lit_reduce]
      could not fire (e.g. one argument is symbolic). *)
  let is_nat_builtin_name (name : Name.t) : bool =
    let mk_name s = Name.Str (Name.Str (Name.Anon, "Nat"), s) in
    name = mk_name "succ" || name = mk_name "add"
    || name = mk_name "sub" || name = mk_name "mul"
    || name = mk_name "pow" || name = mk_name "div"
    || name = mk_name "mod" || name = mk_name "beq"
    || name = mk_name "ble"

  let is_nat_builtin (e : Expr.t) : bool =
    let hd, _args = Expr.get_apps e in
    match Expr.node hd with
    | Expr.Const { name; _ } -> is_nat_builtin_name name
    | _ -> false

end

(** Infer the type of the given [expr].*)
let rec infer (env : Env.t) (expr : Expr.t) : Expr.t =
  match Hashtbl.find_opt infer_memo (Expr.tag expr) with
  | Some ty -> ty
  | None ->
    let frame = InferTrace.enter env expr in
    (match infer_impl env expr with
    | ty ->
      InferTrace.leave_success env frame ty;
      Hashtbl.replace infer_memo (Expr.tag expr) ty;
      ty
    | exception exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      InferTrace.leave_failure env frame exn;
      Printexc.raise_with_backtrace exn backtrace)

and infer_impl (env : Env.t) (expr : Expr.t) : Expr.t =
  let module Logger = (val env.logger) in
  match Expr.node expr with
  | Expr.Sort u -> Expr.sort (Level.Succ u)
  | Expr.FreeVar { name; expr; info; fvarId } ->
    expr
  | Expr.Lam { name; btype; binfo; body } ->
    (*
      infer Lambda(binder, body):
       assert! infersAsSort(binder.type)
       let binderFvar := fvar(binder)
       let bodyType := infer $ instantiate(body, binderFVar)
       Pi binder (abstract bodyType binderFVar)
    *)
    (match whnf env (infer env btype) |> Expr.node with
    | Expr.Sort _ ->
      let binder_free_var =
        Expr.fvar name btype binfo (Nyaya_parser.Util.Uid.mk ())
      in
      let body_type =
        infer env
          (Expr.instantiate ~logger:env.logger ~free_var:binder_free_var
             ~expr:body ())
      in
      let target_id = Expr.get_fvar_id binder_free_var in
      Expr.pi name btype binfo (Expr.abstract_fvar ~target_id ~k:0 body_type)
    | _ ->
      Logger.err "binder type is not a sort: %a" (TypeError expr) Expr.pp expr)
  | Expr.Forall { name; btype; binfo; body } ->
    (*
      infer Pi binder body:
      let l := inferSortOf binder
      let r := inferSortOf $ instantiate body (fvar(binder))
      imax(l, r)
    *)
    let l = infer_sort_of env btype in
    let free_var =
      Expr.fvar name btype binfo (Nyaya_parser.Util.Uid.mk ())
    in
    let r =
      infer_sort_of env
        (Expr.instantiate ~logger:env.logger ~free_var ~expr:body ())
    in
    Expr.sort (Level.IMax (l, r) |> Level.simplify)
  | Expr.Const { name; uparams } ->
    (*
       infer Const name levels:
       let knownType := environment[name].type
       substituteLevels (e := knownType) (ks := knownType.uparams) (vs := levels)
    *)
    let known_type : Decl.t = Hashtbl.find env.tbl name in
    let known_type_uparams =
      CCList.map Level.param (Decl.get_uparams known_type)
    in
    Expr.subst_levels (known_type |> Decl.get_type) known_type_uparams uparams
  | Expr.App (f, arg) ->
    (*
    infer App(f, arg):
      match (whnf $ infer f) with
      | Pi binder body => 
        assert! defEq(binder.type, infer arg)
        instantiate(body, arg)
      | _ => error
    *)
    (match whnf env (infer env f) |> Expr.node with
    | Expr.Forall { btype; body; _ } ->
      let arg_type = infer env arg in
      if not (isDefEq env btype arg_type) then
        Logger.err
          "@[Defeq check failed in expr = %a between @,\
           btype = %a and@,\
          \ inferred arg type = %a.@]" (Failure "failed 1") Expr.pp expr Expr.pp
          btype Expr.pp arg_type;
      Expr.instantiate ~logger:env.logger ~free_var:arg ~expr:body ()
    | e ->
      Logger.err "Failed infer at app, got @[%a@] instead of a forall"
        (TypeError f) Expr.pp expr)
  | Let { name; btype; value; body }  ->
    (*
       infer Let binder val body:
       assert! inferSortOf binder
       assert! defEq(infer(val), binder.type)
       infer (instantiate body val)
    *)
    (match infer env btype |> Expr.node with
    | Sort _ ->
      if not (isDefEq env btype (infer env value)) then
        Logger.err "@[btype = %a @. arg = %a@]" (Failure "failed 2") Expr.pp
          btype Expr.pp (infer env value);
      infer env
        (Expr.instantiate ~logger:env.logger ~free_var:value ~expr:body ())
    | _ -> Logger.err "binder type is not a sort: %a" (TypeError expr) Expr.pp expr)
  | Proj { name; nat; expr } ->
    (*
       let structType := whnf (infer structure)
       let (const structTyName levels) tyArgs := structType.unfoldApps
       let InductiveInfo := env[structTyName]
       -- This inductive should only have the one constructor since it's claiming to be a structure.
       let ConstructorInfo := env[InductiveInfo.constructorNames[0]]

       let mut constructorType := substLevels ConstructorInfo.type (newLevels := levels)

       for tyArg in tyArgs.take constructorType.numParams
         match (whnf constructorType) with
           | pi _ body => inst body tyArg
           | _ => error

       for i in [0:projIdx]
         match (whnf constructorType) with
           | pi _ body => inst body (proj i structure)
           | _ => error

       match (whnf constructorType) with
         | pi binder _=> binder.type
         | _ => error
    *)
    let struct_type = infer env expr |> whnf env in
    let const, ty_args = Expr.get_apps struct_type in
    (match const |> Expr.node with
    | Const { name; uparams } ->
      let inductive_info = Hashtbl.find env.tbl name in
      let ctor_names = Decl.get_inductive_ctors inductive_info in
      let ctor_num_params = Decl.get_inductive_num_params inductive_info in
      (* This inductive should only have the one constructor since it's claiming to be a structure. *)
      assert (CCList.length ctor_names = 1);
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
            "Constructor type instantiation failed: expected Forall type but \
             got %a"
            (TypeError for_ty) Expr.pp for_ty
      done;
      (* Now, instantiate the projections *)
      for i = 0 to nat - 1 do
        let for_ty = whnf env !ctor_type in
        match for_ty |> Expr.node with
        | Forall { body; _ } ->
          let proj_expr = Expr.proj name i expr  in
          ctor_type :=
            Expr.instantiate ~logger:env.logger ~free_var:proj_expr ~expr:body
              ()
        | _ ->
          Logger.err
            "Projection type instantiation failed: expected Forall type but \
             got %a"
            (TypeError for_ty) Expr.pp for_ty
      done;
      (* Now, the next binder's type is the projection type *)
      let final_ty = whnf env !ctor_type in
      (match final_ty |> Expr.node with
      | Forall { btype; _ } -> btype
      | _ ->
        Logger.err "Final type error: expected Forall type but got %a"
          (TypeError final_ty) Expr.pp final_ty)
    | _ ->
      Logger.err
        "@[While inferring @[%a@] expected a const, got @[%a@] instead@]"
        (TypeError expr) Expr.pp struct_type Expr.pp expr)
  | Literal lit ->
    (match lit with
    | Expr.NatLit _ -> Expr.const (Name.of_string "Nat")
    | Expr.StrLit _ -> Expr.const (Name.of_string "String"))
  | BoundVar _ ->
    (* Since we are using the locally nameless approach, we should not run into
       bound variables during type inference, because all open binders will be
       instantiated with the appropriate free variables. *)
    Logger.err
      "@[<v 0>@[Fatal error: encountered bound variable during inference:@,\
       @[<hv 2> %a@]@]@]" (TypeError expr) Expr.pp expr

and infer_sort_of env (expr : Expr.t) =
  let module Logger = (val env.logger) in
  match whnf env (infer env expr) |> Expr.node with
  | Sort lvl -> lvl
  | _ ->
    Logger.err "infer_sort_of: expr %a is not a sort" (TypeError expr) Expr.pp
      (infer env expr)
      
and whnf (env : Env.t) (expr : Expr.t) : Expr.t =
  match Hashtbl.find_opt whnf_memo (Expr.tag expr) with
  | Some result -> result
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

and whnf_impl (env : Env.t) (expr : Expr.t) : Expr.t =
  let module Logger = (val env.logger) in
  match expr |> Expr.node with
  | Expr.Sort u -> Expr.sort (Level.simplify u)
  | Expr.App (f, arg) ->
    let hd, args = Expr.get_apps expr in
    (* Try kernel numeric builtins BEFORE delta, so that e.g. Nat.add
       on two NatLit args computes in O(1) instead of O(n) iota steps. *)
    (match Reduce.nat_lit_reduce env expr whnf with
    | Some r -> whnf env r
    | None ->
    (* If the head is a known Nat kernel builtin but nat_lit_reduce couldn't
       fire (e.g. a symbolic argument), do NOT delta-unfold — the expression
       is already in WHNF.  Unfolding would expose an O(n) Nat.brecOn/Nat.rec
       reduction on huge literals. *)
    if Reduce.is_nat_builtin expr then expr
    else
    let hd' =
      match hd |> Expr.node with
      | Expr.Const _ -> Reduce.delta_at_head env hd
      | _ -> whnf env hd
    in
    let e1 = Expr.mk_app hd' args in
    let e2 = Reduce.beta e1 in

    (* Now attempt iota at head *)
    let e3 = Reduce.iota_at_head env e2 whnf |> Reduce.beta in
    if e3 == expr then
      e3
    else
      whnf env e3)
  | Expr.Let { name; btype; value; body } ->
    (* Zeta reduction*)
    Expr.instantiate ~logger:env.logger ~free_var:value ~expr:body ()
  | Expr.Const { name; uparams }  ->
    (* Don't delta-unfold Nat kernel builtins — they may later be applied
       to huge literals, and their Nat.brecOn definitions would loop. *)
    if Reduce.is_nat_builtin_name name then expr
    else
      let e' = Reduce.delta_at_head env expr in
      if e' == expr then expr
      else whnf env e'
  | Expr.Forall _ ->
    (* Already in whnf: the head is a Forall constructor, don't reduce under binders. *)
    expr
  | Expr.Proj {name; nat; expr = inner} ->
      let inner' = whnf env inner in
      let (hd, args) = Expr.get_apps inner' in
      (match Expr.node hd with
       | Expr.Const _ ->
         let num_params = Decl.get_inductive_num_params (Hashtbl.find env.tbl name) in
         let idx = nat + num_params in
         (match CCList.get_at_idx idx args with
          | Some field -> whnf env field
          | None -> Expr.proj name nat inner')
       | _ -> Expr.proj name nat inner')
  | _ -> expr

(* TODO: optimize def eq checking by implementing union-find.
   TODO: ensure no other wasteful whnfs show up elsewhere before def eq check
*)
and isDefEq env e1 e2 =
  let frame = DefEqTrace.enter env (e1, e2) in
  match isDefEq_impl env e1 e2 with
  | ans ->
    DefEqTrace.leave_success env frame ans;
    ans
  | exception exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    DefEqTrace.leave_failure env frame exn;
    Printexc.raise_with_backtrace exn backtrace

and isDefEq_impl env e1 e2 =
  let module Logger = (val env.logger) in
  if e1 == e2 then true
  else
  let e1' = whnf env e1 in
  let e2' = whnf env e2 in
  if e1' == e2' then true
  else
  (* Proof irrelevance: if p and q both inhabit Props S and T,
     and S is definitionally equal to T, then p =?= q.
       infer(p) = S, infer(q) = T,
       infer(S) = Sort 0, infer(T) = Sort 0, defEq(S, T) *)
  let is_prop ty =
    match Expr.node (whnf env (infer env ty)) with
    | Expr.Sort u -> Level.is_zero u
    | _ -> false
  in
  let s = infer env e1' in
  let t = infer env e2' in
  if is_prop s && is_prop t && isDefEq env s t then true
  else (
  match Expr.node e1', Expr.node e2' with
  | Expr.Sort u1, Expr.Sort u2 -> Level.(u1 === u2)
  | Expr.FreeVar { fvarId = f1; _ }, Expr.FreeVar { fvarId = f2; _ } -> f1 = f2
  | ( Expr.Forall { name = n; btype = s; body = a; binfo },
      Expr.Forall { btype = t; body = b; _ } ) ->
    if isDefEq env s t then (
      let free_var =
        Expr.fvar n s binfo (Nyaya_parser.Util.Uid.mk ())
      in
      isDefEq env
        (Expr.instantiate ~logger:env.logger ~free_var ~expr:a ())
        (Expr.instantiate ~logger:env.logger ~free_var ~expr:b ())
    ) else
      false
  | Expr.App (f, a), Expr.App (g, b) ->
    isDefEq env (Reduce.delta_at_head env f) (Reduce.delta_at_head env g)
    && isDefEq env a b
  | ( Expr.Const { name = n1; uparams = us },
      Expr.Const { name = n2; uparams = vs } ) ->
    (* We test for structural equality of names here and not pointer equality
       since there are names which are not pointer equal (e.g., we infer Nat
       literals as Const("Nat",[]))
    *)
    n1 = n2
    && CCList.fold_left2 (fun acc u v -> acc && Level.(u === v)) true us vs
  | ( Expr.Lam { name = n; btype = s; body = a; binfo },
      Expr.Lam { btype = t; body = b; _ } ) ->
    if isDefEq env s t then (
      let free_var =
        Expr.fvar n s binfo (Nyaya_parser.Util.Uid.mk ())
      in
      isDefEq env
        (Expr.instantiate ~logger:env.logger ~free_var ~expr:a ())
        (Expr.instantiate ~logger:env.logger ~free_var ~expr:b ())
    ) else
      false
  | Literal (Expr.NatLit n1), Literal (Expr.NatLit n2) -> Z.equal n1 n2
  | Proj { nat = n1; expr = e1; _ }, Proj { nat = n2; expr = e2; _ } ->
    n1 == n2 && isDefEq env e1 e2
  | ( Expr.Let { name = n1; btype = s1; value = v1; body = a },
      Expr.Let { name = n2; btype = s2; value = v2; body = b } ) ->
    if isDefEq env s1 s2 && isDefEq env v1 v2 then (
      let free_var =
        Expr.fvar n1 s1 Expr.Default (Nyaya_parser.Util.Uid.mk ())
      in
      isDefEq env
        (Expr.instantiate ~logger:env.logger ~free_var ~expr:a ())
        (Expr.instantiate ~logger:env.logger ~free_var ~expr:b ())
    ) else
      false
  | _ ->
    Logger.err "failed def eq: %a =?= %a" Defeq_failure Expr.pp e1 Expr.pp e2
  )

(* TODO: complete ctor checks. *)
let check_ctor (decl : Decl.t) (env : Env.t) =
  match decl with
  | Decl.Ctor { num_params; inductive_name; _ } ->
    let inductive = Hashtbl.find env.tbl inductive_name in
    assert (num_params = Decl.get_inductive_num_params inductive);
    (* The constructor's type/telescope has to share the same parameters as the
       type of the inductive being declared. *)
    let ensure_same_params = true in
    (* For the non-parameter elements of the constructor type's telescope, the
       binder type must actually be a type (must infer as Sort _). *)
    let non_param_as_sort = true in
    (* For any non-parameter element of the constructor type's telescope, the
       element's inferred sort must be less than or equal to the inductive type's
       sort, or the inductive type being declared has to be a prop. *)
    let sort_le_inductive_sort = true in
    (* No argument to the constructor may contain a non-positive occurrence of
       the type being declared *)
    let non_positive = true in
    (* The end of the constructor's telescope must be a valid application of
       arguments to the type being declared *)
    let end_of_telescope_match = true in
    ensure_same_params && non_param_as_sort && sort_le_inductive_sort
    && non_positive && end_of_telescope_match
  | _ -> Logger.err "Ctor check called on non-ctor declaration" (Failure "")

let check (env : Env.t) (decl : Decl.t) : bool =
  let module Logger = (val env.logger) in
  match (decl : Decl.t) with
  | Def { info; value; red_hint = _red_hint } ->
    (* TODO: definitions should be unfolded according to reducibility hints. *)
    Logger.debugf Pp.pp_check (value, info.ty);
    let inf = (infer env value) in
    Logger.app "Inference complete";
    let ans = isDefEq env inf info.ty in
    Logger.success "@[Successfully type-checked @[%a@].@]" Name.pp info.name;
    ans
  | Thm { info; value } ->
    Logger.debugf Pp.pp_check (value, info.ty);
    let inf = (infer env value) in
    Logger.app "Inference complete";
    let ans = isDefEq env (inf) info.ty in
    Logger.success "@[Successfully type-checked @[%a@].@]" Name.pp info.name;
    ans
  | Axiom { name; uparams; ty } ->
    Logger.debugf Pp.pp_check_name (name, ty);
    true
  | Ctor { info; inductive_name; _ } as d -> check_ctor d env
  | Rec _ -> (* TODO: what goes here? *) true
  | Inductive _ -> (* TODO: what goes here? *) true
  | _ ->
    (* Logger.warn "not checking decl: %a" Decl.pp decl;
       true *)
    Logger.err "failed checking decl: %a" (Failure "type checking failed")
      Decl.pp decl

(** We check if any declaration in the environment has 1) duplicate uparams or 2) lingering free variables in the type or 3) the type of its type is a sort. 
  If yes, we call that declaration well-posed and only typecheck those. *)
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
      match infer env info.ty |> Expr.node with
      | Expr.Sort _ -> true
      | _ ->
        Logger.debug "info.ty : %a@." Expr.pp (infer env info.ty);
        false
    with TypeError e ->
      Logger.err "While inferring %a could not infer type: %a" (TypeError e)
        Name.pp info.name Pp.pp_failed_inferring e
  in

  no_dup_uparams && no_free_vars && type_is_sort

let typecheck (env : Env.t) =
  let iter = env.tbl |> Iter.of_hashtbl in
  let success = ref 0 in
  Iter.iter2
    (fun n d ->
      let module DeclLogger : Env.LOGGER = Nyaya_parser.Util.MakeLogger (struct
        let header = CCFormat.to_string Name.pp n
      end) in
      let env = Env.with_logger env (module DeclLogger) in
      let decl_name_str = CCFormat.to_string Name.pp n in
      let prev_level = Logs.level () in
      (match Sys.getenv_opt "NYAYA_DECL_DEBUG" with
      | Some target when String.equal target decl_name_str ->
        Logs.set_level (Some Logs.Debug)
      | _ -> ());
      InferTrace.reset ();
      WhnfTrace.reset ();
      DefEqTrace.reset ();
      Hashtbl.reset whnf_memo;
      Hashtbl.reset infer_memo;
      let info = Decl.get_decl_info d in
      (* Check well-posedness. *)
      let is_well_posed = well_posed env info in
      if not is_well_posed then
        DeclLogger.err "Declaration %a is not well-posed" Not_well_posed Name.pp
          n
      else
        DeclLogger.success "Declaration %a is well-posed." Name.pp n;
      (* Decl is well-posed, so perform typechecking. *)
      (try
        if check env d then
          (DeclLogger.success "Type checked decl %a." Name.pp (Decl.get_name d);
          success := !success + 1)
        else
          ()
      with TypeError e ->
        Logger.success "Failed after checking %d declarations in environment."
          !success;
        Logger.err "Type checking failed when checking %a." (TypeError e)
          Name.pp n);
      Logs.set_level prev_level)
    iter;
  Logger.success "Successfully checked %d declarations in environment." !success
