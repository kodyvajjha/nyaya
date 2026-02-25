(* TODO: Expressions need to store some data inline or cache it somewhere to prevent prohibitively expensive recomputation. Additionally, we would like to limit retention overhead by using the Ephemeron module to garbage collect away unnecessary exprs after they've been used. *)

open Nyaya_parser

module Logger = Util.MakeLogger (struct
  let header = "Expr"
end)

module Fmt = CCFormat

exception InstantiateError

type binfo =
  | Default
  | Implicit
  | InstanceImplicit
  | StrictImplicit
[@@deriving show]

let binfo_of_ast (info : Ast.info) =
  match info with
  | Ast.IBD -> Default (* () *)
  | Ast.IBI -> Implicit (* {} *)
  | Ast.IBS -> StrictImplicit (* {{}}*)
  | Ast.IBC -> InstanceImplicit (* [] *)

type literal =
  | NatLit of Z.t [@printer Z.pp_print]
  | StrLit of string
[@@deriving show]

type t = expr Hc.hash_consed

and expr =
  | BoundVar of int
  | FreeVar of {
      name: Name.t;
      expr: t;
      info: binfo;
      fvarId: int; (* de Bruijn levels *)
    }
  | Const of {
      name: Name.t;
      uparams: Level.t list;
    }
  (* Sort 0 | Sort 1 | ... | Sort  u
     Prop   | Type   | ... | Type (u+1) *)
  | Sort of Level.t
  | App of t * t
  (*
    binderName      body
      |            |
fun (foo : Bar) => 0 
            |         
        binderType    
  *)
  | Lam of {
      name: Name.t;
      btype: t;
      (* binderInfo is reflected by the style of brackets used
         to surround the binder*)
      binfo: binfo;
      body: t;
    }
  | Forall of {
      name: Name.t;
      btype: t;
      binfo: binfo;
      body: t;
    }
      (** 
      binderName   val
      |            |
let (foo : Bar) := 0; foo
            |          |
        binderType     .... body
  *)
  | Let of {
      name: Name.t;
      btype: t;
      value: t;
      body: t;
    }
  (* Structure projections. *)
  | Proj of {
      name: Name.t;
      nat: int;
      expr: t;
    }
  | Literal of literal

let node (e : t) = e.node

let tag (e : t) = e.tag

module E = struct
  type t = expr

  let equal_hc (x : t Hc.hash_consed) (y : t Hc.hash_consed) = x == y

  let hash_level (l : Level.t) = Hashtbl.hash (Level.simplify l)

  let hash_uparams (uparams : Level.t list) =
    Hashtbl.hash (CCList.map Level.simplify uparams)

  let equal x y =
    match (x, y : expr * expr) with
    | BoundVar i, BoundVar j -> i = j
    | FreeVar fx, FreeVar fy ->
      fx.fvarId = fy.fvarId && fx.name = fy.name && fx.info = fy.info
      && equal_hc fx.expr fy.expr
    | Const cx, Const cy ->
      cx.name = cy.name && CCList.equal Level.eq cx.uparams cy.uparams
    | Sort lx, Sort ly -> Level.eq lx ly
    | App (f1, a1), App (f2, a2) -> equal_hc f1 f2 && equal_hc a1 a2
    | Lam l1, Lam l2 ->
      l1.name = l2.name && l1.binfo = l2.binfo && equal_hc l1.btype l2.btype
      && equal_hc l1.body l2.body
    | Forall f1, Forall f2 ->
      f1.name = f2.name && f1.binfo = f2.binfo && equal_hc f1.btype f2.btype
      && equal_hc f1.body f2.body
    | Let z1, Let z2 ->
      z1.name = z2.name && equal_hc z1.btype z2.btype
      && equal_hc z1.value z2.value && equal_hc z1.body z2.body
    | Proj p1, Proj p2 ->
      p1.name = p2.name && p1.nat = p2.nat && equal_hc p1.expr p2.expr
    | Literal (NatLit n1), Literal (NatLit n2) -> Z.equal n1 n2
    | Literal (StrLit s1), Literal (StrLit s2) -> String.equal s1 s2
    | _, _ -> false

  let hash = function
    | BoundVar i -> Hashtbl.hash (0, i)
    | FreeVar { name; expr; info; fvarId } ->
      Hashtbl.hash (1, name, tag expr, info, fvarId)
    | Const { name; uparams } -> Hashtbl.hash (2, name, hash_uparams uparams)
    | Sort l -> Hashtbl.hash (3, hash_level l)
    | App (f, a) -> Hashtbl.hash (4, tag f, tag a)
    | Lam { name; btype; binfo; body } ->
      Hashtbl.hash (5, name, tag btype, binfo, tag body)
    | Forall { name; btype; binfo; body } ->
      Hashtbl.hash (6, name, tag btype, binfo, tag body)
    | Let { name; btype; value; body } ->
      Hashtbl.hash (7, name, tag btype, tag value, tag body)
    | Proj { name; nat; expr } -> Hashtbl.hash (8, name, nat, tag expr)
    | Literal (NatLit n) -> Hashtbl.hash (9, Z.to_bits n)
    | Literal (StrLit s) -> Hashtbl.hash (10, s)
end

module HExpr = Hc.Make (E)

let sort l = HExpr.hashcons (Sort l)

let lambda name btype binfo body =
  HExpr.hashcons (Lam { name; btype; binfo; body })

let app f e = HExpr.hashcons (App (f, e))

(** Transform [(f, [x0; x1; ...; xN])] into [App(App(...App(f, x0), x1), ... , xN)]. Note: this is the inverse of [get_apps] above.*)
let mk_app f args = CCList.fold_left (fun acc a -> app acc a) f args

let pi name btype binfo body =
  HExpr.hashcons (Forall { name; btype; binfo; body })

let letin name btype value body =
  HExpr.hashcons (Let { name; btype; value; body })

let const ?(ups = []) s = HExpr.hashcons (Const { name = s; uparams = ups })

let bv i = HExpr.hashcons (BoundVar i)

let fvar name expr info fvarId =
  HExpr.hashcons (FreeVar { name; expr; info; fvarId })

let proj x i e = HExpr.hashcons (Proj { name = x; nat = i; expr = e })

let natlit i = HExpr.hashcons (Literal (NatLit i))

let strlit s = HExpr.hashcons (Literal (StrLit s))

(** Collect a chain of [App(App(App(f, x_0), x_1) ... x_N)] as [f,[x_0;x_1;x_2;...;x_N]]*)
let get_apps e =
  let rec aux head running =
    match node head with
    | App (f, a) -> aux f (running @ [ a ])
    | _ -> head, CCList.rev running
  in
  aux e []

let gather_lams e =
  let rec aux running final =
    match node final with
    | Lam { name; btype; body; _ } -> aux (running @ [ name, btype ]) body
    | _ -> running, final
  in
  aux [] e

let gather_foralls f =
  let rec aux running final =
    match node final with
    | Forall { name; btype; body; _ } -> aux (running @ [ name, btype ]) body
    | _ -> running, final
  in
  aux [] f

module Pp = struct
  module Prec = struct
    type t =
      | Bot
      | Proj
      | App
      | Arrow
      | Let
      | Atom
      | Sort
      | Binder
      | Top

    let rank = function
      | Top -> 10
      | Atom -> 6
      | Proj -> 5
      | App -> 4
      | Sort -> 3 (* ensure this is <= App so it gets parens as arg *)
      | Arrow -> 2
      | Let -> 1
      | Binder -> 0
      | Bot -> -1
  end

  let bracks (info : binfo) =
    match info with
    | Default -> "(", ")"
    | Implicit -> "{", "}"
    | InstanceImplicit -> "[", "]"
    | StrictImplicit -> "{{", "}}"

  (** put "()" around [fmt] if needed. Parentheses are needed when the current node binds looser than the surrounding context. *)
  let wrap here ctx out fmt =
    if Prec.rank here < Prec.rank ctx then (
      CCFormat.string out "(";
      Format.kfprintf (fun _ -> CCFormat.string out ")") out fmt
    ) else
      Format.kfprintf (fun _ -> ()) out fmt

  let rec pp prec fpf expr =
    match node expr with
    | Sort u ->
      if Level.is_zero u then
        wrap Prec.Sort prec fpf "Prop"
      else
        wrap Prec.Sort prec fpf "Sort %a" Level.pp u
    | BoundVar i ->
      (* TODO: maintain a stack of bound var names and print those according to de Bruijn pointer instead of plain indices.*)
      Fmt.fprintf fpf "#%d" i
    | FreeVar { name; fvarId; _ } ->
      Fmt.fprintf fpf "%a##%d" Name.pp name fvarId
    | Const { name; uparams } ->
      if CCList.is_empty uparams then
        Fmt.fprintf fpf "%a" Name.pp name
      else
        Fmt.fprintf fpf "@[<h>%a.@[<h>{%a}@]@]" Name.pp name
          Fmt.(list ~sep:(fun fpf _ -> Fmt.fprintf fpf ",") Level.pp)
          uparams
    | App _ ->
      let f, args = get_apps expr in
      wrap Prec.App prec fpf "@[<2>%a@ %a@]" (pp prec) f
        Fmt.(list ~sep:Fmt.pp_print_space (pp Prec.Atom))
        args
    | Lam _ ->
      let binders, final = gather_lams expr in
      let pp_binder fpf (name, btype) =
        (* TODO: print brackets according to binfo. *)
        Fmt.fprintf fpf "@[(%a : %a)@]" Name.pp name (pp Prec.Binder) btype
      in
      wrap Prec.Arrow prec fpf "@[<v 0>@[<hv 2>fun @[%a@] => %a@]@]"
        Fmt.(list ~sep:Fmt.pp_print_cut pp_binder)
        binders (pp prec) final
    | Forall _ ->
      let binders, final = gather_foralls expr in
      let pp_binder fpf (name, btype) =
        (* TODO: print brackets according to binfo. *)
        Fmt.fprintf fpf "@[(%a : %a)@]" Name.pp name (pp Prec.Binder) btype
      in
      wrap Prec.Arrow prec fpf "@[<v 0>@[<hv 2>forall @[%a@], %a@]@]"
        Fmt.(list ~sep:Fmt.pp_print_cut pp_binder)
        binders (pp prec) final
    | Let { name; btype; value; body } ->
      wrap Prec.Let prec fpf "@[let @[<2>%a : %a :=@ %a@] in@ %a@]" Name.pp name
        (pp prec) btype (pp prec) value (pp prec) body
    | Proj { name; nat; expr } ->
      wrap Prec.Proj prec fpf "%a.%a.%d" (pp Prec.Proj) expr Name.pp name nat
    | Literal l ->
      (match l with
      | NatLit i -> Fmt.fprintf fpf "%a" Z.pp_print i
      | StrLit s -> Fmt.fprintf fpf "%S" s)
end

let pp fpf e = Pp.pp Pp.Prec.Bot fpf e

let to_string e = Fmt.to_string pp e

let rec has_free_vars (expr : t) =
  match node expr with
  | BoundVar _ -> false
  | FreeVar _ -> true
  | Const _ -> false
  | Sort _ -> false
  | App (f, x) -> has_free_vars f || has_free_vars x
  | Lam { btype; body; _ } -> has_free_vars btype || has_free_vars body
  | Forall { btype; body; _ } -> has_free_vars btype || has_free_vars body
  | Let { btype; value; body; _ } ->
    has_free_vars btype || has_free_vars value || has_free_vars body
  | Proj { expr; _ } -> has_free_vars expr
  | Literal _ -> false

let is_free_var expr =
  match expr with
  | FreeVar _ -> true
  | _ -> false

let get_fvar_id expr =
  match node expr with
  | FreeVar { fvarId; _ } -> fvarId
  | _ ->
    Logger.err "Expr %a is not a free variable" (Failure "get_fvar_id") pp expr

let rec num_loose_bvars expr =
  match node expr with
  | Sort _ | Const _ | FreeVar _ | Literal _ -> 0
  | BoundVar i -> i + 1
  | App (f, a) -> Int.max (num_loose_bvars f) (num_loose_bvars a)
  | Forall { btype; body; _ } | Lam { btype; body; _ } ->
    Int.max (num_loose_bvars btype) (num_loose_bvars body - 1)
  | Let { btype; value; body; _ } ->
    Int.max
      (Int.max (num_loose_bvars btype) (num_loose_bvars value))
      (num_loose_bvars body - 1)
  | Proj { expr; _ } -> num_loose_bvars expr

(** Substitute the [free_var] at the [expr] (has to be a bound variable). TODO: this needs to be optimized by counting the number of loose bound variables in the expr.  *)
let instantiate
    ?(logger =
      (module Util.MakeLogger (struct
        let header = "Expr"
      end) : Util.LOGGER)) ~(free_var : t) ~(expr : t) () =
  let module Logger = (val logger) in
  Logger.debugf
    (fun fpf (e1, e2) ->
      CCFormat.fprintf fpf
        "@[<v 0>@{<yellow>instantiate:@} @[<hov 2>%a@] in @[<hov 2>%a@]@]" pp e1
        pp e2)
    (free_var, expr);
  let rec instantiate_aux (free_var : t) (expr : t) (offset : int) =
    if num_loose_bvars expr <= offset then
      expr
    else (
      match node expr with
      | BoundVar i ->
        if offset <= i then
          (* Logger.debugf
             (fun fpf (e1, e2) ->
               CCFormat.fprintf fpf
                 "@[<v 0>@{<blue>At offset %d instantiated:@}@,\
                  @[<hov 2>%a@] @,\
                  in@,\
                  @[<hov 2>%a@]@]" offset pp e1 pp e2)
             (free_var, expr); *)
          free_var
        else
          expr
      | FreeVar _ | Const _ | Sort _ | Literal _ -> expr
      | App (f, a) ->
        app
          (instantiate_aux free_var f offset)
          (instantiate_aux free_var a offset)
      | Lam { name; btype; binfo; body } ->
        lambda name
          (instantiate_aux free_var btype offset)
          binfo
          (instantiate_aux free_var body (offset + 1))
      | Forall { name; btype; binfo; body } ->
        pi name
          (instantiate_aux free_var btype offset)
          binfo
          (instantiate_aux free_var body (offset + 1))
      | Let { name; btype; value; body } ->
        letin name
          (instantiate_aux free_var btype offset)
          (instantiate_aux free_var value offset)
          (instantiate_aux free_var body (offset + 1))
      | Proj { name; nat; expr } ->
        proj name nat (instantiate_aux free_var expr offset)
    )
  in
  let inst = instantiate_aux free_var expr 0 in
  Logger.debugf
    (fun fpf t ->
      CCFormat.fprintf fpf
        "@[@{<Blue>Type after instantiation@}: @,@[<hov 2>%a@]@]" pp t)
    inst;
  inst

(** Abstract a specific free var (by [target_id]) at depth [k], producing a body
   suitable to be wrapped by a binder inserted at that same depth [k]. TODO: This will need to be optimized later by checking if the expr has any free variables. *)
let rec abstract_fvar ~(target_id : int) ~(k : int) (e : t) =
  match node e with
  | BoundVar i ->
    (* we are inserting a binder at depth k; bump existing indices >= k *)
    if i >= k then
      bv (i + 1)
    else
      e
  | FreeVar fv ->
    if fv.fvarId = target_id then
      bv k
    else
      e
  | Const _ | Sort _ | Literal _ -> e
  | App (f, a) ->
    app (abstract_fvar ~target_id ~k f) (abstract_fvar ~target_id ~k a)
  | Lam { name; btype; binfo; body } ->
    lambda name
      (abstract_fvar ~target_id ~k btype)
      binfo
      (abstract_fvar ~target_id ~k:(k + 1) body)
  | Forall { name; btype; binfo; body } ->
    pi name
      (abstract_fvar ~target_id ~k btype)
      binfo
      (abstract_fvar ~target_id ~k:(k + 1) body)
  | Let { name; btype; value; body } ->
    letin name
      (abstract_fvar ~target_id ~k btype)
      (abstract_fvar ~target_id ~k value)
      (abstract_fvar ~target_id ~k:(k + 1) body)
  | Proj { name; nat; expr = e1 } ->
    proj name nat (abstract_fvar ~target_id ~k e1)

let rec subst_levels (expr : t) (ks : Level.t list) (vs : Level.t list) =
  match node expr with
  | BoundVar _ | Literal _ -> expr
  | Sort l -> sort (Level.subst_simp ~level:l ~ks ~vs)
  | FreeVar { name; expr; info; fvarId } ->
    fvar name (subst_levels expr ks vs) info fvarId
  | Const { name; uparams } ->
    let uparams' = Level.subst_levels ~levels:uparams ~ks ~vs in
    const name ~ups:uparams'
  | App (f, a) -> app (subst_levels f ks vs) (subst_levels a ks vs)
  | Lam { name; btype; binfo; body } ->
    lambda name (subst_levels btype ks vs) binfo (subst_levels body ks vs)
  | Forall { name; btype; binfo; body } ->
    pi name (subst_levels btype ks vs) binfo (subst_levels body ks vs)
  | Let { name; btype; value; body } ->
    letin name (subst_levels btype ks vs) (subst_levels value ks vs)
      (subst_levels body ks vs)
  | Proj { name; nat; expr } -> proj name nat (subst_levels expr ks vs)

open Nyaya_parser

let getter tbl key excp =
  match CCHashtbl.get tbl key with
  | Some v -> v
  | None -> failwith @@ Fmt.sprintf "Could not find id %d in %s" key excp

let table name_table level_table (ast : Ast.t) : (Ast.eidx, t) Hashtbl.t =
  let resolved_table = Hashtbl.create (CCList.length ast.items) in
  let expr_table = Ast.Hashed.exprs ast in
  let rec resolve (eid : int) =
    match Hashtbl.find_opt resolved_table eid with
    | Some expr -> expr
    | None ->
      let resolved =
        let open CCOption in
        let+ expr = Hashtbl.find_opt expr_table eid in
        match expr with
        | Ast.Expr.EVExpr { num; _ } -> bv num
        | Ast.Expr.ESExpr { uid; _ } ->
          sort (getter level_table uid "level table in ES")
        | Ast.Expr.ECExpr { nid; uids; _ } ->
          let name = getter name_table nid "name table in EC" in
          let uparams =
            CCList.(
              uids >|= fun id -> getter level_table id "level table in EC")
          in
          const name ~ups:uparams
        | Ast.Expr.EAExpr { eid2; eid3; _ } -> app (resolve eid2) (resolve eid3)
        | Ast.Expr.ELExpr { info; nid; eid2; eid3; _ } ->
          let name = getter name_table nid "name table in EL" in
          let binfo = binfo_of_ast info in
          let btype = resolve eid2 in
          let body = resolve eid3 in
          lambda name btype binfo body
        | Ast.Expr.EPExpr { info; nid; eid2; eid3; _ } ->
          let name = getter name_table nid "name table in EP" in
          let binfo = binfo_of_ast info in
          let btype = resolve eid2 in
          let body = resolve eid3 in
          pi name btype binfo body
        | Ast.Expr.EZExpr { nid; eid2; eid3; eid4; _ } ->
          let name = getter name_table nid "name table in EZ" in
          let btype = resolve eid2 in
          let value = resolve eid3 in
          let body = resolve eid4 in
          letin name btype value body
          (* Let { name; btype; value; body } *)
        | Ast.Expr.EJExpr { nid; num; eid2; _ } ->
          let name = getter name_table nid "name table in EJ" in
          let nat = num in
          let expr = resolve eid2 in
          proj name nat expr
        | Ast.Expr.ELNExpr { num; _ } ->
          let nat = Z.of_string num in
          natlit nat
        | Ast.Expr.ELSExpr { hexhex; _ } ->
          let str =
            let bytes = Bytes.create (List.length hexhex) in
            List.iteri
              (fun i h ->
                let byte = int_of_string ("0x" ^ h) in
                Bytes.set bytes i (Char.chr byte))
              hexhex;
            Bytes.to_string bytes
          in
          strlit str
        | Ast.Expr.EMExpr _ ->
          failwith "Do not know how to resolve #EM just yet!!"
      in

      (match resolved with
      | Some nm ->
        Hashtbl.add resolved_table eid nm;
        nm
      | None ->
        failwith @@ Fmt.sprintf "@[expr resolution failed for id %d@.@]" eid)
  in

  (* Resolve every expr *)
  Hashtbl.iter (fun eid _ -> ignore (resolve eid)) expr_table;
  Logger.info "Finished resolving exprs. Total number : %d."
    (Hashtbl.length expr_table);
  resolved_table
