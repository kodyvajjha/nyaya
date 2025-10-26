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

type t =
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
[@@deriving show]

(** Collect a chain of [App(App(App(f, x_0), x_1) ... x_N)] as [f,[x_0;x_1;x_2;...;x_N]]*)
let get_apps e =
  let rec aux head running =
    match head with
    | App (f, a) -> aux f (running @ [ a ])
    | _ -> head, CCList.rev running
  in
  aux e []

let gather_lams e =
  let rec aux running final =
    match final with
    | Lam { name; btype; body; _ } -> aux (running @ [ name, btype ]) body
    | _ -> running, final
  in
  aux [] e

let gather_foralls f =
  let rec aux running final =
    match final with
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
    match expr with
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
    | App _ as e ->
      let f, args = get_apps e in
      wrap Prec.App prec fpf "@[<2>%a@ %a@]" (pp prec) f
        Fmt.(list ~sep:Fmt.pp_print_space (pp Prec.Atom))
        args
    | Lam _ as l ->
      let binders, final = gather_lams l in
      let pp_binder fpf (name, btype) =
        (* TODO: print brackets according to binfo. *)
        Fmt.fprintf fpf "@[(%a : %a)@]" Name.pp name (pp Prec.Binder) btype
      in
      wrap Prec.Arrow prec fpf "@[<v 0>@[<hv 2>fun @[%a@] => %a@]@]"
        Fmt.(list ~sep:Fmt.pp_print_cut pp_binder)
        binders (pp prec) final
    | Forall _ as f ->
      let binders, final = gather_foralls f in
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
      | NatLit i -> Fmt.fprintf fpf "nat literal %a" Z.pp_print i
      | StrLit s -> Fmt.fprintf fpf "str literal %S" s)
end

let pp fpf e = Pp.pp Pp.Prec.Bot fpf e

let to_string e = Fmt.to_string pp e

let sort l = Sort l

let lambda name btype body = Lam { name; btype; binfo = Default; body }

(** Transform [(f, [x0; x1; ...; xN])] into [App(App(...App(f, x0), x1), ... , xN)]. Note: this is the inverse of [get_apps] above.*)
let mk_app f args = CCList.fold_left (fun acc a -> App (acc, a)) f args

let pi x ty body = Forall { name = x; btype = ty; binfo = Default; body }

let letin x ty v b = Let { name = x; btype = ty; value = v; body = b }

let const ?(ups = []) s = Const { name = s; uparams = ups }

let bv i = BoundVar i

let rec has_free_vars (expr : t) =
  match expr with
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
  match expr with
  | FreeVar { fvarId; _ } -> fvarId
  | _ ->
    Logger.err "Expr %a is not a free variable" (Failure "get_fvar_id") pp expr

let rec num_loose_bvars expr =
  match expr with
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
let instantiate ~(free_var : t) ~(expr : t) =
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
      match expr with
      | BoundVar i ->
        if offset <= i then (
          Logger.debugf
            (fun fpf (e1, e2) ->
              CCFormat.fprintf fpf
                "@[<v 0>@{<blue>At offset %d instantiated:@}@,\
                 @[<hov 2>%a@] @,\
                 in@,\
                 @[<hov 2>%a@]@]" offset pp e1 pp e2)
            (free_var, expr);
          free_var
        ) else
          expr
      | FreeVar _ | Const _ | Sort _ | Literal _ -> expr
      | App (f, a) ->
        App
          (instantiate_aux free_var f offset, instantiate_aux free_var a offset)
      | Lam { name; btype; binfo; body } ->
        Lam
          {
            name;
            btype = instantiate_aux free_var btype offset;
            binfo;
            body = instantiate_aux free_var body (offset + 1);
          }
      | Forall { name; btype; binfo; body } ->
        Forall
          {
            name;
            btype = instantiate_aux free_var btype offset;
            binfo;
            body = instantiate_aux free_var body (offset + 1);
          }
      | Let { name; btype; value; body } ->
        Let
          {
            name;
            btype = instantiate_aux free_var btype offset;
            value = instantiate_aux free_var value offset;
            body = instantiate_aux free_var body (offset + 1);
          }
      | Proj { name; nat; expr } ->
        Proj { name; nat; expr = instantiate_aux free_var expr offset }
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
  match e with
  | BoundVar i ->
    (* we are inserting a binder at depth k; bump existing indices >= k *)
    if i >= k then
      BoundVar (i + 1)
    else
      e
  | FreeVar fv ->
    if fv.fvarId = target_id then
      BoundVar k
    else
      e
  | Const _ | Sort _ | Literal _ -> e
  | App (f, a) ->
    App (abstract_fvar ~target_id ~k f, abstract_fvar ~target_id ~k a)
  | Lam { name; btype; binfo; body } ->
    Lam
      {
        name;
        btype = abstract_fvar ~target_id ~k btype;
        binfo;
        body = abstract_fvar ~target_id ~k:(k + 1) body;
      }
  | Forall { name; btype; binfo; body } ->
    Forall
      {
        name;
        btype = abstract_fvar ~target_id ~k btype;
        binfo;
        body = abstract_fvar ~target_id ~k:(k + 1) body;
      }
  | Let { name; btype; value; body } ->
    Let
      {
        name;
        btype = abstract_fvar ~target_id ~k btype;
        value = abstract_fvar ~target_id ~k value;
        body = abstract_fvar ~target_id ~k:(k + 1) body;
      }
  | Proj { name; nat; expr = e1 } ->
    Proj { name; nat; expr = abstract_fvar ~target_id ~k e1 }

module Reduce = struct
  let beta e =
    Logger.debug "@[Beta reducing @[<hov 2> %a@]]" pp e;
    let rec aux f args =
      match f, args with
      | Lam { body; _ }, v :: vs -> aux (instantiate ~free_var:body ~expr:v) vs
      | _, _ -> mk_app f args
    in
    let f, args = get_apps e in
    let ans = aux f args in
    Logger.debugf
      (fun fpf (t1, t2) ->
        CCFormat.fprintf fpf
          "@[<hov 0>After beta reduction@;\
           @[<hov 2>%a@]@;\
           becomes@;\
           @[<hov 2>%a@]@]" pp t1 pp t2)
      (e, ans);
    ans
end

let rec subst_levels (expr : t) (ks : Level.t list) (vs : Level.t list) =
  match expr with
  | BoundVar _ | Literal _ -> expr
  | Sort l -> Sort (Level.subst_simp ~level:l ~ks ~vs)
  | FreeVar { name; expr; info; fvarId } ->
    FreeVar { name; expr = subst_levels expr ks vs; info; fvarId }
  | Const { name; uparams } ->
    let uparams' = Level.subst_levels ~levels:uparams ~ks ~vs in
    Const { name; uparams = uparams' }
  | App (f, a) -> App (subst_levels f ks vs, subst_levels a ks vs)
  | Lam { name; btype; binfo; body } ->
    Lam
      {
        name;
        btype = subst_levels btype ks vs;
        binfo;
        body = subst_levels body ks vs;
      }
  | Forall { name; btype; binfo; body } ->
    Forall
      {
        name;
        btype = subst_levels btype ks vs;
        binfo;
        body = subst_levels body ks vs;
      }
  | Let { name; btype; value; body } ->
    Let
      {
        name;
        btype = subst_levels btype ks vs;
        value = subst_levels value ks vs;
        body = subst_levels body ks vs;
      }
  | Proj { name; nat; expr } ->
    Proj { name; nat; expr = subst_levels expr ks vs }

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
        | Ast.Expr.EVExpr { num; _ } -> BoundVar num
        | Ast.Expr.ESExpr { uid; _ } ->
          Sort (getter level_table uid "level table in ES")
        | Ast.Expr.ECExpr { nid; uids; _ } ->
          let name = getter name_table nid "name table in EC" in
          let uparams =
            CCList.(
              uids >|= fun id -> getter level_table id "level table in EC")
          in
          Const { name; uparams }
        | Ast.Expr.EAExpr { eid2; eid3; _ } -> App (resolve eid2, resolve eid3)
        | Ast.Expr.ELExpr { info; nid; eid2; eid3; _ } ->
          let name = getter name_table nid "name table in EL" in
          let binfo = binfo_of_ast info in
          let btype = resolve eid2 in
          let body = resolve eid3 in
          Lam { name; binfo; btype; body }
        | Ast.Expr.EPExpr { info; nid; eid2; eid3; _ } ->
          let name = getter name_table nid "name table in EP" in
          let binfo = binfo_of_ast info in
          let btype = resolve eid2 in
          let body = resolve eid3 in
          Forall { name; binfo; btype; body }
        | Ast.Expr.EZExpr { nid; eid2; eid3; eid4; _ } ->
          let name = getter name_table nid "name table in EZ" in
          let btype = resolve eid2 in
          let value = resolve eid3 in
          let body = resolve eid4 in
          Let { name; btype; value; body }
        | Ast.Expr.EJExpr { nid; num; eid2; _ } ->
          let name = getter name_table nid "name table in EJ" in
          let nat = num in
          let expr = resolve eid2 in
          Proj { name; nat; expr }
        | Ast.Expr.ELNExpr { num; _ } ->
          let nat = Z.of_string num in
          Literal (NatLit nat)
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
          Literal (StrLit str)
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
