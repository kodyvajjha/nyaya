(* TODO: Expressions need to store some data inline or cache it somewhere to prevent prohibitively expensive recomputation.
   Perhaps we could do hashconsing.*)

type binfo =
  | Default
  | Implicit
  | InstanceImplicit
  | StrictImplicit
[@@deriving show]

let binfo_of_ast (info : Nyaya_parser.Ast.info) =
  match info with
  | Nyaya_parser.Ast.IBD -> Default
  | Nyaya_parser.Ast.IBI -> Implicit
  | Nyaya_parser.Ast.IBS -> StrictImplicit
  | Nyaya_parser.Ast.IBC -> InstanceImplicit

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
      fvarId: int; (* de Bruijn indices*)
    }
  | Const of {
      name: Name.t;
      uparams: Level.t list;
    }
  | Sort of Level.t
  | App of t * t
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

open Nyaya_parser

let table (ast : Ast.t) : (Ast.eidx, t) Hashtbl.t =
  let resolved_table = Hashtbl.create (CCList.length ast.items) in
  let item_table = Ast.Hashed.items ast in
  let rec resolve (eid : int) =
    match Hashtbl.find_opt resolved_table eid with
    | Some expr -> expr
    | None ->
      let resolved =
        let open CCOption in
        let* item = Hashtbl.find_opt item_table eid in
        let+ expr = Ast.Item.get_expr item in
        match expr with
        | Ast.Expr.EVExpr { num; _ } -> BoundVar num
        | Ast.Expr.ESExpr { uid; _ } ->
          Sort (Hashtbl.find (Level.table ast) uid)
        | Ast.Expr.ECExpr { nid; uids; _ } ->
          let name = Hashtbl.find (Name.table ast) nid in
          let uparams =
            CCList.(uids >|= fun id -> Hashtbl.find (Level.table ast) id)
          in
          Const { name; uparams }
        | Ast.Expr.EAExpr { eid2; eid3; _ } -> App (resolve eid2, resolve eid3)
        | Ast.Expr.ELExpr { info; nid; eid2; eid3; _ } ->
          let name = Hashtbl.find (Name.table ast) nid in
          let binfo = binfo_of_ast info in
          let btype = resolve eid2 in
          let body = resolve eid3 in
          Lam { name; binfo; btype; body }
        | Ast.Expr.EPExpr { info; nid; eid2; eid3; _ } ->
          let name = Hashtbl.find (Name.table ast) nid in
          let binfo = binfo_of_ast info in
          let btype = resolve eid2 in
          let body = resolve eid3 in
          Forall { name; binfo; btype; body }
        | Ast.Expr.EZExpr _ -> assert false
        | Ast.Expr.EJExpr _ -> assert false (* NYI *)
        | Ast.Expr.ELNExpr _ -> assert false (* NYI *)
        | Ast.Expr.ELSExpr _ -> assert false (* NYI *)
        | Ast.Expr.EMExpr _ -> assert false (* NYI *)
      in

      (match resolved with
      | Some nm ->
        Hashtbl.add resolved_table eid nm;
        nm
      | None ->
        failwith
        @@ CCFormat.sprintf "@[expr resolution failed for id %d@.@]" eid)
  in

  (* Resolve every name *)
  Hashtbl.iter (fun eid _ -> ignore (resolve eid)) item_table;
  resolved_table
