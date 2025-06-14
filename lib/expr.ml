(* TODO: Expressions need to store some data inline or cache it somewhere to prevent prohibitively expensive recomputation.
   Perhaps we could do hashconsing.*)
open Nyaya_parser

module Logger = Util.MakeLogger (struct
  let header = "Expr"
end)

type binfo =
  | Default
  | Implicit
  | InstanceImplicit
  | StrictImplicit
[@@deriving show]

let binfo_of_ast (info : Ast.info) =
  match info with
  | Ast.IBD -> Default
  | Ast.IBI -> Implicit
  | Ast.IBS -> StrictImplicit
  | Ast.IBC -> InstanceImplicit

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

let getter tbl key excp =
  match CCHashtbl.get tbl key with
  | Some v -> v
  | None -> failwith @@ CCFormat.sprintf "Could not find id %d in %s" key excp

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
        failwith
        @@ CCFormat.sprintf "@[expr resolution failed for id %d@.@]" eid)
  in

  (* Resolve every expr *)
  Hashtbl.iter (fun eid _ -> ignore (resolve eid)) expr_table;
  Logger.info "Finished resolving exprs. Total number : %d."
    (Hashtbl.length expr_table);
  resolved_table
