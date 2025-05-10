(* This module contains the first of the Lean kernel's primitive types; the
   type of names. It provides kernel items with a way of addressing things. *)
open Nyaya_parser
module Fmt = CCFormat

type t =
  | Anon
  | Str of (t * string)
  | Num of (t * int)
[@@deriving show]

let is_anon t =
  match t with
  | Anon -> true
  | _ -> false

let rec pp fpf name =
  match name with
  | Anon -> Fmt.fprintf fpf ""
  | Str (n1, str) ->
    if is_anon n1 then
      Fmt.fprintf fpf "%s" str
    else
      Fmt.fprintf fpf "%a.%s" pp n1 str
  | Num (n1, id) -> Fmt.fprintf fpf "%a.%d" pp n1 id

let resolver (ast : Ast.t) : (Ast.nidx, t) Hashtbl.t =
  let resolved_table : (int, t) Hashtbl.t =
    Hashtbl.create (CCList.length ast.items)
  in
  let name_table = Ast.Hashed.names ast in
  (* Recursive resolution with memoization *)
  let rec resolve (nid : int) =
    match Hashtbl.find_opt resolved_table nid with
    | Some name -> name
    | None when nid = 0 -> Anon
    | None ->
      let resolved =
        match Hashtbl.find name_table nid with
        | NSName { nid2; str; _ } ->
          let parent_name = resolve nid2 in
          Str (parent_name, str)
        | NIName { nid2; nat; _ } ->
          let parent_name = resolve nid2 in
          Num (parent_name, nat)
      in
      Hashtbl.add resolved_table nid resolved;
      resolved
  in

  (* Resolve every name *)
  Hashtbl.iter (fun nid _ -> ignore (resolve nid)) name_table;
  resolved_table
