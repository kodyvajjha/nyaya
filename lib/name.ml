(* This module contains the first of the Lean kernel's primitive types; the
   type of names. It provides kernel items with a way of addressing things. *)

open Nyaya_parser
module Fmt = CCFormat

module Logger = Util.MakeLogger (struct
  let header = "Name"
end)

type t =
  | Anon
  | Str of (t * string)
  | Num of (t * int)

let of_string str = Str (Anon, str)

let of_num num = Num (Anon, num)

let is_anon t =
  match t with
  | Anon -> true
  | _ -> false

let rec has_seg seg = function
  | Anon -> false
  | Str (n, s) -> s = seg || has_seg seg n
  | Num (n, _) -> has_seg seg n

let rec ends_with_hyg = function
  | Str (_, "_hyg") -> true
  | Num (n, _) -> ends_with_hyg n (* allow trailing .<num> *)
  | Str (n, _) -> ends_with_hyg n
  | Anon -> false

let is_hyg_name n = has_seg "_@" n && ends_with_hyg n

let rec pp fpf name =
  if is_hyg_name name then
    Fmt.string fpf "_"
  else (
    match name with
    | Anon -> Fmt.fprintf fpf ""
    | Str (n1, str) ->
      if is_anon n1 then
        Fmt.fprintf fpf "%s" str
      else
        Fmt.fprintf fpf "%a.%s" pp n1 str
    | Num (n1, id) -> Fmt.fprintf fpf "%a.%d" pp n1 id
  )

(** Create a name table resolving names by mapping Ast name ids to typed name constructors. *)
let table (ast : Ast.t) : (Ast.nidx, t) Hashtbl.t =
  let resolved_table : (int, t) Hashtbl.t =
    Hashtbl.create (CCList.length ast.items)
  in
  Hashtbl.add resolved_table 0 Anon;
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
  Logger.info "Finished resolving names. Total number : %d."
    (Hashtbl.length name_table);
  resolved_table
