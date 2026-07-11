(** Parser for the JSON-based Lean 4 export format ("format_ndjson").

    See https://github.com/leanprover/lean4export/blob/master/format_ndjson.md

    The export is newline-delimited JSON: a leading [meta] object followed by
    one object per line describing a Name, Level, Expression or Declaration.
    Each primitive carries its own index ([in]/[il]/[ie]); references between
    primitives are those integer indices.

    This parser translates the JSON representation into the same flat {!Ast.t}
    that the legacy positional parser produced, so that the downstream
    environment builder ({!Nyaya.Env}) is left untouched. In particular
    inductive blocks are re-encoded into the positional integer lists that
    {!Ast.Decl.Inductive} / {!Ast.Decl.Recursor} expect, and each recursor
    rule is handed a freshly-allocated index. *)

module J = Yojson.Safe

module NdjsonLogger = Util.MakeLogger (struct
  let header = "Ndjson"
end)

(* ------------------------------------------------------------------ *)
(* JSON accessors                                                      *)
(* ------------------------------------------------------------------ *)

let member = J.Util.member

(** [has k j] is [true] when object [j] has a non-null member [k]. *)
let has k j =
  match member k j with
  | `Null -> false
  | _ -> true

(** Read an integer, tolerating JSON [Intlit] (integers too large for the
    native representation are still returned faithfully via {!int_of_string}). *)
let to_int = function
  | `Int i -> i
  | `Intlit s -> int_of_string s
  | j ->
    failwith (Printf.sprintf "ndjson: expected integer, got %s" (J.to_string j))

let to_string = function
  | `String s -> s
  | j ->
    failwith (Printf.sprintf "ndjson: expected string, got %s" (J.to_string j))

let to_list = function
  | `List l -> l
  | j ->
    failwith (Printf.sprintf "ndjson: expected array, got %s" (J.to_string j))

let to_bool = function
  | `Bool b -> b
  | j ->
    failwith (Printf.sprintf "ndjson: expected bool, got %s" (J.to_string j))

(** Member [k] of [j] as an [int]. *)
let mint k j = to_int (member k j)

(** Member [k] of [j] as an [int list] (e.g. [levelParams], [us], [all]). *)
let mints k j = List.map to_int (to_list (member k j))

let b2i b = if b then 1 else 0

(* ------------------------------------------------------------------ *)
(* Primitives                                                          *)
(* ------------------------------------------------------------------ *)

(* [{"in": self, "str": {"pre": p, "str": s}}]
   [{"in": self, "num": {"pre": p, "i": n}}]  *)
let name_of_json (j : J.t) : Ast.Name.t =
  let nid1 = mint "in" j in
  if has "str" j then (
    let s = member "str" j in
    Ast.Name.NSName { nid1; nid2 = mint "pre" s; str = to_string (member "str" s) }
  ) else if has "num" j then (
    let n = member "num" j in
    Ast.Name.NIName { nid1; nid2 = mint "pre" n; nat = mint "i" n }
  ) else
    failwith "ndjson: name is neither str nor num"

(* [{"il": self, "succ": a}]
   [{"il": self, "max": [a, b]}]
   [{"il": self, "imax": [a, b]}]
   [{"il": self, "param": nid}]  *)
let level_of_json (j : J.t) : Ast.Level.t =
  let uid1 = mint "il" j in
  if has "succ" j then
    Ast.Level.USLevel { uid1; uid2 = mint "succ" j }
  else if has "max" j then (
    match mints "max" j with
    | [ a; b ] -> Ast.Level.UMLevel { uid1; uid2 = a; uid3 = b }
    | _ -> failwith "ndjson: max expects two levels"
  ) else if has "imax" j then (
    match mints "imax" j with
    | [ a; b ] -> Ast.Level.UIMLevel { uid1; uid2 = a; uid3 = b }
    | _ -> failwith "ndjson: imax expects two levels"
  ) else if has "param" j then
    Ast.Level.UPLevel { uid = uid1; nid = mint "param" j }
  else
    failwith "ndjson: unrecognised level node"

let info_of_string = function
  | "default" -> Ast.IBD
  | "implicit" -> Ast.IBI
  | "strictImplicit" -> Ast.IBS
  | "instImplicit" -> Ast.IBC
  | s -> failwith (Printf.sprintf "ndjson: unknown binderInfo %S" s)

let info_of_json j = info_of_string (to_string (member "binderInfo" j))

(** Encode a string's UTF-8 bytes as the two-hex-digit-per-byte list the AST
    string-literal node stores (the legacy textual format used this encoding;
    {!Nyaya.Expr} decodes it back to a string). *)
let hexhex_of_string (s : string) : string list =
  List.init (String.length s) (fun i ->
      Printf.sprintf "%02x" (Char.code (String.get s i)))

(* [{"ie": self, <variant>: ...}] *)
let expr_of_json (j : J.t) : Ast.Expr.t =
  let eid = mint "ie" j in
  if has "bvar" j then
    Ast.Expr.EVExpr { eid; num = mint "bvar" j }
  else if has "sort" j then
    Ast.Expr.ESExpr { eid; uid = mint "sort" j }
  else if has "const" j then (
    let c = member "const" j in
    Ast.Expr.ECExpr { eid; nid = mint "name" c; uids = mints "us" c }
  ) else if has "app" j then (
    let a = member "app" j in
    Ast.Expr.EAExpr { eid1 = eid; eid2 = mint "fn" a; eid3 = mint "arg" a }
  ) else if has "lam" j then (
    let l = member "lam" j in
    Ast.Expr.ELExpr
      {
        eid1 = eid;
        info = info_of_json l;
        nid = mint "name" l;
        eid2 = mint "type" l;
        eid3 = mint "body" l;
      }
  ) else if has "forallE" j then (
    let f = member "forallE" j in
    Ast.Expr.EPExpr
      {
        eid1 = eid;
        info = info_of_json f;
        nid = mint "name" f;
        eid2 = mint "type" f;
        eid3 = mint "body" f;
      }
  ) else if has "letE" j then (
    let z = member "letE" j in
    Ast.Expr.EZExpr
      {
        eid1 = eid;
        nid = mint "name" z;
        eid2 = mint "type" z;
        eid3 = mint "value" z;
        eid4 = mint "body" z;
      }
  ) else if has "proj" j then (
    let p = member "proj" j in
    Ast.Expr.EJExpr
      {
        eid1 = eid;
        nid = mint "typeName" p;
        num = mint "idx" p;
        eid2 = mint "struct" p;
      }
  ) else if has "natVal" j then
    (* natVal is a decimal string; [Nyaya.Expr] parses it with [Z.of_string]. *)
    Ast.Expr.ELNExpr { eid; num = to_string (member "natVal" j) }
  else if has "strVal" j then
    Ast.Expr.ELSExpr
      { eid; hexhex = hexhex_of_string (to_string (member "strVal" j)) }
  else if has "mdata" j then (
    let m = member "mdata" j in
    Ast.Expr.EMExpr { eid1 = eid; mptr = None; eid2 = mint "expr" m }
  ) else
    failwith "ndjson: unrecognised expression node"

(* ------------------------------------------------------------------ *)
(* Declarations                                                        *)
(* ------------------------------------------------------------------ *)

let hint_of_json (j : J.t) : Ast.hint =
  match j with
  | `String "opaque" -> Ast.HO
  | `String "abbrev" -> Ast.HA
  | `Assoc _ when has "regular" j -> Ast.HR (mint "regular" j)
  | _ -> failwith "ndjson: unrecognised reducibility hint"

(** A monotonically increasing source of recursor-rule indices. Rule indices
    live in their own table downstream, so they only need to be unique amongst
    themselves. *)
let fresh_rid =
  let counter = ref 0 in
  fun () ->
    let r = !counter in
    incr counter;
    r

(* An inductive block bundles several inductive types, their constructors and
   recursors. We flatten it back to the legacy per-declaration items and, for
   inductives/recursors, to the positional integer lists that {!Nyaya.Env}
   decodes. *)

(* [name; type; isReflexive; isRec; numNested; numParams; numIndices;
    |all|; all...; |ctors|; ctors...; levelParams...] *)
let inductive_item (t : J.t) : Ast.Item.t =
  let all = mints "all" t in
  let ctors = mints "ctors" t in
  let lparams = mints "levelParams" t in
  let l =
    [
      mint "name" t;
      mint "type" t;
      b2i (to_bool (member "isReflexive" t));
      b2i (to_bool (member "isRec" t));
      mint "numNested" t;
      mint "numParams" t;
      mint "numIndices" t;
      List.length all;
    ]
    @ all
    @ (List.length ctors :: ctors)
    @ lparams
  in
  Ast.Item.EDecl (Ast.Decl.Inductive l)

(* [{"cidx"; "induct"; "name"; "numFields"; "numParams"; "type";
     "levelParams"}] -> {!Ast.Decl.Constructor} *)
let constructor_item (c : J.t) : Ast.Item.t =
  Ast.Item.EDecl
    (Ast.Decl.Constructor
       {
         name = mint "name" c;
         expr = mint "type" c;
         parent_inductive = mint "induct" c;
         ctor_id = mint "cidx" c;
         num_params = mint "numParams" c;
         num_fields = mint "numFields" c;
         uparams = mints "levelParams" c;
       })

(* A recursor yields its own [Recursor] item plus one [ERecRule] item per rule.
   Positional list:
   [name; type; |all|; all...; numParams; numIndices; numMotives; numMinors;
    |rules|; ruleIds...; isK; levelParams...] *)
let recursor_items (r : J.t) : Ast.Item.t list =
  let all = mints "all" r in
  let lparams = mints "levelParams" r in
  let rules = to_list (member "rules" r) in
  let rule_items, rule_ids =
    List.split
      (List.map
         (fun rule ->
           let rid = fresh_rid () in
           let item =
             Ast.Item.ERecRule
               {
                 rid;
                 ctorName = mint "ctor" rule;
                 numFields = mint "nfields" rule;
                 value = mint "rhs" rule;
               }
           in
           (item, rid))
         rules)
  in
  let l =
    [ mint "name" r; mint "type" r; List.length all ]
    @ all
    @ [
        mint "numParams" r;
        mint "numIndices" r;
        mint "numMotives" r;
        mint "numMinors" r;
        List.length rules;
      ]
    @ rule_ids
    @ [ b2i (to_bool (member "k" r)) ]
    @ lparams
  in
  Ast.Item.EDecl (Ast.Decl.Recursor l) :: rule_items

let inductive_items (j : J.t) : Ast.Item.t list =
  let block = member "inductive" j in
  let types = to_list (member "types" block) in
  let ctors = to_list (member "ctors" block) in
  let recs = to_list (member "recs" block) in
  List.map inductive_item types
  @ List.map constructor_item ctors
  @ List.concat_map recursor_items recs

(* [{"axiom"|"def"|"opaque"|"thm"|"quot": {...}}] *)
let simple_decl_items (j : J.t) : Ast.Item.t list =
  let one d = [ Ast.Item.EDecl d ] in
  if has "axiom" j then (
    let d = member "axiom" j in
    one
      (Ast.Decl.Axiom
         { name = mint "name" d; expr = mint "type" d; uparams = mints "levelParams" d })
  ) else if has "def" j then (
    let d = member "def" j in
    one
      (Ast.Decl.Definition
         {
           name = mint "name" d;
           expr = mint "type" d;
           value = mint "value" d;
           hint = hint_of_json (member "hints" d);
           uparams = mints "levelParams" d;
         })
  ) else if has "opaque" j then (
    let d = member "opaque" j in
    one
      (Ast.Decl.Opaque
         {
           name = mint "name" d;
           expr = mint "type" d;
           value = mint "value" d;
           uparams = mints "levelParams" d;
         })
  ) else if has "thm" j then (
    let d = member "thm" j in
    one
      (Ast.Decl.Theorem
         {
           name = mint "name" d;
           expr = mint "type" d;
           value = mint "value" d;
           uparams = mints "levelParams" d;
         })
  ) else if has "quot" j then (
    let d = member "quot" j in
    one
      (Ast.Decl.Quotient
         { name = mint "name" d; expr = mint "type" d; uparams = mints "levelParams" d })
  ) else if has "inductive" j then
    inductive_items j
  else
    failwith "ndjson: unrecognised declaration node"

(* ------------------------------------------------------------------ *)
(* Line dispatch and driver                                            *)
(* ------------------------------------------------------------------ *)

type parsed_line =
  | Meta of int list (* format version, when parseable *)
  | Items of Ast.Item.t list

(** Parse the [format.version] field of the [meta] object into an int list,
    tolerating anything that isn't a dotted numeric version. *)
let version_of_meta (j : J.t) : int list =
  match member "format" (member "meta" j) with
  | `Null -> []
  | fmt -> (
    match member "version" fmt with
    | `String v -> (
      try List.map int_of_string (String.split_on_char '.' v) with _ -> [])
    | _ -> [])

let parse_line (j : J.t) : parsed_line =
  if has "meta" j then Meta (version_of_meta j)
  else if has "in" j then Items [ Ast.Item.EName (name_of_json j) ]
  else if has "il" j then Items [ Ast.Item.ELevel (level_of_json j) ]
  else if has "ie" j then Items [ Ast.Item.EExpr (expr_of_json j) ]
  else Items (simple_decl_items j)

let parse_from_file (filename : string) : Ast.t =
  NdjsonLogger.info "Opening ndjson file %s for parsing..." filename;
  let ch = open_in filename in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ch)
    (fun () ->
      let version = ref [] in
      let items = ref [] in
      let lineno = ref 0 in
      (try
         while true do
           let line = input_line ch in
           incr lineno;
           let trimmed = String.trim line in
           if String.length trimmed > 0 then (
             let j =
               try J.from_string trimmed
               with Yojson.Json_error msg ->
                 failwith
                   (Printf.sprintf "%s:%d: invalid JSON: %s" filename !lineno msg)
             in
             match parse_line j with
             | Meta v -> version := v
             | Items its -> items := List.rev_append its !items)
         done
       with End_of_file -> ());
      NdjsonLogger.info "Finished parsing %s (%d items)." filename
        (List.length !items);
      { Ast.version = !version; items = List.rev !items })
