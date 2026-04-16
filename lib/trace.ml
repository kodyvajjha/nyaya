(* Hierarchical trace for the checker's recursive procedures.

   Each [Make] application produces a tracer parameterised by its
   input/output summaries; each [create ()] returns a fresh instance
   with its own stack and id counter. Logging is threaded in
   explicitly via a first-class [LOGGER] rather than pulled from
   module-level state, so traces and loggers can be combined freely.

   Behaviour matches the previous [Nyaya_parser.Util.MakeTrace]
   (module-level refs), with two deliberate differences:
     - state is per-instance, so concurrent or nested sessions do not
       clobber each other;
     - env-var driven configuration is read at [create] time. *)

exception Depth_limit of string

module type DATA = sig
  type input

  type output

  val kind : string
  (** A short tag (e.g. ["i"], ["w"], ["d"]) used in trace lines. *)

  val elide_ok_env : string
  (** Env var controlling whether successful trace lines are hidden.
      Truthy values: [1], [true], [TRUE], [yes], [YES]. *)

  val max_depth_env : string
  (** Env var setting the maximum trace depth. Exceeding it raises
      [Depth_limit]. Unset or unparseable values fall back to a
      sensible default. *)

  val input_summary : input -> string

  val output_summary : output -> string
end

module type S = sig
  type input

  type output

  type frame

  type t

  val create : unit -> t

  val reset : t -> unit

  val enter : t -> (module Nyaya_parser.Util.LOGGER) -> input -> frame

  val leave_success :
    t -> (module Nyaya_parser.Util.LOGGER) -> frame -> output -> unit

  val leave_failure :
    t -> (module Nyaya_parser.Util.LOGGER) -> frame -> exn -> unit
end

module Make (D : DATA) :
  S with type input = D.input and type output = D.output = struct
  type input = D.input

  type output = D.output

  type frame = {
    id: int;
    input: input;
  }

  type t = {
    mutable stack: frame list;
    mutable next_id: int;
    elide_ok: bool;
    max_depth: int;
  }

  let max_path_entries = 8

  let env_bool var =
    match Sys.getenv_opt var with
    | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> true
    | _ -> false

  let env_int ~default var =
    match Sys.getenv_opt var with
    | Some s -> (try int_of_string s with _ -> default)
    | None -> default

  let create () =
    {
      stack = [];
      next_id = 0;
      elide_ok = env_bool D.elide_ok_env;
      max_depth = env_int ~default:2000 D.max_depth_env;
    }

  let depth t = List.length t.stack

  let current_path t =
    let entries =
      CCList.take max_path_entries t.stack
      |> List.rev
      |> CCList.map (fun f -> string_of_int f.id)
    in
    let d = depth t in
    if d > max_path_entries then
      "..." ^ ">" ^ String.concat ">" entries
    else
      String.concat ">" entries

  let enter t logger input =
    let module L = (val logger : Nyaya_parser.Util.LOGGER) in
    let id = t.next_id in
    let frame = { id; input } in
    t.next_id <- id + 1;
    t.stack <- frame :: t.stack;
    let d = depth t in
    if d > t.max_depth then (
      let msg =
        Printf.sprintf "[%s#%d] depth %d exceeds max %d, input=%s" D.kind id d
          t.max_depth (D.input_summary input)
      in
      L.app "DEPTH LIMIT: %s" msg;
      raise (Depth_limit msg)
    );
    if not t.elide_ok then
      L.debug "[%s#%d d=%d p=%s] -> %s" D.kind id d (current_path t)
        (D.input_summary input);
    frame

  let leave_success t logger frame output =
    let module L = (val logger : Nyaya_parser.Util.LOGGER) in
    t.stack <- CCList.tl t.stack;
    if not t.elide_ok then
      L.debug "[%s#%d d=%d p=%s] <- ok %s" D.kind frame.id (depth t)
        (current_path t)
        (D.output_summary output)

  let leave_failure t logger frame exn =
    let module L = (val logger : Nyaya_parser.Util.LOGGER) in
    t.stack <- CCList.tl t.stack;
    match exn with
    | Depth_limit _ -> ()
    | _ ->
      L.debug "[%s#%d d=%d p=%s] !! %s input=%s" D.kind frame.id (depth t)
        (current_path t) (Printexc.to_string exn)
        (D.input_summary frame.input)

  let reset t =
    t.stack <- [];
    t.next_id <- 0
end
