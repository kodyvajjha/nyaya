(* Typecheck orchestration.

   [typecheck] iterates the environment, running [well_posed] and
   [check] per declaration. A per-declaration logger is swapped into
   the checker context so log headers identify the decl in progress.
   On checker failure, the declaration is replayed once at [Debug] log
   level into a per-declaration trace file — the only place in the
   code where the global logs-reporter state is swapped. *)

module Logger = Nyaya_parser.Util.MakeLogger (struct
  let header = "Checker"
end)

let mk_decl_logger (name : string) : (module Nyaya_parser.Util.LOGGER) =
  (module Nyaya_parser.Util.MakeLogger (struct
    let header = name
  end))

let is_checker_failure = function
  | Checker.TypeError _ | Checker.Defeq_failure _ -> true
  | _ -> false

(** Replay a single declaration with full debug tracing to a file. *)
let replay_debug (ctx : Ctx.t) (decl : Decl.t) ~(decl_name : string) :
    string =
  let sanitized =
    String.map
      (fun c ->
        if c = '.' || c = ' ' || c = '/' then
          '_'
        else
          c)
      decl_name
  in
  let debug_file = "debug_" ^ sanitized ^ ".txt" in
  let oc = open_out debug_file in
  let debug_ppf = Format.formatter_of_out_channel oc in
  let module DeclLogger = (val Ctx.logger ctx : Ctx.LOGGER) in
  let saved_reporter = Logs.reporter () in
  let saved_level = Logs.level () in
  Logs.set_reporter (DeclLogger.reporter debug_ppf);
  Logs.set_level (Some Logs.Debug);
  Ctx.reset_session ctx;
  (try ignore (Check.check ctx decl) with _ -> ());
  Format.pp_print_flush debug_ppf ();
  close_out oc;
  Logs.set_reporter saved_reporter;
  Logs.set_level saved_level;
  debug_file

(** Temporarily override the global log level for a single decl whose
    name matches [NYAYA_DECL_DEBUG].  The old level is returned so the
    caller can restore it. *)
let override_level_for_debug decl_name =
  let prev_level = Logs.level () in
  (match Sys.getenv_opt "NYAYA_DECL_DEBUG" with
  | Some target when String.equal target decl_name ->
    Logs.set_level (Some Logs.Debug)
  | _ -> ());
  prev_level

let typecheck_one (ctx : Ctx.t) (root_logger : (module Ctx.LOGGER))
    (name : Name.t) (decl : Decl.t) ~success =
  let decl_name = CCFormat.to_string Name.pp name in
  let decl_logger = mk_decl_logger decl_name in
  let module DeclLogger = (val decl_logger : Ctx.LOGGER) in
  Ctx.set_logger ctx decl_logger;
  let prev_level = override_level_for_debug decl_name in
  Ctx.reset_session ctx;
  let info = Decl.get_decl_info decl in
  if not (Check.well_posed ctx info) then
    DeclLogger.err "Declaration %a is not well-posed"
      (Check.Not_well_posed "declaration not well-posed")
      Name.pp name
  else
    DeclLogger.success "Declaration %a is well-posed." Name.pp name;
  (try
     if Check.check ctx decl then begin
       DeclLogger.success "Type checked decl %a." Name.pp (Decl.get_name decl);
       incr success
     end
   with exn when is_checker_failure exn ->
     let module L = (val root_logger : Ctx.LOGGER) in
     L.success "Failed after checking %d declarations in environment." !success;
     let debug_file = replay_debug ctx decl ~decl_name in
     L.info "Debug trace written to %s" debug_file;
     L.err "typecheck: failed on %a: %s"
       (Checker.TypeError ("typecheck: " ^ Printexc.to_string exn))
       Name.pp name (Printexc.to_string exn));
  Logs.set_level prev_level

let typecheck (env : Env.t) : unit =
  let root_logger : (module Ctx.LOGGER) =
    (module Logger : Nyaya_parser.Util.LOGGER)
  in
  let ctx = Ctx.create env ~logger:root_logger in
  let success = ref 0 in
  Iter.iter2
    (fun n d -> typecheck_one ctx root_logger n d ~success)
    (Env.to_iter env);
  Logger.success "Successfully checked %d declarations in environment." !success
