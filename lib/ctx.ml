(* Checker context — the single home for operational state.

   [Ctx.t] bundles:
     - the pure declaration environment ([env]);
     - the current [logger] (swapped per declaration by the driver);
     - a fresh-id generator for free variables;
     - memo tables for [infer] and [whnf];
     - three tracers for [infer], [whnf] and [is_def_eq].

   Core checker code takes a [Ctx.t] and never reaches out to module-
   level refs. All mutation in the typechecker passes through here. *)

module type LOGGER = Nyaya_parser.Util.LOGGER

let truncate ?(max_len = 400) s =
  if String.length s <= max_len then
    s
  else
    String.sub s 0 (max_len - 3) ^ "..."

let expr_summary expr = CCFormat.asprintf "%a" Expr.pp expr |> truncate

let defeq_summary (lhs, rhs) =
  truncate (CCFormat.asprintf "%a =?= %a" Expr.pp lhs Expr.pp rhs)

module Infer_trace = Trace.Make (struct
  type input = Expr.t

  type output = Expr.t

  let kind = "i"

  let elide_ok_env = "NYAYA_INFER_TRACE_ELIDE_OK"

  let max_depth_env = "NYAYA_INFER_MAX_DEPTH"

  let input_summary = expr_summary

  let output_summary = expr_summary
end)

module Whnf_trace = Trace.Make (struct
  type input = Expr.t

  type output = Expr.t

  let kind = "w"

  let elide_ok_env = "NYAYA_WHNF_TRACE_ELIDE_OK"

  let max_depth_env = "NYAYA_WHNF_MAX_DEPTH"

  let input_summary = expr_summary

  let output_summary = expr_summary
end)

module Defeq_trace = Trace.Make (struct
  type input = Expr.t * Expr.t

  type output = bool

  let kind = "d"

  let elide_ok_env = "NYAYA_DEFEQ_TRACE_ELIDE_OK"

  let max_depth_env = "NYAYA_DEFEQ_MAX_DEPTH"

  let input_summary = defeq_summary

  let output_summary = string_of_bool
end)

type t = {
  env: Env.t;
  mutable logger: (module LOGGER);
  fresh: Fresh.t;
  infer_memo: (int, Expr.t) Memo.t;
  whnf_memo: (int, Expr.t) Memo.t;
  infer_trace: Infer_trace.t;
  whnf_trace: Whnf_trace.t;
  defeq_trace: Defeq_trace.t;
}

let create (env : Env.t) ~(logger : (module LOGGER)) : t =
  {
    env;
    logger;
    fresh = Fresh.create ();
    infer_memo = Memo.create 4096;
    whnf_memo = Memo.create 4096;
    infer_trace = Infer_trace.create ();
    whnf_trace = Whnf_trace.create ();
    defeq_trace = Defeq_trace.create ();
  }

let env t = t.env

let logger t = t.logger

let set_logger t logger = t.logger <- logger

(** Allocate a fresh free variable within this context. *)
let fresh_fvar t ~name ~btype ~binfo = Expr.fvar name btype binfo (Fresh.next t.fresh)

(** Reset the per-declaration caches (memos + tracers). Invoked by the
    driver between declarations; the declaration environment and the
    fresh-id counter are left alone. *)
let reset_session t =
  Memo.clear t.infer_memo;
  Memo.clear t.whnf_memo;
  Infer_trace.reset t.infer_trace;
  Whnf_trace.reset t.whnf_trace;
  Defeq_trace.reset t.defeq_trace
