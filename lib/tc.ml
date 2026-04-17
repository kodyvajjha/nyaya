(* Thin façade over the refactored checker.

   Historically [Tc] held the entire checker in one file.  Its public
   surface — [typecheck] plus the checker's exceptions — is preserved
   so callers ([bin/main.ml], [test/test_main.ml]) don't change.

   The real implementation lives in:
     - [Fresh]   : instance-based fvar-id counter
     - [Memo]    : typed memoisation wrapper
     - [Trace]   : instance-based hierarchical tracer
     - [Ctx]     : the single home for operational state
     - [Reduce]  : β / δ / ι / nat-builtin / string-lit reductions
     - [Checker] : [infer], [whnf], [is_def_eq] (mutual core)
     - [Check]   : per-decl well-posedness and typing
     - [Driver]  : the [typecheck] loop and auto-debug replay
*)

exception TypeError = Checker.TypeError

exception Defeq_failure = Checker.Defeq_failure

exception Not_well_posed = Check.Not_well_posed

let typecheck = Driver.typecheck
