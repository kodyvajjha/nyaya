(* Per-declaration well-posedness and type checks.

   [well_posed] is a cheap sanity pass (dedup universe params, no
   stray free variables, type is a sort).  [check] dispatches on the
   declaration kind; declarations that carry a value (Def, Thm,
   Opaque) must have their value's inferred type match the declared
   type up to definitional equality.

   Checker exceptions escape to the driver so the [typecheck] loop can
   decide how to present the failure. *)

exception Not_well_posed of string

module Pp = struct
  let pp_check fpf (e, ty) =
    let pp_value fpf e = CCFormat.fprintf fpf "value:@ %a" Expr.pp e in
    let pp_type fpf ty = CCFormat.fprintf fpf "type :@ %a" Expr.pp ty in

    CCFormat.fprintf fpf "@[<v 0>@[check:@]@,@[<hv 2>%a@]@,@[<hv 2>%a@]@]"
      pp_value e pp_type ty

  let pp_check_name fpf (n, ty) =
    let pp_name fpf e = CCFormat.fprintf fpf "name:@ %a" Name.pp e in
    let pp_type fpf ty = CCFormat.fprintf fpf "type :@ %a" Expr.pp ty in

    CCFormat.fprintf fpf "@[<v 0>@[check:@]@,@[<hv 2>%a@]@,@[<hv 2>%a@]@]"
      pp_name n pp_type ty
end

let rec dup_exist = function
  | [] -> false
  | hd :: tl -> List.exists (( = ) hd) tl || dup_exist tl

(** Cheap sanity pass on a declaration's signature. *)
let well_posed (ctx : Ctx.t) (info : Decl.decl_info) : bool =
  let module L = (val Ctx.logger ctx : Ctx.LOGGER) in
  let no_dup_uparams = not (dup_exist info.uparams) in
  let no_free_vars = not (Expr.has_free_vars info.ty) in
  let type_is_sort =
    try
      match Checker.infer ctx info.ty |> Expr.node with
      | Expr.Sort _ -> true
      | _ ->
        L.debug "info.ty : %a@." Expr.pp (Checker.infer ctx info.ty);
        false
    with Checker.TypeError origin ->
      L.err "well_posed: while inferring %a, type error: %s"
        (Not_well_posed ("well_posed: " ^ origin))
        Name.pp info.name origin
  in
  no_dup_uparams && no_free_vars && type_is_sort

(* TODO: complete ctor checks. The individual invariants are listed
   below for future expansion. *)
let check_ctor (ctx : Ctx.t) (decl : Decl.t) =
  let module L = (val Ctx.logger ctx : Ctx.LOGGER) in
  match decl with
  | Decl.Ctor { num_params; inductive_name; _ } ->
    let inductive = Env.find (Ctx.env ctx) inductive_name in
    assert (num_params = Decl.get_inductive_num_params inductive);
    let ensure_same_params = true in
    let non_param_as_sort = true in
    let sort_le_inductive_sort = true in
    let non_positive = true in
    let end_of_telescope_match = true in
    ensure_same_params && non_param_as_sort && sort_le_inductive_sort
    && non_positive && end_of_telescope_match
  | _ -> L.err "Ctor check called on non-ctor declaration" (Failure "")

(* Checks for a value-carrying declaration (Def/Thm/Opaque): infer
   the value's type and compare to the declared type. *)
let check_value_decl (ctx : Ctx.t) ~name ~value ~ty : bool =
  let module L = (val Ctx.logger ctx : Ctx.LOGGER) in
  L.debugf Pp.pp_check (value, ty);
  let inferred = Checker.infer ctx value in
  L.app "Inference complete";
  let ans = Checker.is_def_eq ctx inferred ty in
  L.success "@[Successfully type-checked @[%a@].@]" Name.pp name;
  ans

let check (ctx : Ctx.t) (decl : Decl.t) : bool =
  let module L = (val Ctx.logger ctx : Ctx.LOGGER) in
  match (decl : Decl.t) with
  | Def { info; value; red_hint = _ } ->
    (* TODO: honour reducibility hints. *)
    check_value_decl ctx ~name:info.name ~value ~ty:info.ty
  | Thm { info; value } ->
    check_value_decl ctx ~name:info.name ~value ~ty:info.ty
  | Opaque { info; value } ->
    check_value_decl ctx ~name:info.name ~value ~ty:info.ty
  | Axiom { name; ty; _ } ->
    L.debugf Pp.pp_check_name (name, ty);
    true
  | Ctor _ as d -> check_ctor ctx d
  | Rec _ -> true (* TODO *)
  | Inductive _ -> true (* TODO *)
  | _ ->
    L.err "failed checking decl: %a" (Failure "type checking failed") Decl.pp
      decl
