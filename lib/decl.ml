type decl_info = {
  name: Name.t;
  uparams: Level.t list;
  ty: Expr.t;
}
[@@deriving show]

type hint =
  | Abbrev
  | Opaque
  | Reg of int
[@@deriving show]

type rule = {
  ctor_name: Name.t;
  ctor_num_args: int;
  value: Expr.t;
}
[@@deriving show]

type t =
  | Axiom of decl_info
  | Def of {
      info: decl_info;
      value: Expr.t;
      red_hint: hint;
    }
  | Thm of {
      info: decl_info;
      value: Expr.t;
    }
  | Opaque of {
      info: decl_info;
      value: Expr.t;
    }
  | Quot of { info: decl_info }
  | Inductive of {
      info: decl_info;
      is_recursive: bool;
      num_params: int;
      num_idx: int;
      all_names: Name.t list;
      ctor_names: Name.t list;
    }
  | Ctor of {
      info: decl_info;
      name: Name.t;
      num_params: int;
      num_fields: int;
    }
  | Rec of {
      info: decl_info;
      num_params: int;
      num_idx: int;
      num_motives: int;
      num_minors: int;
      rules: rule list;
      is_K: bool;
    }
[@@deriving show]
