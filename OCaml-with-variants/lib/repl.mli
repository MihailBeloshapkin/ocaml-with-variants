(** Copyright 2021-2022, Mihail Beloshapkin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Try infer type of expression *)
val try_infer : Ast.exps -> (Infer.TypedTree.typ, Infer.error) result

(** Infer top level expression (declarations and applications) *)
val try_infer_top_level
  :  Ast.top_level_expressions
  -> (Infer.TypedTree.typ, Infer.error) result

(** Infer list of several declarations *)
val infer_declaration_list : Ast.top_level_expressions list -> Infer.TypeEnv.t
