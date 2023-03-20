(** Copyright 2021-2022, Mihail Beloshapkin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Output inference *)
val print_result_of_inference : (Infer.TypedTree.typ, Infer.error) result -> unit

(** Output result of evaluation *)
val output : (Inter.Interpreter.ContextData.t, string) result -> unit

(** Read code *)
val read_code_from_file : string -> Ast.top_level_expressions list

(** Read sevsral delaration from the input string and build ast for each of them *)
val read_several_declarations : string -> Ast.top_level_expressions list

(** Convers top level to interpreter context *)
val to_ctx
  :  Ast.top_level_expressions list
  -> Inter.Interpreter.ContextData.t Inter.IdMap.t
