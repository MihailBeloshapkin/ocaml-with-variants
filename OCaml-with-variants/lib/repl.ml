(** Copyright 2021-2022, Mihail Beloshapkin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Infer

let try_infer e = infer e TypeEnv.empty
let try_infer_top_level = infer_top_level_expressions TypeEnv.empty

let infer_declaration_list =
  let rec helper env = function
    | Declaration (ext_type, name, decl) :: tail ->
      let typed =
        Infer.infer_top_level_expressions env (Declaration (ext_type, name, decl))
      in
      let env =
        match typed with
        | Result.Ok t -> extent_type_env env name t
        | _ -> env
      in
      helper env tail
    | _ :: tail -> helper env tail
    | _ -> env
  in
  helper TypeEnv.empty
;;
