(** Copyright 2021-2022, Mihail Beloshapkin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ocaml_with_var.Repl
open Ocaml_with_var.Infer
open Ocaml_with_var.Inter
open Ocaml_with_var.Parser
open Ocaml_with_var.Utils

let try_infer e = infer e TypeEnv.empty

let run_repl file =
  let ctx = read_code_from_file file in
  let env = infer_declaration_list ctx in
  let ctx = to_ctx ctx in
  Caml.Format.printf "\n> ";
  let s = Stdio.In_channel.input_all Caml.stdin in
  match parse_exp s with
  | Result.Ok exp_ast ->
    (match exp_ast with
     | Declaration _ -> Format.printf "Application expected"
     | Application exp as appl ->
       let open Caml.Format in
       let typed = infer_top_level_expressions env appl in
       let result = Interpreter.eval ctx exp in
       printf "Value: ";
       output result;
       print_result_of_inference typed)
  | Error msg -> Format.printf "Some error: %s" msg
;;

let () =
  let open Caml.Format in
  printf "\nInput file path:";
  let file = Stdio.In_channel.input_all Caml.stdin in
  run_repl file
;;
