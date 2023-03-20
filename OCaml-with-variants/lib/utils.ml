(** Copyright 2021-2022, Mihail Beloshapkin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Infer
open Inter
open Parser
open Inter.Interpreter.ContextData

let print_t = print_sig

let print_result_of_inference = function
  | Result.Ok res ->
    Caml.Format.printf "\nType: ";
    print_t res
  | Result.Error e -> pp_error e
;;

let get_exp_type = function
  | Exp_fun _ as f -> Func (IdMap.empty, f)
  | Exp_literal (Int i) -> Int i
  | Exp_literal (Float f) -> Float f
  | _ -> Undefined
;;

let rec output = function
  | Ok value -> print_value value
  | Error msg -> Caml.Format.printf "impossible to print: %s " msg

and print_value = function
  | Interpreter.ContextData.Int i -> Caml.Format.printf "%i " i
  | Interpreter.ContextData.Float f -> Caml.Format.printf "%f " f
  | Interpreter.ContextData.String s -> Caml.Format.printf "%s " s
  | Interpreter.ContextData.Bool b -> Caml.Format.printf "%b " b
  | Interpreter.ContextData.Func _ -> Caml.Format.printf "Function"
  | Interpreter.ContextData.Polyvar (name, data) ->
    Caml.Format.printf "%s (" name;
    print_polyvar data;
    Caml.Format.printf ")"
  | _ -> Caml.Format.printf "FAILED"

and print_polyvar = function
  | head :: t ->
    print_value head;
    print_polyvar t
  | _ -> ()
;;

let read_next prev =
  let input = read_line () in
  input ^ prev
;;

(** Load source code from the file. *)
let get_file filename =
  let current_channel = open_in filename in
  let data = really_input_string current_channel (in_channel_length current_channel) in
  close_in current_channel;
  data
;;

open Caml.Format

let read_code_from_file path =
  let declaration_list = path |> get_file |> parse_several_declarations in
  match declaration_list with
  | Result.Ok funcs -> funcs
  | Result.Error msg ->
    printf "Incorrect file content: %s" msg;
    []
;;

let read_several_declarations code =
  let declaration_list = code |> parse_several_declarations in
  match declaration_list with
  | Result.Ok funcs ->
    (*List.fold ~f:(fun acc -> function Declaration (_, name, expr) -> (name, get_exp_type expr) :: acc | _ -> acc) ~init:[] funcs*)
    funcs
  | Result.Error msg ->
    printf "Incorrect file content: %s" msg;
    []
;;

let to_ctx top_level_list =
  let new_ctx_list =
    List.fold
      ~f:
        (fun acc -> function
          | Declaration (_, name, expr) -> (name, get_exp_type expr) :: acc
          | _ -> acc)
      ~init:[]
      top_level_list
  in
  Interpreter.add_list_to_env IdMap.empty new_ctx_list
;;
