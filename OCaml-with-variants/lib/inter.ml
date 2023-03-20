(** Copyright 2021-2022, Mihail Beloshapkin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

exception Evaluarion_error of string

module IdMap = Map.Make (struct
  type t = string

  let compare = compare
end)

module I = struct
  include Base.Result

  let ( let* ) x f = x >>= f

  let run x ~ok ~err =
    match x with
    | Ok v -> ok v
    | Error e -> err e
  ;;
end

module Interpreter = struct
  open I
  open Base

  module ContextData = struct
    type t =
      | Int of int
      | Float of float
      | String of string
      | Bool of bool
      | Func of context * exps
      | Polyvar of string * t list
      | Undefined

    and context = t IdMap.t
  end

  let get_val_from_ctx ctx name =
    List.find_exn ~f:(fun (n, _) -> String.equal n name) ctx |> snd
  ;;

  let get_fun_args_and_body f =
    let rec helper exp acc =
      match exp with
      | Exp_fun (name, next) -> helper next (name :: acc)
      | _ as body -> acc |> List.rev, body
    in
    helper f []
  ;;

  let eval_binop op l r =
    let open ContextData in
    let open Caml in
    match op, l, r with
    | AddInt, Int x, Int y -> return (Int (x + y))
    | SubInt, Int x, Int y -> return (Int (x - y))
    | MulInt, Int x, Int y -> return (Int (x * y))
    | DivInt, Int x, Int y -> return (Int (x / y))
    | AddFloat, Float x, Float y -> return (Float (x +. y))
    | SubFloat, Float x, Float y -> return (Float (x -. y))
    | MulFloat, Float x, Float y -> return (Float (x *. y))
    | DivFloat, Float x, Float y -> return (Float (x /. y))
    | EqInt, Int x, Int y -> return (Bool (x = y))
    | LeqInt, Int x, Int y -> return (Bool (x < y))
    | GeqInt, Int x, Int y -> return (Bool (x > y))
    | _ -> fail "Unrecognised operation"
  ;;

  let rec pop_func_arguments n = function
    | Exp_fun (_, exp) when n > 0 -> pop_func_arguments (n - 1) exp
    | _ as exp -> exp
  ;;

  let add_to_env name value env = IdMap.add name value env

  let add_list_to_env env =
    List.fold ~f:(fun env (name, value) -> add_to_env name value env) ~init:env
  ;;

  let get_from_env name env = IdMap.find name env

  let union_env env1 env2 =
    IdMap.fold (fun name value acc -> add_to_env name value acc) env1 env2
  ;;

  let rec eval ctx = function
    | Exp_literal (Int i) -> return (ContextData.Int i)
    | Exp_literal (Float f) -> return (ContextData.Float f)
    | Exp_literal (String s) -> return (ContextData.String s)
    | Exp_polyvar (name, exp_list) ->
      let constructor_data =
        exp_list
        |> List.map ~f:(fun arg ->
             match eval ctx arg with
             | Ok data -> data
             | _ -> raise @@ Evaluarion_error "Cant evaluate polyvar constructor")
      in
      return (ContextData.Polyvar (name, constructor_data))
    | Exp_fun _ as f -> return (ContextData.Func (IdMap.empty, f))
    | Exp_binop (binop, left, right) ->
      let* l_evaluated = eval ctx left in
      let* r_evaluated = eval ctx right in
      let* res = eval_binop binop l_evaluated r_evaluated in
      return res
    | Exp_ident id -> return @@ get_from_env id ctx
    | Exp_letbinding (_, id, ex, next) ->
      let* _, updated_ctx = eval_let ctx (id, ex) in
      return updated_ctx >>= fun s -> eval s next
    | Exp_apply (f, args) ->
      let* func = eval ctx f in
      let* result =
        match func with
        | ContextData.Func (fun_ctx, e) ->
          let arg_names, body = get_fun_args_and_body e in
          let arg_values =
            args
            |> List.map ~f:(fun arg ->
                 match eval ctx arg with
                 | Ok data -> data
                 | _ -> raise @@ Evaluarion_error "Cant evaluate function argument")
          in
          (match List.length arg_names, List.length arg_values with
           | l1, l2 when l1 = l2 ->
             let new_ctx_list = List.zip_exn arg_names arg_values in
             let new_ctx = add_list_to_env IdMap.empty new_ctx_list in
             eval (union_env new_ctx ctx |> union_env fun_ctx) body
           | _, l2 ->
             let new_func = pop_func_arguments l2 e in
             let new_ctx_list = List.zip_exn (List.take arg_names l2) arg_values in
             let new_ctx = add_list_to_env IdMap.empty new_ctx_list in
             let updated_ctx = union_env new_ctx ctx |> union_env fun_ctx in
             return (ContextData.Func (updated_ctx, new_func)))
        | _ -> fail "Error: function expected"
      in
      return result
    | Exp_ifthenelse (cond, f_branch, s_branch) ->
      let* cond_eval = eval ctx cond in
      let result =
        match cond_eval with
        | ContextData.Bool true -> eval ctx f_branch
        | ContextData.Bool false -> eval ctx s_branch
        | _ -> fail "oh"
      in
      result
    | Exp_match (e, cases) ->
      let* exp_value = eval ctx e in
      eval_match exp_value ctx cases
    | _ -> fail "not impl"

  and eval_let ctx (id, ex) =
    let* computed_val = eval ctx ex in
    let updated_ctx = add_to_env id computed_val ctx in
    return (computed_val, updated_ctx)

  and eval_match value ctx = function
    | (Exp_ident id, right) :: _ ->
      let ctx = add_to_env id value ctx in
      eval ctx right
    | (Exp_polyvar (name0, list0), right) :: tail ->
      (match value with
       | Polyvar (name1, list1) when String.equal name0 name1 ->
         let* is_eq, new_ctx = compare_polyvar_struct ctx list1 list0 in
         if is_eq then eval new_ctx right else eval_match value ctx tail
       | _ -> eval_match value ctx tail)
    | (left, right) :: tail ->
      let* computed_left = eval ctx left in
      let* is_equal, new_ctx = compare_values ctx value computed_left in
      if is_equal then eval new_ctx right else eval_match value ctx tail
    | _ -> fail "sorry, no variants"

  and compare_values ctx first second =
    let open ContextData in
    let open Caml in
    match first, second with
    | Int a, Int b -> return (a = b, ctx)
    | Float a, Float b -> return (a = b, ctx)
    | String s1, String s2 -> return (String.equal s1 s2, ctx)
    | Bool b1, Bool b2 -> return (b1 = b2, ctx)
    | _ -> return (false, ctx)

  and compare_polyvars ctx l1 l2 =
    match l1, l2 with
    | head :: t1, Exp_ident name :: t2 ->
      let* head_value = eval ctx head in
      let ctx = add_to_env name head_value ctx in
      compare_polyvars ctx t1 t2
    | h1 :: t1, h2 :: t2 ->
      let* value_h1 = eval IdMap.empty h1 in
      let* value_h2 = eval IdMap.empty h2 in
      let* is_equal, new_ctx = compare_values ctx value_h1 value_h2 in
      if is_equal then compare_polyvars new_ctx t1 t2 else return (false, ctx)
    | [], [] -> return (true, ctx)
    | _ -> return (false, ctx)

  and compare_polyvar_struct ctx (computed : ContextData.t list) matched =
    let open Caml in
    match computed, matched with
    | Int x :: tail1, Exp_literal (Int y) :: tail2 when x = y ->
      compare_polyvar_struct ctx tail1 tail2
    | Float x :: tail1, Exp_literal (Float y) :: tail2 when x = y ->
      compare_polyvar_struct ctx tail1 tail2
    | Bool x :: tail1, Exp_literal (Bool y) :: tail2 when x = y ->
      compare_polyvar_struct ctx tail1 tail2
    | String x :: tail1, Exp_literal (String y) :: tail2 when x = y ->
      compare_polyvar_struct ctx tail1 tail2
    | Polyvar (name1, list1) :: tail1, Exp_polyvar (name2, list2) :: tail2
      when name1 = name2 ->
      let* is_eq, new_ctx = compare_polyvar_struct ctx list1 list2 in
      if is_eq then compare_polyvar_struct new_ctx tail1 tail2 else return (false, ctx)
    | value :: tail1, Exp_ident name :: tail2 ->
      let ctx = add_to_env name value ctx in
      compare_polyvar_struct ctx tail1 tail2
    | [], [] -> return (true, ctx)
    | _ -> return (false, ctx)
  ;;
end

let eval = Interpreter.eval

let p =
  Interpreter.eval
    IdMap.empty
    (Exp_binop (AddInt, Exp_literal (Int 30), Exp_literal (Int 30)))
;;

let%test _ = p = Ok (Interpreter.ContextData.Int 60)

let p =
  Interpreter.eval
    IdMap.empty
    (Exp_binop
       ( AddInt
       , Exp_literal (Int 30)
       , Exp_binop (AddInt, Exp_literal (Int 3), Exp_literal (Int 4)) ))
;;

let%test _ = p = Ok (Interpreter.ContextData.Int 37)

let p =
  Interpreter.eval
    (Interpreter.add_to_env "x" (Interpreter.ContextData.Int 3) IdMap.empty
    |> Interpreter.add_to_env "y" (Interpreter.ContextData.Int 4))
    (Exp_binop (AddInt, Exp_ident "x", Exp_ident "y"))
;;

let%test _ = p = Ok (Interpreter.ContextData.Int 7)

let p =
  Interpreter.eval
    (Interpreter.add_to_env
       "f"
       (Interpreter.ContextData.Func
          ( IdMap.empty
          , Exp_fun ("x", Exp_binop (AddInt, Exp_ident "x", Exp_literal (Int 1))) ))
       IdMap.empty)
    (Exp_apply (Exp_ident "f", [ Exp_literal (Int 30) ]))
;;

let%test _ = p = Ok (Interpreter.ContextData.Int 31)

let p =
  Interpreter.eval
    (Interpreter.add_to_env
       "f"
       (Interpreter.ContextData.Func
          ( IdMap.empty
          , Exp_fun
              ( "x"
              , Exp_letbinding
                  ( NonRec
                  , "v"
                  , Exp_literal (Int 1)
                  , Exp_binop (AddInt, Exp_ident "x", Exp_ident "v") ) ) ))
       IdMap.empty)
    (Exp_apply (Exp_ident "f", [ Exp_literal (Int 30) ]))
;;

let%test _ = p = Ok (Interpreter.ContextData.Int 31)

let p =
  Interpreter.eval
    (Interpreter.add_to_env
       "f"
       (Interpreter.ContextData.Func
          ( IdMap.empty
          , Exp_fun
              ( "x"
              , Exp_letbinding
                  ( NonRec
                  , "v"
                  , Exp_literal (Int 1)
                  , Exp_letbinding
                      ( NonRec
                      , "r"
                      , Exp_literal (Int 4)
                      , Exp_binop
                          ( AddInt
                          , Exp_binop (MulInt, Exp_ident "x", Exp_ident "v")
                          , Exp_ident "r" ) ) ) ) ))
       IdMap.empty)
    (Exp_apply (Exp_ident "f", [ Exp_literal (Int 30) ]))
;;

let%test _ = p = Ok (Interpreter.ContextData.Int 34)
