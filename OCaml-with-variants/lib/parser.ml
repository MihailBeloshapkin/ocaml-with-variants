(** Copyright 2021-2022, Mihail Beloshapkin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Caml.Format
open Ast

let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let integer = take_while1 is_digit

exception Syntax_error of string

module FloatNumParser = struct
  let sign =
    peek_char
    >>= function
    | Some '-' -> advance 1 >>| fun () -> "-"
    | Some '+' -> advance 1 >>| fun () -> "+"
    | Some c when is_digit c -> return "+"
    | _ -> fail "Sign or digit expected"
  ;;

  let number =
    sign
    >>= fun sign ->
    take_while1 is_digit
    <* char '.'
    >>= fun whole ->
    take_while1 is_digit
    >>= fun part -> return (String.concat "" [ sign; whole; "."; part ])
  ;;
end

module OCamlParser = struct
  open Base

  let keywords_list = [ "let"; "in"; "rec"; "if"; "fun"; "then"; "else"; "match"; "with" ]
  let is_keyword id = List.exists ~f:(fun s -> String.equal s id) keywords_list
  let token_separator = take_while is_whitespace

  let is_small_letter = function
    | 'a' .. 'z' -> true
    | _ -> false
  ;;

  let is_big_letter = function
    | 'A' .. 'Z' -> true
    | _ -> false
  ;;

  let is_letter l = is_small_letter l || is_big_letter l

  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false
  ;;

  let space = take_while is_whitespace
  let space1 = take_while1 is_whitespace
  let token s = space *> string s

  (* CHANGED!!!!!! *)
  module Literals = struct
    let int_token =
      space *> take_while1 is_digit
      >>= fun res -> return @@ Exp_literal (Int (int_of_string res))
    ;;

    let float_token =
      space *> FloatNumParser.number
      >>= fun res -> return @@ Exp_literal (Float (float_of_string res))
    ;;

    let string_token =
      space *> char '"' *> take_while (fun c -> not (Char.equal c '"'))
      <* char '"'
      >>= fun res -> return @@ Exp_literal (String res)
    ;;

    let true_token = token "true" >>= fun _ -> return @@ Exp_literal (Bool true)
    let false_token = token "false" >>= fun _ -> return @@ Exp_literal (Bool false)
  end

  module BinOperators = struct
    let ar_operators =
      choice
        [ token "+."
        ; token "-."
        ; token "*."
        ; token "/."
        ; token "+"
        ; token "-"
        ; token "/"
        ; token "*"
        ]
    ;;

    let log_operators = choice [ token "&&"; token "||" ]
    let compare_operators = choice [ token ">"; token "<"; token "=" ]
    let binops = choice [ ar_operators; log_operators; compare_operators ]
  end

  let new_ident =
    space *> take_while1 is_small_letter
    >>= fun str ->
    if is_keyword str then fail "Keyword in the wrong place of program" else return str
  ;;

  let int_number = take_while1 is_digit >>= fun s -> return @@ int_of_string s

  let rec fun_constructor args body =
    match args with
    | [ a ] -> Exp_fun (a, body)
    | h :: t -> Exp_fun (h, fun_constructor t body)
    | _ -> raise (Syntax_error "No args!")
  ;;

  let let_binding_constructor name arg_list body next_ex =
    match arg_list with
    | [] -> Exp_letbinding (NonRec, name, body, next_ex)
    | _ ->
      let func = fun_constructor arg_list body in
      Exp_letbinding (NonRec, name, func, next_ex)
  ;;

  let binop_constructor op1 operator op2 =
    match operator with
    | "+" -> Exp_binop (AddInt, op1, op2)
    | "-" -> Exp_binop (SubInt, op1, op2)
    | "*" -> Exp_binop (MulInt, op1, op2)
    | "/" -> Exp_binop (DivInt, op1, op2)
    | "+." -> Exp_binop (AddFloat, op1, op2)
    | "-." -> Exp_binop (SubFloat, op1, op2)
    | "*." -> Exp_binop (MulFloat, op1, op2)
    | "/." -> Exp_binop (DivFloat, op1, op2)
    | "&&" -> Exp_binop (And, op1, op2)
    | "||" -> Exp_binop (Or, op1, op2)
    | "<" -> Exp_binop (LeqInt, op1, op2)
    | ">" -> Exp_binop (GeqInt, op1, op2)
    | "=" -> Exp_binop (EqInt, op1, op2)
    | _ -> raise (Syntax_error "Parsing error")
  ;;

  let ident_parser = new_ident >>= fun res -> return @@ Exp_ident res
  let unit_parser = token "()" >>= fun _ -> return @@ Exp_unit
  let if_then_else_constructor cond b1 b2 = Exp_ifthenelse (cond, b1, b2)

  let literal_parser =
    choice
      [ Literals.float_token
      ; Literals.int_token
      ; Literals.string_token
      ; Literals.true_token
      ; Literals.false_token
      ]
  ;;

  let arg_of_application = choice [ ident_parser; literal_parser ]

  type dispatch =
    { e : dispatch -> exps t
    ; d : dispatch -> exps t
    }

  let example =
    many (char 'a')
    >>= fun list_a ->
    many (char 'b')
    >>= fun list_b ->
    if List.length list_a = List.length list_b
    then many (char 'c') >>= fun list_c -> return (List.length list_b = List.length list_c)
    else return false
  ;;

  let type_d =
    let letbinding_parser d =
      fix
      @@ fun _ ->
      lift4
        (fun name args body next -> let_binding_constructor name args body next)
        (token "let" *> space1 *> new_ident)
        (many (space1 *> new_ident))
        (space *> token "=" *> space *> d.e d <* space <* token "in")
        (d.e d)
      <?> "letb"
    in
    let lambda_parser d =
      fix
      @@ fun _ ->
      let body =
        lift2
          fun_constructor
          (token "fun" *> many (space1 *> new_ident) <* space <* token "->")
          (d.e d)
      in
      token "(" *> body <* token ")" <?> "lambda"
    in
    let polyvar_parser d =
      fix
      @@ fun _ ->
      let name =
        space *> take_while1 is_big_letter
        >>= fun part1 -> take_while is_letter >>= fun part2 -> return @@ part1 ^ part2
      in
      let constructor =
        space *> char '(' *> space *> d.e d
        >>= fun first ->
        many (char ',' *> d.e d) <* char ')' >>= fun next -> return @@ (first :: next)
      in
      space
      *> lift2
           (fun name expr_list -> Exp_polyvar (name, expr_list))
           name
           (option [] constructor)
    in
    let app_parser d =
      let app_arg_p =
        space1
        *> (lambda_parser d <|> polyvar_parser d <|> ident_parser <|> literal_parser)
      in
      fix
      @@ fun _ ->
      choice
        [ char '(' *> d.e d <* space <* char ')'
        ; (ident_parser
          <|> lambda_parser d
          >>= fun f ->
          many1 (app_arg_p <|> (space1 *> char '(' *> d.e d <* space <* char ')'))
          >>= fun args -> return @@ Exp_apply (f, args))
        ]
      <* space
      <?> "app_parser"
    in
    let binop_parser d =
      fix
      @@ fun _ ->
      let c =
        choice
          [ char '(' *> d.e d <* char ')'; app_parser d; ident_parser; literal_parser ]
      in
      lift3
        binop_constructor
        (space *> c <* space)
        BinOperators.binops
        (space *> c <* space)
    in
    let ifthelse_parser d =
      fix
      @@ fun _ ->
      lift3
        (fun cond fbranch sbranch -> if_then_else_constructor cond fbranch sbranch)
        (token "if" *> d.e d <* token "then")
        (d.e d)
        (token "else" *> d.e d)
    in
    let match_parser d =
      fix
      @@ fun _ ->
      let case_parser =
        space *> token "|" *> space *> d.e d
        <* space
        <* token "->"
        >>= fun left -> d.e d >>= fun right -> return @@ (left, right)
      in
      lift2
        (fun e cases -> Exp_match (e, cases))
        (token "match" *> d.e d <* token "with" <* space)
        (many1 case_parser)
    in
    let e d =
      letbinding_parser d
      <|> polyvar_parser d
      <|> ifthelse_parser d
      <|> match_parser d
      <|> binop_parser d
      <|> app_parser d
      <|> lambda_parser d
      <|> literal_parser
      <|> ident_parser
      <* space
      <?> "general expr parser"
    in
    let d d = app_parser d in
    { e; d }
  ;;

  let e_p = type_d.e type_d
  let d_p = type_d.d type_d

  let hl_fun_decl =
    lift3
      (fun (is_rec, name) b c -> Declaration (is_rec, name, fun_constructor b c))
      (space
       *> token "let"
       *> option NonRec (space1 *> string "rec" >>= fun _ -> return @@ Rec)
      >>= fun is_rec -> space1 *> new_ident >>= fun name -> return (is_rec, name))
      (many1 (space1 *> new_ident))
      (space *> token "=" *> space *> e_p <* space <* string ";;" <* space)
  ;;

  let p = choice [ hl_fun_decl; (e_p >>= fun res -> return @@ Application res) ]
  let parse_declarations = many hl_fun_decl
end

module Printer = struct
  open Base

  let print_let = function
    | name, Int i -> printf "Name: %s; Val: %i\n" name i
    | name, Float i -> printf "Name: %s; Val: %f\n" name i
    | name, String i -> printf "Name: %s; Val: %s\n" name i
    | name, Bool i ->
      printf "Name: %s; Val: " name;
      print_bool i
  ;;

  let print_literal = function
    | Int i -> printf "(Int: %i)" i
    | Float f -> printf "(Float: %f)" f
    | String s -> printf "(String: %s)" s
    | Bool i ->
      printf "(Bool:";
      print_bool i;
      printf ")"
  ;;

  let rec print_ast = function
    | Exp_letbinding (_, id, value, _) ->
      printf "(LetB: Name=%s value=" id;
      print_ast value;
      printf ")"
    | Exp_literal l -> print_literal l
    | Exp_ident i -> printf "(Ident: %s)" i
    | Exp_fun (arg, e) ->
      printf "(Fun: arg=%s" arg;
      print_ast e;
      printf ")"
    | Exp_seq (e1, e2) ->
      printf "Seq (";
      print_ast e1;
      print_ast e2;
      printf ")"
    | Exp_apply (f, arg_list) ->
      printf "(Apply:";
      print_ast f;
      List.iter ~f:print_ast arg_list;
      printf ")"
    | Exp_binop (_, l, r) ->
      printf "Binop(";
      print_ast l;
      print_ast r;
      printf ")"
    | Exp_ifthenelse (c, b1, b2) ->
      printf "IfThenElse(";
      print_ast c;
      printf ",";
      print_ast b1;
      printf ",";
      print_ast b2;
      printf ")"
    | Exp_match (e, cases) ->
      printf "match (";
      print_ast e;
      cases
      |> List.iter ~f:(fun (left, right) ->
           print_ast left;
           print_ast right);
      printf ")"
    | Exp_polyvar (name, exp_list) ->
      printf "Polyvar (";
      printf "Name: %s" name;
      exp_list |> List.iter ~f:print_ast;
      printf ")"
    | _ -> printf "Unrecognised Ast Node"
  ;;
end

let parse_exp code =
  let result = Angstrom.parse_string OCamlParser.p ~consume:Angstrom.Consume.All code in
  result
;;

let parse_several_declarations code =
  let result =
    Angstrom.parse_string
      OCamlParser.parse_declarations
      ~consume:Angstrom.Consume.All
      code
  in
  result
;;

let print_result = function
  | Result.Ok res -> Printer.print_ast res
  | Result.Error s -> printf "SOMETHING WENT WRONG: %s\n" s
;;

let p2 = parse_several_declarations "let f x = x + 1;; let f x = x - 1;;"

let%test _ =
  match p2 with
  | Result.Ok _ -> true
  | _ -> false
;;

let p2 = parse_exp "if a then b + k else c"

let%test _ =
  p2
  = Result.Ok
      (Application
         (Exp_ifthenelse
            ( Exp_ident "a"
            , Exp_binop (AddInt, Exp_ident "b", Exp_ident "k")
            , Exp_ident "c" )))
;;

let p2 =
  parse_exp
    {|
    let func x = 
      match x with 
      | A (0) -> 0
      | B (1, 4) -> 1
    ;;
  |}
;;

let p2 =
  parse_exp
    {|
    let func x = 
      match x with 
      | A (0) -> C
      | B (1, 4) -> D
    ;;
  |}
;;

let rec fix f = f (fix f)
let p2 = parse_exp "fact "

let%test _ = p2 = Result.Ok (Application (Exp_ident "fact"))

let p2 = parse_exp "a = 1"
let p2 = parse_exp "let rec fact n = if n < 2 then 1 else n * (fact (n - 1));;"
let p2 = parse_exp "4 + 2"

let%test _ =
  p2
  = Result.Ok (Application (Exp_binop (AddInt, Exp_literal (Int 4), Exp_literal (Int 2))))
;;

let p2 = parse_exp "abc - asdf "

let%test _ =
  p2 = Result.Ok (Application (Exp_binop (SubInt, Exp_ident "abc", Exp_ident "asdf")))
;;

let p2 = parse_exp "(fun x -> x + 1)"

let%test _ =
  p2
  = Result.Ok
      (Application (Exp_fun ("x", Exp_binop (AddInt, Exp_ident "x", Exp_literal (Int 1)))))
;;

let p2 = parse_exp "(fun x -> x + 1) 5 "

let%test _ =
  p2
  = Result.Ok
      (Application
         (Exp_apply
            ( Exp_fun ("x", Exp_binop (AddInt, Exp_ident "x", Exp_literal (Int 1)))
            , [ Exp_literal (Int 5) ] )))
;;

let p2 = parse_exp "match x with | a b -> 1"
let p2 = parse_exp "(f a) + (g b)"

let%test _ =
  p2
  = Result.Ok
      (Application
         (Exp_binop
            ( AddInt
            , Exp_apply (Exp_ident "f", [ Exp_ident "a" ])
            , Exp_apply (Exp_ident "g", [ Exp_ident "b" ]) )))
;;

let p2 = parse_exp "a + 2"

let%test _ =
  p2 = Result.Ok (Application (Exp_binop (AddInt, Exp_ident "a", Exp_literal (Int 2))))
;;

let p2 = parse_exp "fact 5 "

let%test _ =
  p2 = Result.Ok (Application (Exp_apply (Exp_ident "fact", [ Exp_literal (Int 5) ])))
;;

let p2 = parse_exp "1.5 +. 2.3  \n\n"

let%test _ =
  p2
  = Result.Ok
      (Application
         (Exp_binop (AddFloat, Exp_literal (Float 1.5), Exp_literal (Float 2.3))))
;;

let p2 = parse_exp "a < b"

let%test _ =
  p2 = Result.Ok (Application (Exp_binop (LeqInt, Exp_ident "a", Exp_ident "b")))
;;

let p2 = parse_exp "a > 1.0"

let%test _ =
  p2
  = Result.Ok (Application (Exp_binop (GeqInt, Exp_ident "a", Exp_literal (Float 1.0))))
;;

let p2 = parse_exp "let func x = match x with | 0 -> 1 | 1 -> 3;;"

let%test _ =
  p2
  = Result.Ok
      (Declaration
         ( NonRec
         , "func"
         , Exp_fun
             ( "x"
             , Exp_match
                 ( Exp_ident "x"
                 , [ Exp_literal (Int 0), Exp_literal (Int 1)
                   ; Exp_literal (Int 1), Exp_literal (Int 3)
                   ] ) ) ))
;;

let p2 = parse_exp "let incr x = x + 1;;"

(*
let%test _ =
  match p2 with
  | Result.Error m ->
    printf "Error: %s" m;
    false
  | Result.Ok (Declaration (_, name, r)) ->
    printf "Name: %s" name;
    Printer.print_ast r;
    true
  | Result.Ok (Application r) ->
    Printer.print_ast r;
    true
;;
*)

let p2 = parse_exp "let rec fix f x = f (fix f) x;;"

let%test _ =
  p2
  = Result.Ok
      (Declaration
         ( Rec
         , "fix"
         , Exp_fun
             ( "f"
             , Exp_fun
                 ( "x"
                 , Exp_apply
                     ( Exp_ident "f"
                     , [ Exp_apply (Exp_ident "fix", [ Exp_ident "f" ]); Exp_ident "x" ]
                     ) ) ) ))
;;

let p2 = parse_exp "a = incr 30"

let%test _ =
  p2
  = Result.Ok
      (Application
         (Exp_binop
            (EqInt, Exp_ident "a", Exp_apply (Exp_ident "incr", [ Exp_literal (Int 30) ]))))
;;

let p2 = parse_exp "let f x = if x = 1 then 30 else x ;;"

let%test _ =
  p2
  = Result.Ok
      (Declaration
         ( NonRec
         , "f"
         , Exp_fun
             ( "x"
             , Exp_ifthenelse
                 ( Exp_binop (EqInt, Exp_ident "x", Exp_literal (Int 1))
                 , Exp_literal (Int 30)
                 , Exp_ident "x" ) ) ))
;;

let p2 = parse_exp "let f x = let a = 10 in a;;"

let p2 =
  parse_exp
    {|
    let matcher x = 
      let t = 
        match x with 
        | 0 -> 1 
        | 2 -> 0 
        | 5 -> fact 5 
        | v -> (incr v) * 30
      in 
      t
    ;; 
    |}
;;

let%test _ =
  match p2 with
  | Result.Ok _ -> true
  | _ -> false
;;
