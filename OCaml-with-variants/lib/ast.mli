(** Copyright 2021-2022, Mihail Beloshapkin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type ident = string

type exp_type =
  | Rec (** Recursive functions *)
  | NonRec (** non recursive functions *)

type literal =
  | Int of int (** Int literal *)
  | Float of float (** Float literal *)
  | String of string (** String literal *)
  | Bool of bool (** Bool literal *)

(** built-in binary operators *)
type binop =
  | AddInt (** Integer addition *)
  | SubInt (** Integer subtraction *)
  | MulInt (** Integer multiplication *)
  | DivInt (** Integer division *)
  | AddFloat (** Float addition *)
  | SubFloat (** Float subtraction *)
  | MulFloat (** Float multiplication *)
  | DivFloat (** Float division *)
  | And (** And operation *)
  | Or (** Or operation *)
  | LeqInt (** Less operation *)
  | GeqInt (** More operation *)
  | EqInt (** Equal operation *)

type exps =
  | Exp_fun of ident * exps (** Function: arg + body *)
  | Exp_letbinding of exp_type * ident * exps * exps
      (** Let binding: name + value + next exp *)
  | Exp_ident of ident (** Identifier *)
  | Exp_literal of literal (** Literal *)
  | Exp_seq of exps * exps (** Sequence of expressions *)
  | Exp_apply of exps * exps list (** Apply function expression *)
  | Exp_binop of binop * exps * exps (** Built-in binary operation *)
  | Exp_ifthenelse of exps * exps * exps (** Ifhtenelse expression: condition and cases *)
  | Exp_match of exps * case list (** Matching expresiion: exp and cases *)
  | Exp_polyvar of ident * exps list (** Polymorphic variant *)
  | Exp_unit (** Unit *)

and case = exps * exps

type declaration = exp_type * string * exps

type top_level_expressions =
  | Declaration of declaration (** Function declaration *)
  | Application of exps (** Function application (or enother type of expressions) *)
