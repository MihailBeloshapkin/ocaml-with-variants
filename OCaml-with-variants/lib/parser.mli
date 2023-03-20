(** Copyright 2021-2022, Mihail Beloshapkin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

val parse_exp : ident -> (top_level_expressions, ident) result
val parse_several_declarations : ident -> (top_level_expressions list, ident) result
