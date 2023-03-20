(** Copyright 2021-2022, Mihail Beloshapkin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module TypedTree = struct
  open Ast

  module IdMap = Map.Make (struct
    type t = ident

    let compare = compare
  end)

  type value =
    | Vint of int
    | Vfloat of float
    | Vstring of string
    | Vbool of bool
    | Vunit

  type enviroment = value IdMap.t
  type type_num = int

  type base_type =
    | Tunit
    | Tint
    | Tfloat
    | Tstring
    | Tbool

  type typ =
    | TBase of base_type
    | TVar of type_num
    | Arrow of typ * typ

  type scheme = (type_num, Base.Int.comparator_witness) Base.Set.t * typ
end

open TypedTree

let use_logging = false

let log fmt =
  if use_logging
  then Format.kasprintf (fun s -> Format.printf "%s\n%!" s) fmt
  else Format.ifprintf Format.std_formatter fmt
;;

type error =
  | Occurs_check
  | No_variable of string
  | Not_implemented of string
  | Unification_failed

let pp_error = function
  | Occurs_check -> Format.printf "\nOccurs check failed"
  | No_variable s -> Format.printf "\nUndefined variable '%s'" s
  | Not_implemented msg -> Format.printf "\nNot implemented: %s" msg
  | Unification_failed -> Format.printf ""
;;

module R : sig
  open Base

  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold_left
      :  (int, 'a, Base.Int.comparator_witness) Base.Map.t
      -> init:'b t
      -> f:(int -> 'a -> 'b -> 'b t)
      -> 'b t
  end

  (** Creation of a fresh name from internal state *)
  val fresh : int t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Result.t
end = struct
  (* A compositon: State monad after Result monad *)
  open Base

  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
   fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Error x
    | Ok a -> f a last
 ;;

  let fail e st = st, Result.fail e
  let return x last = last, Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
   fun x f st ->
    match x st with
    | st, Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.Error e
 ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  module RMap = struct
    let fold_left map ~init ~f =
      Base.Map.fold map ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f key data acc)
    ;;
  end

  let fresh last = last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module Type = struct
  type t = typ

  let rec occurs_in v = function
    | TVar name -> name = v
    | Arrow (l, r) -> occurs_in v l || occurs_in v r
    | TBase _ -> false
  ;;

  let free_vars =
    let empty_set = Base.Set.empty (module Base.Int) in
    let rec helper acc = function
      | TVar name -> Base.Set.add acc name
      | Arrow (l, r) -> helper (helper acc l) r
      | TBase _ -> acc
    in
    helper empty_set
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> typ -> t R.t

  (** Getting value from substitution. May raise [Not_found] *)
  val find_exn : fresh -> t -> typ

  val find : fresh -> t -> typ option
  val apply : t -> typ -> typ
  val unify : typ -> typ -> t R.t

  (** Compositon of substitutions *)
  val compose : t -> t -> t R.t

  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
end = struct
  open R
  open R.Syntax

  (* An association map *)
  type t = (fresh, typ, Base.Int.comparator_witness) Base.Map.t

  let empty = Base.Map.empty (module Base.Int)

  let mapping key value =
    if Type.occurs_in key value then fail Unification_failed else return (key, value)
  ;;

  let singleton key value =
    let* key, value = mapping key value in
    return @@ Base.Map.update empty key ~f:(fun _ -> value)
  ;;

  let find_exn key subst = Base.Map.find_exn subst key
  let find key subst = Base.Map.find subst key
  let remove subst key = Base.Map.remove subst key

  let apply s =
    let rec helper = function
      | TVar id ->
        (match find_exn id s with
         | exception Base.Not_found_s _ -> TVar id
         | x -> x)
      | TBase t -> TBase t
      | Arrow (l, r) -> Arrow (helper l, helper r)
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TBase l, TBase r when l == r -> return empty
    | TVar a, TVar b when Int.equal a b -> return empty
    | TVar b, t | t, TVar b -> singleton b t
    | Arrow (l1, r1), Arrow (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | _ -> fail Unification_failed

  and extend k v s =
    match find k s with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      RMap.fold_left s ~init:(return s2) ~f:(fun k v acc ->
        let v = apply s2 v in
        let* k, v = mapping k v in
        return (Base.Map.update acc k ~f:(fun _ -> v)))
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2

  and compose s1 s2 = RMap.fold_left s2 ~init:(return s1) ~f:extend

  let compose_all ss =
    Base.List.fold_left ss ~init:(return empty) ~f:(fun acc subst ->
      let* acc = acc in
      compose acc subst)
  ;;
end

module VarSet = struct
  let fold_right f ini set =
    Base.Set.fold_right set ~init:ini ~f:(fun x acc ->
      let open R.Syntax in
      let* acc = acc in
      f acc x)
  ;;
end

module Scheme = struct
  type t = scheme

  let occurs_in v = function
    | s, t -> (not (Base.Set.mem s v)) && Type.occurs_in v t
  ;;

  let free_vars = function
    | s, t -> Base.Set.diff (Type.free_vars t) s
  ;;

  let apply sub (s, t) =
    let s2 = Base.Set.fold s ~init:sub ~f:(fun acc k -> Subst.remove acc k) in
    s, Subst.apply s2 t
  ;;
end

module TypeEnv = struct
  type t = (string, scheme, Base.String.comparator_witness) Base.Map.t

  let extend env id scheme = Base.Map.update env id ~f:(fun _ -> scheme)
  let empty = Base.Map.empty (module Base.String)

  let free_vars : t -> (type_num, Base.Int.comparator_witness) Base.Set.t =
    Base.Map.fold
      ~init:(Base.Set.empty (module Base.Int))
      ~f:(fun ~key:_ ~data acc -> Base.Set.union acc (Scheme.free_vars data))
  ;;

  let apply s env = Base.Map.map env ~f:(Scheme.apply s)
  let find_exn name map = Base.Map.find_exn ~equal:String.equal map name
end

open R
open R.Syntax

let unify = Subst.unify
let fresh_var = fresh >>| fun n -> TVar n

let instantiate (set, t) =
  VarSet.fold_right
    (fun typ name ->
      let* f1 = fresh_var in
      let* s = Subst.singleton name f1 in
      return @@ Subst.apply s typ)
    (return t)
    set
;;

let generalize env typ =
  let free = Base.Set.diff (Type.free_vars typ) (TypeEnv.free_vars env) in
  free, typ
;;

let lookup_env e map =
  match Base.Map.find map e with
  | None -> fail Unification_failed
  | Some scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans)
;;

open Ast
open Caml.Format

let print_tbase = function
  | Tunit -> printf "unit "
  | Tint -> printf "int "
  | Tfloat -> printf "float "
  | Tstring -> printf "string "
  | Tbool -> printf "bool "
;;

let rec print_sig = function
  | TBase t -> print_tbase t
  | Arrow (l, r) ->
    print_sig l;
    printf "->";
    print_sig r
  | TVar i -> printf "%i " i
;;

let extent_type_env env name t =
  TypeEnv.extend env name (Base.Set.empty (module Base.Int), t)
;;

let init_infer =
  let rec (helper : TypeEnv.t -> exps -> (Subst.t * typ) R.t) =
   fun env -> function
    | Exp_literal c ->
      (match c with
       | Bool _ -> return (Subst.empty, TBase Tbool)
       | Int _ -> return (Subst.empty, TBase Tint)
       | Float _ -> return (Subst.empty, TBase Tfloat)
       | String _ -> return (Subst.empty, TBase Tstring))
    | Exp_ident id -> lookup_env id env
    | Exp_binop (op, arg1, arg2) ->
      let* arg1_sub, arg1_t = helper env arg1 in
      let* arg2_sub, arg2_t = helper env arg2 in
      (match op with
       | AddInt | SubInt | MulInt | DivInt ->
         let* arg1_sub' = unify arg1_t (TBase Tint) in
         let* arg2_sub' = unify arg2_t (TBase Tint) in
         let* final = Subst.compose_all [ arg1_sub; arg2_sub; arg1_sub'; arg2_sub' ] in
         return (final, TBase Tint)
       | AddFloat | SubFloat | MulFloat | DivFloat ->
         let* arg1_sub' = unify arg1_t (TBase Tfloat) in
         let* arg2_sub' = unify arg2_t (TBase Tfloat) in
         let* final = Subst.compose_all [ arg1_sub; arg2_sub; arg1_sub'; arg2_sub' ] in
         return (final, TBase Tfloat)
       | EqInt | LeqInt | GeqInt ->
         let* arg1_sub' = unify arg1_t (TBase Tint) in
         let* arg2_sub' = unify arg2_t (TBase Tint) in
         let* final = Subst.compose_all [ arg1_sub; arg2_sub; arg1_sub'; arg2_sub' ] in
         return (final, TBase Tbool)
       | _ -> fail Unification_failed)
    | Exp_apply (name, arg_list) ->
      let result = infer_app name env (List.rev arg_list) in
      result
    | Exp_fun (name, exp) ->
      let* var_typ = fresh_var in
      let env = TypeEnv.extend env name (Base.Set.empty (module Base.Int), var_typ) in
      let* sub, t = helper env exp in
      let res_typ = Arrow (Subst.apply sub var_typ, t) in
      return (sub, res_typ)
    | Exp_letbinding (NonRec, name, expr, next) ->
      let* expr_sub, expr_t = helper env expr in
      let env = TypeEnv.apply expr_sub env in
      let _, next_type = generalize env expr_t in
      let* next_sub, gen_t =
        helper
          (TypeEnv.extend env name (Base.Set.empty (module Base.Int), next_type))
          next
      in
      let* final = Subst.compose expr_sub next_sub in
      return (final, gen_t)
    | Exp_letbinding (Rec, name, expr, next) ->
      let* var_typ = fresh_var in
      let env = TypeEnv.extend env name (Base.Set.empty (module Base.Int), var_typ) in
      let* expr_sub, expr_t = helper env expr in
      let* next_sub = unify (Subst.apply expr_sub var_typ) expr_t in
      let* s = Subst.compose next_sub expr_sub in
      let env = TypeEnv.apply s env in
      let next_t = snd (generalize env (Subst.apply s var_typ)) in
      let* next_sub, next_t =
        helper
          TypeEnv.(extend (apply s env) name (Base.Set.empty (module Base.Int), next_t))
          next
      in
      let* final = Subst.compose s next_sub in
      return (final, next_t)
    | Exp_ifthenelse (cond, fst_branch, snd_branch) ->
      let* cond_sub, cond_t = helper env cond in
      let* fst_branch_sub, fst_branch_t = helper env fst_branch in
      let* snd_branch_sub, snd_branch_t = helper env snd_branch in
      let* sub = unify cond_t (TBase Tbool) in
      let* sub' = unify fst_branch_t snd_branch_t in
      let* final_sub =
        Subst.compose_all [ cond_sub; fst_branch_sub; snd_branch_sub; sub; sub' ]
      in
      return (final_sub, Subst.apply final_sub fst_branch_t)
    | Exp_match (e, (left, right) :: cases) ->
      let* e_sub, e_type = helper env e in
      let* left_sub, left_t = helper env left in
      let* right_sub, right_t = helper env right in
      let* sub = unify e_type left_t in
      let* final_sub = Subst.compose_all [ e_sub; left_sub; right_sub; sub ] in
      let* final_sub = infer_match e_type right_t final_sub env cases in
      return (final_sub, Subst.apply final_sub right_t)
    | Exp_polyvar _ -> fail (Not_implemented "polyvar")
    | _ -> fail Unification_failed
  and infer_match e_type return_type prev_subst env = function
    | (Exp_ident name, right) :: tail ->
      let new_env = extent_type_env env name e_type in
      let* right_sub, right_t = helper new_env right in
      let* sub = unify return_type right_t in
      let* final_sub = Subst.compose_all [ prev_subst; right_sub; sub ] in
      infer_match e_type return_type final_sub env tail
    | (left, right) :: tail ->
      let* left_sub, left_t = helper env left in
      let* right_sub, right_t = helper env right in
      let* sub = unify e_type left_t in
      let* sub' = unify return_type right_t in
      let* final_sub = Subst.compose_all [ prev_subst; left_sub; right_sub; sub; sub' ] in
      infer_match e_type return_type final_sub env tail
    | _ -> return prev_subst
  and infer_app f env = function
    | [ a ] ->
      let* fun_sub, fun_t = helper env f in
      let* a_sub, a_t = helper (TypeEnv.apply fun_sub env) a in
      let* var_t = fresh_var in
      let* unified_sub = unify (Arrow (a_t, var_t)) (Subst.apply a_sub fun_t) in
      let res_typ = Subst.apply unified_sub var_t in
      let* final = Subst.compose_all [ fun_sub; a_sub; unified_sub ] in
      return (final, res_typ)
    | h :: t ->
      let* fun_sub, fun_t = infer_app f env t in
      let* a_sub, a_t = helper (TypeEnv.apply fun_sub env) h in
      let* var_t = fresh_var in
      let* unified_sub = unify (Arrow (a_t, var_t)) (Subst.apply a_sub fun_t) in
      let res_typ = Subst.apply unified_sub var_t in
      let* final = Subst.compose_all [ fun_sub; a_sub; unified_sub ] in
      return (final, res_typ)
    | _ -> fail Unification_failed
  in
  helper
;;

let infer exp env = Caml.Result.map snd (run (init_infer env exp))
let infer_with_env exp env = run (init_infer env exp)

let infer_top_level_expressions env = function
  | Declaration (exp_type, name, decl) ->
    infer (Exp_letbinding (exp_type, name, decl, Exp_ident name)) env
  | Application exp -> infer exp env
;;

open Caml.Format

let print_tbase = function
  | Tunit -> printf "unit "
  | Tint -> printf "int "
  | Tfloat -> printf "float "
  | Tstring -> printf "string "
  | Tbool -> printf "bool "
;;

let rec print_sig = function
  | TBase t -> print_tbase t
  | Arrow (l, r) ->
    print_sig l;
    printf "->";
    print_sig r
  | TVar i -> printf "%i " i
;;
