module Inter

open Absyn
open Env

(* A runtime value is an integer, a list, or a function closure
    Ciera Headley
    Raquib Talukder

 *)

type value = 
  | Int of int
  | List of value list
  | Closure of string option * string * expr * value env   

let rec toString v t = 
  match (v, t) with
  | (_, AnyT)          -> "value" 
  | (_, ArrowT (_, _)) -> "closure"  
  | (_, UnitT)         -> "null"
  | (Int i, IntT)      -> string i
  | (Int 0, BoolT)     -> "false"
  | (Int 1, BoolT)     -> "true"
  | (List l, ListT t1) -> "[" + listToString l t1 + "]"
  | _ ->  failwith "toString: mismatched type and value"
and listToString l t =
  match l with
  |      [] -> ""
  | v :: [] -> toString v t
  | v :: vs -> (toString v t) + "; " + (listToString vs t)

let rec eval (e : Absyn.expr) (env : value Env.env) : value =
    match e with
    | (Con i, _) -> Int i
    | (EListC, _) -> List []
    | (Var x, _) -> lookup env x
    | (Op1 (op, e1), _) ->
        let v1 = eval e1 env in
        match (op, v1) with
        | ("not", Int 1) -> Int 0
        | ("not", Int 0) -> Int 1
        | ("hd", List l) -> 
            match l with
            | (h :: _ ) -> h
            | _ -> failwith "Empty List"
        | ("tl", List l) -> 
            match l with
            | (h :: t) -> List t
            | _ -> failwith "Empty List"
        | ("ise", List l) ->
            match l with
            | (h :: _ ) -> Int 0
            | ( [] )    -> Int 1
            | _         -> failwith "List not given"
        | ("print", v1) -> Int 0; 

    | (Op2 (op, e1, e2), _) ->
        let v1 = eval e1 env in 
        let v2 = eval e2 env in
        match (op, v1, v2) with
        | ("+", Int v1, Int v2) -> Int (v1 + v2)
        | ("-", Int v1, Int v2) -> Int (v1 - v2)
        | ("*", Int v1, Int v2) -> Int (v1 * v2)
        | ("/", Int v1, Int v2) -> Int (v1 / v2)
        | ("=", Int v1, Int v2) -> Int (if v1 = v2 then 1 else 0)
        | ("<>", Int v1, Int v2)-> Int (if v1 = v2 then 0 else 1)
        | ("<", Int v1, Int v2) -> Int (if v1 < v2 then 1 else 0)
        | ("<=", Int v1, Int v2)-> Int (if v1 > v2 then 0 else 1)
        | ("::", List v1, List v2) -> List (v1 @ v2)
        | (";", v1, v2) -> eval e2 env

    | (If (e1, e2, e3), _) ->
        let thisExpr = eval e1 env
        match thisExpr with
        | Int 0 -> eval e3 env
        | Int _ -> eval e2 env
        | _     -> failwith "Error: If is not correct"

    | (Let (bind, toeval), t) ->
        match bind with
        | V (s, e) -> (eval toeval (s, e)::env)
        | F (s, t, h, expr) -> eval toeval (str, h)::env
        | _ -> failwith "Error: Let is not correct"

    | (Lam (tname, e1), t) -> eval e1 env

    | (Call (e1, e2), _) -> 
        let fClosure = eval e1 env
        match fClosure with
        | Closure(f, x, fBody, fDeclEnv) ->
          let xVal = eval eArg env in 
          let fBodyEnv = (x, xVal) :: (f, fClosure) :: fDeclEnv
          in eval fBody fBodyEnv
        | _ -> failwith "Error: Call not a function"
            
            