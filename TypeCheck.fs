(*

    Ciera Headley
    Raquib Talukder

*)


module TypeCheck

type expr = expr1 * htype
and expr1 =
  | Con of int
  | EListC
  | Var of string
  | Op1 of string * expr
  | Op2 of string * expr * expr
  | If of expr * expr * expr
  | Let of binding * expr
  | Lam of tname * expr
  | Call of expr * expr
and binding = 
  | V of string * expr
  | F of string * tname * htype * expr
and htype =
  | AnyT
  | IntT
  | UnitT
  | BoolT
  | ArrowT of htype * htype
  | ListT of htype
and tname = string * htype

let htype ((_, t) : expr) = t

(* Environment operations *)

type 'v env = (string * 'v) list

let rec lookup env x =
    match env with 
    | []          -> failwith (x + " not found in environment")
    | (y, v) :: r -> if x = y then v else lookup r x

let rec check (e : expr) : expr = 
    match e with
    | (Con i, t) -> if t = AnyT then failwith "Error: AnyT returned" else e
    | (EListC, t) -> if t = AnyT then failwith "Error: AnyT returned" else e 
    | (Var x, t) -> if t = AnyT then failwith "Error: AnyT returned" else e
    | ((Lam (t, e)),_) -> 
        let e1 = check e
        match (t, e1) with
        | ((_, h),(_,m)) -> (Lam (t, e1), ArrowT(h , m))
    | ((Let (b, e)),_) ->
       let e1 = check e
       match b with
       | V(s, v) -> 
            let e2 = check v
            match (e2, e1) with 
            |((_, r),(_, t)) -> if  r = t then ((Let (V (s, e2), e1)), r) else failwith("Error: Binding and expr are different")
       | F(s, t, h, f) ->   
            let f1 = check f
            match f1 with
            |(_,t1) -> 
                if h <> t1 then failwith("Error: Return Type does not match expected return type") else ((Let (F (s,t,h,f1), e1)), ArrowT(t1,h))       
    | ((If (x, y, z)),_) -> 
        let y1 = check y
        let z1 = check z
        match (y1, z1) with
        |((_, i),(_, j)) -> 
        if i = j then (if x = ((Con 0), IntT) then z1 else y1) else failwith("Error: Different types recieved in If statement") 
    | (Op1 (x, y),_) -> 
        let t1 = check y
        match (x, t1) with
        |("not", _) -> (Op1(x, t1), BoolT)
        |("ise", _) -> (Op1(x, t1), BoolT)
        |("hd", (_, z)) -> (Op1(x, t1), z) 
        |("tl", (_, z)) -> (Op1(x, t1), z)
        |_ -> failwith("Unknown op or type error") 
    | (Op2 (x, y, z),_) ->
        let t1 = check y 
        let t2 = check z 
        match (x, t1, t2) with
        |("=", (_, IntT), (_, IntT)) -> (Op2(x, t1, t2), BoolT)
        |("<>", (_, IntT), (_, IntT)) -> (Op2(x, t1, t2), BoolT)
        |("<", (_, IntT), (_, IntT)) -> (Op2(x, t1, t2), BoolT)
        |("<=", (_, IntT), (_, IntT)) -> (Op2(x, t1, t2), BoolT)
        |("+", (_, IntT), (_, IntT)) -> (Op2(x, t1, t2), IntT)
        |("-", (_, IntT), (_, IntT)) -> (Op2(x, t1, t2), IntT)
        |("*", (_, IntT), (_, IntT)) -> (Op2(x, t1, t2), IntT)
        |("::", (_, i), (_, j)) ->
            if i = j then (Op2(x, t1, t2), i)
            else failwith("List types do not match") 
        |("/", (_, IntT), (_, IntT)) -> (Op2(x, t1, t2), IntT)
        |(";", (_, _), (_, t)) -> (Op2(x, t1, t2), t)
        |_ -> failwith("Unknown op or type error") 
    | (Call (e1 , e2), _) ->
        let f1 = check e1 
        let f2 = check e2
        match (f1, f2) with
        |((_,t1),(_,t2)) -> if t1 = t2 then (Call (f1, f2), t1) else failwith("Different Types")