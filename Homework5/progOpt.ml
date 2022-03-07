(* progOpt.ml -- optimizations for our little programming language *)

open Program
exception OptError of string
let opt_err s = raise (OptError s)
let _checker = function
| IntC _ |BoolC _ | ListC _ | Name _-> true
| _ -> false
let is_const_exp = function
 IntC _ | BoolC _ | ListC _ -> true
 | _ -> false

(* insert (correct) definition of const_fold here *)
let rec fold_consts e = match e with
  | (IntC _) | (BoolC _) | Name _ -> e
  | Print e -> Print (fold_consts e)
  | Apply (e1, e2) -> Apply (fold_consts e1, fold_consts e2)
  | Add (e1, e2) -> (match (fold_consts e1, fold_consts e2) with
    | (IntC x, IntC y) -> IntC(x+y)
    | (e1, e2) -> (Add(fold_consts e1, fold_consts e2)))
  | Mul (e1,e2) -> (match (fold_consts e1, fold_consts e2) with
    |(IntC x, IntC y) -> IntC(x*y)
    | (e1, e2) -> (Mul(fold_consts e1, fold_consts e2)))
  | Sub (e1,e2) -> (match (fold_consts e1, fold_consts e2) with
    |(IntC x, IntC y) -> IntC(x-y)
    | (e1, e2) -> (Sub(fold_consts e1, fold_consts e2)))
  | Div (e1,e2) -> (match (fold_consts e1, fold_consts e2) with
    |(IntC x, IntC y) -> IntC(x/y)
    | (e1, e2) -> (Div(fold_consts e1, fold_consts e2)))
  | And (e1,e2) -> (match(fold_consts e1, fold_consts e2) with
    | (BoolC true, BoolC true)->  BoolC true
    | (BoolC true,_) -> BoolC false
    | (_, BoolC true) -> BoolC false
    | (BoolC false, BoolC false) -> BoolC false;
    | (e1, e2) -> (And(fold_consts e1, fold_consts e2)))
  | Or (e1,e2) -> (match(fold_consts e1, fold_consts e2) with
    | (BoolC true,_) -> BoolC true
    | (_, BoolC true) -> BoolC true
    | (BoolC false, BoolC false) -> BoolC false
    | (BoolC false, BoolC true) -> BoolC true
    | (BoolC true, BoolC false) -> BoolC true
    | (e1, e2) -> (Or(fold_consts e1, fold_consts e2)))
  | Not (e1) -> (match (fold_consts e1) with
    | (BoolC true) -> BoolC false | (BoolC false) -> BoolC true
    | e1 -> (Not(fold_consts e1)))
  | Gt (e1, e2) -> (match (fold_consts e1, fold_consts e2) with
    | (IntC x, IntC y) -> if(x>y) then BoolC true else BoolC false
    | (e1, e2) -> Gt(fold_consts e1, fold_consts e2))
  | Lt (e1, e2) -> (match (fold_consts e1, fold_consts e2) with
    | (IntC x, IntC y) -> if(x<y) then BoolC true else BoolC false
    | (e1, e2) -> Lt(fold_consts e1, fold_consts e2))
  | Eq (e1, e2) -> (match (fold_consts e1, fold_consts e2) with
    | (IntC x, IntC y) -> if(x=y) then BoolC true else BoolC false
    | (e1, e2) -> Eq(fold_consts e1, fold_consts e2))
  | Cons (e1, e2) -> (match (fold_consts e1, fold_consts e2) with
    | (ListC x, ListC y) -> ListC (x@y)
    | (e1, e2) -> Cons(fold_consts e1, fold_consts e2))
  | Seq e -> (let rec _helper acc lst = match lst with
    | [] -> if (List.length acc =1 && _checker (List.hd acc)) then List.hd acc else Seq (List.rev acc)
    | h::t -> if (_checker(fold_consts h)) && (List.length lst > 1) then _helper acc t else _helper ((fold_consts h)::acc)t
  in _helper [] e)
  | While (x, body) -> (match fold_consts x with
    | (BoolC false) -> Seq []
    | x -> While(fold_consts x, fold_consts body))
  | If (c, thn, els) -> if fold_consts c = BoolC true then fold_consts thn else if fold_consts c = BoolC false then fold_consts els else If (c, fold_consts thn, fold_consts els)
  | Let (str, e1, e2) -> (match (fold_consts e1, fold_consts e2) with
    | (e1, e2) when (is_const_exp e1 && is_const_exp e2) -> e2
    | (e1, e2) -> Let (str, e1, e2)  )
  | Set (str, e1) -> Set(str, fold_consts e1)
  | Fun (e1, e2, e3) -> Fun(e1, e2, fold_consts e3)
  | Head e -> match e with
    | ListC l -> if l = [] then opt_err "Head" else Head(ListC l)
    | e-> Head(fold_consts e)



  | _ -> e (* it's up to you to finish the rest, this is just here so things compile initially *)
