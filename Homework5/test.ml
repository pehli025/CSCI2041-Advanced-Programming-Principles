(* don't remove these, they make tests much easier to express *)
open Program
open Parser
open ProgOpt

let test1 = (try let _ = parse_program (tokens (wordlist "[1 2 ")) in false with SyntaxError _ -> true | _ -> false)
let test2 = fold_consts(Let ("x", IntC 1, Let ("x", IntC 2, Name "x")))= Let("x", IntC 1, Let ("x", IntC 2, Name "x"))
let test3 = fold_consts(If(BoolC true, IntC 1, IntC 2)) = (IntC 1)
let test4 = fold_consts(If(Gt(Mul(IntC 2, IntC 1), IntC 0), BoolC true, BoolC false)) = BoolC true
let test5 = fold_consts(If(And(Gt(Mul(IntC 2, IntC 1), IntC 0), BoolC true), BoolC true, BoolC false)) = BoolC true
let test6 = fold_consts(Not(Or(BoolC false, BoolC true))) = BoolC false
let test7 = fold_consts( Mul( Add( If(BoolC true, IntC 5, IntC 20), IntC 12), Sub(IntC 23, Let("x", IntC 0, If(BoolC true, IntC 20, IntC 3))))) = IntC 51

let test8 = (try let _ = typeof (Head (IntC 0)) [] in false with TypeError _ -> true)
let test9 = (try let _ = typeof (Head(Div(IntC 1, IntC 2))) [] in false with TypeError _ -> true)
let test10 = (try let _ = typeof (Cons(IntC 0, IntC 1)) [] in false with TypeError _ -> true)

let test11 =eval (Tail(Cons(Head(Seq [Set ("a", IntC 3); ListC [3;2;1;4;2]]), Cons(Name "a", ListC [])))) [("a", IntR 5); ("b", ListR [3;2;1;4;2])] = (ListR [3], [("a", IntR 3); ("b", ListR [3; 2; 1; 4; 2])])

let test12 = (try let _ = _parser[LB; OP; TIMES; OP; PLUS; ICONST 0; ICONST 1; ICONST 2; CP; ICONST 3; CP; RB] in false with _ -> true)

let test13  = (fold_consts (While (Eq (Mul (IntC 21, Mul (IntC 13, Mul (IntC 2, IntC 1))), Mul (IntC 21, Mul (IntC 13, Mul (IntC 2, IntC 1)))),Seq[ If (Gt (Add (IntC 1, Mul (IntC 1, Sub (IntC 1, IntC 1))), Add (IntC 1, Mul (Sub (IntC 1, IntC 1), IntC 1))), IntC 1, Print (IntC 1)); If (Lt (Sub (IntC 2, Add (IntC 2, Mul (IntC 2, IntC 2))), Sub (IntC 2, Add (Mul (IntC 2, IntC 2), IntC 2))), IntC 2, Print (IntC 2)); If (Eq (Mul (IntC 3, Sub (IntC 3, Add (IntC 3, IntC 3))), Mul (IntC 3, Sub (Add (IntC 3, IntC 3), IntC 3))), IntC 3, Print (IntC 3));If (Gt (Add (IntC 4, Add (IntC 4, Add (IntC 4, IntC 4))), Mul (IntC 4, Mul (Add (IntC 4, IntC 4), IntC 4))), IntC 4, Print (IntC 4)); If (Lt (Mul (IntC 5, Add (IntC 5, Add (IntC 5, IntC 5))), Add (IntC 5, Add (Add (IntC 5, IntC 5), IntC 5))), IntC 5, Print (IntC 5));If (Eq (Mul (IntC 6, Mul (IntC 6, Mul (IntC 6, IntC 6))), Sub (IntC 6, Sub (Sub (IntC 6, IntC 6), IntC 6))), IntC 6, Print (IntC 6));If (Gt (Add (IntC 7, Add (IntC 7, Add (IntC 7, IntC 7))), Mul (IntC 7, Mul (Mul (IntC 7, IntC 7), IntC 7))), IntC 7, Print (IntC 7));If (Lt (Mul (IntC 8, Add (IntC 8, Mul (IntC 8, IntC 8))), Mul (IntC 8, Sub (Mul (IntC 8, IntC 8), IntC 8))), IntC 8, Print (IntC 8));If (Eq (Add (IntC 9, Add (IntC 9, Add (IntC 9, IntC 9))), Sub (IntC 9, Add (Add (IntC 9, IntC 9), IntC 9))), IntC 9, Print (IntC 9));]))) = While (BoolC true,
 Seq
  [Print (IntC 1); Print (IntC 2); Print (IntC 3); Print (IntC 4);
   Print (IntC 5); Print (IntC 6); Print (IntC 7); Print (IntC 8);
   Print (IntC 9)])
let test14 = fold_consts(If(And(Gt(IntC 2, IntC 4), Lt(Add(IntC 2, IntC 2), IntC 2)), IntC 3, IntC 212 )) = IntC 212
let test15 = fold_consts(Let("x", IntC 0, While(Lt(Name "x",IntC 5), Print(Name "x")))) = Let ("x", IntC 0, While (Lt (Name "x", IntC 5), Print (Name "x")))
let test16 = fold_consts(Seq [Name "x"; IntC 1]) = IntC 1
let test17 = typeof (Cons(Head(ListC [1;2]), Tail(ListC [3;4]))) [] = ListT
let test18 = typeof (Let ("dsa", Fun ("x", IntT,   Let ("b", IntC 1, Seq[While (Gt (Name "x", IntC 0), Seq[Set ("b", Mul (Name "b", Name "x")); Set("x", Sub (Name "x", IntC 1))]); Name "b"])), Apply(Name "dsa", IntC 32))) [] = IntT
let test19 = fold_consts(If(Not(BoolC true), Add(Mul(IntC 0, Div(IntC  4, IntC 2)), IntC 0), Print(IntC 20) )) = Print (IntC 20)
let test20 = fold_consts(If(Not(BoolC false),Add (Mul (IntC 4, Div (IntC 4, IntC 2)), IntC 2), IntC 0)) = IntC 10
