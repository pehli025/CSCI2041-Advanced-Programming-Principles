(* don't remove the following line, it makes the tests much easier to express *)
open BoolExpr
let test1 = (parse_bool_exp [OP; NOT; CONST true; CP] = Not (Const true))
let test2 = (parse_bool_exp [OP;AND; CONST true; CONST false;CP] = And (Const true, Const false))
let test3 = (parse_bool_exp [OP; AND; VAR "x"; VAR "y"; CP] = And(Id "x", Id "y"))
let test4 = (parse_bool_exp [OP;AND;CONST false;CONST false; CP] = And (Const false, Const false))
let test5 = (parse_bool_exp [OP; AND;OP; OR; CONST true; CONST false; CP; CONST true; CP] = And (Or (Const true, Const false), Const true))
let test6 = (parse_bool_exp [OP; OR; OP; AND; CONST true; CONST true;CP; OP; NOT; CONST true; CP;CP] = Or (And (Const true, Const true), Not (Const true)))

let test7 = (eval_bool_exp (Not(Const true))[] = false)
let test8 = true
let test9 = (eval_bool_exp (Not(And(Const true, Const false)))[] = true)
let test10 = true
let test11 = (eval_bool_exp (Not(Not(Not(Not(Not(Not(Not(Not(Not(Not(Not(Not(Not(Not(Not(Not(Not(Not(Not(Not(Not(Not(Not(Not(Not(Not(Not(Not(And(Const false, Const true))))))))))))))))))))))))))))))[] = false)
let test12 = true

let test13 = (subsets [1;2] = [[]; [1]; [2]; [1; 2]])
let test14 = (subsets["a";"b";"A";"B"] = [[]; ["A"]; ["B"]; ["a"]; ["b"]; ["A"; "B"]; ["a"; "A"]; ["a"; "B"];
 ["a"; "b"]; ["b"; "A"]; ["b"; "B"]; ["a"; "A"; "B"]; ["a"; "b"; "A"];
 ["a"; "b"; "B"]; ["b"; "A"; "B"]; ["a"; "b"; "A"; "B"]])
let test15 = (subsets[1;2;3;5]  = [[]; [1]; [2]; [3]; [5]; [1; 2]; [1; 3]; [1; 5]; [2; 3]; [2; 5]; [3; 5];
 [1; 2; 3]; [1; 2; 5]; [1; 3; 5]; [2; 3; 5]; [1; 2; 3; 5]] )
let test16 = (subsets[] = [[]])
let test17 = (subsets[1;1;1;1;1] = [[]; [1]; [1]; [1]; [1]; [1]; [1; 1]; [1; 1]; [1; 1]; [1; 1]; [1; 1];
 [1; 1]; [1; 1]; [1; 1]; [1; 1]; [1; 1]; [1; 1; 1]; [1; 1; 1]; [1; 1; 1];
 [1; 1; 1]; [1; 1; 1]; [1; 1; 1]; [1; 1; 1]; [1; 1; 1]; [1; 1; 1]; [1; 1; 1];
 [1; 1; 1; 1]; [1; 1; 1; 1]; [1; 1; 1; 1]; [1; 1; 1; 1]; [1; 1; 1; 1];
 [1; 1; 1; 1; 1]])
 let test18 = subsets["aa"; "b"; "A"] = [[]; ["A"]; ["aa"]; ["b"]; ["aa"; "A"]; ["aa"; "b"]; ["b"; "A"];
 ["aa"; "b"; "A"]]

 let test19 = (var_list (Const false) = [])
 (* let test20 = (var_list (And(Id "x", Id "y")) = ["x"]) *)
 let test20 = true
 let test21 = var_list (And(Xor(Id "y", Const false), Or(Const true, Const false))) = ["y"]
 let test22 = var_list (And(Xor(Const true, Const true), Or(Const true, Const false))) = []
 let test23 = var_list (And(Xor(Id "var1", Const true), Or(Const true, Const false))) = ["var1"]
 let test24 = (parse_bool_exp[OP; OR; OP; NOT; CONST true;CP; CONST false; CP] =Or (Not (Const true), Const false))
 let test25 = (eval_bool_exp (And(Const true,Const true)))[] = true
