(*open Core*)
open Printf
open Hashtbl

type sExpr = 
    | Atom of string
    | List of sExpr list

type expr = 
    | Num of float
    | Var of string
    | Op1 of string*expr
    | Op2 of string*expr*expr
    | Fct of string * expr list

type statement = 
    | Assign of string*expr
    | Return of expr
    | Expr of expr
    | If of expr*statement list * statement list
    | While of expr*statement list
    | For of statement*expr*statement*statement list
    | FctDef of string * string list * statement list
    | Break
    | Continue

type block = statement list 

type env = Scope of (string, statement) Hashtbl.t

type envQueue = ScopeList of env list

exception ReturnExcep of float
exception BreakExcep
exception ContinueExcep

(*
let varEval (v: string) (q:env list): float  = 
    match (List.nth q (List.length q - 1)) with
    | Scope(table) -> Hashtbl.find table v
;;

(*was:  let rec evalExpr (e: expr) (q:envQueue): float  = *)
let rec evalExpr (e: expr) (q:env list): float  = 
    match e with
    | Num(value) -> value
    | Var(id) -> varEval id q
    | Op1(oper, v1) -> 
        (match oper with
            | "++" -> (evalExpr v1 q) +. 1.0
            | "--" -> (evalExpr v1 q) -. 1.0 
            | _ -> 0.0
        )
    | Op2(oper, v1, v2) ->
        (match oper with
            | "+" -> (evalExpr v1 q) +. (evalExpr v2 q)
            | "-" -> (evalExpr v1 q) -. (evalExpr v2 q)
            | "*" -> (evalExpr v1 q) *. (evalExpr v2 q)
            | "/" -> (evalExpr v1 q) /. (evalExpr v2 q)
            | "^" -> (evalExpr v1 q) ** (evalExpr v2 q)
            | ">" -> if((evalExpr v1 q) > (evalExpr v2 q)) then 1.0 else 0.0
            | "<" -> if((evalExpr v1 q) < (evalExpr v2 q)) then 1.0 else 0.0
            | ">=" -> if((evalExpr v1 q) >= (evalExpr v2 q)) then 1.0 else 0.0
            | "<=" -> if((evalExpr v1 q) <= (evalExpr v2 q)) then 1.0 else 0.0 
            | "==" -> if((evalExpr v1 q) = (evalExpr v2 q)) then 1.0 else 0.0
            | _ -> 0.0)
    | _ -> 0.0
;;
let assignVariable (id: string) (express: expr) (q: env list): float =
    match (List.nth q (List.length q - 1)) with
    | Scope(table) -> Hashtbl.add table id (evalExpr express q); 1.0
;;
*)


(* Trying to create Return labels. Things will get complicated. 
Previous code is in "Code Backup.txt" *)

let rec evalStatement (state: statement) (q: env list): float =
    match state with
    | Assign(id, express) -> assignVariable id express q
    | Expr(expr) -> evalExpr expr q
    | If(expr, trueCode, falCode) -> 
        let condition = evalExpr expr q in
            if(condition = 1.0)
            then (runCode trueCode q; 1.0)
            else (runCode falCode q; 0.0)
    | While(expr, code) -> 
        (try
        runWhile expr code q; 0.0
        with BreakExcep -> 0.0)
    | For(assign, check, incre, code) ->
        evalStatement assign q |> ignore;
        runFor assign check incre code q; 0.0;
    | FctDef(id, params, code) -> assignFunction id (FctDef(id, params, code)) q
    | Return(expr) -> let answer = evalExpr expr q in
                        raise (ReturnExcep answer)
    | Continue -> raise ContinueExcep
    | Break -> raise BreakExcep

and runCode (code: statement list) (q: env list): unit = 
    match code with
    | [] -> ()
    | _ -> (try 
            evalStatement (List.hd code) q |> ignore;
            runCode (List.tl code) q;
            with ReturnExcep(answer) -> raise (ReturnExcep answer))

and varEval (v: string) (q:env list): float  = 
    match (List.nth q (List.length q - 1)) with
    | Scope(table) -> let test = Hashtbl.find_opt table v in
                        match test with
                        | None -> if(List.length q = 1)
                                    then (failwith "Variable doesn't exist";)
                                    else (varEval v (List.rev(List.tl(List.rev q)));)
                        | Some(state) -> evalStatement state q;

(*was:  let rec evalExpr (e: expr) (q:envQueue): float  = *)
and evalExpr (e: expr) (q:env list): float  = 
    match e with
    | Num(value) -> value
    | Var(id) -> varEval id q
    | Op1(oper, v1) -> 
        (match oper with
            | "++" -> (evalExpr v1 q) +. 1.0
            | "--" -> (evalExpr v1 q) -. 1.0 
            | _ -> 0.0
        )
    | Op2(oper, v1, v2) ->
        (match oper with
            | "+" -> (evalExpr v1 q) +. (evalExpr v2 q)
            | "-" -> (evalExpr v1 q) -. (evalExpr v2 q)
            | "*" -> (evalExpr v1 q) *. (evalExpr v2 q)
            | "/" -> (evalExpr v1 q) /. (evalExpr v2 q)
            | "^" -> (evalExpr v1 q) ** (evalExpr v2 q)
            | ">" -> if((evalExpr v1 q) > (evalExpr v2 q)) then 1.0 else 0.0
            | "<" -> if((evalExpr v1 q) < (evalExpr v2 q)) then 1.0 else 0.0
            | ">=" -> if((evalExpr v1 q) >= (evalExpr v2 q)) then 1.0 else 0.0
            | "<=" -> if((evalExpr v1 q) <= (evalExpr v2 q)) then 1.0 else 0.0 
            | "==" -> if((evalExpr v1 q) = (evalExpr v2 q)) then 1.0 else 0.0
            | _ -> 0.0)
    | Fct(id, params) -> let newScope = Scope(Hashtbl.create 123456)  in 
                        let newList = List.append q (newScope :: []) in
                        let functionDef = funcEval id newList in
                        (match functionDef with
                        | FctDef (id2, localVar, code) -> assignParams localVar params newList; 
                            (try 
                            runCode code newList; 0.0;
                            with ReturnExcep(value) -> value)
                        | _ -> 0.0)
    
and assignVariable (id: string) (express: expr) (q: env list): float =
    match (List.nth q (List.length q - 1)) with
    | Scope(table) -> Hashtbl.add table id (Expr(Num(evalExpr express q))); 1.0

and assignFunction (id: string) (func: statement) (q:env list): float = 
    match (List.nth q (List.length q - 1)) with
    | Scope(table) -> Hashtbl.add table id func; 1.0

and funcEval (id: string) (q: env list): statement = 
    match (List.nth q (List.length q - 1)) with
    | Scope(table) -> let test = Hashtbl.find_opt table id in
                        match test with
                        | None -> if(List.length q = 1)
                                    then (failwith "Variable doesn't exist";)
                                    else (funcEval id (List.rev(List.tl(List.rev q)));)
                        | Some(state) -> state;

and assignParams (names: string list) (values: expr list) (q: env list): unit =
    if (List.length names = List.length values)
    then
    (
        match names with
        | [] -> ()
        | _ -> assignVariable (List.hd names) (List.hd values) q |> ignore; 
                assignParams (List.tl names) (List.tl values) q
    )
    else
    (
    )
and start (state: statement) (q: env list): float = 
    (try
    match state with
    | Assign(id, express) -> assignVariable id express q
    | Expr(expr) -> evalExpr expr q
    | If(expr, trueCode, falCode) -> 
        let condition = evalExpr expr q in
            if(condition = 1.0)
            then (runCode trueCode q; 1.0)
            else (runCode falCode q; 0.0)
    | While(expr, code) -> 
        runWhile expr code q; 0.0
    | For(assign, check, incre, code) ->
        evalStatement assign q |> ignore;
        runFor assign check incre code q; 0.0;
    | FctDef(id, params, code) -> assignFunction id (FctDef(id, params, code)) q
    | Return(expr) -> let answer = evalExpr expr q in
                        raise (ReturnExcep answer)
    | Continue -> raise ContinueExcep
    | Break -> raise BreakExcep
    with ReturnExcep(answer) -> answer)

and runWhile (check: expr) (code: statement list) (q: env list) : unit =
    
    (try
    if(evalExpr check q = 1.0)
    then (try 
        runCode code q; 
        runWhile check code q; 
        with ContinueExcep -> runWhile check code q;)
    else ();
    with BreakExcep -> ())

and runFor (assign: statement) (check: expr) (incre: statement) (code: statement list) (q: env list) : unit =

    (try
    if(evalExpr check q = 1.0)
    then 
        (try 
        runCode code q; 
        evalStatement incre q |> ignore; 
        runFor assign check incre code q; 
        with ContinueExcep -> runFor assign check incre code q;)
    else ();
    with BreakExcep -> ())

;;
(* Test for expression *)
(*
let%expect_test "evalNum" = 
    evalExpr (Num 10.0) [] |>
    printf "%F";
    [%expect {| 10. |}]
*)

(*
let evalCode (_code: block) (_q:envQueue): unit = 
    (* crate new environment *)
    (* user fold_left  *)
    (* pop the local environment *)
    print_endline "Not implemented"

let runCode (code: block): unit = 

    print_endline "Not Implemented"
*)
(*
let evalStatement (s: statement) (q:envQueue): envQueue =
    match s with 
        | Assign(_v, _e) -> (* eval e and store in v *) q
        | If(e, codeT, codeF) -> 
            let condition = evalExpr e q in
                if(condition>0.0) then
                    runCode codeT q 
                else
                    runCode codeF q
            ;q
        | _ -> q (*ignore *)
*)


(*My Testing Code*)
print_string "---Testing 1 evalExpr Basic Operations. Should be 49: ";

let jimmy = Scope(Hashtbl.create 123456) :: [] in
print_float (evalExpr (Op2("*", Num(7.0), Num(7.0))) jimmy);
print_endline "";

print_string "---Testing 2 evalExpr Boolean Operations. Should be 1.0: ";

let jimmy = Scope(Hashtbl.create 123456) :: [] in
print_float (evalExpr (Op2("==", Num(7.0), Num(7.0))) jimmy);
print_endline "";

print_string "---Testing 3 evalStatement Variable Assignment. Should be 1.0: ";

let jimmy = Scope(Hashtbl.create 123456) :: [] in
print_float (evalStatement (Assign("A", Num(7.0))) jimmy);
print_endline "";

print_string "---Testing 4 evalExpr Variable Access. Should be 7.0: ";

let jimmy = Scope(Hashtbl.create 123456) :: [] in
evalStatement (Assign("A", Num(7.0))) jimmy |> ignore;
print_float (evalExpr (Var("A")) jimmy);
print_endline "";

print_string "---Testing 5 evalExpr Variable Access and Incrementing. Should be 8.0: ";

let jimmy = Scope(Hashtbl.create 123456) :: [] in
evalStatement (Assign("A", Num(7.0))) jimmy |> ignore;
print_float (evalExpr (Op1("++", (Var("A")))) jimmy);
print_endline "";

print_string "---Testing 6 runCode If statements. Should be 5.0: ";

let jimmy = Scope(Hashtbl.create 123456) :: [] in
evalStatement (If(Op2(">", Num(5.0), Num(4.0) ), Assign("A", Num(5.0)) :: [], Assign("A", Num(4.0)) :: [])) jimmy |> ignore;
print_float (evalExpr(Var("A")) jimmy);
print_endline "";


print_string "---Testing 7 runCode While statements. Should be 5.0: ";

let jimmy = Scope(Hashtbl.create 123456) :: [] in
evalStatement (Assign("A", Num(0.0))) jimmy |> ignore;
evalStatement (While(Op2("<", Var("A"), Num(5.0)), Assign("A", Op1("++", Var("A"))) :: [])) jimmy |> ignore;
print_float (evalExpr(Var("A")) jimmy);
print_endline "";


print_string "---Testing 8 runCode For statements. Should be 6.0: ";

let jimmy = Scope(Hashtbl.create 123456) :: [] in
evalStatement (Assign("A", Num(0.0))) jimmy |> ignore;
evalStatement (For(Assign("i", Num(0.0)), Op2("<", Var("i"), Num(6.0)), Assign("i", Op1("++", Var("i"))), Assign("A", Op1("++", Var("A"))) :: [])) jimmy |> ignore;
print_float (evalExpr(Var("A")) jimmy);
print_endline "";

print_string "---Testing 9 Functions. Should be 4.0: ";

let jimmy = Scope(Hashtbl.create 123456) :: [] in
evalStatement (Assign("A", Num(0.0))) jimmy |> ignore;
evalStatement (FctDef("add", "A" :: "B" :: [], Return(Op2("+", Var("A"), Var("B"))) :: [])) jimmy |> ignore;
print_float(evalExpr (Fct("add", Num(1.0) :: Num(3.0) :: [])) jimmy);
print_endline "";

print_string "---Testing 10 Return. Should be 4.0: ";

let jimmy = Scope(Hashtbl.create 123456) :: [] in
print_float (start (Return(Op2("+", Num(2.0), Num(2.0)))) jimmy);
print_endline "";

print_string "---Testing 10 Return. Should be 10.0: ";

let jimmy = Scope(Hashtbl.create 123456) :: [] in
print_float (start (If(Op2(">", Num(5.0), Num(4.0) ), Return(Num(10.0)) :: [], Return(Num(7.0)) :: [])) jimmy);
print_endline "";

print_string "---Testing 11 While statements with Break. Should be 3.0: ";

let jimmy = Scope(Hashtbl.create 123456) :: [] in
start (Assign("A", Num(0.0))) jimmy |> ignore;
start (While(Op2("<", Var("A"), Num(5.0)), If(Op2(">=", Var("A"), Num(3.0)), Break :: [], Assign("A", Op1("++", Var("A"))) :: []) :: [])) jimmy |> ignore;
print_float (evalExpr(Var("A")) jimmy);
print_endline "";

print_string "---Testing 12 runCode For statements with Return. Should be 7.0: ";

let jimmy = Scope(Hashtbl.create 123456) :: [] in
start (Assign("A", Num(0.0))) jimmy |> ignore;
print_float (start (For(Assign("i", Num(0.0)), Op2("<", Var("i"), Num(6.0)), Assign("i", Op1("++", Var("i"))), Return(Num(7.0)) :: [])) jimmy);
print_endline "";


print_string "---Testing 13 Recursive Functions. Should be 15.0: ";

let jimmy = Scope(Hashtbl.create 123456) :: [] in
start(FctDef("Rec", "X" :: [], If(Op2("<", Var("X"), Num(1.0)), Return(Num(0.0)) :: [], Return(Op2("+", Var("X"), Fct("Rec", Op2("-", Var("X"), Num(1.0)) :: []))) :: []) :: [])) jimmy |> ignore;
print_float(start(Expr(Fct("Rec", Num(5.0) :: []))) jimmy);
print_endline "";


print_string "---Testing 14 Recursive Functions. Should be 3.0: ";

let jimmy = Scope(Hashtbl.create 123456) :: [] in
start(Assign("X", Num(5.0))) jimmy |> ignore;
start(FctDef("Rec", "X" :: [], Return(Var("X")) :: [])) jimmy |> ignore;
print_float(start(Expr(Fct("Rec", Num(3.0) :: []))) jimmy);
print_endline "";


(* 
    v = 10; 
    v // display v
 *)

 (*
let p1: block = [
        Assign("v", Num(1.0));
        Expr(Var("v")) 
]

let%expect_test "p1" =
    evalCode p1 []; 
    [%expect {| 1. |}]
*)
(*
    v = 1.0;
    if (v>10.0) then
        v = v + 1.0
    else
        for(i=2.0; i<10.0; i++) {
            v = v * i
        }
    v   // display v
*)
(*
let p2: block = [
    Assign("v", Num(1.0));
    If(
        Op2(">", Var("v"), Num(10.0)), 
        [Assign("v", Op2("+", Var("v"), Num(1.0)))], 
        [For(
            Assign("i", Num(2.0)),
            Op2("<", Var("i"), Num(10.0)),
            Expr(Op1("++a", Var("i"))),
            [
                Assign("v", Op2("*", Var("v"), Var("i")))
            ]
        )]
    );
    Expr(Var("v"))
]

let%expect_test "p1" =
    evalCode p2 []; 
    [%expect {| 3628800. |}]

(*  Fibbonaci sequence
    define f(x) {
        if (x<1.0) then
            return (1.0)
        else
            return (f(x-1)+f(x-2))
    }

    f(3)
    f(5)
 *)
let p3: block = 
    [
        FctDef("f", ["x"], [
            If(
                Op2("<", Var("x"), Num(1.0)),
                [Return(Num(1.0))],
                [Return(Op2("+",
                    Fct("f", [Op2("-", Var("x"), Num(1.0))]),
                    Fct("f", [Op2("-", Var("x"), Num(1.0))])
                ))])
        ]);
        Expr(Fct("f", [Num(3.0)]));
        Expr(Fct("f", [Num(5.0)]));
    ]

let%expect_test "p3" =
    evalCode p3 []; 
    [%expect {| 
        2. 
        5.      
    |}]
*)