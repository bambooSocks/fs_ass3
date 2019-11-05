open System

type ExprTree =
               | Const of int
               | Ident of string
               | Sum of ExprTree * ExprTree
               | Let of string * ExprTree * ExprTree
               | App of string * ExprTree list 

let rec constFind expr  = match expr with
                          | Const x -> [x]
                          | Ident _ -> []
                          | Sum (e1,e2) -> constFind e1 @ constFind e2 
                          | Let (_,e1,e2) -> constFind e1 @ constFind e2 
                          | App (_,e3) -> List.foldBack ( fun x acc-> List.append (constFind x) acc) e3 List.empty

let rec  freeIdent expr acc = match expr with 
                              | Const _ -> acc
                              | Ident x -> Set.add (Ident x) acc
                              | Sum (e1,e2) -> freeIdent (e1) (freeIdent e2 acc )
                              | Let(x,e1,e2) -> Set.union (freeIdent e1 acc) (Set.remove (Ident x) (freeIdent e2 acc)) 
                              | App(_, e3) -> Set.foldBack ( fun x acc-> Set.union (freeIdent x Set.empty) acc) (Set.ofList e3) Set.empty

let rec subst x n t = match t with
                      | Const i -> Const i
                      | Ident a when a= x -> Const n 
                      | Ident _ -> t
                      | Sum(e1,e2) -> Sum ( subst x n e1, subst x n e2)
                      | Let (y,e1,e2) -> Let(y, subst x n e1, subst x n e2)
                      | App(s, e3) -> App (s,List.foldBack ( fun y acc-> (subst x n y)::acc) e3 List.empty)

//tests constFind 
// checks if the last branch of the function is executed and returns [2;3;3;3]
let test1= constFind (App("h", [Const 2; Ident "x"; Const 3;Let ("a", Const 3 ,Sum (Ident "a", Const 3) )]))
// checks if the last branch of the function is executed and returns [2;20;3;3]
let test2= constFind (App("h", [Const 2; Ident "x";  Sum(Const 20,Ident "x");Let ("a", Const 3 ,Sum (Ident "a", Const 3) )])) 

//tests freeIdent
//checks if the last branch of the function is executed and returns a list containing the elements Ident "x"; Ident "b"
//set.union does not preserve the initial order of the identifiers occuring in the tree
let test9 = freeIdent (App("h", [Const 2; Ident "x"; Const 3;Let ("a", Const 3 ,Sum (Ident "b", Const 3) )])) (Set.ofList [])
//checks if the last branch of the function is executed and returns a list containing the elements Ident "x", Ident "b", Ident "y" and Ident "z"
//set.union does not preserve the initial order of the identifiers occuring in the tree
let test10 = freeIdent (App("h", [Const 2; Ident "x"; Const 3;Sum(Sum(Ident "x",Ident "y"),Sum(Ident "z",Ident "z"));Let ("a", Const 3 ,Sum (Ident "b", Const 3) )])) (Set.ofList [])
//tests subst 
//checks if the last branch of the function is executed and returns  App("h", [Const 2; Const 10; Const 3;Let ("a", Const 3 ,Sum (Const 10, Const 3) )])
let test11 = subst "x" 10 (App("h", [Const 2; Ident "x"; Const 3;Let ("a", Const 3 ,Sum (Ident "x", Const 3) )])) 
//checks if the last branch of the function is executed and returns 
// App("h",[Const 2; Const 25; Const 3;Sum (Sum (Const 25,Ident "y"),Sum (Ident "z",Const 25));Let ("a",Const 3,Sum (Const 25,Const 3))])
let test12= subst "x" 25  (App("h", [Const 2; Ident "x"; Const 3;Sum(Sum(Ident "x",Ident "y"),Sum(Ident "z",Ident "x"));Let ("a", Const 3 ,Sum (Ident "x", Const 3) )]))
[<EntryPoint>]
let main argv =
    printfn " The result is %A" test12
    0 // return an integer exit code