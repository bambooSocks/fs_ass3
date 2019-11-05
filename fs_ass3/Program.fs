// Third Assignemnt for course 02157 Functional programming
// Mihaela-Elena Nistor - s183190, Matej Majtan - s184457

type ExprTree = | Const of int
                | Ident of string
                | Sum of ExprTree * ExprTree
                | Let of string * ExprTree * ExprTree

// ex 1
let c = Const 42
let i = Ident "x"
let s = Sum (Ident "x", Const 6)
let l = Let ("y", Const 10, Sum(Ident "y", Const 42))
let l2 = Let ("y", Const 10, Let ("x", Sum(Ident "y", Const 66), Sum(Sum(Ident "y", Ident "x"),Sum(Const 6, Sum(Const 13, Ident "y")))))

// ex 2
// constList: ExprTree -> int list
let rec constList = function
    | Const x         -> [x]
    | Ident _         -> []
    | Sum (e1, e2)    -> constList e1 @ constList e2
    | Let (_, e1, e2) -> constList e1 @ constList e2 

// ex 3
// extractFreeIdent: Set<ExprTree> -> ExprTree -> Set<ExprTree>
let rec extractFreeIdent ids = function
    | Const _         -> ids
    | Ident x         -> Set.add (Ident x) ids
    | Sum (e1, e2)    -> Set.union (extractFreeIdent ids e1) (extractFreeIdent ids e2)
    | Let (x, e1, e2) -> Set.difference (Set.union (extractFreeIdent ids e1) (extractFreeIdent ids e2)) (Set.add (Ident x) Set.empty)

// ex 4
// substAux: string -> int -> ExprTree -> ExprTree
// subst: string -> int -> ExprTree -> ExprTree
let rec substAux x n = function
    | Const m         -> Const m
    | Ident y         -> if x=y then Const n else Ident y
    | Sum (e1, e2)    -> Sum (substAux x n e1, substAux x n e2)
    | Let (y, e1, e2) -> if x=y then Let (y, substAux x n e1, e2)
                         else Let (y, substAux x n e1, substAux x n e2)

let subst x n t =
    if Set.contains (Ident x) (extractFreeIdent Set.empty t) then
        substAux x n t
    else
        t
        
// ex 5
type ExprTree2 = | Const of int
                 | Ident of string
                 | Sum of ExprTree2 * ExprTree2
                 | Let of string * ExprTree2 * ExprTree2
                 | App of string * ExprTree2 list

// constList2: ExprTree -> int list
let rec constList2 = function
    | Const x         -> [x]
    | Ident _         -> []
    | Sum (e1, e2)    -> constList2 e1 @ constList2 e2
    | Let (_, e1, e2) -> constList2 e1 @ constList2 e2
    | App (_, e)      -> List.foldBack (fun el acc -> (constList2 el) @ acc) e []

// extractFreeIdent2: Set<ExprTree2> -> ExprTree -> Set<ExprTree2>
let rec extractFreeIdent2 ids = function
    | Const _         -> ids
    | Ident x         -> Set.add (Ident x) ids
    | Sum (e1, e2)    -> Set.union (extractFreeIdent2 ids e1) (extractFreeIdent2 ids e2)
    | Let (x, e1, e2) -> Set.difference (Set.union (extractFreeIdent2 ids e1) (extractFreeIdent2 ids e2)) (Set.add (Ident x) Set.empty)
    | App (x, e)      -> List.foldBack (fun el acc -> extractFreeIdent2 acc el) e ids
    
// substAux2: string -> int -> ExprTree2 -> ExprTree2
// subst2: string -> int -> ExprTree2 -> ExprTree2
let rec substAux2 x n = function
    | Const m         -> Const m
    | Ident y         -> if x=y then Const n else Ident y
    | Sum (e1, e2)    -> Sum (substAux2 x n e1, substAux2 x n e2)
    | Let (y, e1, e2) -> if x=y then Let (y, substAux2 x n e1, e2)
                         else Let (y, substAux2 x n e1, substAux2 x n e2)
    | App (y, e)      -> App (y, List.foldBack (fun el acc -> (substAux2 x n el)::acc) e [])

let subst2 x n t =
    if Set.contains (Ident x) (extractFreeIdent2 Set.empty t) then
        substAux2 x n t
    else
        t

//tests ex2

// result : [3;3]
let test1 = constList (Let ("a", Const 3 ,Sum (Ident "a", Const 3) )) 
//result : [3]
let test2 = constList (Sum(Ident "x",Const 3))
//result : [3]
let test3 = constList (Const 3)
//result []
let test4 = constList (Ident "x")

//tests ex3
//result : []
let test5 = extractFreeIdent (Let ("x", Sum(Const 2,Ident "x"), Sum(Ident "x",Const 3))) Set.empty
//result: [Ident "y"]
let test6 = extractFreeIdent (Let ("x", Sum(Const 2,Ident "x"), Sum(Ident "y",Const 3))) Set.empty
//result  [Ident "y"]
let test7 = extractFreeIdent (Let ("x", Sum(Const 2,Ident "x"), Sum(Ident "y",Const 3))) Set.empty
//result : []
let test8 = extractFreeIdent (Let("x", Const 2, Sum(Ident "x", Ident "x"))) Set.empty
//result : []
let test9 = (Set.ofList [Ident "z"]) extractFreeIdent (Let("x", Const 2, Sum(Ident "x", Ident "x"))) 



//tests ex4

//result: (Sum(Const 2,Const 3))
let test10 = subst "x" 3 (Sum(Const 2,Ident "x"))
//result : Let("x", Const 2, Sum(Const 3, Const 3))
let test11 = subst "x" 3 (Let("x", Const 2, Sum(Ident "x", Ident "x")))
//result : Let("x", Sum(Const 3, Const 3),Sum(Ident "y", Ident "y")))
let test12 = subst "x" 3 (Let("x", Sum(Ident "x", Ident "x"),Sum(Ident "y", Ident "y")))

//tests Exercise 5

//tests constFind 
// checks if the last branch of the function is executed and returns [2;3;3;3]
let test13= constList2 (App("h", [Const 2; Ident "x"; Const 3;Let ("a", Const 3 ,Sum (Ident "a", Const 3) )]))
// checks if the last branch of the function is executed and returns [2;20;3;3]
let test14= constList2 (App("h", [Const 2; Ident "x";  Sum(Const 20,Ident "x");Let ("a", Const 3 ,Sum (Ident "a", Const 3) )])) 

//tests freeIdent
//checks if the last branch of the function is executed and returns a list containing the elements Ident "x"; Ident "b"
//set.union does not preserve the initial order of the identifiers occuring in the tree
let test15 = extractFreeIdent2 (App("h", [Const 2; Ident "x"; Const 3;Let ("a", Const 3 ,Sum (Ident "b", Const 3) )])) (Set.ofList [])
//checks if the last branch of the function is executed and returns a list containing the elements Ident "x", Ident "b", Ident "y" and Ident "z"
//set.union does not preserve the initial order of the identifiers occuring in the tree
let test16 = extractFreeIdent2 (App("h", [Const 2; Ident "x"; Const 3;Sum(Sum(Ident "x",Ident "y"),Sum(Ident "z",Ident "z"));Let ("a", Const 3 ,Sum (Ident "b", Const 3) )])) (Set.ofList [])
//tests subst 
//checks if the last branch of the function is executed and returns  App("h", [Const 2; Const 10; Const 3;Let ("a", Const 3 ,Sum (Const 10, Const 3) )])
let test17 = subst2 "x" 10 (App("h", [Const 2; Ident "x"; Const 3;Let ("a", Const 3 ,Sum (Ident "x", Const 3) )])) 
//checks if the last branch of the function is executed and returns 
// App("h",[Const 2; Const 25; Const 3;Sum (Sum (Const 25,Ident "y"),Sum (Ident "z",Const 25));Let ("a",Const 3,Sum (Const 25,Const 3))])
let test18= subst2 "x" 25  (App("h", [Const 2; Ident "x"; Const 3;Sum(Sum(Ident "x",Ident "y"),Sum(Ident "z",Ident "x"));Let ("a", Const 3 ,Sum (Ident "x", Const 3) )]))
