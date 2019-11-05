open System


type ExprTree =
               | Const of int
               | Ident of string
               | Sum of ExprTree * ExprTree
               | Let of string * ExprTree * ExprTree
      

let v1 = Const 3
let v2 = Ident "x"
let v3 = Sum (Ident "x", Const 3)
let v4 = Let ("a", Const 3 ,Sum (Ident "a", Const 3) )



//returns an int list with the constants occuring in the ExprTree

let rec constFind expr  = match expr with
                          | Const x -> [x]
                          | Ident _ -> []
                          | Sum (e1,e2) -> constFind e1 @ constFind e2 
                          | Let (_,e1,e2) -> constFind e1 @ constFind e2 
//version 1 of ex 3

let rec  g expr acc = match expr with 
                      | Const _ -> acc
                      | Ident x -> Set.add x acc
                      | Sum (e1,e2) -> g (e1) (g e2 acc )
                      | Let(x,e1,e2) -> Set.union (g e1 acc) (Set.remove x (g e2 acc)) 

//version 2 and final version of ex 3

let rec  freeIdent expr acc = match expr with 
                              | Const _ -> acc
                              | Ident x -> Set.add (Ident x) acc
                              | Sum (e1,e2) -> freeIdent (e1) (freeIdent e2 acc )
                              | Let(x,e1,e2) -> Set.union (freeIdent e1 acc) (Set.remove (Ident x) (freeIdent e2 acc)) 


// function ExpTree -> Set<string> -> Set<string>


//e value of the function is the expression tree obtained from t by replacing every
//free occurrence of the identifier given by x with the constant given by n. State the type
//of the function in a comment.
//string -> int -> ExprTree -> ExprTree
let rec subst x n t = match t with
                      | Const i -> Const i
                      | Ident a when a= x -> Const n 
                      | Ident _ -> t
                      | Sum(e1,e2) -> Sum ( subst x n e1, subst x n e2)
                      | Let (y,e1,e2) -> Let(y, subst x n e1, subst x n e2)



// function type string -> int -> ExprTree ->ExprTree

//tests ex2

let test1 = constFind (Let ("a", Const 3 ,Sum (Ident "a", Const 3) )) 
let test2 = constFind (Sum(Ident "x",Const 3))
let test3 = constFind (Const 3)
let test4 = constFind (Ident "x")

//tests ex3

let test5 = freeIdent (Let ("x", Sum(Const 2,Ident "x"), Sum(Ident "x",Const 3))) Set.empty
let test6 = freeIdent (Let ("x", Sum(Const 2,Ident "x"), Sum(Ident "y",Const 3))) Set.empty
let test7 = freeIdent (Let ("x", Sum(Const 2,Ident "x"), Sum(Ident "y",Const 3))) Set.empty
let test8 = freeIdent (Let("x", Const 2, Sum(Ident "x", Ident "x"))) Set.empty
let test9 = freeIdent (Let("x", Const 2, Sum(Ident "x", Ident "x"))) (Set.ofList [Ident "z"])



//tests ex4

let test10 = subst "x" 3 (Sum(Const 2,Ident "x"))
let test11 = subst "x" 3 (Let("x", Const 2, Sum(Ident "x", Ident "x")))
let test12 = subst "x" 3 (Let("x", Sum(Ident "x", Ident "x"),Sum(Ident "y", Ident "y")))
[<EntryPoint>]
let main argv =
    printfn " The result is %A" test11
    0 // return an integer exit code