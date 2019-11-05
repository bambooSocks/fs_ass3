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

constList l2

// ex 3
// extractFreeIdent: Set<ExprTree> -> ExprTree -> Set<ExprTree>
let rec extractFreeIdent ids = function
    | Const _         -> ids
    | Ident x         -> Set.add (Ident x) ids
    | Sum (e1, e2)    -> Set.union (extractFreeIdent ids e1) (extractFreeIdent ids e2)
    | Let (x, e1, e2) -> Set.difference (Set.union (extractFreeIdent ids e1) (extractFreeIdent ids e2)) (Set.add (Ident x) Set.empty)
    
extractFreeIdent Set.empty l2

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
