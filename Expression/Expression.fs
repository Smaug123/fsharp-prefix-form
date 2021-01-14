namespace Expression

open Expression.Internals

type BinaryOperation =
    | Plus
    | Minus
    | Times
    | Divide

type Expr =
    | Variable of char
    | Const of int
    | BinaryOperation of BinaryOperation * Expr * Expr

type Token =
    | BinaryOperation of BinaryOperation
    | Variable of char
    | Const of int

[<RequireQualifiedAccess>]
module Expr =
    [<RequireQualifiedAccess>]
    module Seq =
        let tokenise (input : string) : Token seq =
            seq {
                let mutable i = 0
                let mutable currentInt = None

                while i < input.Length do
                    match currentInt with
                    | None ->
                        match input.[i] with
                        | '+' -> yield Token.BinaryOperation BinaryOperation.Plus
                        | '*' -> yield Token.BinaryOperation BinaryOperation.Times
                        | '-' -> yield Token.BinaryOperation BinaryOperation.Minus
                        | '/' -> yield Token.BinaryOperation BinaryOperation.Divide
                        | ' ' -> ()
                        | Int j ->
                            currentInt <- Some j
                        | x -> yield Token.Variable x
                        i <- i + 1
                    | Some j ->
                        match input.[i] with
                        | Int k ->
                            currentInt <- Some (10 * j + k)
                            i <- i + 1
                        | _ ->
                            yield Token.Const j
                            currentInt <- None

                match currentInt with
                | None -> ()
                | Some j -> yield Token.Const j
            }

        let make (t : Token seq) : Expr =
            use e = t.GetEnumerator ()
            let mutable opStack : (BinaryOperation * Expr option) list = []
            let mutable answer = None
            let rec addArg (expr : Expr) =
                match opStack with
                | [] ->
                    match answer with
                    | None -> answer <- Some expr
                    | Some _ -> failwith "imbalance"
                | (op, None) :: rest ->
                    opStack <- (op, Some expr) :: rest
                | (op, Some x) :: rest ->
                    opStack <- rest
                    addArg (Expr.BinaryOperation (op, x, expr))

            while e.MoveNext () do
                match e.Current with
                | Token.Const i ->
                    addArg (Expr.Const i)
                | Token.Variable x ->
                    addArg (Expr.Variable x)
                | Token.BinaryOperation op ->
                    opStack <- (op, None) :: opStack

            match answer with
            | None -> failwith "still waiting"
            | Some a -> a

    let tokenise (input : string) : Token list =
        let rec go (currentInt : int option) (acc : Token list) (i : int) =
            match currentInt with
            | None ->
                if i >= input.Length then acc else
                match input.[i] with
                | '+' -> go None (Token.BinaryOperation BinaryOperation.Plus :: acc) (i + 1)
                | '*' -> go None (Token.BinaryOperation BinaryOperation.Times :: acc) (i + 1)
                | '-' -> go None (Token.BinaryOperation BinaryOperation.Minus :: acc) (i + 1)
                | '/' -> go None (Token.BinaryOperation BinaryOperation.Divide :: acc) (i + 1)
                | ' ' -> go None acc (i + 1)
                | Int j -> go (Some j) acc (i + 1)
                | x -> go None (Token.Variable x :: acc) (i + 1)
            | Some j ->
                if i >= input.Length then Token.Const j :: acc else
                match input.[i] with
                | Int k -> go (Some (10 * j + k)) acc (i + 1)
                | _ -> go None (Token.Const j :: acc) i

        go None [] 0
        |> List.rev

    let make (t : Token list) : Expr =
        let rec go (t : Token list) : Expr * Token list =
            match t with
            | [] -> failwith "Received an empty token list!"
            | Token.BinaryOperation op :: rest ->
                let expr1, rest = go rest
                let expr2, rest = go rest
                Expr.BinaryOperation (op, expr1, expr2), rest
            | Token.Const i :: rest -> Expr.Const i, rest
            | Token.Variable x :: rest -> Expr.Variable x, rest

        let expr, rest = go t
        if not rest.IsEmpty then failwith "Oh no!"
        expr

    let rec eval (variables : Map<char, int Set>) (e : Expr) : int Set =
        match e with
        | Expr.Const i -> Set.singleton i
        | Expr.Variable x -> variables.[x]
        | Expr.BinaryOperation (op, e1, e2) ->
            let s1 = eval variables e1
            let s2 = eval variables e2
            Seq.allPairs s1 s2
            |> Seq.choose (fun (a, b) ->
                match op with
                | Plus -> Some (a + b)
                | Times -> Some (a * b)
                | Divide -> if b = 0 then None else Some (a / b)
                | Minus -> Some (a - b)
            )
            |> Set.ofSeq

    let naiveMax (variables : Map<char, int Set>) (e : Expr) : int =
        eval variables e
        |> Set.maxElement

    let mapUnion (resolve : 'v -> 'v -> 'v) (m1 : Map<'k, 'v>) (m2 : Map<'k, 'v>) : Map<'k, 'v> =
        m1
        |> Map.map (fun k v1 ->
            match Map.tryFind k m2 with
            | None -> v1
            | Some v2 -> resolve v1 v2
        )

    let rec counts (e : Expr) : Map<char, int> =
        match e with
        | Expr.BinaryOperation (_, e1, e2) ->
            mapUnion (+) (counts e1) (counts e2)
        | Expr.Const _ -> Map.empty
        | Expr.Variable x -> Map.ofList [x, 1]

    let max (variables : Map<char, int * int>) (e : Expr) : int =
        // If a variable appears only once in the expression, then the maximum will be attained at an extreme or at 0 or +-1.
        // I can't be bothered to think too carefully about that, so just try them all.
        let singleOccurrences =
            counts e
            |> Map.toSeq
            |> Seq.choose (fun (k, v) -> if v = 1 then Some k else None)
            |> Set.ofSeq
        let variables =
            variables
            |> Map.map (fun k (min, max) ->
                if Set.contains k singleOccurrences then
                    if min < 0 && max > 0 then Set.ofList [min; -1; 0; 1; max]
                    elif min < 0 && max = 0 then Set.ofList [min; -1; 0]
                    elif min < 0 && max < 0 then Set.ofList [min; max]
                    elif min = 0 then Set.ofList [0; 1; max]
                    else Set.ofList [min ; max]
                else Set.ofList [min..max]
            )
        naiveMax variables e

