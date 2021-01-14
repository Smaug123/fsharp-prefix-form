namespace Expression.Test

open NUnit.Framework
open FsUnitTyped
open Expression

[<TestFixture>]
module TestExpression =
    [<Test>]
    let ``Test tokenise`` () =
        Expr.tokenise "* + 2 x y"
        |> Expr.make
        |> shouldEqual (Expr.BinaryOperation (BinaryOperation.Times, Expr.BinaryOperation (BinaryOperation.Plus, Expr.Const 2, Expr.Variable 'x'), Expr.Variable 'y'))
        Expr.tokenise "+ 6 * - 4 + 2 3 8"
        |> Expr.make
        |> shouldEqual (Expr.BinaryOperation (BinaryOperation.Plus, Expr.Const 6, Expr.BinaryOperation (Times, Expr.BinaryOperation (Minus, Expr.Const 4, Expr.BinaryOperation (Plus, Expr.Const 2, Expr.Const 3)), Expr.Const 8)))

    [<Test>]
    let ``Test Seq.tokenise`` () =
        Expr.Seq.tokenise "* + 2 x y"
        |> Expr.Seq.make
        |> shouldEqual (Expr.BinaryOperation (BinaryOperation.Times, Expr.BinaryOperation (BinaryOperation.Plus, Expr.Const 2, Expr.Variable 'x'), Expr.Variable 'y'))
        Expr.Seq.tokenise "+ 6 * - 4 + 2 3 8"
        |> Expr.Seq.make
        |> shouldEqual (Expr.BinaryOperation (BinaryOperation.Plus, Expr.Const 6, Expr.BinaryOperation (Times, Expr.BinaryOperation (Minus, Expr.Const 4, Expr.BinaryOperation (Plus, Expr.Const 2, Expr.Const 3)), Expr.Const 8)))

    [<Test>]
    let testEval () =
        Expr.tokenise "* + 2 x y"
        |> Expr.make
        |> Expr.eval (Map.ofList ['x', Set.ofList [0 ; 1] ; 'y', Set.ofList [2 ; 3]])
        |> shouldEqual (Set.ofList [for x in 0..1 do for y in 2..3 do yield (2+x)*y])

        Expr.tokenise "- 0 10"
        |> Expr.make
        |> Expr.eval Map.empty
        |> shouldEqual (Set.singleton -10)

        Expr.tokenise "+ 6 * - 4 + 2 3 8"
        |> Expr.make
        |> Expr.eval Map.empty
        |> shouldEqual (Set.singleton -2)

        Expr.tokenise "+*3 2 1"
        |> Expr.make
        |> Expr.eval Map.empty
        |> shouldEqual (Set.singleton 7)

    [<Test>]
    let testEvalSeq () =
        Expr.Seq.tokenise "* + 2 x y"
        |> Expr.Seq.make
        |> Expr.eval (Map.ofList ['x', Set.ofList [0 ; 1] ; 'y', Set.ofList [2 ; 3]])
        |> shouldEqual (Set.ofList [for x in 0..1 do for y in 2..3 do yield (2+x)*y])

        Expr.Seq.tokenise "- 0 10"
        |> Expr.Seq.make
        |> Expr.eval Map.empty
        |> shouldEqual (Set.singleton -10)

        Expr.Seq.tokenise "+ 6 * - 4 + 2 3 8"
        |> Expr.Seq.make
        |> Expr.eval Map.empty
        |> shouldEqual (Set.singleton -2)

        Expr.Seq.tokenise "+*3 2 1"
        |> Expr.Seq.make
        |> Expr.eval Map.empty
        |> shouldEqual (Set.singleton 7)
