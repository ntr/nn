namespace NNCore

module Samples =

    open Microsoft.FSharp.Math
    open System.IO
    open CheckGradient
    open MatrixUtils
    open GradCost

    //simple gradient descent
    let gradientDescentImpl = 
        let gradDescent (X:matrix) (y:matrix) (theta:matrix) (alpha:float) numiters =
            let cgs = costGradSimple X y 0.0
            let getNextTheta (theta, lst) = 
                let vals = cgs theta
                let t = theta -  (snd vals)* alpha
                (t, fst vals::lst)
            let res = [1..numiters] |> Seq.fold(fun t i -> getNextTheta t) (theta,[])
            (res |> snd |> List.head, fst res)
           
        let matr = load @"data\s1.txt" ','
        let X = matr.Columns (0,1) |> addBiasColumn
        let y = matr.Column 1 |> Matrix.ofVector

        let iter = 1500
        let alpha = 0.01
        let theta = Matrix.create 2 1 0.0
        let j,t = gradDescent X y theta alpha iter

        let theta2 = Matrix.create 2 1 0.0

        let cgs_function = costGradSimple X y 0.0
        let t2 = checkGradient cgs_function theta2
        0

    //logistic regression with mutiple variants    
    let LogisticRegressionSample=
        let matr = load @"data\s2.txt" ','
        let X = matr.Columns (0, 2) 
        let X = mapXToPolynomial X 6 |> Seq.toList |> Matrix.ofList
        let y = matr.Column 2 |> Matrix.ofVector
        let theta = Matrix.create X.NumCols 1 0.0

        let (theta_res, lst, ind) = checkGradient (costGradLogistic X y 1.0) theta None
        let predict = (X*theta_res) 
                        |> Matrix.map sigmoid
                        |> Matrix.map round
                        |> Matrix.foldi (fun row _ seed v -> if v = y.[row, 0] then seed + 1.0 else seed) 0.0
        predict / (size y |> fst |> float) 
