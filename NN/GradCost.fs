module GradCost

open MatrixUtils
open System

let setFirstRowZeros m = 
    m |>  Matrix.mapi(fun x y z -> if x = 0 then 0.0 else z) 

let setFirstColZeros m = 
    m |>  Matrix.mapi(fun x y z -> if y = 0 then 0.0 else z) 

let cost (X:matrix) (y:matrix) (theta:matrix) lambda=
    let m = float y.NumRows
    let h = X*theta
    let c = sqr(h-y) * (1.0/(2.0*m)) + sqr (setFirstRowZeros theta) * (lambda/(2.0*m))
    c.[0,0]

let grad (X:matrix) (y:matrix) (theta:matrix) lambda = 
    let m = float y.NumRows
    let h = X*theta
    !X*(h-y)*(1.0/m) + (setFirstRowZeros theta)*(lambda/m)

let sigmoid (x:float) = 
    1.0/(1.0 + Math.E**(-x))
let sigmoidGradient x = 
    let s = sigmoid x
    s * (1.0 - s)

let (.-) (x:float) y = 
    y |> Matrix.map ((-) x)

let costLogistic (X:matrix) (y:matrix) (theta:matrix) lambda=
    let m = float y.NumRows
    let h = X*theta |> Matrix.map sigmoid
    let tmp1 = ((-(!y)) * Matrix.map log h) 
    let tmp2 = (!(1.0.-y))* (Matrix.map (fun el -> log (1.0-el)) h)
    let tt = setFirstRowZeros theta
    let tmp3 = sqr (setFirstRowZeros theta) * (lambda/(2.0*m))
    let c = (tmp1-tmp2)*(1.0/m) + sqr (setFirstRowZeros theta) * (lambda/(2.0*m))
    c |> Matrix.toScalar

let gradLogistic (X:matrix) (y:matrix) (theta:matrix) lambda = 
    let m = float y.NumRows
    let h = X*theta |> Matrix.map sigmoid
    let tmp = (!X)*(h-y)*(1.0/m)
    let tmp2 = (setFirstRowZeros theta)*(lambda/m)
    !X*(h-y)*(1.0/m) + (setFirstRowZeros theta)*(lambda/m)

let build c g X y lambda = 
    fun theta -> (c X y theta lambda, g X y theta lambda)

let costGradSimple (X:matrix) (y:matrix) lambda = 
    build cost grad X y lambda

let costGradLogistic (X:matrix) (y:matrix) lambda = 
    build costLogistic gradLogistic X y lambda