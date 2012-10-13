// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
#if INTERACTIVE
#I "bin\debug"
#r "FSharp.PowerPack"
#r "FSharp.Data.TypeProviders"
#r "MathProvider"
#load "MatrixUtils.fs"
#load "FMinCg.fs"
#load "GradCost.fs"
#load "NN.fs"
#endif
module main

open MatrixUtils
open Samples
open NN

[<EntryPoint>]
let main argv = 
    let X = load @"data\nn_X.txt" ' '
    let y = load @"data\nn_y.txt" ' '
  
    //compareWithNumericalGradient (nnCost 400 25 10 X y 0.0)  it
    printfn "starting training"
    let layers = [400; 25; 10]//first and last should remain unchanged for this example
    let theta = trainNN X y layers 3.0 300
  
    printfn "accuracy is %e" (accuracy theta X y layers)
    0 
