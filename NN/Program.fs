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
open Microsoft.FSharp.Data.TypeProviders
open MathProvider

module La = LinearAlgebra

[<EntryPoint>]
let main argv = 
    let getFeaturesCount (X:matrix) = 
        X.NumCols 
    let getLabelsCount = 
        10 //classfying numbers

    let X = load @"data\nn_X.txt" ' '
    let y = load @"data\nn_y.txt" ' '
  
    printfn "starting training"
    let layers = [getFeaturesCount X; 25; getLabelsCount]//first and last should remain unchanged for this example
    let theta = trainNN X y layers 3.0 30
  
    printfn "accuracy is %e" (accuracy theta X y layers)
    0 
