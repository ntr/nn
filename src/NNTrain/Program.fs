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
open NNCore
open MatrixUtils
open Samples
open NN
open System

let getFeaturesCount (X:matrix) = 
    X.NumCols 
let getLabelsCount = 
    10 //classfying numbers

let trainOnSampleDS ()=  //code used to train neural network on a standart dataset (5000 examples for all numbers)
    let X = load @"data\nn_X.txt" ' '
    let y = load @"data\nn_y.txt" ' '
    let ((X,y),(valX,valY)) = splitToTrainAndValidationSet X y 4000
    let layers = [getFeaturesCount X; 50; 25; getLabelsCount]
    let theta = trainNN X y layers 3.0 150
    printfn "accuracy of train set is %e" (accuracy theta X y layers)
    printfn "accuracy of validation set is %e" (accuracy theta valX valY layers)
    0
  
[<EntryPoint>]
let main argv = 
    let X = load @"data\X_custom.txt" ' '
    let y = load @"data\Y_custom.txt" ' '
    let trainRatio = 0.8 //percentage of all examples that goes to train set
    let ((X,y),(valX,valY)) = splitToTrainAndValidationSet X y (float X.NumRows * trainRatio |> int)

    printfn "starting training"
    let layers = [getFeaturesCount X; 50; 25; 5]//first and last should remain unchanged for this example
    let theta = trainNN X y layers 7.0 250
   // save "theta_custom.txt" theta
   
    printfn "accuracy of train set is %e" (accuracy theta X y layers)
    printfn "accuracy of validation set is %e" (accuracy theta valX valY layers)

    printfn "Save theta? (y/n)"
    let input = Console.ReadLine()
    if input = "y" then
        save "theta.txt" theta
        printfn "saved to theta.txt"

    0 
