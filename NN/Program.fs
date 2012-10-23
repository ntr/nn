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
open System

module La = LinearAlgebra

[<EntryPoint>]
let main argv = 
    let getFeaturesCount (X:matrix) = 
        X.NumCols 
    let getLabelsCount = 
        10 //classfying numbers

    let splitToTrainAndValidationSet (X:matrix) Y =
        let addEndColumn (m:matrix) (col:vector) =
            let res = Matrix.init m.NumRows (m.NumCols+1) (fun x y -> if y = m.NumCols then col.[x] else m.[x,y])
            res
        let splitRows elemsCount (m:matrix) = 
            (m.[0..elemsCount-1,0..m.NumCols-1], m.[elemsCount..m.NumRows-1,0..m.NumCols-1])
        let splitCols elemsCount (m:matrix)= 
            (m.[0..m.NumRows-1,0..elemsCount-1], m.[0..m.NumRows-1,elemsCount..m.NumCols-1])
        let splitByLastCol (m:matrix) = 
            splitCols (m.NumCols-1) m
        
        let (train,validation) = addEndColumn X (Y|>Matrix.toVector) 
                                    |> shuffleRows
                                    |> splitRows 4000
        

        (splitByLastCol train, splitByLastCol validation)

    
    let X = load @"data\nn_X.txt" ' '
    let y = load @"data\nn_y.txt" ' '

    let ((X,y),(valX,valY)) = splitToTrainAndValidationSet X y

  
    printfn "starting training"
    let layers = [getFeaturesCount X; 25; getLabelsCount]//first and last should remain unchanged for this example
    let theta = trainNN X y layers 3.0 150
  
    printfn "accuracy of train set is %e" (accuracy theta X y layers)
    printfn "accuracy of validation set is %e" (accuracy theta valX valY layers)
    0 
