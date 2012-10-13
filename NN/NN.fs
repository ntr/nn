module NN

open Microsoft.FSharp.Math
open CheckGradient
open MatrixUtils
open GradCost

let allocateThetas thetas layers=
    let thetaAlloc (prevVal, totalAllocated, thetasList) value =
        match prevVal with
        | 0 -> (value, totalAllocated, thetasList) //allocating theta only on a second list element
        | _ -> (value, totalAllocated + value * (prevVal+1), (reshape thetas totalAllocated value (prevVal+1)) :: thetasList)
    let (_,_, thAll) = layers |> List.fold thetaAlloc (0,0,[])
    thAll

let calcAZ thAll X = 
    let getAZ (A,Z) value = //calculates a(i-1) and z(i) for ith layer
        let nextZ a theta =
            a * (!theta)
        let a = match Z with
                | [] -> addBiasColumn X
                | z::_ -> z |> Matrix.map sigmoid |> addBiasColumn
        a::A, nextZ a value::Z
    thAll |> List.rev|> List.fold getAZ ([], []) 

let nnCost layers X y lambda thetas = 
    let calculateCost zippedHY lambda th m = 
      let GetJ (yii,hii) = 
        let tmp = (hii |> Vector.map (fun x -> log(1.0-x))).[0]
        let v =  (1.0 / float m) * (-(!yii) * (hii |> Vector.map log) - !(1.0.-yii) * (hii |> Vector.map (fun x -> log(1.0-x)))) 
        v.[0]

      let reg = th |> List.sumBy (fun m -> m |> Matrix.map (fun i -> i*i) |> Matrix.sum)
      let J = (zippedHY |> List.sumBy GetJ) + ((lambda * reg)/(2.0 * float m))
      J

    let calculateGrad zippedHY m a z thAll= 
        let lastD = zippedHY 
                    |> Seq.map (fun (y,h)-> h-(y|> Matrix.toVector) ) 
                    |> Seq.map Vector.toArray 
                    |> Matrix.ofSeq 
                    |> Matrix.transpose

        let rec getThetaGradient d (a:matrix list) z (thetas:matrix list) (thetasZeroes:matrix list) =
            let calcGrad d (a:matrix) (th:matrix) = 
                 (!d * a)*(1.0/(float m)) + th * (lambda/(float m))
            match (a, z, thetas, thetasZeroes) with 
            | (a::a_,z::z_,t::t_,th::th_)-> 
                let nextD = ((d * t) |> removeFirstColumn) .* (z |> Matrix.map sigmoidGradient)
                getThetaGradient nextD a_ z_ t_ th_ @ [calcGrad d a th]
            | (a::a_,[],t::t_,th::th_) -> [calcGrad d a th]
            | _ -> []
        getThetaGradient lastD a (z |>Seq.skip 1 |> Seq.toList) thAll (thAll |> List.map setFirstColZeros) 
    
    let thAll = allocateThetas thetas layers    
    let (a,z) = calcAZ thAll X
    let h = z |> List.head |> Matrix.map sigmoid
    let m = X.NumRows
    let num_labels = layers |> Seq.last
    let th = thAll |> List.map setFirstColZeros

    let zippedHY = [0..num_labels-1] 
                    |> List.map h.Column
                    |> List.zip ([0..num_labels-1] 
                    |> List.map (fun i -> y |> Matrix.map (fun el -> if el = float i then 1.0 else 0.0))) //each item in list is an outcome for each experiment

    (calculateCost zippedHY lambda th m, unroll (calculateGrad zippedHY m a z thAll))

let trainNN X y layers lambda numiters = 
    let rnd =
        let r = new System.Random()
        let eps = 0.12
        let getv _ _ = 
            r.NextDouble() * 2.0 * eps - eps
        fun x y -> Matrix.init x y getv

    let l1 = layers |> Seq.skip 1
    let initialThetas = layers |> Seq.windowed 2 |> Seq.map (fun arr -> rnd arr.[1] (arr.[0] + 1)) |> Seq.toList |> unroll
    let (thRes, lst,ind) = checkGradient (nnCost layers X y lambda) initialThetas (Some numiters)
    thRes

let accuracy thRes X (y:matrix) layers =
    let thAll = allocateThetas thRes layers    
    let (a,z) = calcAZ thAll X 
    let h2 = z |> List.head |> Matrix.map sigmoid

    let getMax i (maxind, maxv) v = 
        if v>=maxv then (i,v) else (maxind, maxv)
    seq { for i in 0..h2.NumRows-1 do
            yield h2.Row i |> RowVector.transpose |> Vector.foldi getMax (-1,-1.0) } 
        |> Seq.map (fst)
        |> Seq.mapi (fun i x ->  if (y.[i,0] = float x) then 1.0 else 0.0)  |> Seq.average