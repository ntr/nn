module NN

open Microsoft.FSharp.Math
open CheckGradient
open MatrixUtils
open GradCost

let allocateThetas thetasVector layers=
    let thetaAlloc (prevVal, totalAllocated, thetasList) value =
        match prevVal with
        | 0 -> (value, totalAllocated, thetasList) //allocating theta only on a second list element
        | _ -> (value, totalAllocated + value * (prevVal+1), (reshape thetasVector totalAllocated value (prevVal+1)) :: thetasList)
    let (_,_, thAll) = layers |> List.fold thetaAlloc (0,0,[])
    thAll

let calcLayers thAll X = 
    //calculates nn layers for theta
    let getAZ (A,Z) value = //calculates a(i-1) and z(i) for ith layer
        let nextZ a theta =
            a * (!theta)
        let a = match Z with
                | [] -> addBiasColumn X
                | z::_ -> z |> Matrix.map sigmoid |> addBiasColumn
        a::A, nextZ a value::Z
    thAll |> List.rev|> List.fold getAZ ([], []) //todo lot of time here - review

let nnCost layers X y lambda thetasVector = 
    //calculates cost in last layer with regularization
    let calculateCost zippedHY lambda th m = 
      let GetJ (yi,hi) = 
        let tmp = (hi |> Vector.map (fun x -> log(1.0-x))).[0]
        let v =  (1.0 / float m) * (-(!yi) * (hi |> Vector.map log) - !(1.0.-yi) * (hi |> Vector.map (fun x -> log(1.0-x)))) 
        v.[0]

      let reg = th |> List.sumBy (fun m -> m |> Matrix.map (fun i -> i*i) |> Matrix.sum)
      let J = (zippedHY |> List.sumBy GetJ) + ((lambda * reg)/(2.0 * float m))
      J

    //calculates gradient using backpropagation algorithm
    let calculateGrad zippedHY m a z thetasAll thetasZeroed= 
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
        getThetaGradient lastD a (z |>Seq.skip 1 |> Seq.toList) thetasAll thetasZeroed 
    
    let thetas = allocateThetas thetasVector layers    
    let (a,z) = calcLayers thetas X
    let h = z |> List.head |> Matrix.map sigmoid
    let examplesCount = X.NumRows
    let num_labels = layers |> Seq.last
    let thetasZeroed = thetas |> List.map setFirstColZeros

    let zippedHY = [0..num_labels-1] 
                    |> List.map (fun i -> (y |> Matrix.map (fun el -> if el = float i then 1.0 else 0.0)), h.Column(i))
                    
    (calculateCost zippedHY lambda thetasZeroed examplesCount, unroll (calculateGrad zippedHY examplesCount a z thetas thetasZeroed))

let trainNN X y layers lambda numiters = 
    let rnd =
        let r = new System.Random()
        let eps = 0.12
        let getv _ _ = 
            r.NextDouble() * 2.0 * eps - eps
        fun x y -> Matrix.init x y getv

    let initialThetas = layers |> Seq.windowed 2 |> Seq.map (fun arr -> rnd arr.[1] (arr.[0] + 1)) |> Seq.toList |> unroll
    let (thRes, lst,ind) = checkGradient (nnCost layers X y lambda) initialThetas (Some numiters)
    thRes

let accuracy thetasVector X (y:matrix) layers =
    let thetas = allocateThetas thetasVector layers    
    let (a,z) = calcLayers thetas X 
    let h2 = z |> List.head |> Matrix.map sigmoid

    let getMax i (maxind, maxv) v = 
        if v>=maxv then (i,v) else (maxind, maxv)
    seq { for i in 0..h2.NumRows-1 do
            yield h2.Row i |> RowVector.transpose |> Vector.foldi getMax (-1,-1.0) } 
        |> Seq.mapi (fun i x ->  if (y.[i,0] = float (fst x)) then 1.0 else 0.0)  |> Seq.average