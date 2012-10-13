module MatrixUtils

open System.IO
open System

let size (M:matrix) = 
    (M.NumRows, M.NumCols)

let sqr (M:matrix) = 
    (M |> Matrix.transpose) * M

let (!) (M:matrix) = 
    (M |> Matrix.transpose)

let unroll x = 
    let rec getLst x = 
        let mtl m = 
            m |> Matrix.fold (fun l i -> i::l) [] |> List.rev
        match x with
        | h::t -> mtl h @ getLst t
        | [] -> []
    x |> getLst |> Vector.ofList |> Matrix.ofVector

let reshape (M:matrix) from rows columns = 
    Matrix.init rows columns (fun r c -> M.[from + r * columns + c,0])

let addBiasColumn (M:matrix) = 
    let res = Matrix.create M.NumRows (M.NumCols+1) 1.0
    res.[0..M.NumRows-1, 1..M.NumCols] <- M
    res

let removeFirstColumn (m:matrix) = 
    m.GetSlice(None, None, Some(1), None)

let load path (del:Char) = 
    let csvLineToFloatArr (line:string) = 
        line.Split([|del|], StringSplitOptions.RemoveEmptyEntries) |> Seq.map float
    File.ReadLines path 
            |> Seq.filter (String.IsNullOrWhiteSpace >> not)
            |> Seq.map csvLineToFloatArr 
            |> Matrix.ofSeq
  
let normalize (x:Vector<float>) = 
    let stddevMean nums = 
        let sqr x = x * x
        let mean = nums |> Seq.average
        let variance = nums |> Seq.averageBy (fun x -> sqr(x - mean))
        (mean, sqrt(variance))
    let m,s = x |> stddevMean
    let norm = x |> Vector.map (fun i -> (i - m)/s)
    (norm,m,s)

let mapXToPolynomial (M:matrix) d =
    let makePolynomial (x:float) (y:float) d = 
        seq { for i in 1..d do 
                for j in 0..i do 
                    yield (x**float (i-j)) * (y**float j) } 
    let l = seq { for i in 0..M.NumRows-1 do 
                    yield (M.[i,0], M.[i,1]) }
              |> Seq.map (fun (x, y) ->  1.0::(makePolynomial x y d |> Seq.toList))
    l