module Diagnostics

let computeNumericalGradient J (theta:matrix) = 
    let e = 1e-4;
    let numels = theta.NumCols * theta.NumRows
    let add i v = 
        theta |> Matrix.mapi (fun x y item -> if x = i then item + v else item)

    let numgrad = seq { for i in 0.. numels do
                        let t1 = add i e
                        let t2 = add i -e
                        yield (J(t1) - J(t2)) /(2.0 * e) }
    numgrad 

//used for checking gradient part of cost function, comparing with numerical gradient
let compareWithNumericalGradient costf theta= 
   let c,g = costf theta
   let getDiff i v = 
       let gradValue = Matrix.get g i 0
       (gradValue, gradValue - v)
   computeNumericalGradient (costf >> fst) theta 
                   |> Seq.take 10  
                   |> Seq.mapi getDiff |> Seq.iter (fun (v, diff) -> printfn "value %e, diff %e (should be less then e-10)" v diff)

