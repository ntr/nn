//Initial code was taken fron Mathlab/Octave checkGrad function under following copyright:
//
// Copyright (C) 2001 and 2002 by Carl Edward Rasmussen. Date 2002-02-13
//
//
// (C) Copyright 1999, 2000 & 2001, Carl Edward Rasmussen
// 
// Permission is granted for anyone to copy, use, or modify these
// programs and accompanying documents for purposes of research or
// education, provided this copyright notice is retained, and note is
// made of any changes that have been made.
// 
// These programs and documents are distributed without any warranty,
// express or implied.  As the programs were written for research
// purposes only, they have not been tested to the degree that would be
// advisable in any important application.  All use of these programs is
// entirely at the user's own risk.
module CheckGradient
open MatrixUtils
open  Microsoft.FSharp.Math

open Microsoft.FSharp.Data.TypeProviders
open MathProvider

module La = LinearAlgebra

exception Error of string

let safeSingle m = 
    let x,y = size m
    if (x <> 1) then raise (Error(sprintf "expected 1 row but %d present" x))
    if (y <> 1) then raise (Error(sprintf "expected 1 column but %d present" y))
    m.[0,0]

let horAppend m1 m2 =
    m1

let checkGradient gc X numiters= 
    let bti x = 
        if x then 1 else 0
    let isreal x =
        true

    let length = match numiters with
        | Some(i) -> i
        | _ -> 400

    let RHO = 0.01;                            // a bunch of constants for line searches
    let SIG = 0.5;       // RHO and SIG are the constants in the Wolfe-Powell conditions
    let INT = 0.1;    // don't reevaluate within 0.1 of the limit of the current bracket
    let EXT = 3.0;                    // extrapolate maximum 3 times the current bracket
    let MAX = 20;                         // max 20 function evaluations per line search
    let RATIO = 100.0;                                      // maximum allowed slope ratio

    //missed block
    //let length = 1;
    let red =1.0;

    let mutable ls_failed = false;                             // no previous line search has failed
    let mutable fX = [];
    let mutable f1, df1 = gc X
    let i = 0
    let mutable i = if length < 0 then i+1 else i                // count epochs?!
    let mutable s = -df1;                                        // search direction is steepest
    let mutable d1 = - sqr(s) |> safeSingle//* // this is the slope
    let mutable z1 = red/(1.0-d1);                                  // initial step is red/(|s|+1)
    let mutable X = X;
    let mutable outerCont = true;
    while i < abs(length) && outerCont do                                      // while not finished
        i <- i + bti(length>0);                                      // count iterations?!
        let X0 = X; 
        let f0 = f1; 
        let df0 = df1;                  // make a copy of current values
        X <- X + z1*s;                                             // begin line search
        let mutable f2, df2 = gc X
        //printfn "0: %s %4i | Cost: %8.12e\r"  "Iteration" i f2;
        i <- i + bti(length<0);                                          // count epochs?!
        let mutable d2 = (df2 |> Matrix.transpose)*s |> safeSingle;
        let mutable f3 = f1; 
        let mutable d3 = d1; 
        let mutable z3 = -z1;             // initialize point 3 equal to point 1
        let mutable M = (if length>0 then MAX else min MAX (-length-i));
        let mutable success = false; 
        let mutable limit = -1.0;                     // initialize quanteties
        
        let mutable cont = true;
        while cont do
            let mutable z2 = 0.0;
            while (((f2 > f1+z1*RHO*d1) || (d2 > -SIG*d1)) && (M > 0)) do
              limit <- z1;                                         // tighten the bracket
              if (f2 > f1) then
                z2 <- z3 - (0.5*d3*z3*z3)/(d3*z3+f2-f3);                 // quadratic fit
              else
                let A = 6.0*(f2-f3)/z3+3.0*(d2+d3);                                 // cubic fit
                let B = 3.0*(f3-f2)-z3*(d3+2.0*d2);
                z2 <- (sqrt(B*B-A*d2*z3*z3)-B)/A;       // numerical error possible - ok!
              if ( z2=nan || z2 = infinity) then
                z2 <- z3/2.0;                  // if we had a numerical problem then bisect
              let tmp1= min z2 (INT*z3)
              let tmp2 = (1.0-INT)*z3
              z2 <- max tmp1 tmp2;  // don't accept too close to limits
              z1 <- z1 + z2;                                           // update the step
              X <- X + z2*s;
              let res = gc X
              f2 <- fst res
              df2 <- snd res
             // printfn "1: %s %4i | Cost: %8.12e\r"  "Iteration" i f2;
              M <- M - 1; 
              i <- i + bti(length<0);                           // count epochs?!
              d2 <- (df2 |> Matrix.transpose)*s |> safeSingle;
              z3 <- z3-z2;                    // z3 is now relative to the location of z2
            if (f2 > f1+z1*RHO*d1) || (d2 > -SIG*d1) then
              cont <- false                                           // this is a failure
            elif (d2 > SIG*d1) then
              success <- true 
              cont <- false                                              // success
            elif M = 0 then
              cont <- false                                                     // failure

            if cont then
               let A = 6.0*(f2-f3)/z3+3.0*(d2+d3);                      // make cubic extrapolation
               let B = 3.0*(f3-f2)-z3*(d3+2.0*d2);
               z2 <- -d2*z3*z3/(B+sqrt(B*B-A*d2*z3*z3));        // num. error possible - ok!
               if not (isreal z2) || z2 = nan || z2 = infinity || z2 < 0.0 then   // num prob or wrong sign?
                 if limit < -0.5 then                               // if we have no upper limit
                   z2 <- z1 * (EXT-1.0);                // the extrapolate the maximum amount
                 else
                   z2 <- (limit-z1)/2.0;                                   // otherwise bisect
               elif (limit > -0.5) && (z2+z1 > limit) then         // extraplation beyond max?
                 z2 <- (limit-z1)/2.0;                                              // bisect
               elif (limit < -0.5) && (z2+z1 > z1*EXT) then       // extrapolation beyond limit
                 z2 <- z1*(EXT-1.0);                           // set to extrapolation limit
               elif z2 < -z3*INT then
                 z2 <- -z3*INT;
               elif (limit > -0.5) && (z2 < (limit-z1)*(1.0-INT)) then   // too close to limit?
                 z2 <- (limit-z1)*(1.0-INT);
               f3 <- f2; d3 <- d2; z3 <- -z2;                  // set point 3 equal to point 2
               z1 <- z1 + z2; X <- X + z2*s;                      // update current estimates
               let res = gc X
               f2 <- fst res
               df2 <- snd res
             //  printfn "2: %s %4i | Cost: %8.12e\r"  "Iteration" i f2;
               M <- M - 1; i <- i + bti(length<0);                             // count epochs?!
               d2 <- (df2 |> Matrix.transpose)*s |> safeSingle;
        if success then                                        // if line search succeeded
          f1 <- f2;
          let tmp = f1
          fX <- f1 :: fX
          printfn "R: %s %4i | Cost: %8.12e\r"  "Iteration" i f1;

          let inverted = La.inv (sqr df1) |> safeSingle
          let tmpM = (sqr df2-(df1 |> Matrix.transpose)*df2)|> safeSingle
          s <-  (tmpM*inverted)*s - df2;      // Polack-Ribiere direction


          let tmp = df1; 
          df1 <- df2;
          df2 <- tmp;                         // swap derivatives
          d2 <- (df1 |> Matrix.transpose)*s |> safeSingle;
          if d2 > 0.0 then                                      // new slope must be negative
            s <- -df1;                              // otherwise use steepest direction
            d2 <- -sqr s |> safeSingle; 
             
          z1 <- z1 * min RATIO (d1/(d2-System.Double.Epsilon));          // slope ratio but max RATIO
          d1 <- d2;
          ls_failed <- false;                              // this line search did not fail
        else 
          X <- X0; f1 <- f0; df1 <- df0;  // restore point from before failed line search
          if ls_failed || i > abs(length) then        // line search failed twice in a row
            outerCont <- false
          else                             // or we ran out of time, so we give up
            let tmp = df1; 
            df1 <- df2; df2 <- tmp;                         // swap derivatives
            s <- -df1;                                                    //try steepest
            d1 <- -sqr s |> safeSingle;
            z1 <- 1.0/(1.0-d1);                     
            ls_failed <- true;                                   // this line search failed

    (X, fX, i)