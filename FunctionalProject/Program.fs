open System

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.


//Question 1A 

let r = System.Random()  

let rec sumHelper func a b maxValue count countAbove  =
    if (count = 20000) then countAbove
    //if (count = 100000) then countAbove
                        else let x = a + (b - a) * r.NextDouble()
                             let y = r.NextDouble() * maxValue                        
                             if (y < func x ) then sumHelper func a b maxValue (count+1) (countAbove+1.0)
                                               else sumHelper func a b maxValue (count+1) (countAbove)

let rec maxHelper func a b count highest  =
    if (count = 10000) then highest
                       else let x = a + (b - a) * r.NextDouble()                                                     
                            let result = func x 
                            if (result > highest) then maxHelper func a b (count+1) result
                                                  else maxHelper func a b (count+1) highest
 
let threadHelper func a b maxValue=
    let x = async {return sumHelper func a b maxValue 0 0.0}          
           |> Async.RunSynchronously
    x   

let sum lis =
    let rec sumHelper lis s : float = 
        match lis with
        |[] -> s
        |x::xs -> sumHelper xs (s + x)
    sumHelper lis 0.0



let integrate func a b =
    let max = maxHelper func a b 0 0.0     
    let area  = max * (b-a)
    let lg = [for x in 0..6 do yield threadHelper func a b max]
    let total = sum lg
    //let total = sumHelper func a b max 0 0.0
    (total/100000.00) *  area

//just test functions and usages, intgerate is what you should call to test things :) there lines that were commented out to adapt the solution to use threads.
let func1 x=
    0.5 *x *x

integrate func1 1.0 2.0  


//Question 2a
let la = [1 .. 10]
           
let rec succHelper la x1 x2 x3 x4 =
    match la with     
    |a::b::c::d::xs -> if a*b*c*d > x1 *x2 *x3 *x4 then succHelper (b::c::d::xs) a b c d
                                                   else succHelper (b::c::d::xs) x1 x2 x3 x4
    |_ -> ("" + x1.ToString()+"*"+x2.ToString()+"*"+x3.ToString() + "*" + x4.ToString() , x1 *x2 *x3 *x4)     
                                 

let successMultiples lis = 
    succHelper lis 1 -999999 1 1
    

successMultiples la

//Question 2b

let rec sumListUnless7 lis total =
    match lis with
    |[] -> total
    |x::xs -> if x%7 =0 then sumListUnless7 xs total 
                        else sumListUnless7 xs (total+x) 

let concatList lis=
    let D = [for a in lis do yield (a.ToString() + a.ToString())|> int]    
    sumListUnless7 D 0    

concatList la