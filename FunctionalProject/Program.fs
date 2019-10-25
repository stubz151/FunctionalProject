// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.


//Question 1A 

let r = System.Random()  

let rec sumHelper func a b maxValue count countAbove  =
    if (count = 100000) then countAbove
                        else let x = r.Next(a,b+1)
                             let y = r.Next(maxValue)
                             if (y < func x ) then sumHelper func a b maxValue (count+1) (countAbove+1.0)
                                               else sumHelper func a b maxValue (count+1) (countAbove)

let rec maxHelper func a b count highest  =
    if (count = 10000) then highest
                        else let x = r.Next(a,b+1)
                             let result = func x 
                             if (result > highest) then maxHelper func a b (count+1) result
                                                   else maxHelper func a b (count+1) highest
         
let func1 x=
    x*2

//maxHelper func1 1 2 0 1   

        


let integrate func a b =
    let max = maxHelper func a b 0 0     
    let area  = max * (b-a)
    let countPoints = sumHelper func a b max 0 0.0
    (countPoints/100000.00) *  float(area)

integrate func1 1 5  




