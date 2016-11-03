open System

let N = 10;
let minValue = -100.0;
let maxValue = 100.0;
let rnd = new Random();

//#region teachPairInit
let listInit =
    [
        for i in 1..N do
            yield minValue + rnd.NextDouble() * (maxValue - minValue)
    ]

let maxL(L) =
    List.max(L)

let rec teachPairs(count) =
    let bufList = listInit
    let max = maxL(bufList)
    match count with
        | _ when count > 1 ->
            [(bufList, max)] @ teachPairs(count - 1)
        | _ ->
            [(bufList, max)]  
//#endregion

let initWPerc = 
    [
        for i in 1..N do
            yield rnd.NextDouble() - 1.0
    ]

let getNetwork =
    [
        for i in 1..N do
            yield initWPerc
    ]


[<EntryPoint>]
let main argv = 
    printfn "%A" getNetwork
    0 // return an integer exit code
