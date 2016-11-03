open System

let N = 10
let t = 100.0
let minValue = -100.0
let maxValue = 100.0
let rnd = new Random()

//#region teachPairInit
let listInit =
    [
        for i in 1..N do
            yield minValue + rnd.NextDouble() * (maxValue - minValue)
    ]

let maxL(L) =
    List.max(L)

let rec genTeachPairs(count) =
    let bufList = listInit
    let max = maxL(bufList)
    match count with
        | _ when count > 1 ->
            [(bufList, max)] @ genTeachPairs(count - 1)
        | _ ->
            [(bufList, max)]  
//#endregion

//region netwrork
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

let activateFunc(x:float) =
    1.0 / (1.0 + exp(-t * x))

let percResult(perc, input) =
    List.zip perc input |> List.fold (fun acc (fst, snd) -> acc + fst * snd) 0.0

let rec netResult(nw , input) =
    match nw with
        | [] -> []
        | head :: tail ->
            let res = percResult(head, input) |> activateFunc
            res :: netResult(tail, input)
          

//end region

[<EntryPoint>]
let main argv = 
    let network = getNetwork
    let teachPairs =genTeachPairs(100)


    0 // return an integer exit code
