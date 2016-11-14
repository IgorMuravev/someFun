open System

let countEp = 10.0
let N = 2
let mainEps = 0.4
let mutable eps = 0.4
let t = 100.0
let minValue = 0.0
let maxValue = 1.0
let rnd = new Random()

//#region teachPairInit
let listInit() =
    [
        for i in 1..N do
            yield minValue + rnd.NextDouble() * (maxValue - minValue)
    ]

let maxL(L) =
    let max = List.max(L)
    let rec create(max,list,accum) =
        match list with
           | [] -> accum
           | head :: tail when head = max ->
                create(max, tail, accum @ [1.0])
           | head :: tail ->
                create(max, tail, accum @ [0.0])
    create(max, L, [])
           

let genTeachPairs(count) =
    let rec generator(count, accum) =    
        match count with
            | 0 -> accum
            | _ ->
                let bufList = listInit()
                let max = maxL(bufList) 
                generator(count - 1, (bufList, max) :: accum)
    generator(count, [])
//#endregion

//region netwrork
let initNeuron() = 
    [
        for i in 1..N do
            yield 0.0
    ]

let neuronResult(perc, input) =
    List.zip perc input |> List.fold (fun acc (fst, snd) -> acc + fst * snd) 0.0

let activateNeuron(x:float) =
   if x > 0.0 then 
    1.0
   else
    0.0

let getNetwork =
    [
        for i in 1..N do
            yield initNeuron()
    ]

let queryNetwork(network, input) =
    let rec loop(network, input, accum) =
        match network with
            | [] -> accum
            | neuron :: tail ->
                let active = activateNeuron(neuronResult(neuron, input))
                loop(tail, input, accum @ [active])
    loop(network, input, [])

let teachNet(network, teachPair) =
    
    let rec recalcNeuron(neuron, delta, input ,accum) =
        match neuron with
            | [] -> accum
            | neuron::net ->
                let x, ainput = List.head(input), List.tail(input)
                let newWeight = neuron + delta * x
                recalcNeuron(net, delta , ainput ,accum @ [newWeight])

    let rec loop(network, correct, answer, input, accum) =
        match network with
            | [] -> accum
            | _ ->
                  let d, acorrect = List.head(correct), List.tail(correct)
                  let y, aanswer = List.head(answer), List.tail(answer)
                  let neuron, aneuron = List.head(network), List.tail(network)
                  let delta = eps * (d - y)
                  let newNeuron = recalcNeuron(neuron, delta, input, [])
                  loop(aneuron, acorrect, aanswer, input, accum @ [newNeuron])

    let answer = queryNetwork(network, fst teachPair)
    loop(network, snd teachPair, answer, fst teachPair, [])                        
 
 
let teachByAll(network, pairs) =
    let rec teach(network, pairs) =
        match pairs with
           | [] -> network
           | pair :: tail ->
                let net = teachNet(network, pair)
                teach(net, tail)
    teach(network, pairs)
                  


let teachByEp(net) =
    let teachPairs = genTeachPairs(100000)      
      
    let rec epoTeach(net, ep) =
        eps <- mainEps / ep
        let teachedNetwork = teachByAll(net, teachPairs)
        match ep with
            | _  when ep > countEp -> teachedNetwork
            | _ -> epoTeach(teachedNetwork, ep + 1.0)

    epoTeach(net, 1.0)

          
//end region

[<EntryPoint>]
let main argv = 
  
    let network = getNetwork
    printfn "%A" network

    let teachedNetwork = teachByEp(getNetwork)
    printfn "%A" teachedNetwork

    let answer = queryNetwork(teachedNetwork, [0.5 ; 0.3])
    printfn "%A" answer

    let answer = queryNetwork(teachedNetwork, [0.2 ; 0.3] )
    printfn "%A" answer

    let answer = queryNetwork(teachedNetwork, [0.1 ; 0.05])
    printfn "%A" answer
    0 
