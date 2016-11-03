open System

let N = 3
let eps = 0.01
let t = 100.0
let minValue = -100.0
let maxValue = 100.0
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
            yield rnd.NextDouble() - 1.0
    ]

let neuronResult(perc, input) =
    List.zip perc input |> List.fold (fun acc (fst, snd) -> acc + fst * snd) 0.0

let activateNeuron(x:float) =
    1.0 / (1.0 + exp(-t * x))

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
    let answer = queryNetwork(network, fst teachPair)

    let rec loop(neuron, correct, answer, input, accum) =
        match neuron with
            | [] -> accum
            | _ ->
                  let d, acorrect = List.head(correct), List.tail(correct)
                  let y ,aanswer = List.head(answer), List.tail(answer)
                  let x , ainput = List.head(input), List.tail(input)
                  let w , aneuron = List.head(neuron), List.tail(neuron)
                  let delta = eps * (d - y) * x
                  loop(aneuron, acorrect, aanswer,ainput, accum @ [w + delta])
                       
    let rec recalNetwork(network, correct, answer, input, newNet) =
        match network with
            | [] -> newNet
            | neuron::net ->
                let newWeight = loop(neuron , correct, answer, input, [])
                recalNetwork(net, correct, answer, input, newNet @ [newWeight])

    recalNetwork(network,snd(teachPair), answer, fst(teachPair),[])
 
 
let teachByAll(network, pairs) =
    let rec teach(network, pairs) =
        match pairs with
           | [] -> network
           | pair :: tail ->
                let net = teachNet(network, pair)
                teach(net, tail)
    teach(network, pairs)
                  

          
//end region

[<EntryPoint>]
let main argv = 
    let teachPairs = genTeachPairs(100)
    let network = getNetwork
    printfn "%A" network

    let teachedNetwork = teachByAll(network, teachPairs)
    printfn "%A" teachedNetwork

    let answer = queryNetwork(teachedNetwork, [-1.0 ; 2.3 ; 1.3])
    printfn "%A" answer
    0 
