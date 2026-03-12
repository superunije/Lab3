open System

let rec inputRome () = 
    printfn "Введите римскую цифру"
    let input = Console.ReadLine()
    match input with
    | "I" ->
        "I" :: inputRome()
    | "II" ->
        "II" :: inputRome()
    | "III" ->
        "III" :: inputRome()
    | "IV" ->
        "IV" :: inputRome()
    | "V" ->
        "V" :: inputRome()
    | "VI" ->
        "VI" :: inputRome()
    | "VII" ->
        "VII" :: inputRome()
    | "VIII" ->
        "VIII" :: inputRome()
    | "IX" ->
        "IX" :: inputRome()
    | "" ->
        []
    | _ ->
        printfn "Введена не римская цифра I - IX"
        inputRome()

let romeToInt x =
    match x with
    | "I" -> 1
    | "II" -> 2
    | "III" -> 3
    | "IV" -> 4
    | "V" -> 5
    | "VI" -> 6
    | "VII" -> 7
    | "VIII" -> 8
    | "IX" -> 9

let mapFun list =
    list
    |> List.map romeToInt

let rec inputInt () =
    printfn "Введите десятичную цифру"
    let input = Console.ReadLine()
    if input = "" then
        []
    else
        match Int32.TryParse(input) with
        | true, value ->
            if value >= 0 && value <= 9 then
                value :: inputInt()
            else
                printfn "Ошибка! Введите десятичную цифру"
                inputInt()
        | false, _ ->
            printfn "Ошибка! Введите десятичную цифру"
            inputInt()

let foldFun list =
    list
    |> List.fold (fun acc x -> 
        if x % 2 = 0 then
            acc * 10 + x
        else
            acc
        ) 0
    
[<EntryPoint>]
let main argvs =
    printfn "Введите номер задания"
    let task = Console.ReadLine()
    match task with
    | "1" ->
        printfn "Введите список из римских цифр"
        let listRome = inputRome()
        printfn "Список римских цифр переведённых на десятичное представление: %A" (mapFun listRome)
    | "2" ->
        printfn "Введите список из десятичных цифр"
        let listToFold = inputInt()
        printfn "Число из десятичных цифр списка: %i" (foldFun listToFold)
    | _ ->
        ()
    0