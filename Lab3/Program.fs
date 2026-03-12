open System
open System.IO

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
    | _ -> 0

let mapFun list =
    list
    |> Seq.map romeToInt

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
    |> Seq.fold (fun acc x -> 
        if x % 2 = 0 then
            acc * 10 + x
        else
            acc
        ) 0

let findRarestExtension path =
    Directory.GetFiles(path, "*.*", SearchOption.AllDirectories)
    |> Seq.map Path.GetExtension
    |> Seq.groupBy (fun x -> x)
    |> Seq.map (fun (ext, files) -> ext, Seq.length files)
    |> Seq.minBy snd
    
[<EntryPoint>]
let main argvs =
    printfn "Введите номер задания"
    let task = Console.ReadLine()
    match task with
    | "1" ->
        printfn "Введите список из римских цифр"
        let listRome = inputRome() |> mapFun
        printf "Список римских цифр переведённых на десятичное представление: "
        for n in listRome do printf $"{n} "
    | "2" ->
        printfn "Введите список из десятичных цифр"
        let listToFold = inputInt() |> foldFun
        printfn "Число из десятичных цифр списка: %i" listToFold
    | "3" ->
        printf "Введите путь к каталогу: "
        let path = Console.ReadLine()
        let ext, count = findRarestExtension path
        printfn "Самое редкое расширение: %s (%d файлов)" ext count
    | _ ->
        ()
    0