open System
open System.IO

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
    | _ -> 
        printfn ""
        printf "Не римская цифра: %s" x
        0

//let rec inputRome () =
//    seq {
//        printfn "Введите римскую цифру"
//        let input = Console.ReadLine()
//        match input with
//        | "" -> ()
//        | "I" | "II" | "III" | "IV" | "V"
//        | "VI" | "VII" | "VIII" | "IX" ->
//            yield input
//            yield! inputRome()
//        | _ ->
//            printfn "Введена не римская цифра I - IX"
//            yield! inputRome()
//    }

let rec inputRome () =
    printfn "Введите римскую цифру"
    Console.ReadLine().Split(' ')
    |> Seq.map romeToInt
    |> Seq.filter (fun x -> x > 0)

let rec inputInt () =
    seq {
        printfn "Введите десятичную цифру"
        let input = Console.ReadLine()
        if input = "" then
            ()
        else
            match Int32.TryParse(input) with
            | true, value when value >= 0 && value <= 9 ->
                yield value
                yield! inputInt()
            | _ ->
                printfn "Ошибка! Введите цифру от 0 до 9"
                yield! inputInt()
    }

let foldFun seq =
    seq
    |> Seq.fold (fun acc x -> 
        if x % 2 = 0 then
            acc * 10 + x
        else
            acc
        ) 0

let rec findRarestExtension() =
    printf "Введите путь к каталогу: "
    let path = Console.ReadLine()
    if Directory.Exists(path) then
        let files = Directory.EnumerateFiles(path, "*.*", SearchOption.AllDirectories) 
        if (Seq.isEmpty files) then
            printfn "Каталог пустой. Повторите ввод пути"
            findRarestExtension()
        else
            files
            |> Seq.map Path.GetExtension
            |> Seq.groupBy (fun x -> x)
            |> Seq.map (fun (ext, files) -> ext, Seq.length files)
            |> Seq.minBy snd
    else
        printfn "Ошибка. Повторите ввод пути"
        findRarestExtension()
    
[<EntryPoint>]
let main argvs =
    printfn "Введите номер задания"
    let task = Console.ReadLine()
    match task with
    | "1" ->
        printfn "Введите список из римских цифр"
        let seqRome = inputRome()
        printfn "Список римских цифр переведённых на десятичное представление: "
        seqRome |> Seq.iter (fun n -> printf "%i " n)
    | "2" ->
        printfn "Введите список из десятичных цифр"
        let seqToFold = inputInt() |> foldFun
        printfn "Число из десятичных цифр списка: %i" seqToFold
    | "3" ->
        let ext, count = findRarestExtension()
        printfn "Самое редкое расширение: %s (%d файлов)" ext count
    | _ ->
        ()
    0