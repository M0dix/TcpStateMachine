open System

//1. Определяем состояния с помощью типа-союза
type State = 
    | CLOSED
    | LISTEN
    | SYN_SENT
    | SYN_RCVD 
    | ESTABLISHED
    | CLOSE_WAIT
    | LAST_ACK
    | FIN_WAIT_1
    | FIN_WAIT_2
    | CLOSING
    | TIME_WAIT
    | ERROR

//2. Определяем события с помощью типа-союза
type Event =
    | APP_PASSIVE_OPEN
    | APP_ACTIVE_OPEN
    | APP_SEND
    | APP_CLOSE
    | APP_TIMEOUT
    | RCV_SYN
    | RCV_ACK
    | RCV_SYN_ACK
    | RCV_FIN
    | RCV_FIN_ACK

//3. Создаём функцию для парсинга строки в определённое событие
//   c учётом ошибки ввода
let stringToEvent = function
    | "APP_PASSIVE_OPEN" -> APP_PASSIVE_OPEN
    | "APP_ACTIVE_OPEN" -> APP_ACTIVE_OPEN
    | "APP_SEND" -> APP_SEND
    | "APP_CLOSE" -> APP_CLOSE
    | "APP_TIMEOUT" -> APP_TIMEOUT
    | "RCV_SYN" -> RCV_SYN
    | "RCV_ACK" -> RCV_ACK
    | "RCV_SYN_ACK" -> RCV_SYN_ACK
    | "RCV_FIN" -> RCV_FIN
    | "RCV_FIN_ACK" -> RCV_FIN_ACK
    | _ -> failwith "Неизвестное событие"

//4. Создаем карту с правилами перехода событий, карта сосоит из кортежа
//   с текущим состоянием и событием, возвращает новое состояние
let transitionMap = 
    Map.ofList [
        (CLOSED, APP_PASSIVE_OPEN), LISTEN
        (CLOSED, APP_ACTIVE_OPEN), SYN_SENT
        (LISTEN, RCV_SYN), SYN_RCVD
        (LISTEN, APP_SEND), SYN_SENT
        (LISTEN, APP_CLOSE), CLOSED
        (SYN_RCVD, APP_CLOSE), FIN_WAIT_1
        (SYN_RCVD, RCV_ACK), ESTABLISHED
        (SYN_SENT, RCV_SYN), SYN_RCVD
        (SYN_SENT, RCV_SYN_ACK), ESTABLISHED
        (SYN_SENT, APP_CLOSE), CLOSED
        (ESTABLISHED, APP_CLOSE), FIN_WAIT_1
        (ESTABLISHED, RCV_FIN), CLOSE_WAIT
        (FIN_WAIT_1, RCV_FIN), CLOSING
        (FIN_WAIT_1, RCV_FIN_ACK), TIME_WAIT
        (FIN_WAIT_1, RCV_ACK), FIN_WAIT_2
        (CLOSING, RCV_ACK), TIME_WAIT
        (FIN_WAIT_2, RCV_FIN), TIME_WAIT
        (TIME_WAIT, APP_TIMEOUT), CLOSED
        (CLOSE_WAIT, APP_CLOSE), LAST_ACK
        (LAST_ACK, RCV_ACK), CLOSED
    ]


//5. Создаём функцию для обработки событий 
//   В ней используется рекурсивная функция aux, которая принимает 
//   текущее состояние и список событий (неявно)
let processEvents events = 
    let rec aux state = function
        | [] -> state
        | event::tail ->
            match Map.tryFind (state, event) transitionMap with
                | Some newState -> aux newState tail
                | None -> ERROR
    //Начальный вызов рекурсивной функции с состоянием "CLOSED"
    aux CLOSED events


//6.  Точка входа в приложение
//    Запрашивается ввод событий от пользователя и выводит конечное состояние
[<EntryPoint>]
let main argv =
    let rec loop () = 
        printfn "Введите события через запятую (например, APP_PASSIVE_OPEN,APP_SEND,RCV_SYN_ACK) или нажмите Enter для выхода:"
        let input = Console.ReadLine()
        if input.Trim() <> "" then
            try 
                let events = input.Split(',') |> Array.toList |> List.map stringToEvent
                let finalState = processEvents events
                printfn "Конечное состояние TCP-автомата: %A" finalState 
            with
            | ex -> printfn "Ошибка: %s" ex.Message
            loop()
    loop()
    0