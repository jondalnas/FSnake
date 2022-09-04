open System

module ConsoleInterface =
    (* Writes a character to the screen *)
    let WriteScreen (x: char) =
        Console.Write x
    
    (* Moves the cursor the a given x and y coordinate *)
    let MoveCursor (x, y) =
        Console.SetCursorPosition (x, y)

    (* Clears the screen *)
    let ClearScreen =
        Console.Clear()

module KeyBoard =
    (* Returns the key which is currently being pressed *)
    let ReadKey () =
        if Console.KeyAvailable then Console.ReadKey().Key
        else ConsoleKey.Sleep

    (* Checks if a certain key is down *)
    let GetKeyDown (x: ConsoleKey) =
        let key = ReadKey()
        if key = x then true
        else false

module Console =
    (* Moves the cursor to the specified location and prints fruit character *)
    let FruitAtLocation (x, y) =
        ConsoleInterface.MoveCursor (x, y)
        ConsoleInterface.WriteScreen '@'

    (* Draws a border from 0, 0 to w, h *)
    let DrawBorder (w, h) =
        let rec Border = function
            | (0, 0, _, _) -> ConsoleInterface.WriteScreen('#')
            | (0, y, w, h) -> ConsoleInterface.WriteScreen('#'); ConsoleInterface.WriteScreen('\n'); Border (w, y-1, w, h)
            | (x, 0, w, h) -> ConsoleInterface.WriteScreen('#'); Border (x-1, 0, w, h)
            | (x, y, w, h) when x = w -> ConsoleInterface.WriteScreen('#'); Border (x-1, y, w, h)
            | (x, y, w, h) when y = h -> ConsoleInterface.WriteScreen('#'); Border (x-1, y, w, h)
            | (x, y, w, h) -> ConsoleInterface.WriteScreen(' '); Border (x-1, y, w, h)
        Border(w-1, h-1, w-1, h-1)

    let rec DrawSnake = function
        | [] -> ()
        | first::tail -> ConsoleInterface.MoveCursor first; ConsoleInterface.WriteScreen '*'; DrawSnake tail

module Input =
    (* Returns if the program should exit or not *)
    let ShouldExit () =
        KeyBoard.GetKeyDown (ConsoleKey.Q)

module Game =
    let rec MainLoop() =
        Console.FruitAtLocation (1,1)
        let snake = [(5,5); (5,6); (6,6); (6,7); (6,8); (7,8); (8,8); (8,7); (7,7)]
        Console.DrawSnake snake
        let exit = Input.ShouldExit()
        if not exit then MainLoop()

[<EntryPoint>]
let main args =
    ConsoleInterface.ClearScreen
    Console.DrawBorder(10, 10)
    Game.MainLoop()
    0