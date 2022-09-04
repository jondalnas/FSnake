open System

module ListTools =
    let rec ReplaceFront elm = function
        | [] -> []
        | front::tail -> elm::ReplaceFront front tail

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

    (* Draws a snake recursively *)
    let rec DrawSnake = function
        | [] -> ()
        | first::tail -> ConsoleInterface.MoveCursor first; ConsoleInterface.WriteScreen '*'; DrawSnake tail

    (* Clears the old snake recursively *)
    let ClearOldSnake snake =
        ConsoleInterface.MoveCursor snake
        ConsoleInterface.WriteScreen ' '

module Input =
    (* Returns if the program should exit or not *)
    let ShouldExit () =
        KeyBoard.GetKeyDown (ConsoleKey.Q)

    (* Returns which direction player should move *)
    let Direction () =
        if KeyBoard.GetKeyDown (ConsoleKey.RightArrow) then 0
        elif KeyBoard.GetKeyDown (ConsoleKey.DownArrow) then 1
        elif KeyBoard.GetKeyDown (ConsoleKey.LeftArrow) then 2
        elif KeyBoard.GetKeyDown (ConsoleKey.UpArrow) then 3
        else -1

module Game =
    let rec MainLoop(snake: (int*int) list, dir) =
        (* Snake game logic *)
        (* Move snake *)
        let snakeFront = snake[0]
        let (sfx, sfy) = snakeFront
        let mutable newSnake = snake
        match dir with
        | 0 -> newSnake <- ListTools.ReplaceFront (sfx + 1, sfy) newSnake
        | 1 -> newSnake <- ListTools.ReplaceFront (sfx, sfy + 1) newSnake
        | 2 -> newSnake <- ListTools.ReplaceFront (sfx - 1, sfy) newSnake
        | _ -> newSnake <- ListTools.ReplaceFront (sfx, sfy - 1) newSnake

        (* Draw everything *)
        Console.FruitAtLocation (1,1)
        
        Console.ClearOldSnake snake[snake.Length-1]
        Console.DrawSnake newSnake

        (* Sleep before next update *)
        Threading.Thread.Sleep(1000)

        (* If exit button is not pressed, then call MainLoop *)
        let exit = Input.ShouldExit()
        if not exit then 
            (* Update snake direction *)
            let sd = Input.Direction()
            if sd = -1 then
                MainLoop(newSnake, dir)
            else
                MainLoop(newSnake, sd)

[<EntryPoint>]
let main args =
    ConsoleInterface.ClearScreen
    Console.DrawBorder(13, 13)
    Game.MainLoop ([(7,6); (6,6); (5,6)], 0)
    0