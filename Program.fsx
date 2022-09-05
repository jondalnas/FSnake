open System

module ListTools =
    let rec ReplaceFront elm = function
        | [] -> []
        | front::tail -> elm::ReplaceFront front tail

    let rec Contains elm = function
        | head::_ when head=elm -> true
        | _::tail -> Contains elm tail
        |_ -> false

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

module Keyboard =
    (* Read all keys pressed since last frame *)
    let rec ReadKeyboard () =
        if Console.KeyAvailable then Console.ReadKey(false).Key::ReadKeyboard()
        else []

    (* Checks if a certain key is down *)
    let GetKeyDown (keyboardList, key:ConsoleKey) =
        ListTools.Contains key keyboardList

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
    let ShouldExit (keyboardList) =
        Keyboard.GetKeyDown (keyboardList, ConsoleKey.Q)

    (* Returns which direction player should move *)
    let Direction (keyboardList) =
        if Keyboard.GetKeyDown (keyboardList, ConsoleKey.RightArrow) then 0
        elif Keyboard.GetKeyDown (keyboardList, ConsoleKey.DownArrow) then 1
        elif Keyboard.GetKeyDown (keyboardList, ConsoleKey.LeftArrow) then 2
        elif Keyboard.GetKeyDown (keyboardList, ConsoleKey.UpArrow) then 3
        else -1

module Game =
    let rec MainLoop(snake: (int*int) list, dir) =
        (* Read keyboard *)
        let keyboardList = Keyboard.ReadKeyboard()

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
        Threading.Thread.Sleep(250)

        (* If exit button is not pressed, then call MainLoop *)
        let exit = Input.ShouldExit(keyboardList)
        if not exit then 
            (* Update snake direction *)
            let sd = Input.Direction(keyboardList)
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