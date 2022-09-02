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
    (* Moves the cursor to the specified location and prints player character *)
    let PlayerCharAtLocation (x, y) =
        ConsoleInterface.MoveCursor (x, y)
        ConsoleInterface.WriteScreen '@'

module Input =
    (* Returns if the program should exit or not *)
    let ShouldExit () =
        KeyBoard.GetKeyDown (ConsoleKey.Q)

module Game =
    let rec MainLoop() =
        Console.PlayerCharAtLocation (1,1)
        let exit = Input.ShouldExit()
        if not exit then MainLoop()

[<EntryPoint>]
let main args =
    ConsoleInterface.ClearScreen
    Game.MainLoop()
    0