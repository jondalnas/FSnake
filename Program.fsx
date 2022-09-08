open System

module ListTools =
    let rec ReplaceFront elm = function
        | [] -> []
        | front::tail -> elm::ReplaceFront front tail

    let rec Contains elm = function
        | head::_ when head=elm -> true
        | _::tail -> Contains elm tail
        |_ -> false
    
    let rec Remove elm = function
        | [] -> []
        | x::tail when x = elm -> tail
        | head::tail -> head::Remove elm tail

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
    let rec ReadKeyboard lastKeyList =
        if Console.KeyAvailable then 
            let key = Console.ReadKey(false).Key
            let keyList = ReadKeyboard lastKeyList

            if ListTools.Contains key keyList then
                keyList
            else
                key::keyList
        else lastKeyList

    (* Checks if key is the next in the queue *)
    let GetKeyDown (key:ConsoleKey) = function
        | frontKey::rest when frontKey = key ->  (rest, true)
        | list -> (list, false)

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

module Collision =
    let PlayerColBorder (w, h) = function
        | (x,_) when x=w-1 || x=0 -> true
        | (_,y) when y=h-1 || y=0 -> true
        |_ -> false

    let rec PlayerColPoint (p, snake: (int*int) list) = 
        let head = snake[0]
        let tail = snake.Tail

        if p = head then true
        elif not tail.IsEmpty then PlayerColPoint(p, tail)
        else false

    let rec PlayerColSelf (snake:(int*int) list) =
        let snakeHead = snake[0]
        let snakeTail = snake.Tail

        if snakeTail.IsEmpty then false
        elif PlayerColPoint (snakeHead, snakeTail) then true
        else PlayerColSelf snakeTail


module Input =
    (* Returns if the program should exit or not *)
    let ShouldExit (keyboardList) =
        ListTools.Contains ConsoleKey.Q keyboardList

    (* Returns which direction player should move *)
    let Direction (keyboardList) =
        let (rightList, right) = Keyboard.GetKeyDown ConsoleKey.RightArrow keyboardList
        if right then (0, rightList)
        else
            let (downList, down) = Keyboard.GetKeyDown ConsoleKey.DownArrow keyboardList
            if down then (1, downList)
            else
                let (leftList, left) = Keyboard.GetKeyDown ConsoleKey.LeftArrow keyboardList
                if left then (2, leftList)
                else
                    let (upList, up) = Keyboard.GetKeyDown ConsoleKey.UpArrow keyboardList
                    if up then (3, upList)
                    else (-1, keyboardList)

module Game =
    let rec NewFruit (snake, rand: Random) =
        let fruit = (rand.Next() % 11 + 1, rand.Next() % 11 + 1)
        if Collision.PlayerColPoint (fruit, snake) then NewFruit (snake, rand)
        else fruit

    let rec MainLoop (snake: (int*int) list, dir, fruit, rand: Random, lastKeyboardList) =
        (* Read keyboard *)
        let keyboardList = Keyboard.ReadKeyboard lastKeyboardList

        (* Find new snake head *)
        let snakeFront = snake[0]
        let (sfx, sfy) = snakeFront
        let newHead = 
            match dir with
            | 0 -> (sfx + 1, sfy)
            | 1 -> (sfx, sfy + 1)
            | 2 -> (sfx - 1, sfy)
            | _ -> (sfx, sfy - 1)

        (* Check fruit collision *)
        let fruitCol = (fruit = newHead)

        (* Create new snake *)
        let newSnake = 
            if fruitCol then newHead::snake
            else ListTools.ReplaceFront newHead snake

        (* Get new fruit location *)
        let newFruit = 
            if fruitCol then NewFruit (newSnake, rand)
            else fruit

        (* Check collision *)
        let selfCol = Collision.PlayerColSelf newSnake
        let borderCol = Collision.PlayerColBorder (13, 13) newSnake[0]

        (* Draw everything *)
        Console.FruitAtLocation fruit
        
        Console.ClearOldSnake snake[snake.Length-1]
        Console.DrawSnake newSnake

        ConsoleInterface.MoveCursor(0, 0)
        let rec printKeys = function
            | [] -> ()
            | head::tail ->
                let str = head.ToString()
                printf "%s " str
                printKeys tail
                ()
        printKeys keyboardList

        (* If player collides with something, then exit MainLoop *)
        if not (selfCol || borderCol) then 
            (* If exit button is not pressed, then call MainLoop *)
            let exit = Input.ShouldExit(keyboardList)
            if not exit then
                (* Sleep before next update *)
                Threading.Thread.Sleep(250)

                (* Update snake direction *)
                let mutable (sd, sdList) = Input.Direction(keyboardList)
                (* If the new snake direction is two away from old snake direction, then player tries to move into themself *)
                if Math.Abs(dir - sd) = 2 then sd <- -1

                if sd = -1 then
                    MainLoop(newSnake, dir, newFruit, rand, keyboardList)
                else
                    MainLoop(newSnake, sd, newFruit, rand, sdList)

[<EntryPoint>]
let main args =
    ConsoleInterface.ClearScreen
    Console.DrawBorder(13, 13)
    Game.MainLoop ([(7,6); (6,6); (5,6)], 0, (6, 4), Random(), [])
    0