//  Original C++ code by Javidx9
//	https://www.github.com/onelonecoder
//	https://www.onelonecoder.com
//	https://www.youtube.com/javidx9

open System
open System.Collections.Generic 

let screenWidth = 120          // Console Screen Size X (columns)
let screenHeight = 40          // Console Screen Size Y (rows)
let mapWidth = 16              // World Dimensions
let mapHeight = 16
let PI2 = Math.PI * 2.0

let mutable playerX = 14.7     // Player Start Position
let mutable playerY = 5.09 
let mutable playerA = 0.0      // Player Start Rotation
let fov = 3.14159 / 4.0        // Field of View
let depth = 16.0               // Maximum rendering distance
let speed = 5.0                // Walking Speed

[<EntryPoint>]
let main argv =
    let screen: char [] = Array.zeroCreate (screenWidth * screenHeight)
    Console.CursorVisible <- false
    Console.SetWindowSize(screenWidth, screenHeight + 1)

    // Create Map of world space # = wall block, . = space
    let map =
        "#########.......\
         #...............\
         #.......########\
         #..............#\
         #......##......#\
         #......##......#\
         #..............#\
         ###............#\
         ##.............#\
         #......####..###\
         #......#.......#\
         #......#.......#\
         #..............#\
         #......#########\
         #..............#\
         ################"
         |> Array.ofSeq

    let clock = new System.Diagnostics.Stopwatch()
    clock.Start()
    let mutable tp1 = clock.ElapsedMilliseconds
    let mutable tp2 = tp1
    
    while true do
        Threading.Thread.Sleep(10)
        tp2 <- clock.ElapsedMilliseconds
        let elapsedTime = float (tp2 - tp1) / 1000.0
        tp1 <- tp2

        if Console.KeyAvailable then
            match Console.ReadKey(true).KeyChar |> Char.ToLower with
            | 'a' -> playerA <- playerA - (speed * 0.75) * elapsedTime
                     if playerA < 0.0 then playerA <- playerA + PI2

            | 'd' -> playerA <- playerA + (speed * 0.75) * elapsedTime
                     if playerA > PI2 then playerA <- playerA - PI2
            
            | 'w' ->
                playerX <- playerX + sin(playerA) * speed * elapsedTime
                playerY <- playerY + cos(playerA) * speed * elapsedTime
                if map.[int playerX * mapWidth + int playerY] = '#' then
                    playerX <- playerX - sin(playerA) * speed * elapsedTime
                    playerY <- playerY - cos(playerA) * speed * elapsedTime
            
            | 's' ->
                playerX <- playerX - sin(playerA) * speed * elapsedTime
                playerY <- playerY - cos(playerA) * speed * elapsedTime
                if map.[int playerX * mapWidth + int playerY] = '#' then
                    playerX <- playerX + sin(playerA) * speed * elapsedTime
                    playerY <- playerY + cos(playerA) * speed * elapsedTime
            
            | _ -> ()


        for x = 0 to screenWidth - 1 do
            let rayAngle = (playerA - fov / 2.0) + (float x / float screenWidth) * fov

            let stepSize = 0.1
            let mutable distanceToWall = 0.0

            let mutable hitWall = false
            let mutable boundary = false

            let eyeX = sin(rayAngle)
            let eyeY = cos(rayAngle)

            while (not hitWall && distanceToWall < depth) do
                distanceToWall <- distanceToWall + stepSize
                let testX = int (playerX + eyeX * distanceToWall)
                let testY = int (playerY + eyeY * distanceToWall)
                
                if (testX < 0 || testX >= mapWidth || testY < 0 || testY >= mapHeight) then
                    hitWall <- true
                    distanceToWall <- depth
                else
                    if map.[testX * mapWidth + testY] = '#' then
                        hitWall <- true
                        let p = new List<float*float>()
                        for tx in 0.0 ..1.0 do
                            for ty in 0.0 .. 1.0 do
                                let vy = float testY + ty - playerY
                                let vx = float testX + tx - playerX
                                let d = sqrt(vx * vx + vy * vy)
                                let dot = (eyeX * vx / d) + (eyeY * vy / d)
                                p.Add(d, dot)
                                

                        let p = p.ToArray() |> Array.sortBy fst
                        
                        let bound = 0.01
                        if (acos(snd p.[0]) < bound) then boundary <- true
                        if (acos(snd p.[1]) < bound) then boundary <- true
                        if (acos(snd p.[2]) < bound) then boundary <- true
        
            let ceiling = (float screenHeight / 2.0) - float screenHeight / (float distanceToWall)
            let floor = float screenHeight - ceiling
            
            let mutable shade = ' '
            if (distanceToWall <= depth / 4.0)     then      shade <- '\u2588' // White
            else if (distanceToWall < depth / 3.0) then      shade <- '\u2593' 
            else if (distanceToWall < depth / 2.0) then      shade <- '\u2592' 
            else if (distanceToWall < depth)       then      shade <- '\u2591' 
            else                                             shade <- ' '      // Black

            if (boundary) then shade <- ' '
            
            for y in 0.0 .. float (screenHeight - 1) do
                if y <= ceiling  then
                    screen.[int y * screenWidth + x] <- ' '
                else if y > ceiling && y <= floor then
                    screen.[int y * screenWidth + x] <- shade
                else
                    let b = 1.0 - ((float y - float screenHeight / 2.0) / (float screenHeight / 2.0));
                    if b < 0.25      then shade <- '@'
                    else if b < 0.5  then shade <- '#'
                    else if b < 0.75 then shade <- '+'
                    else if b < 0.9  then shade <- '-'
                    else                  shade <- ' '
                    screen.[int y * screenWidth + x] <- shade


        // Update Map
        for nx = 0 to mapWidth - 1 do
            for ny = 0 to mapWidth - 1 do
                screen.[(ny + 1) * screenWidth + nx] <- map.[ny * mapWidth + nx]

        let playerFigure = @"-\!/-\i/".[int ((playerA / PI2) * 8.0)]     
        screen.[(int playerX + 1) * screenWidth + int playerY] <- playerFigure
        
        // Display Frame
        let frame =
            [| for y in 0.. screenHeight - 1 do
                   for x in 0.. screenWidth - 1 do
                        yield screen.[y * screenWidth + x]
            |] |> String

        Console.SetCursorPosition(0, 0)
        printf "%s" frame

        // Display Stats
        Console.SetCursorPosition(0, screenHeight)
        printf "X=%3.2f, Y=%3.2f, A=%3.2f FPS=%3.2f "  playerX  playerY  (playerA * (180.0 / Math.PI))  (1.0 / elapsedTime)

    0