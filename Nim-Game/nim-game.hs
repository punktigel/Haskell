import Data.Char

--Nim Game
main:: IO ()
main = do
    putStrLn "\nTake-Game\nNumber of sticks:"
    numStick <- getLine
    if numStick == "" then do putStrLn "\n\nNo Input!" >> main else if not (testNum numStick) then do putStrLn "\n\nNot an Integer!" >> main else do 
    let num = read numStick :: Int
    let counter = 0

    let numberPlayers = 2 --Change for more players
    move num counter numberPlayers


move :: Int -> Int -> Int -> IO ()
move numStick playerNum numberPlayers = do
    putStrLn ("\n\nPlayer: " ++ show (playerNum + 1) ++ "\n" ++ show numStick ++ " sticks are left\nTake 1-3 sticks:")
    takeNum <- getLine
    if takeNum == "" then do putStrLn "No Input" >> move numStick playerNum numberPlayers else if not (testNum takeNum) then do putStrLn "Not an Integer" >> move numStick playerNum numberPlayers else do 
    let removeNum = read takeNum :: Int

    if removeNum < 1 || removeNum > 3 then putStrLn "\nYou can only take 1-3" >> move numStick playerNum numberPlayers else do
    let newanzahl = numStick - removeNum
    if newanzahl <= 0 then putStrLn ("Player " ++ show (playerNum + 1) ++ " has won the game\nGame Over") >> return () else do 
    move newanzahl (mod (playerNum+1) numberPlayers) numberPlayers
    

testNum :: [Char] -> Bool
testNum [] = True
testNum (x:xs)
    |isDigit x = testNum xs
    |otherwise = False
