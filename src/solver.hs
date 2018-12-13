import System.IO


--data Coords = Coords {rowVal::Int, columnVal::Int, count::Char} deriving (Show, Read)
--plansza = [Coords]
--plansza2 = [((Int, Int), Char)]

--strToPlansza :: String -> [((Int, Int), Char)]
--strToPlansza [] = []
--strToPlansza (x:xs) = do
--                      xCoor <- 0
--                      yCoor <- 0
--                      plansza <- []
--                      if (x == " ")
--                      then do strToPlansza(xs)
--                      else ((xCoor,yCoor), x):plansza
--                      return plansza


--strToTab :: String -> [[Char]]
--strToTab [] = []
--strToTab (x:xs) = do
--    line <- 0
--    if (x == "/n")
--      then do tab:[]
--              line = line+1
--    else
--      tab:x
--  return tab

main = do
    putStrLn "File name:"
    name <- getLine
    handle <- openFile ("../plikiTestowe/" ++ name) ReadMode
    combStr <- hGetContents handle
    let tab = lines combStr
    return tab
