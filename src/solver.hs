import System.IO
import Data.List

--data Coords = Coords {rowVal::Int, columnVal::Int, count::Char} deriving (Show, Read)
--plansza = [Coords]
--plansza2 = [((Int, Int), Char)]




checkWords [] [] = []
checkWords planszaTab slowaTab = do
    let xC = 0
    let yC = 0
    let letter = slowaTab !! xC !! yC
    let row = 0
    let planszaRowsNum = length planszaTab
    let foundIndexes = [elemIndices letter (planszaTab !! row) | row <- [0..planszaRowsNum-1]]
    return foundIndexes

main = do
    putStrLn "Plansza:"
    --planszaPath <- getLine
    handlePlansza <- openFile ("../plikiTestowe/" ++ "lam1_plansza.txt") ReadMode
    putStrLn "Slowa:"
    --slowaPath <- getLine
    handleSlowa <- openFile ("../plikiTestowe/" ++ "lam1_slowa.txt") ReadMode
    handledPlansza <- hGetContents handlePlansza
    handledSlowa <- hGetContents handleSlowa
    let tabPlansza = lines handledPlansza
    let tabSlowa = lines handledSlowa
    let chW = checkWords tabPlansza tabSlowa
    return (tabPlansza, tabSlowa, chW)
