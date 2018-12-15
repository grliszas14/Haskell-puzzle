import System.IO
import Data.List


data Orientation = Vert | Hori | Diag | None deriving (Enum, Eq, Show)

get_horizontal_list::[[Char]]->Int->Int->[(Int, Int)]
get_horizontal_list all_letters x y = if (x < (length all_letters)) then (x,y):(get_horizontal_list all_letters (x+1) y) else []

get_vertical_list::[[Char]]->Int->Int->[(Int, Int)]
get_vertical_list all_letters x y = if (y < (length all_letters)) then (x,y):(get_vertical_list all_letters x (y+1)) else []

get_diagonal_list::[[Char]]->Int->Int->[(Int, Int)]
get_diagonal_list all_letters x y = if (x < (length all_letters)) then (x,y):(get_diagonal_list all_letters (x+1) (y+1)) else []

get_word_from_list::[[Char]]->[(Int, Int)]->[Char]
get_word_from_list all_letters ((x, y):coords) = (all_letters !! x !! y ):(get_word_from_list all_letters coords)
get_word_from_list _ [] = []

begins_with :: [ Char ] -> [ Char ] -> Bool
begins_with (a:word) (b:prefix) = if (a == b) then (begins_with word prefix) else False
begins_with [] _ = True
begins_with _ [] = False


is_word_in_place :: [ [ Char ] ] -> Int -> Int -> [ Char ] -> Orientation
is_word_in_place all_letters x y word
    | begins_with word ( get_word_from_list all_letters (get_horizontal_list all_letters x y ) ) = Hori
    | begins_with word ( get_word_from_list all_letters (get_vertical_list all_letters x y) ) = Vert
    | begins_with word ( get_word_from_list all_letters (get_diagonal_list all_letters x y) ) = Diag
    | otherwise = None

get_next :: [ [ Char ] ] -> (Int, Int) -> (Int, Int)
get_next all_letters (x, y)
    | (x+1) < (length all_letters) = ((x+1), y)
    | (y+1) < (length all_letters) = (0, (y+1))
    | otherwise  = (-1, -1)


find_word :: String -> [ [ Char ] ] -> (Int, Int)-> ( (Int, Int), Orientation )
find_word word all_letters (x, y) = if ((is_word_in_place all_letters x y word)==None) then (find_word word all_letters (get_next all_letters (x, y)) ) else ( (x, y), (is_word_in_place all_letters x y word))

get_removed_list :: String -> Int -> [ [ Char ] ] -> [ [ Char ] ]

get_all_unused_letters :: [ [ Char ] ] -> [ String ] -> [ [ Char ] ]
get_all_unused_letters letters words = get_unused_letters_impl letters words letters

get_unused_letters_impl :: [ [ Char ] ] -> [ String ] -> [ [ Char ] ]
get_unused_letters_impl all_letters (word:words) = get_removed_list (find_word word all_letters) (len word) (get_unused_letters_impl all_letters words)
get_unused_letters_impl all_letters [] = all_letters

--data Coords = Coords {rowVal::Int, columnVal::Int, count::Char} deriving (Show, Read)
--plansza = [Coords]
--plansza2 = [((Int, Int), Char)]




-- checkWords :: [ [ Char ] ] -> [ String ] -> [ Int ]
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
