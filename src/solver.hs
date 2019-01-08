import System.IO
import Data.List


data Orientation = Vert | Hori | Diag | None deriving (Enum, Eq, Show)

get_horizontal_list::[[Char]]->Int->Int->[(Int, Int)]
get_horizontal_list all_letters x y = if (x < (length (head all_letters))) then (x,y):(get_horizontal_list all_letters (x+1) y) else []

get_vertical_list::[[Char]]->Int->Int->[(Int, Int)]
get_vertical_list all_letters x y = if (y < (length all_letters)) then (x,y):(get_vertical_list all_letters x (y+1)) else []

get_diagonal_list::[[Char]]->Int->Int->[(Int, Int)]
get_diagonal_list all_letters x y = if ( y < (length all_letters) && x < (length (head all_letters)) ) then (x,y):(get_diagonal_list all_letters (x+1) (y+1)) else []

get_word_from_list::[[Char]]->[(Int, Int)]->[Char]
get_word_from_list all_letters ((x, y):coords) = (all_letters !! y !! x ):(get_word_from_list all_letters coords)
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
    | (x+1) < (length (head all_letters)) = ((x+1), y)
    | (y+1) < (length all_letters) = (0, (y+1))
    | otherwise  = (-1, -1)


find_word :: String -> [ [ Char ] ] -> (Int, Int)-> ( (Int, Int), Orientation )
find_word word all_letters (x, y) = if ((is_word_in_place all_letters x y word)==None) then (find_word word all_letters (get_next all_letters (x, y)) ) else ( (x, y), (is_word_in_place all_letters x y word))

get_n_elems :: [t] -> Int -> [t]
get_n_elems _ 0 = []
get_n_elems (x:xs) a = (x:(get_n_elems xs (a-1)))

orientation_to_list :: ( (Int, Int), Orientation ) -> Int -> [ (Int, Int) ]
orientation_to_list _ 0 = []
orientation_to_list ( (x, y),orien ) len
  | orien == Hori = ( (x, y):(orientation_to_list ( ((x+1), y), Hori) (len-1) ) )
  | orien == Vert = ( (x, y):(orientation_to_list ( (x, (y+1)), Vert) (len-1) ) )
  | orien == Diag = ( (x, y):(orientation_to_list ( ((x+1), (y+1)), Diag) (len-1) ) )
  | otherwise = []

is_in_list :: (Int, Int) -> [ (Int, Int) ] -> Bool
is_in_list _ [] = False
is_in_list (x, y) ((l_x, l_y):rest) = if ( (x == l_x) && (y == l_y) ) then True else (is_in_list (x, y) rest)

get_star_from_single_list :: [ Char ] -> Int -> Int -> [ (Int, Int) ] -> [ Char ]
get_star_from_single_list (letter:rest) x y used_list = if (is_in_list (x, y) used_list) then ('*':(get_star_from_single_list rest (x+1) y used_list)) else (letter:(get_star_from_single_list rest (x+1) y used_list))
get_star_from_single_list [] _ _ _ = []

get_removed_from_list :: [ [ Char ] ] -> Int -> [ (Int, Int) ] -> [ [ Char ] ]
get_removed_from_list (first:rest) y used_list = ((get_star_from_single_list first 0 y used_list):(get_removed_from_list rest (y+1) used_list))
get_removed_from_list [] _ _ = []

get_all_unused_letters :: [ [ Char ] ] -> [ String ] -> [ [ Char ] ]
get_all_unused_letters letters words = (get_unused_letters_impl letters words)

get_unused_letters_impl :: [ [ Char ] ] -> [ String ] -> [ [ Char ] ]
get_unused_letters_impl all_letters [] = all_letters
get_unused_letters_impl all_letters (word:words) = (get_removed_from_list (get_unused_letters_impl all_letters words) 0 ( orientation_to_list (find_word word all_letters (0, 0)) (length word) )  )

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
    handleBoard <- openFile ("../plikiTestowe/" ++ "lam1_plansza.txt") ReadMode
    putStrLn "Slowa:"
    --slowaPath <- getLine
    handleWords <- openFile ("../plikiTestowe/" ++ "lam1_slowa.txt") ReadMode
    handledBoard <- hGetContents handleBoard
    handledWords <- hGetContents handleWords
    let tabBoard = lines handledBoard
    let tabWords = lines handledWords
    let tabPlanszaRozwiazana = get_unused_letters_impl ["CZAREK", "MICHAL", "GRZESI"] ["YY", "CM", "CI"]

    putStrLn "Plansza początkowa:"
    putStrLn handledBoard
    putStrLn "\n"

    return (tabBoard, tabWords, tabPlanszaRozwiazana)
