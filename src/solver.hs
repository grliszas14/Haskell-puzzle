import System.IO
import Data.List


data Orientation = Vert | Hori | Diag | Updi | None deriving (Enum, Eq, Show)

get_horizontal_list::[[Char]]->Int->Int->[Char]
get_horizontal_list all_letters x y = if (x < (length (head all_letters))) then (all_letters !! y !! x):(get_horizontal_list all_letters (x+1) y) else []

get_vertical_list::[[Char]]->Int->Int->[Char]
get_vertical_list all_letters x y = if (y < (length all_letters)) then (all_letters !! y !! x):(get_vertical_list all_letters x (y+1)) else []

get_diagonal_list::[[Char]]->Int->Int->[Char]
get_diagonal_list all_letters x y = if ( y < (length all_letters) && x < (length (head all_letters)) ) then (all_letters !! y !! x):(get_diagonal_list all_letters (x+1) (y+1)) else []

get_up_list::[[Char]]->Int->Int->[Char]
get_up_list all_letters x y = if ( y >= 0 && x < (length (head all_letters)) ) then (all_letters !! y !! x):(get_up_list all_letters (x+1) (y-1)) else []

begins_with :: [ Char ] -> [ Char ] -> Bool
begins_with (a:word) (b:prefix) = if (a == b) then (begins_with word prefix) else False
begins_with [] _ = True
begins_with _ [] = False


is_word_in_place :: [ [ Char ] ] -> Int -> Int -> [ Char ] -> Orientation -> Bool
is_word_in_place all_letters x y word orien
  | orien == Hori = begins_with word ( get_horizontal_list all_letters x y )
  | orien == Vert = begins_with word ( get_vertical_list all_letters x y)
  | orien == Diag = begins_with word ( get_diagonal_list all_letters x y)
  | orien == Updi = begins_with word ( get_up_list all_letters x y)
  | otherwise = False

get_next :: [ [ Char ] ] -> (Int, Int) -> Orientation -> ((Int, Int), Orientation)
get_next all_letters (x, y) orien
  | orien == Hori = ((x, y), Vert)
  | orien == Vert = ((x, y), Diag)
  | orien == Diag = ((x, y), Updi)
  | (x+1) < (length (head all_letters)) = (((x+1), y), Hori)
  | (y+1) < (length all_letters) = ((0, (y+1)), Hori)
  | otherwise  = ((-1, -1), None)


find_words :: String -> [ [ Char ] ] -> ((Int, Int), Orientation) -> [ ( (Int, Int), Orientation ) ]
find_words word all_letters ((x, y), orien) = if (found_word == ( (-1, -1), None)) then [] else (found_word:(find_words word all_letters (get_next all_letters (x, y) orien))) where found_word = (find_word word all_letters ((x, y), orien))

find_word :: String -> [ [ Char ] ] -> ((Int, Int), Orientation)-> ( (Int, Int), Orientation )
find_word _ _ ((-1, -1), None) = ( (-1, -1), None )
find_word word all_letters ((x, y), orien) = if (is_word_in_place all_letters x y word orien) then ( (x, y), orien ) else (find_word word all_letters (get_next all_letters (x, y) orien))

get_n_elems :: [t] -> Int -> [t]
get_n_elems _ 0 = []
get_n_elems (x:xs) a = (x:(get_n_elems xs (a-1)))

orientations_to_lists :: [ ( (Int, Int), Orientation ) ] -> Int -> [ [ ( Int, Int) ] ]
orientations_to_lists [] _ = []
orientations_to_lists (orien:rest) len = ((orientation_to_list orien len):(orientations_to_lists rest len))

orientation_to_list :: ( (Int, Int), Orientation ) -> Int -> [ (Int, Int) ]
orientation_to_list _ 0 = []
orientation_to_list ( (x, y),orien ) len
  | orien == Hori = ( (x, y):(orientation_to_list ( ((x+1), y), Hori) (len-1) ) )
  | orien == Vert = ( (x, y):(orientation_to_list ( (x, (y+1)), Vert) (len-1) ) )
  | orien == Diag = ( (x, y):(orientation_to_list ( ((x+1), (y+1)), Diag) (len-1) ) )
  | orien == Updi = ( (x, y):(orientation_to_list ( ((x+1), (y-1)), Updi) (len-1) ) )
  | otherwise = []

is_in_list :: (Int, Int) -> [ (Int, Int) ] -> Bool
is_in_list _ [] = False
is_in_list (x, y) ((l_x, l_y):rest) = if ( (x == l_x) && (y == l_y) ) then True else (is_in_list (x, y) rest)

get_star_from_single_list :: [ Char ] -> Int -> Int -> [ (Int, Int) ] -> [ Char ]
get_star_from_single_list (letter:rest) x y used_list = if (is_in_list (x, y) used_list) then ('*':(get_star_from_single_list rest (x+1) y used_list)) else (letter:(get_star_from_single_list rest (x+1) y used_list))
get_star_from_single_list [] _ _ _ = []

get_removed_from_lists :: [ [ Char ] ] -> [ [ (Int, Int) ] ] -> [ [ Char ] ]
get_removed_from_lists all_letters [] = all_letters
get_removed_from_lists all_letters (coord:rest) = (get_removed_from_list (get_removed_from_lists all_letters rest) 0 coord)

get_removed_from_list :: [ [ Char ] ] -> Int -> [ (Int, Int) ] -> [ [ Char ] ]
get_removed_from_list (first:rest) y used_list = ((get_star_from_single_list first 0 y used_list):(get_removed_from_list rest (y+1) used_list))
get_removed_from_list [] _ _ = []

get_all_unused_letters :: [ [ Char ] ] -> [ String ] -> [ [ Char ] ]
get_all_unused_letters letters words = (get_unused_letters_impl letters words)

get_unused_letters_impl :: [ [ Char ] ] -> [ String ] -> [ [ Char ] ]
get_unused_letters_impl all_letters [] = all_letters
get_unused_letters_impl all_letters (word:words) = (get_removed_from_lists (get_unused_letters_impl all_letters words) ( orientations_to_lists (find_words word all_letters ((0, 0), Hori)) (length word) )  )

boardToString [] = []
boardToString (line:rest) = (line++"\n"++boardToString rest)


main = do
    putStrLn "Wybierz numer lamiglowki:"
    nr <- getLine
    let planszaPath | nr == "1" = "lam1_plansza.txt"
                    | nr == "2" = "lam2_plansza.txt"
                    | nr == "3" = "lam3_plansza.txt"
                    | otherwise = error "Nie ma takiej łamigłówki"
    handleBoard <- openFile ("../plikiTestowe/" ++ planszaPath) ReadMode
    let slowaPath   | nr == "1" = "lam1_slowa.txt"
                    | nr == "2" = "lam2_slowa.txt"
                    | nr == "3" = "lam3_slowa.txt"
                    | otherwise = error "Nie ma takiej łamigłówki"
    handleWords <- openFile ("../plikiTestowe/" ++ slowaPath) ReadMode
    handledBoard <- hGetContents handleBoard
    handledWords <- hGetContents handleWords
    let tabBoard = lines handledBoard
    let tabWords = lines handledWords
    putStrLn "Plansza wczytana"
    let tabPlanszaRozwiazana = get_unused_letters_impl tabBoard tabWords
    putStrLn "\n"
    putStrLn "Plansza początkowa:"
    putStrLn handledBoard
    putStrLn "\n"
    putStrLn "Plansza po rozwiązaniu:"
    putStrLn (boardToString tabPlanszaRozwiazana)
    return (handledBoard, tabWords, tabPlanszaRozwiazana)
