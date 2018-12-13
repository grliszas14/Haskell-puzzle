import System.IO

main = do
    putStrLn "File name:"
    name <- getLine
    handle <- openFile ("../plikiTestowe/" ++ name) ReadMode
    combStr <- hGetContents handle
    putStrLn combStr
    return False
