
import Globber (matchGlob)

import System.Environment
import System.IO

main :: IO ()
main = do args <- getArgs
          if (length args) < 1 then 
              putStrLn "usage: glob <glob-pattern> [file 1] [file 2] ... [file N]"
          else if (length args) == 1 then do 
                                       let glob = args !! 0
                                       string <- getLine
                                       if (matchGlob glob string) then
                                           putStrLn $ string
                                       else
                                           putStrLn ""
                                       main
               else do
                 let glob = args !! 0
                     fileNames = tail args
                     searchFile fileName = do
                                            fileHandle <- openFile fileName ReadMode
                                            contents <- hGetContents fileHandle
                                            let matches = filter (matchGlob glob) . words $ contents
                                            mapM_ (\match -> putStrLn $ fileName ++ ": " ++ match) matches
                 mapM searchFile fileNames
                 return ()
