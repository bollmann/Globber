
import Globber (matchGlob)

import System.Environment

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
               else do undefined -- todo: write function which opens
                                 -- each file provided on the
                                 -- command-line and checking if the
                                 -- provided pattern matches somewhere
                                 -- inside the file.
