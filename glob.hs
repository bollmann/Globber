
import Globber (matchGlob)

import System.Environment (getArgs)
import System.IO
import Control.Monad (when)

main :: IO ()
main = do args <- getArgs
          case (length args) of
            0 -> putStrLn "usage: glob <glob-pattern> [file 1] [file 2] ... [file N]"
            1 -> stdinGlob (args !! 0)
            _ -> mapM_ (fileGlob (args !! 0)) (tail args)

    where stdinGlob :: String -> IO ()
          stdinGlob glob = do string <- getLine
                              when (matchGlob glob string) (putStrLn string)
                              stdinGlob glob

          fileGlob :: String -> String -> IO ()
          fileGlob glob fileName = withFile fileName ReadMode $ \h -> 
                                   do contents <- hGetContents h
                                      let matches = filter (matchGlob glob) . words $ contents
                                      mapM_ (\match -> putStrLn $ fileName ++ ": " ++ match) matches
