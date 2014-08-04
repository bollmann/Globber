-- a simple glob matcher fulfilling the specification at:
-- http://www.scs.stanford.edu/14sp-cs240h/labs/lab1.html

module Globber where

import Data.List

type GlobPattern = String

matchGlob :: GlobPattern -> String -> Bool
matchGlob glob string = matchGlob' (collapseStars glob) string

matchGlob' :: GlobPattern -> String -> Bool
matchGlob' glob string
  | null glob && null string = True
  | null glob && not (null string) = False
  | null string && glob == "*" = True
  | null string && glob /= "*" = False
matchGlob' (p:ps) (s:sx)
  | p == '?' = matchGlob' ps sx
  | p == '\\' = matchEscapedChar ps (s:sx)
  | p == '*' = matchStar ps (s:sx)
  | p == '[' = matchSet (p:ps) (s:sx)
  | otherwise = p == s && matchGlob' ps sx

matchEscapedChar :: GlobPattern -> String -> Bool
matchEscapedChar [] (_:_) = False
matchEscapedChar (p:ps) (s:sx) = p == s && matchGlob' ps sx

matchStar :: GlobPattern -> String -> Bool
matchStar [] _ = True
matchStar (p:ps) input = let tailInput = snd $ splitAtElem p input
                         in case tailInput of [] -> False
                                              (_:_) -> matchGlob' (p:ps) tailInput

matchSet :: GlobPattern -> String -> Bool
matchSet pattern (s:sx) = let maybeCharSet = extractCharSet $ pattern
                              stripOffBrackets set = tail . init $ set
                          in case maybeCharSet of Nothing -> False
                                                  (Just []) -> False
                                                  (Just charSet) -> s `elem` (buildCharChoices . stripOffBrackets $ charSet)
                                                                    && matchGlob' (drop (length charSet) pattern) sx

extractCharSet :: String -> Maybe String
extractCharSet pattern
  | find (==']') pattern == Nothing = Nothing
  | otherwise = let currentCharSet = takeWhile (/=']') pattern
                in if null currentCharSet || last currentCharSet /= '\\' then
                     return $ currentCharSet ++ "]"
                   else
                     extractCharSet (drop (length currentCharSet + 1) pattern)
                     >>= (\restCharSet -> return $ currentCharSet ++ "]" ++ restCharSet)

buildCharChoices :: String -> [Char]
buildCharChoices [] = []
buildCharChoices set@(x:xs)
  | isRange (take 3 set) = (addRange (take 3 set)) ++ (buildCharChoices (drop 3 set))
  | x == '\\' = if (null xs) then [] else (head xs):(buildCharChoices (tail xs))
  | otherwise = x:(buildCharChoices xs)
  where isRange chars = if (length chars) == 3 && (chars !! 1) == '-' then
                        True
                      else
                        False
        addRange chars = enumFromTo (chars !! 0) (chars !! 2)

splitAtElem :: (Eq a) => a -> [a] -> ([a], [a])
splitAtElem x xs = foldl splitPred ([], []) xs
  where splitPred = \(pre,post) elem -> if (null post) && elem /= x then
                                          (pre ++ [elem], [])
                                        else
                                          (pre, post ++ [elem])

collapseStars :: String -> String
collapseStars [] = []
collapseStars [x] = [x]
collapseStars (x1:x2:xs) = if x1 == '*' && x1 == x2 then
                             collapseStars (x2:xs)
                           else
                             x1:(collapseStars (x2:xs))
