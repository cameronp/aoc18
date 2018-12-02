import qualified Data.Map.Strict as Map
import qualified Data.List as List

type Label = String

type LetterCounts = Map.Map Char Integer

labels :: IO [Label]
labels = do
    ls <- readFile "input.txt"
    return (lines ls)


letterCount :: Char -> LetterCounts -> Integer
letterCount c lcs =
    case Map.lookup c lcs of
        Nothing -> 0
        Just i -> i

incrementCount :: Char -> LetterCounts -> LetterCounts
incrementCount c lcs =
    Map.alter 
        (\maybeCount ->
            case maybeCount of 
                Nothing -> Just 1
                Just i -> Just (i + 1)        
        )
        c
        lcs

countLetters :: Label -> LetterCounts
countLetters l = countLetters' l Map.empty

countLetters' :: Label -> LetterCounts -> LetterCounts
countLetters' [] lcs = lcs
countLetters' (l : ls) lcs = countLetters' ls (incrementCount l lcs)

hasMatches :: Integer -> Label -> Bool
hasMatches n l =
    let
        lcs = countLetters l
    in
        List.elem n (Map.elems lcs)

checksum :: [Label] -> Integer
checksum ls = 
    let
        twos = toInteger . length $ filter (hasMatches 2) ls 
        threes = toInteger . length $ filter (hasMatches 3) ls
    in
        twos * threes

closeMatch :: Label -> Label -> Bool
closeMatch a b = closeMatch' a b 0

closeMatch' "" "" 1 = True
closeMatch' "" "" _ = False
closeMatch' _ _ 2 = False
closeMatch' (h1 : t1) (h2 : t2) n
    | h1 == h2 = closeMatch' t1 t2 n
    | otherwise = closeMatch' t1 t2 (n + 1) 

findCloseMatch :: [Label] -> (Label, Label)
findCloseMatch ls =
    head $ [(a,b) | a <- ls, b <- ls, closeMatch a b]

commonLetters :: (Label, Label) -> Label
commonLetters (a,b) = commonLetters' a b

commonLetters' :: Label -> Label -> Label
commonLetters' "" "" = ""
commonLetters' (h1 : t1) (h2 : t2) 
    | h1 == h2 = h1 : (commonLetters' t1 t2)
    | otherwise = commonLetters' t1 t2


part1 :: IO Integer
part1 = do
    ls <- labels
    return (checksum ls)

part2 :: IO Label
part2 = do
    ls <- labels
    return (commonLetters $ findCloseMatch ls)
    
        
