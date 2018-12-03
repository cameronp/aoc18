module Main where

import qualified Data.Map.Strict as Map
import Data.List

import Text.Regex.PCRE

data Claim =
  Claim Integer Integer Integer Integer Integer
  deriving (Eq, Show)

claimId :: Claim -> Integer
claimId (Claim i _ _ _ _) = i

topleft :: Claim -> (Integer,Integer)
topleft (Claim _ x y _ _) = (x,y)

type Cloth = Map.Map (Integer,Integer) [Integer]

inputLines :: IO [String]
inputLines = do
    ls <- readFile "input.txt"
    return (lines ls)

subMatches :: String -> AllTextSubmatches [] String
subMatches s =
  s =~ "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)"

tokenize :: String -> [String]
tokenize =
  tail . getAllTextSubmatches . subMatches 

parse :: [String] -> [Integer]
parse = fmap read

buildClaim :: [Integer] -> Claim
buildClaim [id,x,y,w,h] = Claim id x y w h

load :: IO [Claim]
load = do
  ls <- inputLines
  return $ fmap (buildClaim . parse . tokenize) ls

claim :: Claim -> Cloth -> Cloth
claim c cloth =
  let 
     (Claim id x y w h) = c
  in
    claimGrid id x y w h cloth

claimGrid :: Integer -> Integer -> Integer -> Integer -> Integer -> Cloth -> Cloth
claimGrid _ _ _ _ 0 cloth = cloth
claimGrid id x y w h cloth =
  let
     oneRow = claimRow id x y w h cloth
  in 
     claimGrid id x (y + 1) w (h - 1) oneRow

claimRow :: Integer -> Integer -> Integer -> Integer -> Integer -> Cloth -> Cloth     
claimRow _ _ _ 0 _ cloth = cloth
claimRow id x y w h cloth =
  let
    oneCell = claimCell id x y cloth
  in
    claimRow id (x + 1) y (w - 1) h oneCell

claimCell :: Integer -> Integer -> Integer -> Cloth -> Cloth     
claimCell id x y cloth =
  Map.alter
    (
      \maybeClaims ->
        case maybeClaims of
          Nothing -> Just [id]
          Just claims -> Just (id : claims)
    )
    (x,y)
    cloth

makeClaims :: [Claim] -> Cloth
makeClaims cs =
  foldr claim Map.empty cs

countOverlappingPoints :: Cloth -> Int
countOverlappingPoints cloth =
  length $ filter (\cs -> length cs > 1) (Map.elems cloth)

removeOverlaps :: [Integer] -> [Integer] -> [Integer]
removeOverlaps point ids =
  if (length point) > 1 then
    ids \\ point
  else
    ids

findNonOverlappingClaim :: [Claim] -> Integer
findNonOverlappingClaim claims =
  let
    ids = fmap claimId claims
    cloth = makeClaims claims
    points = Map.elems cloth
  in
    head $ foldr removeOverlaps ids points

part1 :: IO ()
part1 = do
  claims <- load
  putStrLn (show $ countOverlappingPoints $ makeClaims claims)

part2 :: IO ()
part2 = do
  claims <- load
  putStrLn (show $ findNonOverlappingClaim claims)

main :: IO ()
main = do
  part1
  part2
