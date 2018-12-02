import qualified Data.Set as Set

type Commands = [Integer]

data DupeState =
    Init Commands
    | Process Commands Commands (Set.Set Integer) Integer
    | Complete Integer

commands :: IO Commands
commands = do
    cs <- readFile "input.txt"
    return (parseCommands $ lines cs)

parseCommands :: [String] -> [Integer]
parseCommands =
    fmap (read . stripPlus)

stripPlus :: String -> String
stripPlus = filter (\c -> c /= '+')

findDupe :: Commands -> Integer
findDupe cs =
    findDupe' (Init cs)

findDupe' :: DupeState -> Integer
findDupe' (Init cs) = findDupe' (Process cs cs Set.empty 0)
findDupe' (Process allCs [] freqs f) = findDupe' (Process allCs allCs freqs f)
findDupe' (Process allCs cs freqs f) =
    let 
        newF = f + head cs
    in
        case Set.member newF freqs of
            True -> findDupe' (Complete newF)
            False -> findDupe' $ Process allCs (tail cs) (Set.insert newF freqs) newF

findDupe' (Complete i) = i


part1 :: IO Integer
part1 = do
    cs <- commands
    return (sum cs)
    
part2 :: IO Integer
part2 = do
    cs <- commands
    return (findDupe cs) 

main = 
    do
        p1 <- part1
        p2 <- part2
        putStrLn $ show p1
        putStrLn $ show p2