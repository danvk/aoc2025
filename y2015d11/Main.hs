-- https://adventofcode.com/2015/day/11
import System.Environment (getArgs)

nextPass :: String -> String
nextPass = snd . nextHelp
    where nextHelp "" = (False, "")
          nextHelp [x] = if x < 'z' then (False, [succ x]) else (True, "a")
          nextHelp (x:xs) = let (carry, rest) = nextHelp xs in case (carry, x) of
            (True, 'z') -> (True, 'a':rest)
            (True, _) -> (False, succ x:rest)
            (False, _) -> (False, x:rest)

hasStraight :: String -> Bool
hasStraight (a:b:c:xs) = (c == succ b && b == succ a) || hasStraight (b:c:xs)
hasStraight _ = False

hasBadLetter :: String -> Bool
hasBadLetter = any (`elem` "iol")

hasDouble :: String -> Bool
hasDouble (a:b:xs) = a == b || hasDouble (b:xs)
hasDouble _ = False

-- should bbbb satisfy this requirement?
hasDoubleDouble :: String -> Bool
hasDoubleDouble (a:b:xs) = ((a == b) && hasDouble xs) || hasDoubleDouble (b:xs)
hasDoubleDouble _ = False

allOf :: [a -> Bool] -> a -> Bool
allOf fns x = all (\fn -> fn x) fns

isValid :: String -> Bool
isValid = allOf [hasStraight, not . hasBadLetter, hasDoubleDouble]

main :: IO ()
main = do
    args <- getArgs
    let initPass = head args
        part1 = head $ filter isValid $ iterate nextPass initPass
    print part1
