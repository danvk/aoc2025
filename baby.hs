doubleMe x = x + x

doubleUs x y = x * 2 + y * 2

doubleSmallNumber x =
  if x > 100
    then x
    else x * 2

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
