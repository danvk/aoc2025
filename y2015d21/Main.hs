-- https://adventofcode.com/2015/day/21
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Data.Function
import Data.List

data Item = Item
  { name :: String,
    cost :: Int,
    damage :: Int,
    armor :: Int
  }
  deriving (Show, Eq)

addItems :: Item -> Item -> Item
addItems a b = Item {name = name a ++ ", " ++ name b, cost = cost a + cost b, damage = damage a + damage b, armor = armor a + armor b}

weapons :: [Item]
weapons =
  [ Item {name = "Dagger", cost = 8, damage = 4, armor = 0},
    Item {name = "Shortsword", cost = 10, damage = 5, armor = 0},
    Item {name = "Warhammer", cost = 25, damage = 6, armor = 0},
    Item {name = "Longsword", cost = 40, damage = 7, armor = 0},
    Item {name = "Greataxe", cost = 74, damage = 8, armor = 0}
  ]

armors :: [Item]
armors =
  [ Item {name = "Leather", cost = 13, damage = 0, armor = 1},
    Item {name = "Chainmail", cost = 31, damage = 0, armor = 2},
    Item {name = "Splintmail", cost = 53, damage = 0, armor = 3},
    Item {name = "Bandedmail", cost = 75, damage = 0, armor = 4},
    Item {name = "Platemail", cost = 102, damage = 0, armor = 5}
  ]

rings :: [Item]
rings =
  [ Item {name = "Damage +1", cost = 25, damage = 1, armor = 0},
    Item {name = "Damage +2", cost = 50, damage = 2, armor = 0},
    Item {name = "Damage +3", cost = 100, damage = 3, armor = 0},
    Item {name = "Defense +1", cost = 20, damage = 0, armor = 1},
    Item {name = "Defense +2", cost = 40, damage = 0, armor = 2},
    Item {name = "Defense +3", cost = 80, damage = 0, armor = 3}
  ]

data Role = Player | Boss deriving (Show, Eq)

data Actor = Actor {role :: Role, hp :: Int, actor_damage :: Int, actor_armor :: Int} deriving (Show)

boss :: Actor
-- boss = Actor {role = Boss, hp = 12, actor_damage = 7, actor_armor = 2}

boss = Actor {role = Boss, hp = 104, actor_damage = 8, actor_armor = 1}

player :: Actor
player = Actor {role = Player, hp = 8, actor_damage = 5, actor_armor = 5}

hit :: Actor -> Int -> Actor
hit a d = a {hp = max 0 (hp a - d)}

step :: (Actor, Actor) -> (Actor, Actor)
step (attack, defend) = (hit defend (max 1 (actor_damage attack - actor_armor defend)), attack)

-- takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
-- takeWhileInclusive _ [] = []
-- takeWhileInclusive p (x : xs) = x : if p x then takeWhileInclusive p xs else []

winner :: Actor -> Actor -> Role
winner attack defend = role winner
  where
    Just (_, winner) = find (\(a, _) -> hp a == 0) $ iterate step (attack, defend)

choose :: Int -> [a] -> [[a]]
choose 0 _ = []
choose _ [] = []
choose 1 xs = [[x] | x <- xs]
choose n (x : xs) = takeIt ++ dontTakeIt
  where
    takeIt = map (x :) $ choose (n - 1) xs
    dontTakeIt = choose n xs

kits :: [Item]
kits = sortBy (compare `on` cost) $ map (foldl1 addItems) $ do
  weapon <- choose 1 weapons
  armor <- [] : choose 1 armors
  ring <- [] : (choose 1 rings ++ choose 2 rings)
  return $ weapon ++ armor ++ ring

playerWithKit :: Int -> Item -> Actor
playerWithKit hp item = Actor {role = Player, hp = hp, actor_damage = damage item, actor_armor = armor item}

main :: IO ()
main = do
  let part1 = find (\kit -> winner (playerWithKit 100 kit) boss == Player) kits
  print part1
