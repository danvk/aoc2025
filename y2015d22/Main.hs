-- https://adventofcode.com/2015/day/21

import Data.Heap qualified
import Data.Maybe

data Spell = MagicMissile | Drain | Shield | Poison | Recharge deriving (Show, Eq)

type Effect = (Spell, Int)

spells :: [(Int, Effect)]
spells =
  [ (53, (MagicMissile, 0)), -- instantly do 4 damage
    (73, (Drain, 0)), -- instantly do 2 damage and heals you for 2 hp
    (113, (Shield, 6)), -- starts an effect to increase armor by 7
    (173, (Poison, 6)), -- starts an effect to deal boss 3 damage each pen
    (229, (Recharge, 5)) -- starts an effect to give you 101 mana
  ]

data Turn = PlayerAct | BossSpell | BossAct | PlayerSpell | PlayerWin | BossWin deriving (Show, Eq)

data Boss = Boss {boss_hp :: Int, damage :: Int} deriving (Show)

data Player = Player {player_hp :: Int, mana :: Int, armor :: Int, effects :: [Effect]} deriving (Show)

-- player :: Player
-- player = Player {player_hp = 10, mana = 250, armor = 0, effects = []}

-- mana spent, attacker, player, boss
type State = (Int, Turn, Player, Boss)

-- Update the player in light of effects
applyPlayerEffects :: Player -> Player
applyPlayerEffects p = p {mana = new_mana, armor = new_armor, effects = new_effects}
  where
    active_effects = map fst $ effects p
    worn_effects = [e | (e, n) <- effects p, n == 1]
    new_mana = mana p + if Recharge `elem` active_effects then 101 else 0
    new_armor = armor p - if Shield `elem` worn_effects then 7 else 0
    new_effects = [(e, n - 1) | (e, n) <- effects p, n > 1]

applyBossEffects :: Player -> Boss -> Boss
applyBossEffects p b = b {boss_hp = new_boss_hp}
  where
    active_effects = map fst $ effects p
    new_boss_hp = max 0 $ boss_hp b - if Poison `elem` active_effects then 3 else 0

castSpell :: State -> (Int, Effect) -> Maybe State
castSpell (mana_spent, _, p, b) (cost, e@(spell, _)) =
  if mana p < cost
    then Nothing
    else
      Just
        ( mana_spent + cost,
          if boss_hp nb > 0 then BossSpell else PlayerWin,
          np,
          nb
        )
  where
    bhp = boss_hp b
    mehp = player_hp p
    (np, nb) = case spell of
      MagicMissile -> (p, b {boss_hp = max 0 (bhp - 4)})
      Drain -> (p {player_hp = mehp + 2}, b {boss_hp = max 0 (bhp - 2)})
      Shield -> (p {armor = 7 + armor np, effects = e : effects p}, b)
      _ -> (p {effects = e : effects p}, b)

step :: State -> [State]
step (mana, PlayerSpell, p, b) = [(mana, if isBossAlive then PlayerAct else PlayerWin, newPlayer, newBoss)]
  where
    newPlayer = applyPlayerEffects p
    newBoss = applyBossEffects p b
    isBossAlive = boss_hp newBoss > 0
step (mana, BossSpell, p, b) = [(mana, if isBossAlive then BossAct else PlayerWin, newPlayer, newBoss)]
  where
    newPlayer = applyPlayerEffects p
    newBoss = applyBossEffects p b
    isBossAlive = boss_hp newBoss > 0
step (mana, BossAct, p, b) = [(mana, if isAlive then PlayerSpell else BossWin, damagedPlayer, b)]
  where
    hpDamage = max 1 (damage b - armor p)
    newHp = max 0 (player_hp p - hpDamage)
    isAlive = newHp > 0
    damagedPlayer = p {player_hp = newHp}
step state@(mana, PlayerAct, p, b) = if null nextStates then [(mana, BossWin, p, b)] else nextStates
  where
    activeSpells = map fst $ effects p
    availableSpells = filter (\s -> fst (snd s) `notElem` activeSpells) spells
    nextStates = mapMaybe (castSpell state) availableSpells
step (_, PlayerWin, _, _) = []
step (_, BossWin, _, _) = []

bfs :: (a -> [a]) -> (a -> Int) -> (a -> Bool) -> [a] -> Maybe a
bfs stepFn weight done starts = go initHeap
  where
    initList = zip (map weight starts) starts
    initHeap = Data.Heap.fromList initList `asTypeOf` (undefined :: Data.Heap.MinPrioHeap Int a)
    go h = case Data.Heap.view h of
      Just ((_, val), rest) ->
        if done val then Just val else go (insertAll rest $ map (\x -> (weight x, x)) (stepFn val))
      Nothing -> Nothing
    insertAll = foldr Data.Heap.insert

manaUsed :: State -> Int
manaUsed (x, _, _, _) = x

isPlayerWin :: State -> Bool
isPlayerWin (_, x, _, _) = x == PlayerWin

main :: IO ()
main = do
  -- You start with 50 hit points and 500 mana points.
  -- Boss {boss_hp=51, damage=9}
  -- let boss = Boss {boss_hp = 13, damage = 8}
  --     player = Player {player_hp = 10, mana = 77, armor = 0, effects = [(Poison, 6)]}
  let boss = Boss {boss_hp = 51, damage = 9}
      player = Player {player_hp = 50, mana = 500, armor = 0, effects = []}
      state0 = (0, PlayerAct, player, boss)
      result = bfs step manaUsed isPlayerWin [state0]
  print result

--     state1s = step state0
--     state2s = concatMap step state1s
--     state3s = concatMap step state2s
--     state4s = concatMap step state3s
--     state5s = concatMap step state4s
-- print state0
-- print state1s
-- print state2s
-- print state3s
-- print state4s
-- print state5s

-- TODO: sort next states by mana spent, iterate until you win.
