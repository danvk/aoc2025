import Data.Maybe

-- https://adventofcode.com/2015/day/21

data Spell = MagicMissile | Drain | Shield | Poison | Recharge deriving (Show, Eq)

type Effect = (Spell, Int)

spells :: [(Int, Effect)]
spells =
  [ (53, (MagicMissile, 0)),
    (73, (Drain, 0)),
    (113, (Shield, 6)),
    (173, (Poison, 6)),
    (229, (Recharge, 5))
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
castSpell (mana, _, p, b) (cost, (spell, turns)) =
  if mana < cost
    then Nothing
    else Just (mana, BossSpell, p, b)

-- TODO: ^^^ apply this spell

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
    nextStates = mapMaybe (castSpell state) spells
step (_, PlayerWin, _, _) = []
step (_, BossWin, _, _) = []

main :: IO ()
main = do
  -- Boss {boss_hp=51, damage=9}
  let boss = Boss {boss_hp = 13, damage = 8}
      player = Player {player_hp = 10, mana = 250, armor = 0, effects = [(Poison, 6)]}
      state0 = (77, BossSpell, player, boss)
      state1s = step state0
      state2s = concatMap step state1s
      state3s = concatMap step state2s
  print state0
  print state1s
  print state2s
  print state3s
