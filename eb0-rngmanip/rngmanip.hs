import Prelude hiding ((!?))
import Data.Word
import Data.Bits
import Data.List hiding ((!?))
import Text.Printf
import Data.Maybe
import Control.Monad.Cont
import Control.Monad.Identity
import Data.Map (Map)
import qualified Data.Map as M
import Debug.Trace
import Data.Functor

-- types
data Range = All | Ally | Enemy deriving Show

data TurnState = Win | Continue | Loss deriving (Show, Eq, Ord)

data Member = Member
    { name ::       String
    , charID ::     Word8
    , hp ::         Word16
    , maxHp ::      Word16
    , pp ::         Word16
    , maxPp ::      Word16
    , attack ::     Word16
    , defense ::    Word16
    , fight ::      Word8
    , speed ::      Word8
    , wisdom ::     Word8
    , strength ::   Word8
    , force ::      Word8
    , attackSel ::  Word8
    , attackTarg :: Int
    , status ::     Word8
    , statusMask :: Word8
    , resistance :: Word8
    , buff ::       Word8
    , level ::      Word8
    , experience :: Word32
    , yieldedXp ::  Word32
    , attackList :: [Word8]
    , bag ::        [Word8]
    , learnedPsi :: [Word8]
    } deriving (Show, Eq, Ord)

-- constants
fightValMap :: [Word8]
fightValMap =
    [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x0c, 0x0c, 0x0c,
      0x0c, 0x0c, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
      0x01, 0x01, 0x01, 0x01, 0x02, 0x02, 0x02, 0x02,
      0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x03, 0x03,
      0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03,
      0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04,
      0x04, 0x04, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05,
      0x05, 0x05, 0x05, 0x14, 0x06, 0x06, 0x06, 0x06,
      0x06, 0x06, 0x06, 0x06, 0x06, 0x15, 0x07, 0x07,
      0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x11, 0x11,
      0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08,
      0x12, 0x12, 0x09, 0x09, 0x09, 0x09, 0x09, 0x09,
      0x09, 0x0f, 0x0f, 0x0f, 0x0a, 0x0a, 0x0a, 0x0a,
      0x0a, 0x0a, 0x0d, 0x0d, 0x0d, 0x0d, 0x0b, 0x0b,
      0x0b, 0x0b, 0x0b, 0x0b, 0x0e, 0x0e, 0x0e, 0x0e,
      0x10, 0x10, 0x10, 0x13, 0x13, 0x16, 0x17, 0x18 ]

attackRangeMap :: [Word8]
attackRangeMap =
    [ 0x2a, 0xaa, 0x00, 0xa2, 0x0a, 0x22, 0x81, 0x11,
      0x4a, 0x20, 0x15, 0x04, 0xa0, 0x89, 0x40, 0xaa,
      0xa8, 0xa2, 0x02, 0x00, 0x20, 0x02, 0x01, 0x01,
      0x89, 0x58, 0x28, 0xaa, 0x15, 0x54, 0xa5, 0x40,
      0x80, 0x44, 0x00, 0x00, 0x00, 0x00, 0x01, 0x0a,
      0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x05,
      0x00, 0x00, 0x00, 0x03, 0x3c, 0x00, 0x00, 0x04,
      0x3c, 0x00, 0x01, 0x02, 0x3c, 0x00, 0x02, 0x01 ]

emptyMember :: Member
emptyMember = Member
    { name =       ""
    , charID =     0x00
    , hp =         0x0000
    , maxHp =      0x0000
    , pp =         0x0000
    , maxPp =      0x0000
    , attack =     0x0000
    , defense =    0x0000
    , fight =      0x00
    , speed =      0x00
    , wisdom =     0x00
    , strength =   0x00
    , force =      0x00
    , attackSel =  0x00
    , attackTarg = 0
    , status =     0x00
    , statusMask = 0x00
    , resistance = 0x00
    , buff =       0x00
    , level =      0x00
    , experience = 0x000000
    , yieldedXp =  0x000000
    , attackList = []
    , bag =        []
    , learnedPsi = [] }

-- stat growth constants map: charID -> (Fight, Speed, Wisdom, Strength, Force)
statUpMap :: Map Word8 (Word8, Word8, Word8, Word8, Word8)
statUpMap = M.fromList
    [ (0x01, (0x04, 0x04, 0x04, 0x04, 0x04))   -- Ninten
    , (0x03, (0x03, 0x01, 0x07, 0x03, 0x02))   -- Lloyd
    , (0x06, (0x05, 0x03, 0x05, 0x07, 0x01)) ] -- EVE

levelUpMap :: Map Word8 Word8
levelUpMap = M.fromList [ (0x01, 0xd6), (0x03, 0xc0) ]

-- learned PSI list - (name, level required for Ninten) (not implemented for Ana)
psiTable :: [(String, Word8)]
psiTable =
    [ ("           ", 0xff), ("Telepathy  ", 0x00), ("Teleport   ", 0xff), ("           ", 0xff)
    , ("           ", 0xff), ("           ", 0xff), ("           ", 0xff), ("           ", 0xff)
    , ("LifeUp a   ", 0x02), ("LifeUp B   ", 0x1c), ("LifeUp r   ", 0x20), ("LifeUp p   ", 0x25)
    , ("LifeUp O   ", 0xff), ("           ", 0xff), ("           ", 0xff), ("           ", 0xff)
    , ("Healing a  ", 0x04), ("Healing B  ", 0x0f), ("Healing r  ", 0x12), ("Healing p  ", 0x15)
    , ("SuprHealing", 0x22), ("PSIShield a", 0x0c), ("PSIShield B", 0x1d), ("Powershield", 0x21)
    , ("Brainshock ", 0xff), ("BrainCyclon", 0xff), ("           ", 0xff), ("Paralysis  ", 0xff)
    , ("Darkness   ", 0xff), ("PSI-Magnet ", 0xff), ("Shield-Off ", 0xff), ("PSI-Block  ", 0xff)
    , ("OffenseUp  ", 0x11), ("DefenseUp a", 0x06), ("DefenseUp B", 0x17), ("QuickUp    ", 0x0d)
    , ("Def.Down a ", 0x08), ("Def.Down B ", 0x1a), ("4th-D Slip ", 0x0a), ("Hypnosis   ", 0x03)
    , ("PK Freeze a", 0xff), ("PK Freeze B", 0xff), ("PK Freeze r", 0xff), ("PK Freeze O", 0xff)
    , ("PK Fire a  ", 0xff), ("PK Fire B  ", 0xff), ("PK Fire r  ", 0xff), ("PK Fire O  ", 0xff)
    , ("PK Beam a  ", 0xff), ("PK Beam B  ", 0xff), ("PK Beam r  ", 0xff), ("PK Beam O  ", 0xff)
    , ("PK Thundera", 0xff), ("PK ThunderB", 0xff), ("PK Thunderr", 0xff), ("PK ThunderO", 0xff)
    , ("PK Thundera", 0xff), ("PK ThunderB", 0xff), ("PK Thunderr", 0xff), ("           ", 0xff)
    , ("           ", 0xff), ("           ", 0xff), ("           ", 0xff), ("           ", 0xff) ]

itemIDMap :: Map Word8 String
itemIDMap = M.fromList [ (0x24, "Super Bomb"), (0x49, "PSI Stone")]

encFileMap :: Map Word8 FilePath
encFileMap = M.fromList [ (0x92, "mt_itoi/enc92.txt")
                        , (0x9a, "mt_itoi/enc9a.txt") ]

encounterThresholds = [ 0x60, 0x20, 0x15, 0x10, 0x0d, 0x0a, 0x08, 0x06, 0x05 ]

waitFrames = 32
waitRngSkip = 24

-- global utilities
to8 :: (Integral a) => a -> Word8
to8 = fromIntegral

to16 :: (Integral a) => a -> Word16
to16 = fromIntegral

to32 :: (Integral a) => a -> Word32
to32 = fromIntegral

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split a l =
    let (l', n) = break (== a) l in 
    case n of
    x : n' -> l' : split a n'
    _      -> [l']

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen _ [] = []
splitWhen f l =
    let (l', n) = break f l in 
    case n of
    x : n' -> l' : splitWhen f n'
    _      -> [l']

insertAt :: [a] -> Int -> a -> [a]
insertAt l i e =
    let (l1, _ : l2) = splitAt i l in
    l1 ++ e : l2

firstJust f (x : l) =
    case f x of
    Just y -> Just y
    Nothing -> firstJust f l
firstJust f [] = Nothing

debugFlag = False

debug str a = if debugFlag then trace str a else a

ror (a, c) = ((a .>>. 1) + (if c then 0x80 else 0x00), (a .&. 0x01) /= 0x00)

execCont = flip runCont id

label a = callCC $ \k -> let go b = k (go, b) in return (go, a)

-- show functions
showHex8 :: Word8 -> String
showHex8 = printf "0x%02x"

showHex16 :: Word16 -> String
showHex16 = printf "0x%04x"

showHex24 :: Word32 -> String
showHex24 x = printf "0x%06x" (x .&. 0xffffff)

readHex :: String -> Maybe Int
readHex ('0' : 'x' : n) =
    foldM (\acc c ->
        (+) <$> elemIndex c "0123456789abcdef" <*> pure (acc * 16)) 0 n
readHex _ = Nothing

showMember :: Member -> String
showMember member =
    "Member {\n" ++
        "  name: " ++ name member ++ "\n" ++
        "  id:   " ++ showHex8 (charID member) ++ "\n" ++
        "  hp:   " ++ showHex16 (hp member) ++ "\n" ++
        "  mhp:  " ++ showHex16 (maxHp member) ++ "\n" ++
        "  pp:   " ++ showHex16 (pp member) ++ "\n" ++
        "  mpp:  " ++ showHex16 (maxPp member) ++ "\n" ++
        "  atk:  " ++ showHex16 (attack member) ++ "\n" ++
        "  def:  " ++ showHex16 (defense member) ++ "\n" ++
        "  fig:  " ++ showHex8 (fight member) ++ "\n" ++
        "  spe:  " ++ showHex8 (speed member) ++ "\n" ++
        "  wis:  " ++ showHex8 (wisdom member) ++ "\n" ++
        "  str:  " ++ showHex8 (strength member) ++ "\n" ++
        "  for:  " ++ showHex8 (force member) ++ "\n" ++
        "  ats:  " ++ showHex8 (attackSel member) ++ "\n" ++
        "  ato:  " ++ show (attackTarg member) ++ "\n" ++
        "  sta:  " ++ showHex8 (status member) ++ "\n" ++
        "  stm:  " ++ showHex8 (statusMask member) ++ "\n" ++
        "  res:  " ++ showHex8 (resistance member) ++ "\n" ++
        "  lvl:  " ++ showHex8 (level member) ++ "\n" ++
        "  xp:   " ++ showHex24 (experience member) ++ "\n" ++
        "  yxp:  " ++ showHex24 (yieldedXp member) ++ "\n" ++
        "  atl:  " ++ "[ " ++ intercalate ", " (map showHex8 (attackList member)) ++ " ]\n" ++
        "  bag:  " ++ "[ " ++ intercalate ", " (map showHex8 (bag member)) ++ " ]\n" ++
        "  psi:  " ++ "[ " ++ intercalate ", " (map showHex8 (learnedPsi member)) ++ " ]\n" ++
    "}"

-- read from file functions
readMember :: [String] -> Member
readMember entries =
    let assq = M.fromList $ map (fromJust . list2tuple . split ':') entries
        name = assq M.!? "name"
        id' = to8 <$> (readHex =<< assq M.!? "charID")
        hp' = to16 <$> (readHex =<< assq M.!? "hp")
        mhp = to16 <$> (readHex =<< assq M.!? "maxHp")
        pp' = to16 <$> (readHex =<< assq M.!? "pp")
        mpp = to16 <$> (readHex =<< assq M.!? "maxPp")
        atk = to16 <$> (readHex =<< assq M.!? "attack")
        def = to16 <$> (readHex =<< assq M.!? "defense")
        fig = to8 <$> (readHex =<< assq M.!? "fight")
        spe = to8 <$> (readHex =<< assq M.!? "speed")
        wis = to8 <$> (readHex =<< assq M.!? "wisdom")
        str = to8 <$> (readHex =<< assq M.!? "strength")
        for = to8 <$> (readHex =<< assq M.!? "force")
        ats = to8 <$> (readHex =<< assq M.!? "attackSel")
        ato = read <$> (assq M.!? "attackTarg")
        sta = to8 <$> (readHex =<< assq M.!? "status")
        stm = to8 <$> (readHex =<< assq M.!? "statusMask")
        res = to8 <$> (readHex =<< assq M.!? "resistance")
        lvl = to8 <$> (readHex =<< assq M.!? "level")
        xp' = to32 <$> (readHex =<< assq M.!? "experience")
        yxp = to32 <$> (readHex =<< assq M.!? "yieldedXp")
        atl = map to8 <$> (mapM readHex . split ',' =<< assq M.!? "attackList")
        bg' = map to8 <$> (mapM readHex . split ',' =<< assq M.!? "bag")
        psi = map to8 <$> (mapM readHex . split ',' =<< assq M.!? "learnedPsi") in
    emptyMember { name = memberF "name" name
                , charID = memberFOpt charID id'
                , hp = memberF "hp" hp'
                , maxHp = memberFOpt maxHp mhp
                , pp = memberF "pp" pp'
                , maxPp = memberFOpt maxPp mpp
                , attack = memberF "attack" atk
                , defense = memberF "defense" def
                , fight = memberF "fight" fig
                , speed = memberF "speed" spe
                , wisdom = memberF "wisdom" wis
                , strength = memberF "strength" str
                , force = memberF "force" for
                , attackSel = memberFOpt attackSel ats
                , attackTarg = memberFOpt attackTarg ato
                , status = memberF "status" sta
                , statusMask = memberF "statusMask" stm
                , resistance = memberF "resistance" res
                , level = memberFOpt level lvl
                , experience = memberFOpt experience xp'
                , yieldedXp = memberFOpt yieldedXp yxp
                , attackList = memberFOpt attackList atl
                , bag = memberFOpt bag bg'
                , learnedPsi = memberFOpt learnedPsi psi }
    where list2tuple [x, y] = Just (x, y)
          list2tuple _ = Nothing
          memberF _ (Just e) = e
          memberF f _ = error ("readMember: field " ++ f ++ " is missing!")
          memberFOpt _ (Just e) = e
          memberFOpt f _ = f emptyMember

readTeam :: FilePath -> IO [Member]
readTeam teamFile = do
    teamStr <- readFile teamFile
    let teamStrList = split "" (lines teamStr)
        teamList = map readMember teamStrList
    return $ take 4 $ teamList ++ repeat emptyMember

readLoot :: [String] -> (Word8, Word8)
readLoot entries =
    let assq = M.fromList $ map (fromJust . list2tuple . split ':') entries
        yield = to8 <$> (readHex =<< assq M.!? "yieldedItem")
        rate = to8 <$> (readHex =<< assq M.!? "lootRate") in
    (getYield yield, fromMaybe 0x00 rate)
    where list2tuple [x, y] = Just (x, y)
          list2tuple _ = Nothing
          getYield Nothing = error "field yieldedItem is missing!"
          getYield (Just y) = y

readEnemies :: FilePath -> IO ([Member], Word8, Word8)
readEnemies enemyFile = do
    enemyStr <- readFile enemyFile
    let enemyStrList = split "" (lines enemyStr)
        enemyList = map readMember (init enemyStrList)
        (yield, rate) = readLoot (last enemyStrList)
    return (take 4 (enemyList ++ repeat emptyMember), yield, rate)

readBattleMembers :: FilePath -> FilePath -> IO ([Member], Word8, Word8)
readBattleMembers teamFile enemyFile = do
    team <- readTeam teamFile
    (enemies, yield, rate) <- readEnemies enemyFile
    return (team ++ enemies, yield, rate)

readEncounterTable :: FilePath -> IO [Word8]
readEncounterTable tableFile = do
    tableContent <- readFile tableFile
    let tableStr : _ = lines tableContent
        ret = map to8 (fromJust (mapM readHex (split ' ' tableStr)))
    return ret

-- RNG functions
nextRng :: Word16 -> Word16
nextRng x =
    let x0 = to8 x
        x1 = to8 (x .>>. 8)
        x0' = x0 + 0x75
        x1' = x1 + x0 + 0x63 + (if x0 > 0x8a then 1 else 0) in
    to16 x0' + (to16 x1' .<<. 8)

skipRng :: Int -> Word16 -> Word16
skipRng i x = iterate nextRng x !! i

-- battle system
alive :: Member -> Bool
alive m = (status m .&. 0x80) == 0x00 && statusMask m /= 0x00

valueRoll :: Word16 -> Word16 -> (Word16, Word16)
valueRoll a seed =
    let seed' = nextRng seed
        b = to32 a .<<. 6
        c = (b `div` 100) * to32 (fightValMap !! fromIntegral (seed' .>>. 9))
        -- depending on rng LSB, neg/pos value
        d = to16 $ (if (seed' .&. 0x100) /= 0x00 then b - c else b + c) .>>. 6 in
    (d, seed')

battlePriority :: [Member] -> Word16 -> (Int, Word16)
battlePriority members seed =
    loop (0, 0, 0, seed) members where 
    loop (maxSpeed, maxTarget, i, seed') [] =
        debug ("[TRACE] battlePriority - maxSpeed: " ++ showHex16 maxSpeed ++ ", selected target: " ++
              show maxTarget ++ ", seed: " ++ showHex16 seed')
        (maxTarget, seed')
    loop (maxSpeed, maxTarget, i, seed) (m : members) =
        case attackSel m of
        -- continue
        0xff -> loop (maxSpeed, maxTarget, i + 1, seed) members
        -- break with current index
        0x5e -> (i, seed)
        -- find max
        _ -> let (s, seed') = valueRoll (0x00ff .&. to16 (speed m)) seed
                 s' = min s 0xff in
             debug ("[TRACE] battlePriority - value rolled: " ++ showHex16 s' ++ " for member: " ++ show i ++ ", seed: " ++ showHex16 seed')
             debug ("[TRACE] battlePriority - maxSpeed: " ++ showHex16 maxSpeed ++ ", selected target: " ++ show maxTarget)
             loop (if s' >= maxSpeed
                   then (s', i, i + 1, seed')
                   else (maxSpeed, maxTarget, i + 1, seed')) members

attackSelect :: [Member] -> Int -> Word16 -> Int -> (Word8, [Member], Word16)
attackSelect members from seed skip =
    let m = members !! from in
    if from >= 4
    then
        let seed' = nextRng seed
            attack = attackList m !! (fromIntegral (seed' .>>. 8) .&. 0b111) in
        debug ("[TRACE] attackSelect - from: " ++ show from ++ ", attack: " ++ showHex8 attack ++ ", seed: " ++ showHex16 seed') $
        if attack == 0x53 || attack == 0x59
        then
            let m' = m { buff = buff m .|. 0x08 } in
            (attack, insertAt members from m', seed')
        else (attack, members, seed')
    -- always default attack during manips, selection takes 24 rng cycles
    else
      let skipFrames = if from == 0 then waitRngSkip * skip else 0 in
      (0x01, members, skipRng ((if charID m /= 0x06 then 48 else 0) + skipFrames) seed)

selectTarget :: [Member] -> Int -> Range -> Word16 -> (Int, Word16)
selectTarget members from range seed
    | charID (members !! from) == 0x06 = noncharloop seed
    | from < 4 = (4, seed)
    | otherwise = loop seed
    where
    loop seed =
        let seed' = nextRng seed
            sel = fromIntegral (seed' .>>. 13) in
        debug ("[TRACE] selectTarget.loop - from: " ++ show from ++ ", sel: " ++ show sel)
        debug ("[TRACE] selectTarget.loop - from: " ++ show from ++ ", seed: " ++ showHex16 seed') $
        if alive (members !! sel) && rangeSelect range sel then (sel, seed')
        else loop seed'
    rangeSelect range sel =
        case range of
        Enemy -> sel < 4
        Ally  -> sel >= 4
        _     -> error "rangeSelect: can't select target for range \"All\""
    noncharloop seed =
        let seed' = nextRng seed
            sel = fromIntegral (seed' .>>. 13) .|. 4 in
        debug ("[TRACE] selectTarget.noncharloop - from: " ++ show from ++ ", sel: " ++ show sel)
        debug ("[TRACE] selectTarget.noncharloop - from: " ++ show from ++ ", seed: " ++ showHex16 seed') $
        if alive (members !! sel) then (sel, seed')
        else noncharloop seed'

getAttackRange :: Word8 -> Range
getAttackRange attackId =
    let x = fromIntegral (attackId .&. 0x03) + 0x01
        a = fst $ foldr ($) (0x01, True) (replicate x (ror . ror))
        b = a .&. (attackRangeMap !! fromIntegral (attackId .>>. 2))
        r = loop a b in 
    case r of
    0 -> All
    1 -> Ally
    _ -> Enemy
    where
    loop a b = if (a .&. 0x01) /= 0x00 then b else loop (a .>>. 1) (b .>>. 1)

-- for crit  - from: caster,   to: receiver
-- for dodge - from: receiver, to: caster
cdRate :: [Member] -> Int -> Int -> Word16 -> (Bool, Word16)
cdRate members from to seed =
    let mFrom = members !! from
        mTo = members !! to
        rate = to8 (fight mFrom - fight mTo) `div` 2
        seed' = nextRng seed
        rate' = if fight mFrom < fight mTo
                then if ((rate - 0x66) .&. 0x80) /= 0x00 then 0x00 else rate - 0x66
                else rate + 0x1a in
    (to8 (seed' .>>. 8) < rate', seed')

attackDamage :: [Member] -> Int -> Int -> Word16
attackDamage members from to =
    let mFrom = members !! from
        mTo = members !! to
        dmg | defense mTo <= attack mFrom = attack mFrom - (defense mTo `div` 2)
            | (attack mFrom * 3) < defense mTo = 0x00
            | otherwise = ((attack mFrom * 3) - defense mTo) `div` 4 in
    if dmg == 0x00 then 0x0001 else dmg

critDamage :: [Member] -> Int -> Word16 -> (Word16, Word16)
critDamage members from =
    valueRoll (attack (members !! from))

battleEnd :: [Member] -> TurnState
battleEnd members
    | not (any alive (mustLive $ take 4 members)) = Loss
    | not (any alive (drop 4 members)) = Win
    | otherwise = Continue
    where
    mustLive = filter ((`elem` [ 0x01, 0x03 ]) . charID)

applyDamage :: Word16 -> [Member] -> Int -> Int -> Word16 -> (TurnState, [Member], [String], Word16)
applyDamage dmg members from to s =
    let mFrom = members !! from
        mTo = members !! to
        (dmg', s') = valueRoll dmg s
        dmg'' = if (buff mTo .&. 0x10) /= 0x00 then dmg' `div` 2 else dmg'
        dmg''' = if (buff mTo .&. 0x08) /= 0x00 then dmg'' `div` 2 else dmg''
        dmg'''' = if dmg''' == 0x0000 then 0x0001 else dmg'''
        (members', history, s'') = 
            if hp mTo <= dmg''''
            then (insertAt members to (mTo { hp = 0x0000, status = status mTo .|. 0x80, statusMask = 0x00 }),
                  [printf "%s was beaten!" (name mTo)], s')
            else 
                let mTo' = if charID mTo /= 0x06 then mTo { hp = hp mTo - dmg'''' } else mTo
                    (mTo'', history', s'') = if (status mTo' .&. 0x0c) /= 0x00 then
                        let s'' = nextRng s' in
                        if (s'' .&. 0xc0) == 0x00 then
                            (mTo' { status = status mTo' .&. 0xf3 },
                             [printf "%s wasn't confused anymore!" $ name mTo], s'')
                        else (mTo', [], s'')
                    else (mTo', [], s')
                    (mTo''', history'', s''') = if (status mTo' .&. 0x10) /= 0x00 then
                        let s''' = nextRng s'' in
                        if (s''' .&. 0xc0) == 0x00 then
                            (mTo'' { status = status mTo'' .&. 0xef },
                             history' ++ [printf "%s woke up." $ name mTo], s''')
                        else (mTo'', history', s''')
                    else (mTo'', history', s'') in
                (insertAt members to mTo''', history'', s''') in
    debug ("[TRACE] applyDamage - seed: " ++ showHex16 s)
    debug ("[TRACE] applyDamage - dmg: " ++ show dmg)
    debug ("[TRACE] applyDamage - dmg': " ++ show dmg')
    debug ("[TRACE] applyDamage - dmg'': " ++ show dmg'')
    debug ("[TRACE] applyDamage - dmg''': " ++ show dmg''')
    debug ("[TRACE] applyDamage - dmg'''': " ++ show dmg'''')
    debug ("[TRACE] applyDamage - inflicted damage: " ++ show dmg'''' ++ ", from: " ++ show from ++ ", to: " ++ show to ++ ", seed: " ++ showHex16 s'')
    (battleEnd members', members', printf "%s suffered damage of %d." (name mTo) dmg'''' : history, s'')

type ActionRet = (TurnState, [Member], [String], Word16)

performStatus :: (Monad m) => [Member] -> Int -> Word16 ->
                 (ActionRet -> m ActionRet) ->
                 m ActionRet
performStatus m from s exit
    -- status mask 0x00
    | statusMask (m !! from) == 0x00 = exit (Continue, m, [], s)
    -- dead
    | not (alive (m !! from)) = exit (Continue, m, [], s)
    -- stone
    | (status (m !! from) .&. 0x40) /= 0x00 =
        exit (Continue, m, [printf "%s turned into a stone." $ name (m !! from)], s)
    -- paralyzed
    | (status (m !! from) .&. 0x20) /= 0x00 =
        exit (Continue, m, [printf "%s can't move." $ name (m !! from)], s)
    -- sleep
    | (status (m !! from) .&. 0x10) /= 0x00 = do
        s <- return $ nextRng s
        if s .&. 0xe0 /= 0x00 then
            exit (Continue, m, [printf "%s is asleep." $ name (m !! from)], s)
        else do
            -- cure from sleep
            m <- return $ insertAt m from ((m !! from) { status = status (m !! from) .&. 0xef })
            return (Continue, m, [printf "%s woke up." $ name (m !! from)], s)
    -- day dreaming
    | (status (m !! from) .&. 0x04) /= 0x00 =
        exit (Continue, m, [printf "%s is day-dreaming." $ name (m !! from)], s)
    -- asthma
    | (buff (m !! from) .&. 0x02) /= 0x00 && (attackSel (m !! from) /= 0x76) =
        exit (Continue, m, [printf "%s had an asthma attack." $ name (m !! from), "Wheeze..."], s)
    -- rope
    | (buff (m !! from) .&. 0x20) /= 0x00 = do
        s <- return $ nextRng s
        if s .&. 0xc0 /= 0x00 then
            exit (Continue, m, [printf "%s escaped from the Rope." $ name (m !! from)], s)
        else do
            m <- return $ insertAt m from ((m !! from) { status = status (m !! from) .&. 0xdf })
            return (Continue, m, [printf "%s is bound tightly." $ name (m !! from)], s)
    -- confusion
    | (status (m !! from) .&. 0x08) /= 0x00 = 
        return (Continue, m, [printf "%s is so confused." $ name (m !! from)], s)
    | otherwise = return (Continue, m, [], s)

-- actions framework
data ActionParam = ActionParam
    { afFixedDamage :: Maybe Word16
    , afPP :: Maybe Word16
    , afBuff :: Maybe Word8 }

data ActionMem = ActionMem
    { afFrom :: Int
    , afTo :: Int
    , afDamage :: Maybe Word16
    , afParam :: ActionParam }

type ActionCont = (TurnState, [Member], [String], Word16, ActionMem)

type ActionFun = ActionCont -> (ActionRet -> ContT ActionRet Identity (Bool, ActionCont)) ->
                 ContT ActionRet Identity (Bool, ActionCont)
returnY x = return (True, x)
returnN x = return (False, x)

type ActionMap = Map Int ActionFun

data ActionDecNode = Next Int | NextYN (Int, Int)
type ActionDec = Map Int ActionDecNode

type ActionGraph = (ActionMap, ActionDec, ActionParam)

-- turn loop based on attack graphs
runAction :: ActionRet -> Int -> ActionGraph -> ActionRet
runAction (ts, m, h, s) fr (amap, adec, afp) =
    let initMem = ActionMem { afFrom = fr, afTo = attackTarg (m !! fr), afDamage = Nothing, afParam = afp }
        initCont = (ts, m, h, s, initMem) in
    execCont $ callCC $ \exit -> do
        (loop, (cont, i)) <- label (initCont, 0)
        (b, cont) <- (amap M.! i) cont exit
        (case (adec M.! i, b) of
         (Next j, _) -> loop (cont, j)
         (NextYN (j, _), True) -> loop (cont, j)
         (NextYN (_, j), False) -> loop (cont, j))

afConfusion :: ActionFun
afConfusion (ts, m, h, s, am) _ =
    let fr = afFrom am in
    if (status (m !! fr) .&. 0x08) /= 0x00 then do
        let loop s =
                let s' = nextRng s
                    sel = fromIntegral (s' .>>. 5) in
                if alive (m !! sel) then (sel, s)
                else loop s
        (sel, s) <- return $ loop s
        m <- return $ insertAt m fr (m !! fr) { attackTarg = sel }
        returnY (ts, m, h, s, am)
    else returnY (ts, m, h, s, am)

afWideConfusion :: ActionFun
afWideConfusion (ts, m, h, s, am) _ =
    let fr = afFrom am in
    if (status (m !! fr) .&. 0x08) /= 0x00 then
        let s' = nextRng s
            conf = (s' .&. 0x8000) /= 0x00
            am' = am { afTo = if (fr < 4) || (conf && fr >= 4)
                              then 4 
                              else 0 } in
        returnY (ts, m, h, s', am')
    else
        let am' | fr < 4 = am { afTo = 4 }
                | otherwise = am { afTo = 0 } in
        returnY (ts, m, h, s, am')

afAttacksText :: ActionFun
afAttacksText (ts, m, h, s, am) _ =
    returnY (ts, m, h ++ [printf "%s's attack!" $ name (m !! afFrom am)], s, am)

afNoTarget :: ActionFun
afNoTarget (ts, m, h, s, am) _ =
    let fr = afFrom am
        to = afTo am in
    if not (alive (m !! fr)) || not (alive (m !! to))
        then returnY (ts, m, h, s, am)
        else returnN (ts, m, h, s, am)

afNoCaster :: ActionFun
afNoCaster (ts, m, h, s, am) _ =
    let fr = afFrom am in
    if not (alive (m !! fr))
        then returnY (ts, m, h, s, am)
        else returnN (ts, m, h, s, am)

afGone :: ActionFun
afGone (ts, m, h, s, am) exit =
    let to = afTo am in
    exit (Continue, m, h ++ [printf "%s was already gone." $ name (m !! to)], s)

afCrit :: ActionFun
afCrit (ts, m, h, s, am) _ = do
    let fr = afFrom am
        to = afTo am
    (crit, s) <- return $ cdRate m fr to s
    if crit then do
        let oldDmg = fromMaybe 0x0000 (afDamage am)
            dmg = attack (m !! fr)
        h <- return $ h ++ ["SMAAAASH!!"]
        am <- return $ am { afDamage = Just (oldDmg + dmg) }
        returnY (ts, m, h, s, am)
    else returnN (ts, m, h, s, am)

afDodge :: ActionFun
afDodge (ts, m, h, s, am) _ = do
    let fr = afFrom am
        to = afTo am
    if status (m !! to) .&. 0x70 /= 0x00 || buff (m !! to) .&. 0x80 /= 0x00
    then returnN (ts, m, h, s, am)
    else do
      (dodge, s) <- return $ cdRate m to fr s
      if not dodge then returnN (ts, m, h, s, am)
      else returnY (ts, m, h ++ [printf "%s dodged swiftly." $ name (m !! to)], s, am)

afDamageCompute :: ActionFun
afDamageCompute (ts, m, h, s, am) _ = do
    let fr = afFrom am
        to = afTo am
        oldDmg = fromMaybe 0x0000 (afDamage am)
        dmg = attackDamage m fr to
    am <- return $ am { afDamage = Just (oldDmg + dmg) }
    returnY (ts, m, h, s, am)

afApplyDamage :: ActionFun
afApplyDamage (ts, m, h, s, am) exit = do
    let fr = afFrom am
        to = afTo am
        dmg = fromJust (afDamage am)
    (fr, to, am, h) <- if (buff (m !! to) .&. 0x04) /= 0x00 then do
        -- swap users
        am <- return $ am { afFrom = to, afTo = fr }
        return (to, fr, am, h ++ [printf "%s bounced back the attack!" $ name (m !! to)])
    else return (fr, to, am, h)
    (ts, m, h', s) <- return $ applyDamage dmg m fr to s
    h <- return $ h ++ h'
    (case ts of
     Continue -> returnY (ts, m, h, s, am)
     _ -> exit (ts, m, h, s))

afContinuous :: ActionFun
afContinuous (ts, m, h, s, am) _ =
    returnY (ts, m, h ++ ["Continuous attack!"], s, am)

afExit :: ActionFun
afExit (ts, m, h, s, _) exit = do
    if all (\m -> statusMask m == 0x00 || status m .&. 0xe0 /= 0x00) m then
        exit (Loss, m, h ++ ["You lost the battle."], s)
    else exit (ts, m, h, s)

afBitText :: ActionFun
afBitText (ts, m, h, s, am) _ =
    let fr = afFrom am
        to = afTo am in
    returnY (ts, m, h ++ [printf "%s bit %s!" (name (m !! fr)) (name (m !! to))], s, am)

afCheckPP :: ActionFun
afCheckPP (ts, m, h, s, am) _ =
    let fr = afFrom am
        mpp = fromJust (afPP $ afParam am) in
    if buff (m !! fr) .&. 0x40 /= 0 then
        returnN (ts, m, h ++ ["But, the PSI was blocked."], s, am)
    else if pp (m !! fr) < mpp then
        returnN (ts, m, h ++ ["Not enough PP's."], s, am)
    else do
        m <- return $ insertAt m fr (m !! fr) { pp = pp (m !! fr) - mpp }
        returnY (ts, m, h, s, am)

afFixedDamageCompute :: ActionFun
afFixedDamageCompute (ts, m, h, s, am) _ =
    returnY (ts, m, h, s, am { afDamage = Just $ fromJust $ afFixedDamage $ afParam am })

afPsiBeamAText :: ActionFun
afPsiBeamAText (ts, m, h, s, am) _ =
    returnY (ts, m, h ++ [printf "%s tried PK Beam a!" (name (m !! afFrom am))], s, am)

afPsiBeamBText :: ActionFun
afPsiBeamBText (ts, m, h, s, am) _ =
    returnY (ts, m, h ++ [printf "%s tried PK Beam B!" (name (m !! afFrom am))], s, am)

afPsiBeamRText :: ActionFun
afPsiBeamRText (ts, m, h, s, am) _ =
    returnY (ts, m, h ++ [printf "%s tried PK Beam r!" (name (m !! afFrom am))], s, am)

afBeamFranklin :: ActionFun
afBeamFranklin (ts, m, h, s, am) _ =
    let fr = afFrom am
        to = afTo am in
    if to < 4 && elem 0x68 (bag (m !! to)) then
        -- swapping users
        returnY (ts, m, h ++ [printf "But, %s's FranklinBadge bounced back the Beam!" $ name (m !! to)],
                 s, am { afFrom = to, afTo = fr })
    else returnN (ts, m, h, s, am)

afBeamResistance :: ActionFun
afBeamResistance (ts, m, h, s, am) _ =
    let to = afTo am in
    if resistance (m !! to) .&. 0x80 /= 0x00 then
        returnY (ts, m, h ++ [printf "There was no effect on %s." $ name (m !! to)], s, am)
    else returnN (ts, m, h, s, am)

afInstantKill :: ActionFun
afInstantKill (ts, m, h, s, am) exit =
    let fr = afFrom am
        to = afTo am in
    if charID (m !! to) == 0x06 then do
        s <- return $ nextRng s
        let dmg = (s .>>. 8) .&. 0x03
        am <- return $ am { afDamage = Just dmg }
        afApplyDamage (ts, m, h, s, am) exit
    else do
        m <- return $ insertAt m to (m !! to) { status = status (m !! to) .|. 0x80
                                              , statusMask = 0x00
                                              , hp = 0x0000 }
        returnY (battleEnd m, m, h ++ [printf "%s was beaten!" $ name (m !! to)], s, am)

afPlaceBuff :: ActionFun
afPlaceBuff (ts, m, h, s, am) _ =
    let fr = afFrom am
        to = afTo am
        pBuff = fromJust $ afBuff $ afParam am
        m' = insertAt m to (m !! to) { buff = buff (m !! to) .|. pBuff } in
    returnY (ts, m, h, s, am)

afPlaceBuffYN :: ActionFun
afPlaceBuffYN (ts, m, h, s, am) _ =
    let fr = afFrom am
        to = afTo am
        pBuff = fromJust $ afBuff $ afParam am in
    if buff (m !! to) .&. pBuff /= 0x00 then
        returnN (ts, m, h ++ [printf "There was no effect on %s." $ name (m !! to)], s, am)
    else do
        m <- return $ insertAt m to (m !! to) { buff = buff (m !! to) .|. pBuff }
        returnY (ts, m, h, s, am)

afShieldedText :: ActionFun
afShieldedText (ts, m, h, s, am) _ =
    returnY (ts, m, h ++ [printf "%s was shielded." (name (m !! afTo am))], s, am)

afPSIShieldAText :: ActionFun
afPSIShieldAText (ts, m, h, s, am) _ =
    returnY (ts, m, h ++ [printf "%s tried PSIShield a!" (name (m !! afFrom am))], s, am)

afReadyText :: ActionFun
afReadyText (ts, m, h, s, am) _ =
    returnY (ts, m, h ++ [printf "%s is ready for an attack." (name (m !! afFrom am))], s, am)

afTrippedFell :: ActionFun
afTrippedFell (ts, m, h, s, am) _ =
    returnY (ts, m, h ++ [printf "%s tripped and fell." (name (m !! afFrom am))], s, am)

afUsedBombText :: ActionFun
afUsedBombText (ts, m, h, s, am) _ =
    returnY (ts, m, h ++ [printf "%s USEd Bomb!" (name (m !! afFrom am))], s, am)

afIncrTarg :: ActionFun
afIncrTarg (ts, m, h, s, am) _ =
    let am' = am { afTo = afTo am + 1 } in
    if afTo am' == 4 || afTo am' == 8
        then returnN (ts, m, h, s, am')
        else returnY (ts, m, h, s, am')

afDrawsNearText :: ActionFun
afDrawsNearText (ts, m, h, s, am) _ =
    returnY (ts, m, h ++ [printf "%s draws near!" (name (m !! afFrom am))], s, am)

afRanAwayText :: ActionFun
afRanAwayText (ts, m, h, s, am) _ =
    returnY (ts, m, h ++ [printf "%s ran away!" (name (m !! afFrom am))], s, am)

afCoinFlip :: ActionFun
afCoinFlip (ts, m, h, s, am) _ =
    let s' = nextRng s in
    if s' .&. 0x8000 /= 0x0000
        then returnY (ts, m, h, s', am)
        else returnN (ts, m, h, s', am)

afDidntWorkText :: ActionFun
afDidntWorkText (ts, m, h, s, am) _ =
    returnY (ts, m, h ++ ["...But it didn't work out."], s, am)

afFled :: ActionFun
afFled (ts, m, h, s, am) exit = exit (Win, m, h, s)

-- attack DAG constants
map01 = M.fromList
    [ (0, afConfusion)
    , (1, afAttacksText)
    , (2, afNoTarget)
    , (3, afCrit)
    , (4, afDodge)
    , (5, afDamageCompute)
    , (6, afApplyDamage)
    , (7, afExit)
    , (8, afGone) ]
dec01 = M.fromList
    [ (0, Next 1)
    , (1, Next 2)
    , (2, NextYN (8, 3))
    , (3, NextYN (6, 4))
    , (4, NextYN (7, 5))
    , (5, Next 6)
    , (6, Next 7) ]
afp01 = ActionParam
    { afFixedDamage = Nothing
    , afPP = Nothing
    , afBuff = Nothing }

map02 = M.fromList
    [ (0, afConfusion)
    , (1, afAttacksText)
    , (2, afNoTarget)
    , (3, afDodge)
    , (4, afDamageCompute)
    , (5, afApplyDamage)
    , (6, afContinuous)
    , (7, afDodge)
    , (8, afDamageCompute)
    , (9, afApplyDamage)
    , (10, afExit)
    , (11, afGone) ]
dec02 = M.fromList
    [ (0, Next 1)
    , (1, Next 2)
    , (2, NextYN (11, 3))
    , (3, NextYN (6, 4))
    , (4, Next 5)
    , (5, Next 6)
    , (6, Next 7)
    , (7, NextYN (10, 8))
    , (8, Next 9)
    , (9, Next 10) ]
afp02 = ActionParam
    { afFixedDamage = Nothing
    , afPP = Nothing
    , afBuff = Nothing }

map03 = M.fromList
    [ (0, afConfusion)
    , (1, afBitText)
    , (2, afDamageCompute)
    , (3, afNoTarget)
    , (4, afDodge)
    , (5, afApplyDamage)
    , (6, afExit)
    , (7, afGone) ]
dec03 = M.fromList
    [ (0, Next 1)
    , (1, Next 2)
    , (2, Next 3)
    , (3, NextYN (7, 4))
    , (4, NextYN (6, 5))
    , (5, Next 6) ]
afp03 = ActionParam
    { afFixedDamage = Nothing
    , afPP = Nothing
    , afBuff = Nothing }

map10 = M.fromList
    [ (0, afUsedBombText)
    , (1, afFixedDamageCompute)
    , (2, afWideConfusion)
    , (3, afNoTarget)
    , (4, afApplyDamage)
    , (5, afIncrTarg)
    , (6, afExit) ]
dec10 = M.fromList
    [ (0, Next 1)
    , (1, Next 2)
    , (2, Next 3)
    , (3, NextYN (5, 4))
    , (4, Next 5)
    , (5, NextYN (3, 6)) ]
afp10 = ActionParam
    { afFixedDamage = Just 0x003c
    , afPP = Nothing
    , afBuff = Nothing }

map12 = M.fromList
    [ (0, afPsiBeamAText)
    , (1, afCheckPP)
    , (2, afFixedDamageCompute)
    , (3, afConfusion)
    , (4, afNoTarget)
    , (5, afApplyDamage)
    , (6, afExit)
    , (7, afGone) ]
dec12 = M.fromList
    [ (0, Next 1)
    , (1, NextYN (2, 6))
    , (2, Next 3)
    , (3, Next 4)
    , (4, NextYN (7, 5))
    , (5, Next 6) ]
afp12 = ActionParam
    { afFixedDamage = Just 0x001e
    , afPP = Just 0x04
    , afBuff = Nothing }

map13 = M.fromList
    [ (0, afPsiBeamBText)
    , (1, afCheckPP)
    , (2, afFixedDamageCompute)
    , (3, afConfusion)
    , (4, afNoTarget)
    , (5, afApplyDamage)
    , (6, afExit)
    , (7, afGone) ]
dec13 = M.fromList
    [ (0, Next 1)
    , (1, NextYN (2, 6))
    , (2, Next 3)
    , (3, Next 4)
    , (4, NextYN (7, 5))
    , (5, Next 6) ]
afp13 = ActionParam
    { afFixedDamage = Just 0x0050
    , afPP = Just 0x07
    , afBuff = Nothing }

map15 = M.fromList
    [ (0, afPsiBeamRText)
    , (1, afCheckPP)
    , (2, afConfusion)
    , (3, afNoTarget)
    , (4, afBeamFranklin)
    , (5, afBeamResistance)
    , (6, afInstantKill)
    , (7, afExit)
    , (8, afGone) ]
dec15 = M.fromList
    [ (0, Next 1)
    , (1, NextYN (2, 7))
    , (2, Next 3)
    , (3, NextYN (8, 4))
    , (4, Next 5)
    , (5, NextYN (7, 6))
    , (6, Next 7) ]
afp15 = ActionParam 
    { afFixedDamage = Nothing
    , afPP = Just 0x0c
    , afBuff = Nothing }

map38 = M.fromList
    [ (0, afPSIShieldAText)
    , (1, afCheckPP)
    , (2, afConfusion)
    , (3, afNoTarget)
    , (4, afPlaceBuffYN)
    , (5, afShieldedText)
    , (6, afExit)
    , (7, afGone) ]
dec38 = M.fromList
    [ (0, Next 1)
    , (1, NextYN (2, 6))
    , (2, Next 3)
    , (3, NextYN (7, 4))
    , (4, NextYN (5, 6))
    , (5, Next 6) ]
afp38 = ActionParam 
    { afFixedDamage = Nothing
    , afPP = Just 0x10
    , afBuff = Just 0x10 }

map46 = M.fromList
    [ (0, afTrippedFell)
    , (1, afExit) ]
dec46 = M.fromList [(0, Next 1)]
afp46 = ActionParam
    { afFixedDamage = Nothing
    , afPP = Nothing
    , afBuff = Nothing }

map48 = M.fromList
    [ (0, afRanAwayText)
    , (1, afCoinFlip)
    , (2, afDidntWorkText)
    , (3, afNoCaster)
    , (4, afFled)
    , (5, afExit) ]
dec48 = M.fromList
    [ (0, Next 1)
    , (1, NextYN (3, 2))
    , (2, Next 5)
    , (3, NextYN (5, 4)) ]
afp48 = ActionParam
    { afFixedDamage = Nothing
    , afPP = Nothing
    , afBuff = Nothing }

map53 = M.fromList
    [ (0, afReadyText)
    , (1, afPlaceBuff)
    , (2, afExit) ]
dec53 = M.fromList
    [ (0, Next 1)
    , (1, Next 2) ]
afp53 = ActionParam
    { afFixedDamage = Nothing
    , afPP = Nothing
    , afBuff = Just 0x08 }

map5e = M.fromList
    [ (0, afDrawsNearText)
    , (1, afExit) ]
dec5e = M.fromList [(0, Next 1)]
afp5e = ActionParam
    { afFixedDamage = Nothing
    , afPP = Nothing
    , afBuff = Nothing }

performAttack :: [Member] -> Int -> Word16 -> ActionRet
performAttack m from s =
    execCont $ callCC $ \exit -> callCC $ \exit2 -> do
    (ts, m, h, s) <- performStatus m from s exit2
    (if ts /= Continue then exit (ts, m, h, s) else
     (case attackSel (m !! from) of
      -- skip turn
      0x00 -> return (Continue, m, h, s)
      -- normal attack
      0x01 -> runGraph (map01, dec01, afp01) m h s
      -- continuous attack
      0x02 -> runGraph (map02, dec02, afp02) m h s
      -- bite attack
      0x03 -> runGraph (map03, dec03, afp03) m h s
      -- USEd Bomb
      0x10 -> runGraph (map10, dec10, afp10) m h s
      -- PK Beam a
      0x12 -> runGraph (map12, dec12, afp12) m h s
      -- PK Beam b
      0x13 -> runGraph (map13, dec13, afp13) m h s
      -- PK Beam r
      0x15 -> runGraph (map15, dec15, afp15) m h s
      -- PSIShield a
      0x38 -> runGraph (map38, dec38, afp38) m h s
      -- tripped and fell
      0x46 -> runGraph (map46, dec46, afp46) m h s
      -- ran away
      0x48 -> runGraph (map48, dec48, afp48) m h s
      -- ready for an attack
      0x53 -> runGraph (map53, dec53, afp53) m h s
      -- draws near
      0x5e -> runGraph (map5e, dec5e, afp5e) m h s
      e -> error $ printf "performAttack: unsupported attack \"0x%02hhx\"." e))
    where runGraph g m h s = return $ runAction (Continue, m, h, s) from g

runMembersAttacks :: ActionRet -> ActionRet
runMembersAttacks (ts, members, history, seed) =
    runUntilStop $
    scanl (\(ts, members, history, seed) _ ->
        let (caster, seed') = battlePriority members seed
            (ts, members', history', seed'') = performAttack members caster seed'
            members'' = case ts of
                        Continue ->
                            let m = members' !! caster in
                            insertAt members' caster (m { attackSel = 0xff })
                        _ -> members' in
            debug ("[TRACE] runMembersAttacks - current caster: " ++ show caster ++ ", seed: " ++ showHex16 seed')
        (ts, members'', history ++ history', seed'')) (Continue, members, [], seed) members
    where
    runUntilStop scan =
        case find (\(ts, _, _, _) -> ts /= Continue) scan of
        Just x -> x
        Nothing -> last scan

battleTurn :: [Member] -> Word16 -> Int -> ActionRet
battleTurn members seed skip =
    let (members', seed') = foldl' (\(members, seed) (m, i) ->
            let m1 = m  { attackSel = 0x00, buff = buff m .&. 0xf7 } in
            if statusMask m1 == 0x00 || status m1 .&. 0xf4 /= 0x00 || buff m1 .&. 0x20 /= 0x00
            then (insertAt members i m1, seed) else
            let (id, members', seed') = attackSelect (insertAt members i m1) i seed skip
                m2 = (members' !! i) { attackSel = id }
                members'' = insertAt members' i m2
                range = getAttackRange id in
            debug ("[TRACE] battleTurn - selected attack: " ++ showHex8 id ++ ", range: " ++ show range ++ ", from: " ++ show i ++ ", seed': " ++ showHex16 seed' ++ ", seed: " ++ showHex16 seed) $
            case range of
            All ->
                let m3 = m2 { attackTarg = i } in
                (insertAt members'' i m3, seed')
            _ ->
                let (j, seed'') = selectTarget members'' i range seed'
                    m3 = m2 { attackTarg = j } in
                debug ("[TRACE] battleTurn - selected target: " ++ show j)
                (insertAt members'' i m3, seed'')) (members, seed) (zip members [0..]) in
    debug ("[TRACE] battleTurn - members after move selection: \n" ++ intercalate "\n" (map showMember members'))
    debug ("[TRACE] battleTurn - seed after move selection: " ++ showHex16 seed')
    runMembersAttacks (Continue, members', [], seed')

battleBegin :: [Member] -> Word16 -> ActionRet
battleBegin members seed =
    let members' = zipWith (\m i ->
          let attk | i < 4 = 0x00
                   | alive m = 0x5e
                   | otherwise = 0x00 in
          m { attackSel = attk }) members [0..] in
    debug ("[TRACE] battleBegin - members: \n" ++ intercalate "\n" (map showMember members'))
    runMembersAttacks (Continue, members', [], seed)

-- nbAlive :: [Member] -> Int
-- nbAlive = length . filter (\m -> alive m && (charID m /= 0x06))

levelUp :: Member -> [String] -> Word16 -> (Member, [String], Word16)
levelUp member history seed =
    let id = charID member
        name' = name member
        h = history ++ [printf "%s advanced to the next Level!" name']
        (figC, speC, wisC, stgC, forC) = statUpMap M.! id
        s = skipRng waitRngSkip seed in
    execCont $ callCC $ \exit -> do
        s <- return $ nextRng s
        let figI = statFunc figC s
        s <- return $ nextRng s
        let speI = statFunc speC s
        s <- return $ nextRng s
        let wisI = statFunc wisC s
        s <- return $ nextRng s
        let stgI = statFunc stgC s
        s <- return $ nextRng s
        let forI = statFunc forC s
        member <- debug ("[TRACE] levelUp - seed after increases: " ++ showHex16 s) $
                  debug ("[TRACE] levelUp - FIGHT increase: " ++ show figI) $
                  debug ("[TRACE] levelUp - SPEED increase: " ++ show speI) $
                  debug ("[TRACE] levelUp - WISDOM increase: " ++ show wisI) $
                  debug ("[TRACE] levelUp - STRENGTH increase: " ++ show stgI) $
                  debug ("[TRACE] levelUp - FORCE increase: " ++ show forI) $
                  return $ member
            { attack = incrStat16 (attack member) (to16 figI)
            , defense = incrStat16 (defense member) (to16 speI)
            , fight = incrStat (fight member) figI
            , speed = incrStat (speed member) speI
            , wisdom = incrStat (wisdom member) wisI
            , strength = incrStat (strength member) stgI
            , force = incrStat (force member) forI
            , level = incrStat (level member) 0x01 }
        (h, s) <- return $ textFunc (figI, speI, wisI, stgI, forI) h s
        s <- return $ nextRng s
        let hpI = hpFunc (strength member) (maxHp member) s
        (h, s) <- debug ("[TRACE] levelUp - seed for HP increase: " ++ showHex16 s) $
                  debug ("[TRACE] levelUp - HP increase: " ++ show hpI) $
                  return $ if hpI /= 0x00
                           then (h ++ [printf "Maximum HP has increased by %d." hpI], skipRng waitRngSkip s)
                           else (h, s)
        member <- return $ member { maxHp = maxHp member + to16 hpI }
        when (charID member == 0x03) (exit (member, h, s))
        s <- return $ nextRng s
        let ppI = ppFunc (force member) (maxPp member) s
        (h, s) <- debug ("[TRACE] levelUp - seed for PP increase: " ++ showHex16 s) $
                  debug ("[TRACE] levelUp - PP increase: " ++ show ppI) $
                  return $ if ppI /= 0x00
                           then (h ++ [printf "Maximum PP has increased by %d." ppI], skipRng waitRngSkip s)
                           else (h, s)
        member <- return $ member { maxPp = maxPp member + to16 ppI }
        return (member, h, s)
    where
    statFunc const seed = (const + to8 (seed .>>. 14)) .>>. 1
    incrStat s x = if to16 s + to16 x > 0x00ff then 0xff else s + x
    incrStat16 s x = if to32 s + to32 x > 0x0000ffff then 0xffff else s + x
    textFuncLoop ((i, txt) : l) h s =
        if i == 0x00 then textFuncLoop l h s else
        let s' = skipRng waitRngSkip s
            h' = h ++ [printf "%s has increased by %d." txt i] in
        textFuncLoop l h' s'
    textFuncLoop _ h s = (h, s)
    textFunc (figI, speI, wisI, stgI, forI) h s =
        let l = [(figI, "FIGHT"), (speI, "SPEED"), (wisI, "WISDOM"),
                 (stgI, "STRENGTH"), (forI, "FORCE")] in
        textFuncLoop l h s
    hpppf val stat seed =
        let n = val - stat
            y | val < stat = 0x02
              | n .&. 0xff00 == 0x0000 = min 0x10 (2 * to8 n)
              | otherwise = 0x10
            x = to8 (seed .>>. 11) in
        debug ("[TRACE] hpppf - val: " ++ showHex16 val) $
        debug ("[TRACE] hpppf - stat: " ++ showHex16 stat) $
        debug ("[TRACE] hpppf - n: " ++ showHex16 n) $
        debug ("[TRACE] hpppf - x: " ++ showHex8 x) $
        debug ("[TRACE] hpppf - y: " ++ showHex8 y) $
        execCont $ do
            (x, c) <- return $ ror (x, False)
            (i, a) <- return (0x08, 0x00)
            (loop, (x, c, i, a)) <- label (x, c, i, a)
            (x, c) <- return $ ror (x, c)
            (a, c) <- return $ if c then (a + y, to16 a + to16 y > 0x00ff) else (a, c)
            (a, c) <- return $ ror (a, c)
            i <- return $ i - 1
            when (i /= 0x00) (loop (x, c, i, a))
            (x, _) <- return $ ror (x, c)
            return $ to8 (x .>>. 4)
    hpFunc str mhp seed =
        let n = to16 (if to16 str + 0x0014 > 0x00ff then 0xff else str + 0x14) + to16 str in
        hpppf n mhp seed
    ppFunc for mpp seed =
        let n = to16 ((for .>>. 1) + for) in
        hpppf n mpp seed

learnNewPsi :: Member -> [String] -> Word16 -> (Member, [String], Word16)
learnNewPsi member history seed =
    if charID member `notElem` [ 0x01, 0x02 ] then (member, history, seed) else
    let (member', history', seed') = foldl' (\(member, history, seed) (i, (psiName, reqLevel)) ->
            let psiListI = (i `div` 8)
                psiElemI = 0x80 .>>. (i `mod` 8)
                elem = learnedPsi member !! psiListI in
            if level member > reqLevel && (elem .&. psiElemI) == 0x00 then
                debug ("[TRACE] learnNewPsi - PSI name: " ++ psiName) $
                debug ("[TRACE] learnNewPsi - seed: " ++ showHex16 seed) $
                debug ("[TRACE] learnNewPsi - level: " ++ show (level member)) $
                debug ("[TRACE] learnNewPsi - required level: " ++ show reqLevel) $
                let seed' = nextRng seed in
                if (seed' .&. 0xc000) == 0x0000 then
                    let history' = history ++ [printf "%s learned a new PSI-power through battle!" (name member)]
                        member' = member { learnedPsi = insertAt (learnedPsi member) psiListI (elem .|. psiElemI) } in
                    debug ("[TRACE] learnNewPsi - learning " ++ psiName)
                    (member', history', skipRng waitRngSkip seed')
                else (member, history, seed')
            else (member, history, seed)) (member, history, seed) (zip [0..] psiTable) in
    (member', history', seed')

charLeveling :: [Member] ->  Word16 -> ([Member], [String], Word16)
charLeveling members seed =
    let totXp = sum (map yieldedXp (filter (not . alive) (drop 4 members)))
        n = to32 ((length . filter (\m -> alive m && (charID m /= 0x06))) (take 4 members))
        divXp = totXp `div` n
        modXp = totXp `mod` n
        xp = divXp + (if modXp /= 0 then 0x000001 else 0x000000)
        members' = map (\m -> if alive m && (charID m /= 0x06)
                              then m { experience = experience m + xp }
                              else m) (take 4 members)
        (members'', history, seed') = levelUpLoop members' [] [] seed xp in
    debug ("[TRACE] charLeveling - xp: " ++ showHex24 xp ++ ", n: " ++ show n ++ ", totXp: " ++ showHex24 totXp)
    (members'' ++ drop 4 members, history, seed')
    where
    levelUpLoop [] members history seed xp = (members, history, seed)
    levelUpLoop (member : ml) members history seed xp =
        let id = charID member
            lvl = to32 (level member + 1)
            neededXp = (lvl * lvl * (lvl + 1) * to32 (fromJust (levelUpMap M.!? id))) .>>. 8 in
        debug ("[TRACE] charLeveling - id: " ++ showHex8 id)
        debug ("[TRACE] charLeveling - xp: " ++ showHex24 (experience member))
        debug ("[TRACE] charLeveling - lvl: " ++ showHex8 (level member)) $
        if id /= 0x06 && alive member && level member < 99 && experience member >= neededXp
        then let (member', history', seed') = levelUp member history seed in
             debug ("[TRACE] charLeveling - neededXp: " ++ showHex24 neededXp)
             levelUpLoop (member' : ml) members history' seed' xp
        else let (member', history', seed') = learnNewPsi member history seed in
             levelUpLoop ml (members ++ [member']) history' seed' xp

lootItem :: [Member] -> Word8 -> Word8 -> Word16 -> ([Member], [String], Word16)
lootItem members 0x00 _ seed = (members, [], seed)
lootItem members yield rate seed =
    let seed' = nextRng seed in
    debug ("[TRACE] lootItem - seed': " ++ showHex16 seed') $
    debug ("[TRACE] lootItem - rate: " ++ showHex8 rate) $
    if to8 (seed' .>>. 8) .&. rate == 0x00 then
        case placeItemMember members yield of
        Just members' -> (members', ["Picked up " ++ (itemIDMap M.! yield) ++ "!"], skipRng waitRngSkip seed')
        _ -> (members, [], seed')
    else (members, [], seed')
    where
    placeBagRec [] _ = []
    placeBagRec (space : bag) item =
        if space == 0x00 then item : bag else space : placeBagRec bag item
    placeBag bag item =
        let bag' = placeBagRec bag item in
        if bag /= bag' then Just bag' else Nothing
    placeItemMemberRec [] _ = []
    placeItemMemberRec (m : members) item =
        case placeBag (bag m) item of
        Just bag' -> m { bag = bag' } : members
        Nothing -> m : placeItemMemberRec members item
    placeItemMember members item =
        let members' = placeItemMemberRec members item in
        if members /= members' then Just members' else Nothing

turnTimeHeuristic :: Int -> Int -> [String] -> Int
turnTimeHeuristic time wait hist =
    time + sum (map length hist) + (length hist * 10) + wait * waitFrames

battleWin :: [Member] -> Word8 -> Word8 -> [String] -> Word16 -> ([Member], [String], Word16)
battleWin members yield rate history seed =
    let (m, h, s) = charLeveling members seed
        (m', h', s') = lootItem m yield rate s
        history' = history ++ ["YOU WIN"] ++ h ++ h' in
    (m', history', s')

successfulBattle :: [Member] -> Word8 -> Word8 -> Word16 -> (Int, ActionRet, [Int])
successfulBattle members yield rate seed =
    let (ts, members', hist, seed') = battleBegin members seed
        time = turnTimeHeuristic 0 0 hist in
    loop hist members' seed' time []
    where
    loop hist members seed time waits =
        let ((ts, members', hist', seed'), wait) = findBattleTurn members seed
            -- TODO: compute real frames, this is an heuristic
            time' = turnTimeHeuristic (time + 100) wait hist'
            rhist = hist ++ hist'
            rwaits = waits ++ [wait] in
        (case ts of
         Continue -> loop rhist members' seed' time' rwaits
         Win ->
            let (members'', hist'', seed'') = battleWin members' yield rate rhist seed' in
            (time', (ts, members'', hist'', seed''), rwaits)
         Loss -> error "successfulBattle: found a Loss during search")
    findBattleTurn members seed =
        fromJust $ firstJust (\i ->
            case battleTurn members seed i of
            (Loss, _, _, _) -> Nothing
            ret -> Just (ret, i)) [0..]

successfulFBattle :: [Member] -> Word8 -> Word8 -> Word16
                     -> ([Member] -> Word16 -> Int -> Maybe (ActionRet, Int))
                     -> ((Int, ActionRet, [Int]) -> Maybe (Int, ActionRet, [Int]))
                     -> (Int, ActionRet, [Int])
successfulFBattle members yield rate seed fbattle fwin =
    let (ts, members', hist, seed') = battleBegin members seed
        time = turnTimeHeuristic 0 0 hist in
    findWin hist members' seed' time
    where
    loop hist members seed time waits =
        let ((ts, members', hist', seed'), wait) = findBattleTurn members seed fbattle
            -- TODO: compute real frames, this is an heuristic
            time' = turnTimeHeuristic (time + 100) wait hist'
            rhist = hist ++ hist'
            rwaits = waits ++ [wait] in
        case ts of
        Continue -> loop rhist members' seed' time' rwaits
        Win ->
            let (members'', hist'', seed'') = battleWin members' yield rate rhist seed' in
            (time', (Win, members'', hist'', seed''), rwaits)
        Loss -> error "successfulBattle: found a Loss during search"
    findBattleTurn members seed fbattle =
        fromJust $ firstJust (fbattle members seed) [0..]
    findWin hist members seed time =
        let ((time', ar, waits), i) =
              fromJust $ firstJust (\i ->
                  fwin (loop hist members (skipRng (i * waitRngSkip) seed) time []) <&> (, i)) [0..] in
        (time', ar, (head waits + i) : tail waits)

fastestBattle :: [Member] -> Word8 -> Word8 -> Word16 -> (Int, ActionRet, [Int])
fastestBattle members yield rate seed =
    let (timeout, orgA, orgW) = successfulBattle members yield rate seed
        (ts, members', hist, seed') = battleBegin members seed
        time = turnTimeHeuristic 0 0 hist in
    fromMaybe (timeout, orgA, orgW) $ loop hist members' seed' time timeout []
    where
    loop hist members seed time timeout waits =
        let waits' = takeWhile (\wait -> time + waitFrames * wait < timeout) [0..]
            battles = catMaybes $ allBattleTurn members seed waits'
            state = foldl' (\ret ((ts, members', hist', seed'), wait) ->
                let timeout' = fromMaybe timeout (ret >>= (\(t, _, _) -> return t))
                    timeout'' = min timeout timeout'
                    time' = turnTimeHeuristic (time + 100) wait hist'
                    rhist = hist ++ hist'
                    rwaits = waits ++ [wait] in
                if time' >= timeout'' then ret else
                case ts of
                Continue ->
                    case loop rhist members' seed' time' timeout'' rwaits of
                    Just (time'', ar, rwaits') -> selRet ret (time'', ar, rwaits')
                    _ -> ret
                Win ->
                    let (members'', hist'', seed'') = battleWin members' yield rate rhist seed'
                        v = (time' , (Win, members'', hist'', seed''), rwaits) in
                    selRet ret v
                Loss -> ret) Nothing battles in
        case state of
        Just (t, _, w) -> state
        _ -> Nothing
    selRet ret v =
        Just $ case ret of
        Just v' -> min v v'
        Nothing -> v
    allBattleTurn members seed =
        map (\i ->
            case battleTurn members seed i of
            (Loss, _, _, _) -> Nothing
            ret -> Just (ret, i))

fastestBattleB :: [Member] -> Word8 -> Word8 -> Word16 -> (Int, ActionRet, [Int])
fastestBattleB members yield rate seed =
    let (timeout, orgA, orgW) = successfulBattle members yield rate seed
        (_, members', hist, seed') = battleBegin members seed
        time = turnTimeHeuristic 0 0 hist in
    fromMaybe (timeout, orgA, orgW) (loop [(time, hist, members', seed', [])] Nothing timeout)
    where
    loop [] f _ = f
    loop l f timeout =
        let
        m' = foldr (\(time, hist, members, seed, waits) m ->
            let
            waits' = takeWhile (\wait -> time + waitFrames * wait < timeout) [0..]
            battles = catMaybes (allBattleTurn members seed waits')
            states = mapMaybe (\((ts, members', hist', seed'), wait) ->
                let
                rhist = hist ++ hist'
                rwaits = waits ++ [wait]
                time' = turnTimeHeuristic (time + 100) wait hist' in
                if time' < timeout && ts /= Loss
                then Just (ts, rhist, members', seed', time', rwaits)
                else Nothing) battles in
            foldr (\(ts, hist, members, seed, time, waits) m ->
                M.insertWith min (members, seed) (time, ts, hist, waits) m) m states) M.empty l
        (cstates, fstate) = foldr (\((members, seed), (time, ts, hist, waits)) (cs, fs) ->
            case ts of
            Continue -> ((time, hist, members, seed, waits) : cs, fs)
            Win ->
                let (members', hist', seed') = battleWin members yield rate hist seed in
                (cs, selMin fs (time, (Win, members', hist', seed'), waits))
            Loss -> (cs, fs)) ([], f) (M.assocs m')
        timeout' =
            case (fstate, cstates) of
            (Nothing, []) -> timeout
            (Nothing, _) ->
                let
                msb = map (\(time, _, members, seed, _) ->
                    let
                    (t, _, _) = successfulBattle members yield rate seed
                    in (t + time)) cstates
                in min timeout (minimum msb)
            (Just (t, _, _), _) -> t in
        debug ("[TRACE] fastestBattleB - cstates len: " ++ show (length cstates)) $
        loop cstates fstate timeout'
    allBattleTurn members seed =
        map (\i ->
            case battleTurn members seed i of
            (Loss, _, _, _) -> Nothing
            ret -> Just (ret, i))
    selMin f s =
        Just $ case f of
        Nothing -> s
        Just f' -> min f' s

fastestFBattle :: [Member] -> Word8 -> Word8 -> Word16
                  -> ([Member] -> Word16 -> Int -> Maybe (ActionRet, Int))
                  -> ((Int, ActionRet, [Int]) -> Maybe (Int, ActionRet, [Int]))
                  -> (Int, ActionRet, [Int])
fastestFBattle members yield rate seed fbattle fwin =
    let (timeout, orgA, orgW) = successfulFBattle members yield rate seed fbattle fwin
        (_, members', hist, seed') = battleBegin members seed
        time = sum (map length hist) + (length hist * 10) + 100 in
    fromMaybe (timeout, orgA, orgW) (loop hist members' seed' time timeout [])
    where
    loop hist members seed time timeout waits =
        let waits' = takeWhile (\wait -> time + waitFrames * wait <= timeout) [0..]
            battles = catMaybes $ allBattleTurn members seed waits'
            state = foldl' (\ret ((ts, members', hist', seed'), wait) ->
                let timeout' = fromMaybe timeout (ret >>= (\(t, _, _) -> return t))
                    time' = turnTimeHeuristic (time + 100) wait hist'
                    rhist = hist ++ hist'
                    rwaits = waits ++ [wait] in
                if time' >= timeout' then ret else
                case ts of
                Continue ->
                    case loop rhist members' seed' time' timeout' rwaits of
                    Just (time'', ar, rwaits') -> selRet ret (time'', ar, rwaits')
                    _ -> ret
                Win ->
                    let (members'', hist'', seed'') = battleWin members' yield rate rhist seed'
                        v = (time', (Win, members'', hist'', seed''), rwaits) in
                    selRet ret v
                Loss -> ret) Nothing battles in
        case state of
        Just (t, _, w) -> state
        _ -> Nothing
    selRet ret v =
        Just $ case ret of
        Just v' -> min v v'
        Nothing -> v
    allBattleTurn members seed = map (fbattle members seed)

fastestFBattleB :: [Member] -> Word8 -> Word8 -> Word16
                   -> ([Member] -> Word16 -> Int -> Maybe (ActionRet, Int))
                   -> ((Int, ActionRet, [Int]) -> Maybe (Int, ActionRet, [Int]))
                   -> (Int, ActionRet, [Int])
fastestFBattleB members yield rate seed fbattle fwin =
    let (timeout, orgA, orgW) = successfulFBattle members yield rate seed fbattle fwin
        (_, members', hist, seed') = battleBegin members seed
        time = turnTimeHeuristic 0 0 hist in
    fromMaybe (timeout, orgA, orgW) (loop [(hist, members', seed', time, [])] Nothing timeout)
    where
    loop [] f _ = f
    loop l f timeout =
        let
        m' = foldr (\(hist, members, seed, time, waits) m ->
            let
            waits' = takeWhile (\wait -> time + waitFrames * wait < timeout) [0..]
            battles = catMaybes (allBattleTurn members seed waits')
            states = mapMaybe (\((ts, members', hist', seed'), wait) ->
                let
                time' = turnTimeHeuristic (time + 100) wait hist'
                rhist = hist ++ hist'
                rwaits = waits ++ [wait] in
                if time' < timeout
                then Just (ts, rhist, members', seed', time', rwaits)
                else Nothing) battles in
            foldr (\(ts, hist, members, seed, time, waits) m ->
                M.insertWith min (members, seed) (time, ts, hist, waits) m) m states) M.empty l
        (cstates, fstate) = foldr (\((members, seed), (time, ts, hist, waits)) (cs, fs) ->
            case ts of
            Continue -> ((hist, members, seed, time, waits) : cs, fs)
            Win ->
                let
                (members', hist', seed') = battleWin members yield rate hist seed
                v = (time, (Win, members', hist', seed'), waits) in
                case fwin v of
                Just v' -> (cs, selMin fs v')
                Nothing -> (cs, fs)
            Loss -> (cs, fs)) ([], f) (M.assocs m')
        timeout' =
            case (fstate, cstates) of
            (Nothing, []) -> timeout
            (Nothing, _) ->
                let
                msb = map (\(_, members, seed, time, _) ->
                    let
                    (t, _, _) = successfulFBattle members yield rate seed fbattle fwin in
                    (t + time)) cstates in
                min timeout (minimum msb)
            (Just (t, _, w), _) -> t in
        debug ("[TRACE] fastestFBattleB - cstates len: " ++ show (length cstates)) $
        loop cstates fstate timeout'
    allBattleTurn members seed = map (fbattle members seed)
    selMin f s =
        Just $ case f of
        Nothing -> s
        Just f' -> min f' s

performBattle :: [Member] -> Word8 -> Word8 -> Word16 -> [Int] -> (Int, ActionRet)
performBattle members yield rate seed waits =
    let (_, members', hist, seed') = battleBegin members seed
        time = turnTimeHeuristic 0 0 hist in
    loop hist members' seed' time waits
    where
    loop hist members seed time [] = (time, (Continue, members, hist, seed))
    loop hist members seed time (wait : waits) =
        let (ts, members', hist', seed') = battleTurn members seed wait
            -- TODO: compute real frames, this is an heuristic
            time' = turnTimeHeuristic (time + 100) wait hist'
            rhist = hist ++ hist' in
        case ts of
        Continue -> loop rhist members' seed' time' waits
        Loss -> (time', (ts, members', hist', seed'))
        Win ->
            let (members'', hist'', seed'') = battleWin members' yield rate rhist seed' in
            (time', (ts, members'', hist'', seed''))

getEncounters :: Word16 -> Word8 -> [(Word16, Bool)]
getEncounters seed threshold =
    let rngs = iterate nextRng seed
        rh rng = (rng, to8 (rng .>>. 8) < threshold) in
    map rh rngs

prettyStepAnalysis :: Word16 -> Word8 -> Int -> Int -> ([String], [Int])
prettyStepAnalysis seed threshold window steps =
    let (_, s, b) = foldl' (\(s, d, p) _ ->
          let field = getEncounters (nextRng s) threshold
              wind = take window field
              skip = any snd wind
              (n,ext) = fromJust $ uncons $ take 80 $ drop (if skip then 23 else 0) field in
          (fst n, d ++ [showHex16 (nextRng s) ++ ":[" ++ prettyMap wind ++ "]" ++ prettyMap (n:ext)], p ++ [skip]))
          (seed, [], []) [0..steps]
        p = map ((+1) . length) (split True b) in
    (s, p)
    where prettyMap = map (\(_, b) -> if b then '#' else '|')

encountersFromSeed :: Word16 -> Word8 -> [Word8] -> [(Word16, Maybe (Word8, Word16))]
encountersFromSeed seed threshold encTable =
    map (\s -> (s, encounter s)) (iterate nextRng seed)
    where
    encounter :: Word16 -> Maybe (Word8, Word16)
    encounter s =
        if to8 (s .>>. 8) < threshold then
            firstJust (\s ->
                let enc = (encTable !! fromIntegral (s .>>. 12)) in
                if enc /= 0x00 then Just (enc, s) else Nothing) (tail (iterate nextRng s))
        else Nothing

prettyEFS = map (\(s, e) -> showHex16 s ++ " - " ++ maybe "N/A" (\(e, s') -> "("
    ++ showHex8 e ++ ", " ++ showHex16 s' ++ ")") e)

flatEFS :: Word16 -> Word8 -> [Word8] -> [(Word16, (Word8, Word16))]
flatEFS seed threshold encTable =
    mapMaybe (\(s, o) -> o >>= (\k -> Just (s, k))) (encountersFromSeed seed threshold encTable)

prettyFlatEFS = map (\(s, (e, s')) -> showHex16 s ++ " - (" ++ showHex8 e ++ ", " ++ showHex16 s' ++ ")")

automateLevels :: [(Word8, Int)] -> [Member] -> Word16 -> (Int, Int) -> [Word8]
                  -> ([Member] -> Word16 -> Int -> Maybe (ActionRet, Int))
                  -> ((Int, ActionRet, [Int]) -> Maybe (Int, ActionRet, [Int]))
                  -> IO (Int, [Member], [(Word16, Int, [Int], [String])])
automateLevels desiredBattles team seed (encThrPointer, encLevel) encTable fbattle fwin = do
    enemyMap <- foldM (\m enc -> do v <- readEnemies (encFileMap M.! enc); return $ M.insert enc v m) M.empty (map fst desiredBattles)
    return $ loop desiredBattles team seed (encThrPointer, encLevel) enemyMap 0 []
    where
    loop [] team seed _ _ time encHist = (time, team, encHist)
    loop desiredBattles team seed (encThrPointer, encLevel) enemyMap time encHist =
        let encs = encountersFromSeed seed (encounterThresholds !! (encThrPointer + encLevel)) encTable
            ((arrivalSeed, Just (enc, seed')), frames) = head $ filter (encFindFunc desiredBattles) (zip encs [0..])
            (enemies, yield, rate) = enemyMap M.! enc
            (_, (_, members', h, seed''), w) = fastestFBattleB (team ++ enemies) yield rate seed' fbattle fwin
            time' = turnTimeHeuristic 0 0 h
            encLevel' = min (encLevel + 1) 2
            desiredBattles' = consumeBattles enc desiredBattles
            team' = take 4 members'
            encHist' = encHist ++ [(arrivalSeed, frames, w, h)] in
        loop desiredBattles' team' seed'' (encThrPointer, encLevel') enemyMap (time + time' + frames) encHist'
    encFindFunc desiredBattles ((_, enc), _) = (fst <$> enc) `elem` map (Just . fst) desiredBattles
    reduceBattles _ [] = []
    reduceBattles enc ((e, c) : desiredBattles) =
        (if enc == e then (e, c - 1) else (e, c)) : reduceBattles enc desiredBattles
    consumeBattles enc desiredBattles =
        filter ((/= 0) . snd) (reduceBattles enc desiredBattles)

fastestAutoLevels :: [(Word8, Int)] -> [Member] -> Word16 -> (Int, Int) -> [Word8]
                  -> ([Member] -> Word16 -> Int -> Maybe (ActionRet, Int))
                  -> ((Int, ActionRet, [Int]) -> Maybe (Int, ActionRet, [Int]))
                  -> IO (Int, [Member], [(Word16, Int, [Int], [String])])
fastestAutoLevels desiredBattles team seed (encThrPointer, encLevel) encTable fbattle fwin = do
    (timeout, orgM, orgH) <- automateLevels desiredBattles team seed (encThrPointer, encLevel) encTable fbattle fwin
    enemyMap <- foldM (\m enc -> do
        v <- readEnemies (encFileMap M.! enc)
        return (M.insert enc v m)) M.empty (map fst desiredBattles)
    case loop [(desiredBattles, team, seed, encLevel, 0, [])] Nothing enemyMap encThrPointer timeout of
        Just ret -> return ret
        Nothing -> return (timeout, orgM, orgH)
    where
    loop [] f _ _ _ = f
    loop l f enemyMap encThrPointer timeout =
        trace ("number of states: " ++ show (length l)) $
        let
        m = foldr (\(desiredBattles, team, seed, encLevel, time, encHist) m ->
            let
            encs = encountersFromSeed seed (encounterThresholds !! (encThrPointer + encLevel)) encTable
            selectEncs = filter (encFindFunc desiredBattles) (zip encs [0..(timeout - time)])
            states = mapMaybe (\((arrivalSeed, Just (enc, seed')), frames) ->
                let
                (enemies, yield, rate) = enemyMap M.! enc
                (_, (_, members', h, seed''), w) = fastestFBattleB (team ++ enemies) yield rate seed' fbattle fwin
                time' = turnTimeHeuristic 0 0 h
                time'' = time + time' + frames
                encLevel' = min (encLevel + 1) 2
                team' = take 4 members'
                encHist' = encHist ++ [(arrivalSeed, frames, w, h)]
                desiredBattles' = consumeBattles enc desiredBattles
                in
                if time'' < timeout
                then Just (time'', seed'', encLevel', team', encHist', desiredBattles')
                else Nothing) selectEncs in
            trace ("- number of battles in state(" ++ show time ++ "): " ++ show (length selectEncs)) $
            foldr (\(time, seed, encLevel, team, encHist, desiredBattles) m ->
                M.insertWith min (seed, desiredBattles) (time, encLevel, team, encHist) m) m states) M.empty l
        (cstates, fstate) = foldr (\((seed, desiredBattles), (time, encLevel, team, encHist)) (cs, fs) ->
            if null desiredBattles
            then (cs, selMin fs (time, team, encHist))
            else ((desiredBattles, team, seed, encLevel, time, encHist) : cs, fs)) ([], f) (M.assocs m)
        in
        loop cstates fstate enemyMap encThrPointer timeout
    encFindFunc desiredBattles ((_, enc), _) = (fst <$> enc) `elem` map (Just . fst) desiredBattles
    reduceBattles _ [] = []
    reduceBattles enc ((e, c) : desiredBattles) =
        (if enc == e then (e, c - 1) else (e, c)) : reduceBattles enc desiredBattles
    consumeBattles enc desiredBattles = filter ((/= 0) . snd) (reduceBattles enc desiredBattles)
    selMin f s =
        Just $ case f of
        Nothing -> s
        Just f' -> min f' s

prettyAutoHist :: [(Word16, Int, [Int], [String])] -> [String]
prettyAutoHist [] = []
prettyAutoHist ((seed, frames, waits, hist) : h) =
    let pressWindow = takeWhile (\s -> (s .>>. 8) >= 0x06) (drop 1 (iterate (skipRng 65535) seed))
        ppw = "[" ++ intercalate ", " (map showHex16 pressWindow) ++ "]" in
    [ "enc. infos: " ++ showHex16 seed ++ ", " ++ show frames ++ ", " ++ show waits
    --, "ppw: " ++ ppw
    , "len. ppw: " ++ show (length pressWindow)
    , "h:" ]
    ++ hist
    ++ prettyAutoHist h

-- #1 house manips
-- main = do
--     team <- readTeam "team_house.txt"
--     enemies <- readTeam "house/lamp.txt"
--     let (time, (_, members', h, _), w) = fastestBattleB' (team ++ enemies) 0x00 0x00 0x40b3
--         team' = take 4 members'
--     putStrLn ("time: " ++ show time)
--     putStrLn ("w: " ++ show w)
--     putStrLn "h:"
--     mapM_ putStrLn h
--     enemies <- readTeam "house/doll.txt"
--     let (time, (_, _, h, _), w) = fastestBattleB' (team' ++ enemies) 0x00 0x00 0x838e
--     putStrLn ("time: " ++ show time)
--     putStrLn ("w: " ++ show w)
--     putStrLn "h:"
--     mapM_ putStrLn h

-- #2 starman jr1
-- main = do
--     team <- readTeam "team_zoo.txt"
--     enemies <- readTeam "zoo/starman.txt"
--     let (t, (_, members', h, _), w) = fastestBattleB (team ++ enemies) 0x00 0x00 0xf6f0
--         team' = take 4 members'
--     putStrLn ("t: " ++ show t)
--     putStrLn ("w: " ++ show w)
--     putStrLn "h:"
--     mapM_ putStrLn h

-- #3 starman jr2
-- main = do
--     team <- readTeam "team_zoo.txt"
--     enemies <- readTeam "zoo/starman.txt"
--     let (_, (_, members', h, _), w) = fastestBattle (team ++ enemies) 0x00 0x00 0x12da
--         team' = take 4 members'
--     putStrLn ("w: " ++ show w)
--     putStrLn "h:"
--     mapM_ putStrLn h

-- #4 stray dog front of duncan
-- main = do
--     team <- readTeam "team_duncan.txt"
--     enemies <- readTeam "duncan/enc07.txt"
--     let l = map (successfulBattle (team ++ enemies) 0x00 0x00) (take 240 $ iterate nextRng 0x16dc)
--     mapM_ (\(_, (_, _, _, s), w) -> putStrLn ("s: " ++ showHex16 s ++ ", w: " ++ show w)) l

-- #5 mt itoi battle search
-- main = do
--     encTable <- readEncounterTable "maps/map35.txt"
--     team <- readTeam "team_itoi.txt"
--     let fbattle members seed i =
--             case battleTurn members seed i of
--             (Loss, _, _, _) -> Nothing
--             ret ->
--                 let (_, ninten : _, _, _) = ret in
--                 if alive ninten
--                 then Just (ret, i)
--                 else Nothing
--         fwin (time, (ts, members, hist, seed), waits) =
--             let items = concatMap bag members
--                 starMiner = members !! 4 in
--             if (0x00 `elem` items || 0x24 `elem` items) &&
--                (name starMiner /= "Star Miner" || (name starMiner == "Star Miner" && 0x24 `elem` items))
--             then Just (time, (ts, members, hist, seed), waits)
--             else Nothing
--     (time, team, hist) <- fastestAutoLevels [(0x92, 1), (0x9a, 10)] team 0xcaa2 (5, 0) encTable fbattle fwin
--     putStrLn ("time: " ++ show time)
--     mapM_ (putStrLn . showMember) team
--     mapM_ putStrLn (prettyAutoHist hist)

main = do
    let seed = 0xc45a
        threshold = 0x0d
        window = 8
    let (s, p) = prettyStepAnalysis seed threshold window 500
    mapM_ putStrLn s
    print p
    -- let t = flatEFS seed threshold [ 0x47, 0x5a, 0x5b, 0x5c, 0x5d, 0x61, 0x62, 0x63
    --                                , 0x64, 0x66, 0x69, 0x00, 0x00, 0x00, 0x00, 0x00 ]
    -- mapM_ putStrLn (take 5 $ prettyFlatEFS t)
