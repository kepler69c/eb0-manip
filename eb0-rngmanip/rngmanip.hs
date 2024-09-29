import Data.Word
import Data.Bits
import Data.List
import Text.Printf
import Data.Maybe
import Control.Monad.Cont
import Control.Monad.Identity
import Data.Map (Map, (!), fromList)
import Control.Monad.Cont.Class
import Control.Monad (when)

-- types
data Range = All | Self | One deriving Show

data TurnState = Win | Continue | Loss deriving (Show, Eq)

data Member = Member
    { name ::       String
    , hp ::         Word16
    , pp ::         Word16
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
    , resistence :: Word8
    , buff ::       Word8
    , attackList :: [Word8]
    } deriving (Show, Eq)

-- constants
-- note: 0x92 is bomber, 0x9a is 4 starmen
encounterTable :: [Word8]
encounterTable = 
    [ 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99,
      0x9a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]

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

emptyMember = Member {
    name =       "", 
    hp =         0x0000, 
    pp =         0x0000,
    attack =     0x0000,
    defense =    0x0000,
    fight =      0x00,
    speed =      0x00,
    wisdom =     0x00,
    strength =   0x00,
    force =      0x00, 
    attackSel =  0x00,
    attackTarg = 0,
    status =     0x00,
    statusMask = 0x00,
    resistence = 0x00,
    buff =       0x00,
    attackList = []
}

battleMembers = [
        Member {
            name =       "A",
            hp =         0x0020,
            pp =         0x000b,
            attack =     0x0012,
            defense =    0x0009,
            fight =      0x0b,
            speed =      0x09,
            wisdom =     0x09,
            strength =   0x0a,
            force =      0x0b,
            attackSel =  0x01,
            attackTarg = 0,
            status =     0x00,
            statusMask = 0xff,
            resistence = 0x00,
            buff =       0x00,
            attackList = []
        },
        Member {
            name =       "B",
            hp =         0x0000,
            pp =         0x0000,
            attack =     0x0004,
            defense =    0x0002,
            fight =      0x04,
            speed =      0x02,
            wisdom =     0x08,
            strength =   0x04,
            force =      0x03,
            attackSel =  0x01,
            attackTarg = 0,
            status =     0x00,
            statusMask = 0xff,
            resistence = 0x00,
            buff =       0x00,
            attackList = []
        },
        Member {
            name =       "EVE",
            hp =         0x03e7,
            pp =         0x0000,
            attack =     0x03e7,
            defense =    0x03e7,
            fight =      0xff,
            speed =      0xff,
            wisdom =     0xff,
            strength =   0xff,
            force =      0xff,
            attackSel =  0x01,
            attackTarg = 0,
            status =     0x00,
            statusMask = 0xff,
            resistence = 0x00,
            buff =       0x00,
            attackList = []
        },
        emptyMember,
        Member {
            name =       "LastStarmanA",
            hp =         0x0078,
            pp =         0x0064,
            attack =     0x005a,
            defense =    0x008c,
            fight =      0x46,
            speed =      0x3c,
            wisdom =     0x3c,
            strength =   0x50,
            force =      0x32,
            attackSel =  0x01,
            attackTarg = 0,
            status =     0x00,
            statusMask = 0x9b,
            resistence = 0x00,
            buff =       0x00,
            attackList = [
                0x13, 0x13, 0x38, 0x15,
                0x01, 0x01, 0x53, 0x53
            ]
        },
        Member {
            name =       "LastStarmanB",
            hp =         0x0078,
            pp =         0x0064,
            attack =     0x005a,
            defense =    0x008c,
            fight =      0x46,
            speed =      0x3c,
            wisdom =     0x3c,
            strength =   0x50,
            force =      0x32,
            attackSel =  0x01,
            attackTarg = 0,
            status =     0x00,
            statusMask = 0x9b,
            resistence = 0x00,
            buff =       0x00,
            attackList = [
                0x13, 0x13, 0x38, 0x15,
                0x01, 0x01, 0x53, 0x53
            ]
        },
        Member {
            name =       "LastStarmanC",
            hp =         0x0078,
            pp =         0x0064,
            attack =     0x005a,
            defense =    0x008c,
            fight =      0x46,
            speed =      0x3c,
            wisdom =     0x3c,
            strength =   0x50,
            force =      0x32,
            attackSel =  0x01,
            attackTarg = 0,
            status =     0x00,
            statusMask = 0x9b,
            resistence = 0x00,
            buff =       0x00,
            attackList = [
                0x13, 0x13, 0x38, 0x15,
                0x01, 0x01, 0x53, 0x53
            ]
        },
        Member {
            name =       "LastStarmanD",
            hp =         0x0078,
            pp =         0x0064,
            attack =     0x005a,
            defense =    0x008c,
            fight =      0x46,
            speed =      0x3c,
            wisdom =     0x3c,
            strength =   0x50,
            force =      0x32,
            attackSel =  0x01,
            attackTarg = 0,
            status =     0x00,
            statusMask = 0x9b,
            resistence = 0x00,
            buff =       0x00,
            attackList = [
                0x13, 0x13, 0x38, 0x15,
                0x01, 0x01, 0x53, 0x53
            ]
        }
    ]

-- global utilities
to8 :: (Integral a) => a -> Word8
to8 = fromIntegral

to16 :: (Integral a) => a -> Word16
to16 = fromIntegral

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

-- show functions
showHex16 :: Word16 -> String
showHex16 = printf "0x%04x"

showHex8 :: Word8 -> String
showHex8 = printf "0x%02xto16 "

prettyWindow :: [(Word16, Bool)] -> String
prettyWindow w = "[" ++ intercalate ", " (map (\(s, h) ->
            showHex16 s ++ if h then "!" else "") w) ++ "]"

prettyPresses :: [(Int, [(Word16, Bool)])] -> String
prettyPresses info =
    unlines $ map (\(s, w) ->
        "steps: " ++ show s ++ ", window: " ++ prettyWindow w) info

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
alive m = (status m .&. 0x80) == 0x00

valueRoll :: Word16 -> Word16 -> (Word16, Word16)
valueRoll a seed =
    let seed' = nextRng seed
        b = (a .&. 0xfcff) .<<. 6
        c = (b `div` 100) * to16 (fightValMap !! fromIntegral (seed' .>>. 9))
        -- depending on rng LSB, neg/pos value
        d = (if (seed' .&. 0x100) /= 0x00 then b - c else b + c) .>>. 6 in
    (d, seed')

battlePriority :: [Member] -> Word16 -> (Int, Word16)
battlePriority members seed =
    loop (0, 0, 0, seed) members where 
    loop (maxSpeed, maxTarget, i, seed') [] = (maxTarget, seed')
    loop (maxSpeed, maxTarget, i, seed') (m : members) =
        case attackSel m of
        -- continue
        0xff -> loop (maxSpeed, maxTarget, i + 1, seed') members
        -- break
        0x5e -> (maxTarget, seed')
        -- find max
        _ -> let (s, seed'') = valueRoll (0x00ff .&. to16 (speed m)) seed' in
             loop (if s >= maxSpeed
                   then (s, i, i + 1, seed'')
                   else (maxSpeed, maxTarget, i + 1, seed'')) members

attackSelect :: [Member] -> Int -> Word16 -> (Word8, [Member], Word16)
attackSelect members from seed =
    if from >= 4
    then
        let seed' = nextRng seed
            m = members !! from
            attack = attackList m !! (fromIntegral (seed' .>>. 8) .&. 0b111) in
        if attack == 0x53 || attack == 0x59
        then
            let m' = m { buff = buff m .|. 0x08 } in
            (attack, insertAt members from m', seed')
        else (attack, members, seed')
    -- always default attack during manips
    else (0x01, members, seed)

selectOpponent :: [Member] -> Int -> Word16 -> (Int, Word16)
selectOpponent members from seed =
    if from < 4 then (4, seed)
    else loop seed where
    loop seed =
            let seed' = nextRng seed
                sel = fromIntegral (seed' .>>. 5) in
            if alive (members !! sel) && sel < 4 then (sel, seed')
            else loop seed'

getAttackRange :: Word8 -> Range
getAttackRange attackId =
    let x = fromIntegral (attackId .&. 0x03) + 0x01
        a = fst $ foldr ($) (0x01, True) (replicate x (ror . ror))
        b = a .&. (attackRangeMap !! fromIntegral (attackId .>>. 2))
        r = loop a b in 
    case r of
    0 -> All
    1 -> Self
    _ -> One
    where
    ror (a, c) = ((a .>>. 1) + (if c then 0x80 else 0x00), (a .&. 0x01) /= 0x00)
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
    | hp (head members) == 0x0000 = Loss
    | all (== 0x0000) (take 4 $ map hp members) = Loss
    | all (== 0x0000) (drop 4 $ map hp members) = Win
    | otherwise = Continue

applyDamage :: Word16 -> [Member] -> Int -> Int -> Word16 -> (TurnState, [Member], [String], Word16)
applyDamage dmg members from to s =
    let mFrom = members !! from
        mTo = members !! to
        (dmg', s') = valueRoll dmg s
        dmg'' = if (buff mTo .&. 0x10) /= 0x00 then dmg' `div` 2 else dmg'
        dmg''' = if (buff mTo .&. 0x08) /= 0x00 then dmg'' `div` 2 else dmg''
        dmg'''' = if dmg''' == 0x0000 then 0x0001 else dmg'''
        (members', history, s'') = 
            if hp mTo < dmg''
            then (insertAt members to (mTo { hp = 0x0000, status = status mTo .|. 0x80 }),
                  [printf "%s was beaten!" (name mTo)], s')
            else 
                let mTo' = mTo { hp = hp mTo - dmg''' }
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
    (battleEnd members', members', printf "%s suffered damage of %d." (name mTo) dmg''' : history, s'')

type ActionRet = (TurnState, [Member], [String], Word16)

performStatus :: (Monad m) => [Member] -> Int -> Word16 ->
                 (ActionRet -> m ActionRet) ->
                 m ActionRet
performStatus m from s exit
    -- dead
    | (status (m !! from) .&. 0x80) /= 0x00 = exit (Continue, m, [], s)
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

performConfusion :: (Monad m) => [Member] -> Int -> Word16 -> m (Int, Word16)
performConfusion m from s =
    let to = attackTarg (m !! from) in
    if (status (m !! from) .&. 0x08) /= 0x00 then do
        let loop s =
                let s' = nextRng s
                    sel = fromIntegral (s' .>>. 5) in
                if alive (m !! sel) then (sel, s)
                else loop s
        (sel, s) <- return $ loop s
        return (sel, s)
    else return (to, s)

performNoTarget :: (Monad m) => [Member] -> Int -> [String] -> Word16 ->
                   (ActionRet -> m ()) ->
                   m ()
performNoTarget m from h s exit =
    let to = attackTarg (m !! from) in
    when (statusMask (m !! from) == 0x00 || statusMask (m !! to) == 0x00 ||
          not (alive (m !! from)) || not (alive (m !! to)))
        (exit (Continue, m, h ++ [printf "%s was already gone." $ name (m !! to)], s))

performCrit :: (Monad m) => [Member] -> Int -> [String] -> Word16 ->
               m (Word16, [String], Maybe Word16)
performCrit m from h s = do
    let to = attackTarg (m !! from)
    (crit, s) <- return $ cdRate m from to s
    if crit then do
        h <- return $ h ++ ["SMAAAASH!!"]
        (dmg, s) <- return $ critDamage m from s
        return (s, h, Just dmg)
    else return (s, h, Nothing)

-- TODO: DAG of attack behavior (CPS)
performAttack :: [Member] -> Int -> Word16 -> ActionRet
performAttack m from s =
    let to = attackTarg (m !! from) in
    runCont (callCC $ \exit -> callCC $ \exit2 -> do
    (ts, m, h, s) <- performStatus m from s exit2
    case attackSel (m !! from) of
        -- PK Beam a
        0x12 -> do
            let h = [printf "%s tried PK Beam a!" $ name (m !! from)]
                cpp = pp (m !! from)
                dmg = 0x1e
            when (cpp < 0x04) (exit (Continue, m, h ++ ["Not enough PP!"], s))
            m <- return $ insertAt m from (m !! from) { pp = cpp - 0x04 }
            dmg <- return $ if (resistence (m !! to) .&. 0x02) /= 0x00
                then if (dmg `div` 2) == 0x0000 then 0x0001 else dmg `div` 2
                else dmg
            (ts, m, h', s) <- return $ applyDamage dmg m from to s
            return (ts, m, h ++ h', s)
        -- PK Beam b
        0x13 -> do
            let h = [printf "%s tried PK Beam b!" $ name (m !! from)]
                cpp = pp (m !! from)
                dmg = 0x50
            when (cpp < 0x07) (exit (Continue, m, h ++ ["Not enough PP!"], s))
            m <- return $ insertAt m from (m !! from) { pp = cpp - 0x07 }
            dmg <- return $ if (resistence (m !! to) .&. 0x02) /= 0x00
                then if (dmg `div` 2) == 0x0000 then 1 else dmg `div` 2
                else dmg
            (ts, m, h', s) <- return $ applyDamage dmg m from to s
            return (ts, m, h ++ h', s)
        -- PK Beam r
        0x15 -> do
            let h = [printf "%s tried PK Beam r!" $ name (m !! from)]
                cpp = pp (m !! from)
            when (cpp < 0x0c) (exit (Continue, m, h ++ ["Not enough PP!"], s))
            m <- return $ insertAt m from (m !! from) { pp = cpp - 0x0c }
            h <- return $ h ++ [printf "But, %s's FranklinBadge bounced back the Beam!" $ name (m !! to)]
                            ++ [printf "There was no effect on %s." $ name (m !! to)]
            return (Continue, m, h, s)
        -- PSIShield a (writes 0x10 to ally's shield? data - reduces PK attacks)
        0x38 -> do
            let h = [printf "%s tried PSIShield a!" $ name (m !! from)]
                cpp = pp (m !! from)
            when (cpp < 0x04) (exit (Continue, m, h ++ ["Not enough PP!"], s))
            m <- return $ insertAt m from (m !! from) { pp = cpp - 0x04 }
            h <- return $ h ++ [printf "%s was shielded." $ name (m !! to)]
            m <- return $ insertAt m to (m !! to) { buff = buff (m !! to) .|. 0x10 }
            return (Continue, m, h, s)
        -- ready for an attack
        0x53 -> return (Continue, m, [printf "%s is ready for an attack." $ name (m !! from)], s)
        e -> error $ printf "performAttack: unsupported attack \"0x%02hhx\"." e) id

-- actions framework
data ActionMem = ActionMem
    { afDamage :: Maybe Word16
    , afPSIDamage :: Maybe Word16
    , afPP :: Maybe Word16 }

type ActionCont = (TurnState, [Member], [String], Word16, ActionMem)

type ActionFun = ActionCont -> Int -> (ActionRet -> ContT ActionRet Identity (Bool, ActionCont)) -> ContT ActionRet Identity (Bool, ActionCont)
returnY x = return (True, x)
returnN x = return (False, x)

type ActionMap = Map Int ActionFun

data ActionDecNode = Nil | Next Int | NextYN (Int, Int)
type ActionDec = Map Int ActionDecNode

type ActionGraph = (ActionMap, ActionDec, ActionMem)

runAction :: ActionRet -> Int -> ActionGraph -> ActionRet
runAction (ts, m, h, s) fr (amap, adec, afm) =
    let initCont = (ts, m, h, s, afm) in
    runCont (callCC $ \exit -> do
        (loop, (cont, i)) <- label (initCont, 0)
        (b, cont) <- (amap ! i) cont fr exit
        (case (adec ! i, b) of
         (Next j, _) -> loop (cont, j)
         (NextYN (j, _), True) -> loop (cont, j)
         (NextYN (_, j), False) -> loop (cont, j)
         _ -> undefined)) id

afConfusion :: ActionFun
afConfusion (ts, m, h, s, am) fr exit =
    let to = attackTarg (m !! fr) in
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

-- deprecated: keep for history reasons
afStatusRng :: ActionFun
afStatusRng (ts, m, h, s, am) fr exit =
    returnY (ts, m, h, if status (m !! fr) .&. 0x80 /= 0x00 then nextRng s else s, am)

afAttacksText :: ActionFun
afAttacksText (ts, m, h, s, am) fr exit =
    returnY (ts, m, h ++ [printf "%s's attack!" $ name (m !! fr)], s, am)

afNoTarget :: ActionFun
afNoTarget (ts, m, h, s, am) fr exit =
    let to = attackTarg (m !! fr) in
    if statusMask (m !! fr) == 0x00 || not (alive (m !! fr)) ||
       statusMask (m !! to) == 0x00 || not (alive (m !! to))
    then exit (Continue, m, h ++ [printf "%s was already gone." $ name (m !! to)], s)
    else returnY (ts, m, h, s, am)

afCrit :: ActionFun
afCrit (ts, m, h, s, am) fr exit = do
    let to = attackTarg (m !! fr)
    (crit, s) <- return $ cdRate m fr to s
    if crit then do
        let oldDmg = fromMaybe 0x0000 (afDamage am)
        h <- return $ h ++ ["SMAAAASH!!"]
        (dmg, s) <- return $ critDamage m fr s
        am <- return $ am { afDamage = Just (oldDmg + dmg) }
        returnY (ts, m, h, s, am)
    else returnN (ts, m, h, s, am)

afDodge :: ActionFun
afDodge (ts, m, h, s, am) fr exit = do
    let to = attackTarg (m !! fr)
    if status (m !! to) .&. 0x70 /= 0x00 || buff (m !! to) .&. 0x80 /= 0x00
    then returnN (ts, m, h, s, am)
    else do
      (dodge, s) <- return $ cdRate m to fr s
      if not dodge then returnN (ts, m, h, s, am)
      else returnY (ts, m, h ++ [printf "%s dodged swiftly." $ name (m !! to)], s, am)

afDamageCompute :: ActionFun
afDamageCompute (ts, m, h, s, am) fr exit = do
    let to = attackTarg (m !! fr)
        oldDmg = fromMaybe 0x0000 (afDamage am)
        dmg = attackDamage m fr to
    am <- return $ am { afDamage = Just (oldDmg + dmg) }
    returnY (ts, m, h, s, am)

afApplyDamage :: ActionFun
afApplyDamage (ts, m, h, s, am) fr exit = do
    let to = attackTarg (m !! fr)
        dmg = fromMaybe undefined (afDamage am)
    (ts, m, h) <- if (buff (m !! to) .&. 0x04) /= 0x00 then do
        (ts, m, h', s) <- return $ applyDamage dmg m to fr s
        h <- return $ h ++ [printf "%s bounced back the attack!" $ name (m !! to)] ++ h'
        return (ts, m, h)
    else do
        (ts, m, h', s) <- return $ applyDamage dmg m fr to s
        h <- return $ h ++ h'
        return (ts, m, h)
    (case ts of
     Continue -> returnY (ts, m, h, s, am)
     _ -> exit (ts, m, h, s))

afContinuous :: ActionFun
afContinuous (ts, m, h, s, am) _ exit =
    returnY (ts, m, h ++ ["Continuous attack!"], s, am)

afExit :: ActionFun
afExit (ts, m, h, s, _) _ exit = do
    if all (\m -> statusMask m == 0x00 || status m .&. 0xe0 /= 0x00) m then
        exit (Loss, m, h ++ ["You lost the battle."], s)
    else exit (ts, m, h, s)

afBitText :: ActionFun
afBitText (ts, m, h, s, am) fr exit =
    let to = attackTarg (m !! fr) in
    returnY (ts, m, h ++ [printf "%s bit %s!" (name (m !! fr)) (name (m !! to))], s, am)

afCheckPP :: ActionFun
afCheckPP (ts, m, h, s, am) fr exit =
    let mpp = fromMaybe undefined (afPP am) in
    if buff (m !! fr) .&. 0x40 /= 0 then
        returnN (ts, m, h ++ ["But, the PSI was blocked."], s, am)
    else if pp (m !! fr) < mpp then
        returnN (ts, m, h ++ ["Not enough PP's."], s, am)
    else do
        m <- return $ insertAt m fr (m !! fr) { pp = pp (m !! fr) - mpp }
        returnY (ts, m, h, s, am)

afPSIDamageCompute :: ActionFun
afPSIDamageCompute (ts, m, h, s, am) fr exit =
    returnY (ts, m, h, s, am { afDamage = Just $ fromMaybe undefined $ afPSIDamage am })

afPSIaText :: ActionFun
afPSIaText (ts, m, h, s, am) fr exit =
    returnY (ts, m, h ++ [printf "%s tried PK Beam a!" (name (m !! fr))], s, am)

-- attacks constants
map01 = fromList [
    (0, afConfusion),
    (1, afAttacksText),
    (2, afNoTarget),
    (3, afCrit),
    (4, afDodge),
    (5, afDamageCompute),
    (6, afApplyDamage),
    (7, afExit)]
dec01 = fromList [
    (0, Next 1),
    (1, Next 2),
    (2, Next 3),
    (3, NextYN (6, 4)),
    (4, NextYN (7, 5)),
    (5, Next 6),
    (6, Next 7),
    (7, Nil)]
afm01 = ActionMem
    { afDamage = Nothing
    , afPSIDamage = Nothing
    , afPP = Nothing }

map02 = fromList [
    (0, afConfusion),
    (1, afAttacksText),
    (2, afNoTarget),
    (3, afDodge),
    (4, afDamageCompute),
    (5, afApplyDamage),
    (6, afContinuous),
    (7, afDodge),
    (8, afDamageCompute),
    (9, afApplyDamage),
    (10, afExit)]
dec02 = fromList [
    (0, Next 1),
    (1, Next 2),
    (2, Next 3),
    (3, NextYN (6, 4)),
    (4, Next 5),
    (5, Next 6),
    (6, Next 7),
    (7, NextYN (10, 8)),
    (8, Next 9),
    (9, Next 10),
    (10, Nil)]
afm02 = ActionMem
    { afDamage = Nothing
    , afPSIDamage = Nothing
    , afPP = Nothing }

map03 = fromList [
    (0, afConfusion),
    (1, afBitText),
    (2, afDamageCompute),
    (3, afNoTarget),
    (4, afDodge),
    (5, afApplyDamage),
    (6, afExit)]
dec03 = fromList [
    (0, Next 1),
    (1, Next 2),
    (2, Next 3),
    (3, Next 4),
    (4, NextYN (6, 5)),
    (5, Next 6),
    (6, Nil)]
afm03 = ActionMem
    { afDamage = Nothing
    , afPSIDamage = Nothing
    , afPP = Nothing }

map12 = fromList [
    (0, afPSIaText),
    (1, afCheckPP),
    (2, afPSIDamageCompute),
    (3, afConfusion),
    (4, afNoTarget),
    (5, afApplyDamage),
    (6, afExit)]
dec12 = fromList [
    (0, Next 1),
    (1, NextYN (6, 2)),
    (2, Next 3),
    (3, Next 4),
    (4, Next 5),
    (5, Next 6),
    (6, Nil)]
afm12 = ActionMem
    { afDamage = Nothing
    , afPSIDamage = Just 0x1e
    , afPP = Just 0x04 }

performAttack2 :: [Member] -> Int -> Word16 -> ActionRet
performAttack2 m from s =
    let to = attackTarg (m !! from) in
    runCont (callCC $ \exit -> callCC $ \exit2 -> do
    (ts, m, h, s) <- performStatus m from s exit2
    (case attackSel (m !! from) of
     0x01 -> return $ runAction (Continue, m, [], s) from (map01, dec01, afm01)
     0x02 -> return $ runAction (Continue, m, [], s) from (map02, dec02, afm02)
     0x03 -> return $ runAction (Continue, m, [], s) from (map03, dec03, afm03)
     0x12 -> return $ runAction (Continue, m, [], s) from (map12, dec12, afm12)
     e -> error $ printf "performAttack: unsupported attack \"0x%02hhx\"." e)) id

battleTurn :: [Member] -> Word16 -> ActionRet
battleTurn members seed =
    let (members', seed') = foldl' (\(members, seed) (m, i) ->
            let m1 = m  { attackSel = 0x00, buff = buff m .&. 0xf7 } in
            if statusMask m1 == 0x00 || status m1 .&. 0xf4 /= 0x00 || buff m1 .&. 0x20 /= 0x00
            then (insertAt members i m1, seed) else
            let (id, members', seed1) = attackSelect (insertAt members i m1) i seed
                m2 = (members' !! i) { attackSel = id }
                members'' = insertAt members' i m2 in
            case getAttackRange id of
            One ->
                let (j, seed2) = selectOpponent members'' i seed1
                    m3 = m2 { attackTarg = j } in
                (insertAt members'' i m3, seed2)
            _ ->
                let m3 = m2 { attackTarg = i } in
                (insertAt members'' i m3, seed1)) (members, seed) (zip members [0..]) in
    runUntilStop $ 
    scanl (\(ts, members, history, seed) _ ->
        let (caster, seed1) = battlePriority members seed
            (ts, members', history', seed2) = performAttack members caster seed1
            members'' = case ts of 
                        Continue -> 
                            let m = members' !! caster in
                            insertAt members' caster (m { attackSel = 0xff })
                        _ -> members' in
        (ts, members'', history ++ history', seed2)) (Continue, members', [], seed') members where
    runUntilStop scan =
        case find (\(st, _, _, _) -> st /= Continue) scan of
        Just x -> x
        Nothing -> last scan

nbAlive :: [Member] -> Int
nbAlive = length . filter alive

getEncounters :: Word16 -> Word8 -> [(Word16, Bool)]
getEncounters seed threshold =
    let rngs = iterate nextRng seed 
        rh rng = (rng, to8 (rng .>>. 8) < threshold) in
    map rh rngs
 
getPresses :: Word16 -> Word8 -> Int -> [(Int, [(Word16, Bool)])]
getPresses seed threshold w =
    let rngs = drop 1 $ iterate nextRng seed 
        windows = map (\x -> take w $ getEncounters x threshold) rngs
        analyze_win = map (\x -> (x, any snd x)) windows
        applyPress ls =
            let (l, r) = break snd ls in
            l ++ [head r] ++ applyPress (drop 24 r)
        applied = applyPress analyze_win 
        splits = splitWhen snd applied
        keep = map fst $ filter snd applied in 
    zip (map (\x -> 1 + length x) splits) keep

getEnemy :: Word16 -> [Word8] -> (Word16, Word8)
getEnemy seed encounterTable =
    let rngs = iterate nextRng seed in
    head $ dropWhile (\(_, x) -> x == 0x00) $ map (\x ->
        (x, encounterTable !! fromIntegral (to8 (x .>>. 12)))) rngs

main = do
    let (i, s1) = selectOpponent battleMembers 0 0xec64
        m = insertAt battleMembers 0 (head battleMembers) { attackTarg = i }
        (_, _, history, _) = performAttack2 m 0 s1
    putStrLn $ intercalate "\n" history
