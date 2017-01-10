{-# LANGUAGE TemplateHaskell, RecordWildCards, RankNTypes, TupleSections #-}

module Combat where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import System.Random
import Data.Monoid
import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq

data ShipClass = SC { _combat, _science, _hull, _shields, _presence, _defense :: Int, _class_name :: String } deriving Show
makeLenses ''ShipClass

data Status = Destroyed | Disabled | Functional (Sum Double) deriving Show
makePrisms ''Status

data Ship = Ship {  _hp, _sh :: Int, _status :: Status, _sclass :: ShipClass, _ship_name :: Maybe String } deriving Show
makeLenses ''Ship

effCombat :: Ship -> Sum Double
effCombat  sh  =  (Sum . fromIntegral $ sh ^. sclass.combat) * (sh ^. status . _Functional)

deleteAt :: Int -> Seq a -> Seq a
deleteAt i seq = Seq.take i seq <> Seq.drop (i+1) seq

data Fleet = Fleet { _functional, _nonFunctional :: Seq Ship, _fleet_name :: String, _fleetMul :: Double} deriving Show
makeLenses ''Fleet

data Battle = Battle {_attacker, _defender :: Fleet } deriving Show
makeLenses ''Battle

data Result = Win Battle | Loss Battle deriving Show

isWin (Win _) = True
isWin _ = False

fleetCombat :: Fleet -> Double
fleetCombat fl = ((fl ^. fleetMul) * (getSum . sum $ fl ^.. functional.traversed. to effCombat))**1.15

fight :: StateT Battle IO ()
fight = do
        v <- lift $ randomRIO (0:: Double, 1) 
        at <- use $ attacker . to fleetCombat
        def <- use $ defender . to fleetCombat
        gen <- lift $ getStdGen 
        if v <= at / (at + def) 
                then defender %= hit gen                
                else {- lift (putStrLn "attacker hit") >> -} attacker %= hit gen

        
hit :: StdGen -> Fleet -> Fleet
hit gen fl | len <= 0 = nonFunctional . traversed . status .~ Destroyed $ fl
           | otherwise = 
              let (i, gen') = randomR (0, len -1) gen
                  ship = hitShip gen' $ fl^..functional.ix i
              in if (ship ^. status. _Functional) > 0 
                 then functional.ix i .~ ship $ fl
                 else (functional %~ deleteAt i) .  (nonFunctional %~ ( ship Seq.<|)) $ fl 
        where len = length $ fl ^. functional
hitShip :: StdGen -> [Ship] -> Ship                                          
hitShip gen [ship] | ship ^. sh > 0 = sh -~ 1 $ ship
                   | ship ^. hp > 1 = let (hp', ship') = hp <<-~ 1 $ ship
                                          eff = (/4) . fromInt . ceiling $ (fromInt $ hp' *4) / (fromInt $ ship ^. sclass . hull)
                                      in status .~ (Functional . Sum $ eff) $ ship'     
                   | otherwise = status .~ Disabled $ (hp .~ 0 $ ship)             

                   
fightUntil :: (Fleet -> Bool) -> Fleet -> Fleet -> IO Result
fightUntil tac att def = do
        battle <- execStateT loop $ Battle att def
        if tac (battle ^. attacker) then return $ Loss battle else  return $ Win battle

       -- until ((||) <$> tac . fst <*> tac . snd) (uncurry fight) (att, def)
                
    where 
          loop = do
           battle <- get
           if tac (battle ^. attacker) || tac (battle ^. defender)
                then return battle
                else fight >> loop
        
dead :: Fleet -> Bool
dead  = null . _functional

entr = SC 9 9 7 9 10 6 "Enterprise"
ent = SC 9 9 7 8 10 6 "Enterprise"
courageous = SC 8 9 6 7 8 6 "Courageous"
sarek = SC 8 9 7 7 8 6 "Sarek"
miracht = SC 7 7 5 6 7 6 "Miracht"
cenR = SC 3 3 2 3 3 3 "Centaur-A"
cen = SC 3 2 2 2 2 2 "Centaur"
ex = SC 6 5 4 5 5 6 "Excelsior"
ellR = SC 4 3 2 2 2 4 "Constellation-A"
ell = SC 3 2 2 2 2 3 "Constellation"
titR = SC 5 3 3 4 3 5 "Constitution-B"
connie = SC 5 4 4 4 3 5 "Constitution"
challorn = SC 4 3 3 3 3 3 "Constellation"
blEx = SC 7 6 5 6 6 6 "Excelsior"

cheron = SC 6 5 5 5 6 5 "Cheron"
jaldun = SC 4 3 4 4 3 4 "Jaldun"
mir = SC 3 1 1 2 1 2 "Miranda"
riala = SC 6 4 5 7 5 6 "Riala"
peace = SC 3 3 3 3 3 5 "peace"
rennie = SC 5 3 4 5 4 5 "Renaissance"
takaaki = SC 3 2 3 3 1 2 "takaaki"
cTak = SC 4 2 3 3 1 2 "takaaki"
escort = SC 4 2 3 4 3 4 "escort"
kaldar = SC 5 3 4 4 3 5 "Kaldar"
lorgot = SC 7 4 3 5 4 6 ""
ambassador = SC 7 7 5 8 7 7 "ambassador"
souvie = SC 12 8 9 12 8 10 "souvereign"
def = SC 10 1 3 6 4 4 "defiant"
inter = SC 6 10 4 4 6 5 "Intrepid"

fl1 = Fleet (fmap makeShip . Seq.replicate 3 $ ellR) Seq.empty "Ellies"

groupBC = [(3::Int, jaldun), (1, takaaki), (2, cTak), (2, kaldar)]
cd = makeShip jaldun

singleJaldun = makeFleet 1 [(1, jaldun)]

makeFleet :: Double -> [(Int, ShipClass)] -> Fleet
makeFleet mul shs= Fleet (Seq.fromList . concatMap (\ (i, s) -> replicate i $ makeShip s) $ shs) Seq.empty "" mul

makeShip :: ShipClass -> Ship
makeShip sc@SC{..} = Ship _hull _shields (Functional 1) sc Nothing

stats :: (Fleet -> Bool) -> Fleet -> Fleet -> IO Double
stats tac fl1 fl2 = return . (/(fromInt n)) . fromInt . length . filter isWin =<< (sequence . replicate n $ fightUntil tac fl1 fl2)
        where n = 10000::Int
fromInt :: Int -> Double       
fromInt = fromIntegral 