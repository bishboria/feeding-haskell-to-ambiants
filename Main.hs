module Main where

import Data.Map (keys)
import Data.List (unfoldr)
import System.Environment

import Ants
import Biology
import Visualize
import Geography
import NeuroCartography
import Kinetics

main :: IO ()
main = do
    args <- getArgs
    s  <- readFile $ args !! 0  -- world file
    rb <- readFile $ args !! 1  -- red brain
    bb <- readFile $ args !! 2  -- black brain
    let i  = read (args !! 3) :: Int
        rs = readBrainState $ lines rb
        bs = readBrainState $ lines bb
        firstWorld = parse (mkWorld rs bs 12345) s
        lastWorld  = last $ take i $ go firstWorld
    printInfo firstWorld
    printInfo lastWorld
    putScore Red redHill lastWorld
    putScore Black blackHill lastWorld

printInfo w = do
    putStrLn $ render w
    putStrLn $ show . length . keys $ ants w

go w = unfoldr (\x -> Just (x, multistep x)) w

score h w = let pos = keys $ h w
            in  foldr (\x y -> foodAt w x + y) 0 pos

putScore color hill w = putStrLn $ (show color) ++ " score: " ++ (show $ score hill w)
