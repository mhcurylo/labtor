module Main where

import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Char (toLower, isSpace, isAlpha)
import Data.Bifunctor (bimap)

main :: IO ()
main = do
  filterOutWords <- countFromFile <$> readFile "c.txt"
  labour <- countFromFile <$> readFile "l.txt"
  conservative <- countFromFile <$> readFile "t.txt"
  let common = zipTogether (M.difference labour filterOutWords) (M.difference conservative filterOutWords)
  let pointsMap = tuppleMap common
  let points = M.keys pointsMap
  let cosOf = cosPoints pointsMap
  print $ ascCommon pointsMap
  print $ ascCommon $ pointsWithin cosOf 0.1 pointsMap

countFromFile :: String -> M.Map String Int
countFromFile = M.fromListWith (+) . (`zip` repeat 1) . filter (\x -> length x > 2) . words . filter (\x -> isAlpha x || isSpace x) . map toLower

zipTogether :: M.Map String Int -> M.Map String Int -> M.Map String (Int, Int)
zipTogether = M.intersectionWith (\x y -> (x, y))

tuppleMap :: M.Map String (Int, Int) -> M.Map (Int, Int) [String]
tuppleMap = M.fromListWith (++) . map (\(k, (a, b)) -> ((a, b), [k]) ) . M.toList

ascCommon :: M.Map (Int, Int) [String] -> [(Int, [String])]
ascCommon = take 10 . M.toDescList . M.fromListWith (++) . map (\((a, b), v) -> (a + b, map (\s -> show (a, b) ++ ": " ++ s) v)) . M.toList

cosPoints :: M.Map (Int, Int) a -> Float
cosPoints = cosPoint . foldr1 sumP . M.keys
  where
    sumP (a, b) (c, d) = (a + c, b + d)

cosPoint :: (Int, Int) -> Float
cosPoint = cosP . floatPoint
  where
    floatPoint (a, b) = (fromIntegral a, fromIntegral b)
    cosP (a, b) = b / sqrt(a^2 + b^2)

cosPointWithin :: Float -> Float -> (Int, Int) -> Bool
cosPointWithin ini diff v = c > (ini - diff) && c < (ini + diff)
  where
    c = cosPoint v

pointsWithin :: Float -> Float -> M.Map (Int, Int) a -> M.Map (Int, Int) a
pointsWithin ini diff = M.filterWithKey (\k _ -> cosPointWithin ini diff k)
