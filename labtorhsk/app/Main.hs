module Main where

import qualified Data.Map as M
import Data.Char (toLower, isSpace, isAlpha)

main :: IO ()
main = do
  filterOutWords <- countFromFile <$> readFile "c.txt"
  labour <- rank . (M.\\ filterOutWords) . countFromFile <$> readFile "l.txt"
  conservative <- rank . (M.\\ filterOutWords) . countFromFile <$> readFile "t.txt"
  print $ take 10 $ ascCommon . inverseMap $ zipTogether labour conservative

countFromFile :: String -> M.Map String Int
countFromFile = M.fromListWith (+) . (`zip` repeat 1) . filter (\x -> length x > 2) . words . filter (\x -> isAlpha x || isSpace x) . map toLower

rank :: M.Map String Int -> M.Map String Int
rank = M.fromList . concatMap attachRank . zip [0..] . M.toDescList . inverseMap
  where
    attachRank (k, (_, ws)) = zip ws (repeat k)

zipTogether :: M.Map String Int -> M.Map String Int -> M.Map String (Int, Int)
zipTogether = M.intersectionWith (\x y -> (x, y))

ascCommon :: M.Map (Int, Int) [String] -> [(Int, [String])]
ascCommon = take 10 . M.assocs . inverseMapWith (\((a, b), v) -> (a + b, map (\s -> show (a, b) ++ ": " ++ s) v))

inverseMap :: Ord k => Ord v => M.Map k v -> M.Map v [k]
inverseMap = inverseMapWith (\(k, v) -> (v, [k]))

inverseMapWith :: Ord k => Ord v => Ord a => Ord b => ((k, v) -> (a, [b])) -> M.Map k v -> M.Map a [b]
inverseMapWith f = M.fromListWith (++) . map f . M.toList
