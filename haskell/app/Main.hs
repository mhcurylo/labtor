module Main where

import qualified Data.Map as M
import Data.Char (toLower, isSpace, isAlpha)

main :: IO ()
main = do
  filterOutWords <- countFromFile <$> readFile "sl.txt"
  labour <- rank . (M.\\ filterOutWords) . countFromFile <$> readFile "l.txt"
  conservative <- rank . (M.\\ filterOutWords) . countFromFile <$> readFile "t.txt"
  print $ ascCommon . inverseMap $ zipTogether labour conservative

countWords :: String -> M.Map String Int
countWords = M.fromListWith (+) . (`zip` repeat 1) . words . map toLower

countFromFile :: String -> M.Map String Int
countFromFile = M.fromListWith (+) . (`zip` repeat 1) . filter longerThan2 . words . filter isAlphaOrSpace . map toLower
  where
    longerThan2 x = length x > 2
    isAlphaOrSpace x =  isAlpha x || isSpace x

rank :: M.Map String Int -> M.Map String Int
rank = M.fromList . concatMap attachRank . zip [0..] . M.toDescList . inverseMap
  where
    attachRank (k, (_, ws)) = zip ws (repeat k)

zipTogether :: M.Map String Int -> M.Map String Int -> M.Map String (Int, Int)
zipTogether = M.intersectionWith (\x y -> (x, y))

ascCommon :: M.Map (Int, Int) [String] -> [String]
ascCommon = take 10 . concat . map snd . M.assocs . inverseMapWith (\((a, b), v) -> (a + b, v))

inverseMap :: Ord k => Ord v => M.Map k v -> M.Map v [k]
inverseMap = inverseMapWith (\(k, v) -> (v, [k]))

inverseMapWith :: Ord k => Ord v => Ord a => Ord b => ((k, v) -> (a, [b])) -> M.Map k v -> M.Map a [b]
inverseMapWith f = M.fromListWith (++) . map f . M.toList
