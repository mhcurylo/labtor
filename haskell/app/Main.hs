module Main where

import qualified Data.Map as M
import Data.Char (toLower, isSpace, isAlpha)
import Data.Tuple
import Data.Bifunctor

main :: IO ()
main = do
  filterOutWords <- countFromFile <$> readFile "sl.txt"
  labour <- rank . (M.\\ filterOutWords) . countFromFile <$> readFile "l.txt"
  conservative <- rank . (M.\\ filterOutWords) . countFromFile <$> readFile "t.txt"
  words <- countWords <$> readFile "l.txt"
  print $ ascCommon . inverseMap $ zipTogether labour conservative
  print $ words

countWords :: String -> M.Map String Int
countWords = M.fromListWith (+) . (`zip` repeat 1) . words . filter isAlphaOrSpace . map toLower
  where
    isAlphaOrSpace x =  isAlpha x || isSpace x

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
zipTogether = M.intersectionWith $ curry id

ascCommon :: M.Map (Int, Int) [String] -> [String]
ascCommon = take 10 . concat . map snd . M.assocs . inverseMapWith (first $ uncurry (+))
--(\((a, b), v) -> (a + b, v))

inverseMap :: Ord k => Ord v => M.Map k v -> M.Map v [k]
inverseMap = inverseMapWith $ (second $ replicate 1) . swap

inverseMapWith :: Ord k => Ord v => Ord a => Ord b => ((k, v) -> (a, [b])) -> M.Map k v -> M.Map a [b]
inverseMapWith f = M.fromListWith (++) . map f . M.toList
