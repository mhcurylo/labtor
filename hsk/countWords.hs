import qualified Data.Map as M
import Data.Char
  
ls = "bii bII! boo ooo"  

main = do
  print $ listTokens ls
  print $ countTokens (listTokens ls)
  print $ countWords ls
  

addNum:: Int -> [Int] -> [Int]
addNum = (:) 
   
listTokens :: String -> [String]
listTokens = words . filter isAlphaOrSpace . map toLower
  where
    isAlphaOrSpace x =  isAlpha x || isSpace x

countTokens :: [String] -> M.Map String Int
countTokens = M.fromListWith (+) . (`zip` repeat 1)

countWords :: String -> M.Map String Int
countWords = countTokens . listTokens


