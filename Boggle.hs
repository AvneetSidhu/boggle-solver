module Boggle (boggle) where
import Data.Map (Map)
import Data.List ()
import qualified Data.Map as Map

{--
    Fill in the boggle function below. Use as many helpers as you want.
    Test your code by running 'cabal test' from the tester_hs_simple directory.
--}

    
boggle :: [String] -> [String] -> [ (String, [ (Int, Int) ] ) ]
boggle board words = Map.toList ans
    where
        trie = createTrie words Map.empty
        wordsMap = wordsToMap words Map.empty
        ans = runner board (0,0) trie Map.empty (length board - 1) wordsMap

test :: Map.Map String [(Int, Int)]
test = ans
    where
        trie = createTrie ["as","at","ate","eat","set","sat","eats","east","seat","sate"] Map.empty
        board = ["ea","st"]
        visited = []
        currentWord = ""
        words = wordsToMap ["as","at","ate","eat","set","sat","eats","east","seat","sate"] Map.empty
        wordsToReturn = Map.empty
        ans = runner board (0,0) trie Map.empty (length board - 1) words


runner :: [String] -> (Int, Int) -> Map.Map String Int -> Map.Map String [(Int,Int)] -> Int -> Map.Map String Int -> Map.Map String [(Int,Int)]
runner board (row, col) trie wordsToReturn maxLen words
    | (row == maxLen) && (col == maxLen) = dfs board (maxLen, maxLen) [] wordsToReturn trie "" words
    | col == maxLen = runner board (row + 1, 0) trie o maxLen words
    | otherwise = runner board (row, col + 1) trie o maxLen words
        where 
            o = dfs board (row, col) [] wordsToReturn trie "" words

wordsToMap :: [String] -> Map.Map String Int -> Map.Map String Int
wordsToMap [] map = map
wordsToMap (word : rest) map = wordsToMap rest newMap
    where 
        newMap = Map.insert word 1 map

dfs :: [String] -> (Int, Int) -> [(Int, Int)] -> Map.Map String [(Int, Int)] -> Map.Map String Int  -> String -> Map.Map String Int -> Map.Map String [(Int, Int)]
dfs board (y, x) visited wordsToReturn trie currentWord words
    | Map.member currentWord trie && notElem (y, x) visited = c
    | otherwise = wordsToReturn 
    where 
        movesList = [(1,0),(-1,0),(0,1),(0,-1),(-1,-1),(1,-1),(-1,1),(1,1)]
        newLetter = (board !! y) !! x
        newCurrentWord = currentWord ++ [newLetter]
        newVisited = (y, x) : visited 
        newWordsToReturn = if Map.member newCurrentWord words then Map.insert newCurrentWord (reverse newVisited) wordsToReturn else wordsToReturn
        nextMoves = possibleMoves (y,x) movesList (length board) []
        c = if not (Map.member newCurrentWord trie) then wordsToReturn else foldl (\acc (row, col) -> dfs board (row, col) newVisited acc trie newCurrentWord words) newWordsToReturn nextMoves


possibleMoves :: (Int, Int) -> [(Int, Int)] -> Int -> [(Int, Int)] -> [(Int, Int)]
possibleMoves (y, x) [] fullLength possibleMovesList =  possibleMovesList
possibleMoves (y, x) ((y2, x2) : rest) fullLength possibleMovesList = possibleMoves (y, x) rest fullLength newPossibleMovesList
    where 
        newPossibleMovesList = 
            if y + y2 <= fullLength - 1 && y + y2 >= 0 && x + x2 <= fullLength - 1 && x + x2 >= 0 
                then  (y + y2, x + x2) : possibleMovesList
                else possibleMovesList
        

createTrie :: [String] -> Map.Map String Int -> Map.Map String Int 
createTrie [] dict = dict 
createTrie (word : rest ) dict = createTrie rest updatedDict 
    where 
        updatedDict = addWordToDict word dict 0 


addWordToDict :: String -> Map.Map String Int -> Int -> Map.Map String Int
addWordToDict s d l
    | l == length s = Map.insert (take l s) 1 d 
    | otherwise = addWordToDict s d2 l2
        where 
            d2 = Map.insert (take l s) 1 d 
            l2 = l + 1