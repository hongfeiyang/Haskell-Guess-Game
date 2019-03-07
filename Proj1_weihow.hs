{- Code for the all the functions for Project 1 Chord Probe Game
   COMP30020 Project 1
   Wei How Ng (828472)
-}

-------------------------------------------------------------------------------
--Initial module includes
module Proj1 (initialGuess, nextGuess, GameState) where
-------------------------------------------------------------------------------
--Standard Library Importations
import Data.List
--Type Declarations
type GameState = [[[String]]]

-------------------------------------------------------------------------------
--Takes a list and turns them into combinations of 3 for each guess
--Recursively call itself on smaller subsets of combinations of the lists
combThree :: [a] -> Int -> [[a]]
combThree _ 0 = [[]]
combThree [] _ = []
combThree (x:xs) k = (map (x:) (combThree xs (k-1)) ++ (combThree xs k))

-------------------------------------------------------------------------------
--guess the pitches initially, returns the guesses and gamestate
initialGuess :: ([String], GameState)
initialGuess = (["A1","B1","C2"], [allGuess, allComb])
    --initialising the list of all possible pitches
    where pitches = ["A1","A2","A3","B1","B2","B3","C1","C2","C3","D1","D2","D3","E1","E2","E3","F1","F2","F3","G1","G2","G3"]
          allComb = combThree pitches 3
          allGuess = allComb

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--guess the pitches with received information afterwards
--takes the
nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)
nextGuess (response, [allGuess, allComb]) score = output
    where reducedAllGuess = reduceAllGuess allGuess response score
          output = ((minimax reducedAllGuess allComb), [reducedAllGuess, reducedAllGuess])

-------------------------------------------------------------------------------
--minimax function which takes the GameState and derives the next best guess
--the GameState contains the reduced list of allGuess and constant allComb
--Inputs: GameState (allGuess,allComb)
--Outputs: the nextGuess
minimax :: [[String]]-> [[String]] -> [String]
minimax allGuess allComb = output
    where minList = minListBuild allGuess allComb
          indexOfMax = maybeNot(elemIndex (maximum minList) minList)
          output = allComb !! indexOfMax
-------------------------------------------------------------------------------
--minListBuild function to build a list of scores for each guess in allComb
--list would be the same length of allComb
--Inputs: allGuess (reduced) and allComb (fixed)
--Ouputs: List of scores pertaining to each guess in allComb
minListBuild :: [[String]] -> [[String]] -> [Int]
minListBuild _ [] = []
minListBuild allGuess (c:cs) = (minRemove c allGuess):(minListBuild allGuess cs)
-------------------------------------------------------------------------------
--maybeNot function which takes the Maybe Int that elemIndex ouputs and turns
--it into an Int
--Inputs: Maybe Int
--Outputs: Int for the index
maybeNot :: (Maybe Int) -> Int
maybeNot Nothing = (-1)
maybeNot (Just n) = n
-------------------------------------------------------------------------------
--countScore function to deduce the number of matches each score has with others
--Inputs: score to match, rest of the scores
--Outputs: value representing the
countScore :: (Int,Int,Int) -> [(Int,Int,Int)] -> Int
countScore _ [] = 0
countScore tuple (x:xs)
    | tuple == x = 1 + countScore tuple xs
    | otherwise = countScore tuple xs
-------------------------------------------------------------------------------
--minScore function to derive the minimum scores of each guess
--creates a list of scores for each guess in allComb
--Inputs: two lists of scoreList, tuple scores of one guess against allGuess
--Outputs: List of scores matching the the guess
minScore :: [(Int,Int,Int)] -> [(Int,Int,Int)]-> [Int]
minScore [] _ = []
minScore (x:xs) scoreList = score:(minScore xs scoreList)
    where score = (length scoreList) - (countScore x scoreList)
-------------------------------------------------------------------------------
--minRemove function to produce the minimum score of one guess in allComb
--returns the  minimum number of possibilities the guess can remove in allGuess
--Inputs: single guess in allComb and allGuess
--Outputs: score of a guess in allComb
minRemove :: [String] -> [[String]] -> Int
minRemove _ [] = 0
minRemove target allGuess = minRemovable
    where --create a score list of tuples for allGuess against target in allComb
          scoreList = generateMatch allGuess target
          minRemovable = minimum (minScore scoreList scoreList)
-------------------------------------------------------------------------------
--Count guess function to deduce the tuple matches of each guess
--Using the minimax technique
--Takes an input of a list of strings for guess and the score tuple and the
--set of allGuess to produce a minimum number of guesses this guess would
--eliminate in allGuess
countGuess :: Eq a => [[a]] -> [[[a]]] -> (Int,Int,Int) -> Int
countGuess _ [] _ = 0
countGuess guess (x:xs) scores = if tupleMatch scores (scoreMark guess x)
                                    then  1 + (countGuess guess xs scores)
                                    else countGuess guess xs scores
-------------------------------------------------------------------------------
--Score Match Function takes two tuples of scores and determines if
--they are the same
--Inputs: Target score tuple and guess score tuple
--Output: Returns a boolean variable
tupleMatch :: (Int,Int,Int) -> (Int, Int, Int) -> Bool
tupleMatch (x,y,z) (a,b,c) = if (x == a) && (y == b)  && (z == c)
                                then True
                                else False

-------------------------------------------------------------------------------
{-=============================================================================
==========================TUPLE SCORING FUNCTIONS==============================
=============================================================================-}
-------------------------------------------------------------------------------
scoreMark :: Eq a => [[a]] -> [[a]] -> (Int,Int,Int)
scoreMark target guess = (right, rightNote, rightOctave)
  where guess'      = nub guess
        right       = length $ intersect guess' target
        num         = length guess'
        rightNote   = num - (length $ deleteFirstsBy (eqNth 0) guess' target)
                    - right
        rightOctave = num - (length $ deleteFirstsBy (eqNth 1) guess' target)
                    - right


-- | eqNth n l1 l2 returns True iff element n of l1 is equal to
--   element n of l2.
eqNth :: Eq a => Int -> [a] -> [a] -> Bool
eqNth n l1 l2 = (l1 !! n) == (l2 !! n)

-------------------------------------------------------------------------------



{-=============================================================================
========================REMOVE WRONG GUESSES FUNCTIONS=========================
=============================================================================-}
-------------------------------------------------------------------------------
--Elimination function to get rid of guesses in allGuess which produce the
--the same response as the previous guess
--Inputs: Input set of the list of guesses to remove and set of allGuess
--Outputs: The 'cleaned out' allGuess set
elimGuess :: Eq a => [[[a]]] -> [[[a]]] -> [[[a]]]
elimGuess guesses allGuess = filter (not . (`elem` guesses)) allGuess
-------------------------------------------------------------------------------
--reduceAllGuess function to reduce the size of allGuess
--removes the guesses which do not give the same response as the previous guess
--Inputs: allGuess, the previous guess, tuple score of guess
--Outputs: reduced list of allGuess
reduceAllGuess :: [[String]] -> [String] -> (Int,Int,Int) -> [[String]]
reduceAllGuess [] _ _ = []
reduceAllGuess allGuess prevGuess score = reducedGuesses
    where --create a score list of tuples for allGuess against prevGuess
          scoreList = generateMatch allGuess prevGuess
          --derive the list of combinations to eliminate from allGuess
          elimList = generateElims allGuess scoreList score
          --eliminate the ones which do not have the same score
          reducedGuesses = elimGuess elimList allGuess

-------------------------------------------------------------------------------
--generateElims function to take note of the guesses we want to remove
--outputs a list of guesses that do not have the same tuple score
--Inputs: allGuess, scoreList of allGuess onto correct score, correct score
--Outputs: List of guesses which scores do not match with the correct score
generateElims :: [[String]] -> [(Int,Int,Int)] -> (Int,Int,Int) -> [[String]]
generateElims [] _ _ = []
generateElims _ [] _ = []
generateElims (x:xs) (t:ts) target
    | tupleMatch target t = generateElims xs ts target
    | otherwise = x:(generateElims xs ts target)
-------------------------------------------------------------------------------
--generateMatch function to generate the scores of allGuess against prevGuess
--creates a list of score tuples to determine if the ones to take out
--Inputs: allGuess, previous guess treated as correct one
--Outputs: List of tupleScore for all guess
generateMatch :: [[String]] -> [String] -> [(Int,Int,Int)]
generateMatch [] _ = []
generateMatch _ [] = []
generateMatch guesses target = map (target `scoreMark`) guesses
-------------------------------------------------------------------------------
