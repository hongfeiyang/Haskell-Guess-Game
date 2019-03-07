-- COMP30020 Declarative Programming Project 1 ChordProbe code, written by
-- Hongfei Yang 783661 <hongfeiy1@student.unimelb.edu.au>.
-- 
-- The code implements mini-max techique, trying to guess the correct chord
-- using as few guesses as possible. After each guess, the next guess is made
-- based on the feedback of the previous guess.

module Proj1 (initialGuess, nextGuess, GameState) where

import Data.List

-- GameState consists a list of possible combinations, each combination is a
-- list of three distinct strings, with one letter from A to G and a number 
-- from 1 to 3
type GameState = [[String]] 

-- Generate all 1330 combinations
initialGuess:: ([String], GameState)
initialGuess = (firstGuess, allCombi)
    where
        firstGuess = ["A1", "B1", "C2"]
        allPitch = [a:b:[] | a <- ['A','B'..'G'], b <- ['1','2'..'3']]
        allCombi = [[a,b,c] | a <- allPitch, b <- allPitch, c <- allPitch,
         a < b, b < c]

-- Calculate the score of two combinations 
getScore :: [String] -> [String] -> (Int, Int, Int)
getScore guess target = (cPitchCount, cNoteCount, cOctaveCount)
    where
        -- get the list of correct pitches
        cPitchList = intersect guess target

        -- then get the number of the correct pitches
        cPitchCount = length cPitchList

        -- List of the remaining incorrect pitches
        remainTarget = target \\ cPitchList
        wrongGuess = guess \\ cPitchList

        -- Get the second and third digit in the score (x,x,x)

        remainTargetNote = getRemainNote remainTarget
        remainTargetOctave = getRemainOctave remainTarget
        remainGuessNote = getRemainNote wrongGuess
        remainGuessOctave = getRemainOctave wrongGuess
        
        -- Number of correct note in the incorrect pitches guessed
        cNoteCount = length remainTargetNote - (length (remainTargetNote
         \\ remainGuessNote))

        -- Number of correct octave in the incorrect pitches guessed
        cOctaveCount = length remainTargetOctave - (length (remainTargetOctave
         \\ remainGuessOctave))

        -- Extract the list of incorrect note guessed from the incorrect
        -- pitches
        getRemainNote :: [String] -> [Char]
        getRemainNote [] = []
        getRemainNote (oneNote:rest) = oneNote!!0 : getRemainNote rest

        -- Extract the list of incorrect octave guessed from the incorrect
        -- pitches
        getRemainOctave :: [String] -> [Char]
        getRemainOctave [] = []
        getRemainOctave (oneOctave:rest) = oneOctave!!1 : getRemainOctave rest

-- Make the next guess based on the feedback of the previous guess
nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)
nextGuess (lastGuess, prevReducedSet) score = (nGuess, nextReducedSet)
    where
        -- Reduce the size of the possible candidates based on the feedback,
        -- This is to remove all combinations that do not have the same score
        -- as if the guess is the actual target
        nextReducedSet = reduce lastGuess prevReducedSet score

        -- The next guess is made by choosing the maximum of the minimum 
        -- number each guess can clear as if it is the guess. Initially 
        -- the current minimum will be set to be a large number
        nGuess = miniMax nextReducedSet nextReducedSet [] 1330

-- Reduce the number of possible candidates by only keeping the ones that has
-- the same score
reduce :: [String] -> [[String]] -> (Int, Int, Int) -> [[String]]
reduce target [] targetScore = []
reduce target (candidate:rest) targetScore
    | getScore candidate target == targetScore = candidate : reduce
     target rest targetScore
    | otherwise = reduce target rest targetScore

-- Group the scores and their occurence frequencies, by comparing a given
-- combination against a set of combinations 
groupByScore :: [String] -> [[String]] -> [((Int, Int, Int), Int)]
groupByScore target [] = []
groupByScore target (oneGuess:rest) = updateScore (getScore oneGuess target)
 (groupByScore target rest)

-- Update the frequency of occurence of a score in a dictionary of scores
-- frequencies
updateScore :: (Int, Int, Int) -> [((Int, Int, Int), Int)] -> 
    [((Int, Int, Int), Int)]
updateScore newScore [] = [(newScore, 1)]
updateScore newScore ((currScore, count):rest)
    | newScore == currScore = ((currScore, count+1):rest)
    | otherwise = (currScore, count):(updateScore newScore rest)

-- Get the maximum number of occurence of a given score group
getMaxCount :: [((Int, Int, Int), Int)] -> Int -> Int
getMaxCount [] currMax = currMax
getMaxCount ((score, count):rest) currMax
    | count > currMax = getMaxCount rest count
    | otherwise = getMaxCount rest currMax

-- Apply mini-max technique to get the maximum of the minimum number each guess
-- can clear as if it is the guess, then get the guess with that can clear the 
-- largest number of the number of minimum guess it can clear in the given set.
miniMax :: [[String]] -> [[String]] -> [String] -> Int -> [String]
miniMax [] _ currMinGuess currMinScore = currMinGuess
miniMax (thisGuess:restGuess) reducedSet currMinGuess currMinScore
    | thisScore < currMinScore = 
        miniMax restGuess reducedSet thisGuess thisScore
    | otherwise = miniMax restGuess reducedSet currMinGuess currMinScore
    where
        thisScore = getMaxCount (groupByScore thisGuess reducedSet) (-1)



        