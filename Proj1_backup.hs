module Proj1 (initialGuess, nextGuess, GameState) where

import Data.List

type GameState = [[[String]]] -- [AllPossibleSet, ReducedSet]

initialGuess:: ([String], GameState)

initialGuess = (firstGuess, [allCombi, allCombi \\ [firstGuess]])
    where
        firstGuess = ["B1", "C3", "G1"]
        allPitch = [a:b:[] | a <- ['A','B'..'G'], b <- ['1','2'..'3']]
        allCombi = [[a,b,c] | a <- allPitch, b <- allPitch, c <- allPitch, a < b, b < c]

getScore :: [String] -> [String] -> (Int, Int, Int)
getScore guess target = (cPitchCount, cNoteCount, cOctaveCount)
    where
        cPitchList = intersect guess target
        cPitchCount = length cPitchList
        remainTarget = target \\ cPitchList
        wrongGuess = guess \\ cPitchList
        remainTargetNote = getRemainNote remainTarget
        remainTargetOctave = getRemainOctave remainTarget
        remainGuessNote = getRemainNote wrongGuess
        remainGuessOctave = getRemainOctave wrongGuess
        cNoteCount = length remainTargetNote - (length (remainTargetNote \\ remainGuessNote))
        cOctaveCount = length remainTargetOctave - (length (remainTargetOctave \\ remainGuessOctave))

        getRemainNote :: [String] -> [Char]
        getRemainNote [] = []
        getRemainNote (x:xs) = x!!0 : getRemainNote xs

        getRemainOctave :: [String] -> [Char]
        getRemainOctave [] = []
        getRemainOctave (x:xs) = x!!1 : getRemainOctave xs

nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)

nextGuess (lastGuess, [prevUSet, prevSSet]) score = (nGuess, [allPossibleSet, reducedSet])
    where
        nGuess = miniMax reducedSet reducedSet score [] (-1)

        allPossibleSet = prevUSet \\ [lastGuess]
        reducedSet = reduce lastGuess prevSSet score

        reduce :: [String] -> [[String]] -> (Int, Int, Int) -> [[String]]
        reduce target [] targetScore = []
        reduce target (candidate:rest) targetScore
            | getScore candidate target == targetScore = candidate : reduce target rest targetScore
            | otherwise = reduce target rest targetScore

        -- takes one possible guess and get the number of guesses it can reduce from the reduce set of possible guesses
        getOneScore :: [String] -> [[String]] -> (Int, Int, Int) -> Int
        getOneScore _ [] _ = 0
        getOneScore target (x:xs) targetScore
            | getScore x target == targetScore = 1 + getOneScore target xs targetScore
            | otherwise = 0 + getOneScore target xs targetScore

        miniMax :: [[String]] -> [[String]] -> (Int, Int, Int) -> [String] -> Int -> [String]
        miniMax [] _ _ currMaxGuess currMaxScore = currMaxGuess
        miniMax (thisGuess:restGuess) reducedSet targetScore currMaxGuess currMaxScore
            | length reducedSet == 1 = reducedSet !! 0
            | thisScore > currMaxScore = miniMax restGuess reducedSet targetScore thisGuess thisScore
            | otherwise = miniMax restGuess reducedSet targetScore  currMaxGuess currMaxScore
            where
                thisScore = getOneScore thisGuess reducedSet targetScore

        