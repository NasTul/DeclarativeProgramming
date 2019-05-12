-- Declarative Programming 
-- Haskell Proj1 
-- Author: Lin Li <lili1@student.unimelb.edu.au>

-- Purpose: This is a game of musician, the composer choose three pitch musical
-- chord, where each pitch comprises a musical note, one of A, B, C, D, E, F,
-- or G, and an octave, one of 1, 2, or 3. And the performer repeatedly chooses
-- a similarly defined chord as a guess Until the performer get it right. This 
-- computer program is used to simulate this guessing process.

-- Vsion 2.1: Using feedback to Select a list of all possible guesses with the
-- same score as the original guess Find the expectation of all the remaining
-- possible guesses, and Take the Minimum expectation guess from the list as
-- the next guess Repeat until get the correct answer
-- Points earned : 70
-- Total CPU T =  6266ms

-- Vsion 2: Using feedback to Select a list of all possible guesses with the
-- same score as the original guess Take the first guess from the list as the
-- next guess Repeat until get the correct answer
-- Points earned : 69.603
-- Total CPU T =  19ms

-- Vsion 1.1: 
-- Using [A1,A1,A1,B2,B2,B2,C3,C3,C3,D1,D1,D1,E1,E1,E1,F1,F1,F1,G1,G1,G1] as an
-- auxiliary list Pick 3 pitch in order as a guess, Move Tree pitch backward at
-- a time, Until the list is empty,Remove the letter and numer from all goals
-- no longer, Then combine the remaining letter and number
-- Points earned : 56.157
-- Total CPU T =  6ms

-- Vision 1: 
-- Using [A1,B1,C1,D1,E1,F1,G1,A2,B2,C2,D2,E2,F2,G2,A3,B3,C3,D3,E3,F3,G3] as an
-- auxiliary list Pick 3 pitch in order as a guess, Move one pitch backward at
-- a time, Until the list is empty,Remove the pitch from all goals no longer,
-- Then combine the remaining pitch
-- Points earned : 39.5471
-- Total CPU T =  8ms

-- module declaration
module Proj1 (Pitch, toPitch, feedback,
GameState, initialGuess, nextGuess) where

-- import for debug, Output intermediate variable
-- Import necessary libraries
import Debug.Trace
import Data.List

-- Define the pitch, is defined as two char, note and octave
type Note   = Char
type Octave = Char
-- Record the number of identical scores
type SameScoreNum = (Int,(Int,Int,Int))
data Pitch =Pitch Note Octave  deriving (Eq)

-- Instantiate the pitch and TargetList show
instance  Show Pitch where show = showPitch
showPitch :: Pitch -> String
-- Change the pitch show function as Pitch 'A' '1' ->  A1
showPitch (Pitch note octave) = [note]++[octave]

-- Define the GameState, Store game state, Stores a list of possible guesses
type AllPossiblePitchPool = [Pitch]
data GameState = GameState  [AllPossiblePitchPool]  deriving (Eq,Show)

-- Enter pitch and get the pitch note and octave
firstNote :: Pitch -> Char   
firstNote (Pitch firstnote _ ) = firstnote   
firstOctave :: Pitch -> Char   
firstOctave (Pitch  _ firstoctave) = firstoctave   

-- Judge whether the target pitch is in the list
-- Return True if the pitch is in the list
isInPitchList :: Pitch->[Pitch]->Bool
isInPitchList inputPitch [] = False
isInPitchList inputPitch (headPitchList:tailPitchList) 
-- Returns true if the pitch entered is equal to the first element in the list
    |inputPitch == headPitchList = True 
    |otherwise = isInPitchList inputPitch tailPitchList


-- Convert the string into pitch format
toPitch :: String -> Maybe Pitch
toPitch inputStr = if 
-- Constrain the pitch structure, the range of the first element is A-G
-- the second element is 1-3
   (length(inputStr) == 2) &&
   (head(inputStr) < 'H') &&
   (head(inputStr) > '@') &&
   (tail(inputStr) < "4") &&
   (tail(inputStr) > "0") 
-- If the string matches the pitch data structure, the pitch data is
-- constructed. If not , out put Nothing
   then Just(Pitch (inputStr!!0) (inputStr!!1)) else Nothing

-- Calculate guess pitches in scoring goals
-- If the pitch in the guess is exactly the same as the pitch in the target,
-- the first parameter is scored 1 point The second parameter of the same note
-- gets one point,The same octave third parameter plus one point Exactly the
-- same pitch score is not added to the second and third parameters To avoid
-- duplicate scoring, The removeSamePitch function is used to remove the same
-- element
feedback :: [Pitch] -> [Pitch] -> (Int,Int,Int)
feedback [] [] = (0,0,0)
feedback x []  = (0,0,0)
feedback [] y  = (0,0,0)
feedback needPitch ansPitchList = (scoreWholeEq,scoreHeadEq,scoreTailEq)
    where
-- Calculate equal scores for both note and octave
    scoreWholeEq = scoreOfwholeEq 0 needPitch ansPitchList
-- Calculate equal scores for note, To prevent duplicate scoring, remove
-- duplicate elements from the list
    scoreHeadEq = 
        calListEq 0  afterRmSamePitchNoteNeed afterRmSamePitchNoteAns
-- Calculate equal scores for both octave,To prevent duplicate scoring, remove
-- duplicate elements from the list
    scoreTailEq = 
        calListEq 0 afterRmSamePitchOctaveNeed afterRmSamePitchOctaveAns
    afterRmSamePitchNoteNeed   =
        (splitPitchNote (removeSamePitch ansPitchList needPitch))
    afterRmSamePitchNoteAns    =
        (splitPitchNote (removeSamePitch needPitch ansPitchList))
    afterRmSamePitchOctaveNeed =
        (splitPitchOctave (removeSamePitch ansPitchList needPitch))
    afterRmSamePitchOctaveAns  =
        (splitPitchOctave (removeSamePitch needPitch ansPitchList))

-- Removes all target elements from the list
remove :: Eq a => a -> [a] -> [a]
remove element list = filter (\e -> e/=element) list

-- Removes the same target element from the list
removeSamePitch :: [Pitch] -> [Pitch] -> [Pitch]
removeSamePitch [] target = target
removeSamePitch (inPuthead:inPuttail) target 
    |isInPitchList inPuthead target == True =
        removeSamePitch inPuttail removeInputFromList 
    |otherwise = removeSamePitch inPuttail target
    where 
    removeInputFromList = remove inPuthead target

-- Divide the pitch list into note lists
splitPitchNote :: [Pitch]->[Note]
splitPitchNote [] = []
-- Take notes from the Pitch list and splice them into a list of notes
splitPitchNote wholePitch@(inPuthead:inPuttail) = 
    [firstNote inPuthead] ++ splitPitchNote inPuttail

-- Divide the pitch list into octave lists
splitPitchOctave :: [Pitch]->[Note]
splitPitchOctave [] = []
-- Take octave from the Pitch list and splice them into a list of octave
splitPitchOctave wholePitch@(inPuthead:inPuttail) = 
    [firstOctave inPuthead] ++ splitPitchOctave inPuttail

-- Calculate the same number of note or octave in two pitch lists
calListEq :: Int -> [Char] -> [Char] -> Int
calListEq acc input [] = acc
calListEq acc [] target = acc
calListEq acc (inPuthead:inPuttail) target 
    |(isElementEq inPuthead target) == True =
        calListEq (acc+1) inPuttail afterDel
    |otherwise =  calListEq acc inPuttail target
    where
    afterDel =  delete inPuthead target


-- Determine whether the note or octave is in the list
isElementEq :: Char -> [Char] -> Bool
isElementEq input [] = False
isElementEq input (inPuthead:inPuttail)
    |input == inPuthead = True 
    |otherwise = isElementEq input inPuttail

-- Calculate the number of guesses in the pitch list of the target list
scoreOfwholeEq :: Int -> [Pitch] -> [Pitch] -> Int
scoreOfwholeEq acc input [] = acc
scoreOfwholeEq acc [] target = acc
scoreOfwholeEq  acc (inPuthead:inPuttail) target 
    |(isInPitchList inPuthead target) == True =
        scoreOfwholeEq (acc+1) inPuttail target 
    |otherwise = scoreOfwholeEq acc inPuttail target

-- Initializes the guess, outputting the first guess and the game state
-- Try initial Guess one by one and The results of A1,B2 and C3 were get full
-- mark
initialGuess :: ([Pitch],GameState)
initialGuess = ([(Pitch 'A' '1'),(Pitch 'B' '2'),(Pitch 'C' '3')], 
    GameState (getCombinations 3 (genertePitchPool "ABCDEFG" "123")))

-- Combine two input parameters
getCombinations :: Int -> [a] -> [[a]] 
getCombinations 0 xs = [[]] 
getCombinations n [] = [] 
getCombinations n (x:xs) =
    (map (x:) (getCombinations (n-1) xs)) ++ (getCombinations n xs)

-- Convert the string into pitch
mytoPitch :: String ->  Pitch
mytoPitch inputStr =  Pitch (inputStr!!0) (inputStr!!1) 

-- Generate the possible pitch list through the combination of the first
-- parameter and the second parameter
genertePitchPool ::  String -> String -> [Pitch]
genertePitchPool letter [] =[]
genertePitchPool [] number =[]
genertePitchPool wholeList@(letter:tailletter) (number:tailnumber) =
    (combineLetter wholeList number) ++ genertePitchPool wholeList tailnumber

-- Combine the first parameter and the second into a ptich
combineLetter :: String -> Char -> [Pitch]
combineLetter [] number =[]
combineLetter (letter:tailletter) number = 
    [mytoPitch(letter : [number])] ++ combineLetter tailletter number

-- Enter the previous guess and game state, and the current score, out put the
-- next guess and game state Increase the probability of guessing by
-- continually eliminating the set of possible guesses Find all guesses with
-- the same score as the previous guess. Use these guesses as the rest of the
-- waiting list
nextGuess ::([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)
nextGuess  (preGuess, GameState allPosiblePool) (a,b,c) =
    (nextGuess, GameState possiblePitchPool) 
    where
-- Eliminate guesses with different scores from the guess list
    possiblePitchPool = removeNotSameScore preGuess allPosiblePool (a,b,c)
-- The first parameter to findSmallExpect is initialized to an unlikely maximum
    nextGuess = findSmallExpect 9999 possiblePitchPool []

-- Judge whether the two pitch are the same as the score of taget
isSameScore :: [Pitch] -> [Pitch] -> (Int,Int,Int) -> Bool
isSameScore preGuess waitGuess (a,b,c) |
    feedback preGuess waitGuess == (a,b,c) = True |
    otherwise = False

-- Remove the pitch with different scores from the Possible Pitch List
removeNotSameScore :: [Pitch] -> [[Pitch]] -> (Int,Int,Int) -> [[Pitch]]
removeNotSameScore preGuess []  (a,b,c)  = []
removeNotSameScore preGuess (pitchListHead:pitchListtail) (a,b,c) |
    isSameScore preGuess pitchListHead (a,b,c) == True =
    pitchListHead : removeNotSameScore preGuess pitchListtail (a,b,c) |
    otherwise = removeNotSameScore preGuess pitchListtail (a,b,c) 

-- Find the pitch that has the least expectation
findSmallExpect :: Int -> [[Pitch]] ->[Pitch] -> [Pitch] 
findSmallExpect acc [] recordPitch = recordPitch
-- Iterate through the expectations of each element in the list
findSmallExpect  acc whole@(inPuthead:inPuttail) recordPitch |
    findExpect inPuthead whole < acc = 
    findSmallExpect (findExpect inPuthead whole) inPuttail inPuthead |
    otherwise =  findSmallExpect acc inPuttail recordPitch

-- Calculate the pitch expectation
findExpect :: [Pitch] ->[[Pitch]] -> Int
findExpect input pitchPool =
    calExpect (sameFeedbackNum (generateExpectList input pitchPool) []) 

-- Generate score list,For each stay guess pitches to calculate score according
-- to the feedback The same score is put together
generateExpectList :: [Pitch] ->[[Pitch]] -> [SameScoreNum]
generateExpectList pitch [] = []
generateExpectList pitch (inPuthead:inPuttail) =
    sort ((0,(feedback pitch inPuthead)) : generateExpectList pitch inPuttail) 

-- Count the number of elements in the score list that have the same score
sameFeedbackNum :: [SameScoreNum] -> [SameScoreNum] -> [SameScoreNum]
sameFeedbackNum [] calPool = calPool
sameFeedbackNum (inPuthead:inPuttail) calPool
-- The second parameter is the list to be output
-- When the length of the tape output list is 1, count directly in the first
-- element If the next element is the same as the current element, the
-- cumulative value is increased by 1, and if it is different, different
-- elements are added to the list to be output
    |(length calPool) > 1 && (snd inPuthead) == snd (last calPool)  =
        sameFeedbackNum inPuttail (init calPool ++ [(addNum,scoreTuple)]) 
    |(length calPool) == 1 && (snd inPuthead) == snd (last calPool) =
        sameFeedbackNum inPuttail [(addNum,scoreTuple)] 
    |otherwise = 
        sameFeedbackNum inPuttail (calPool ++ [inPuthead])
    where
    addNum = (fst (last calPool))+1
    scoreTuple = (snd (last calPool))

-- Calculate the expectation based on the score
-- Dividing by the length of the list does not affect the expected comparison,
-- so there is no division at this point
calExpect ::  [SameScoreNum] -> Int 
calExpect []  = 0 
calExpect whole@(inPuthead:inPuttail) =
    ((fst inPuthead) * (fst inPuthead)) + calExpect inPuttail

 


