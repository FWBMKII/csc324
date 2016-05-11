{- Assignment 2 - Finite Automata (due November 11, noon)

Notes:
- You may import Data.List; you may not import any other modules

***Write the names and CDF accounts for each of your group members below.***
<Name>, <CDF>
<Name>, <CDF>
-}
module Dfa (State, Symbol, Transition, Automaton(..),
            allStrings, tableToDelta, extend, possibleOutcomes,
            accept, language,
            removeUseless, isFiniteLanguage, language',
            epsilonClosure) where

import Data.List

-- Basic data types
type State = Integer
type Symbol = Char 
type Transition = (State, Symbol, State) 

t_start :: Transition -> State
t_start (st, _, _) = st
t_symbol :: Transition -> Symbol
t_symbol (_, sy, _) = sy
t_end :: Transition -> State
t_end (_, _, en) = en

-- Automaton Data Type
-- Automaton states alphabet transitions initial final
data Automaton = Automaton [State] [Symbol] [Transition] State [State] deriving Show
-- Some helper functions for you to access the different automaton components
states :: Automaton -> [State]
states (Automaton s _ _ _ _) = s
alphabet :: Automaton -> [Symbol]
alphabet (Automaton _ a _ _ _) = a
transitions :: Automaton -> [Transition]
transitions (Automaton _ _ ts _ _) = ts
initial :: Automaton -> State
initial (Automaton _ _ _ i _) = i
final :: Automaton -> [State]
final (Automaton _ _ _ _ f) = f

ex = Automaton [0,1,2]
               ['a','b']
               [(0,'a',1),
                (0,'b',2),
                (1,'a',1),
                (1,'b',1),
                (1,'b',2)]
               0
               [0,2]

--Hint: concatMap
-- Questions 1-4: transitions
tableToDelta :: [Transition] -> State -> Symbol -> [State]
tableToDelta ts = \st sy -> sort (nub (map (\x -> (t_end x))
                                           (filter (\x -> ((t_start x) == st && (t_symbol x) == sy))
                                                   ts)))
exf = tableToDelta (transitions ex)

extend :: (State -> Symbol -> [State]) -> (State -> String -> [State])
extend f =  \st syl -> sort (nub (extendHelper f [st] syl))
extendHelper :: (State -> Symbol -> [State]) -> [State] -> [Symbol] -> [State]
extendHelper f stl [] = stl
extendHelper f [] _ = []
extendHelper f stl syl = extendHelper f
                                      (foldl (\acc st -> acc ++ (f st (head syl)))
                                             []
                                             stl)
                                      (tail syl)

allStrings :: [Symbol] -> [[String]]
allStrings syl = [[""]] ++ map (\x -> (allStrHelper x syl)) (allStrings syl)

allStrHelper :: [String] -> [Symbol] -> [String]
allStrHelper l syl = foldl (\acc a -> acc ++ (map (\x -> a ++ [x]) 
                                                   syl))
                           []
                           l

possibleOutcomes :: Automaton -> State -> [[(String, [State])]]
possibleOutcomes a s = [[("", [s])]] ++ map (\x -> (pocHelper a x s))
                                            (possibleOutcomes a s)

pocHelper :: Automaton -> [(String, [State])] -> State -> [(String, [State])]
pocHelper atm lt s = foldl (\acc a -> acc ++ (foldl (\acc sy -> acc ++ [((fst a ++ [sy]), ((extend (tableToDelta (transitions atm))) s ((fst a) ++ [sy])))])
                                                  []
                                                  (alphabet atm)))
                         []
                         lt


-- Questions 5-6: acceptance

-- helper functionL isin, return True iff x is in list l
isin :: State -> [State] -> Bool
isin x [] = False
isin x l = if x == (head l)
           then True
           else isin x (tail l)

accept :: Automaton -> String -> Bool
accept atm syl = (filter (\x -> (isin x (final atm)))
                         (acceptHelper (tableToDelta (transitions atm)) [(initial atm)] syl [])) /= []

acceptHelper f [] _ l = []
acceptHelper f stl [] l = sort (nub (l ++ stl))
acceptHelper f stl syl l = let current_stl = foldl (\acc st -> acc ++ (f st (head syl)))
                                                   [] stl
                           in (acceptHelper f current_stl (tail syl) (l ++ current_stl))

language :: Automaton -> [String]
language atm = languageHelper atm 0

languageHelper atm n = let poc = (possibleOutcomes atm (initial atm)) !! n
                           filterpoc = filter (\t -> accept atm (fst t)) (poc)
                       in (if (filter (\t -> ((snd t) /= [])) poc) == []
                           then []
                           else (map (\t -> fst t) filterpoc) ++ languageHelper atm (n + 1))

--find the place for last string in language with length n
takeLanguageN :: [String] -> Int -> Int -> Int
takeLanguageN [] n counter = counter
takeLanguageN ll n counter = if length (head ll) > n 
                             then counter
                             else takeLanguageN (tail ll) n (counter + 1)
-- Questions 7-9: finiteness
removeUseless :: Automaton -> Automaton
removeUseless atm = let useful = sort (nub ([initial atm] ++ (uselessHelper atm)))
                    in (Automaton useful 
                                  (alphabet atm) 
                                  (filter (\t -> (isin (t_start t) useful) && (isin (t_end t) useful)) 
                                          (transitions atm)) 
                                  (initial atm) 
                                  (final atm))

uselessHelper atm = let strl = take (takeLanguageN (language atm) (length (states atm)) 0) (language atm)
                        exatm = extend (tableToDelta (transitions atm))
                    in (foldl (\acc str -> if (filter (\s -> (isin s (final atm))) (exatm (initial atm) str)) /= []
                                           then (sort (nub acc)) ++ (exatm (initial atm) str)
                                           else acc)
                              []
                              strl)


isFiniteLanguage :: Automaton -> Bool
isFiniteLanguage atm = let n_1 = (takeLanguageN (language atm) (length (states atm) + 1) 0)
                           n = (takeLanguageN (language atm) (length (states atm)) 0)
                       in ((take n (language atm)) == (take n_1 (language atm)))

language' :: Automaton -> [String]
language' atm = take (takeLanguageN (language atm) (length (states atm) + 1) 0) (language atm)


-- Question 10: epsilon transitions

epsilonClosure :: Automaton -> [State] -> [State]
epsilonClosure atm s = ecHelper (ecextendH atm) [] (sort (nub s)) 1
-- a infinite string with all elements to be epsilon
infempty = " " ++ map (\x -> x) infempty;
-- add a transition function to all states to itself with symbol epsilon
ecextendH atm = Automaton (states atm) (alphabet atm)
                          (foldl (\acc x -> acc ++ [(x, ' ',x)])
                                 (transitions atm)
                                 (states atm))
                          (initial atm) (final atm)

ecHelper :: Automaton -> [State] -> [State] -> Int -> [State]
ecHelper atm lasts s n = let current_s = (sort (nub (lasts ++ (extendHelper (tableToDelta (transitions atm)) s (take n infempty)))))
                         in (if lasts == current_s
                             then lasts
                             else (ecHelper atm current_s s (n + 1)))