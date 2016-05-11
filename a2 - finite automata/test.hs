import Test.HUnit
import Dfa (State, Symbol, Transition, Automaton(..),
            allStrings, tableToDelta, extend, possibleOutcomes,
            accept, language,
            removeUseless, isFiniteLanguage, language', epsilonClosure)


tableToDeltaTests = TestList [
    [2] ~=? tableToDelta [(1, 'f', 2)] 1 'f',
    -- Note: a symbol could be passed in that doesn't appear in any transition
    [] ~=? tableToDelta [(1, 'f', 2)] 1 'b',
    -- test order
    [2, 3] ~=? tableToDelta [(1, 'a', 2), (1, 'a', 3)] 1 'a',
    -- test duplicate 
    [2] ~=? tableToDelta [(1, 'a', 2), (1, 'a', 2)]  1 'a',
    -- test start not exist
    [] ~=? tableToDelta [(1, 'a', 2), (3, 'a', 3)] 2 'a'
    ]

extendTests = TestList [
    [2] ~=? extend (tableToDelta [(1, 'f', 2), (2, 'f', 2)]) 1 "ff",
    -- test empty string
    [1] ~=? extend (tableToDelta [(1, 'a', 1)]) 1 "",
    -- test one char
    [1, 2] ~=? extend (tableToDelta [(1, 'a', 1), (1, 'a', 2)]) 1 "a",
    -- test duplicate
    [1] ~=? extend (tableToDelta [(1, 'a', 1), (1, 'a', 1)]) 1 "aa",
    -- test run out of range
    [] ~=? extend (tableToDelta [(1, 'a', 2)]) 1 "aa",
    -- test part of string out of range
    [1, 2] ~=? extend (tableToDelta [(1, 'a', 1), (1, 'a', 2)]) 1 "aaa",
    -- test order
    [1, 2, 3, 4] ~=? extend (tableToDelta [(1, 'a', 1), (1, 'a', 2),
											(1, 'a', 4),
											(1, 'a', 3)]) 1 "aaaaa",
	-- test not in the alpha
	[] ~=? extend (tableToDelta [(1, 'a', 1), (1, 'a', 2)]) 1 "b",
	[] ~=? extend (tableToDelta [(1, 'a', 1), (1, 'a', 2)]) 1 "ab",
	[] ~=? extend (tableToDelta [(1, 'a', 1), (1, 'a', 2)]) 1 "ba",
	--test start not exist
	[] ~=? extend (tableToDelta [(1, 'a', 1)]) 2 "aaaa"
    ]

allStringsTests = TestList [
    ["aa", "ab", "ba", "bb"] ~=? allStrings "ab" !! 2,
    -- test first sublist
    [""] ~=? allStrings "ab" !! 0,
    -- test only one char
    ["aaaa"] ~=? allStrings "a" !! 4,
    -- test order
    ["aaa","aab","aac","aba","abb","abc","aca","acb","acc","baa","bab",
    "bac","bba","bbb","bbc","bca","bcb","bcc","caa","cab","cac","cba",
    "cbb","cbc","cca","ccb","ccc"] ~=? (allStrings "abc") !! 3
    ]
    

possibleOutcomesTests = TestList [
    [("aa",[1]), ("ab",[0,2]), ("ba",[0,2]), ("bb",[1])] ~=?
        (possibleOutcomes (Automaton [0,1,2]
                                     ['a','b']
                                     [(0,'a',1),
                                      (1,'a',2),
                                      (0,'b',0),
                                      (1,'b',1),
                                      (2,'b',2),
                                      (1,'a',0)] 0 [2]) 1) !! 2,
     -- test no transitions
     [("a", []), ("b", [])] ~=?
		(possibleOutcomes (Automaton [0,1,2]
                                     ['a','b']
                                     [] 0 [2]) 1) !! 1,
     -- test order of string
     [("aa", []), ("ab", []),("ba", []), ("bb", [])] ~=?
		(possibleOutcomes (Automaton [0,1,2]
                                     ['a','b']
                                     [] 0 [2]) 1) !! 2,
     -- test order of output states
     [("a", [1, 2, 3, 4, 5])] ~=?
		(possibleOutcomes (Automaton [0,1,2]
                                     ['a']
                                     [(1, 'a', 1),
                                     (1, 'a', 5),
                                     (1, 'a', 4),
                                     (1, 'a', 2),
                                     (1, 'a', 3)] 0 [2]) 1) !! 1,
     -- test alpha not in the transition
     [("a", [1, 2, 3, 4, 5]), ("b", [])] ~=?
		(possibleOutcomes (Automaton [0,1,2]
                                     ['a', 'b']
                                     [(1, 'a', 1),
                                     (1, 'a', 5),
                                     (1, 'a', 4),
                                     (1, 'a', 2),
                                     (1, 'a', 3)] 0 [2]) 1) !! 1,
     -- test out of range
     [("aa", [])] ~=?
		(possibleOutcomes (Automaton [0,3]
                                     ['a']
                                     [(1, 'a', 3)] 0 [3]) 1) !! 2							
    ]

a1 = Automaton [0,1] ['a'] [(0,'a',1),(1,'a',0)] 0 [0]
a1' = Automaton [0,1] ['a'] [(0,'a',1),(1,'a',0)] 0 [1]
a1'' = Automaton [0,1] ['a'] [(0,'a',1)] 0 [1]
a1''' = Automaton [0,1] ['a'] [(1,'a',0)] 0 [1]

acceptTests = TestList [
    True ~=? accept a1 "aa",
    -- test empty input
    True ~=? accept a1 "",
    False ~=? accept a1' "",
    -- test not in the alpha
    False ~=? accept a1 "b",
    False ~=? accept a1 "ab",
    False ~=? accept a1 "ba",
    -- test out of range
    False ~=? accept a1'' "aa",
    -- test start not in the transation
    False ~=? accept a1''' "a"
    ]

l1 = Automaton [0,1] ['a'] [] 0 [0]  
l2 = Automaton [0] ['a', 'b'] [(0, 'a', 0), (0, 'b', 0)] 0 [0]
languageTests = TestList [
    ["","aa"] ~=? take 2 (language a1),
    -- test no transations
    [""] ~=? take 1 (language l1),
    -- test order
    ["", "a", "b", "aa", "ab", "ba", "bb"] ~=? take 7 (language l2)
    ]

a2 = Automaton [0,1] ['a'] [(0,'a',1)] 0 [0]

eq :: Automaton -> Automaton -> Bool
eq (Automaton s1 a1 ts1 i1 f1) (Automaton s2 a2 ts2 i2 f2) =
    s1 == s2 &&
    a1 == a2 &&
    ts1 == ts2 &&
    i1 == i2 &&
    f1 == f2

r1 = Automaton [0, 1] ['a'] [(0, 'a', 1)] 0 [1]
r2 = Automaton [0] [] [] 0 [0]
r3 = Automaton [0, 1, 2, 3, 4] ['a', 'b', 'c'] 
		[(0, 'a', 1), (1, 'a', 2), (1, 'c', 1),
		(3, 'a', 3)] 0 [1]
r4 = Automaton [0, 1, 2] ['a'] 
		[(0, 'a', 1), (2, 'b', 1)] 0 [1]


removeUselessTests = let a3 = removeUseless a2
    in
    TestList [
        True ~=? eq a3 (Automaton [0] ['a'] [] 0 [0]),
	-- test for an automaton that is fully useful
	True ~=? eq r1 (removeUseless r1),
	-- test for an empty automaton
	True ~=? eq r2 (removeUseless r2),
	-- test for removing states and transations
	-- while others keeps the same
	True ~=? eq (Automaton [0, 1] ['a', 'b', 'c'] 
	[(0, 'a', 1), (1, 'c', 1)]
	0 [1]) (removeUseless r3),
	-- test for not in the alpha
	True ~=? eq (Automaton [0, 1] ['a'] [(0, 'a', 1)]
		0 [1]) (removeUseless r4)
        ]

isFiniteLanguageTests = TestList [
    True ~=? isFiniteLanguage a2,
    -- test for empty automaton
    True ~=? isFiniteLanguage r2,
    -- test for usefull automaton
    True ~=? isFiniteLanguage r1,
    -- test for a not fully usefull automaton
    True ~=? isFiniteLanguage r4,
    False ~=? isFiniteLanguage r3,
    -- before or after removeUesless should not matter
    isFiniteLanguage l1 ~=? isFiniteLanguage (removeUseless l1),
    isFiniteLanguage l2 ~=? isFiniteLanguage (removeUseless l2),
    isFiniteLanguage r1 ~=? isFiniteLanguage (removeUseless r1),
    isFiniteLanguage r2 ~=? isFiniteLanguage (removeUseless r2),
    isFiniteLanguage r3 ~=? isFiniteLanguage (removeUseless r3),
    isFiniteLanguage r4 ~=? isFiniteLanguage (removeUseless r4)
    ]


language'Tests = TestList [
    [""] ~=? language' a2,
    -- test for the empty automaton
    [""] ~=?  language' r2,
    -- the finite automaton will not
    -- be influenced by removeUseless
    language' r1 ~=? language' (removeUseless r1),
    language' r4 ~=? language' (removeUseless r4),
    -- test order
    ["", "a", "b", "aa", "ab", "ba", "bb"] ~=? take 7 (language' l2),
    -- when the automaton is infinite 
    -- language do the same thing as language'
    take 7 (language' r3) ~=? take 7 (language r3)
    ]

a3 = Automaton [0,1,2] ['a','b'] [(0,' ',2),(0,'a',1),(2,'b',0)] 0 [1]

e1 = Automaton [1] ['a'] [(1, ' ', 1)] 1 [1]
e2 = Automaton [1, 2] ['a'] [(1, ' ', 1), (1, ' ', 2)] 1 [1]
e3 = Automaton [1, 2, 3, 4] ['a'] 
		[(1, ' ', 1), (1, ' ', 4), (1, ' ', 2), (1, ' ', 3)] 1 [1]
e4 = Automaton [1, 2, 3, 4] ['a'] 
		[(1, ' ', 2), (2, ' ', 3), (3, ' ', 4), (4, ' ', 1)] 1 [1]
e5 = Automaton [1, 2, 3, 4] ['a'] 
		[(1, ' ', 2), (2, ' ', 2), (3, ' ', 4), (4, ' ', 3)] 1 [1]

epsilonClosureTests = TestList [
    [0,2] ~=? epsilonClosure a3 [0],
    -- test for empty automaton
    [0] ~=? epsilonClosure r2 [0],
    -- test for termination
    [1] ~=? epsilonClosure e1 [1],
    [1, 2] ~=? epsilonClosure e2 [1],
    -- test for order
    [1, 2, 3, 4] ~=? epsilonClosure e3 [1],
    [1, 2, 3, 4] ~=? epsilonClosure e4 [2],
    -- test for duplicate
    [1, 2, 3, 4] ~=? epsilonClosure e3 [1, 1],
    [1, 2, 3, 4] ~=? epsilonClosure e3 [1, 2, 3],
    [1, 2, 3, 4] ~=? epsilonClosure e3 [1, 2, 2],
    -- order in the second input does not matter
    epsilonClosure e4 [1, 4] ~=? epsilonClosure e4 [4, 1],
    epsilonClosure e5 [1, 4] ~=? epsilonClosure e5 [4, 1]
    ]

main :: IO ()
main = do
    -- Put each call to "runTestTT" on a separate line
    runTestTT tableToDeltaTests
    runTestTT extendTests
    runTestTT allStringsTests
    runTestTT possibleOutcomesTests
    runTestTT acceptTests
    runTestTT languageTests
    runTestTT removeUselessTests
    runTestTT isFiniteLanguageTests
    runTestTT language'Tests
    runTestTT epsilonClosureTests
    return () 
