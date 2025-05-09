-- import for foldM func
import Control.Monad

-- define states
data State = S | Qss | Qos1 | Qos2 | Qu | Qbb1 | Qbb2 | Qbb3 | Qbb4 | Qbbf 
           | Qsb1 | Qsb2 | Qsb3 | Qsss | Qsfu | Qssu | Qc1 | Qc2 | Qc3 | Qcsb | Qcu | Qcbb 
           | Qcfu | Qmc1 | Qmc2 | Qmcu | Qmcs | Qfben | Qfbew1 | Qfbew2
           deriving (Eq, Show) -- use this to automatically make functions for == and /=, as well as turning the state into a string

-- define the symbols in the language
data Symbol = B | Y | F | R | C | D | W deriving (Eq, Show)

-- define start state
mh_start_state :: State
mh_start_state = S

-- define accepting states function to check if a state is in the list of accepting states
mh_accept_func :: State -> Bool
mh_accept_func = (`elem` [S, Qss, Qos1, Qos2, Qu, Qbb1, Qbb2, Qbb3, Qbb4, Qbbf, Qsb3, Qsss, Qsfu, Qssu, Qc1, Qc3, Qcsb, Qcu, Qcbb, Qcfu, Qmcu, Qmcs, Qfben, Qfbew1, Qfbew2])

-- transition function
-- take in the current state, the input symbol, and return the out states
mh_transition :: State -> Symbol -> [State]
mh_transition S Y = [Qos1, Qss]
mh_transition S F = [Qfben, Qfbew1]
mh_transition S R = [Qc1]
mh_transition S B = [Qbb1]
mh_transition Qss Y = [Qos2]
mh_transition Qss B = [Qbb1]
mh_transition Qss R = [Qc1]
mh_transition Qss C = [Qsb1]
mh_transition Qos1 Y = [Qos2]
mh_transition Qos1 B = [Qbb1]
mh_transition Qos1 R = [Qc1]
mh_transition Qos1 C = [Qsb1]
mh_transition Qos2 Y = [Qu]
mh_transition Qos2 B = [Qbb1]
mh_transition Qos2 R = [Qc1]
mh_transition Qos2 C = [Qsb1]
mh_transition Qu B = [Qbb1]
mh_transition Qu R = [Qc1]
mh_transition Qu C = [Qsb1]
mh_transition Qu D = [Qmc1]
mh_transition Qbb1 B = [Qbb2]
mh_transition Qbb1 R = [Qc1]
mh_transition Qbb2 B = [Qbb3]
mh_transition Qbb2 R = [Qc1]
mh_transition Qbb3 B = [Qbb4]
mh_transition Qbb3 R = [Qc1]
mh_transition Qbb4 B = [Qbbf]
mh_transition Qbb4 D = [Qmc1]
mh_transition Qbb4 R = [Qc1]
mh_transition Qbbf C = [Qsb1]
mh_transition Qbbf R = [Qc1]
mh_transition Qsb1 W = [Qsss] 
mh_transition Qsb1 Y = [Qsss]
mh_transition Qsb2 W = [Qsb3]
mh_transition Qsb2 Y = [Qsfu]
mh_transition Qsb3 Y = [Qsfu]
mh_transition Qsss Y = [Qos2]
mh_transition Qsss B = [Qbb1]
mh_transition Qsss R = [Qc1]
mh_transition Qsfu R = [Qc1]
mh_transition Qsfu B = [Qbb1]
mh_transition Qsfu Y = [Qos1]
mh_transition Qc1 W = [Qc2]
mh_transition Qc1 R = [Qcsb]
mh_transition Qc1 Y = [Qcsb]
mh_transition Qc1 F = [Qfben, Qfbew1]
mh_transition Qc2 W = [Qc3]
mh_transition Qc2 R = [Qcu]
mh_transition Qc2 Y = [Qcu]
mh_transition Qc2 F = [Qfben, Qfbew1]
mh_transition Qc3 R = [Qcbb]
mh_transition Qc3 Y = [Qcbb]
mh_transition Qc3 C = [Qmc1]
mh_transition Qc3 F = [Qfben, Qfbew1]
mh_transition Qcsb Y = [Qcfu]
mh_transition Qcsb B = [Qss]
mh_transition Qcsb R = [Qc1]
mh_transition Qcfu Y = [Qos1]
mh_transition Qcfu B = [Qbb1]
mh_transition Qcfu R = [Qc1]
mh_transition Qcfu C = [Qsb1]
mh_transition Qmc1 W = [Qmc2]
mh_transition Qmc1 R = [Qmcu]
mh_transition Qmc1 Y = [Qmcu]
mh_transition Qmc2 R = [Qmcs]
mh_transition Qmc2 Y = [Qmcs]
mh_transition Qmcu Y = [Qos1, Qss]
mh_transition Qmcu B = [Qbb1]
mh_transition Qmcu C = [Qsb1]
mh_transition Qmcs Y = [Qos1, Qss]
mh_transition Qmcs B = [Qbb1]
mh_transition Qmcs C = [Qsb1]
mh_transition Qfben B = [Qbb1]
mh_transition Qfben R = [Qc1]
mh_transition Qfbew1 R = [Qc1]
mh_transition Qfbew1 W = [Qfbew2]
mh_transition Qfbew2 R = [Qc1]
mh_transition Qfbew2 Y = [Qos2]
-- if something is not in the grammar, don't transition to any state
mh_transition _ _ = []

-- generalized NFA definition
data NFA q s = NFA {
    -- remember definition of an NFA
    -- start state
    start_state :: q,
    -- accepting states
    accept_func :: q -> Bool,
    -- and transitions
    transition_func :: q -> s -> [q]
    -- (set of states and set of inputs are defined elsewhere)
}

-- create a specific one for our use for monster hunter wilds
mh_NFA :: NFA State Symbol
mh_NFA = NFA {
    start_state = mh_start_state,
    accept_func = mh_accept_func,
    transition_func = mh_transition
}

findFirstPath :: NFA State Symbol -> [Symbol] -> IO ()
findFirstPath nfa symbols = do
    -- start at S (start state)
    let initialState = start_state nfa
    let initialPath = []
    
    -- get all possible paths through the NFA
    let allPaths = explorePaths nfa initialPath initialState symbols
    
    -- get all paths that are accepting
    -- filter and take only the paths that are accepting
    let acceptingPaths = filter (isAccepting nfa) allPaths
    
    -- print result
    if null acceptingPaths
        then do
            -- no accepting paths, print
            putStrLn "reject"
        else do
            -- since acceptingPaths returns a tuple of path and ending state,
            -- take the first tuple, then take only the transition list
            let (transitions, _) = head acceptingPaths
            -- print output
            putStrLn "accept"
            printTransitions transitions

-- helper function to explore all paths
-- input:
--      NFA
--      current path (represented by the list of tuples of (state, read symbol, next state))
--      the current state
--      the list of symbols left to be read
-- returns:
--      the paths (which are a list of tuples, where each tuple is the ending path and its ending state
explorePaths :: NFA State Symbol -> [(State, Symbol, State)] -> State -> [Symbol] -> [([(State, Symbol, State)], State)]
-- base case:
-- if out of symbols to read, then the path is the remaining path, and ending state is the current state
explorePaths nfa path current [] = [(path, current)]
-- take the next symbol out of the list
-- concat takes a list of lists and turns it into a list
-- get all states reachable from current state (current) with transition symbol (sym)
-- for each possible reachable state, traverse it, then add the new info to the current path
explorePaths nfa path current (sym:syms) = concat [explorePaths nfa (path ++ [(current, sym, next)]) next syms | next <- transition_func nfa current sym]

-- check if a path ends in accepting state
-- input:
--      NFA
--      current path (represented by tuple of a list of (state, read symbol, next state) and ending state)
-- output:
--      is the state in an accepting state or not
isAccepting :: NFA State Symbol -> ([(State, Symbol, State)], State) -> Bool
-- check if final state is accepted by the NFA
isAccepting nfa (_, finalState) = accept_func nfa finalState

-- print transitions in the correct format
-- ex:
-- S B Qbb1
-- Qbb1 R Qc1
printTransitions :: [(State, Symbol, State)] -> IO ()
-- print out each transition in the list (use mapM_ to carry out actions)
printTransitions list = mapM_ printTransition list

-- helper function to print a single transition
printTransition :: (State, Symbol, State) -> IO ()
printTransition (start, sym, end) = putStrLn (show start ++ " " ++ show sym ++ " " ++ show end)

-- accept function as described in assignment
accept :: NFA State Symbol -> String -> IO ()
    -- get map each character from the input to their symbols, and find (then print) the first path
accept nfa str = findFirstPath nfa (map char_to_symbol str)

-- character to symbol
char_to_symbol :: Char -> Symbol
char_to_symbol 'B' = B
char_to_symbol 'Y' = Y
char_to_symbol 'F' = F
char_to_symbol 'R' = R
char_to_symbol 'C' = C
char_to_symbol 'D' = D
char_to_symbol 'T' = W -- map the T, time wait character, to W because I messed up the naming and don't want to change it
char_to_symbol _ = error "Invalid character input."
