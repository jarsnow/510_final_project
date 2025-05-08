-- import for foldM func
import Control.Monad
import System.IO

-- define states
data State = S | Qss | Qos1 | Qos2 | Qu | Qbb1 | Qbb2 | Qbb3 | Qbb4 | Qbbf 
           | Qsb1 | Qsb2 | Qsb3 | Qsss | Qsfu | Qssu | Qc1 | Qc2 | Qc3 | Qcsb | Qcu | Qcbb 
           | Qcfu | Qmc1 | Qmc2 | Qmcu | Qmcs | Qfben | Qfbew1 | Qfbew2
           deriving (Eq, Show) -- use this to automatically make functions for == and /=, as well as turning the state into a string

-- define the symbols in the language
-- TODO: add the rest of the symbols
data Symbol = B | Y | F | R | C | D | W deriving (Eq, Show)

-- define accepting states
mh_accept_func :: State -> Bool
-- `elem` determines if something is in a list
-- could probably be improved, maybe to just only true?
mh_accept_func = (`elem` [S, Qss, Qos1, Qos2, Qu, Qbb1, Qbb2, Qbb3, Qbb4, Qbbf, Qsb1, Qsb2, Qsb3, Qsss, Qsfu, Qssu, Qc1, Qc2, Qc3, Qcsb, Qcu, Qcbb, Qcfu, Qmc1, Qmc2, Qmcu, Qmcs, Qfben, Qfbew1, Qfbew2])

-- transition function
-- take in the current state, the input symbol, and return the out states
mh_transition :: State -> Symbol -> [State]
-- TODO: add the rest of these
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
    start_state = S,
    accept_func = mh_accept_func,
    transition_func = mh_transition
}

-- function to check if the NFA accepts a string
-- adapted from https://github.com/leonidas/codeblog/blob/master/2011/2011-12-18-haskell-nfa.md 
check_accept :: NFA q s -> [s] -> Bool
check_accept nfa input = any (accept_func nfa) (foldM (transition_func nfa) (start_state nfa) input)

-- function for the user to check an input string
--accept :: NFA q s -> IO ()
accept input_NFA str = do
    let symbol_list = [char_to_symbol x | x <- str]
    if (check_accept input_NFA symbol_list)
        then do
            putStrLn "accept"
        else do
            putStrLn "reject"

-- character to symbol
char_to_symbol :: Char -> Symbol
char_to_symbol 'B' = B
char_to_symbol 'Y' = Y
char_to_symbol 'F' = F
char_to_symbol 'R' = R
char_to_symbol 'C' = C
char_to_symbol 'D' = D
char_to_symbol 'W' = W
char_to_symbol _ = error "Invalid character input."
