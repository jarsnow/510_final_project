-- import for foldM func
import Control.Monad

-- define states
data State = S | Qss | Qos1 | Qos2 | Qu | Qbb1 | Qbb2 | Qbb3 | Qbb4 | Qbbf 
           | Qsb1 | Qsb2 | Qsb3 | Qc1 | Qc2 | Qc3 | Qcsb | Qcu | Qccb 
           | Qcfu | Qmc1 | Qmc2 | Qmcu | Qmcs | Qfben | Qfbew1 | Qfbew2 | Qj 
           deriving (Eq, Show) -- use this to automatically make functions for == and /=, as well as turning the state into a string

-- define the symbols in the language
-- TODO: add the rest of the symbols
data Symbol = Y | B deriving (Eq, Show)

-- define accepting states
mh_accept_func :: State -> Bool
-- `elem` determines if something is in a list
-- could probably be improved, maybe to just only true?
mh_accept_func = (`elem` [S, Qss, Qos1, Qos2, Qu, Qbb1, Qbb2, Qbb3, Qbb4, Qbbf, Qsb1, Qsb2, Qsb3, Qc1, Qc2, Qc3, Qcsb, Qcu, Qccb, Qcfu, Qmc1, Qmc2, Qmcu, Qmcs, Qfben, Qfbew1, Qfbew2, Qj])

-- transition function
-- take in the current state, the input symbol, and return the out states
mh_transition :: State -> Symbol -> [State]
-- TODO: add the rest of these
mh_transition S Y = [Qos1, Qss]
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

-- Function to check if the NFA accepts a string
-- adapted from https://github.com/leonidas/codeblog/blob/master/2011/2011-12-18-haskell-nfa.md 
accept :: NFA q s -> [s] -> Bool
accept nfa input = any (accept_func nfa) (foldM (transition_func nfa) (start_state nfa) input)
