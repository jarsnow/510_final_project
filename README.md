# 510 Final Project
A project by John Rader and Andrew Huang

### Running The Script

```
ghci main.hs
> accept mh_NFA [input_string]
```

### Background

Our project focuses on the game *Monster Hunter Wilds*.
The game tasks players with hunting large monsters with equally large weapons.
Each weapon has its own long list of combo strings, similar to characters in arcade games such as *Mortal Kombat* and *Street Fighter*: the way your character reacts to a given input depends on the input given prior.

Because the similarities between the gameplay in *Monster Hunter Wilds* and Non-deterministic Finite Automata, reason Andrew and I decided it would be fun to model the full combo paths for a weapon in the game.
For the purposes of our project, we will focus on one weapon, the hammer.

### What does a valid string of inputs look like in the game?

Before we discuss the anything related to our language and implementation, it is important to understand the mechanics of the game in order to understand the scope of our project.

As mentioned before, the gameplay (in terms of inputs) is very similar to arcade fighting games.
After the user presses a certain button, the game will check the current state of the character to see if the pressed buttons are inputs to a valid move.

However, in addition to the player's inputs, **time** is also a major factor for the game in deciding what action the character should take.
If the player inputs the buttons to the move too early, the game may not recognize it correctly.
Press the buttons too late, and the character may reset back to a default state before the player's input is handled.
However, if the player presses their buttons with the correct timing, the game can correctly handle input, and the character can perform their moves accordingly.

For the purposes of our implementation, we will assume that the player is playing with correct timing for each input.

### What do our symbols and alphabet look like?

The symbols that make up our language represents a small portion of inputs one would press on an Xbox controller to input moves with the Hammer.
For simplification, most inputs such as rolling, player movement with the left stick, camera movement with the right stick, toggling focus mode on / off, etc. are ignored.
If they are necessary for a certain move, then we assume that the player is in a state for which that move is valid.

For example, the move **Overhead Smash 1** is performed from the neutral state by pressing *B*.
The move, **Side Smash** is performed from the neutral state by pressing *B* *while holding the left stick*.
For the purposes of our representation of the game, these are both represented by a *B* input from the neutral state.
In the game, these moves and the game itself are deterministic, however, for our automata, these moves are represented by the same input, and are non-deterministic.

The remaining symbols that we will care about for input are:

* B - representing the B button input
* Y - representing the Y button input
* F - representing the focus strike input*
* R - representing the right trigger input
* C - representing the double simultaneous Y and B button input
* D - representing the triple simultaneous Y, B, and right trigger input
* T - representing holding a button for a small period of time

\* The input for the focus strike requires you to be in focus mode, which the player is assumed to be in at the time of the input.

### What does a valid string look like in our language?

A valid string in our language consists of any single string of inputs that have an effect on the player's moves.
For example, pressing *Y* while in the state **Big Bang 1** has no effect on the player, as there is no move associated with *Y* and the given move.

### Our Data Structure

```
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
-- many more states below...
```

### Our Grammar and Automaton

[final-project.pdf](https://github.com/user-attachments/files/20129698/final-project.pdf)
