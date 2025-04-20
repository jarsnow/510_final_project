# 510 Final Project
A project by John Rader and Andrew Huang

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

### What does a valid string look like in our language?

