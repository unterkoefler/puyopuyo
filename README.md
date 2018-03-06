# puyopuyo
Classic Puyo-Puyo Tetris-like game


About:

This project contains the file puyo.rkt which is an implementation of the classic Sega game Puyo-Puyo in the Intermediate Student Language of Racket, using the images and universe libraries. To play, you must download and install DrRacket from https://racket-lang.org/
Game instructions are below. Feel free to comment or contribute with suggestions, improvements and feedback. Enjoy!

Game Instructions:

Starting the Game:
1. Run the program
2. Enter the following into the interactions window: (play 0)
3. To play again, enter (play 0) or use the shortcut ESC p

Objective: The objective of the game is to score the most possible points by creating blocks and chains of like-colored puyos. A block is 4 or more horizontally or vertically adjacent puyos, and a chain occurs when the removal of one block causes another block to form. Chains get you the most points!

Game-play: (Controls are very similar to Tetris)
1. Use the directional arrows to move the pair of puyos to the left or right
2. Use the down arrow to drop the puyos as far as they will go
3. Use the space bar to rotate the pair

Scoring: Each puyo in the block earns 10 points, times the chain multiplier, which increases as more and more blocks are formed in a chain reaction.

Ending the game: The game ends when a Puyo is stuck in the top row of the grid, in the initial starting column. You can also end the game by closing the window.

For more information, visit https://en.wikipedia.org/wiki/Puyo_Puyo_Tetris


Modifications:
1. Changing colors: To change colors, simply change one or more of the 4 colors listed in the random-color function
2. Changing difficulty: To make the game more difficult, decrease the constant PLAY-SPEED to some decimal value less than 1. The smaller, the faster.
3. Resizing the screen: Adjust the constant CELL-SIZE according to your preference


Features to be added soon (hopefully):
1. Play/pause button
2. Some way to store high scores between games
3. Restart button
4. More efficient searching for blocks
5. Preview window of on-deck payoff
