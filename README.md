# MineSweeper
Mips assembly code used to make Minesweeper

The Software used is MIPSym created by the professor of the course Dr. Doering at CSUEB
http://mipsym.com/

As of recently the link is down to downlaod the software yourself

However the documentation page is up and titled Documentation.html

## How to Play

Goal of minesweeper is to clear up all the and flag all the bombs. Timer starts at your first click and repeat with different levels of diffuclity and random maps every game. 
There's three types of tiles hidden under the top lay: a clear tile, a number tile, or a bomb. A clear tile means their are no adjacent bombs and it is your goal to expose all clear tiles. 
A number tiles their are that many number of bombs adjacent and it is also your goal to expose these tiles. A bomb tile is a bomb, you don't want to expose the bombs but rather flag them.

### Game Setup

The game first prompts the user for a seed. "Enter the seed for random number generator(can not be zero): " Seeds are used for testing and is prefeffered to play as random. Enter any interger for the seed.
Next the prompt is the kind of difficulty level. "Pick a difficulty: Beginner(b), Intermediate(i), Advance(a), Custom(c) " 

Beginner is made up of 9 rows, 9 cols, and 10 bombs 

Intermediate is made up of 16 rows, 16 cols, and 40 bombs

Advance is made up of 16 rows, 30 cols, and 99 bombs

Custom allows the user to select rows from 9-24, cols from 9-79, and bombs from 10-500 as long as the number of bombs are below 50% of the area

### Controls

Left CLick: Left clicks checks if the tile is clear. If the tile is clear, it will clear all adjacent clear tiles until a number tile is showen. If you left click on a number tile then that is all that is releaved.
If you click on a bomb then the game is lost.

Right Click: RIght clicks adds a flag, once a flag is added then the bomb counter decreases. If one flag or more are missed placed then the game continues. However if all flags are correctly then the game is won.

Middle Click: If you middle click a number tile and all of the adjacent bombs are flagged correctly then the all the adjacent number tiles clear up any clear tiles adjacent to them. 
If middle on a number tile and the adjacent flags are not flagged correctly then the game is lost.

## Different States of the Game Explained
The assignment had different goals throughout the semester. Updating the project almost weekly.

Part 1: 
1. complete the random number generator (LFSR PRNG) as detailed in course materials.
2. for this assignment only: ask the user for the Seed value (not zero) for the LFSR
3. ask the user for the game level: beginner, intermediate, advanced or custom
4. use the "card sorting algorithm" discussed in class to populate the screen with mines for the minesweeper game.
5. each time the user types any character after the field is populated, clear the screen and repopulate it continuing with the current seed.

Part 2:
1. For each grid cell on the game board count the number of mines in the surrounding eight cells. If a cell has no neighboring mines, leave it blank.
2. Each keypress after running should generate another board.

Part 3:
1. hide the mines and counts
2. have left clicks that are on the board "clear" the hidden symbol with the hidden information
3. If the hidden cell is blank, then automatically click on all 8 neighboring cells, revealing their contents. (this should be recursive, so that if another blank is found it will cause more revealing)
4. if a left click reveals a mine, then end the game.
5. right clicks should flag the cell without revealing the underlying contents, and decrease the displayed mine count. A right click on a cell already flagged should remove the flag and increase the count.
6. if the displayed mine count goes to zero, check to see if all flags are on mines (not misplaced), if so win! otherwise just keep playing.
7. also keep track of how many cells have been cleared, either to blank or a number, so if that count reaches the total cells minus mines the game will also win.

Part 4: 
1. Add a counter to count down the unmined cells that the player has cleared. If it reachs zero, game is won.
2. check the win status if the number of flags remaining is zero. ( one or more could be in the wrong place!)
3. add the timer operation, displaying the time since the first click every second.
4. Add the split-click or middle click behavior for clearing neighbors around "satisfied" numbers.
5. Add a Win (or Loss) dialog.

Part 5: (Uncompleted)
1. Add file based record keeping for best time for each level Beginner, Intermediate, Advanced.( you may omit custom records)




