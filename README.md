# 2048 GAME
This project is a simple implementation of the 2048 Game using Haskell.
The game is played on a 4x4 grid, where the player combines tiles with the same numbers to achieve the tile with the number 2048.

# Features
- Console-based gameplay
- Leaderboard to track high scores
- Multiple players with individual high scores (Use Switch Player feature)
- Move the tiles up, down, left, and right to merge and create higher-value tiles
- Random tile generation after each move
- Hack Leaderboard - Modify Player's Score
- Hack Leaderboard - Remove Player from Leaderboard (Cannot remove Current Player | Need to switch player first)
- Hack Leaderboard - Reset all scores in Leaderboard to 0

# Getting Started
To run the game, you need to have Haskell installed on your system. Follow these steps to get started:

1. Clone the repository:
```shell
git clone https://github.com/jasonyapri/2048-haskell.git
```

2. Navigate to the src project directory:
```shell
cd 2048-haskell
```

3. Build the Project and Run the App
```shell
cabal build
```

```shell
cabal run
```

# Step by Step Walkthrough

1. After running the game, first thing you need to do is enter your name.<br />
![](https://i.ibb.co/JRtXwrg/image.png)
2. Then you will be greeted with the Main Menu (8 options)<br />
![](https://i.ibb.co/QPVxCVN/image.png)
3. Choose your desired action entering the number on the Main Menu<br />
4. Enter '9' to Show How To Play (?)<br />
![](https://i.ibb.co/JyJXDKG/image.png)
5. Enter '1' to Start Playing<br />
![](https://i.ibb.co/m4BQJ8B/image.png)<br />
![](https://i.ibb.co/0JHndn7/image.png)<br />
![](https://i.ibb.co/XWRTDph/image.png)<br />
![](https://i.ibb.co/WfCwJy4/image.png)
- Enter 'w' to move the tile up
- Enter 'a' to move the tile left
- Enter 's' to move the tile down
- Enter 'd' to move the tile right
- Enter 'q' to go back to the main menu<br />
![](https://i.ibb.co/YNSJ1Xd/image.png)

6. Enter '2' to Show Leaderboard<br />
![](https://i.ibb.co/5LJ7Lh1/image.png)

7. Enter '3' to Switch Player<br />
![](https://i.ibb.co/Yf6Tw3B/image.png)

8. Enter '4' to Hack Leaderboard (Modify)<br />
![](https://i.ibb.co/kK2VNFz/image.png)
- We cannot enter a name that doesn't exist in the leaderboard yet.
- Use Switch Player to input a new Player's name<br />
![](https://i.ibb.co/4s104rW/image.png)<br />
![](https://i.ibb.co/nnMpHV4/image.png)

9. Enter '5' to Hack Leaderboard (Remove)<br />
![](https://i.ibb.co/TkDCJ7P/image.png)
- We cannot remove a player that doesn't exist in the leaderboard.
- Use Switch Player to input a new Player's name<br />
![](https://i.ibb.co/K2rDrBd/image.png)
- You cannot remove current Player's score.
- In my case, I cannot delete Stefan's score, as I am playing using Stefan.<br />
![](https://i.ibb.co/VYnr6qd/image.png)<br />
![](https://i.ibb.co/vZdS5Wc/image.png)<br />
- Deleted Jason's score. View the changes by entering '2' (Show Leaderboard)

10. Enter '6' to Hack Leaderboard (Reset)<br />
![](https://i.ibb.co/hD1GDBG/image.png)<br />
![](https://i.ibb.co/N2jt052/image.png)
11. Enter '0' to Exit Game<br />
![](https://i.ibb.co/Zm0jvy0/image.png)

# Created by
Jason Yapri<br />
https://linkedin.com/in/jasonyapri/<br />
https://jasonyapri.com

# Reference
https://play2048.co/<br />
(Created by Gabriele Cirulli)<br />
![2048 Game on Website](https://i.ibb.co/sPHbBqC/image.png)
