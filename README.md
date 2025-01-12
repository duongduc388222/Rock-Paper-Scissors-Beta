# Rock-Paper-Scissors-Beta
This is my first Machine Learning Project. It's a Rock Paper Scissors Algorithm that tries to learn your pattern and beats you in a game of RPS. It is written in the language Scamper and the code can be run online using https://scamper.cs.grinnell.edu.

How the algorithm works: 
1. Create a log of all the moves the player have made.
2. Calculate the standard deviation of the log.
3. If SD > 1, this means that the player has some bias in their moves, so we would predictthat the player is more likely to play the moves that's been played the MOST.
   If SD <= 1, this measn that the player has no strategy and is playing randomly, so they would try to even the counts of moves out if they feel like they're playing a certain move too much. In this case, we will target the move that is LEAST played so far.
4. Computer plays its own move trying to win. 
