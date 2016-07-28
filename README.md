# Modelling Sequential Games using the Selection Monad

[Demo](http://www.sequential-games.online)

###Description
            
####The Selection Monad:

```haskell
type J r x = (x -> r) -> x
```

This site hosts a series of prototypes demonstrating the selection monad's ability to solve sequential games, while exhibiting the feasibility of relying on this algorithm discover optimal moves in real-time in order to provide an "AI" for online competition.  This computation provides a generic interface for information-complete, turn-based games and singleplayer puzzles, given that the user specifies three features about that particular game:

1. A pool of all the possible moves that can be chosen by either player
2. A predicate that articulates the win conditions
3. The maximum number of turns that can occur during play

While this requirement implies that a great number of games can be modelled, the user must know all the moves that can be chosen at any time: this rules out games such as Nim (matchsticks), which offers players the option to remove a varied number of matches only restricted by those available. The river crossing game is another example which cannot be modelled; the maximum number
of turns is undetermined.

###Implementation
The original works that produced this computation provided it in Haskell, leveraging it's lazy evaluation and infinite lists to define complicated structures founded in mathematics. Further examination has revealed that the algorithm supports parallel computations of multiple possible outcomes (another facility greatly simplified by Haskell). Communication between the site and the game logic is handled by Yesod.

Additionally, the competitive games are accompanied with an online multiplayer/chat option, if the AI proves disatisfactory.

