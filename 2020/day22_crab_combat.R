library(collections)
a <- rfp('2020','22')

sep <- collapse::whichv(a, '') # Separation between player's decks
d1 <- strtoi(a[2L:(sep-1L)]) # Player's 1 deck
d2 <- strtoi(a[(sep+2L):length(a)]) # Player's 2 deck

# Play a game without any recursion (vectorized version)
gameP1 <- \(p1,p2) {
  ml <- length(p1) # Length to compare
  while (ml > 0L) { # While the smallest deck is not empty
    l <- 1:ml # Range of cards to test
    w <- p1[l]>p2[l] # Winners for all turns
    p1_tmp <- c(p1[-l], rbind(p1[l][w], p2[l][w])) # Remaining deck + cards not lost + cards won from other player
    p2 <- c(p2[-l], rbind(p2[l][!w], p1[l][!w])) # Same for player 2
    p1 <- p1_tmp
    ml <- min(length(p1), length(p2)) # Smallest deck length
  }
  wdeck <- if(length(p1)>1) p1 else p2 # Find winning deck
  sum(wdeck*(length(wdeck):1)) # Compute deck score
}
gameP1(d1,d2) # Part 1 (32083): Score of winning deck

# Play a game with recursion (no vectorized solution)
gameP2 <- \(p1, p2, first=T) {
  # If we are in a sub game, the winning deck is not important and we can optimise
  # If player 1 has the highest card and no recursion is possible, player 1 will eventually win the sub game:
  # 2 outcomes: Either player 1 win all cards from P2, or there is a loop and P1 is declared winner
  # No recursion possible when the highest card is higher than length of both decks - 2 cards
  if(first==F) { # In a sub game
    mp1 <- max(p1) # Highest P1 card
    mp2 <- max(p2) # Highest P2 card
    if(mp1>mp2 && mp1>(length(p1)+length(p2)-2L)) return(T) # P1 has the highest card and there is no recursion
  }
  mem <- dict() # Dict to store the explored states
  repeat {
    id <- c(p1, -1L, p2) # Unique id of both players decks
    if(mem$has(id)) return(T) # P1 win if the state is already explored
    mem$set(id,T) # Add state to explored states
    n1 <- p1[1]
    n2 <- p2[1]
    recur <- length(p1)>n1 && length(p2)>n2 # Recursion possible
    # Find the winner either with the highest card, or with a sub game
    w <- if(!recur) n1>n2 else Recall(p1[2:(n1+1L)], p2[2:(n2+1L)], F)
    
    if(w) { # P1 win
      p1 <- c(p1[-1L], n1,n2) # P1 new deck
      if(length(p2) == 1L) return(if(first) p1 else T) # If P2 has no more cards, P1 wins
      p2 <- p2[-1L] # P2 new deck
    } else {
      p2 <- c(p2[-1L], n2,n1) # P2 new deck
      if(length(p1) == 1L) return(if(first) p2 else F) # If P1 has no more cards, P2 wins
      p1 <- p1[-1L] # P1 new deck
    }
  }
}
wdeck2 <- gameP2(d1,d2) # Find winning deck
sum(wdeck2*(length(wdeck2):1)) # Part 2 (35495): Score of winning deck
