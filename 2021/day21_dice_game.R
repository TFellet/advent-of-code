library(matrixStats)
a <- rfp(2021,21)
pos <- strtoi(substr(a, nchar(a), nchar(a)))

# Part 1
s <- matrix(colSums(matrix(rep(1:100, 21),nrow=3)),nrow=2) # Dices rolls for each player
s[,1] <- (s[,1]+pos) # Add initial position of players
s2 <- (rowCumsums(s)-1)%%10+1 # Convert dices rolls to position on board (fom 1 to 10)
s3 <- rowCumsums(s2) # Compute cumulative score of players
w <- which.max(s3>=1000) # Winning index
s3[(w-1)]*w*3 # Score of loosing player * number of dice rolls

# Part 2
library(data.table)
options(scipen = 50)
lim <- 22L # Limit score

dice <- CJ(1:3,1:3,1:3)[,adv := rowSums(.SD)][,.(f=.N),keyby=adv] # All possible rolls of 3 dices with frequency
dice <- dice[,.(p0=1:10,f=f),by=adv] # Generate position 0 from 1 to 10 for each roll
dice[,pn := (adv+p0-1)%%10+1] # Find position aftere the rolls
dice <- dice[,.(f=sum(f)),by=.(p0,pn)] # Sum the frequencies of going from p0 to p1

# dt keeps track of all possibles universes explored and not yet won
# A universe is defined by position of players 1 and 2 (1 to 10), and their current scores (1 to 21)
# To simplify the problem, we count the number of universe in each state instead of simulating them all individually
dt <- data.table(p1=pos[1], p2=pos[2], s1=1, s2=1, n=1) # Initial state
keys <- names(dt)[1:4] # Id of a possible state
setkeyv(dt, keys)

play <- c('1', '2') # Players id
posid <- paste0('p', play) # Variable of player position
posm <- paste0(posid, '==p0') # Join to do on each loop
sco <- paste0('s',play) # Variable of score by player
dtw <- dt[0] # Empty dt which will contain universes won
id <- 2 
repeat {
  # Get variables of current player 
  id <- 3-id
  sid <- sco[id]
  pid <- posid[id]
  
  t <- dt[s1 < lim & s2 < lim] # keep only universes not won
  if(nrow(t) == 0) break # If there are none, exit
  
  dt <- t[dice,on=posm[id],nomatch=0, allow.cartesian=T] # Find the next position of each state after 3 rolls
  dt[,(sid) := get(sid) + pn] # Add new position to score
  dt[,(pid) := pn] # Update position
  dt[,`:=`(n = n*f)] # Count the universes in each state
  dt <- dt[,.(n=sum(n)),keys] # Regroup universes by state
  dtw <- rbind(dtw, dt[s1 >= lim | s2 >= lim]) # Add winning universes to the winning list
}

max(dtw[s1>=22,sum(n)], dtw[s2>=22,sum(n)])
