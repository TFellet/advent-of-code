a <- rfp(2021,11)
am <- toGrid(a)

d <- list() # List of functions to look in any direction
d$N <- \(m) rbind(0L, m[1:(nrow(m)-1),]) # Look neighbour North
d$S <- \(m) rbind(m[2:nrow(m),], 0L)     # Look South
d$W <- \(m) cbind(0L, m[,1:(ncol(m)-1)]) # West
d$E <- \(m) cbind(m[,2:ncol(m)], 0L)     # East
d$NE <- \(m) d$N(d$E(m));d$NW <- \(m) d$N(d$W(m));d$SE <- \(m) d$S(d$E(m));d$SW <- \(m) d$S(d$W(m)) # All diagonals

step <- \(b) { # Simulate one step of time
  b <- b+1L # Increase values by 1
  flashAll <- F # Store all the flashes for this step
  flashNew <- b >= 10 # New flashes
  while(sum(flashNew) > 0) { # While there is new flashing
    # Look in all directions; If any neighbour produce a new flash, add 1
    for(fn in d) b <- b + fn(flashNew)
    flashAll <- flashAll | flashNew # Add new flash to list of all flashes for this step
    flashNew <- (b >= 10) & !flashAll # New flash produced if the value is above 10 and it hasn't flashed yet
  }
  b[flashAll] <- 0 # Reset all points that have flashed
  attr(b, 'flashAll') <- sum(flashAll)
  b # Return new state and all flashes
}

# Part 1
b <- am
flashes <- 0L
for (i in 1:100) {
  b <- step(b)
  flashes <- flashes + attr(b, 'flashAll')
}
flashes

# Part 2
repeat {
  i <- i+1L
  b <- step(b)
  if(attr(b, 'flashAll') == length(am)) break
}
i
