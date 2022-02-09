a <- rfp('2021','11')
am <- toGrid(a)

look <- adja(am, n=8)
dims <- dim(look)

step <- \(b) { # Simulate one step of time
  b <- b+1L # Increase values by 1
  flashAll <- F # Store all the flashes for this step
  flashNew <- b >= 10 # New flashes
  while(any(flashNew)) { # While there is new flashing
    # Look in all directions; Add 1 for every new flash
    b <- b + matrixStats::rowSums2(flashNew[look], na.rm = T, dim. = dims)
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
