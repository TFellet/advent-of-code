a <- rfp('2019','6') # Read input
l <- strsplit(a, ')', fixed = T) |> data.table::transpose() # Split center planet and orbiting planet in 2 lists
orbits <- fastmatch::fmatch(l[[1]], l[[2]]) # Find center planet of all planets

# Count total number of orbits
countOrbits <- \(orbits) {
  total <- length(orbits) + length(collapse::na_rm(orbits)) # Each planet has 1 orbit and 1 exploration has already been done
  dest <- orbits # Init variable dest
  repeat {
    dest <- collapse::na_rm(orbits[dest]) # Find center planet of all planets and remove planets not orbiting another (COM)
    total <- total + length(dest) # Add to running total of orbits
    if(!length(dest)) return(total) # If there is no planet left, all orbits are accounted for
  }
}
countOrbits(orbits) # Part 1 (204521): Total number of orbits of all planets

# Find shortest path between 2 planets
# Using bidirectional search, the exploration is done simultaneously from both ends, and is done when they meet
findPath <- \(orbits, start = 'YOU', end = 'SAN') {
  dest <- fastmatch::fmatch(c(start, end), l[[2]]) # Starts of exploration
  visited <- rep(NA, length(orbits)) # Count number of steps to get to each planet
  steps <- -1L # The start planets don't count, the next planet will be 0
  repeat {
    visited[dest] <- steps # Save known number of steps
    dest <- orbits[dest] # Move to next planet
    steps <- steps + 1L # increase move count
    v <- visited[dest] # Are destination planets already visited ?
    if(any(!is.na(v))) { # If at least one is visited
      v[is.na(v)] <- steps # Fill NA with current number of steps if necessary
      return(sum(v)) # Returns total number of moves
    }
  }
}
findPath(orbits) # Part 2 (307): Shortest path between 'YOU' and 'SAN'
