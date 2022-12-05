a <- rfp('2019','15') # Read input code
comp <- intcode::intcode(a) # Init computer
map <- matrix(-1L, nrow = 100, ncol = 100) # Init empty map
pos <- nrow(map)/2 + (ncol(map)/2-1) * nrow(map) # Initial position
map[pos] <- 1L # Initial position is not a wall
insert <- fetch <- 0L # Init variables
mouv <- c(1L, -1L, nrow(map), -nrow(map)) # Effect of mouvement in map
tries <- list(c(1L,3L,4L), c(2L,3L,4L),c(1L,2L,3L),c(1L,2L,4L)) # Possible explorations
li <- vector('list', 2000L) # Empty list to store states and paths explored
valid <- rep(0L, 2000L) # Store valid paths

# Fill in first positions to try
for (i in 1:4) li[[insert <- insert + 1L]] <- list(code = comp$code, path = i, coord = pos+mouv[i])
while(insert > fetch) { # While there are still elements to explore
  elem <- li[[fetch <- fetch + 1L]] # Fetch state and path from list of exploration
  try <- flast(elem$path) # Mouvement to run
  comp$code <- elem$code # Change computer state
  res <- comp$run(try) # Run computer with new state on new mouvement
  map[elem$coord] <- res # Store result in map
  valid[fetch] <- res
  if(res == 1) { # If the cell is empty
    for (j in tries[[try]]) { # Generate next positions to try
      npos <- elem$coord + mouv[j] # New position in map
      # If new position is not explored, add it to list
      if(map[npos] == -1L) li[[insert <- insert + 1L]] <- list(code = comp$code, path = c(elem$path, j), coord = npos)
    }
  } else if(res == 2) path_oxygen <- elem$path # Remember path to oxygen
}
length(path_oxygen) # Part 1 (336): Shortest path from start to oxygen

### With BFS search, every point is obtained in the minimum amount of steps ###
# Shortest path between oxygen and other dead ends is equal to 
# length of path from origin to oxygen
# + length of path from origin to dead end
# - twice the length of the path before the paths splits
li2 <- li[1:insert][valid == 1L] # Remove paths ending in a wall
# Find neighbours of every points in all paths
# A dead end is only connected to 1 other non-wall cell
deadend <- map[adja(map)[sapply(li2, .subset2, 'coord'),]] |> matrixStats::rowSums2(dim.=c(length(li2), 4L)) == 1L
end_paths <- li2[deadend] |> lapply(.subset2, 'path')

l <- length(path_oxygen) # Length of reference path
compare <- \(x) which.min(`length<-`(x, l) == path_oxygen) # Returns number of identical elements in 2 paths
same <- vapply(end_paths, compare, 1L) - 1L # Compare all paths to reference
max(l + lengths(end_paths) - 2*same) # Part 2 (360): Distance between oxygen and furthest point on the map
