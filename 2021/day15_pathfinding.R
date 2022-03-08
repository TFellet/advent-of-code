library(fastmatch)
a <- rfp(2021,15)
am <- toGrid(a)

# Duplicate a grid 5 times on the right and down
repGrid <- \(m) {
  full <- rbind(m-1L, m, m+1L, m+2L, m+3L)
  full2 <- cbind(full, full+1L, full+2L, full+3L, full+4L)%%9L
  collapse::setop(full2,'+',1L)
}

### Fast vectorized Dijkstra
findPath <- function(maze) {
  queue <- 1L
  risk_vec <- 0L
  l <- ncol(maze)
  to <- nrow(maze) * ncol(maze) # Destination = last cell
  tbv <- rep(T, length(maze)) # Cells to be visited
  parent <- integer() # Current cells
  look <- adja(maze, 0L) # Find all possibles neighbours of all cells in the matrix
  
  while (!(to %in% parent)) {
    cur_risk <- min(risk_vec) # Minimum risk in queue
    idx <- collapse::whichv(risk_vec, cur_risk) # Indexes of min risks in queue
    parent <- kit::funique(queue[idx]) # Indexes of min values in matrix
    tbv[parent] <- F # Mark parent as visited
    nei <- unlist(look[parent,]) # All neighbour of all values being visited
    nei <- nei[nei>0] # Filter invalid neighbours
    nei <- nei[tbv[nei]] # Keep neighbour to be visited
    risk <- maze[nei] # Risk of all neighbour
    idx2 <- queue %fin% parent # remove explored from queue
    risk_vec <-  c(risk_vec[!idx2], cur_risk + risk) # Risk vector => unused values and visited values + risk of neighbours
    queue <- c(queue[!idx2], nei) # Indexes of values from risk vec
  }
  return(cur_risk)
}

findPath(am)
full <- repGrid(am)
findPath(full)
# Full = 2855 iterations / value of 2876
