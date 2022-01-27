a <- rfp(2021,15)
am <- toGrid(a)

# Duplicate a grid 5 times on the right and down
repGrid <- \(m) {
  s <- nrow(m); full <- matrix(0L, nrow = s*5,ncol = s*5) # Init variables and matrix
  for (i in 0:4) { for (j in 0:4) full[(i*s+1):((i+1)*s),(j*s+1):((j+1)*s)] <- m+i+j} # Generate duplicates
  full[full >= 10L] <- full[full >= 10L] - 9L;full # Change values above 10
}

### Fast vectorized Dijkstra
findPath <- function(maze) {
  queue <- 1L
  risk_vec <- 0L
  l <- ncol(maze)
  to <- nrow(maze) * ncol(maze) # Destination = last cell
  tbv <- rep(T, length(maze)) # Cells to be visited
  parent <- integer() # Current cells
  
  # Function to find neighbours of a cell
  adja <- \(x,l){xl=x%%l;xpl=x+l;c(ifelse(xl!=1L,x-1L,0L),ifelse(xl!=0L,x+1L,0L),x-l,ifelse(xpl>l^2,0L,xpl))}
  # Find all possibles neighbours of all cells in the matrix
  look <- matrix(adja(seq_along(maze), l), nrow=4,byrow = T)
  
  while (!to %in% parent) {
    cur_risk <- min(risk_vec) # Minimum risk in queue
    idx <- risk_vec == cur_risk # Indexes of min risks in queue
    parent <- unique(queue[idx]) # Indexes of min values in matrix
    tbv[parent] <- F # Mark parent as visited
    nei <- unlist(look[,parent]) # All neighbour of all values being visited
    nei <- nei[nei>0] # Filter invalid neighbours
    nei <- nei[tbv[nei]] # Keep neighbour to be visited
    risk <- maze[nei] # Risk of all neighbour
    idx2 <- queue %in% parent # remove explored from queue
    risk_vec <-  c(risk_vec[!idx2], cur_risk + risk) # Risk vector => unused values and visited values + risk of neighbours
    queue <- c(queue[!idx2], nei) # Indexes of values from risk vec
  }
  return(cur_risk)
}
findPath(am)
full <- repGrid(am)
findPath(full)
# Full = 2855 iterations / value of 2876

