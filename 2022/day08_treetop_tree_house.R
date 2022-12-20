a <- rfp('2022','8') |> toGrid() # Read input as grid
visible <- array(F, dim = dim(a)) # Which trees are visible from the sides
visible[1,] <- visible[,1] <- visible[nrow(visible),] <- visible[,ncol(visible)] <- T # All trees on the border are visible
# A tree is visible from a side iif all trees before it are smaller
# cummax keep track of the tallest trees, diffs tells us where a new max happens (thus the tree is visible)
visible[-1,] <- visible[-1,] | (matrixStats::colCummaxs(a) |> matrixStats::colDiffs() != 0) # Trees visibles from the top
visible[,-1] <- visible[,-1] | (matrixStats::rowCummaxs(a) |> matrixStats::rowDiffs() != 0) # Trees visibles from the left
visible[(nrow(visible)-1):1,] <- visible[(nrow(visible)-1):1,] | (matrixStats::colCummaxs(a[nrow(a):1,]) |> matrixStats::colDiffs() != 0) # Trees visibles from the bottom
visible[,(ncol(visible)-1):1] <- visible[,(ncol(visible)-1):1] | (matrixStats::rowCummaxs(a[,ncol(a):1]) |> matrixStats::rowDiffs() != 0) # Trees visibles from the right
sum(visible) # Part 1 (1827): Number of visible trees

# Indices of the "inside" trees
ind <- array(seq_along(a), dim = dim(a))[-c(1,nrow(a)),-c(1,nrow(a))]
# Rotate a matrix 90°, 180° and 270°
rotate <- function(x, i) switch(i, t(x[nrow(x):1,]), x[nrow(x):1,ncol(x):1], t(x)[nrow(x):1,])
# Rotate the matrix from 0 to 3 times and look up everytime
vis <- lapply(0:3, \(i) {
  visible <- array(0L, dim=dim(a))
  if(i > 0) a <- rotate(a, i) # Rotate initial matrix
  shift <- 1L # Initial shift
  while(length(ind) > 0 && shift < ncol(a)) { # While there are still trees
    visible[ind] <- visible[ind] + 1L # Add 1 to the visibility score
    ind <- ind[(ind-shift) %% ncol(a) != 1] # Remove indices at the top row
    ind <- ind[a[ind] > a[ind-shift]] # Remove trees where the view is blocked
    shift <- shift + 1L
  }
  if(i > 0) visible <- rotate(visible, 4-i) # Rotate final result
  visible
})
Reduce(`*`, vis) |> max() # Part 2 (335580): Best viewing score
