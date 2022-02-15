a <- rfp('2020','17') |> toGrid(int=F)=='#' # Read input as boolean grid

# Add empty cells around base grid to fit all iterations 
padGrid <- \(mat, nd = 3L, iter=6L) {
  padl <- iter*2 # Number of padding cells to add in each dimension
  dims <- c(dim(mat), rep(1L, nd-2)) # Initial dimensions 
  b <- array(F, dim = dims+padl) # Final dimensions
  i <- nrow(mat)-1L # Id where to start storing data
  # Write initial grid in middle of final grid
  if (nd == 3) { b[i:(2*i), i:(2*i), i] <- a 
  } else b[i:(2*i), i:(2*i), i, i] <- a
  b
}

# Find all neighbours cells
adjaNd <- \(mat, pad=NA) {
  dim_mat1 <- dim(mat)+1L
  nd <- length(dim_mat1) # Number of dimensions
  n <- 3^nd-1L # Number of neighbours
  mat.pad <- array(pad, dim=dim_mat1+1L) # Pad matrix with parameter
  mid_x <- 2:dim_mat1[1] # Middle cells in each dimension
  mid_y <- 2:dim_mat1[2]
  mid_z <- 2:dim_mat1[3]
  mid_w <- 2:dim_mat1[3]
  # Write matrix in middle of padded matrix
  if(nd==3L) { mat.pad[mid_x,mid_y,mid_z] <- seq_along(mat)
  } else mat.pad[mid_x,mid_y,mid_z,mid_w] <- seq_along(mat)
  
  neigh_l <- vector(mode = "list", length = n+1L)
  # Visit neighbours in each dimensions and add it to list
  i <- 1L
  for (x in -1:1) {
    for (y in -1:1) {
      for (z in -1:1) {
        if(nd==4L) {
          for (w in -1:1) {
            neigh_l[[i]] <- mat.pad[mid_x+x,mid_y+y,mid_z+z,mid_w+w]
            i <- i+1L
          }
        } else {
          neigh_l[[i]] <- mat.pad[mid_x+x,mid_y+y,mid_z+z]
          i <- i+1L
        }
      }
    }
  }
  neigh_l[[n/2+1]] <- NULL # Remove origin point (x/y/z/w == 0)
  neigh <- do.call(cbind, neigh_l) # Combine all results
  dim(neigh) <- c(length(mat), n) # Reshape to correct dimensions
  neigh
}

# Generate padded grids
p1 <- padGrid(a)
p2 <- padGrid(a, 4L)

# Compute lookup tables
look <- adjaNd(p1, pad=1L) # Pad with a FALSE value (slightly faster than NA pad)
look2 <- adjaNd(p2, pad=1L)

for (i in 1:6) {
  neigh_act1 <- matrixStats::rowSums2(p1[look], dim. = dim(look)) # Compute number of active neighbours of each cell
  p1 <- (neigh_act1 == 3) | (p1 & neigh_act1 == 2) # Update grid based on active neighbours
  neigh_act2 <- matrixStats::rowSums2(p2[look2], dim. = dim(look2))
  p2 <- (neigh_act2 == 3) | (p2 & neigh_act2 == 2)
}
sum(p1) # Part 1 (213): Number of active cells in 3D after 6 cycles
sum(p2) # Part 2 (1624): Number of active cells in 4D after 6 cycles
