a <- brio::read_file(fp('2020', '11')) # Read full file as str
n_col <- (stringi::stri_locate_first_fixed(a, '\n')[[1]]-1L) # Number of columns in grid
a <- stringi::stri_replace_all_fixed(a, '\n', '') # Remove \n
emptys <- stringi::stri_locate_all_fixed(a, '.')[[1]][,1] # Find all positions without seats
empty_bool <- collapse::alloc(T,nchar(a)) # Vector of valid seats
empty_bool[emptys] <- F # Remove invalid
valid <- as.vector(matrix(empty_bool, ncol=n_col, byrow = T)) # Convert indices from (col,row) to (row,col)

b <- matrix(collapse::alloc(0L,nchar(a)), ncol=n_col) # Matrix to be used in solving

# Function to find neighbours indices in 8 directions of all points in matrix
look <- adja(b, pad=which.min(valid), n=8L) # Apply function on matrix, use first invalid seat as padding

# Simulate people movement based on neighbour table and occupation limit
solve <- \(b, look, lim_occup=4L) {
  continue <- T
  dims <- c(length(b),8L) # Initial dimensions of matrix
  while(continue) {
    nei <- b[look] # Find neighbours of all points in matrix
    nei <- matrixStats::rowSums2(nei, dim. = dims) # Sum the number of neighbour presents
    add <- valid & nei == 0L & b == 0L # Where people will be seating
    sub <- valid & nei >= lim_occup & b == 1L # Where people will leave
    continue <- any(add) || any(sub) # If there is movement, continue
    b[add] <- 1L # Add people where needed
    b[sub] <- 0L # Remove people where needed
  }
  sum(b)
}
solve(b, look) # Part 1 (2275): Total of occupied seats

# Function to find neighbours indices in 8 directions of all points in matrix
# If there is no adjacent neighbour, look further until it is found
adjaFar <- \(mat, valid, pad=0L) {
  mat2 <- seq_along(mat) 
  dim(mat2) <- dim(mat)
  mat.pad <- unname(rbind(pad, cbind(pad, mat2, pad), pad)) # Add padding arround matrix
  valid.pad <- unname(rbind(T, cbind(T, matrix(valid, ncol=n_col), T), T)) # Positions where we don't look further
  ind_row <- 2:(nrow(mat) + 1) # row indices of the "middle"
  ind_col <- 2:(ncol(mat) + 1) # column indices of the "middle"
  mat.pad[!valid.pad] <- 0L # Replace invalid seats by 0
  
  dirs_row <- c(-1,-1,0,1,1, 1, 0,-1) # List of all row directions
  dirs_col <- c( 0, 1,1,1,0,-1,-1,-1) # List of all col directions. With dirs_row, it forms a clockwise pattern
  neigh <- vector(mode = "list", length = length(dirs_col))
  
  for (i in seq_along(dirs_row)) { # For each direction
    dr <- dirs_row[i]
    dc <- dirs_col[i]
    dn <- mat.pad[ind_row + dr, ind_col + dc] # Get adjacent neighbour
    
    iter <- 1L
    repeat {
      wdn <- !dn
      dn0 <- which(wdn, arr.ind = T) # Indices with invalid neighbour
      if(!nrow(dn0)) break # Stop when there are no more positions without neighbour
      dn0[,1] <- dn0[,1]+(dr*iter) # Look further row wise
      dn0[,2] <- dn0[,2]+(dc*iter) # Look further column wise
      dn[wdn] <- dn[dn0] # Replace 0s by the next value
      iter <- iter+1L # increase range
    }
    neigh[[i]] <- dn # Add result to list
  }
  neigh2 <- do.call(cbind, neigh) # Combine all results
  dim(neigh2) <- c(length(mat), 8L) # Reshape to correct dimensions
  neigh2
}
lookFar <- adjaFar(b, valid, pad=which.min(valid)) # Find far neighbours

solve(b, lookFar, lim_occup=5L) # Part 2 (2121): Total of occupied seats
