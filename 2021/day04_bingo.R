library(data.table)
library(matrixStats)

n <- stringi::stri_split_fixed(rfp('2021','4',n=1L), ',')[[1]] |> strtoi() # Bingo rolls
a <- sfp('2021','4', 0L,skip=2)
score <- \(grid, i) sum(grid[grid!=-1]*i)

bingo <- \(a, n) {
  d3 <- length(a)%/%25L # Number of grids
  grids_row <- array(a, dim=c(5,5,d3)) # Convert input to 3d array
  grids_col <- aperm(grids_row, c(2,1,3)) # Transpose array column wise
  score_p1 <- 0L
  loosings <- rep(T, d3)
  
  for (i in n) { # Draw a number
    collapse::setv(grids_row, i, -1L)  # Mark corresponding number in the grids
    collapse::setv(grids_col, i, -1L)
    wins <- c(collapse::whichv(colSums2(grids_row, dim. = c(5,d3*5)),-5L),
              collapse::whichv(colSums2(grids_col, dim. = c(5,d3*5)),-5L)) # Indices of wins
    
    loosings[((wins-1L) %/% 5L + 1L)] <- F # Update loosing boards
    
    if(sum(loosings) == 1L) last <- which.max(loosings) # Remember index of last loosing grid
    if(sum(loosings) == 0L) { # When last grid looses
      gridp2 <- grids_col[,,last] # Get it
      score_p2 <- score(gridp2, i) # Compute score
      break
    }
    
    if(!score_p1 && length(wins)) { # First win
      grid_w <- grids_col[,,((min(wins)-1L) %/% 5L)+1L] # Find winning grid
      score_p1 <- score(grid_w, i) # Score of winning grid
    }
  }
  c(score_p1, score_p2)
}
bingo(a,n)
# 1.97ms
