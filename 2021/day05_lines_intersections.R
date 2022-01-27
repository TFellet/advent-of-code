library(data.table)
# Read lines, change <- to ',', split on ',', convert to int and store in matrix
am <- gsub(' -> ', ',',rfp(2021,5)) |> toGrid(sep=',')
p1 <- list() # List of points
nd <- rep_len(F, nrow(am))
for (i in 1:nrow(am)) { # For each line
  l <- am[i,] # Get line coordinates
  y <- l[[2]]:l[[4]] # Get Y values
  x <- l[[1]]:l[[3]] # Get X values 
  nd[i] <- length(y) == 1 | length(x) == 1 # Horizontal or vertical line
  
  if (length(y) == 1)  # Vertical line
    y <- rep_len(y, length(x)) # Replicate Y to match X
  else if (length(x) == 1)  # Horizontal line
    x <- rep_len(x, length(y)) # Replicate X to match Y
  
  p1[[i]] <- matrix(c(x,y),ncol=2) # Put matrix of points into list (including diagonal)
}
lp1 <- do.call(rbind, p1[nd]) # Put all points in a matrix
lp2 <- do.call(rbind, p1)
as.data.table(lp1)[,nrow(unique(.SD[duplicated(.SD)]))] # Count number of point duplicated at least once
as.data.table(lp2)[,nrow(unique(.SD[duplicated(.SD)]))] 
# 10.2 ms
