a <- rfp('2020','3')
r <- c(3L,1L,5L,7L,1L) # Right moves
d <- c(1L,1L,1L,1L,2L) # Down moves
nc <- nchar(a[1])

ntrees <- \(r, d) {
  coor_x <- (seq.int(0L, r * length(a) - 1L, by = r) %% nc)+1L # List of x coordinates
  coor_y <- seq.int(1L, length(a), by = d) # List of y coordinates
  sum(substr(a[coor_y], coor_x, coor_x) == '#') # Check if coordinates == #
}
trees <- mapply(ntrees, r=r, d=d) # Find number of trees on each slope
trees[1] # Part 1 (151): Number of trees on the first slope
prod(trees) # Part 2 (7540141059): Product of the number of trees on all slopes
