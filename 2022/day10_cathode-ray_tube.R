a <- rfp('2022','10') # Read input
m <- matrix(c(rep_len(0L, length(a)), strtoi(substr(a, 6, nchar(a)))), nrow=2, byrow = T) # Init matrix with a 0 before each number
X <- cumsum(c(1L, m[!is.na(m)][-240])) # Cumulative sum of numbers without last value
idx <- 0:5 * 40 + 20 # Indices to check
sum(X[idx] * idx) # Part 1 (13860): Sum of all numbers at specified indexes
draw <- abs(0:(length(X)-1) %% 40 - X) < 2 # When pointer overlap with sprite (3 pixels wide) draw a pixel
dim(draw) <- c(40, 6) # Reshape to 40x6 matrix
# Part 2 (RZHFGJCB): Visible letters in the matrix
# Pretty print for part 2
# library(ggplot2)
# ggplot(as.data.frame(which(t(draw),arr.ind = T)), aes(col, -row)) + geom_tile() + coord_equal() + theme_void()
