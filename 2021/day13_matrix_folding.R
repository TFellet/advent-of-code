library(data.table)
library(ggplot2)
a <- fread(fp(2021,13), fill=T) # read data with empty line

folds <- a[is.na(V2),tail(V1,-1)] # Fold lines
folds <- strsplit(substr(folds, 12, nchar(folds)), '=') # Split fold lines
a <- a[!is.na(V2),.(x = as.integer(V1)+1L,y = as.integer(V2)+1L)] # Convert values

m <- array(F, c(max(a$y), max(a$x))) # Empty matrix
for (i in 1:nrow(a)) a[i,m[y,x] <<- T] # Assign values
res1 <- NULL
for (i in folds) { # For each fold
  line <- as.integer(i[2])+1L # coordinate of line
  # Take the matrix before the line and superpose it with the other half of the matrix reversed
  if(i[1] == 'y') m <- m[1:(line-1),] | m[nrow(m):(line+1),] 
  else m <- m[,1:(line-1)] | m[,ncol(m):(line+1)]
  if(is.null(res1)) res1 <- sum(m)
}
# Part 1
res1
# Pretty print for part 2
ggplot(as.data.table(which(m,arr.ind = T)), aes(col, -row)) + geom_tile() + coord_equal() + theme_void()
