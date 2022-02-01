library(matrixStats)

mc <- \(x) as.integer(.5 + mean(x)) # Mots common bit of a vector
lc <- \(x) 1L-mc(x) # Find the least common bit of a vector
cv <- \(x) sum(2^((length(x)-1):0)*x) # Convert binary to base 10

# Read data
a <- brio::read_file(fp('2021','3')) # Read file a single string
n_col <- stringi::stri_locate_first_fixed(a, '\n')[[1]]-1L # Find end of first line 
a2 <- stringi::stri_replace_all_fixed(a, '\n','') # Remove end line
am <- matrix(strtoi(strsplit(a2,'',fixed=T)[[1]]), ncol=n_col, byrow=T) # Matrix from file

# Part 1
x <- floor(colMeans2(am)+.5) # Find the most common bit on each column
cv(x)*cv(1L-x) # Part 1: Most common bit * least common bit # 16.5µs 

# Part 2
y<-z<-collapse::alloc(T,nrow(am)) # Filter on  lines for least / most common bits
for (i in 1:ncol(am)) { # For each column
  # If there is more than 1 number, update filter with lines containing the most / least common bit of the column
  y <- (am[,i]==mc(am[y,i])) & y
  if(sum(z) > 1L) z <- (am[,i]==lc(am[z,i])) & z
}
cv(am[y])*cv(am[z])
# 805µs
