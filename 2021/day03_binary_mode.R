library(matrixStats)

mc <- \(x) as.integer(.5 + mean(x)) # Mots common bit of a vector
lc <- \(x) 1-mc(x) # Find the least common bit of a vector
cv <- \(x) sum(2^((length(x)-1):0)*x) # Convert binary to base 10

# Read data
a <- rfp(2021,3) # Read input as text
am <- t(matrix(strtoi(unlist(strsplit(a, ""))),nrow=nchar(a[1]))) # Split text into integer matrix

# Part 1
x<-floor(colMeans2(am)+.5);cv(x)*cv(1L-x) # 40 µs # Find the most common bit on each column

# Part 2
y<-z<-rep_len(T,nrow(am)) # Filter on  lines for least / most common bits
for (i in 1:ncol(am)) { # For each column
  # If there is more than 1 number, update filter with lines containing the most / least common bit of the column
  y <- (am[,i]==mc(am[y,i])) & y 
  if(sum(z) > 1) z <- (am[,i]==lc(am[z,i])) & z
}
cv(am[y])*cv(am[z]) 
# 440 µs
