library(expm) # Exponents on matrices

a <- strtoi(strsplit((rfp('2021','6')), ',', fixed=T)[[1]]) # Read data
V0 <- c(0L, tabulate(a, 8L)) # Starting fishs
A <- matrix(0L, nrow = 9, ncol = 9) # Create empty matrix
A[seq.int(10,80,10)] <- A[7,1] <- A[9,1] <- 1L # Fill 1 in matrix to represent fishs births
nf <- \(f, d) sum(A %^% d %*% f) # From starting fishs f, compute number after d days
nf(V0, 80) # Part 1
nf(V0, 256) # Part 2 
# 64.4µs
