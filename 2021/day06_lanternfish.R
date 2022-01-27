library(expm) # Exponents on matrices
options(scipen = 50) # Show all digits in final result

a <- scan(fp(2021,6),0L,sep=',',quiet=T) # Read data
V0 <- unname(c(0,table(a), 0, 0, 0)) # Compute frequencies of fish as first state and pad with 0
A <- matrix(0L, nrow = 9, ncol = 9) # Create empty matrix
diag(A[,2:9]) <- A[7,1] <- A[9,1] <- 1L # Fill 1 in matrix to represent fishs births
nf <- \(f, d) sum(A %^% d %*% f) # From starting fishs f, compute number after d days
nf(V0, 80) # 5.15µs
nf(V0, 256) # 5.93µs
