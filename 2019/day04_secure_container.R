a <- strsplit(rfp('2019','4'), '-')[[1]] |> strtoi() # Read input as 2 ints
size <- 1e5L
fn <- as.integer(a[[1]] / size) # First digit
ln <- as.integer(a[[2]] / size) # Last digit

dig <- sapply(5:0, \(d) { # For each digit position
  if(d==5) frepEach(fn:ln, size) # For first position, generate only from 1 to 6 (depending on input)
  else frepEach(0L:9L, 10^d) |> rep.int((ln-fn+1L)*10^(4-d)) # Generate a all digits for a position
})[(a[[1]]+1-size):(a[[2]]+1-size),] # Filter on given range

for (i in 1:5) dig <- dig[dig[,i]<=dig[,(i+1)],] # Remove decreasing numbers
diffs1 <- matrixStats::rowDiffs(dig) # Compute consecutives differences
dupl <- matrixStats::rowAnys(diffs1, value=0L) # Find consecutives duplicates
sum(dupl) # # Part 1 (2220): Number of passwords matching conditions

di <- collapse::mctl(diffs1[dupl,] == 0L) # Filter lines with consecutives duplicates and convert to list
f2 <- di[[1]] & !di[[2]] | !di[[4]] & di[[5]] # Double digit at start or end of number
for (i in 2:4) f2 <- f2 | (!di[[(i-1)]] & di[[i]] & !di[[(i+1)]])  # Double digit in middle of number
sum(f2) # Part 2 (1515): Number of passwords with at least one double (and not triple or more) consecutive digit
