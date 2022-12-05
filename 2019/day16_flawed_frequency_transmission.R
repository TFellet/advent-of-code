txt <- rfp('2019','16') # Read input number
a <- txt |> strsplit('', fixed=T) |> .subset2(1) |> strtoi() # Convert to vector of digits
pattern <- c(0L,1L,0L,-1L) # Parttern to repeat

genPattern <- \(n) pattern[1:min((length(a)/n+1L), length(pattern))] |> # Filter input pattern
    frepEach(n) |> rep_len(length(a)+1) |> .subset(-1) # Repeat filtered pattern according to n

m <- t(vapply(seq_along(a), genPattern, seq_along(a), USE.NAMES = F)) # Generate matrix of patterns

res <- a # Init result
for (i in seq_len(100)) res <- abs(m %*% res) %% 10 # Apply transform 100 times
res[1:8,] |> paste0(collapse = '') # Part 1 (44098263): First 8 digits of result after 100 transforms

offset <- txt |> substr(1,7) |> strtoi(base=10) # First 7 digits of input is the offset
# After n/2 digits, the pattern matrix is an upper triangle matrix containing only 1's
# Therefore, the last n/2 digits are easier to compute as they only depend on the previous n/2 digits,
# and they are just a cumulative sum starting from the last digit
res <- rep(a, 1e4L)[(length(a)*1e4L):(offset+1)] # Init result from end to offset
for (i in seq_len(100)) res <- cumsum(res) %% 10L # Repeat cumulative sum 100 times
res[length(res):(length(res)-7)] |> paste0(collapse = '') # Part 2 (12482168): First 8 digits after offset
