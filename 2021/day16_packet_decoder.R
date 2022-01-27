library(stringi)
a <- strsplit(rfp(2021,16), '')[[1]]
hex <- setNames(c('0000', '0001', '0010', '0011', '0100', '0101', '0110', '0111', '1000', '1001', '1010', '1011', '1100', '1101', '1110', '1111'),
                c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'))

b <- paste0(hex[a], collapse = '') # Convert hexa to binary

dec <- \(x) {s <- as.numeric(strsplit(x,'')[[1]]);sum(s*2^seq(length(s)-1,0))} # Convert part of string to decimal

operator <- \(nums, type) { # Possible operations
  switch (type+1,
          sum(nums), prod(nums), min(nums), max(nums), 0,
          as.integer(nums[1]>nums[2]),
          as.integer(nums[1]<nums[2]),
          as.integer(nums[1]==nums[2]))
}

# Reads a part of a string, and updates current index
readS <- \(x,n) {r <- substr(x, i, (i+n-1L));i <<- i+n;r}

decode <- \(ignored=0L) {
  v <<- v + dec(readS(bits, 3)) # Sum version numbers
  t <- dec(readS(bits, 3)) # Packet type
  if (t==4L) { # Literal packet
    lit <- ''
    go <- '1'
    while (go == '1') { # While continue bit == 1
      go <- readS(bits, 1) # Read continue bit
      lit <- paste0(lit, readS(bits, 4)) # Read 4 bits of literal
    }
    res <- dec(lit) # Return complete literal
  } else { # Operator packet
    lid <- as.integer(readS(bits, 1)) # Type of length
    rloop <- c() # Store recursive results
    if(lid==0L) { # Type 1: Number of bits to read
      len <- dec(readS(bits, 15)) # Number of bits to read
      max_len <- i + len # End of read
      while (i < max_len-1L) { 
        rloop[length(rloop)+1L] <- decode() # Recursive call to find subpackets
      }
    } else { # Type 2: Number of packets
      len <- dec(readS(bits, 11)) # Number of packets to read
      rloop <- vapply(1:len, decode, 0) # Recursive call to find subpackets
    }
    res <- operator(rloop, t) # Apply operator on subpackets
  }
  return(res)
}

exe <- \(bits) {
  # Init variables
  i <- 1L
  v <- 0L
  lit <- 0L
  # Import functions into "exe" environment to access above variables
  environment(readS) <- environment()
  environment(decode) <- environment()
  list(decode(), v) # Return operator results and sum of versions
}
res <- exe(b)

res[[2]] # Part 1
res[[1]] # Part 2
