a <- strsplit(rfp('2019','2'), ',', fixed=T)[[1]] |> strtoi() # Read input as int
a[2:3] <- c(12L, 2L) # Replace needed values
operations <- as.list(a) # Init operations
operations[2:3] <- c('x', 'y') # Position 2 and 3 are the inputs to modify

# Build expression needed to compute value at adress 0 while computing
# value at adress 0 when inputs 1 and 2 are 12 and 2
pos <- 0L # Init position
repeat {
  opc <- a[pos+1L] # Current operation code
  if(opc == 1L) { op <- `+`; ops <- '+' } # Add
  else if(opc == 2L) { op <- `*`; ops <- '*' } # Multiply
  else break # End of program
  p2 <- pos+2L; p3 <- pos+3L; p4 <- pos+4L
  v1 <- operations[a[p2]+1L] # Get expression for value 1
  v2 <- operations[a[p3]+1L] # Get expression for value 2
  operations[[a[p4]+1L]] <- c('(', v1, ops, v2, ')') # Update expression at destination adress
  a[[a[p4]+1L]] <- op(a[a[p2]+1L], a[a[p3]+1L]) # Apply operation on next 3 parameters
  pos <- p4 # Increase position by 4
}
a[1L] # Part 1 (3706713): Value at adress 0 after running the program

goal <- 19690720L # Goal value
expr <- str2lang(stringi::stri_c(unlist(operations[[1]]), collapse = '')) # Parse expression
y <- rep(0L:99L, 100L) # Generate all possibles y values
x <- frepEach(0L:99L, 100L) # Generate all possibles x values
res <- eval(expr) # Evaluate all possibilities at the same time
# With x and y generated as such, the position of the goal is already equal
# to 100 * noun + verb (with 1 difference from R indexing)
collapse::whichv(res,goal)-1L # Part 2 (8609): Which values x and y produces result 19690720 at adress 0
