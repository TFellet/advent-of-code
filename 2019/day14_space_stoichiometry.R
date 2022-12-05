transpose <- data.table::transpose;fmatch <- fastmatch::fmatch
a <- rfp('2019','14') |> c('1 ORE => 1 ORE') |> stringi::stri_split_fixed(' => ') |> transpose() # Read file into input / output columns
output <- strsplit(a[[2]], ' ', fixed = T) |> transpose() # Split chemicals and quantities in output
nout <- strtoi(output[[1]]) # Convert quantities
vout <- output[[2]]
l <- length(vout)

reacts <- matrix(0L, ncol=l, nrow=l) # Init empty matrix to store reactions
inp <- gsub(',', '', a[[1]], fixed=T) |> strsplit(' ', fixed=T) # Split input into quantities and chemicals
unl <- unlist(inp) # Unlist all inputs
### Fill matrix with chemical quantities ###
# row id = match chemicals into output; col id comes from lengths of input
# Index in matrix is row + col id * nrow
reacts[fmatch(unl[c(F,T)], vout)+(rep(seq_len(l)-1,lengths(inp)/2))*l] <- strtoi(unl[c(T,F)])

computeOre <- \(n) {
  goal <- n*reacts[,vout == 'FUEL']
  while(sum(goal>0)>1) {
    n_react <- ceiling(goal / nout) # Number of times each reaction is needed
    goal <- (reacts %*% n_react)[,1] - (n_react * nout - goal) # New goal is sum of chemicals needed for previous goal - leftover production
  }
  goal[l]
}
ore <- computeOre(1L)
ore # Part 1 (278404): ORE required to create 1 FUEL

# Linear interpolation to estimate how much FUEL can be produced with 'goal_ore' amount of ORE
searchFuel <- \(goal_ore = 1e12) {
  x2 <- goal_ore/ore*2 # Second value to try
  y2 <- computeOre(x2) # FUEL produced by second guess
  slope <-  (y2 - ore) / (x2 - 1L) # Linear slope
  floor((goal_ore - ore + slope) / slope) # Estimation of FUEL created
}
searchFuel() # Part 2 (4436981): How many FUEL can be created with 1 trillion ORE
