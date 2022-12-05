a <- rfp('2019','12') |> stringi::stri_match_all_regex('<x=(-?[0-9]+), y=(-?[0-9]+), z=(-?[0-9]+)>') |> # Read file and extract x,y,z coordinates
  lapply(.subset, -1) |> unlist() |> strtoi() # Remove full text and convert all to integer
l <- length(a)
nm <- l/3 # Number of moons
idx <- collapse::mrtl(matrix(1:l,3)) # Column indices in a 3 * (number of moons) matrix

getStep <- \(a, n) {
  vel <- rep(0L, l) # init velocity at 0
  # Use matrix magic to generate all indices of pairs of moons to compare
  idx1 <- matrix(1:l,nm,byrow=T) |> # Indices of X for all moons, then Y indices, then Z indices
    rep(nm) # Repeat for each moon to get an cross join
  idx2 <- frepEach(1:l,nm) # Coordinates of moon 1 each repeated 4 times, then moon 2, etc.
  # Remove comparison of each moon to itself
  diff <- idx1!=idx2;idx1 <- idx1[diff];idx2 <- idx2[diff]
  # Indices for columns in a 3*(3*nm) matrix
  cm1 <- seq.int(1,(nm-1)*l,3);cm2 <- cm1+1L;cm3 <- cm1+2L
  # d <- c(nm-1,3*nm)
  for (i in 1:n) {
    si <- sign(a[idx1]-a[idx2]) # Compare all pairs of moons, get the sign of result
    vel <- vel + si[cm1]+si[cm2]+si[cm3] # Add all effects for each moon to previous velocity (works only if there is 4 moons)
    # vel <- vel + matrixStats::colSums2(si,dim. = d) # Slow version of previous line but works if nm != 4
    a <- a + vel # Update position based on new velocity
  }
  list(a, vel)
}
getStep(a, 1000) |> lapply(abs) |> lapply(matrixStats::colSums2, dim. = c(3,nm)) |> Reduce(f=`*`) |> sum()

# For a given axis, count the number of steps where all moons rturns to their initial states
countStep <- \(pos) {
  vel <- rep(0L, length(pos)) # Init velocites at 0
  p0 <- pos;v0 <- vel # Keep track of initial state
  count <- 0L # Init count
  c1 <- rep.int(seq_len(nm), nm);c2 <- frepEach(seq_len(nm),nm) # Indices for outer product of matrix a
  diff <- c1!=c2;c1 <- c1[diff];c2 <- c2[diff] # Remove useless indices
  collapse::massign(paste0('cm', 1:3), idx)
  # d <- c(nm-1,nm)
  repeat { 
    si <- sign(pos[c1]-pos[c2]) # Ajustements to velocities
    vel <- vel + si[cm1]+si[cm2]+si[cm3] # Add it to previous velocities (works only if there is 4 moons)
    # vel <- vel + matrixStats::colSums2(si,dim. = d) # Slow version of previous line but works if nm != 4
    pos <- pos + vel # Update positions
    count <- count+1L 
    if(all(pos == p0) && all(vel == v0)) return(count) # When we return to the initial state, return current count
  }
}
gcd <- \(a, b) { while (b != 0) { t=b;b=a%%b;a=t }; a } # Algorithm for gcd
lcm <- \(a,b) (a*b)/gcd(a,b) # Algorithm for lcm

res <- sapply(idx, \(x) countStep(a[x])) # Number of steps for each axis to revert to its initial state
Reduce(lcm, res, init=1L) # Part 2 (326365108375488): Number of steps when the system returns to its initial state
