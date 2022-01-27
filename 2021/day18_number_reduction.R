library(parallel)
library(data.table)
a <- rfp(2021,18)

fl <- \(s,wp) {i <- which(s[1:wp]); if(length(i)>0) max(i) else 0} # Find first number on the left
fr <- \(s,wp) {i <- match(T, s[(wp+5):length(s)]);if(!is.na(i)) i+wp+4 else 0} # Find first number on the right
par <- \(s) cumsum(s==-1L)-cumsum(s==-2L) # Count of parenthesis

# Explode pair to ajdacent numbers
expl <- \(s, wp) {
  sn <- s >= 0L
  il <- fl(sn, wp) # Left number
  ir <- fr(sn, wp) # Right number
  if(il != 0) s[il] <- s[il]+s[wp+1] # Add first number of pair to left number
  if(ir != 0) s[ir] <- s[ir]+s[wp+3] # Add second number of pair to right number
  idx1 <- 1:(wp-1) # Data before pair
  idx2 <- (wp+5):length(s) # Data after pair
  p <- attr(s,'par') # Get count of parenthesis
  s <- c(s[idx1], 0L, s[idx2]) # Return new array without exploded pair
  attr(s,'par') <- c(p[idx1], 4L, p[idx2]) # Return new count of parenthesis
  s
}

# Split number
spl <- \(s) {
  wm <- which.max(s>=10) # Find first number above 10
  n <- s[wm] 
  idx1 <- 1:(wm-1) # Data before pair
  idx2 <- (wm+1):length(s) # Data after pair
  p <- attr(s,'par') # Get count of parenthesis
  s <- c(s[idx1], -1L, floor(n/2), -3L, ceiling(n/2), -2L, s[idx2]) # Return new pair with number divided
  pr <- p[max(idx1)] # Number of parenthesis before pair
  attr(s,'par') <- c(p[idx1], rep(pr+1L,4L), pr, p[idx2]) # New count of parenthesis
  s
}

# Reduce function
red <- \(s, wp) {
  n <- 3*s[wp+1]+2*s[wp+3] # Compute "score" of the pair
  c(s[1:(wp-1)], n, s[(wp+5):(length(s))]) # Return new array with score of the pair
}

# Add 2 arrays together
add <- \(s1, s2) {
  s <- c(-1L, s1, -3L, s2, -2L) # Concatenate the 2 arrays
  p <- par(s) # Count parenthesis
  attr(s, 'par') <- p
  repeat {
    p <- attr(s, 'par')
    if (max(p) >= 5) {
      s <- expl(s, which.max(p)) # Explode pair if it's inside 5 parenthesis
    } else if (max(s)>=10) { 
      s <- spl(s) # Split pair if it's above 10
    } else break # Nothing to do, the 2 arrays have been added
  }
  s
}

# Compute magnitude of a string
magn <- \(s) {
  while(length(s) > 4) { # While there is more than 1 number
    p <- par(s)
    wp <- which.max(p)
    s <- red(s, wp) # Compute score of pair
  }
  s[2]
}
b <- lapply(a, \(s) {
  s <- strsplit(s, '')[[1]]
  suppressWarnings(fcase(s=='[', -1L, s==']',-2L,s==',',-3L,s!='œ',as.integer(s)))
})

# Part 1
res <- Reduce(add, b)
magn(res)

# Part 2
mad <- \(n1,n2) magn(add(b[[n1]],b[[n2]]))
nums <- CJ(seq_along(b),seq_along(b),sorted=F)
res <- mcmapply(mad, nums$V1, nums$V2, mc.cores = 11L)
max(res)
