a <- rfp(2021,20)
clean <- strsplit(a[[1]], '')[[1]] == '#' # Convert # and . to T and F
lo <- nchar(a[3]) # Initial matrix width
lo2 <- lo/2 # Middle of initial matrix
m <- matrix(F, 55*2+lo, 55*2+lo) # Create final matrix with empty space on the sides
mid <- ncol(m)/2 # Middle of final matrix
# Store initial matrix into final matrix
m[(mid-lo2+1):(mid+lo2),(mid-lo2+1):(mid+lo2)] <- matrix(unlist(strsplit(a[3:length(a)], '')),ncol=lo,byrow=T) == '#'
l <- ncol(m)

adja <- \(x,l, pad=0L){
  xl=x%%l;xpl=x+l;xml=x-l;xl1=xl!=1L;xl0=xl!=0L
  r <- c(ifelse(xl1,xml-1L,0L), # NW
    ifelse(xl1,x-1L,0L), # N
    ifelse(xl1,xpl-1L,0L), # NE
    xml, # W
    x, # Same point
    xpl, # E
    ifelse(xl0,xml+1L,0L), # SW
    ifelse(xl0,x+1L,0L), # S
    ifelse(xl0,xpl+1L,0L)) # SE
  r[r<=0|r>l^2] <- pad 
  matrix(r, nrow=9,byrow = T)
}
dec <- 2^(8:0) # Vector to convert binary matrix to decimal with matrix multiplication

look <- adja(seq_along(m), l, pad=1L)

conv <- \(m) {
  adjm <- matrix(m[look],nrow=9) # Replace neighbours by their values
  adjdec <- (dec %*% adjm)[1,] # Convert to decimal
  m2 <- clean[adjdec+1L] |> matrix(l) # Find correct indexes in enhancement
}

# Part 1 & 2
m50 <- m
res1 <- NULL
for (i in 0:49) { # Apply the filter 50 times
  m50 <- conv(m50)
  if(i==1) res1 <- sum(m50)
}
res1
sum(m50)
# 0.47 seconds
