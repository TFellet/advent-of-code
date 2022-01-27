library(data.table)
library(matrixStats)
n <- scan(fp(2021,4), 0L,sep=',', nlines = 1, quiet=T) # Bingo rolls
# a <- collapse::qM(fread(fp(2021,4),skip=2, blank.lines.skip = T, sep=' ')) # Bingo grids
a <- matrix(scan(fp(2021,4), 0L,skip=2, quiet=T),ncol=5,byrow = T) # Bingo grids

ch <- \(m) { sum(c(rowSums2(m), colSums2(m)) == -5) } # Check if grid has won

p1 <- \(b) {
  for (i in n) { # Draw a number
    b[b==i] <- -1 # Mark corresponding number in the grids
    for (j in seq(1,nrow(b),by=5)) { # For the first line of each grid 
      m <- b[j:(j+4),] # Get grid
      if(ch(m)) return(sum(m[m!=-1])*i) # If grid won, return score
    }
  }
}
p1(a) # 14.68 ms

p2 <- \(b) {
  w <- seq(1,nrow(b),by=5) # Id of first line of each grid
  wb <- rep_len(T,length(w)) # Boolean if grid is still loosing
  for (i in n) { # Draw a number
    b[b==i] <- -1 # Mark corresponding number in the grids
    for (j in w[wb]) { # For each grid still loosing
      m <- b[j:(j+4),] # Get grid
      if(ch(m)) wb[((j+4)/5)] <- F # If grid won, remove from loosing list
      if(sum(wb) == 0) return(sum(m[m!=-1])*i) # If all the grids are loosing, return score of the last one
    }
  }
}
p2(a) # 34.1 ms
