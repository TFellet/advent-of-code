a <- rfp('2019','19')
counter <- 0L
comp <- intcode::intcode(a)
s <- 50
mapBeam <- \(s) {
  map <- matrix(0L, s, s)
  j_min <- 1L
  last_beam <- 1L
  for (i in 0:(s-1L)) {
    in_tract <- F
    j <- j_min-1L
    repeat {
      p <- comp$reset()$run(c(i,j))
      counter <<- counter +1L
      if(p) {
        if(!in_tract) { # Going from outside the tractor beam to inside
          j_min <- j # Remember the index of entering
          last_beam <- j
          j <- max(j+1L, last_beam-1L) # Increase j a lot to go to the end of the beam
        } else { # Staying in the beam
          last_beam <- j
          j <- j+1L
          if(j>=s) {
            map[(j_min+1):s,i+1] <- 1L
            break
          }
        }
        in_tract <- T
      } else if(in_tract) { # Leaving the tractor beam
        map[(j_min+1):(last_beam+1),i+1] <- 1L
        break
      } else { # Never in the beam in this row
        j <- j+1L
        if(j>=s) break
      }
    }
  }
  map
}
map <- mapBeam(50)
counter
sum(map) # Part 1
# image(map, ylim = c(1,0), xlab = "x", ylab = "y")
x <- 50
y <- which.max(map[x,])
while(!comp$reset()$run(c(y+99,x))) {
  x <- x+1L
  while(!comp$reset()$run(c(y,x+99))) {
    y <- y+1L
  }
}
y*1e4+x # Part 2
