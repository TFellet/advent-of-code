library(stringr)
library(data.table)
a <- rfp(2021,17)
xr <- strtoi(str_match(a, 'x=(.\\d+)..(.\\d+)')[,2:3]) # Target X coordinates
yr <- strtoi(str_match(a, 'y=(.\\d+)..(.\\d+)')[,2:3]) # Target Y coordinates
maX <- max(xr)
miX <- min(xr)
miY <- min(yr)
maY <- max(yr)

xvelmin <- which.max(cumsum(1:100)>miX) # Find first valid velocity to reach target
xvelmax <- maX # Last valid x velocity to reach target
xrange <- xvelmin:xvelmax

yrange <- miY:(-miY) # Valids y velocities to reach target
ranges <- CJ(xvel=xrange, yvel=yrange, sorted=F) # Generate all possible combinations of x and y velocities

dt <- data.table(ranges, x=0L, y=0L, hit=F, ym=0L) # Store all possible trajectories
for(i in 1:(abs(miY)*2)) { # Max number of turns = 2 * miY
  dt[,`:=`(x=x+xvel,y=y+yvel,yvel=yvel-1L,xvel=xvel-sign(xvel))] # Update positions and velocities
  dt[y > ym,ym:=y] # Find max y
  dt[x %between% c(miX,maX) & y %between% c(miY,maY), hit := T] # Compute trajectories in target
}
dt[hit==T,max(ym)] # Part 1
dt[,sum(hit)] # Part 2

