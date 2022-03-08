library(stringi)
library(data.table)
a <- rfp('2021','17')
xr <- strtoi(stri_match_first_regex(a, 'x=(.\\d+)..(.\\d+)')[,2:3]) # Target X coordinates
yr <- strtoi(stri_match_first_regex(a, 'y=(.\\d+)..(.\\d+)')[,2:3]) # Target Y coordinates
maX <- max(xr)
miX <- min(xr)
miY <- min(yr)
maY <- max(yr)

xvelmin <- which.max(cumsum(1:100)>miX) # Find first valid velocity to reach target
xvelmax <- maX # Last valid x velocity to reach target
xrange <- xvelmin:xvelmax

yrange <- miY:(-miY) # Valids y velocities to reach target
ranges <- CJ(xvel=xrange, yvel=yrange, sorted=F) # Generate all possible combinations of x and y velocities

xvel <- ranges$xvel
yvel <- ranges$yvel
x <- y <- ym <- collapse::alloc(0L, length(xvel))
hit <- collapse::alloc(F, length(xvel))
for(i in 1:(abs(miY)*2)) { # Max number of turns = 2 * miY
  x <- x+xvel
  y <- y+yvel
  xvel <- xvel-sign(xvel)
  yvel <- yvel-1L
  collapse::setv(ym, y>ym, y)
  hit[x %between% c(miX,maX) & y %between% c(miY,maY)] <- T
}
max(ym[hit]) # 12561
sum(hit) # 3785
