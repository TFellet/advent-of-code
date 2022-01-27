library(data.table)
options(scipen = 50)
a <- rfp(2021,22)
b <- stringr::str_match(a, '^(.*) x=(-?\\d*)\\.\\.(-?\\d*),y=(-?\\d*)\\.\\.(-?\\d*),z=(-?\\d*)\\.\\.(-?\\d*)')
dt <- as.data.table(b[,-1])
setnames(dt, c('act', 'x1', 'x2', 'y1', 'y2', 'z1', 'z2'))
dt <- dt[,lapply(dt, type.convert, as.is=T)] # Convert all str to better type (int or stay str)
dt[,act:=(act=='on')*2-1] # Convert on / off to +1/-1

intersect <- \(cubes, ca) {
  # ca = new cube. Keep only cubes intersecting ca
  # Find intersect part, inverse action, and add new cubes to the list
  cubes[!(x1 > ca$x2 | ca$x1 > cubes$x2 | y1 > ca$y2 | ca$y1 > cubes$y2 | z1 > ca$z2 | ca$z1 > cubes$z2),
         .(act = -act, x1 = pmax(x1, ca$x1), x2 = pmin(x2, ca$x2), 
           y1 = pmax(y1, ca$y1), y2 = pmin(y2, ca$y2), z1 = pmax(z1, ca$z1), z2 = pmin(z2, ca$z2))]
}

# Part 1
range <- c(-50, 50)
lines <- max(which(dt$x1%between%range))

cubes <- dt[1] 
for (i in 2:lines) {
  # For each new cube, compute intersection with all previous cubes, and add lines to the list
  cubes <- rbind(cubes, intersect(cubes, dt[i]), dt[i][act == 1])
}
cubes <- cubes[,.(act=sum(act)),by=.(x1,x2,y1,y2,z1,z2)][act != 0] # Remove duplicates
setcolorder(cubes, 'act')
cubes[,sum((x2-x1+1) * (y2-y1+1) * (z2-z1+1) * act)] # Compute volume

# Part 2
for (i in (lines+1):nrow(dt)) {
  cubes <- rbind(cubes, intersect(cubes, dt[i]), dt[i][act == 1])
}

cubes[,sum((x2-x1+1) * (y2-y1+1) * (z2-z1+1) * act)]
