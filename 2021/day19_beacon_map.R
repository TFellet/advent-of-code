library(purrr)
library(data.table)
smean <- \(col) col-mean(col) # Function to substract mean
a <- fread(fp(2021,19), fill=T, header=F, sep = '')
a[,scanner := cumsum(grepl('---', a$V1))-1L] # Assign scanner number to lines
b <- a[V1 != '',.SD[-1],by=scanner] # Remove first and last line for each scanner
b[,beacon := 1:.N] # Add unique beacon id
b[,c('x', 'y', 'z') := tstrsplit(V1, ',', type.convert = T)][,V1:=NULL] # Split coordinates into 3 columns
scans <- b[,.(-scanner,0L,0L,0L),by=scanner] # Coordinates of scanners = 0,0,0, beacon_id = -scanner_id
b <- rbind(b, scans, use.names=F) # Add scanners to list of coordinates
b2 <- copy(b) # Working copy of B

matchOne <- \(b2) {
  # Match each beacon to all the others ones in the same scanner
  cross <- merge(b2,b2, by = 'scanner', allow.cartesian = T, suffixes = c('1', '2'))[beacon1 != beacon2]
  cross[,dist := sqrt((x1-x2)^2+(y1-y2)^2+(z1-z2)^2)] # Compute distance between each beacons
  cross[,(6:9):=NULL]
  # All pairs of beacons that are the same distance across scanners
  join <- merge(cross, cross, by='dist',allow.cartesian=T, suffixes = c('1', '2'))[scanner1 != scanner2]
  
  colsby <- c('scanner1', 'beacon11', 'x11','y11','z11', 'scanner2', 'beacon12','x12','y12','z12')
  # Find first match with 12 points. 
  # Take scanner1 = scanner 0 when possible to match all coordinates according to scanner 0
  matches <- join[,.N,by=colsby][N>=11][scanner1 == min(scanner1)][scanner2 == min(scanner2)]
  beacon1 <- matches[,x11:z11]
  beacon2 <- matches[,x12:z12]
  
  # Each column has a different deviation, use it to find beacon's 2 coordinates order in beacon 1
  col_order <- map(beacon1, sd) |> match(map(beacon2, sd))
  setcolorder(beacon2, col_order) # Coordinates with same rotation as beacon 1
  signs <- round(beacon1[,map(.SD, smean)] / beacon2[,map(.SD, smean)]) # Find orientation of each column (-1 or 1)
  beacon2 <- beacon2*signs # Coordinates with same orientation as beacon 1
  translate <- (beacon1-beacon2)[1] |> unlist() # Coordinates difference between beacon 1 and 2
  signs <- unlist(signs[1]) # One copy of signs
  
  b3 <- b2[scanner == matches$scanner2[1] & !beacon%in%matches$beacon12] # Coordinates to modify
  # Update scanner's 2 beacons coordinates according to scanner 1
  # .SDcols is used to take the columns in a different order if necessary
  b3[,c('x','y','z') := pmap(list(.SD, signs, translate), \(c, s, t) c*s+t),.SDcols=(col_order+2L)]
  b3[,scanner := matches$scanner1[1]] # Change scanner 2 to scanner 1
  # New list of point = old list without scanner 2 + new points without duplicates
  rbind(b2[scanner != matches$scanner2[1]], b3)
}

for (i in 2:uniqueN(b$scanner)) { # Match pairs of scanners n-1 times
  b2 <- matchOne(b2)
} #1.5 sec

nrow(b2[beacon>0]) # Part 1
# Part 2
scans <- b2[beacon<=0] # Scanner's coordinates
# Cross join with scans. For each scanner in i, compute the distance with all the other scanners and keep the max
scans[scans,on='scanner',by=.EACHI,max(abs(x-i.x)+abs(y-i.y)+abs(z-i.z))][,max(V1)]
