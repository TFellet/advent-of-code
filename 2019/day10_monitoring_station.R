a <- rfp('2019','10') |> toGrid(int=F)=='#' # Read input to boolean grid
bases <- which(a, arr.ind = T) # Matrix coordinates of asteroids
coords <- bases |> collapse::mctl() # Convert matrix to lists

# Fast way to get a unique value for each angle, similar to atan2
fang <- \(x,y) {
  rat <- y/x
  x0 <- x<0
  y0 <- y<0
  rat[x0] <- rat[x0] - 1e-5
  rat[y0] <- rat[y0] - 5e-5
  rat
}

countAst <- \(coords) {
  l <- length(coords[[1]]) # Number of asteroids
  d0 <- seq.int(1,l^2,l+1) # Indices where asteroids will be matched with themselves in carthesian join
  gr <- frepEach(1L:l, l)[-d0] # Generate an id for each potential base
  crelx <- (frepEach(coords[[1]], l) - rep(coords[[1]], l)) # Carthesian join on x coordinates
  crely <- (frepEach(coords[[2]], l) - rep(coords[[2]], l)) # Carthesian join on y coordinates
  angles <- fang(crely, crelx)[-d0] # Compute all angles
  collapse::BY(angles, gr, collapse::fnunique, use.g.names = F) # Count unique number of angles by base
}

num_ast <- countAst(coords) # Count the number of asteroids seen by each base
base <- which.max(num_ast) # Selected base
num_ast[base] # Part 1 (309): Base with line of sight to maximum number of asteroids

getNthAst <- \(x,y,coords,n=200) {
  c_rel <- list(coords[[1]]-x, coords[[2]]-y) # Coordinates relative to base
  keep <- !(c_rel[[1]] == 0 & c_rel[[2]] == 0) # Base itself
  c_rel <- list(c_rel[[1]][keep], c_rel[[2]][keep]) # Remove base from coordinates
  ratios <- 360 - (atan2(c_rel[[2]], c_rel[[1]]) * (180/pi)) - 180 # Angles to every asteroids
  ratiosun <- radsort(collapse::funique(ratios)) # Unique sorted ratios
  if(n > length(ratiosun)) return('N too large, not implemented yet')
  lines <- which(ratios == ratiosun[n]) # Find nth angle looked
  cx <- coords[[1]][keep][lines] # X coordinates of asteroids at nth angle
  cy <- coords[[2]][keep][lines] # Y coordinates of asteroids at nth angle
  if(length(lines) == 1) return(100*(cy-1)+cx-1) # If there is only one, return its coordinates
  
  dists <- abs(cy) + abs(cx) # Compute dist to each asteroid
  mdist <- which.min(dists) # Find closest
  100*(cy[mdist]-1)+cx[mdist]-1 # Return its coordinates
}
getNthAst(bases[base,1],bases[base,2], coords) # Part 2 (416): Coordinates of asteroid number n
