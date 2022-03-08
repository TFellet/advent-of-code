library(data.table)
a <- fread(fp('2021','12'),sep='-',header=F)
a <- rbind(a, a[,.(V2,V1)],use.names=F)[V2 != 'start' & V1 != 'end'] # Duplicate connexions and filter useless entries

lastName <- \(dt) tail(names(dt),1)

# Part 1
b <- a[V1=='start'] # Starting table
setkeyv(b, lastName(b)) # last col as index
paths <- 0L
while(nrow(b) > 0) { # While there is paths to explore
  b <- b[a,nomatch=0,allow.cartesian=TRUE] # Explore all paths (width first)
  setnames(b, paste0('V',1:ncol(b))) # Change names
  setkeyv(b, lastName(b)) # Last col as index
  last <- lastName(b) # Last column
  ends <- b[,..last][[1]] == 'end' # Number of paths ending
  if(sum(ends) > 0) { # If there is any ending paths
    paths <- paths + sum(ends) # Increase counter of paths
    b <- b[!ends] # Remove ending paths from table
  }
  if(ncol(b) > 3) { # After 2 loops 
    filter <- F # By default, keep all lines
    for (v in 2:(ncol(b)-2)) { # For each cave explored
      filter <- filter | (b[,..v][[1]] == b[,..last][[1]]) # Check if cave has already been visited
    }
    filter <- filter & (tolower(b[,..last][[1]]) == b[,..last][[1]]) # Filter only small caves already visited
    b <- b[!filter] # Remove invalid paths
  }
}
paths # Sum of paths found

# Part 2
b <- a[V1=='start']
b[,V0 := 0] # Counter of small caves explored for each line
setcolorder(b, 'V0')
setkeyv(b, lastName(b))
paths <- 0L
while(nrow(b) > 0) { # While there is paths to explore
  b <- b[a,nomatch=0,allow.cartesian=TRUE] # Explore all paths (width first)
  setnames(b, paste0('V',0:(ncol(b)-1))) # Change names
  setkeyv(b, lastName(b)) # Last col as index
  last <- lastName(b) # Last column name
  ends <- b[[last]] == 'end' # Paths ending
  if(sum(ends) > 0) {
    paths <- paths + sum(ends)
    b <- collapse::ss(b, !ends)
    setkeyv(b, lastName(b))
  }
  if(ncol(b) > 4) {
    last_col <- tolower(b[[last]]) # Last column
    filter <- b[,V0] # Init filter with previous value from table
    for (v in 3:(ncol(b)-2)) {
      filter <- filter + (b[[v]] == last_col) # Add number of already visited small caves 
    }
    b[,V0 := filter] # Update filter in table
    b <- b[filter <= 1] # Remove invalid paths
  }
}
paths # 118803
