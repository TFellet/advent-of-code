a <- rfp('2019','3') |> stringi::stri_split_fixed(',') # Read file and separate each entry
m <- as.integer(2^16) # Use 16 bits to store the x coordinate and 16 bits for y

# From segments, generates all points composing it
fillPoints <- \(w) {
  points <- vector('list', length(w))
  idx <- posx <- posy <- 0L # Start position
  dir <- stringi::stri_sub(w, 1, 1) # List of directions
  len <- stringi::stri_sub(w, 2) |> strtoi() # List of lengths
  for (i in seq_along(w)) { # Store segment position and update current location
    points[[idx <- idx+1L]] <- 
      switch(dir[i],
             'R' = m*((posx+1L):(posx <- posx + len[i])) + posy,
             'L' = m*((posx-1L):(posx <- posx - len[i])) + posy,
             'U' = m*posx + ((posy+1L):(posy <- posy + len[i])),
             'D' = m*posx + ((posy-1L):(posy <- posy - len[i])))
  }
  unlist(points)
}

ap1 <- fillPoints(a[[1]])
ap2 <- fillPoints(a[[2]])

fmatch <- fastmatch::fmatch
cross <- ap1[fmatch(ap2, ap1, 0L)] # Find points in common between wire 1 and 2
mdist <- \(x) abs(re <- round(x / m)) + abs(x-(re*m)) # Manathan distance
min(mdist(cross)) # Part 1 (2427): Minimum Manhattan distance between origin and intersection
min(fmatch(cross, ap1) + fmatch(cross, ap2)) # Part 2 (27890): Minimum length of combined wires at intersection
