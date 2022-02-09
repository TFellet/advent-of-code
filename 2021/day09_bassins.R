am <- rfp('2021','9') |> toGrid() # Read input into a matrix

look <- adja(am, pad=which.max(am==9L), n=4) # Lookup table of 4 adjacents neighbours. Pad with the index of a 9
neigh <- matrix(am[look], ncol=4L) # Find neighbours of all points
min_id <- am < matrixStats::rowMins(neigh) # Find local minimums
sum(am[min_id]+1) # Part 1: Value of each local minima + 1
dims <- dim(neigh)

mg <- am # Matrix of bassins
mg[which(min_id)] <- (1:sum(min_id)) + 10L # Each lowest point has a unique id above 10
mg9 <- collapse::whichv(mg, 9L, T) # Filter to find values different than 9

while(any(mg < 9L)) { # While all points are not in a bassin or a limit (9)
  neigh <- mg[look] # Find neighbours
  maxs <- matrixStats::rowMaxs(neigh, rows = mg9, dim. = dims) # Max neighbour of each point
  idx <- maxs > 10L # When a bassin is found
  mg[mg9[idx]] <- maxs[idx] # Assign bassin id
}
bas2 <- kit::countOccur(mg[mg>10L]) # Count points in each bassin
prod(kit::topn(bas2$Count, 3L, hasna = F, index = F)) # Product of top 3 bassins
