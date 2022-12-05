a <- rfp('2019','17')

##### Code logic #####
# There are 3 main steps:
# 1. Get the map from the intcode computer
# 2. Find a path covering all possible points
# 3. Split the path in 3 subroutines

##### Step 1: get the map #####

comp <- intcode::intcode(a, 2) # Init computer with 2x more ram
res <- comp$run() # Get map
chars <- rawToChar(as.raw(res)) # Convert ascii to characters
n_cols <- stringi::stri_locate_first_fixed(chars, '\n')[[1]] # Width of map
map <- stringi::stri_split_fixed(chars, '\n')[[1]] |> toGrid(int = F) # Convert strings to matrix
hashs <- collapse::whichv(map, '#') # Position of points in the map
nei <- adja(map, which.max(map == '.')) # Adjacency matrix
# Find intersections in the map by looking at the neighbours of all points 
# and finding when they have 4 neighbouring points
inter <- (map[nei[hashs,]] == '#') |> matrixStats::rowSums2(dim. = c(length(hashs), 4)) |> collapse::whichv(4L)
coords <- arrayInd(hashs[inter], dim(map)) # Convert array indices to matrix indices
sum((coords[,1]-1L)*(coords[,2]-1L)) # Part 1 (5948): Product of 0-based intersections's coordinates

##### Step 2: Find the path #####

findPath <- \(map) {
  # Find where to turn to find next #
  findTurn <- \(pos, dir) mod(match('#', `[[<-`(map[nei[pos,]], prevs[dir], '.')) - dir, 4L)
  
  mod <- \(x, y) (x-1L) %% y + 1L # Modulo with base 1 indexing
  prevs <- c(3L, 4L, 1L, 2L) # Direction of previous # depending on current direction
  
  dirs <- c('^', '>', 'v', '<') # Possible directions: 1 is up, 2 right, etc.
  directions <- c('R', NA, 'L') # Possible turning operations: 1 is right, 3 is left (no other possible results)
  path <- vector('character', 100L) # Empty path
  pos <- which.max(map %in% dirs) # Starting position as vector index
  pos_arr <- arrayInd(pos, dim(map))[1,] # Starting position as (x, y) pair
  dir <- which.max(map[pos] == dirs) # Current direction
  counter <- 0L # Current path index
  repeat {
    pos <- pos_arr[1] + (pos_arr[2]-1L)*nrow(map) # Update vector position
    turn <- findTurn(pos, dir) # Find next turn to make
    if(is.na(turn)) break # Exit when path ends
    dir <- mod(dir + turn, 4) # Change current direction
    line <- (switch(dir, # Get values the robot is facing depending on his direction
      map[(pos_arr[1]-1L):1,        pos_arr[2]],               # Values up
      map[pos_arr[1],              (pos_arr[2]+1L):ncol(map)], # Values right
      map[(pos_arr[1]+1L):nrow(map),pos_arr[2]],               # Values down
      map[pos_arr[1],              (pos_arr[2]-1L):1])) == '#' # Values left
    len <- if(all(line)) length(line) else which.min(line) - 1L # Number of # in the line
    switch(dir, # Update (x y) position after walking len steps in the current direction
           pos_arr[1] <- pos_arr[1]-len,
           pos_arr[2] <- pos_arr[2]+len,
           pos_arr[1] <- pos_arr[1]+len,
           pos_arr[2] <- pos_arr[2]-len)
    path[counter <- counter + 1L] <- paste0(directions[turn], ',', len) # Add instructions to list
  }
  length(path) <- counter # Reduce list to its length
  paste0(path, collapse = ',') |> paste0(',') # Concat all values
}

path <- findPath(map) # Find path


##### Step 3: Find subroutines #####

findRoutines <- \(path) {
  # Magic regex to find subroutines in main routine:
  # Find pattern 1 as many times as possible, then pattern 2, 
  # then 1 or 2 as many times as possible, and finally find pattern 3
  regex <- r"[^(.{1,20})\1*(.{1,20})(?:\1|\2)*(.{1,20})(?:\1|\2|\3)*$]"
  patterns <- regmatches(path, regexec(regex, path, perl = T, ignore.case = T))[[1]][2:4] # Extract routines from path
    
  main <- stringi::stri_replace_all_fixed(path, patterns, paste0(LETTERS[1:3], ','), vectorize_all = F) # Replace routines by A, B and C
  lines <- c(main, patterns, 'n,') |> lapply(\(x) substr(x, 1, nchar(x)-1)) # Add new line with n as option and remove final comma from all strings
  toAscii <- \(x) unlist(lapply(x, utf8ToInt)) # Vectorized convert to ascii
  routine <- lapply(lines, paste0, '\n', collapse = '') |> toAscii() # Paste all lines together and convert to ascii
}

routine <- findRoutines(path) # Find routines

invisible(comp$reset()) # Reset computer
comp$code[1] <- 2L # Change first parameter
res2 <- comp$run(routine) # Input routines in computer
flast(res2) # Part 2 (997790): Amount of dust collected after executing routines
