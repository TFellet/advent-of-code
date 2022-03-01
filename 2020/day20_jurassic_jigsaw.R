library(data.table)

ss <- collapse::ss
a <- sfp('2020','20', '')
tid <- which(a == 'Tile') # Separation between each tile
tiles <- list()
for (i in seq_along(tid)) { # At each separation
  # Convert tiles from strings to matrices
  tiles[[i]] <- a[(tid[i]+2):(tid[i]+11)] |> strsplit('') |> unlist() |> matrix(nrow=10)
}

bord <- tiles |> lapply(\(m) {
  # Generate each possible border from a tile
  list(m[1,], rev(m[1,]), m[,1], rev(m[,1]), m[10,], rev(m[10,]), m[,10], rev(m[,10])) |> 
    lapply(stringi::stri_flatten)
})
possibles_dirs <- c('un', 'uf', 'ln', 'lf', 'dn', 'df', 'rn', 'rf') # Operations to get borders above
tmp <- fDT(bord, N=seq_along(bord)) # Store borders in a data table with index in tiles list
# For each tile, store the 8 possibles borders
dt <- tmp[,.(N = rep(N, each=8), id = as.integer(rep(substr(a[(tid+1)],1,4), each=8)), 
             dir=rep_len(possibles_dirs, nrow(tmp)*8), bord = unlist(bord))]

# Self join dt to find all matching borders
dtj <- dt[dt,on=c('bord'),c(.SD, .(i.N=i.N, i.dir=i.dir, i.id=i.id)), nomatch = 0]
dtj <- ss(dtj, dtj$id!=dtj$i.id) # Remove identical ids

corners <- countFilter(dtj$id, 4L) # Corners have 2 matches + 2 matches flipped = present 4 times
prod(corners) # Part 1 (79412832860579): Product of the 4 corner tiles

# Function to put the desired border of a matrix at the top
getRotation <- \(mat, rot) {
  nr <- nrow(mat)
  nc <- ncol(mat)
  switch (rot,
          'un' = mat,
          'uf' = mat[,nc:1],
          'dn' = mat[nr:1,],
          'df' = mat[nr:1,nc:1],
          'rn' = t(mat)[nc:1,],
          'rf' = t(mat)[nc:1,nr:1],
          'ln' = t(mat),
          'lf' = t(mat)[,nr:1])
}

len <- sqrt(length(tiles)) # Square side
mat <- matrix(0L,len,len) # Matrix to store tiles ids
mat_final <- matrix('', len*8,len*8) # Matrix to store result without edges
starts <- seq.int(1,by=8,length.out=len)
ends <- seq.int(8,by=8,length.out=len)
bmat <- list() # 2D list to store each tile in image

# First corner have 2 borders, down and right
corner_id <- ss(dtj$id, dtj$id %in% corners & dtj$dir %chin% c('dn', 'rn')) |> countFilter(2L)
first <- ss(dtj, dtj$id == corner_id & dtj$dir=='dn') # First border is down (fill order of final matrix)

cur_mat <- tiles[[first$N]] # First tile

##### Assemble final matrix #####
bmat[[1]] <- list()
bmat[[1]][[1]] <- cur_mat
mat[1,1] <- first$id # Store first tile id
mat_final[starts[1]:ends[1],starts[1]:ends[1]] <- cur_mat[2:9,2:9] # Store first tile without borders

for (j in 1:len) { # Columns
  for (i in 1:len) { # Rows
    if(i == 1) { # On first row
      if(j==1) next # Skip first cell
      prev <- bmat[[j-1]][[i]] # Find tile on the left instead of top
      prev_id <- mat[i,j-1]
      prev_bord <- stringi::stri_flatten(prev[,10]) # Adjacent border to match
      cur <- ss(dt, dt$bord == prev_bord & dt$id != prev_id) # Find matching border
      cur_mat <- getRotation(tiles[[cur$N]], cur$dir) |> getRotation('ln') # Get tile with correct rotation
      bmat[[j]] <- list()
    } else {
      prev <- bmat[[j]][[i-1]] # Get tile above current
      prev_id <- mat[i-1,j]
      prev_bord <- stringi::stri_flatten(prev[10,])
      cur <- ss(dt, dt$bord == prev_bord & dt$id != prev_id)
      cur_mat <- getRotation(tiles[[cur$N]], cur$dir)
    }
    # Store necessary informations
    mat[i,j] <- cur$id
    bmat[[j]][[i]] <- cur_mat
    mat_final[starts[i]:ends[i],starts[j]:ends[j]] <- cur_mat[2:9,2:9]
  }
}

##### Find pattern #####
stop <- F # Only a single rotation contains pattern. Stop when it is found
mat_pat <- mat_final=='#' # Convert final matrix to boolean matrix
mat_found <- array(T,dim(mat_pat)) # Matrix to remove found patterns
mat_str <- apply(mat_final, 1, stringi::stri_flatten) # List of strings used to filter rows faster
pattern_str <- toGrid(rfp('2020', '20_pattern')|> stringi::stri_trans_char(' ', '.'),int=F) # Regex pattern
pattern <- pattern_str=='#' # Convert pattern to boolean
s <- sum(pattern) # Goal to find
for (x in possibles_dirs) {
  pattern_reg <- apply(getRotation(pattern_str, x), 1, stringi::stri_flatten) # Rotated pattern regex
  pattern_d <- getRotation(pattern, x) # Rotated boolean pattern
  pat_poss <- rep(T, nrow(mat_pat)-length(pattern_reg)+1) # Possibles lines to try
  for (id in seq_along(pattern_reg)) # For each row in the pattern
    # Remove lines where pattern is not present
    pat_poss <- pat_poss & grepl(pattern_reg[id], mat_str[(0+id):(length(mat_str)-(length(pattern_reg)-id))])
  for (i in which(pat_poss)) {
    cp <- nchar(pattern_reg[1])-1L # Pattern width
    idxi <- i:(i+length(pattern_reg)-1L) # Rows to search (pattern height)
    jbase <- 0L:cp # Possible columns
    for (j in 1:(ncol(mat_pat)-cp)) {
      idxj <- jbase+j # Columns to search
      found <- sum(mat_pat[idxi,idxj] & pattern_d) == s # Try to find pattern
      if(found) {
        mat_found[idxi,idxj] <- !pattern_d # Remove pattern from matrix
        stop <- T # Stop when this rotation is done
      }
    }
  }
  if(stop) break
}

sum(mat_found & mat_pat) # Part 2 (2155): Number of # not in a pattern
