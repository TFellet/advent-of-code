library(data.table)

a <- sfp('2020','20', '')
tid <- which(a == 'Tile') # Separation between each tile
tiles <- list()
for (i in seq_along(tid)) { # At each separation
  # Convert tiles from strings to matrices
  tiles[[i]] <- a[(tid[i]+2):(tid[i]+11)] |> strsplit('') |> unlist() |> matrix(nrow=10)
}

bord <- tiles |> lapply(\(m) {
  # Generate each possible border from a tile
  list(m[1,], rev(m[1,]), m[,1], rev(m[,1]), m[10,], rev(m[10,]), m[,10], rev(m[,10])) |> lapply(paste0, collapse='')
})
possibles_dirs <- c('un', 'uf', 'ln', 'lf', 'dn', 'df', 'rn', 'rf') # Operations to get borders above
tmp <- data.table(bord)[,N:=.I] # Store borders in a data table with index in tiles list
# For each tile, store the 8 possibles borders
dt <- tmp[,.(N = rep(N, each=8), id = as.integer(rep(substr(a[(tid+1)],1,4), each=8)), 
             dir=rep_len(possibles_dirs, nrow(tmp)*8), bord = unlist(bord))]

# Self join dt to find all matching borders
dtj <- dt[dt,on=.(bord == bord),c(.SD, .(i.N=i.N, i.dir=i.dir, i.id=i.id)), nomatch = 0][id!=i.id]
corners <- dtj[,.N,by=.(id)][N==4] # Corners have 2 matches + 2 matches flipped
prod(corners$id) # Part 1 (79412832860579): Product of the 4 corner tiles

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

setkey(dtj, id)
setkey(dt, bord)
# First tile is the corner having borders down and right without flip
first <- dtj[.(dtj[corners][dir %in% c('dn', 'rn')][,.N,by=.(id)][N==2,id])][dir=='dn']
cur_mat <- tiles[[first$N]] # First tile

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
      prev_bord <- paste0(prev[,10], collapse = '') # Adjacent border to match
      cur <- dt[bord == prev_bord & id != prev_id] # Find matching border
      cur_mat <- getRotation(tiles[[cur$N]], cur$dir) |> getRotation('ln') # Get tile with correct rotation
      bmat[[j]] <- list()
    } else {
      prev <- bmat[[j]][[i-1]] # Get tile above current
      prev_id <- mat[i-1,j]
      prev_bord <- paste0(prev[10,], collapse = '')
      cur <- dt[bord == prev_bord & id != prev_id]
      cur_mat <- getRotation(tiles[[cur$N]], cur$dir)
    }
    # Store necessary informations
    mat[i,j] <- cur$id
    bmat[[j]][[i]] <- cur_mat
    mat_final[starts[i]:ends[i],starts[j]:ends[j]] <- cur_mat[2:9,2:9]
  }
}

mat_pat <- mat_final=='#' # Convert final matrix to boolean matrix
mat_found <- array(T,dim(mat_pat)) # Matrix to remove found patterns
pattern <- toGrid(rfp('2020', '20_pattern'),int=F)=='#' # Convert pattern to boolean
pattern_list <- lapply(possibles_dirs, getRotation, mat=pattern) # Generate every possible pattern
s <- sum(pattern) # Goal to find
stop <- F # Only a single rotation contains pattern. Stop when it is found

for (x in seq_along(pattern_list)) { # For each pattern
  pattern_d <- pattern_list[[x]] # Get pattern with rotation
  rp <- nrow(pattern_d)-1L
  cp <- ncol(pattern_d)-1L
  for (i in 1:(nrow(mat_pat)-rp)) { # Possible rows
    for (j in 1:(ncol(mat_pat)-cp)) { # Possible cols
      idxi <- i:(i+rp) # Rows to search
      idxj <- j:(j+cp) # Columns to search
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
