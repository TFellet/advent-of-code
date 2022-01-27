a <- rfp(2021,25)
am <- t(matrix(unlist(strsplit(a, "")), nrow = nchar(a[1])))

move_e <- move_s <- T
i <- 0L
n <- nrow(am)
l <- length(am)
ec <- which(am=='>') # Find indices of east moving symbols
sc <- which(am=='v') # Find indices of south moving symbols

while(sum(move_e, move_s)>0) {
  i <- i+1L
  # East movements
  dest <- ((ec+n-1L)%%l)+1L # Find east cell with looping to front
  move_e <- am[dest] == '.' # Valid destinations
  am[dest[move_e]] <- '>' # If valid, put symbol in dest
  am[ec[move_e]] <- '.' # And remove symbol from starting cell
  ec[move_e] <- dest[move_e] # Update positions of symbols
  
  # South movements
  dest <- sc+1L - (sc%%n==0) * n
  move_s <- am[dest] == '.'
  am[dest[move_s]] <- 'v'
  am[sc[move_s]] <- '.'
  sc[move_s] <- dest[move_s]
}
i
