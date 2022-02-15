a <- rfp('2020','14')
masks_pos <- substr(a, 1,2) == 'ma' # Find lines with masks
masks_m <- toGrid(stringi::stri_sub(a[masks_pos], 8)) # Convert all masks to matrix (containing NA)
masks_1 <- collapse::mrtl(collapse::copyv(masks_m, NA, 0L)) # Masks with 0 instead of NA
masks_0 <- collapse::mrtl(collapse::copyv(masks_m, NA, 1L)) # Masks with 1 instead of NA
mem_adr <- strtoi(stringi::stri_match_first_regex(a, '\\[(.*)\\]')[,2]) # Memory adresses
mem_values <- as.double(stringi::stri_match_first_regex(a, '= (.*)')[,2]) |> suppressWarnings() # Values to store in memory

# Convert a vector of values to a matrix of binary lines
numToBin <- \(values, width = 36L) {
  matres <- matrix(F, length(values), width) # Init matrix. 1 Row = 1 number, columns stores bits values
  # Put corresponding bit of each number in correct column
  for (i in seq_len(width)-1) { matres[,ncol(matres)-i] <- values %/% 2^i %% 2 }
  matres
}

# Convert a matrix of binary lines to a vector of decimal numbers
binToNum <- \(mat) {
  res <- rep(0, nrow(mat))
  # Add each power of 2 in each digit
  for (i in 0:(ncol(mat)-1)) { res <- res + (2^i * mat[,ncol(mat)-i]) }
  res
}

# Convert values to binary matrix then to list
mem_values_bin <- mem_values
mem_values_bin[!masks_pos] <- collapse::mrtl(numToBin(mem_values[!masks_pos]))
# Convert memory adress to binary matrix then to list
mem_adr_bin <- mem_adr
mem_adr_bin[!masks_pos] <- collapse::mrtl(numToBin(mem_adr[!masks_pos]))

di <- collections::dict()
mid <- 0L
for (i in seq_along(a)) {
  # If line is a mask, copy specific values and skip to next iteration
  if(masks_pos[i]) {mid <- mid+1L;mask_1 <- masks_1[[mid]]; mask_0 <- masks_0[[mid]];next}
  # Modify value with masks and store to memory
  di$set(mem_adr[i],mem_values_bin[[i]] & mask_0 | mask_1)
}

resn <- binToNum(collapse::qM(data.table::transpose(di$values()))) # Convert binary numbers to decimal
sum(resn) # Part 1 (7997531787333): Sum of values in memory

toDec <- \(s) sum(s*2^seq(length(s)-1,0)) # Convert single number to decimal

wna <- is.na(masks_m) # X positions (represented by NA)
wnal <- collapse::mrtl(wna) |> lapply(which) # X positions by line
sna <- rowSums(wna) # Number of X in each mask
perms <- numToBin(0:(2^9), width = 9L) # All possible combinations of 0/1 to replace X 

# For a given mask, NA positions, and number of NA, return possibles decimal values to replace X
findPoss <- \(mask, wna1, sna1) {
  perm1 <- perms[1:(2^sna1),ncol(perms)-(sna1:1)+1] # Get subset of matrix of combinations
  mult <- 2^(length(mask)-which(wna1)) # Possible decimals values
  as.vector(mult %*% t(perm1)) # All possible combinations of decimals values
}

poss_vals <- mapply(findPoss, masks_1, collapse::mrtl(wna), sna) # Find all decimals values of all masks

memory <- vector('numeric', 1e5) # Init memory
values <- vector('numeric', 1e5)
mid <- 0L
idmem <- 1L
for (i in seq_along(a)) {
  # If line is a mask, copy specific values and skip to next iteration
  if(masks_pos[i]) {mid <- mid+1L;mask <- masks_1[[mid]];pval <- poss_vals[[mid]];na_pos <- wnal[[mid]];next}
  adr <- mem_adr_bin[[i]]
  val <- mem_values[[i]]
  adr[na_pos] <- 0L # Put 0 at X values to get minimal possible adress
  mask_adr <- toDec(mask | adr) # Apply mask and convert to decimal
  adrs <- mask_adr + pval # Add all possible adresses to masked adress
  new_vals <- length(adrs)
  ids_mem <- idmem : (idmem + new_vals - 1)
  memory[ids_mem] <- adrs
  values[ids_mem] <- val
  idmem <- idmem + new_vals
}
# Find last value for each memory adress
sum(values[!kit::fduplicated(memory, fromLast = T)]) # Part 2 (3564822193820): Sum of all memory values
