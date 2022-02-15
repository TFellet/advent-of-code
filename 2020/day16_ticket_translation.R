library(stringi)
library(fastmatch)

a <- rfp('2020','16')
emptys <- collapse::whichv(a, '') # Find empty lines

ranges_raw <- stri_split_fixed(a[1:(emptys[1]-1)], ': ', simplify = T) # Ranges of valid values
ranges <- stri_split_regex(ranges_raw[,2], '( or |-)', simplify = T) |> matToInt() # Integer ranges
ranges_names <- ranges_raw[,1] # Ranges names
tickets <- a[(emptys[2]+2):length(a)] |> toGrid(sep=',') # Others tickets

seq_l <- apply(ranges, 1, \(row) c(row[1]:row[2], row[3]:row[4]),simplify = F) # List of valid sequences

seqs <- unlist(seq_l) # All valid values
invalids <- !(tickets %fin% seqs) # Invalid value for any fields
sum(tickets[invalids]) # Part 1 (27850): Sum of invalid values

tickets_valids <- tickets[matrixStats::rowAlls(invalids, dim. = dim(tickets), value=F),] # Remove invalid tickets
d <- dim(tickets_valids) # Remember dimensions for future stats

compatibles <- t(sapply(seq_l, \(s) { # For each range of valid values 
  in_s <- tickets_valids %fin% s # Find all compatibles values
  matrixStats::colAlls(in_s, dim. = d) # Valid field only if all tickets are valid
}))
# Rows = For a sequence, which field is compatible
# Cols = For a field, which sequence is compatible

cs <- matrixStats::colSums2(compatibles) # For each field, how many sequences are compatibles
rs <- nrow(ranges_raw)+1L-matrixStats::rowSums2(compatibles) # For each sequence, how many fields are INcompatibles
# Field with 1 compatible sequence must match the sequence with 20 compatibles fields
# Field with 2 compatible sequence must match the sequence with 19 compatibles fields ...
ranges_ordered <- ranges_names[fmatch(cs, rs)] # Match fields with sequences
departures_fields <- which(substr(ranges_ordered, 1, 4) == 'depa') # Find departure fields
my_ticket <- strsplit(a[(emptys[1]+2):(emptys[2]-1)], ',',fixed=T)[[1]] |> strtoi() # Parse own ticket
prod(my_ticket[departures_fields]) # Part 2 (491924517533): Product of departure fields in my ticket
