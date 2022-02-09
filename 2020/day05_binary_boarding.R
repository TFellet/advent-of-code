a <- rfp('2020','5')
b <- stringi::stri_trans_char(a, 'BFRL', '1010') # Convert B/F R/L to 1/0
rows <- substr(b, 1,7) |> strtoi(2) # Convert rows to binary
cols <- substr(b, 8,10) |> strtoi(2) # Convert columns to binary
ids <- rows*8L+cols # Compute seat ids
minid <- min(ids); maxid <- max(ids)
maxid # Part 1: max seat id
sum(minid:maxid) - sum(ids) # Part 2: missing seat
