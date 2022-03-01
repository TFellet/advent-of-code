a <- rfp('2020','5')
b <- stringi::stri_trans_char(a, 'BFRL', '1010') # Convert B/F R/L to 1/0
ids <- strtoi(b, 2) # Compute seat ids
minid <- min(ids); maxid <- max(ids)
maxid # Part 1 (998): max seat id
sum(minid:maxid) - sum(ids) # Part 2 (676): missing seat
