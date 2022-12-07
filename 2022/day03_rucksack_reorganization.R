a <- rfp('2022','3') |> strsplit('',fixed=T) # Read input and split into character vectors
ints <- lapply(a, fastmatch::fmatch, c(letters, LETTERS)) # Convert letters to integers
inter <- lapply(ints, matrix, ncol=2) |> sapply(\(x) x[,1][x[,1] %in% x[,2]][1]) # Split into 2 part using matrix and find intersection
sum(inter) # Part 1 (8105): Sum of letters found

tabs <- lapply(ints, tabulate, nbins = 26*2) # Convert single int to long vector
findBadge <- \(x) Reduce(x, f=`&`) |> which.max() # Function to find a badge in a group
badges <- matrix(tabs, ncol = 3, byrow = T) |> collapse::mrtl() |> sapply(findBadge) # Split input into groups using matrix and find all badges
sum(badges) # Part 2 (2363): Sum of all badges
