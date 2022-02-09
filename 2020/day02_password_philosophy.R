library(stringi)
a <- rfp('2020','2')
a2 <- stri_trans_char(a, '-:', '  ') # Replace non fields char by a space
b <- data.table::tstrsplit(a2, ' ', fixed=T, keep = c(1:3,5)) # Split string and transpose 1000 lists * 4 to 4 lists of 1000
b[[1]] <- strtoi(b[[1]])
b[[2]] <- strtoi(b[[2]])
counts <- stri_count_fixed(b[[4]],b[[3]]) # Count occurences of letter in password
sum(counts >= b[[1]] & counts <= b[[2]]) # Part 1: Passwords with correct number of letters
# Part 2: Letter at exactly one specified place
sum(xor(substr(b[[4]], b[[1]], b[[1]]) == b[[3]], substr(b[[4]], b[[2]], b[[2]]) == b[[3]]))
