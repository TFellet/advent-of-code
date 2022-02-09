library(stringi)

a <- brio::read_file(fp('2020', '6')) # Read file as single string
a2 <- stri_split_fixed(a, '\n\n')[[1]] # Split on each group of form
lens <- stri_count_fixed(a2, '\n')+1L # Count number of forms per group
lens[length(lens)] <- lens[length(lens)] - 1L
abid <- stri_replace_all_fixed(a2, '\n', '') # Remove \n left
unl <- strsplit(abid, '', fixed = T) |> vapply(kit::uniqLen, 1L) # Count number of unique answers per group of forms
sum(unl) # Part 1: Sum of unique responses

let2 <- as.vector(matrix(letters, length(abid), length(letters), byrow = T)) # Repeat string for each answer to test
# Count each letter in each form, and find where it is present the same number of times as the number of forms in the group
counts <- stri_count_fixed(abid, let2)==lens
sum(counts) # Part 2: Sum of common responses
