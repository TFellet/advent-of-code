library(stringi)
library(fastmatch)
a <- rfp('2021','8') |> stri_split_fixed(' ')

# Part 1
# Count how many results numbers are composed of 2,3,4 or 7 letters
sum(stri_length(unlist(data.table::transpose(a)[12:15])) %in% c(2,3,4,7)) 

# Part 2
let <- c("abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg") # Numbers in 7 segments
mult <- (10^(3:0)) # Add results numbers
st <- \(x) vapply(strsplit(x, ''), \(s) stri_flatten(stri_sort(s)), '') # Sort vector

# Compute "signature" of a vector
sig <- \(vect) {
  txt <- st(vect[1:10]) # Filter and sort text
  nb <- stri_count_fixed(stri_flatten(txt), letters[1:7]) # Count occurences of letters
  # Replace letters by their occurences
  nbstr <- stri_replace_all_fixed(txt, letters[1:7], as.character(nb), vectorize_all = F)
  list(nb=st(nbstr), txt=txt) # Returns sorted occurence ann sorted initial text
}

# From a signature of input and reference letters, find correspondances
decode <- \(x) {
  found <- sig(x[1:10]) # Signature of input
  dig <- fmatch(found$nb, ref$nb)-1L # Digits from 0 to 9 in the order they appear (row reference)
  (dig[fmatch(st(x[12:15]), found$txt)] * mult) |> sum() # Find result number in row reference
}

ref <- sig(let) # Global letters reference
sum(sapply(a, decode)) # Decode all lines and sum results
