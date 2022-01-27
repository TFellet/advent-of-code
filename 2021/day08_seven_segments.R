library(data.table)
library(purrr)
a <- setDT(scan(fp(2021,8), as.list(rep('', 15)),quiet=T))[,V11:=NULL]

# Part 1
a[,map_int(.SD, ~nchar(.x) %in% c(2:4,7) |> sum()) |> sum(),.SDcols=V12:V15] # 460 µs # 548

# Part 2
let <- c("abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg") # Numbers in 7 segments
st <- \(x) vapply(strsplit(x, NULL), function(x) p0(sort(x)), '') # Sort vector
p0 <- \(x) paste0(x, collapse = '') # Collapse a vector
sig <- \(vect) { # Find signature of a vector
  spl <- vect[1:10] |> strsplit('') # Split each char
  # Count occurence of each char     | In each string, replace letters by their occurences => signature
  nb <- spl |> unlist() |> table() |> (\(t) { spl |> map_chr(~t[.x] |> sort() |> p0())})() 
  data.table(dig = 0:9, nb, txt=st(vect[1:10])) # Signature corresponding to each number with original str
}
decode <- \(x) {
  found <- sig(unlist(x[1:10])) # Signature of input
  comb <- ref[found, on='nb'] # Find match with reference
  setkey(comb, i.txt) # Index on text
  comb[st(unlist(x[11:14])),dig] |> p0() |> as.integer() # Merge output with match and convert to number
}
ref <- sig(let) # Signature of reference digits
apply(a, 1, decode) |> sum() # 463 ms # 1074888
