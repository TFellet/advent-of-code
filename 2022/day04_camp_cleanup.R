a <- brio::read_file(fp('2022','4')) |> # Read input as a single string
  gsub(x=_,',','-',fixed=T) |> # Convert , to - to get a single separator
  scan(text = _, what=list(0L,0L,0L,0L),sep = '-') # Parse string into 4 lists of numbers with scan
collapse::massign(c('w', 'x', 'y', 'z'), a) # Assign simpler names
sum((w<=y & x>=z) | (y<=w & z>=x)) # Part 1 (433): Number of complete overlaps
sum((w<=z & x>=y)) # Part 2 (852): Number of partial overlaps
