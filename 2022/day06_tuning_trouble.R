a <- rfp('2022','6')
a2 <- strsplit(a, "", fixed=T)[[1]] # Read input string
reg <- r'[(.)(?!\1)(.)(?!\1|\2)(.)(?!\1|\2|\3)(.)]' # Regex is faster for part 1

findStart <- function(datastream, num_chars = 4) {
  count <- 1L # First position to check
  repeat {
    chars <- datastream[count:(count+num_chars-1)] # Elements to check
    # Find which elements are duplicated and where; It tells us how many positions we can skip
    matches <- match(chars, chars)
    dup <- matches[matches != seq_len(num_chars)] # Check if all characters are found only at their positions
    # If all characters are unique, returns end position, otherwise increase count where the last duplicate found won't be present
    if(length(dup) == 0L) return(count + num_chars - 1) else count <- count + max(dup)
  }
}

regexec(reg, a, perl=T)[[1]] |> flast() # Part 1 (1361): First occurence of 4 differents characters
findStart(a2, 14) # Part 2 (3263): First occurence of 14 differents characters
