a <- strsplit(rfp('2022','6'), "", fixed=T)[[1]] # Read input string

# Define a function to detect the first start-of-message marker
findStart <- function(datastream, num_chars = 4) {
  # Loop through the datastream, one character at a time
  for (count in num_chars:length(datastream)) {
    # Check if the last 14 characters are all different
    if (length(unique(datastream[(count-num_chars+1):count])) == num_chars) 
      return(count) # If they are, return the current value of the counter
  }
}

findStart(a, 4) # Part 1 (1361): First occurence of 4 differents characters
findStart(a, 14) # Part 2 (3263): First occurence of 14 differents characters
