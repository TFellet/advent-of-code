a <- rfp('2020','13')
t <- strtoi(a[1]) # Timestamp for Part 1
buses <- strsplit(a[2], ',',fixed=T)[[1]] # Split into individual numbers
times <- which(buses!='x') - 1L # Find positions of valids buses
buses <- strtoi(buses[times+1L]) # Keep only valid buses

wait <- abs(t %% buses - buses) # Waiting time after timestamp for each bus
bus <- which.min(wait) # Find minimum wait time
buses[bus]*wait[bus] # Part 1 (174): Min wait time * bus id

start <- 0 # Start of sequences
prods <- cumprod(buses)

for (i in 2:length(buses)) { # For each bus from 2
  end <- prods[i] # End of sequence
  by <- prods[i-1] # Step of sequence
  difft <- times[i] # Time delta between bus 1 and bus i
  seqn <- seq(start, end, by) # Sequence from first possible number to maximum possible number
  start <- seqn[which.max((seqn+difft)%%buses[i]==0)] # Find first valid number in sequence
}
start # Part 2 (780601154795940): First time where buses pass at correct invervals
