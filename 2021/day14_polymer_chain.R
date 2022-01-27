library(data.table)
library(expm) # Matrix exponents
options(scipen = 50)

temp <- strsplit(fread(fp(2021,14),nrows = 1,header = F)[[1]],'')[[1]] # Read polymer template
rules <- fread(fp(2021,14),skip=2,header = F,select = c(1,3)) # Read pairing rules

# Change rules from AB -> C to (AB -> AC / AB -> BC)
rules <- rbind(rules[,.(inp=V1, out=paste0(substr(V1,1,1),V3))], rules[,.(inp=V1, out=paste0(V3, substr(V1,2,2)))])
setkey(rules, inp) # Index on rules

# Create all pairs presents in polymer and sum them
poly <- data.table(inp=paste0(head(temp,-1),tail(temp,-1)))[,.(N = as.double(.N)),keyby=inp]

### Matrix version 223 µs on 40 steps | 397 µs on 1000 steps
un <- sort(unique(c(rules$inp, rules$out))) # All unique pairs present
m <- array(0,rep(length(un),2), list(un,un)) # Matrix to keep track of associations
m[(match(rules$inp, un)-1)*nrow(m) + match(rules$out, un)] <- 1 # Add the rules to the matrix
start <- setNames(rep(0,length(un)), un) # Empty vector with all combinasons of letters
start[poly$inp] <- poly$N # Assign numbers from input

count <- \(l) sum(end[grepl(l, names(end))]) + end[grepl(paste0(l,l), names(end))] # Function to count twice each letter

# Part 1
end <- (m %^% 10 %*% start)[,1] # Find pairs presents after 40 steps
quant <- sapply(unique(substr(names(end),1,1)), count)/2 # Apply above function on each letter
floor(max(quant) - min(quant))# Difference between min and max

# Part 2
end <- (m %^% 40 %*% start)[,1] # Find pairs presents after 40 steps
quant <- sapply(unique(substr(names(end),1,1)), count)/2 # Apply above function on each letter
floor(max(quant) - min(quant))# Difference between min and max
