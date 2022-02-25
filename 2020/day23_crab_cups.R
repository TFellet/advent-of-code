a <- strtoi(strsplit(rfp('2020','23'),'')[[1]]) # Read input as integer vector

# Function that creates the array of neighbour values 
createNxt <- \(input, size) {
  nxt <- 0L # Init empty vector
  if(size == 9L) {
    nxt[input] <- input[c(2:9, 1)] # For each value, look the value right after
    nxt <- c(0L, nxt) # Add a padding for low level code indexes starting at 0
  } else {
    nxt[input] <- c(input, 10L)[c(2:10)] # Same as part 1 but vector doesn't loop to 1, it goes to 10
    nxt <- c(0L, nxt, 11:size, input[1]) # Add padding and remaining neighbours
  }
  nxt
}

# Once again on a pure bruteforce challenge, low level code is used
# R version runs in 2.6 sec while Rust version runs in 200ms
dll <- dyn.load('2020/day23_crab_cups.so') # Load previously compiled code
Rust_rustGame <- getNativeSymbolInfo('func', 'day23_crab_cups') # Get function name in library

rustGame <- \(input, turns, size=9L) { # Create a R function from commpiled code
  nxt <- createNxt(input, size) # Create array of neighbours
  .Call(Rust_rustGame, input, nxt, turns, size) # Call compiled code
}

nxtp1 <- rustGame(a, 100) # Get neighbour array after 100 iterations
resp1 <- nxtp1[2L] # Neighbour of 1
for(j in 2:8) resp1[j] <- nxtp1[resp1[j-1L]+1L] # Find neighbour of each value after neighbour of 1
strtoi(paste0(resp1,collapse = '')) # Part 1 (97342568): Order of cups after the cup labeled 1

nxtp2 <- rustGame(a, 1e7, 1e6) # Get neighbours after 10 millions turns on a 1 million cups circle
nxtp2[2L]*1*nxtp2[nxtp2[2L]+1L] # Part 2 (902208073192): Product of 2 cups after cup labeled 1


##### Base R version #####
# game <- \(input, turns, size=9L) {
#   nxt <- createNxt(input, size)[-1L] # Create neighbours array and remove padding
#   curr_id <- 1L # First current index
#   maxval <- size # Max value to loop under 1
#   curr_val <- input[curr_id] # First current value
# 
#   for (i in 1:turns) { # Each turn
#     pick1 <- nxt[curr_val] # Pick neighbour of value
#     pick2 <- nxt[pick1] # Pick 2nd neighbour
#     pick3 <- nxt[pick2] # Pick 3rd neighbour
# 
#     dest_val <- if(curr_val == 1L) maxval else curr_val-1L # Get destination (and loop if necessary)
#     while (dest_val == pick1 || dest_val == pick2 || dest_val == pick3) { # If destination is picked up
#       dest_val <- if(dest_val == 1L) maxval else dest_val-1L} # Go 1 below and loop if necessary
#     nxt[curr_val] <- nxt[pick3] # New neighbour of current value is the neighbour of 3rd picked up cup
#     nxt[pick3] <- nxt[dest_val] # New neighbour of 3rd cup is neighbour of destination
#     nxt[dest_val] <- pick1 # New neighbour of destination is 1st cup
#     curr_val <- nxt[curr_val] # New current value is neighbour of current cup
#   }
#   nxt
# }
# resp1 <- game(a, 100) |> ti() # 30.8µs vs 3.06µs in Rust
# v <- 1L;for(j in 1:8) cat(v <- resp1[v]) # 97342568
# 
# resp2 <- game(a, 1e7, 1e6) # 2.63s vs 194ms in Rust
# resp2[1L] * 1 * resp2[resp2[1L]] #902208073192

##### Rust code #####
# crabCupsRust <- cargo::rust_fn(input, nxt2, turns, size, '
#     // Parse inputs
#     let input = input.slice_integer().unwrap();
#     let (res, nxt) = nxt2.coerce_integer(&mut pc).unwrap();
#     let turns = turns.as_usize();
#     let size = size.as_usize();
#     
#     // Init starting variables
#     let curr_id = 0;
#     let maxval = size as i32;
#     let mut curr_val = input[curr_id];
#     
#     for _i in 0..turns {
#         // Cups picked up
#         let pick1 = nxt[curr_val as usize];
#         let pick2 = nxt[pick1 as usize];
#         let pick3 = nxt[pick2 as usize];
#         
#         // Find destination 
#         let mut dest_val = if curr_val == 1 {maxval} else {curr_val-1};
#         while dest_val == pick1 || dest_val == pick2 || dest_val == pick3 {
#         dest_val = if dest_val == 1 {maxval} else {dest_val-1};
#         }
#         
#         // Assign values as described in R code
#         nxt[curr_val as usize] = nxt[pick3 as usize];
#         nxt[pick3 as usize] = nxt[dest_val as usize];
#         nxt[dest_val as usize] = pick1;
#         curr_val = nxt[curr_val as usize];
#     }
#     return res;
# ')
