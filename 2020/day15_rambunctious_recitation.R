a <- strsplit(rfp('2020','15'),',',fixed=T)[[1]] |> strtoi()

if(!(exists('onlyR') && onlyR)) {

  rustPlay <- importDll('2020/day15_rambunctious_recitation.so') # Get function name in library
  rustPlay(a, 2020L) # Part 1 (536): Last spoken number after 2020 turns
  rustPlay(a, 3e7L) # Part 2 (24065124): Last spoken number after 30 millions turns
  # Rcpp::sourceCpp("2020/day15_rambunctious_recitation.cpp", dryRun = T, verbose = T, rebuild = T)
  # Rcpp::sourceCpp("2020/day15_rambunctious_recitation.cpp", dryRun = F, verbose = T, rebuild = T)
  # rustPlay(a, 3e7L) |> ti(rep = 10)
  # rcppPlay(a, 3e7L) |> ti(rep = 10) # clang = 400ms (brancheless)
  # rcppPlay(a, 3e7L) |> ti(rep = 10) # g++ = 430ms

} else {
  
##### R optimized function #####
# This R function runs in about 3 sec
# Since this is a simple bruteforce challenge, custom low level code is authorized
  play <- compiler::cmpfun(function(x, turns) {
    l <- length(x)
    diff_time <- collapse::alloc(0L,turns) # Init large enough array to store last spoken turns
    diff_time[x] <- seq_along(x) # Init firsts turns
    last <- x[l] # Last number spoken
    for (turn in (l+1L):turns) { # Simulate each turn
      tmp <- last # Keep track of last number spoken
      # Next spoken number is either the turn difference between the previous 2 times it was spoken, or 0 is it's a new number
      last <- if (diff_time[tmp] != 0L) turn - diff_time[tmp] else 1L
      diff_time[tmp] <- turn-1L # Store turn spoken of previous number
    }
    last-1L # Returns spoken number after X turns (all data was shifted by 1 to adapt to R arrays)
  })

  play(a+1L, 2020L) # 187µs
  play(a+1L, 3e7L) # 3.14s

}

##### Rust code #####
# Runs in 415ms
# rustPlay <- cargo::rust_fn(x, turns, '
#     let x = x.slice_integer().unwrap(); // Declare data as Rust array
#     let turns = turns.as_usize(); // Declare turns as Rust number
#     let l = x.len();
#     let mut diff_time: Vec<i32> = Vec::new(); // Vector to store last spoken turns of each number
#     diff_time.resize(turns, 0); // Resize to fit all turns
#     for i in 0..l {
#         diff_time[x[i] as usize] = i as i32 +1; // Init firsts turns
#     }
#     let mut last = x[l-1] as usize; // Last spoken number
#     let mut tmp;
#     for turn in l as i32 ..turns as i32 { // For each turn
#         tmp = last; // Keep track of last number spoken
#         // Next spoken number is either the turn difference between the previous 2 times it was spoken, or 0 is it\'s a new number
#         last = if diff_time[tmp]>0 { (turn - diff_time[tmp]) as usize } else { 0 };
#         diff_time[tmp] = turn; // Store turn spoken of previous number
#     }
#     return Rval::new(last as i32, pc); // Returns spoken number after X turns
# ', verbose=T)
# rustPlay(a, 2020L) # 4.37µs
# rustPlay(a, 3e7L) # 414ms


##### C++ code #####
# Runs in 440ms
# Rcpp::sourceCpp("2020/day15_rambunctious_recitation.cpp")
# rcppPlay(a, 2020L) # 6.5µs
# rcppPlay(a, 3e7L) # 440ms

# rustPlay <- cargo::rust_fn(x, turns, '
#     let x = x.as_vector_or_stop("error").slice_integer().unwrap(); // Declare data as Rust array
#     let turns = turns.as_usize(); // Declare turns as Rust number
#     let l = x.len();
#     let mut diff_time: Vec<i32> = Vec::new(); // Vector to store last spoken turns of each number
#     diff_time.resize(turns, 0); // Resize to fit all turns
#     for i in 0..l {
#         diff_time[x[i] as usize] = i as i32 +1; // Init firsts turns
#     }
#     let mut last = x[l-1] as usize; // Last spoken number
#     let mut tmp;
#     for turn in l as i32 ..turns as i32 { // For each turn
#         tmp = last; // Keep track of last number spoken
#         // Next spoken number is either the turn difference between the previous 2 times it was spoken, or 0 is it\'s a new number
#         last = if diff_time[tmp]>0 { (turn - diff_time[tmp]) as usize } else { 0 };
#         diff_time[tmp] = turn; // Store turn spoken of previous number
#     }
#     let (res, xres) = RVector::new_integer(1, pc);
#     xres[0] = last as i32;
#     res
# ', verbose=T)
