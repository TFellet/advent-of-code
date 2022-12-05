a <- rfp('2019','7') # Read code
comps <- replicate(5, intcode::intcode(a), F) # Init 5 intcode computers
prev_ind <- c(5L, 1L:4L) # 1 is connected to 5, the others to the previous number

tryPerm <- \(values, r = 0L) {
  # Run amplifier 5 times with sequence and previous result as input
  for (i in 1L:5L) r <- comps[[i]]$reset()$run(c(values[i], r));r
}

findMax <- \(range, func) 
  Rfast::permutation(range) |> # Generate all permutations
  collapse::mrtl() |> # Convert them to list
  sapply(func) |> # 5th amplifier value
  max() # Return max value found

findMax(0L:4L, tryPerm) # Part 1 (101490): Max value after 5 amplify functions

tryPermLoop <- \(values) {
  results <- list(NULL, NULL, NULL, NULL, 0L) # First signal is 0 (amplifier 1 is connected to 5)
  lapply(comps, \(x) x$reset()) # Reset all computers in each try
  first <- T # First loop
  while(!comps[[5]]$end) { # Exit when the last program has completed
    for (i in 1L:5L) { # For each sequence of 5 amplifiers
      prev <- prev_ind[i]
      # On first loop, feed sequence along with input
      if(first) results[[prev]] <- c(values[i], results[[prev]]) 
      # Run computer with previous state and output of previous amplifier
      results[[i]] <- comps[[i]]$run(results[[prev]])
    }
    first <- F
  }
  results[[5]]
}

findMax(5L:9L, tryPermLoop) # Part 2 (61019896): Max value after 5 looping amplify functions
