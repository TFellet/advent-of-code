a <- rfp('2019','5') # Read input
comp <- intcode::intcode(a) # Init computer

comp$run(1L) |> flast() # Part 1 (16348437): Run computer with input 1 and keep last output
comp$reset()$run(5L) # Part 2 (6959377): Run computer with input 5
