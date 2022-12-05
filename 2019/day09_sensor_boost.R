a <- rfp('2019','9') # Read input
comp <- intcode::intcode(a, .3) # Init computer with 30% overflow memory at the start

comp$run(1) # Part 1 (2738720997): Run computer with input 1
comp$reset()$run(2) # Part 2 (50894): Run computer with input 2
