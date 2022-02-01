a <- sfp('2021','2', sep=' ', what = list('a', 0L)) # Read file as 2 lists
nums <- kit::vswitch(a[[1]], c('up', 'down'), c(-1L, 1L), default = 0L) # Convert up / down / forward to -1,1,0
fw <- !nums # Forward instructions
ai <- cumsum(a[[2]]*nums) # Aim at each step
afws <- sum(a[[2]][fw]) # Total forward moves
ai[length(ai)]*afws # Part 1: final aim * forward moves
sum(ai[fw]*a[[2]][fw])*afws # Part 2: On each forward, move towards aim; sum of moves * forward 
