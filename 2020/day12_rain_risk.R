library(data.table)

a <- rfp('2020','12')
let <- substr(a, 1,1) # Letter instruction
num <- strtoi(stringi::stri_sub(a, 2L)) # Number associated
lf <- collapse::whichv(let, 'F') # Forward instructions
moves <- kit::vswitch(let, c('N','E','S','W'), c(1L+0i, 1i, -1L+0i, -1i), 0i)*num # Convert directions to complex numbers
rot <- 1i^(kit::vswitch(let, c('R','L'), c(1L,-1L), 0L)*num %/% 90L) # Convert rotations to complex numbers (to multiply result)
rot[1] <- 1i # Start facing East
rot_all <- cumprod(rot) # rotation at each time step
moves[lf] <- rot_all[lf]*num[lf] # Forward moves the ship in the direction it is facing
ship_end <- sum(moves) # Add all moves to get final position of the ship
abs(Re(ship_end))+abs(Im(ship_end)) # Part 1 (759): Manhattan distance of the ship from the origin

# Put part 2 in a function to force compile loop
p2 <- \() {
  way <- 1L+10i # Initial waypoint position
  ship <- 0i # Initial ship position
  cond <- fcase(let=='F', 3L, let%chin%c('L', 'R'), 2L, default = 1L) # Group instructions in 3 categories
  for (i in seq_along(a)) { # On each line
    switch (cond[i], # Check instruction group
            {way <- way+moves[i]}, # 1 = Move in specified direction
            {way <- way*rot[i]}, # 2 = Rotate waypoint around ship
            {ship <- ship+way*num[i]}) # 3 = Move towards waypoint
  }
  abs(Re(ship))+abs(Im(ship)) # Part 2 (45763): Manhattan distance of the ship from the origin
}
p2()
