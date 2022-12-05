a <- rfp('2019','11') # Read intcode program

paint <- \(init_color = 0L) {
  comp <- intcode::intcode(a) # Init a computer
  pos <- 0+0i # Initial position
  dir <- 1i # Start facing up
  points <- collections::dict(init_color, pos) # Remember tiles visited
  repeat {
    color <- points$get(pos, default = 0L) # Color of current tile
    res <- comp$run(color) # Execute program
    if(is.null(res)) return(points) # Exit when program ends
    points$set(pos, res[1]) # Change color of current tile
    dir <- dir * (if(res[2]) -1i else 1i) # Change direction
    pos <- pos + dir # Advance 1 step forward
  }
}

paint()$size() # Part 1 (2255): Number of points visited when starting on a black tile

points <- paint(1L) # Points visited when starting on a white tile
# Create a table with all tiles and colors corresponding
dt <- fDT(x=Re(unlist(points$keys())), y=Im(unlist(points$keys())), v = unlist(points$values()))

library(ggplot2)
ggplot(collapse::ss(dt, dt$v==1L), aes(x, y)) + geom_tile() # Part 2 (BCKFPCRA): Plot colored tiles to reveal code

# Cleaner slower version
# ggplot(collapse::ss(dt, dt$v==1L), aes(x, y)) + geom_tile() + coord_equal() + theme_void()
