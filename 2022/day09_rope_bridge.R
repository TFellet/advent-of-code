a <- sfp('2022','9', what = list('', 1L))

# Convert L R U D mouvements to X and Y positions of 1 step each
headx <- (fastmatch::fmatch(a[[1]], c('L', '', 'R'), nomatch = 2L) - 2L) |> rep(a[[2]]) |> cumsum()
heady <- (fastmatch::fmatch(a[[1]], c('D', '', 'U'), nomatch = 2L) - 2L) |> rep(a[[2]]) |> cumsum()


tailPositions <- \(headx, heady) {
  tailx <- taily <- 0L # Initial tail position
  resx <- resy <- rep_len(0L, length(headx)) # Where to sotre the tail mouvement
  for (i in 1:length(headx)) { # At every head move
    diffx <- headx[i] - tailx # Relative position between head and tail in X
    diffy <- heady[i] - taily # Relative position between head and tail in y
    if(abs(diffx) + abs(diffy) > 2) { # If the tail doesn't touch the head and is in a diagonal compared to the head
      tailx <- tailx + sign(diffx) # Bring tail closer by 1 in both axes
      taily <- taily + sign(diffy)
    } else if(abs(diffx) > 1) { # Tail is 2 away in the same row
      tailx <- tailx + sign(diffx) # Move tail 1 step closer in X
    } else if(abs(diffy) > 1) { # Tail is 2 away in the same column
      taily <- taily + sign(diffy) # Move tail 1 step closer in y
    }
    resx[i] <- tailx # Record position of the tail
    resy[i] <- taily
  }
  return(list(resx, resy)) # Returns list of tail positions
}

pos_t <- tailPositions(headx, heady) # Record tail positions given a list of head positions
collapse::fnunique(pos_t[[1]]*1e5+pos_t[[2]]) # Part 1 (6314): Number of positions visited by the first knot 

for (i in 2:9) {pos_t <- tailPositions(pos_t[[1]], pos_t[[2]])} # Simulate every knot one after the other
collapse::fnunique(pos_t[[1]]*1e5+pos_t[[2]]) # Part 2 (2504): Number of positions visited by the ninth knot
