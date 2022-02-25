library(purrr)
library(stringr)
library(collections)
library(data.table)
library(matrixStats)
library(parallel)


solve <- \(p2 = F, example = F) {
  # From a state, returns TRUE where pods are correctly placed
  getCorrects <- \(state) {
    each <- (length(state)-12+1)/4 # Number of rows in each room
    corr <- matrix(c(state[12:length(state)] == rep(1:4, each = each)), nrow = each) # correct if first 4 = 1, 4 nexts = 2...
    for (i in ((each-1):1)) {  
      corr[i,] <- corr[i,] & corr[(i+1),] # Each line is correct if previous is also correct
    }
    as.vector(corr)
  }
  
  # From a state, explore the map, and store distances between spots, and which spots in between are visited
  genMasks <- \(state, look) {
    mm <- array(F, dim = rep(length(state),3)) # For each "from", "to", store the spots visited in the state (T/F)
    mc <- array(0L, dim = rep(length(state),2)) # For each "from", "to", store the cost to move from one to another
    ml <- list() # At the end, store each matrix "from" into a list for faster access
    
    # Recursive function for exploration
    expco <- \(id0, id, co, visit) {
      visitid <- visit # Copy mask
      visitid[id] <- T # Current id is visited
      mm[id0, id,] <<- visitid # Store mask
      mc[id0, id] <<- co # Store cost
      nei <- look[[id]] # Get neighbours
      nei <- nei[!visitid[nei]] # Remove visited 
      for (i in nei) expco(id0, i, co+1L, visitid) # Recursive explore neighbours
    }
    
    for (id in 1:length(state)) { # For each "from" in the state
      id0 <- id; visit <- rep(F,length(state)); co <- 0L
      visit[id] <- T # Current id is visited
      mm[id0, id,] <- visit # Store the mask (T/F) in the matrix
      nei <- look[[id]] # Explore neighbours
      
      for (i in nei) expco(id0, i, co+1L, visit) # Explore each neighbour
      mm[id0,,id0] <- F # Remove "from" from mask 
    }
    for (i in 1:length(state)) ml[[i]] <- mm[i,,] # Convert matrix to list
    return(list(ml, mc))
  }
  
  # From a state, return list of possible moves to explore
  getMoves <- \(state, corrects) {
    moves <- list()
    # corrects <- getCorrects(state)
    n <- length(corrects)/4L # Number of pods in each room
    corrects <- c(rep(F,11), corrects)
    occup <- state != 0 # Mask of occupied spots
    
    ids <- which(occup[1:11]) # Hallway pods
    # ids <- which(occup & !corrects) # Pods
    occup8 <- matrix(rep(occup, 8L),byrow = T,nrow=8L) # Replicate occupied 8 times
    vss <- colSums2(matrix(!corrects[unlist(valids)],ncol=4))+(n*0L:3L)+11L # Destination of each type of pod
    
    ids2 <-  which(occup[12:length(occup)] & !corrects[12:length(corrects)])-1L # Pods in rooms
    # ids2[ids2-shift(ids2,fill=50) != 1 | ids2%%n == 0]+12L
    # !((ids2+12L)-1L) %in% ids | ids2%%n == 0
    ids2 <- ids2[ids2-shift(ids2,fill=50) != 1 | ids2%%n == 0]+12L # Remove pods with pods above them
    ids <- c(ids, ids2)
    
    for (id in ids) { # For each wrong pod
      vs <- vss[[state[id]]] # Get destination
      if (id < 12) { # Pod in hallway, can only move to destination
        m <- ml[[id]][vs,] & occup # Get mask from spot to destination
        # m <- m  # Verify if path is available
        dest <- !any(m) # If there is any TRUE, path is blocked
      } else { # Pod in room, can mov to hallway and destination
        lines <- c(1,2,4,6,8,10,11, vs) # Possible moves
        
        # m <- ml[[id]][lines,] # Get 8 masks
        # m <- m & occup8 # Verify paths
        # m <- lines[rowSums2(m) == 0] # Keep only destinations with no blocks
        
        m <- lines[rowSums2((ml[[id]][lines,] & occup8)) == 0]
        dest <- !is.na(match(vs, m)) # Destination is possible
      }
      if(dest) { # Destination available
        moves <- list() # Remove all other moves
        costs <- mc[id,vs] # Get move cost
        names(costs) <- vs
        moves[[as.character(id)]] <- costs
        return(moves) # Stop exploring
      }
      if (id >= 12) { # Destination not available but pod in room
        if(length(m) == 0) next
        costs <- mc[id,m] # Get moves costs
        names(costs) <- m
        moves[[as.character(id)]] <- costs # Add moves to list
      }
    }
    return(moves)
  }
  
  explore <- \(state, cost = 0L) {
    if (cost >= found_cost) return(Inf) # Current exploration is longer than solution found
    explored <- cache$has(state) # Verify if state is already explored
    
    if(explored) {
      cached <- cache$get(state) # Get cached data
      corrects <- cached[['corrects']] # List of corrects pods from cache
    } else {
      corrects <- getCorrects(state) # Compute list of corrects pods
    }
    
    if (all(corrects)) { # Cheaper solution found
      found_cost <<- cost # Update found cost
      cache$set(state, list(corrects = corrects, moves = NULL, cost = cost))
      return(cost)
    }
    
    if(explored) {
      if (cached[['cost']] <= cost) { # If old exploration is cheaper
        return(Inf)
      } else { # If current exploration is cheaper
        moves <- cached[['moves']] # Get moves from cache
        cache$set(state, list(corrects = corrects, moves = moves, cost = cost)) # Update cost
      }
    } else { # If path not explored
      moves <- getMoves(state, corrects) # Compute list of possible moves
      cache$set(state, list(corrects = corrects, moves = moves, cost = cost)) # Cache moves
    }
    
    if(length(moves) == 0L) return(Inf)
    if(first) {
      first <- F
      m2 <- unlist(moves)
      costs_list <- mclapply(mc.cores = 12L, mc.allow.recursive = F, mc.preschedule = T, mc.cleanup = T, seq_along(m2), \(ii) {
        i <- m2[ii]
        m3 <- strsplit(names(i), '.',fixed=T)[[1]]
        i <- m3[[1]]
        j <- strtoi(m3[[2]])
        pod <- as.integer(i) # Id of pod in state
        vals <- m2[ii]
        moves_cost <- vals * 10^(state[pod]-1L) # Correct cost of all moves
        new_cost <- moves_cost + cost
        state2 <- state # Copy state
        state2[j] <- state2[pod] # Move pod to dest
        state2[pod] <- 0L # Change from to 0
        explore(state2, new_cost) # Explore new state with new cost
      })
      return(min(unlist(costs_list)))
    } else {
      for (i in names(moves)) { # For every pod that can move
        pod <- as.integer(i) # Id of pod in state
        vals <- moves[[i]] # Possibles moves
        moves_costs <- vals * 10^(state[pod]-1L) # Correct cost of all moves
        keep <- (moves_costs + cost) < found_cost # Keep only moves cheaper than found cost
        moves_costs <- moves_costs[keep]
        vals <- vals[keep]
        vals_dest <- strtoi(names(vals))
        for (j in seq_along(vals)) { # For every move of the pod
          new_cost <- moves_costs[[j]] + cost
          if(new_cost >= found_cost) return(Inf) # If current cost is more expensive than found solution, exit
          state2 <- state # Copy state
          state2[vals_dest[[j]]] <- state2[pod] # Move pod to dest
          state2[pod] <- 0L # Change from to 0
          explore(state2, new_cost) # Explore new state with new cost
        }
      }
    }
  }
  
  
  a <- rfp(2021,23)
  if(example) a <- rfp(2021,'23_ex')
  # Convert string to array of int 
  b <- strsplit(trimws(gsub('#', '', a)),'') |> map(~str_replace_all(.x,c('[.]' = '0', 'A'='1', 'B'='2', 'C'='3', 'D'='4')) |> strtoi())
  
  # List of all neighbours for part 1
  look <- list(2, c(1,3), c(2, 4, 12), c(3, 5), c(4, 6, 14), c(5, 7), c(6, 8, 16), c(7, 9), c(8, 10, 18), c(9, 11), 10,
               c(3, 13), 12, c(5, 15), 14, c(7, 17), 16, c(9, 19), 18)
  
  # List of pods in rooms
  rooms <- b[c(-1,-2, -length(b))] |> transpose() |> map(unlist)
  first <- T
  if(p2) {
    part2 <- list(c(4,4), c(3,2), c(2,1),c(1,3)) # Values to add for part 2
    rooms <- map2(rooms, part2, ~c(.x[1], .y, .x[2]))
    # List of all neighbours for part 2
    look <- list(2, c(1,3), c(2, 4, 12), c(3, 5), c(4, 6, 16), c(5, 7), c(6, 8, 20), c(7, 9), c(8, 10, 24), c(9, 11), 10,
                 c(3, 13), c(12,14), c(13, 15), 14, # 12 to 15
                 c(5, 17), c(16,18), c(17, 19), 18, # 16 to 19
                 c(7, 21), c(20,22), c(21, 23), 22, # 20 to 23
                 c(9, 25), c(24,26), c(25, 27), 26) # 24 to 27
  }
  hall <- rep(0L, 11L)
  rooms_cols <- c(3, 5, 7, 9) # Intersection between hallway and rooms
  rooms_top <- seq.int(12, by = length(rooms[[1]]), length.out = 4) # Top spot of each room
  valids <- map(rooms_top, ~c(.x:(.x+length(rooms[[1]])-1))) # List of each id in rooms
  state <- c(hall, unlist(rooms)) # First state
  tmp <- genMasks(state, look) # Generate masks and costs
  ml <- tmp[[1]]
  mc <- tmp[[2]]
  cache <- dict()
  found_cost <- Inf # First solution is infinite
  
  explore(state)
}
solve(p2=F)
solve(p2=T)

# TODO: 
# keep hallway and rooms separated
# Rooms = 4x2 or 4x4 matrix
# Keep track of corrects in attr of rooms (4 nums, 0:4)
# Keep track of possibles "to" (4 nums, 0:4, 0=invalid)
# Keep track of possible "from" (4 nums, 0:4, 0=invalid)
# In cache, key becomes c(hall, rooms)
# In gen masks, at the end, remove ids 3,5,7,9 (always F)
# Once gen masks has been run, remove ids 3,5,7,9 from hall

# Example 1 (12521) : 0.584 
# Example 2 (44169) : 9.332
# Input 1 (14350) : 2.379 (cache)
# Input 2 (49742) : 10.069 (cache)
# Input 2 (49742) : 4.97s (parallel 12)
