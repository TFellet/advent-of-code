am <- rfp(2021,9) |> toGrid()

up <- \(m) rbind(10, m[1:(nrow(m)-1),]) # Shift matrix up
do <- \(m) rbind(m[2:nrow(m),], 10)     # Shift matrix down
le <- \(m) cbind(10, m[,1:(ncol(m)-1)]) # Shift matrix left
ri <- \(m) cbind(m[,2:ncol(m)], 10)     # Shift matrix right
low <- \(m) m < up(m) & m < do(m) & m < ri(m) & m < le(m) # Points lower than neighbours
min_id <- low(am) # Find lowest points

# Part 1
sum(am[min_id]+1) # 13.7 µs # Sum of lowest points + 1

# Part 2
mg <- am # Matrix of bassins
mg[which(min_id)] <- (1:sum(min_id)) + 10 # Each lowest point has a unique id above 10
while(sum(mg < 9) > 0) { # While all points are not in a bassin or a limit (9)
  for (fn in list(up, do, ri, le)) { # In each direction
    idx <- mg != 9 & fn(mg) > 10 # If neighbour is in a bassin
    mg[idx] <- fn(mg)[idx] # Assign bassin id 
  }
}
bas <- table(mg[mg>10]) # Count points in each bassin
prod(head(sort(bas,decreasing = T),3)) # Product of top 3 bassins
# 12.19 ms
