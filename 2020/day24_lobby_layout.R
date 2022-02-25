a <- rfp('2020','24')

# Hexagonal neighbours as complex numbers. 
# E/W are positive/negative integers, N/S are positive/negatives complex
coords <- setNames(c(2,1-1i,-1-1i,-2,-1+1i,1+1i), c('e', 'se', 'sw', 'w', 'nw', 'ne'))
b <- stringi::stri_extract_all_regex(a, '[sn].|[ew]') # Find south/north followed by a letter, or find east/west
points <- sapply(b, \(str) sum(coords[str])) # Add up all directions in every line
count <- kit::countOccur(points) # Count number of time tiles have been flipped
btiles <- count$Variable[as.logical(count$Count%%2)] # Keep only tiles flipped an odd number of times (blacks)
length(btiles) # Part 1 (495): Number of black tiles

for (i in 1:100) {
  all_neighs <- c(btiles, unlist(unname(lapply(coords, `+`, btiles)))) # List of tiles and all neighbours
  groups <- collapse::group(all_neighs, group.sizes=T, starts = T) # Find uniques values and number of times they appear
  nums <- all_neighs[attr(groups, 'starts')] # Uniques values
  color <- c(rep(T, length(btiles)), rep(F, length(nums)-length(btiles))) # Firsts values are black tiles, following are whites
  newb <- attr(groups, 'group.sizes') == 2L | (color & attr(groups, 'group.sizes') == 3L) # Rules to flip black / white tiles
  btiles <- nums[newb] # New list of black tiles
}
length(btiles) # Part 2 (4012): Number of black tiles after 100 turns
