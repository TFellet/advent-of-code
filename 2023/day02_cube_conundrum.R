a <- rfp('2023','2') # Read input
a2 <- stringi::stri_match_all_regex(a, '(\\d+) (r|g|b)') # Find number of cubes of each color

a3 <- do.call(rbind, a2) # Combine all matches to a matrix
a4 <- list(id = rep(seq_along(a), times = lengths(a2)/3), V1 = strtoi(a3[,2]), color = a3[,3]) # Add game id information
a5 <- collapse::fmax(a4$V1, g = list(a4$id, a4$color), use.g.names = F) # Find max number of cubes of each color (each group is sorted by blue, green, then red)

# Part 1 (2317): Games possible with less than 12 red, 13 green and 14 blue cubes
(which(a5 > 14L:12L) + 2L) %/% 3L |> unique() |> (\(x) (length(a)/2*(length(a)+1)) - sum(x))()
# part 2 (74804): Product of the minimum set of cubes that must have been present, then sum all products
sum(collapse::fprod(a5, 0L:299L %/% 3L, use.g.names = F))
