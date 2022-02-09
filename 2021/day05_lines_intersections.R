am <- rfp('2021','5') |> stringi::stri_replace_first_fixed(' -> ', ',') |> toGrid(sep=',') # Read data into matrix
coords <- mapply(\(l1,l2,l3,l4){(l1:l3)+(l2:l4*1e-5)}, am[,1], am[,2], am[,3], am[,4]) # Generate coordinates
not_diag <- ((am[,1]==am[,3]) | (am[,2]==am[,4])) # Find diagonal lines (for Part 2)
lp1 <- unlist(coords[not_diag]) # Group straight lines
countDup <- \(lp) sum(attr(collapse::group(lp, group.sizes = T), 'group.sizes')>1L) # Count unique points duplicated
countDup(lp1) # Part 1: Duplicated points in straight lines
countDup(c(lp1, unlist(coords[!not_diag]))) # Part 2: All duplicated points
# 6.33ms
