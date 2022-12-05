a <- rfp('2022','3')
comp1 <- substr(a, 1, nchar(a)/2) |> strsplit('', fixed = T)
comp2 <- substr(a, nchar(a)/2+1, nchar(a)) |> strsplit('', fixed = T)
inter <- mapply(intersect, comp1, comp2) |> paste0(collapse = '')
prio <- utf8ToInt(inter)
sum(prio[prio > 96] - 96) + sum(prio[prio < 91] - 38) # Part 1 (8105)

gr <- frepEach(1:(length(a)/3), 3) # Groups id
findBadge <- \(x) lapply(x, stringi::stri_count_fixed, let) |> Reduce(f=`&`) |> which() # Function to find a badge in a group
badges <- collapse::BY(a, g=gr, findBadge) # Find all badges
sum(badges) # Part 2 (2363)
