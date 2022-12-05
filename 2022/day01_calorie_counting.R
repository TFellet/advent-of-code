a <- rfp('2022','1') |> strtoi() # Read input as int
count <- hutilscpp::cumsum_reset(!is.na(a), a) # Cumsum for each Elf
count_unique <- count[!c(count[2:length(count)],0)] # Keep last value of each group
max(count_unique) # Part 1 (71502): Elf carrying the maximum
sum(count_unique[kit::topn(count_unique, 3)]) # Part 2 (208191): Top 3 elves
