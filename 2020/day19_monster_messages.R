library(stringi)

a <- rfp('2020','19')
sep <- which.max(a=='') # Find separation between rules and strings
strs <- a[(sep+1L):length(a)] # Strings to test
c('rules_ids', 'rules') %=% data.table::transpose(stri_split_fixed(a[1:(sep-1L)], ': ')) # Split rules and rules ids
r0id <- collapse::whichv(rules_ids, '0') # Find rule 0 (final rule to test)
r0 <- rules[r0id]
rules <- rules[-r0id] # Remove rule 0 from list of rules
rules_ids <- strtoi(rules_ids[-r0id]) # Convert ids to int
ord <- iorder(rules_ids) # Sort ids
rules_ord <- rep('', max(rules_ids)) # Init empty list (for examples to work)
rules_ord[rules_ids[ord]] <- paste0('( ', rules[ord], ' )') # Fill empty list with correctly ordered rules and add parenthesis

rules_ord[grepl('a', rules_ord, fixed = T)] <- 'a' # Change rule containing a to only a
rules_ord[grepl('b', rules_ord, fixed = T)] <- 'b' # Change rule containing b to only b

# Function to recursively insert rules instead of rules ids
expand <- \(str, rules) {
  continue <- T
  while(continue) {
    str_spl <- stri_split_fixed(str, ' ')[[1]] # Split on each space
    # Replace ids by their rules when possible and collapse result strings to a single string
    str <- stri_flatten(data.table::fcoalesce(rules[strtoi(str_spl)], str_spl), collapse = ' ')
    continue <- grepl('\\d', str, perl = T) # Keep looping while there are numbers
  }
  stri_c('^', stri_replace_all_fixed(str, ' ', ''), '$') # Remove spaces and add start / end of str in regex
}

reg <- expand(r0, rules_ord) # Expang rule 0 with set of rules
sum(grepl(reg, strs)) # Part 1 (126): Number of strings matching resulting pattern

rulesp2 <- rules_ord # Copy rules for part 2
rulesp2[8] <- '( 42 )+' # Replace rule 8 to match it any number of times
rulesp2[11] <- '(?P<recur> 42 (?&recur)? 31 )' # Replace rule 11 by a recursive regex

reg_2 <- expand(r0, rulesp2) # Expand rule 0 with new set of rules
sum(grepl(reg_2, strs, perl = T)) # Part 2 (282): Numbers of string matching second pattern
