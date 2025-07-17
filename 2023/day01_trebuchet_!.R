a <- rfp("2023", "1") # Equivalent of readLines
mfirst <- stringi::stri_match_first_regex
toint <- \(vec) data.table::fifelse(nchar(vec) == 1L, strtoi(vec), match(vec, num_str))
sumd <- \(x, y, toint = strtoi) sum(toint(x) * 10 + toint(y))
sumd(mfirst(a, "\\d"), stringi::stri_match_last_regex(a, "\\d")) # Part 1 (55971): Sum of first and last digit

num_str <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
reg <- paste0(num_str, collapse = "|") |> paste0("|\\d")
sumd(mfirst(a, reg), mfirst(a, paste0(".*(", reg, ")"))[, 2], toint) # Part 2 (54719): Sum of first and last digit including written digits
