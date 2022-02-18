a <- rfp('2020','18')
# Redefine custom infix operators. They have precedence over all other operators
`%+%` <- `+` # Priority +
`%*%` <- `*` # Priority *
a2 <- stringi::stri_replace_all_fixed(a, '+', '%+%') # Replace all + by priority +
a3 <- stringi::stri_replace_all_fixed(a2, '*', '%*%') # Replace all * by priority *

ans <- sapply(a3, \(str) eval(parse(text=str)), USE.NAMES = F) # Evaluate expressions with priority + and * (no precedence)
sum(ans) # Part 1 (23507031841020): Sum of evaluated expressions with no precedence between + and *

ans2 <- sapply(a2, \(str) eval(parse(text=str)), USE.NAMES = F) # Evaluate expressions with only priority +
sum(ans2) # Part 2 (218621700997826): Sum of evaluated expressions with + having precedence over *
