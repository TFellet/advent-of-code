library(data.table)
library(fastmatch)

a <- rfp('2020','21')

# Split list into ingredients and allergens
c('ingredients', 'allergens') %=% tstrsplit(a, ' (contains ', fixed=T)
allergens <- stringi::stri_sub(allergens, 1, -2) # Remove last )
ingredients_l <- stringi::stri_split_fixed(ingredients, ' ') # Split into individual ingredients
allergens_l <- strsplit(allergens, ', ', fixed = T) # Split into individual allergens

# Unlist all ingredients / allergens into a data table
al_dt <- fDT(id=rep(seq_along(allergens_l),lengths(allergens_l)), al=unlist(allergens_l))
ing_dt <- fDT(id=rep(seq_along(ingredients_l),lengths(ingredients_l)), ing=unlist(ingredients_l))

dt <- al_dt[ing_dt, on='id', allow.cartesian=T] # Join all allergens and all ingredients

# For each allergen, count how many time it is present with each ingredient
counts <- collapse::BY(dt$ing, dt$al, kit::countOccur, return='list')

# Keep only ingredients presents every time the allergen is present
filter_ing <- rbindlist(lapply(counts, \(x) collapse::ss(x, x$Count==max(x$Count),c(1,2))),idcol=T)
setnames(filter_ing, c('al', 'ing', 'N'))

collapse::fnrow(ing_dt)-sum(ing_dt$ing %fin% filter_ing$ing) # Part 1 (2307): Ingredients without any allergens

n <- kit::uniqLen(filter_ing$ing) # Number of differents allergens

corresp <- matrix('',n,2) # Result matrix
filter_ing2 <- collapse::qM(filter_ing[,c('ing', 'al')]) # Matrix with ingredients and allergens left

for (i in 1:(n-1)) { # For each allergen
  cur <- kit::countOccur(filter_ing2[,2]) # Count in how many ingredient each allergen is present
  cur_al <- cur$Variable[which.min(cur$Count)] # Work on the allergen present in only 1 ingredient
  cur_id <- filter_ing2[,2] == cur_al # Line of allergen
  cur_ing <- filter_ing2[cur_id,1] # Ingredient corresponding to allergen
  corresp[i,] <- c(cur_ing, filter_ing2[cur_id,2]) # Store in result matrix
  filter_ing2 <- filter_ing2[filter_ing2[,1] != cur_ing,] # Remove all lines where ingredient is present
}
corresp[n,] <- filter_ing2 # Add last pair

# Part 2 (cljf,frtfg,vvfjj,qmrps,hvnkk,qnvx,cpxmpc,qsjszn): Ingredients ordered by contained allergens
stringi::stri_flatten(isort(corresp[,1], by=corresp[,2]), ',')
