library(data.table)

a <- setDT(list(rfp('2020','7')))
a[,c('color_out', 'bag_list') := tstrsplit(V1, split=' bags? contain ', perl=T)] # Split container and inside bags
nspl <- max(stringi::stri_count_fixed(a$bag_list, ','))+1L # Max number of bags contained
bag_names <- paste0('B',1:nspl)
a[,(bag_names) := tstrsplit(bag_list, ', ', perl=T)] # Split bags inside
b <- melt(a, na.rm = T, id.vars = 'color_out', measure.vars = bag_names, value.name = 'color_in') # Pivot to get 1 bag inside per line
b[,color_in := gsub(' bag.*', '', color_in, perl=T)] # Remove useless chars
b[,number := strtoi(substr(color_in,1,1))] # Number of bags contained
b[,color_in := substr(color_in,3,nchar(color_in))]# # Remove number to get color

nas <- collapse::whichNA(b$number, invert = T)
full_list <- collapse::gsplit(b$number[nas], b$color_out[nas], use.g.names = T)
l2 <- collapse::gsplit(b$color_in[nas], b$color_out[nas])
collapse::setLabels(full_list, l2, 'names')

part1 <- \(b, first='shiny gold') {
  names_bags_ends <- levels(b$color_out) # Get unique colors
  bags_ends <- setNames(rep(F, length(names_bags_ends)), names_bags_ends) # Presence of each bag as container
  colors <- b$color_out[b$color_in == 'shiny gold']
  while (length(colors)>0) {
    bags_ends[colors] <- T # Mark bags as valid containers
    colors <- b$color_out[b$color_in %chin% colors]
  }
  sum(bags_ends) 
}
part1(b) # Part 1: How many bag colors can eventually contain at least one shiny gold bag

part2 <- \(ref, first='shiny gold') {
  colors <- ref[[first]]
  total <- 0L # Count of bags contained
  while (length(colors)>0) {
    total <- total + sum(colors) # Add sum of bags to total
    colors <- Map("*", colors, ref[names(colors)]) |> unname() |> unlist()
  }
  total
}
part2(full_list) # Part 2: How many individual bags are required inside your single shiny gold bag
