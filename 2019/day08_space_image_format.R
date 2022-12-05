a <- rfp('2019','8') # Read image data
layers <- toGrid(a, 25L*6L) # Convert data to int and put each layer in a single line
tab <- tabulate(layers[which.min(matrixStats::rowCounts(layers, value=0L)),]) # Find the layer with the lowest amount of 0
tab[1]*tab[2] # Part 1 (2250): In the found layer, multiply the number of 1's and 2's

pixels <- collapse::dapply(layers != 2L, which.max) # Find first visible pixel in each layer
# Fetch all visible pixels to create the final 25*6 pixels image
image <- matrixStats::colCollapse(layers, pixels) |> matrix(byrow = T,ncol = 25L)

# Optional: Pretty print to see password more clearly
# library(ggplot2)
# ggplot(data.frame(which(image==1L,arr.ind = T)), aes(col, -row)) + geom_tile() + coord_equal() + theme_void()
