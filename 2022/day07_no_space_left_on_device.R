a <- rfp('2022','7') # Read input

path <- 'root' # Initialize the current path to the root directory
dir_size <- list() # Initialize an empty list to store the size of each directory

# For each line in the file system data
for (spl in strsplit(a, ' ', fixed = T)) {
  if(spl[1] == '$') { # Line is a command
    if(spl[2] == 'cd') { # Command is change directories
      # Update the current path based on the command arguments
      path <- switch (spl[3],
                      '/' = 'root', # If the argument is '/', set the path to 'root'
                      '..' = dirname(path), # If the argument is '..', set the path to the parent directory
                      paste0(path, '/', spl[3]) ) # Otherwise, append the argument to the current path
    }
  } else if(spl[1] != 'dir') { # If the line is not a directory
    tmpath <- path # Create a temporary variable to store the current path
    # Iterate over each directory in the path from the current directory up to the root
    while (tmpath != '.') {
      # Initialize the size of the current directory in the path if it hasn't been seen yet
      if(length(dir_size[[tmpath]]) == 0) dir_size[[tmpath]] <- 0L
      # Add the size of the current file to the size of the current directory in the path
      dir_size[[tmpath]] <- dir_size[[tmpath]] + strtoi(spl[1])
      tmpath <- dirname(tmpath) # Move up to the parent directory in the path
    }
  }
}

dir_size2 <- unlist(dir_size) # Flatten the directory sizes into a single vector
sum(dir_size2[dir_size2 < 100000]) # Part 1 (1307902): sum of the sizes of all directories smaller than 100000

goal_dir <- 30000000 - 70000000 + dir_size2['root'] # Target size to free
min(dir_size2[dir_size2 > goal_dir]) # Part 2 (7068748): Smallest folder to delete to free up enough space
