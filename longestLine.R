# 1. Define the directory path and file names
# Note: R uses forward slashes "/" for file paths, even on Windows.
dir_path <- "C:/Users/4xP1600x/Desktop/COURSERA/CAPSTONE/swiftkey_dataset/en_US"
file_names <- c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt") # Corrected from en_US.newss.txt

# 2. Create the full file paths
full_paths <- file.path(dir_path, file_names)

# 3. Initialize variables to hold the max length and the corresponding file name
overall_max_length <- 0
file_with_longest_line <- "" # This will store the name of the file

# 4. Loop through each file to find the longest line
for (file in full_paths) {
  # Check if the file exists before trying to read it
  if (!file.exists(file)) {
    warning(paste("File not found:", file))
    next # Skip to the next file in the loop
  }
  
  # Read all lines from the current file
  # Using tryCatch to handle potential errors during file reading
  lines <- tryCatch({
    readLines(file, warn = FALSE, encoding = "UTF-8")
  }, error = function(e) {
    message(paste("Error reading file:", file, "-", e$message))
    return(NULL) # Return NULL if an error occurs
  })
  
  if (is.null(lines)) {
    next # Skip if the file could not be read
  }
  
  # Find the maximum line length in the current file
  # nchar() calculates the number of characters for each line
  max_in_file <- max(nchar(lines))
  
  # Check if this file's max is greater than the overall max seen so far
  if (max_in_file > overall_max_length) {
    overall_max_length <- max_in_file
    file_with_longest_line <- file # Update the file name when a new max is found
  }
}

# 5. Print the final result
if (file_with_longest_line != "") {
  # basename() is used to extract just the filename from the full path
  print(paste("The longest line is in the file:", basename(file_with_longest_line)))
  print(paste("The length of this line is:", overall_max_length, "characters."))
} else {
  print("Could not process any files or files were empty.")
}
