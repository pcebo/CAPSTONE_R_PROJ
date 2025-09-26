# 1. Define the directory path and the specific file to search
dir_path <- "C:/Users/4xP1600x/Desktop/COURSERA/CAPSTONE/swiftkey_dataset/en_US"
twitter_file_name <- "en_US.twitter.txt"
full_twitter_path <- file.path(dir_path, twitter_file_name)

# 2. Print a status message to the console
print(paste("--- Searching for 'biostats' in", twitter_file_name, "---"))

# 3. Check if the file exists before attempting to read it
if (file.exists(full_twitter_path)) {
  
  # Read all lines from the file, handling potential errors
  lines <- tryCatch({
    readLines(full_twitter_path, warn = FALSE, encoding = "UTF-8")
  }, error = function(e) {
    message(paste("An error occurred while reading the file:", e$message))
    return(NULL) # Return NULL on error
  })
  
  # Proceed only if the file was read successfully
  if (!is.null(lines)) {
    
    # 4. Find all lines containing "biostats" (case-insensitive)
    # The `grepl` function returns a logical vector (TRUE/FALSE)
    # We use that vector to select the matching lines from the original 'lines' vector
    matching_lines <- lines[grepl("biostats", lines, ignore.case = TRUE)]
    
    # 5. Check if any matches were found and print the result
    if (length(matching_lines) > 0) {
      print("Found the following matching line(s):")
      
      # Use cat() with a newline separator for cleaner output, especially for multiple lines
      cat(matching_lines, sep = "\n")
      
    } else {
      print("The word 'biostats' was not found in the file.")
    }
  }
  
} else {
  # This message is printed if the file is not found at the specified path
  warning(paste("File not found. Please check the path:", full_twitter_path))
}
