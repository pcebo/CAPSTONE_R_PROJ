# 1. Define the directory path and the specific file to search
dir_path <- "C:/Users/4xP1600x/Desktop/COURSERA/CAPSTONE/swiftkey_dataset/en_US"
twitter_file_name <- "en_US.twitter.txt"
full_twitter_path <- file.path(dir_path, twitter_file_name)

# 2. Define the exact sentence we are looking for
target_sentence <- "A computer once beat me at chess, but it was no match for me at kickboxing."

# 3. Print a status message to the console
print(paste("--- Counting exact matches for a specific sentence in", twitter_file_name, "---"))

# 4. Check if the file exists before attempting to read it
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
    
    # 5. Count the number of lines that are an exact match
    # The `==` operator performs a case-sensitive, element-by-element comparison.
    # The `sum()` function then counts the number of TRUE results.
    exact_match_count <- sum(lines == target_sentence)
    
    # 6. Print the final count
    print(paste("The sentence was found", exact_match_count, "time(s)."))
    
  }
  
} else {
  # This message is printed if the file is not found at the specified path
  warning(paste("File not found. Please check the path:", full_twitter_path))
}

