# 1. Define the directory path and file names
# Note: R uses forward slashes "/" for file paths, even on Windows.
dir_path <- "C:/Users/4xP1600x/Desktop/COURSERA/CAPSTONE/swiftkey_dataset/en_US"
file_names <- c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt") 

# Love/Hate Ratio Analysis 

# 1. Define the path to the specific twitter file
twitter_file_path <- file.path(dir_path, "en_US.twitter.txt")

# 2. Check if the twitter file exists and process it
if (file.exists(twitter_file_path)) {
  
  # Read all lines from the twitter file
  twitter_lines <- readLines(twitter_file_path, warn = FALSE, encoding = "UTF-8")
  
  # Count lines containing "love" and "hate" (case-sensitive lowercase only, whole word)
  # The \\b ensures we match the whole word "love" and not "glove"
  love_count <- sum(grepl("\\blove\\b", twitter_lines, ignore.case = FALSE))
  hate_count <- sum(grepl("\\bhate\\b", twitter_lines, ignore.case = FALSE))
  
  # 8. Calculate and print the ratio
  print("--- Twitter Data Analysis ---")
  print(paste("Lines containing 'love':", love_count))
  print(paste("Lines containing 'hate':", hate_count))
  
  # Avoid division by zero
  if (hate_count > 0) {
    love_hate_ratio <- love_count / hate_count
    print(paste("The 'love' to 'hate' ratio is:", round(love_hate_ratio, 2)))
  } else {
    print("Cannot calculate ratio because the 'hate' count is zero.")
  }
  
} else {
  warning(paste("Twitter file not found at:", twitter_file_path))
}


