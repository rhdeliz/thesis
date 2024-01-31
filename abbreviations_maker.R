# Required library
# install.packages("stringr")
library(stringr)

# Function to extract abbreviations
extract_abbreviations <- function(filename) {
  
  # Read the file
  text <- readLines(filename, warn = FALSE)
  
  # Concatenate lines into a single string
  text_combined <- paste(text, collapse = " ")
  
  # Use a regular expression to match abbreviations
  # This regex looks for sequences of uppercase letters possibly mixed with numbers.
  abbreviations <- str_extract_all(text_combined, "\\b[A-Z0-9]+[A-Z][A-Z0-9]*\\b")[[1]]
  # abbreviations <- str_extract_all(text_combined, "\\b[A-Z0-9-]+[A-Z][A-Z0-9-]*\\b")[[1]]
  
  # Return unique abbreviations
  return(unique(abbreviations))
}

# Test the function
filename <-  "/Users/r/Downloads/thesis_v3.txt"
abbreviations <- extract_abbreviations(filename)
print(abbreviations)
