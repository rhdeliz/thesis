# Function to extract text inside parentheses from a string
extract_text_inside_parentheses <- function(input_string) {
  
  # Use regular expression to match content inside parentheses
  matches <- regmatches(input_string, gregexpr("\\(([^)]+)\\)", input_string))
  
  # Unlist to get the vector of matched strings
  extracted_texts <- unlist(matches)
  
  # Remove the parentheses from the matched strings
  extracted_texts <- gsub("\\(", "", extracted_texts)
  extracted_texts <- gsub("\\)", "", extracted_texts)
  
  return(extracted_texts)
}

# Read the content of the text file
file_path <- "/Users/r/Downloads/thesis_v3.txt"  # replace with your file's path
file_content <- readLines(file_path, warn = FALSE)

# Extract text inside parentheses from each line
all_matches <- lapply(file_content, extract_text_inside_parentheses)

# Combine all extracted texts
all_texts <- unlist(all_matches)

# Split by semicolon
all_texts <- unlist(strsplit(all_texts, ";"))

# Must contain year
year_strings <- all_texts[grepl("18[0-9]{2}|19[0-9]{2}|200[0-9]|201[0-9]|202[0-9]|2030", all_texts)]

# Must contain comma
comma_strings <- year_strings[grepl("\\,", year_strings)]

# Remove leading spaces
trimmed_string <- sub("^\\s+", "", comma_strings)

# Resort
sorted_strings <- sort(trimmed_string)

# No duplicate entries
sorted_strings <- unique(sorted_strings)

short_strings <- sorted_strings[sapply(strsplit(sorted_strings, "\\s+"), length) < 5]
print(short_strings)

short_strings[grepl("\\&", short_strings)]

Entries <- as.data.frame(short_strings)
names(Entries) <- "original"

library(dplyr)

ref_list <- 
  Entries %>% 
  mutate(
    id = 1:n()
  ) %>% 
  group_by(
    id
  ) %>% 
  mutate(
    last_name = sub(",.*", "", original),
    year = sub(".*, ", "", original)
  ) %>% 
  mutate(
    last_name = gsub(" et al.", "", last_name)
  ) %>% 
  mutate(
    last_name = ifelse(grepl(" \\\\& ", last_name), sub(" \\\\& .*", "", last_name), last_name)
  )


fix_ref_list <-
  ref_list %>% 
  mutate(
    last_name = gsub("\\'", "", last_name)
  ) %>% 
  mutate(
    last_name = gsub(" ", "", last_name)
  ) %>% 
  mutate(
    last_name = gsub("é", "e", last_name)
  ) %>% 
  mutate(
    last_name = gsub("á", "a", last_name)
  ) %>% 
  mutate(
    new = paste0("\\autocite{",last_name, "_", year, "}")
  ) %>% 
  ungroup() %>% 
  select(
    original,
    new
  )

fix_ref_list_both <-
  fix_ref_list %>% 
  mutate(
    original = paste0("(", original, ")")
  ) 

fix_ref_list_open <-
  fix_ref_list %>% 
  mutate(
    original = paste0("(", original, "; ")
  ) 

fix_ref_list_close1 <-
  fix_ref_list %>% 
  mutate(
    original = paste0(original, ")")
  ) 

fix_ref_list_close2 <-
  fix_ref_list %>% 
  mutate(
    original = paste0("; ", original, ")")
  ) 

fix_ref_list_sandwich <-
  fix_ref_list %>% 
  mutate(
    original = paste0("; ", original, "; ")
  ) 

fix_ref_list <- bind_rows(fix_ref_list_both, fix_ref_list_open, fix_ref_list_sandwich, fix_ref_list_close2, fix_ref_list_close1, fix_ref_list)

library(readr)

# 1. Read the txt file into R
txt_file_path <- file_path
text <- read_lines(txt_file_path)

# 3. Perform the replacements
for (i in 1:nrow(fix_ref_list)) {
  pattern <- fix_ref_list$original[i]
  replacement <- fix_ref_list$new[i]
  text <- gsub(pattern, replacement, text, fixed = T)
}

# Function to extract text inside \autocite{} from a given string
extract_autocite_text <- function(string) {
  # Use a regular expression to match and extract the desired text
  matches <- regmatches(string, gregexpr("\\\\autocite\\{([^\\}]+)\\}", string))
  
  # Since regmatches returns a list, unlist it to get a character vector
  unlisted_matches <- unlist(matches)
  
  # Extract the actual citation text from each match
  citation_texts <- gsub("\\\\autocite\\{([^\\}]+)\\}", "\\1", unlisted_matches)
  
  return(citation_texts)
}

citations <- unlist(lapply(text, extract_autocite_text))
citations <- unique(citations)



# 4. Write the modified text back to the txt file (or to a new file if you prefer)
file.remove("/Users/r/Downloads/modified_txt_file.txt")
write_lines(text, "/Users/r/Downloads/modified_txt_file.txt")

