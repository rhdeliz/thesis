library(stringr)

extract_after_first_parenthesis_period <- function(string) {
  pattern <- "\\)\\.\\s*(.*)"
  result <- str_match(string, pattern)
  if (!is.na(result[1,2])) {
    return(result[1,2])
  } else {
    return(NA)  # or return NULL or "" based on your preference
  }
}

remove_after_second_to_last_period <- function(string) {
  # Split the string by periods
  parts <- unlist(strsplit(string, "\\."))
  
  # If there are fewer than 2 parts, return the original string
  if (length(parts) < 2) return(string)
  
  # Remove the last segment
  parts <- parts[-length(parts)]
  
  # Join the segments back with periods
  return(paste0(parts, collapse = "."))
}

extract_before_last_comma <- function(string) {
  # Extract everything before the last comma
  return(sub(",[^,]*$", "", string))
}

extract_number <- function(string) {
  # The regex pattern looks for a comma, possibly followed by a space, 
  # then captures a sequence of digits that's followed by a period
  pattern <- ",\\s*(\\d+)\\."
  result <- str_extract(string, pattern)
  
  # The captured number is returned, or NA if not found
  return(ifelse(is.na(result), NA, gsub(",\\s*|\\.", "", result)))
}

# Helper function to safely extract matches
safe_extract <- function(pattern, text) {
  match <- regmatches(text, regexpr(pattern, text))
  if(length(match) == 0 || match[1] == "") return(NULL)
  match[1]
}

fix_author_entry <- function(entry) {
  # Split the string by comma
  parts <- unlist(strsplit(entry, ","))
  
  # Process each part, pairing last name with first name
  correct_authors <- c()
  for (i in seq(1, length(parts), by = 2)) {
    # Check if there's a next part to pair with
    if (i + 1 <= length(parts)) {
      author = paste0(parts[i], ",", parts[i + 1])
      correct_authors <- c(correct_authors, author)
    } else {
      # If there's no next part, just add the current one
      correct_authors <- c(correct_authors, parts[i])
    }
  }
  
  # Join the corrected authors using "and"
  correct_author_str <- paste(correct_authors, collapse = " and ")
  
  return(correct_author_str)
}


convert_apa_to_bib <- function(apa_string) {
  tryCatch({
    # Extract main parts
    author_part <- safe_extract("^(.+?)\\(", apa_string)
    author_part <- gsub(" \\(", "", author_part)
    author_part <- gsub("\\\\&", "", author_part)
    author_part <- fix_author_entry(author_part)
    author_part <- gsub("\\.\\.\\.","\\.\\.\\. and", author_part)
    author_part <- gsub("  ", " ", author_part)
    author_part <- gsub("  ", " ", author_part)
    
    year <- safe_extract("\\((\\d{4})\\)", apa_string)
    year <- gsub("\\(", "", year)
    year <- gsub("\\)", "", year)
    
    title_part <-  extract_after_first_parenthesis_period(apa_string)
    journal_part <- title_part
    title_part <- remove_after_second_to_last_period(title_part)
    journal_part <- gsub(title_part, "", journal_part, fixed = T)
    journal_part <- gsub("\\. ", "", journal_part)
    journal_entry <- journal_part
    
    if(is.null(
      safe_extract("\\w+,\\s*(\\d+)\\(", journal_part)
    )){
      journal_part <- extract_before_last_comma(journal_part)
      journal_part <- extract_before_last_comma(journal_part)
    } else{
      # journal_part <- safe_extract("\\w+,\\s*(\\d+)\\(", journal_part)
      journal_part <- extract_before_last_comma(journal_part)
      journal_part <- extract_before_last_comma(journal_part)
    }
    
    if(str_count(journal_entry, "\\,") ==1){
      volume <- strsplit(journal_entry, ",\\s*")[[1]][2]
    }
    
    if(str_count(journal_entry, "\\,") ==2){
      volume <- strsplit(journal_entry, ",\\s*")[[1]][2]
      pages <- strsplit(journal_entry, ",\\s*")[[1]][3]
      pages <- gsub("\\.", "", pages)
    }
    
    if(grepl("\\(", volume)){
      
      pattern <- "\\(([^)]+)\\)"
      number <- str_extract_all(volume, pattern)[[1]]
      number <- gsub("\\(", "", number)
      number <- gsub("\\)", "", number)
      
      volume <- safe_extract("\\s*(\\d+)\\(", volume)
      volume <- substr(volume, 1, nchar(volume) - 1)
    }

    # Construct bib key
    first_author <- if(!is.null(author_part)) unlist(strsplit(author_part, ", "))[1] else "unknown_author"
    bib_key <- paste0(first_author, "_", year)
    bib_key <- gsub(" ", "", bib_key)
    
    # Construct base bib entry
    bib_entry <- sprintf("@article{%s,\nauthor = {%s},\ntitle = {%s},\njournal = {%s}", bib_key, author_part, title_part, journal_part)
    
    # Append optional parts
    if(exists("volume")){
      if (!is.null(volume)) bib_entry <- paste0(bib_entry, sprintf(",\nvolume = {%s}", volume))
    }
    if(exists("number")){
      if (!is.null(number)) bib_entry <- paste0(bib_entry, sprintf(",\nnumber = {%s}", number))
    }
    if(exists("pages")){
      if (!is.null(pages)) bib_entry <- paste0(bib_entry, sprintf(",\npages = {%s}", pages))
    }
    if (!is.null(year)) bib_entry <- paste0(bib_entry, sprintf(",\nyear = {%s}", year))
    
    bib_entry <- paste0(bib_entry, "\n}")
    
    return(bib_entry)
  }, error = function(e) {})
}

# Example usage
apa_string <- "Adachi, O., Kawai, T., Takeda, K., Matsumoto, M., Tsutsui, H., Sakagami, M., ... & Akira, S. (1998). Targeted disruption of the MyD88 gene results in loss of IL-1-and IL-18-mediated function. Immunity, 9(1), 143-150."
print(convert_apa_to_bib(apa_string))

# Read the APA txt file
apa_txt_file <- "/Users/r/Downloads/thesis_references.txt"
references <- readLines(apa_txt_file)
references <- references[references!=""]

convert_apa_to_bib(references[[11]])

apa_string <- references[[11]]
# Convert each reference
bib_entries <- lapply(references, convert_apa_to_bib)

bib_entries <- bib_entries[!is.null(bib_entries)]  # Remove NULL entries

# # Write to a .bib file
bib_file <- "output_bib_file.bib"
writeLines(unlist(bib_entries), bib_file)
