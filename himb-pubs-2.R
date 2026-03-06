#

# Install if needed
install.packages(c("httr","jsonlite","dplyr","purrr","stringr","stringi"))

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(stringr)
library(stringi)
library(ggplot2)

# ---------------------------------------
# Helper: fetch all pages from OpenAlex
# ---------------------------------------

fetch_openalex <- function(query){
  
  base <- "https://api.openalex.org/works"
  cursor <- "*"
  results <- list()
  
  repeat {
    
    url <- paste0(
      base,
      "?filter=", query,
      "&per-page=200",
      "&cursor=", cursor
    )
    
    r <- GET(url)
    dat <- fromJSON(content(r, "text", encoding="UTF-8"), simplifyVector = FALSE)
    
    results <- append(results, dat$results)
    
    cursor <- dat$meta$next_cursor
    
    if(is.null(cursor)) break
  }
  
  return(results)
}

# ---------------------------------------
# 1. Works affiliated with University of Hawai‘i
# ---------------------------------------

# OpenAlex institution ID for University of Hawai‘i at Mānoa
UHM_ID <- "I117965899"

# uh_works <- fetch_openalex(
#   paste0("institutions.id:", UHM_ID)
# )
# 
uh_works <- load("all_works.RData")
uh_works <- works
# ---------------------------------------
# 2. Direct affiliation search for HIMB
# ---------------------------------------

himb_variants <- c(
  "Hawaii Institute of Marine Biology",
  "Hawai‘i Institute of Marine Biology",
  "Hawai'i Institute of Marine Biology",
  "Moku o Lo'e",
  "Moku o Loe",
  "Moku o Loʻe",
  "HIMB"
)

variant_queries <- paste0("search:", URLencode(himb_variants))

himb_search_results <- map(
  variant_queries,
  fetch_openalex
)

himb_search_results <- flatten(himb_search_results)

# ---------------------------------------
# 3. Combine results
# ---------------------------------------

all_works <- c(uh_works, himb_search_results)

# ---------------------------------------
# 4. Deduplicate by OpenAlex work ID
# ---------------------------------------

get_id <- function(w){
  str_replace(w$id,"https://openalex.org/","")
}

ids <- map_chr(all_works, get_id)

unique_indices <- !duplicated(ids)

works <- all_works[unique_indices]

# ---------------------------------------
# 5. Detect HIMB affiliations
# ---------------------------------------

is_himb <- function(w){
  
  # Normalize text: remove okina etc, lowercase
  normalize <- function(x){
    stringi::stri_trans_general(x, "Latin-ASCII") |>
      tolower()
  }
  
  # Keywords to identify HIMB
  himb_terms <- c(
    "hawaii institute of marine biology",
    "hawai'i institute of marine biology",
    "himb",
    "kaneohe",
    "moku o loe",
    "moku o lo'e"
  )
  
  # Loop over authors
  for(a in w$authorships){
    
    # ---- raw affiliation strings ----
    if(!is.null(a$raw_affiliation_strings) && length(a$raw_affiliation_strings) > 0){
      aff <- normalize(paste(a$raw_affiliation_strings, collapse=" "))
      if(any(str_detect(aff, himb_terms))) return(TRUE)
    }
    
    # ---- institutions (safely) ----
    if(!is.null(a$institutions) && length(a$institutions) > 0){
      inst_names <- map_chr(
        a$institutions,
        ~ if(!is.null(.x$display_name)) .x$display_name else NA_character_
      )
      inst <- normalize(paste(inst_names, collapse=" "))
      
      if(any(str_detect(inst, himb_terms))) return(TRUE)
    }
  }
  
  return(FALSE)
}

# ----------------------------
# Filter your works
# ----------------------------
# `works` is your OpenAlex works list
himb_works <- purrr::keep(works, is_himb)

cat("Total HIMB works detected:", length(himb_works), "\n")

# ---------------------------------------
# 6. Convert to dataframe
# ---------------------------------------

library(dplyr)
library(purrr)
library(stringr)
library(stringi)

# Helper function: convert "First Middle Last" → "Last F.M."
format_author <- function(name){
  parts <- str_split(name, " +")[[1]]
  if(length(parts) == 0) return(name)
  last <- parts[length(parts)]
  initials <- paste0(substr(parts[-length(parts)],1,1), collapse="")
  paste0(last, " ", initials)
}

# ----------------------------
# Build works_df
# ----------------------------
works_df <- tibble(
  authors = map_chr(himb_works, function(w){
    a <- map_chr(w$authorships, ~.x$author$display_name)
    formatted <- map_chr(a, format_author)
    if(length(formatted) > 5){
      paste0(paste(formatted[1:5], collapse=", "), " et al.")
    } else {
      paste(formatted, collapse=", ")
    }
  }),
  year = map_dbl(himb_works, ~.x$publication_year %||% NA),
  title = map_chr(himb_works, ~.x$title %||% ""),
  journal = map_chr(himb_works, function(w){
    if(!is.null(w$host_venue$display_name) && w$host_venue$display_name != ""){
      w$host_venue$display_name
    } else if(!is.null(w$primary_location$source$display_name) && w$primary_location$source$display_name != ""){
      w$primary_location$source$display_name
    } else if(!is.null(w$biblio$journal_title) && w$biblio$journal_title != ""){
      w$biblio$journal_title
    } else {
      "Unknown Venue"
    }
  }),
  doi = map_chr(himb_works, function(w){
    if(!is.null(w$doi) && nchar(w$doi) > 0){
      paste0("[", w$doi, "](https://doi.org/", w$doi, ")")
    } else {
      ""
    }
  }),
  type = map_chr(himb_works, ~.x$type %||% "other")
)


# Sort newest → oldest
works_df <- works_df %>%
  filter(!is.na(year)) %>%
  arrange(desc(year))

# ----------------------------
# Generate Markdown references
# ----------------------------
markdown_list <- map_chr(1:nrow(works_df), function(i){
  paste0(
    works_df$authors[i], " (", works_df$year[i], "). ",
    works_df$title[i], ". ",
    works_df$journal[i], ". ",
    works_df$doi[i],
    " (", works_df$type[i], ")"
  )
})

markdown_output <- paste(markdown_list, collapse = "\n\n")

# ----------------------------
# Save to file
# ----------------------------
writeLines(markdown_output, "README.md")
cat("Saved HIMB publication references (newest→oldest) to HIMB_publications_references.md\n")


# install if needed

# -------------------------------------
# Count publications per year
# -------------------------------------

articles_df <- works_df %>%
  filter(type %in% c("article", "preprint"))

pubs_per_year <- articles_df %>%
  filter(!is.na(year)) %>%
  group_by(year) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(year)

# -------------------------------------
# Plot
# -------------------------------------

ggplot(pubs_per_year, aes(x = year, y = n)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Number of Publications"
  ) +
  geom_smooth(se = FALSE)


ggsave("figure.png", width = 8, height = 2, dpi = 300)

