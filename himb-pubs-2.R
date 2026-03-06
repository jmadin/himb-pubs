#

# Install if needed
install.packages(c("httr","jsonlite","dplyr","purrr","stringr","stringi"))

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(stringr)
library(stringi)

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

works_df <- tibble(
  title = map_chr(himb_works, ~.x$title %||% ""),
  year = map_dbl(himb_works, ~.x$publication_year %||% NA),
  doi = map_chr(himb_works, ~str_replace(.x$doi %||% "", "https://doi.org/","")),
  type = map_chr(himb_works, ~.x$type %||% "other"),
  authors = map_chr(himb_works, function(w){
    
    a <- map_chr(w$authorships, ~.x$author$display_name)
    
    if(length(a) > 3){
      paste0(a[1], " et al.")
    } else {
      paste(a, collapse=", ")
    }
  })
)

# ---------------------------------------
# 7. Sort newest → oldest
# ---------------------------------------

works_df <- works_df %>%
  filter(!is.na(year)) %>%
  arrange(desc(year))

# ---------------------------------------
# 8. Color document types
# ---------------------------------------

doc_colors <- c(
  "journal-article"="green",
  "book-chapter"="blue",
  "dataset"="orange",
  "monograph"="purple"
)

works_df$color <- doc_colors[works_df$type]
works_df$color[is.na(works_df$color)] <- "gray"

# ---------------------------------------
# 9. Generate Markdown
# ---------------------------------------

md <- map_chr(1:nrow(works_df), function(i){
  
  paste0(
    "- <span style='color:", works_df$color[i], "'>",
    works_df$year[i], " – ",
    works_df$authors[i], " – ",
    "[", works_df$title[i], "](https://doi.org/", works_df$doi[i], ")",
    " (", works_df$type[i], ")",
    "</span>"
  )
})

markdown_output <- paste(md, collapse="\n")

writeLines(markdown_output, "README.md")

cat("Saved HIMB publication list to HIMB_publications.md\n")


#



# install if needed

library(dplyr)
library(ggplot2)

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


ggsave("HIMB_publications_per_year.png", width = 8, height = 4, dpi = 300)
