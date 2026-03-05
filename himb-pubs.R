install.packages(c("httr","jsonlite","dplyr","purrr","stringr","stringi"))

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(stringr)
library(stringi)

# SOEST OpenAlex institution ID
inst_id <- "I117965899"
cursor <- "*"
all_results <- list()

# repeat {
#   
#   res <- GET(
#     "https://api.openalex.org/works",
#     query=list(
#       filter=paste0("institutions.id:", inst_id),
#       per_page=200,
#       cursor=cursor
#     )
#   )
#   
#   dat <- fromJSON(content(res,"text",encoding="UTF-8"), simplifyVector = FALSE)
#   
#   all_results <- append(all_results, dat$results)
#   
#   cursor <- dat$meta$next_cursor
#   
#   if(is.null(cursor)) break
# }
# 
# cat("Total works retrieved:", length(all_results), "\n")

# works <- all_results
# 
# save(works, file="all_works.RData")

load("all_works.RData")

# HIMB

has_himb <- function(authorships){
  
  aff_strings <- unlist(
    lapply(authorships, function(a) a$raw_affiliation_strings)
  )
  
  if(length(aff_strings) == 0) return(FALSE)
  
  aff_strings <- stri_trans_general(aff_strings,"Latin-ASCII")
  aff_strings <- tolower(aff_strings)
  
  any(str_detect(
    aff_strings,
    "hawaii institute of marine biology|himb|coconut island"
  ))
}

himb_idx <- map_lgl(works, function(w) has_himb(w$authorships))
himb_works <- works[himb_idx]

cat("HIMB works detected:", length(himb_works), "\n")

# Refs

format_reference <- function(w){
  
  authors <- sapply(
    w$authorships,
    function(a) a$author$display_name
  )
  
  if(length(authors) > 3){
    author_str <- paste0(authors[1]," et al.")
  } else {
    author_str <- paste(authors, collapse=", ")
  }
  
  year <- w$publication_year
  title <- w$title
  
  journal <- ""
  if(!is.null(w$primary_location$source$display_name)){
    journal <- w$primary_location$source$display_name
  }
  
  doi <- gsub("https://doi.org/","",w$doi)
  
  paste0(
    author_str,
    " (",year,"). ",
    title,". ",
    journal,
    ". doi:",doi
  )
}


n <- min(10, length(himb_works))

refs <- map_chr(1:n, function(i){
  format_reference(himb_works[[i]])
})

cat(paste0(seq_along(refs), ". ", refs, collapse="\n"))

###



# ---------------------------------------------------
# Convert himb_works list to data frame
# ---------------------------------------------------

works_df <- tibble(
  title = sapply(himb_works, function(w) w$title),
  year = sapply(himb_works, function(w) {
    y <- w$publication_year
    if(is.null(y)) return(NA_real_) else return(as.numeric(y))
  }),
  doi = sapply(himb_works, function(w) gsub("https://doi.org/", "", w$doi)),
  type = sapply(himb_works, function(w) w$type),
  authors = sapply(himb_works, function(w) {
    auth <- sapply(w$authorships, function(a) a$author$display_name)
    if(length(auth) > 3){
      paste0(auth[1], " et al.")
    } else {
      paste(auth, collapse=", ")
    }
  })
)

# Remove works with missing year (optional)
works_df <- works_df %>% filter(!is.na(year))

# Sort most recent first
works_df <- works_df %>% arrange(desc(year))

# ---------------------------------------------------
# Assign colors by document type
# Customize as desired
# ---------------------------------------------------
doc_type_colors <- c(
  "journal-article" = "green",
  "book-chapter" = "blue",
  "monograph" = "purple",
  "dataset" = "orange",
  "other" = "gray"
)

works_df$color <- doc_type_colors[works_df$type]
works_df$color[is.na(works_df$color)] <- "gray"  # default for unknown types

# ---------------------------------------------------
# Generate Markdown list
# ---------------------------------------------------
markdown_list <- map_chr(1:nrow(works_df), function(i){
  paste0(
    "- <span style='color:", works_df$color[i], "'>",
    works_df$year[i], " – ",
    works_df$authors[i], " – ",
    "[", works_df$title[i], "](https://doi.org/", works_df$doi[i], ")",
    " (", works_df$type[i], ")",
    "</span>"
  )
})

markdown_output <- paste(markdown_list, collapse="\n")

# Save to file
writeLines(markdown_output, con = "README.md")
cat("HIMB publication list saved to: HIMB_publications.md\n")
