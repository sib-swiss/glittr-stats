library(httr2)

source(".env.R")

# Create a client
parsed <- request("https://glittr.org/api/repositories") |>
  req_perform() |> resp_body_json()

repo_info_list <- lapply(parsed$data, function(x) data.frame(
  repo = x$name,
  author_name = x$author$name,
  stargazers = x$stargazers,
  recency = x$days_since_last_push,
  url = x$url))

repo_info <- do.call(rbind, repo_info_list)

repo_info$provider <- ifelse(grepl("github", repo_info$url), "github", "gitlab")

author_freq <- table(repo_info$author_name)
author_freq <- author_freq[order(author_freq, decreasing = TRUE)]

starsum <- tapply(repo_info$stargazers, repo_info$author_name, sum)
starsum <- starsum[order(starsum, decreasing = TRUE)]

sum(repo_info$stargazers > 1000)
sum(repo_info$stargazers > 100)

author_info_list <- list()
gh_authors <- repo_info$author_name[repo_info$provider == "github"] |> unique()
for(author in gh_authors) {
  
  parsed <- request("https://api.github.com/users/") |>
    req_url_path_append(author) |>
    req_headers(
      Accept = "application/vnd.github+json",
      Authorization = paste("Bearer", pat),
      `X-GitHub-Api-Version` = "2022-11-28",
    ) |>
    req_perform() |> resp_body_json()
  author_info_list[[author]] <- data.frame(
    author = parsed$login,
    type = parsed$type,
    name = ifelse(is.null(parsed$name), NA, parsed$name),
    location = ifelse(is.null(parsed$location), NA, parsed$location)
  )
}

author_info <- do.call(rbind, author_info_list)

author_info_loc <- author_info[!is.na(author_info$location), ]

author_loc <- author_info_loc$location
names(author_loc) <- author_info_loc$author

loc_info <- ggmap::geocode(author_loc,
                           output = 'all')

get_country <- function(loc_results) {
  
  if("results" %in% names(loc_results)) {

    for(results in loc_results$results) {
      address_info <- results$address_components |> 
        lapply(unlist) |> 
        do.call(rbind, args = _) |>
        as.data.frame()
      country <- address_info$long_name[address_info$types1 == "country"]
      if (length(country) == 0) next
    }
    
    if (length(country) == 0) return(NA)
    
    return(country)
  } else {
    return(NA)
  }

}

countries <- sapply(loc_info, get_country)
names(countries) <- names(author_loc)

author_info$country <- countries[author_info$author]

repo_info <- merge(repo_info, author_info, by.x = "author_name", by.y = "author")

country_counts <- table(repo_info$country)
country_counts <- country_counts[order(country_counts, decreasing = TRUE)]
length(country_counts)
