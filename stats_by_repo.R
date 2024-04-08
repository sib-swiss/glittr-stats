library(httr2)
library(ggplot2)
library(dplyr)
library(ggbreak)
library(cowplot)

source(".env.R")

glittr_cols <- c(
  "Scripting and languages" = "#3a86ff",
  "Computational methods and pipelines" = "#fb5607",
  "Omics analysis" = "#ff006e",
  "Reproducibility and data management" = "#ffbe0b",
  "Statistics and machine learning" = "#8338ec",
  "Others" = "#000000")

p <- data.frame(category = factor(names(glittr_cols), levels = names(glittr_cols)), 
           color = glittr_cols) |>
  ggplot(aes(x = category, fill = category)) +
  scale_fill_manual(values = glittr_cols) +
  geom_bar()

legend <- get_legend(p)

grid::grid.newpage()
grid::grid.draw(legend)

# Create a client
parsed <- request("https://glittr.org/api/repositories") |>
  req_perform() |> resp_body_json()

repo_info_list <- lapply(parsed$data, function(x) data.frame(
  repo = x$name,
  author_name = x$author$name,
  stargazers = x$stargazers,
  recency = x$days_since_last_push,
  url = x$url,
  license = ifelse(is.null(x$license), "none", x$license),
  main_tag = x$tags[[1]]$name,
  main_category = x$tags[[1]]$category
))

repo_info <- do.call(rbind, repo_info_list)

repo_info$provider <- ifelse(grepl("github", repo_info$url), "github", "gitlab")

dir.create("data", showWarnings = FALSE)
n_contributors <- sapply(repo_info$repo[repo_info$provider == "github"], function(x) {
  
  # get repo contributors
  resp <- request("https://api.github.com/repos/") |>
    req_url_path_append(x) |>
    req_url_path_append("contributors") |>
    req_url_query(per_page = 1) |>
    req_headers(
      Accept = "application/vnd.github+json",
      Authorization = paste("Bearer", pat),
      `X-GitHub-Api-Version` = "2022-11-28",
    ) |>
    req_perform() 
  
  link_url <- resp_link_url(resp, "last")
  if(is.null(link_url)) {
    return(1)
  } else {
    npages <- strsplit(link_url, "&page=")[[1]][2] |> as.numeric()
    return(npages)
  }
})

saveRDS(n_contributors, "data/n_contributors.rds")

repo_info$contributors <- n_contributors[repo_info$repo]

sum(repo_info$contributors, na.rm = TRUE)
hist(repo_info$contributors, breaks = 100)

repo_info |>
  ggplot(aes(x = contributors)) +
  geom_histogram(fill = "white", col = "black", binwidth = 5) +
  scale_y_break(c(60, 150)) +
  theme_classic() +
  xlab("Number of contributors") +
  ylab("Number of repositories")

nna_contr <- repo_info$contributors[!is.na(repo_info$contributors)]
sum(nna_contr > 10)/length(nna_contr)
# 27.3% have more than 10 contributors
sum(nna_contr > 1)/length(nna_contr)
# 78.6% have more than one contributor
sum(nna_contr == 1)
# 115 repos with only one contributor
sum(nna_contr > 50)/length(nna_contr)

lic_freq_plot <- table(license = repo_info$license,
                       main_category = repo_info$main_category) |>
  as.data.frame()
lic_freq_plot |>
  ggplot(aes(x = reorder(license, Freq), y = Freq, fill = main_category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = glittr_cols) +
  theme_classic() +
  xlab("License type") +
  ylab("Number of repositories") +
  theme(legend.position = "none")


table(category = repo_info$main_category) |>
  as.data.frame() |>
  ggplot(aes(x = reorder(category, Freq), y = Freq, fill = category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = glittr_cols) +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Category") +
  ylab("Number of repositories")

author_freq <- table(author_name = repo_info$author_name, 
                     main_category = repo_info$main_category) |>
  as.data.frame()

repos_per_author <- table(repo_info$author_name)

lf_authors <- names(repos_per_author)[repos_per_author < 5]

author_freq |>
  filter(!author_name %in% lf_authors) |>
  arrange(Freq) |>
  ggplot(aes(x = reorder(author_name, Freq), y = Freq, fill = main_category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("author or organization") +
  ylab("number of repositories") +
  scale_fill_manual(values = glittr_cols) +
  annotate(geom = "text", x = 2, y = 30,
           label = paste("Authors with < 5 repos: ",
                         length(lf_authors)),
           color="black") +
  theme_classic() +
  theme(legend.position = "none")

hist(repo_info$recency, breaks = 100)
sum(repo_info$recency < 180)/nrow(repo_info)

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

# Give me an R function that takes as input two vectors.
# Test if they have exactly the same elements in both vectors.
# The elements in the vectors might have a different order. 
# The function should return TRUE if the vectors have the same elements,
idential_elements <- function(x, y) {
  identical(sort(x), sort(y))
}


