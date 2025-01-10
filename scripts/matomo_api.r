library(httr2)
library(dplyr)

url <- "https://matomo.sib.swiss/?module=API"
token <- "YOUR_TOKEN"

# &method=Actions.getPageTitles&idSite=217&period=day&date=yesterday&format=json

# url <- "https://demo.matomo.cloud/?module=API&method=API.getMatomoVersion&format=xml"
# token <- "YOUR_TOKEN"

# url <- 'https://demo.matomo.cloud/?module=API&method=Actions.getPageTitles&idSite=1&date=yesterday'

# Create and send the request
response <- request(url) |>
  req_body_form(
    method = "Actions.getOutlinks",
    idSite = 217,
    format = "json",
    date = "today",
    period = "year",
    expanded = 1,
    filter_limit = -1,
    token_auth = token
  ) |>
  req_perform() |>
  resp_body_json()

outlinks_list <- list()
for(domain in response) {
  label <- domain$label
  url_info <- lapply(domain$subtable, function(x) {
    data.frame(
      url = ifelse(is.null(x$url),NA , x$url),
      nb_visits = ifelse(is.null(x$nb_visits),NA , x$nb_visits),
      domain = label
    )
  })
  outlinks_list[[domain$label]] <- do.call(rbind, url_info)
}

outlinks_df <- do.call(rbind, outlinks_list)
row.names(outlinks_df) <- NULL

# get all repositories content as nested list
parsed <- request("https://glittr.org/api/repositories") |>
  req_perform() |>
  resp_body_json()

# extract relevant items as dataframe
url_repo_list <- lapply(parsed$data, function(x) data.frame(
  name = x$name,
  repo_url = x$url,
  website = x$website,
  author_profile = x$author$profile,
  author_website = x$author$website
))

url_repo <- do.call(rbind, url_repo_list)



outlinks_df$is_website <- outlinks_df$url %in% url_repo$website
outlinks_df$associated_by_repo_url <- url_repo$name[match(outlinks_df$url, url_repo$website)]

clean_url <- function(url) {
  trimws(url) |> gsub("/$", "", x = _) |> tolower()
}

match_url <- function(outlinks_df, url_repo, column = "repo_url") {
  outlinks_df[[paste0("is_", column)]] <- clean_url(outlinks_df$url) %in% clean_url(url_repo[[column]])
  outlinks_df[[paste0("ass_repo_", column)]] <- url_repo$name[match(clean_url(outlinks_df$url),
                                                                    clean_url(url_repo[[column]]))]
  return(outlinks_df)
}

for(column in c("repo_url", "website", "author_profile", "author_website")) {
  outlinks_df <- match_url(outlinks_df, url_repo, column = column)
}

no_ass <- outlinks_df |>
  select(starts_with("ass_repo")) |>
  apply(1, function(x) all(is.na(x)))

outlinks_df$url[no_ass]

outlinks_df$associated_entry <- outlinks_df |>
  select(ass_repo_repo_url, ass_repo_website) |>
  apply(1, function(x) {
    x <- x[!is.na(x)] |> unique()
    
    if(length(x) == 1) return(x[1]) 
    if(length(x == 0) == 0) return(NA)
    if(length(x == 2)) return("do not correspond")
  })

outlinks_df |>
  filter(!is.na(associated_entry)) |>
  filter(associated_entry == "do not correspond")

visits_by_entry <- outlinks_df |>
  select(url, nb_visits, associated_entry) |>
  filter(!is.na(associated_entry)) |>
  group_by(associated_entry) |>
  summarise(total_visits = sum(nb_visits)) |>
  arrange(desc(total_visits))
