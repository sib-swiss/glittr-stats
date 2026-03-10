#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(httr2)
  library(dplyr)
  library(stringr)
  library(tibble)
})

args <- commandArgs(trailingOnly = TRUE)

# Defaults: conservative full run
run_all_repos <- TRUE
sample_seed <- 42
sample_repo_n <- 30

# Conservative throttling
gh_min_interval_sec <- 1.2
gh_rate_limit_buffer <- 300

for (arg in args) {
  if (arg == "--sample") {
    run_all_repos <- FALSE
  }
  if (startsWith(arg, "--n=")) {
    sample_repo_n <- as.integer(sub("^--n=", "", arg))
  }
  if (startsWith(arg, "--seed=")) {
    sample_seed <- as.integer(sub("^--seed=", "", arg))
  }
  if (startsWith(arg, "--min-interval=")) {
    gh_min_interval_sec <- as.numeric(sub("^--min-interval=", "", arg))
  }
}

pat <- Sys.getenv("PAT")
if (!nzchar(pat)) {
  stop("Environment variable PAT is not set. Configure PAT in .Renviron before running.")
}

base_headers <- c(
  "Accept" = "application/vnd.github+json",
  "X-GitHub-Api-Version" = "2022-11-28"
)

gh_request <- function(url) {
  request(url) |>
    req_user_agent("glittr-stats-orcid/1.0") |>
    req_headers(!!!base_headers) |>
    req_auth_bearer_token(pat)
}

gh_last_api_call <- as.numeric(Sys.time())

gh_paced_perform <- function(req) {
  elapsed_since_last <- as.numeric(Sys.time()) - gh_last_api_call
  if (elapsed_since_last < gh_min_interval_sec) {
    Sys.sleep(gh_min_interval_sec - elapsed_since_last)
  }

  resp <- req |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()

  gh_last_api_call <<- as.numeric(Sys.time())

  remaining <- suppressWarnings(as.numeric(resp_header(resp, "x-ratelimit-remaining")))
  reset_epoch <- suppressWarnings(as.numeric(resp_header(resp, "x-ratelimit-reset")))

  if (!is.na(remaining) && !is.na(reset_epoch) && remaining <= gh_rate_limit_buffer) {
    sleep_seconds <- ceiling(reset_epoch - as.numeric(Sys.time())) + 2
    if (sleep_seconds > 0) {
      cat(sprintf("\n[rate-limit] remaining=%d; sleeping %d seconds until reset\n",
                  as.integer(remaining), sleep_seconds))
      flush.console()
      Sys.sleep(sleep_seconds)
    }
  }

  status <- resp_status(resp)
  if (status >= 400) {
    stop(sprintf("GitHub API request failed with status %s", status))
  }

  resp
}

gh_get_paginated_df <- function(url) {
  resp <- gh_paced_perform(gh_request(url))

  out <- resp_body_json(resp, simplifyVector = TRUE)
  if (!is.data.frame(out)) {
    out <- tibble()
  }

  next_url <- resp_link_url(resp, rel = "next")

  while (!is.null(next_url) && nzchar(next_url)) {
    next_resp <- gh_paced_perform(gh_request(next_url))
    next_out <- resp_body_json(next_resp, simplifyVector = TRUE)

    if (is.data.frame(next_out) && nrow(next_out) > 0) {
      out <- bind_rows(out, next_out)
    }

    next_url <- resp_link_url(next_resp, rel = "next")
  }

  out
}

get_contributors <- function(owner_repo) {
  url <- paste0("https://api.github.com/repos/", owner_repo, "/contributors?per_page=100")

  contrib <- tryCatch(
    gh_get_paginated_df(url),
    error = function(e) tibble()
  )

  if (!nrow(contrib) || !"login" %in% names(contrib)) {
    return(tibble(owner_repo = owner_repo, contributor_login = character(0), contributions = integer(0)))
  }

  contrib |>
    transmute(
      owner_repo = owner_repo,
      contributor_login = login,
      contributions = contributions
    )
}

extract_orcid_id <- function(x) {
  matches <- str_extract(
    x,
    "(https?://orcid\\.org/)?\\d{4}-\\d{4}-\\d{4}-\\d{3}[\\dX]"
  )
  matches <- matches[!is.na(matches)]

  if (!length(matches)) {
    return(NA_character_)
  }

  str_extract(matches[[1]], "\\d{4}-\\d{4}-\\d{4}-\\d{3}[\\dX]")
}

get_orcid <- function(login) {
  social_url <- paste0("https://api.github.com/users/", login, "/social_accounts")

  social <- tryCatch(
    gh_get_paginated_df(social_url),
    error = function(e) tibble()
  )

  if (nrow(social) && "url" %in% names(social)) {
    social_orcid <- extract_orcid_id(social$url)
    if (!is.na(social_orcid)) {
      return(social_orcid)
    }
  }

  profile_html <- tryCatch(
    request(paste0("https://github.com/", login)) |>
      req_user_agent("glittr-stats-orcid/1.0") |>
      req_perform() |>
      resp_body_string(),
    error = function(e) NA_character_
  )

  extract_orcid_id(profile_html)
}

cat("Fetching repositories from glittr.org...\n")
glittr_resp <- request("https://glittr.org/api/repositories") |>
  req_perform() |>
  resp_body_json()

repos <- tibble(repo_url = vapply(glittr_resp$data, function(x) x$url, character(1))) |>
  filter(str_detect(repo_url, "github\\.com")) |>
  mutate(
    repo_url = str_replace(repo_url, "\\.git$", ""),
    repo_url = str_replace(repo_url, "#.*$", ""),
    repo_url = str_replace(repo_url, "\\?.*$", ""),
    owner_repo = str_match(repo_url, "github\\.com/([^/]+/[^/]+)")[, 2]
  ) |>
  filter(!is.na(owner_repo)) |>
  distinct(owner_repo, .keep_all = TRUE)

if (!run_all_repos) {
  set.seed(sample_seed)
  repos <- repos |>
    slice_sample(n = min(sample_repo_n, nrow(repos)))
}

cat(sprintf("Repositories to process: %d\n", nrow(repos)))

repo_ids <- repos$owner_repo
repo_total <- length(repo_ids)
contributors_list <- vector("list", repo_total)

for (i in seq_along(repo_ids)) {
  contributors_list[[i]] <- get_contributors(repo_ids[[i]])
  cat(sprintf("[repos] %d/%d processed: %s\n", i, repo_total, repo_ids[[i]]))
  flush.console()
}

contributors <- bind_rows(contributors_list) |>
  distinct(owner_repo, contributor_login, .keep_all = TRUE)

unique_logins <- contributors |>
  distinct(contributor_login)

cache_file <- "data/orcid_cache.csv"
orcid_cache <- if (file.exists(cache_file)) {
  read.csv(cache_file, stringsAsFactors = FALSE) |>
    as_tibble() |>
    transmute(
      contributor_login = as.character(contributor_login),
      orcid = na_if(as.character(orcid), "")
    ) |>
    distinct(contributor_login, .keep_all = TRUE)
} else {
  tibble(contributor_login = character(), orcid = character())
}

missing_logins <- unique_logins |>
  anti_join(orcid_cache, by = "contributor_login")

cat(sprintf("Unique contributors: %d | Cached: %d | New lookups: %d\n",
            nrow(unique_logins), nrow(unique_logins) - nrow(missing_logins), nrow(missing_logins)))

timed_get_orcid <- function(login) {
  started <- Sys.time()
  orcid <- get_orcid(login)
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))

  tibble(
    contributor_login = login,
    lookup_seconds = elapsed,
    orcid = orcid
  )
}

if (nrow(missing_logins) > 0) {
  login_ids <- missing_logins$contributor_login
  login_total <- length(login_ids)
  orcid_list <- vector("list", login_total)

  for (i in seq_along(login_ids)) {
    orcid_list[[i]] <- timed_get_orcid(login_ids[[i]])
    cat(sprintf("[orcid] %d/%d processed: %s\n", i, login_total, login_ids[[i]]))
    flush.console()
  }

  orcid_lookup_new <- bind_rows(orcid_list)
} else {
  orcid_lookup_new <- tibble(
    contributor_login = character(),
    lookup_seconds = numeric(),
    orcid = character()
  )
}

orcid_cache_updated <- bind_rows(
  orcid_cache,
  orcid_lookup_new |> select(contributor_login, orcid)
) |>
  distinct(contributor_login, .keep_all = TRUE)

write.csv(orcid_cache_updated, cache_file, row.names = FALSE)

orcid_lookup <- unique_logins |>
  left_join(orcid_cache_updated, by = "contributor_login")

contributor_orcid <- contributors |>
  left_join(orcid_lookup, by = "contributor_login") |>
  arrange(owner_repo, desc(contributions), contributor_login)

output_csv <- if (run_all_repos) {
  "data/contributor_orcid_all_repos.csv"
} else {
  sprintf("data/contributor_orcid_sample_%d_seed_%d.csv", sample_repo_n, sample_seed)
}

write.csv(contributor_orcid, output_csv, row.names = FALSE)

total_lookup_seconds <- sum(orcid_lookup_new$lookup_seconds, na.rm = TRUE)
seconds_per_contributor <- ifelse(
  nrow(orcid_lookup_new) > 0,
  total_lookup_seconds / nrow(orcid_lookup_new),
  NA_real_
)

cat("Run complete\n")
cat(sprintf("Output CSV: %s\n", output_csv))
cat(sprintf("Total ORCID lookup time (new lookups): %.2f seconds\n", total_lookup_seconds))
cat(sprintf("Average seconds per contributor (new lookups): %.3f\n", seconds_per_contributor))
cat(sprintf("Contributors with ORCID: %d\n", sum(!is.na(contributor_orcid$orcid))))
