library(dplyr)

contr_repo <- read.csv("data/contributor_orcid_all_repos.csv")
orcid_cache <- read.csv("data/orcid_cache.csv")

n_orcid <- contr_repo |>
  select(orcid) |>
  filter(!is.na(orcid)) |>
  nrow()

n_unique_orcid <- contr_repo |> 
  select(orcid) |> 
  filter(!is.na(orcid)) |>
  distinct() |>
  nrow()

n_repos <- contr_repo |>
  select(owner_repo) |>
  distinct() |>
  nrow()

n_repos_with_orcid <- contr_repo |>
  filter(!is.na(orcid)) |>
  select(owner_repo) |>
  distinct() |>
  nrow()
