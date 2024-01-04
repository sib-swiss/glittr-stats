library(httr2)

# Create a client
parsed <- request("https://glittr.org/api/repositories") |>
  req_perform() |> resp_body_json()

parsed$data[[1]]

repo_info_list <- lapply(parsed$data, function(x) data.frame(
  repo = x$name,
  author_name = x$author$name,
  stargazers = x$stargazers,
  recency = x$days_since_last_push))

repo_info <- do.call(rbind, repo_info_list)

author_freq <- table(repo_info$author_name)
author_freq <- author_freq[order(author_freq, decreasing = TRUE)]

starsum <- tapply(repo_info$stargazers, repo_info$author_name, sum)
starsum <- starsum[order(starsum, decreasing = TRUE)]

sum(repo_info$stargazers > 1000)
sum(repo_info$stargazers > 100)
