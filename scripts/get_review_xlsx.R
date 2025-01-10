library(httr2)
library(writexl)
library(dplyr)

parsed <- request("https://glittr.org/api/repositories") |>
  req_perform() |>
  resp_body_json()

repo_info_list <- lapply(parsed$data, function(x) {
  other_tags <- sapply(x$tags, function(tag) {
    tag$name
  })
  
  if (length(other_tags) > 1) {
    other_tags <- other_tags[2:length(other_tags)] |> paste(collapse = ", ")
  } else {
    other_tags <- ""
  }
  
  data.frame(
    repo = x$name,
    author_name = x$author$name,
    stargazers = x$stargazers,
    recency = x$days_since_last_push,
    url = x$url,
    website = x$website,
    license = ifelse(is.null(x$license), "none", x$license),
    main_category = x$tags[[1]]$category,
    main_tag = x$tags[[1]]$name,
    other_tags = other_tags,
    in_review_by = "",
    main_tag_corrected = "",
    other_tags_corrected = "",
    other_remarks = "",
    should_be_removed = FALSE
  )
})

repo_info <- do.call(rbind, repo_info_list)

repo_info <- repo_info |>
  # Count the frequency of each main_tag
  group_by(main_tag) |>
  mutate(freq = n()) |>
  ungroup() |>
  # Arrange the data.frame based on the frequency in descending order
  arrange(desc(freq), main_tag) |>
  select(-freq)

# reviewed <- readxl::read_excel("reviewed.xlsx")
# 
# repo_info <- repo_info[!repo_info$repo %in% reviewed$repo, ]
# 
# repo_info <- rbind(repo_info, reviewed)

writexl::write_xlsx(repo_info, "review_format.xlsx")

