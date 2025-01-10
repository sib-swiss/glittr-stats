library(httr2)
library(dplyr)
library(wordcloud)

parsed <- request("https://glittr.org/api/tags") |>
  req_perform() |>
  resp_body_json()

tag_dfs <- list()
for(i in seq_along(parsed)) {
  category <- parsed[[i]]$category
  name <- sapply(parsed[[i]]$tags, function(x) x$name)
  repositories <- sapply(parsed[[i]]$tags, function(x) x$repositories)
  tag_dfs[[category]] <- data.frame(name, category, repositories)
}

tag_df <- do.call(rbind, tag_dfs) |> arrange(repositories)

glittr_cols <- c(
  "Scripting and languages" =             "#3a86ff",
  "Computational methods and pipelines" = "#fb5607",
  "Omics analysis" =                      "#ff006e",
  "Reproducibility and data management" = "#ffbe0b",
  "Statistics and machine learning" =     "#8338ec",
  "Others" =                              "#000000")

pdf("wordcloud_tags.pdf")
wordcloud(tag_df$name, tag_df$repositories, max.words = 100, colors = glittr_cols,
          random.color = TRUE)
dev.off()
