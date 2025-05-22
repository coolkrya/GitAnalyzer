#Собирает информацию о всех коммитах и записывает их в файл


library(httr)
token <- "glpat-XxnZ25ToMcXn85S2Cud3"
repo_id <- "69227863"
url <- paste0("https://gitlab.com/api/v4/projects/", repo_id, "/repository/commits")

all_commits <- list()
page <- 1
repeat {
  response <- GET(
    url,
    add_headers("PRIVATE-TOKEN" = token),
    query = list(per_page = 100, page = page)
  )
  if (status_code(response) != 200) break
  batch <- content(response, "parsed")
  if (length(batch) == 0) break
  all_commits <- append(all_commits, batch)
  page <- page + 1
}

commits_df <- do.call(rbind, lapply(all_commits, function(x) {
  data.frame(
    id = x$id,
    short_id = x$short_id,
    title = x$title,
    message = x$message,
    author_name = x$author_name,
    author_email = x$author_email,
    authored_date = x$authored_date,
    committed_date = x$committed_date,
    committer_name = x$committer_name,
    web_url = x$web_url,
    stringsAsFactors = FALSE
  )
}))


write.csv(commits_df, "commits_data.csv", row.names = FALSE, fileEncoding = "UTF-8")
commits_df <- read.csv("commits_data.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
