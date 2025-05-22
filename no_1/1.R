# Пробничек загрузки коммитов


library(httr)
token <- "glpat-XxnZ25ToMcXn85S2Cud3"
repo_id <- "69227863"


url <- paste0("https://gitlab.com/api/v4/projects/", repo_id, "/repository/commits")

response <- GET(url, add_headers("PRIVATE-TOKEN" = token))
status <- status_code(response)
commits <- content(response, "parsed")
commits