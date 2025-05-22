# Пробничек загрузки коммитов


library(httr)
library(dotenv)

dotenv::load_dot_env(file = ".env")
token <- Sys.getenv("API_TOKEN")
repo_id <- "69227863"


url <- paste0("https://gitlab.com/api/v4/projects/", repo_id, "/repository/commits")

response <- GET(url, add_headers("PRIVATE-TOKEN" = token))
status <- status_code(response)
commits <- content(response, "parsed")
commits