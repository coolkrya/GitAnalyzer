#Подготовка данных. Выгрузка абсолютно всего

library(httr)
library(dplyr)
library(jsonlite)
library(stringr)
library(dotenv)

dotenv::load_dot_env(file = ".env")
token <- Sys.getenv("API_TOKEN")
repo_id <- "69227863"

# Получаем список коммитов
url <- paste0("https://gitlab.com/api/v4/projects/", repo_id, "/repository/commits?per_page=100")
response <- GET(url, add_headers("PRIVATE-TOKEN" = token))
commits_df <- content(response, "parsed", simplifyDataFrame = TRUE)

# Создаем пустой список для диффов
diffs_list <- list()

# Для каждого коммита получаем DIFF
for (i in seq_len(nrow(commits_df))) {
  commit_sha <- commits_df$id[i]
  diff_url <- paste0("https://gitlab.com/api/v4/projects/", repo_id, "/repository/commits/", commit_sha, "/diff")
  
  diff_response <- GET(diff_url, add_headers("PRIVATE-TOKEN" = token))
  
  # Обработка возможной ошибки
  if (status_code(diff_response) != 200) next
  
  diff_data <- content(diff_response, "parsed", simplifyDataFrame = TRUE)
  
  if (length(diff_data) == 0) next
  
  # Добавляем commit_id в каждый элемент
  diff_data$commit_id <- commit_sha
  
  # Добавим расчет добавленных и удаленных строк из поля diff
  diff_data <- diff_data %>%
    rowwise() %>%
    mutate(
      lines_added = str_count(diff, regex("^\\+[^+]", multiline = TRUE)),
      lines_removed = str_count(diff, regex("^-[^-]", multiline = TRUE))
    ) %>%
    ungroup()
  
  # Сохраняем
  diffs_list[[i]] <- diff_data
}

# Объединяем в один датафрейм
diffs_df <- bind_rows(diffs_list)

# Предполагаем, что уже есть commits_df и diffs_df
# Убедимся, что поле для join одинаковое
diffs_df$commit_id <- as.character(diffs_df$commit_id)
commits_df$id <- as.character(commits_df$id)

# Объединение
full_df <- merge(diffs_df, commits_df, by.x = "commit_id", by.y = "id")

# Выбираем только нужные поля (по желанию)
full_df <- full_df %>%
  select(
    commit_id,
    authored_date,
    author_name,
    author_email,
    message,
    new_path,
    old_path,
    lines_added,
    lines_removed,
    new_file,
    renamed_file,
    deleted_file,
  )

full_df$file_language <- case_when(
  str_detect(full_df$new_path, "\\.py$") ~ "Python",
  str_detect(full_df$new_path, "\\.js$") ~ "JavaScript",
  str_detect(full_df$new_path, "\\.java$") ~ "Java",
  str_detect(full_df$new_path, "\\.cpp$") ~ "C++",
  str_detect(full_df$new_path, "\\.go$") ~ "Go",
  str_detect(full_df$new_path, "\\.html$") ~ "HTML",
  str_detect(full_df$new_path, "\\.css$") ~ "CSS",
  str_detect(full_df$new_path, "\\.md$") ~ "Markdown",
  TRUE ~ "Other"
)


write.csv(full_df, "commits_combined_1.csv", row.names = FALSE, fileEncoding = "UTF-8")

