library(dplyr)
library(purrr)
library(tidyr)


library(dplyr)
library(jsonlite)
library(stringr)



repo_id <- "3663909"

# Функция для получения всех коммитов
get_all_commits <- function(repo_id, max_pages = 10) {
  all_commits <- list()
  page <- 1
  
  while (page <= max_pages) {
    url <- paste0(
      "https://gitlab.com/api/v4/projects/", 
      repo_id, 
      "/repository/commits?per_page=100&page=", 
      page
    )
    
    response <- GET(url)
    commits <- content(response, "parsed", simplifyDataFrame = TRUE)
    
    if (length(commits) == 0) break  # Если коммитов больше нет, выходим
    
    all_commits <- append(all_commits, list(commits))
    page <- page + 1
  }
  
  # Объединяем все страницы в один датафрейм
  do.call(rbind, all_commits)
}

# Получаем список коммитов
commits_df <- get_all_commits(repo_id, max_pages = 5)

# Создаем пустой список для диффов
diffs_list <- list()

# Для каждого коммита получаем DIFF
for (i in seq_len(nrow(commits_df))) {
  commit_sha <- commits_df$id[i]
  diff_url <- paste0("https://gitlab.com/api/v4/projects/", repo_id, "/repository/commits/", commit_sha, "/diff")
  
  diff_response <- GET(diff_url)
  
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


# фильтруем данные с пропусками важный данных
df <- full_df %>%
  filter(
    !is.na(commit_id),
    !is.na(authored_date),
    !is.na(author_name),
    !is.na(author_email),
    !is.na(new_path)
  ) %>% 
  mutate(
    authored_date = ymd_hms(authored_date, quiet = TRUE)  # не выводит ворнинги
  ) %>%
  filter(!is.na(authored_date))


df <- df %>%
  mutate(authored_date = ymd_hms(authored_date)) %>%
  arrange(new_path, authored_date) 

# заполняем пропуски
df <- df %>%
  filter(!is.na(commit_id) & !is.na(authored_date) & !is.na(author_name) & !is.na(author_email) & !is.na(new_path)) %>%
  mutate(
    message = if_else(is.na(message), "no message", message),
    lines_added = replace_na(lines_added, 0),
    lines_removed = replace_na(lines_removed, 0),
    new_file = replace_na(new_file, FALSE),
    renamed_file = replace_na(renamed_file, FALSE),
    deleted_file = replace_na(deleted_file, FALSE),
    file_language = replace_na(file_language, "unknown")
  )

# обогощаем данные
df <- df %>%
  mutate(
    authored_date = ymd_hms(authored_date),
    hour = hour(authored_date),
    weekday = wday(authored_date, label = TRUE)
  )

df$file_language <- case_when(
  str_detect(df$new_path, "\\.py$") ~ "Python",
  str_detect(df$new_path, "\\.js$") ~ "JavaScript",
  str_detect(df$new_path, "\\.java$") ~ "Java",
  str_detect(df$new_path, "\\.cpp$") ~ "C++",
  str_detect(df$new_path, "\\.go$") ~ "Go",
  str_detect(df$new_path, "\\.html$") ~ "HTML",
  str_detect(df$new_path, "\\.css$") ~ "CSS",
  str_detect(df$new_path, "\\.md$") ~ "Markdown",
  TRUE ~ "Other"
)


