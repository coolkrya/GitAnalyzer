library(dplyr)
library(e1071)
library(ggplot2)
library(lubridate)

# Загрузка данных
df <- read_csv("C:/Users/qq/PycharmProjects/pythonProject/topka/synthetic_commits.csv") %>%
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


# Преобразуем дату и добавим признаки
df <- df %>%
  mutate(
    authored_date = ymd_hms(authored_date),
    hour = hour(authored_date),
    wday = wday(authored_date, label = TRUE),
    commit_size = lines_added + lines_removed,
    file_language = as.factor(file_language),
    lang_code = as.numeric(file_language)
  ) %>%
  filter(!is.na(author_email), !is.na(lines_added), !is.na(lines_removed))





# 1. Добавляем признаки
df_features <- df %>%
  mutate(
    hour = hour(ymd_hms(authored_date)),
    weekday = wday(ymd_hms(authored_date)),
    commit_size = lines_added + lines_removed
  ) %>%
  select(commit_id, author_email, authored_date, hour, weekday, lines_added, lines_removed, commit_size)

# 2. Для хранения результатов
df_results <- data.frame()

# 3. Проходим по каждому автору отдельно
authors <- unique(df_features$author_email)

for (author in authors) {
  df_author <- df_features %>% filter(author_email == author)
  
  # Проверка: должно быть хотя бы 10 записей
  if (nrow(df_author) >= 10) {
    df_scaled <- scale(df_author %>% select(hour, weekday, lines_added, lines_removed, commit_size))
    
    # Обучение SVM
    model <- svm(df_scaled, type = 'one-classification', kernel = 'radial', nu = 0.05)
    
    # Предсказания
    preds <- predict(model, df_scaled)
    
    df_author$anomaly <- ifelse(preds, 0, 1)  # 1 = аномалия
    df_results <- bind_rows(df_results, df_author)
  }
}

# 4. Объединяем с оригинальными данными
df_joined <- df %>%
  left_join(df_results %>% select(commit_id, anomaly), by = "commit_id")
