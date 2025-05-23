library(isotree)
library(dplyr)


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



# Выбор признаков (автор, email, язык)
df_model <- df %>%
  select(author_name, author_email, file_language)

# Преобразуем категориальные данные в факторы, затем в числовой формат
df_encoded <- df_model %>%
  mutate(
    author_name = as.numeric(as.factor(author_name)),
    author_email = as.numeric(as.factor(author_email)),
    file_language = as.numeric(as.factor(file_language))
  )


# Обучаем модель
iso_model <- isolation.forest(df_encoded, ntrees = 100) 

# Получаем anomaly scores
scores <- predict(iso_model, df_encoded, type = "score")

# Помечаем аномалии (например, если score > 0.65 — это аномалия)
threshold <- 0.6
df$anomaly_score <- scores
df$is_anomaly <- scores > threshold

anomalies <- df %>% filter(is_anomaly == TRUE)

print(anomalies)
