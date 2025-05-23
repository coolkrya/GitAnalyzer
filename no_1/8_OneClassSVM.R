library(e1071)
library(dplyr)

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






# Подготовим данные одного автора
author_df <- df %>%
  filter(author_email == "bob@example.com") %>%
  mutate(
    hour = lubridate::hour(ymd_hms(authored_date)),
    weekday = lubridate::wday(ymd_hms(authored_date)),
    commit_size = lines_added + lines_removed
  ) %>%
  select(hour, commit_size, lines_added, lines_removed, weekday)

# Нормализация (очень важна для SVM)
scaled_df <- scale(author_df)

# Обучение модели
svm_model <- svm(scaled_df, type = "one-classification", kernel = "radial", nu = 0.05)

# Предсказания
pred <- predict(svm_model, scaled_df)

# Добавим предсказания к датафрейму
author_df$anomaly <- ifelse(pred, 0, 1)  # 1 = аномалия

# Посмотрим аномалии
print(head(author_df %>% filter(anomaly == 1)))
