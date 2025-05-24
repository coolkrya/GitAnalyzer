# Загрузка библиотек (предполагаем, что они уже установлены)
library(isotree)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(purrr)
library(tidyr)

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






# Получаем список авторов
authors <- unique(df$author_email)

# Функция анализа одного автора
analyze_author <- function(author_data) {
  features <- author_data %>%
    select(lines_added, lines_removed, commit_size, hour, lang_code) %>%
    na.omit()
  
  if (nrow(features) < 10) {
    return(NULL)  # Пропускаем авторов с недостаточно данных
  }
  
  features_matrix <- as.matrix(features)
  
  model <- isolation.forest(features_matrix, ntrees = 100)
  author_data <- author_data[complete.cases(features), ]
  author_data$anomaly_score <- predict(model, features_matrix, type = "score")
  
  author_data
}

# Применяем к каждому автору
results <- df %>%
  split(.$author_name) %>%
  map_dfr(analyze_author)

# Выводим топ аномалий
top_anomalies <- results %>%
  arrange(desc(anomaly_score)) %>%
  group_by(author_name) %>%
  slice_max(anomaly_score, n = 3)  # по 3 на автора

print(top_anomalies)

# Визуализация для одного автора 
for (author in authors) {
print(ggplot(results %>% filter(author_email == author),
       aes(x = commit_size, y = hour, color = anomaly_score)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = paste("Аномалии для", author),
       x = "Размер коммита", y = "Час", color = "Аномальность") +
  theme_minimal())
}