library(isotree)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(purrr)
library(tidyr)

# Удаляем записи с пропусками того, что пропускать нельзя
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








# Преобразуем дату в числовые признаки
df <- df %>%
  mutate(
    authored_date = ymd_hms(authored_date),
    hour = hour(authored_date),
    wday = wday(authored_date, label = TRUE),
    commit_size = lines_added + lines_removed
  )

# Пример кодирования языка в фактор (можно и dummy variables)
df$file_language <- as.factor(df$file_language)
df <- df %>% mutate(lang_code = as.numeric(file_language))

# Формируем набор признаков для модели
features <- df %>%
  select(lines_added, lines_removed, commit_size, hour, lang_code)

# Убираем NA и приводим к матрице
features <- na.omit(features)
features_matrix <- as.matrix(features)

# Обучаем Isolation Forest
iso_model <- isolation.forest(features_matrix, ntrees = 100)

# Получаем аномальные оценки (чем выше, тем более аномально)
anomaly_scores <- predict(iso_model, features_matrix, type = "score")

# Добавляем результаты обратно в df
df_clean <- df %>% filter(complete.cases(features))
df_clean$anomaly_score <- anomaly_scores



# Отображение топ-аномалий
top_anomalies <- df_clean %>%
  arrange(desc(anomaly_score)) %>%
  filter(anomaly_score > 0.6)

print(top_anomalies)

# Визуализация: аномалии по размеру коммитов и времени
print(ggplot(df_clean, aes(x = commit_size, y = hour, color = anomaly_score)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "Аномалии по времени и размеру коммита",
       x = "Размер коммита (строк)", y = "Час суток", color = "Аномальность"))

