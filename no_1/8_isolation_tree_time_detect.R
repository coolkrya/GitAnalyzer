library(isotree)
library(dplyr)
library(lubridate)


df <- read_csv("C:/Users/qq/PycharmProjects/pythonProject/topka/synthetic_commits.csv") %>%
  filter(
    !is.na(commit_id),
    !is.na(authored_date),
    !is.na(author_name),
    !is.na(author_email),
    !is.na(new_path)
  ) %>% 
  mutate(
    authored_date = ymd_hms(authored_date, quiet = TRUE),  # не выводит ворнинги
    weekday_numeric = wday(authored_date, label = FALSE, week_start = 1)
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



# Извлекаем временные признаки
df_time <- df %>%
  mutate(
    hour = hour(authored_date),
    wday = wday(authored_date),  # 1 = воскресенье, 7 = суббота
    is_weekend = ifelse(wday %in% c(1, 7), 1, 0)
  ) %>%
  select(hour, wday, is_weekend)

model_time <- isolation.forest(df_time, ntrees = 100)

# Получаем оценки аномальности
df$anomaly_score_time <- predict(model_time, df_time, type = "score")
df$is_anomaly_time <- df$anomaly_score_time > 0.6

anomalies_time <- df %>% filter(is_anomaly_time == TRUE)
print(anomalies_time, n = 28)