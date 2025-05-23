#Подгрузка синт-данных + статистика

library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)

# Загрузка данных
df <- read_csv("synthetic_commits.csv")

# Преобразование даты
df <- df %>%
  mutate(authored_date = ymd_hms(authored_date),
         hour = hour(authored_date),
         weekday = wday(authored_date, label = TRUE))

# --- Общая статистика по авторам ---
author_stats <- df %>%
  group_by(author_name, author_email) %>%
  summarise(
    commits = n(),
    avg_lines_added = mean(lines_added),
    avg_lines_removed = mean(lines_removed),
    most_common_lang = names(sort(table(file_language), decreasing = TRUE)[1]),
    common_commit_hour = round(mean(hour)),
    .groups = "drop"
  )

print("📊 Статистика по каждому автору:")
print(author_stats)

# --- Частота коммитов по часам ---
ggplot(df, aes(x = hour, fill = author_name)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "Коммиты по часу суток", x = "Час", y = "Количество") +
  theme_minimal()

# --- Топ языков ---
ggplot(df, aes(x = file_language, fill = author_name)) +
  geom_bar(position = "dodge") +
  labs(title = "Языки в коммитах", x = "Язык", y = "Количество") +
  theme_minimal()

# --- Поиск подозрительных email по авторам ---
email_check <- df %>%
  group_by(author_name) %>%
  summarise(unique_emails = n_distinct(author_email)) %>%
  filter(unique_emails > 1)

print("🚨 Авторы с несколькими email-адресами:")
print(email_check)

# --- Поиск "необычных" часов коммитов ---
unusual_times <- df %>%
  group_by(author_name) %>%
  mutate(avg_hour = round(mean(hour))) %>%
  filter(abs(hour - avg_hour) > 5)  # разница более 5 часов

print("⏰ Подозрительные по времени коммиты:")
print(unusual_times[, c("author_name", "authored_date", "hour", "message")])
