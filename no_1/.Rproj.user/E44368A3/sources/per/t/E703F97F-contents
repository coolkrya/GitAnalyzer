#Построение хронологически последовательной истории изменений файлов репозитория

library(readr)
library(dplyr)
library(lubridate)

# Загрузка данных
df <- read_csv("C:/Users/qq/PycharmProjects/pythonProject/topka/synthetic_commits.csv")

# Преобразование даты
df <- df %>%
  mutate(authored_date = ymd_hms(authored_date))

# Построение хронологической истории изменений
file_history <- df %>%
  arrange(new_path, authored_date) %>%                   # Сортировка по имени файла и времени
  group_by(new_path) %>%
  mutate(change_index = row_number()) %>%                # Индексация изменений
  ungroup()

# Посмотрим первые строки
print(head(file_history, 10))
