library(tidyverse)
library(lubridate)

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
df_clean <- df %>%
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

# Рассчитываем строки для каждого файла
df <- df %>%
  group_by(new_path) %>%
  mutate(
    net_lines = lines_added - lines_removed,
    total_lines = cumsum(net_lines),
    commit_index = row_number()
  ) %>%
  ungroup()


# Визуализация истории изменений файлов
print(ggplot(df, aes(x = authored_date, y = total_lines, color = new_path)) +
  geom_line() +
  labs(title = "Хронология изменений файлов", x = "Дата", y = "Количество строк") +
  theme_minimal())


# Группируем по файлам и сортируем по дате
file_history_list <- df %>%
  group_by(new_path) %>%
  arrange(authored_date, .by_group = TRUE) %>%
  group_split()

# Выводим таблицу для каждого файла
for (file_data in file_history_list) {
  cat("\n📄 История файла:", unique(file_data$new_path), "\n")
  print(file_data)
  readline(prompt = "Нажмите [Enter] для следующего файла...")
}