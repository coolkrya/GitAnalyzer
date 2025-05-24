library(markovchain)
library(dplyr)
library(tidyr)
library(stringr)


df <- read_csv("C:/Users/qq/PycharmProjects/pythonProject/topka/synthetic_commits_gv.csv") %>%
  filter(
    !is.na(commit_id),
    !is.na(authored_date),
    !is.na(author_name),
    !is.na(author_email),
    !is.na(new_path)
  ) %>% 
  mutate(
    authored_date = ymd_hms(authored_date, quiet = TRUE),  # не выводит ворнинги
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



df <- df %>%
  mutate(
    action_type = case_when(
      new_file ~ "create",
      deleted_file ~ "delete",
      renamed_file ~ "rename",
      lines_added > 0 ~ "modify",
      TRUE ~ "other"
    )
  )



df <- df %>%
  filter(!is.na(action_type)) %>%
  arrange(author_name, authored_date)

# Построим марковскую модель на всех данных (или можно отдельно по каждому автору)
sequence <- df$action_type

# Создание марковской цепи
mcFit <- markovchainFit(data = sequence, method = "mle")
mc <- mcFit$estimate

# Печать вероятностей переходов
print(mc)

# Вспомогательная функция для вычисления вероятности цепочки
calculate_sequence_prob <- function(chain, seq) {
  prob <- tryCatch(chain[from, from], error = function(e) 0)
  for (i in 1:(length(seq) - 1)) {
    from <- seq[i]
    to <- seq[i + 1]
    p <- tryCatch(chain[from, to], error = function(e) 0)
    if (is.na(p) || p == 0) return(0)
    prob <- prob * p
  }
  return(prob)
}

# Группировка по автору и проверка цепочек по 3 действий
df_sequences <- df %>%
  group_by(author_name) %>%
  mutate(action_seq_id = (row_number() - 1) %/% 3 + 1) %>%
  group_by(author_name, action_seq_id) %>%
  summarise(action_sequence = list(action_type), .groups = "drop") %>%
  mutate(probability = sapply(action_sequence, function(seq) calculate_sequence_prob(mc, seq)),
         is_anomaly = probability < quantile(probability, 0.05))  # нижние 5%


df_sequences <- df_sequences %>%
  filter(lengths(action_sequence) == 3)

# Просмотр аномалий
anomalies <- df_sequences %>% filter(is_anomaly == TRUE)
print(anomalies)