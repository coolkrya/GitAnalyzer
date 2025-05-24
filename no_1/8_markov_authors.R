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
  prob <- 1
  for (i in 1:(length(seq) - 1)) {
    from <- seq[i]
    to <- seq[i + 1]
    p <- tryCatch(chain[from, to], error = function(e) 0)
    if (is.na(p) || p == 0) return(0)
    prob <- prob * p
  }
  return(prob)
}

# 1. Формируем последовательности по 3 действия для каждого автора
df_sequences <- df %>%
  arrange(author_name, authored_date) %>%
  group_by(author_name) %>%
  mutate(action_seq_id = (row_number() - 1) %/% 3 + 1) %>%
  group_by(author_name, action_seq_id) %>%
  summarise(action_sequence = list(action_type),
            commit_ids = list(commit_id),
            .groups = "drop") %>%
  filter(lengths(action_sequence) == 3)

# 2. Строим марковские модели по каждому автору
author_models <- df %>%
  group_by(author_name) %>%
  summarise(model = list(markovchainFit(data = action_type, method = "mle")$estimate), .groups = "drop")

# 3. Соединяем модели с последовательностями
df_sequences <- df_sequences %>%
  left_join(author_models, by = "author_name")

# 4. Считаем вероятность каждой цепочки
df_sequences <- df_sequences %>%
  rowwise() %>%
  mutate(probability = calculate_sequence_prob(model, action_sequence)) %>%
  ungroup()

# 5. Выбираем 2 самые маловероятные цепочки для каждого автора
lowest_prob_sequences <- df_sequences %>%
  group_by(author_name) %>%
  slice_min(order_by = probability, n = 2, with_ties = FALSE) %>%
  ungroup()

# 6. Получаем оригинальные записи из df по commit_id
anomalous_commit_ids <- lowest_prob_sequences %>%
  dplyr::select(author_name, commit_ids) %>%
  unnest(commit_ids) %>%
  rename(commit_id = commit_ids)

anomalous_commits <- df %>%
  inner_join(anomalous_commit_ids, by = c("author_name", "commit_id"))

# 7. Выводим
print(lowest_prob_sequences)
print(anomalous_commits, n=36)























