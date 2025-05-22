#Очистка и нормализация

library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)

df <- read_csv("synthetic_commits.csv")

#Удаляем Nan в date (откуда он там)
df <- df %>%
  filter(!is.na(authored_date)) %>%
  mutate(authored_date = as.POSIXct(authored_date, tz = "UTC"))

#Удаляем дублики
df <- df %>% distinct(commit_id, .keep_all = TRUE)


#Приведение типов
df <- df %>%
  mutate(
    lines_added = as.numeric(lines_added),
    lines_removed = as.numeric(lines_removed),
    new_file = as.logical(new_file),
    deleted_file = as.logical(deleted_file),
    renamed_file = as.logical(renamed_file),
    file_language = factor(file_language),
    author_name = factor(author_name),
    author_email = factor(author_email)
  )


#Бомбим даты
df <- df %>%
  mutate(
    weekday = wday(authored_date, label = TRUE),
    hour = hour(authored_date),
    day_part = case_when(
      hour < 6 ~ "night",
      hour < 12 ~ "morning",
      hour < 18 ~ "day",
      TRUE ~ "evening"
    )
  )


