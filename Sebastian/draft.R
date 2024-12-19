OlekRaw <- read.csv2("data/sleepdataOlek.csv")
SebastianRaw <- read.csv2("data/sleepdataSebastian.csv")
PiotrRaw <- read.csv2("data/sleepdata.xls.csv")

library(dplyr)
library(lubridate)

parse_percentage <- function(str) {
  as.numeric(substr(str, 1, nchar(str)-1))/100
}

process_raw <- function(dt) {
  dt|>
    mutate(
      Sleep.Quality = parse_percentage(Sleep.Quality),
      Regularity = parse_percentage(Regularity),
      Did.snore = Did.snore == "true",
      # Went.to.bed = time_format(Went.to.bed),
      Went.to.bed = ymd_hms(gsub("\\.", "/", Went.to.bed)),
      Woke.up = ymd_hms(gsub("\\.", "/", Woke.up)),
      day = as.Date(Woke.up),
      # Woke.up = ymd_hms(Woke.up),
      Coughing..per.hour. = as.numeric(Coughing..per.hour.),
      Movements.per.hour = as.numeric(Movements.per.hour),
      # Wake.up.window.start = if_else(Wake.up.window.start == "",true = NA, false = time_format(Wake.up.window.start)),
      # Wake.up.window.stop = if_else(Wake.up.window.stop == "", true = NA, false = time_format(Wake.up.window.stop)),
    ) |>
    select(-c(
      City,
      Alertness.score,
      Alertness.reaction.time..seconds.,
      Alertness.accuracy
    ))
}

# Sebastian <- SebastianRaw |>
#   mutate(
#     Sleep.Quality = parse_percentage(Sleep.Quality),
#     Regularity = parse_percentage(Regularity),
#     Did.snore = Did.snore == "true",
#     Went.to.bed = as.POSIXct(Went.to.bed),
#     Woke.up = as.POSIXct(Woke.up),
#     day = as.Date(Woke.up)
#   ) |>
#   select(-c(
#     City,
#     Alertness.score,
#     Alertness.reaction.time..seconds.,
#     Alertness.accuracy
#   ))

Sebastian <- process_raw(SebastianRaw)
Piotr <- process_raw(PiotrRaw)
Olek <- process_raw(OlekRaw)

# View(Sebastian)

library(ggplot2)
library(tidyr)

# Można zmieniać imię osoby na do wykresu
Piotr |>
  select(Time.in.bed..seconds., Time.asleep..seconds., day) |>
  pivot_longer(
    cols = c(Time.in.bed..seconds.,
             Time.asleep..seconds.),
    names_to =  "stat",
    values_to = "val"
  ) |>
  # View()
  ggplot(aes(x = day, y = val, colour = stat)) +
  # geom_line() +
  geom_area(aes(fill = stat), alpha = 0.2) +
  ylim(0, NA) +
  scale_color_manual(labels = c("Time asleep in seconds", "Time in bed in seconds"),
                     values = c("blue", "red")) +
  scale_fill_manual(labels = c("Time asleep in seconds", "Time in bed in seconds"),
                     values = c("blue", "red")) +
  labs(
    y = "time",
    title = "Piotr"
  )
