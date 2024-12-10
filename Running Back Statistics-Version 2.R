install.packages("tidyverse")
install.packages("rvest")
install.packages("tinytex")
install.packages("tidyr")
library(tidyverse)
library(rvest)
library(tinytex)
library(tidyr)
library(ggplot2)


RunningBacks = read.csv("C:\\Users\\benja\\OneDrive - The Pennsylvania State University\\R 184\\2023 RB Stats.csv")

RunningBacksTidy = RunningBacks %>%
  filter(position == "HB") %>%
  filter(attempts >= 100) %>%
  select(-c("position", "player_id", "team_name", "breakaway_attempts", "breakaway_yards", "breakaway_percent", "declined_penalties", "designed_yards", "franchise_id","elu_recv_mtf":"elusive_rating", "gap_attempts":"grades_run_block", "run_plays":"scrambles", "zone_attempts"))

esquisser(data = RunningBacksTidy, viewer = "browser")

ggplot(RunningBacksTidy) +
  aes(x = yards, y = touchdowns, fill = player) +
  geom_point(
    size = 5L,
    shape = "square filled",
    colour = "#112446"
  ) +
  scale_fill_hue(direction = 1) +
  theme_minimal()


RunningBacksTidy %>%
  filter(touchdowns >= 5) %>%
  filter(yards >= 1000) %>%
  ggplot() +
  aes(x = player, y = yco_attempt, fill = player) +
  geom_bar(stat = "summary", fun = "sum") +
  scale_fill_hue(direction = 1) +
  theme_minimal()

