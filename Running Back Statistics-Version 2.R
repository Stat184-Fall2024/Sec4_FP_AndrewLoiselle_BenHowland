install.packages("tidyverse")
install.packages("rvest")
install.packages("tinytex")
install.packages("tidyr")
install.packages("forcats")
library(tidyverse)
library(rvest)
library(tinytex)
library(forcats)
library(tidyr)
library(ggplot2)
library(googlesheets4)

gs4_deauth()
RunningBacksRaw <- read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1COdP4-MxfQJHoQeTmyXjE8EUgMk2gVvCElUp1CxRP-4/edit?usp=sharing"
) 


RunningBacksTidy = RunningBacksRaw %>%
  filter(position == "HB") %>%
  select(-c("position", "player_id", "team_name", "breakaway_attempts", "breakaway_yards", "breakaway_percent", "declined_penalties", "designed_yards", "franchise_id","elu_recv_mtf":"elusive_rating", "gap_attempts":"grades_run_block", "run_plays":"scrambles", "zone_attempts"))

RunningBacksTidy %>%
  ggplot(mapping = aes(x = attempts, y = ypa)) +
  geom_point(colour = "#112446") +
  theme_minimal()

RunningBacksMin100Attempts <- RunningBacksTidy %>%
  filter(attempts >= 100)

RunningBacksMin100Attempts %>%
  ggplot(
    mapping = aes(x = fct_reorder(player,yco_attempt), y = yco_attempt, fill = fct_reorder(player,yco_attempt))) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  labs(
    x = "Player",
    y = "Yards after Contact per Attempt",
    title = "Efficiency in Yards after Contact"
  )

RunningBacksMin100Attempts %>%
  ggplot(
    mapping = aes(x = fct_reorder(player, yprr), y = yprr, fill = fct_reorder(player,yprr))) +
  geom_col() +
  scale_fill_viridis_d(option = "viridis", direction = 1) +
  theme_bw() +
  labs(
    x = "Player",
    y = "Yards per Route Run",
    title = "Efficinecy in Receiving"
  )

RunningBacksMin100Attempts %>%
  ggplot(
    mapping = aes(x = fct_reorder(player, ypa), y = ypa, fill = fct_reorder(player, ypa))) +
  geom_col() +
  scale_fill_viridis_d(option = "magma", direction = 1) +
  theme_minimal() +
  labs(
    x = "Player",
    y = "Yards per Rushing Attempt",
    title = "Efficiency in Rushing"
  )

fivenum(RunningBacksMin100Attempts$yco_attempt)
fivenum(RunningBacksMin100Attempts$yprr)
fivenum(RunningBacksMin100Attempts$ypa)

TopRunningBacks <- RunningBacksMin100Attempts %>%
  filter(
    yco_attempt >= 2.87,
    yprr >= 0.985,
    ypa >= 4.1
  )

view(TopRunningBacks)
