#Install libraries
library(cbbdata)
library(dplyr)
library(glue)
library(gt)
library(cbbplotR)
library(gtExtras)

#Get player stats + team game logs from cbbdata
stats = cbbdata::cbd_torvik_player_season()
games = cbbdata::cbd_torvik_game_stats()

#Get yearly game count for teams
gamecount = games %>% 
  filter(year==2023|year==2024) %>% 
  group_by(team,year) %>% 
  summarise(games = n())

#Filter stats for players who have meet criteria for both freshmen and sophomore year
stats = stats %>% 
  left_join(gamecount) %>% 
  filter(g/games>=0.75 &three_a>=(2*g)) %>% 
  filter(exp=="Fr"|exp=="So") %>% 
  group_by(player) %>% 
  filter(n()==2) %>% 
  ungroup()

#Get stats for just freshmen year
fresh = stats %>% 
  filter(exp=="Fr") %>% 
  select(player,exp,team,three_pct,year)%>% 
  rename(exp_fr = exp, team_fr = team,tpp_fr = three_pct,fr_year=year)

#Get stats for just sophomore year
soph = stats %>% 
  filter(exp=="So") %>% 
  select(player,exp,team,three_pct,year) %>% 
  rename(exp_so = exp, team_so = team,tpp_so = three_pct,so_year=year)

#Left join freshmen + sophomore stats by player
merged = fresh %>% 
  left_join(soph) %>% 
  mutate(jump = tpp_so-tpp_fr) %>% 
  arrange(desc(jump)) 

#Select columns, add logos, and keep top 10
fresh = fresh %>% select(player,team_so,tpp_fr,tpp_so,jump) %>% rename(team = team_so) %>% left_join(logos) %>% 
  arrange(desc(jump)) %>% slice_head(n=10) %>% mutate(jump=paste0("+",round(jump*100,1),"%"))

#Manually edit 2 values that had rounding errors
fresh$jump[7]="+8.0%"
fresh$jump[4]="+10.0%"



#Create Header for Table
title_header <- glue(
  "<div style='display: flex; justify-content: space-between; align-items: center;'>
     <div style='flex-grow: 1;'>
       <span style='font-weight: bold; font-size: 32px; line-height: 0.6;'>Sophomore Jump</span><br>
       <span style='font-size: 16px; font-weight: normal; line-height: 0.3;'> 3-Point Shooting Improvement - 2024 Season</span>
     </div>
     <div>
       <img src='https://i.imgur.com/ZQjA0Hb.png' style='height: 70px; width: auto; vertical-align: right;'>
     </div>
   </div>")

#Create Table
plot = fresh %>% select(player,team_logo,tpp_fr,tpp_so,jump) %>% 
  gt() |> gt::fmt_markdown() %>% 
  tab_header(title = html(title_header))  %>% 
  gt_theme_538() %>% 
  cols_align(
    align = "center",
    columns = everything()) |>
  cols_align(
    align = "left",
    columns = c("player","team_logo")
  ) %>% 
  fmt_percent(
    columns = c("tpp_fr","tpp_so"),
    decimals = 1
  ) %>% 
  tab_spanner(
    label = "3-Point %",
    columns = c("tpp_fr","tpp_so")
  ) %>% 
  cols_width(
    player ~px(175),
    team_logo ~px(145),
    tpp_fr ~ px(85),
    tpp_so ~ px(85),
    jump = ~px(85))%>%
  cols_label(
    player = "Player",
    team_logo="Team",
    tpp_fr = "2023",
    tpp_so = "2024",
    jump = "Change"
  ) |>
  tab_style(
    style = cell_text(weight="bold"),
    locations = cells_body(
      rows = everything(),
      columns = c("jump")
    )) |>
  tab_style(
    style = cell_text(weight="bold"),
    locations = cells_column_labels(columns = c("jump")
    )) %>%
  tab_style(
    style = cell_fill(color = "#f2eadc"),
    locations = cells_body(
      rows = c(1),
      columns = everything()
    )) %>% 
  tab_options(data_row.padding = px(3)) %>% 
  tab_source_note(md("Criteria: 2.5 3PA/G + 75% of Team Games<br>Data via cbbdata"))|> 
  tab_style(
    style = cell_text(size = px(10)),
    locations = cells_source_notes()
  )|>gtsave("/path/3ptjump.png", expand = c(10,20,10,20), zoom=4)


