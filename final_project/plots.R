library(tidyverse)
library(scales)
library(patchwork)
library(ggrepel)
library(showtext)



#loading data
drivers <- read.csv("data/drivers.csv") %>% 
  mutate(full_name = paste(forename, surname))

results <- read.csv("data/results.csv")
results$position <- as.numeric(as.character(results$position))

races <- read.csv("data/races.csv")

driver_standings <- read.csv("data/driver_standings.csv")

constructors <- read.csv("data/constructors.csv")

font_add_google("Titillium Web", "titill")

showtext_auto()
showtext_opts(dpi = 300)

### Driver wins barplots

## wrangling data

# top 10 driver win counts
drv_wins <- results %>% 
  group_by(driverId) %>%
  filter(position == 1) %>% 
  summarise(wins = sum(position)) %>% 
  arrange(desc(wins)) %>% 
  filter(wins >= 25) %>% 
  left_join(drivers, by = "driverId") %>% 
  select(full_name, wins)

#total races entered by each driver
drv_races <- results %>% 
  group_by(driverId) %>% 
  summarise(races = n_distinct(raceId)) %>% 
  arrange(desc(races))

#wins per race for top 10 drivers
drv_wins_per_race <- results %>% 
  group_by(driverId) %>%
  filter(position == 1) %>% 
  summarise(wins = sum(position)) %>%
  left_join(drivers, by = "driverId") %>%
  left_join(drv_races, by ="driverId") %>%
  mutate(wins_prace = wins/races) %>% 
  select(full_name, wins, races, wins_prace) %>% 
  arrange(desc(wins_prace)) %>% 
  filter(full_name %in% drv_wins$full_name)

#creating vector of driver team colors
drv_colors <-  c("Lewis Hamilton" = "#00D2BE",
                 "Michael Schumacher" = "#D40000",
                 "Sebastian Vettel" = "#0600EF",
                 "Alain Prost" = "#FF8700",
                 "Ayrton Senna" = "#FF8700",
                 "Fernando Alonso" = "#FFF500",
                 "Nigel Mansell" = "#005AFF",
                 "Jackie Stewart" = "#0c4190",
                 "Niki Lauda" = "#D40000",
                 "Jim Clark" = "#004225")
##plotting

#plotting total wins
drv_wins_bar <- ggplot(drv_wins, aes(wins, fct_reorder(full_name, wins), fill = full_name)) +
  geom_col(color = "black") +
  scale_fill_manual("Name",
                    values = drv_colors) +
  labs(
    title = "Top 10 Drivers by Win Count",
    subtitle = "1950 - Present",
    x = "Wins",
    y = NULL,
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "titill"),
        plot.title = element_text(face = "bold"))

ggsave("drv_wins_bar.png", plot = drv_wins_bar, height = 5, width = 7)


#plotting wins per race
drv_wins_prace_bar <- ggplot(drv_wins_per_race, aes(wins_prace, fct_reorder(full_name, wins_prace),
                              fill = full_name)) +
  geom_col(color = "black") +
  scale_fill_manual("Name",
                    values = drv_colors) +
  scale_x_continuous(limits = c(0, 0.5),
                     labels = label_percent()) +
  labs(
    title = "Win Rates of the Top 10 Winningest Drivers",
    subtitle = "1950 - Present",
    x = "Win Rate",
    y = NULL,
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "titill"),
        plot.title = element_text(face = "bold"))

ggsave("drv_wins_prace_bar.png", plot = drv_wins_prace_bar, height = 5, width = 7)

#combining both bar plots
drv_bars <- drv_wins_bar / drv_wins_prace_bar

ggsave("drv_bars.png", plot = drv_bars, height = 5, width = 7)


### team wins barplots

## wrangling data

# top 10 highest team win counts
team_wins <- results %>% 
  group_by(constructorId) %>%
  filter(positionOrder == 1) %>% 
  summarise(wins = sum(position)) %>% 
  arrange(desc(wins)) %>% 
  filter(wins >= 23) %>% 
  left_join(constructors, by = "constructorId") %>% 
  select(name, wins)

#total races entered by each team
team_races <- results %>% 
  group_by(constructorId) %>% 
  summarise(races = n_distinct(raceId)) %>% 
  arrange(desc(races))

#wins per race for top 10 teams
team_wins_per_race <- results %>% 
  group_by(constructorId) %>%
  filter(position == 1) %>% 
  summarise(wins = sum(position)) %>%
  left_join(constructors, by = "constructorId") %>%
  left_join(team_races, by ="constructorId") %>%
  mutate(wins_prace = wins/races) %>% 
  select(name, wins, races, wins_prace) %>% 
  arrange(desc(wins_prace)) %>% 
  filter(name %in% team_wins$name)

# creating vector of team colors
team_colors <- c("Mercedes" = "#00D2BE",
                 "Ferrari" = "#DC0000",
                 "Red Bull" = "#0600EF",
                 "McLaren" = "#FF8700",
                 "Williams" = "#005AFF",
                 "Team Lotus" = "#004225",
                 "Benetton" = "#7BEDFB",
                 "Renault" = "#FFF500",
                 "Brabham" = "#303c32",
                 "Tyrrell" = "#0c4190")

##plotting

#plotting total team wins
team_wins_bar <- ggplot(team_wins, aes(wins, fct_reorder(name, wins), fill = name)) +
  geom_col(color = "black") +
  scale_fill_manual("Name",
                    values = team_colors) +
  labs(
    title = "Top 10 Teams by Win Count",
    subtitle = "1950 - Present",
    x = "Wins",
    y = NULL,
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "titill"),
        plot.title = element_text(face = "bold"))

ggsave("team_wins_bar.png", plot = team_wins_bar, height = 5, width = 7)


#plotting team wins per race
team_wins_prace_bar <- ggplot(team_wins_per_race, aes(wins_prace, fct_reorder(name, wins_prace),
                              fill = name)) +
  geom_col(color = "black") +
  scale_fill_manual("Name",
                    values = team_colors) +
  scale_x_continuous(limits = c(0, 0.5),
                     labels = label_percent()) +
  labs(
    title = "WIn Rates of the Top 10 Winningest Teams",
    subtitle = "1950 - Present",
    x = "Win Rate",
    y = NULL,
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "titill"),
        plot.title = element_text(face = "bold"))

ggsave("team_wins_prace_bar.png", plot = team_wins_prace_bar, height = 5, width = 7)


#combining both bar plots
team_bars <- team_wins_bar / team_wins_prace_bar

ggsave("team_bars.png", plot = team_bars, height = 5, width = 7)


###boxplot of overtakes over time

##data wrangling

# creating variables for overtaking and engine type
results <- results %>%
  mutate(pos_change = abs(positionOrder - grid))

races <- races %>%
  mutate(engine = case_when(
    (year >= 2000) & (year <= 2005) ~ "V10",
    (year >= 2006) & (year <= 2013) ~ "V8",
    (year >= 2014) ~ "V6"
  )) %>%
  filter(year >= 2000 & year <= 2021)

#merging data
box_data <- left_join(races, results, by = "raceId")

box_data$engine <- factor(box_data$engine, levels = c("V10", "V8", "V6"),
                          ordered = TRUE)

##plotting
pos_box <- ggplot(box_data, aes(engine, pos_change, fill = engine)) +
  stat_boxplot(geom = "errorbar", width = 0.3) +
  geom_boxplot() +
  scale_x_discrete(labels = c(
    "V10\n(2000 - 2005)",
    "V8\n(2006 - 2013)",
    "V6\n(2014 - 2021)"
  )) +
  scale_fill_manual(values = c(
    "#FF1801", "orange", "yellow"
  )) +
  labs(
    x = NULL,
    y = "Position Changes",
    title = "Distribution of Driver Position Changes per Race by Regulation Era",
    subtitle = "2000-2021"
  ) +
  theme_minimal() + 
  theme(legend.position = "none",
        text = element_text(family = "titill"),
        plot.title = element_text(face = "bold"))

ggsave("pos_box.png", plot = pos_box, height = 5, width = 7)

### lineplot of cumulative points over 2021 season

##filtering and merging data for 2021 season
races_2021 <- races %>% 
  filter(year == 2021)

standings_2021 <- driver_standings %>% 
  filter(raceId %in% races_2021$raceId)

drivers_2021 <- drivers %>% 
  filter(driverId %in% standings_2021$driverId)

line_data <- left_join(standings_2021, races_2021, by = "raceId") %>%
  left_join(drivers_2021, by = "driverId")

#creating variable for 2021 driver teams and teammate finishing order
line_data <- line_data %>% 
  mutate(team = case_when(
    code %in% c("ALO", "OCO") ~ "Alpine",
    code %in% c("GIO", "RAI", "KUB") ~ "Alfa Romeo",
    code %in% c("GAS", "TSU") ~ "AlphaTauri",
    code %in% c("BOT", "HAM") ~ "Mercedes",
    code %in% c("LAT", "RUS") ~ "Williams",
    code %in% c("LEC", "SAI") ~ "Ferrari",
    code %in% c("MAZ", "MSC") ~ "Haas",
    code %in% c("NOR", "RIC") ~ "McLaren",
    code %in% c("PER", "VER") ~ "Red Bull",
    code %in% c("STR", "VET") ~ "Aston Martin"
  ),
  drv_order = ifelse(code %in% c("VER", "HAM", "SAI", "NOR",
                                 "GAS", "ALO", "VET", "RUS", "RAI", "MSC"),
                     "First", "Second")
  )

#factoring teammate finishing order so it can be mapped to linetype
line_data$drv_order <- factor(line_data$drv_order, levels = c("First", "Second"),
                                     ordered = TRUE)

#creating vector of 2021 team colors
team_colors_2021 <- c(
  "Mercedes" = "#00D2BE",
  "Red Bull" = "#0600EF",
  "Ferrari" = "#DC0000",
  "McLaren" = "#FF8700",
  "Alpine" = "#0090FF",
  "AlphaTauri" = "#2B4562",
  "Aston Martin" = "#006F62",
  "Williams" = "#005AFF",
  "Alfa Romeo" = "#900000",
  "Haas" = "#787878"
)

#filtering data to create labels at final standings
label_data <- filter(line_data, round == 22)

##plotting
standings_line <- ggplot(line_data, aes(round, points, color = team,
                      group = code, linetype = factor(drv_order))) +
  geom_line(size = 1) +
  scale_color_manual(values = team_colors_2021) +
  scale_linetype_manual(values = c("First" = "solid",
                                   "Second" = "dashed"),
                        guide = "none") +
  geom_text_repel(data = label_data,
                  aes(label = code),
                   box.padding = 0.6,
                   min.segment.length = 0,
                   hjust = 1,
                  nudge_x = 1,
                  show.legend = FALSE) +
  labs(
    x = "Races",
    y = "Points",
    title = "2021 World Drivers' Championship Standings",
    color = "Team"
  ) +
  theme_minimal() +
  theme(legend.justification = c("left", "top"),
        legend.position = c(0.1,0.98),
        text = element_text(family = "titill"),
        plot.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold")) +
  guides(color = guide_legend(
    override.aes = list(size = 1.3)))

ggsave("standings_line.png", plot = standings_line, height = 5, width = 7)

### scatterplot of average quali position vs avg finishing position

## wrangling data
scatter_data <- left_join(line_data, results, by = c("raceId", "driverId")) %>% 
  group_by(code) %>% 
  mutate(avg_quali = mean(grid, na.rm = TRUE),
         avg_race = mean(position.y, na.rm = TRUE)) %>% 
  distinct(code, .keep_all = TRUE)

##plotting
quali_race_scatter <- ggplot(scatter_data, aes(avg_quali, avg_race, color = team)) +
  geom_point(size = 2.7) +
  geom_abline(linetype = "dashed") +
  geom_text_repel(aes(label = code),
                  box.padding = 0.7,
                  min.segment.length = 0,
                  show.legend = FALSE,
                  max.overlaps = 6) +
  scale_color_manual(values = team_colors_2021) +
  scale_x_continuous(limits = c(0,20)) +
  scale_y_continuous(limits = c(0,20)) +
  labs(
    title = "Drivers' Average Finishing Position Versus Average Qualifying Position",
    subtitle = "2021 Season",
    x = "Qualifying Position",
    y = "Finishing Position",
    color = "Team"
  ) +
  theme_minimal() +
  theme(text = element_text(family = "titill"),
        plot.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))

ggsave("quali_race_scatter.png", plot = quali_race_scatter, height = 5, width = 7)

