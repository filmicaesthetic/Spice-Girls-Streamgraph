library(tidyverse)
library(data.table)
library(ggridges)
library(ggstream)
library(extrafont)
library(grid)
library(gridExtra)
library(cowplot)

# import data
studio_album_tracks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/studio_album_tracks.csv')
lyrics <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/lyrics.csv')
related_artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/related_artists.csv')

# get list of songs
song_list <- lyrics %>% group_by(song_name) %>% summarise(ln = sum(line_number)) %>% select(song_name)

# find individual section artists, create 2 "all" categories for plotting
lyrics_names <- lyrics %>% 
  mutate(Scary = as.numeric((as.numeric(grepl("Scary", section_artist, fixed = TRUE))) > 0), 
         Baby = as.numeric((as.numeric(grepl("Baby", section_artist, fixed = TRUE))) > 0), 
         Ginger = as.numeric((as.numeric(grepl("Ginger", section_artist, fixed = TRUE))) > 0), 
         Sporty = as.numeric((as.numeric(grepl("Sporty", section_artist, fixed = TRUE))) > 0), 
         Posh = as.numeric((as.numeric(grepl("Posh", section_artist, fixed = TRUE))) > 0),
         All1 = as.numeric((as.numeric(grepl("All", section_artist, fixed = TRUE))) > 0) * (5 - Scary - Baby - Ginger - Sporty - Posh) / 2,
         All2 = as.numeric((as.numeric(grepl("All", section_artist, fixed = TRUE))) > 0) * (5 - Scary - Baby - Ginger - Sporty - Posh) / 2)

#select relevant variables
Song <- lyrics_names %>% filter() %>% select(line_number, song_name, track_number, album_name, All1, Scary, Baby, Ginger, Sporty, Posh, All2)

# get total lines in each song
Song_list <- Song %>% group_by(song_name) %>% summarise(lines = max(line_number))

# reshape to long 
Song_long <- melt(setDT(Song), id.vars = c("line_number", "song_name", "track_number", "album_name"), variable.name = "section_artist")
Song_dt <- Song_long %>% filter(value > 0)

# order by line_number
Song_dt <- Song_dt %>% arrange(line_number)

# get total lines from song_list
Song_dt <- merge(Song_dt, Song_list, all.x = TRUE)

# calculate value to balance charts
Song_x <- Song_dt %>% group_by(section_artist, song_name, track_number, album_name) %>% mutate(count = n() / lines)

# format names for display
Song_x$song_name <- toupper(Song_x$song_name)
Song_x$song_name = str_wrap(Song_x$song_name, width = 14)

# Spice plot
Spice <- Song_x %>% filter(album_name == "Spice") %>% ggplot(aes(x = line_number, y = count)) +
  facet_grid(. ~ song_name, scales = "free", space='free', switch = "x") +
  geom_stream(bw = 0.65, aes(fill = section_artist)) +
  scale_fill_manual(Song_x$section_artist, values = alpha(c("#bd9975",wes_palette("Darjeeling1"),"#bd9975"),0.6)) +
  ggtitle("Spice") + 
  theme(panel.background = element_rect(fill = "#ffffff"),
        plot.background = element_rect(fill = "#ffffff"),
        panel.grid.major = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "none",
        legend.box.margin = margin(r = 0, l = 0),
        panel.grid.minor = element_blank(),
        plot.title = element_text(family = "Built Titling Sb", color = "#9e7f60", size = 24, hjust = 0.5),
        axis.text = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(color = "#80664d"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(family="Built Titling Sb", face = "bold", color = "#9e7f60"))


# Spiceworld plot
Spiceworld <- Song_x %>% filter(album_name == "Spiceworld") %>% ggplot(aes(x = line_number, y = count)) +
  facet_grid(. ~ song_name, scales = "free", space='free', switch = "x") +
  geom_stream(bw = 0.65, aes(fill = section_artist)) +
  scale_fill_manual(Song_x$section_artist, values = alpha(c("#bd9975",wes_palette("Darjeeling1"),"#bd9975"),0.6)) +
  ggtitle("Spiceworld") + 
  theme(panel.background = element_rect(fill = "#ffffff"),
        plot.background = element_rect(fill = "#ffffff"),
        panel.grid.major = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "none",
        legend.box.margin = margin(r = 0, l = 0),
        panel.grid.minor = element_blank(),
        plot.title = element_text(family = "Built Titling Sb", color = "#9e7f60", size = 24, hjust = 0.5),
        axis.text = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(color = "#80664d"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(family="Built Titling Sb", face = "bold", color = "#9e7f60"))

# Forever plot
Forever <- Song_x %>% filter(album_name == "Forever") %>% ggplot(aes(x = line_number, y = count)) + 
  facet_grid(. ~ song_name, scales = "free", space='free', switch = "x") + 
  geom_stream(bw = 0.65, aes(fill = section_artist)) + 
  scale_fill_manual(Song_x$section_artist, values = alpha(c("#bd9975","#FF0000", "#00A08A", "#F98400", "#5BBCD6","#bd9975"),0.6)) + 
  ggtitle("Forever") + 
  labs(caption = "Data Source: Jacqui Tran via Spotify and Genius") +
  theme(panel.background = element_rect(fill = "#ffffff"),
        plot.background = element_rect(fill = "#ffffff"),
        panel.grid.major = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "none",
        legend.box.margin = margin(r = 0, l = 0),
        panel.grid.minor = element_blank(),
        plot.title = element_text(family = "Built Titling Sb", color = "#9e7f60", size = 24, hjust = 0.5),
        axis.text = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(color = "#80664d"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(family="Built Titling Sb", face = "bold", color = "#9e7f60"))

# build legend with bar chart
leg <- data.frame(VOCALS = as.factor(c("SCARY", "BABY", "GINGER", "SPORTY", "POSH", "ALL")), value = c(1, 1, 1, 1, 1, 1))
leg$VOCALS <- factor(leg$VOCALS, levels = leg$VOCALS)

# legend plot
hist <- ggplot(leg, aes(x = VOCALS, y = value, fill = VOCALS)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(y = 0.5, label = VOCALS), family = "Built Titling Sb", color = "#ffffff", size = 8) +
  scale_fill_manual(Song_x$section_artist, values = alpha(c(wes_palette("Darjeeling1"),"#bd9975"),0.6)) +
  ggtitle("Spice Girls Songs by Singer") + 
  theme(panel.background = element_rect(fill = "#ffffff"),
        plot.background = element_rect(fill = "#ffffff"),
        panel.grid.major = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "none",
        legend.box.margin = margin(r = 0, l = 0),
        panel.grid.minor = element_blank(),
        plot.title = element_text(family = "Built Titling Sb", color = "#9e7f60", size = 20, face="bold", hjust = 0.5),
        axis.text = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(color = "#80664d"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(family="Built Titling Sb", face = "bold", color = "#9e7f60"))

# combine plots
plot <- plot_grid(hist, Spice, Spiceworld, Forever, nrow = 4, rel_heights = c(1/8, 7/3/8, 7/3/8, 7/3/8))

plot

# save plot image
ggsave("Spice_Plot.png", plot = plot, dpi = 300, width = 25, height = 21, units = "cm")