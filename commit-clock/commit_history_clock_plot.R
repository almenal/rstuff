library(tidyverse)
library(lubridate)
library(showtext)
showtext_auto()


## Generate git log -----------

git_repo_folder <- "/path/to/your/git/folder"
commits <- system(
  paste("cd", 
        git_repo_folder, 
        '&& git log --pretty=format:"%ad,%h,%an,%s" --date=iso'),
  intern = TRUE
)

gitlog <- str_split(commits, ",", n=4, simplify = TRUE) %>% 
  as_tibble() %>% 
  rename("timestamp" = V1, "sha_hash" = V2, "author" = V3, "message" = V4)


# Font shenanigans --------------------------------------------------------
# I did this plot first on windows, where "Bookman Old Style" was available.
# Then I re-did it in a machine where it was not, so had to change it to 
# "American Typewriter"

fonts <- sysfonts::font_files()

tmp <- fonts %>% filter(str_detect( str_to_lower(family), 'bookman')) %>% head(10)
tmp <- fonts %>% filter(str_detect( str_to_lower(family), 'american')) %>% head(10)
rm(tmp)

tryCatch(font_add('Bookman Old Style', regular = "BOOKOS.TTF"),
         error = function(e) NULL)
tryCatch(font_add('American Typewriter', regular = "AmericanTypewriter.ttc"),
         error = function(e) NULL)

if(any(str_detect(fonts$family %>% str_to_lower(), "Bookman Old Style"))){
  font_family <- "Bookman Old Style"
} else {
  font_family <- "American Typewriter"
}


# Color palette -----------------------------------------------------------
# Courtesy of: https://www.nordtheme.com/

nord_theme <- list(
  red="#BF616A",
  orange="#D08770",
  yellow="#EBCB8B",
  green="#A3BE8C",
  purple="#B48EAD",
  cyan="#88C0D0",
  deep_blue="#5E81AC",
  snow_storm_0="#ECEFF4"
)


# Data prep ---------------------------------------------------------------

df <- gitlog %>%
  mutate(
    time = lubridate::as_datetime(timestamp),
    time_h = hour(time),
    time_hm_num = time_h + (minute(time)/60)
  )

polar_annotations <- tibble(time_h = 6, n = c(10,20,30))
hour_freqs <- df %>%
  mutate(time_h = factor(time_h, levels = 0:23)) %>%
  count(time_h, .drop = FALSE) %>%
  mutate(time_h = as.numeric(time_h)-1)
y_labels_pos <- 33 # sorry for the hard-coding!
clock_sections <- tribble(
  ~ section, ~ start, ~ finish,
  'morning', 6, 12,
  'afternoon', 12, 18,
  'evening', 18, 24,
  'night', 0, 6,
)

# Plot --------------------------------------------------------------------

p <- (
  ggplot(hour_freqs, aes(time_h, n))
  
  # Background decorations, axis lines, axis annotations
  + geom_rect(data = clock_sections, inherit.aes = FALSE, alpha = 0.5,
              aes(xmin=start, xmax=finish, ymin=0, ymax=Inf, fill = section))
  + geom_segment(aes(x=0, xend=24, y=10, yend = 10), size = 0.1, color = 'gray60')
  + geom_segment(aes(x=0, xend=24, y=20, yend = 20), size = 0.1, color = 'gray60')
  + geom_segment(aes(x=0, xend=24, y=30, yend = 30), size = 1.5, color = 'gray40')
  + geom_text(data = polar_annotations, aes(label = n), hjust=1.2, size = 9)
  
  # Clock numbers
  + geom_point(aes(y = y_labels_pos), shape = 21, color = 'black',
               fill = '#E5E9F0', size = 10)
  + geom_text(aes(y = y_labels_pos, label = time_h), 
              family = font_family, size = 10)
  
  # Actual data - commits per hour
  + geom_segment(aes(xend = time_h, y=0, yend=n))
  + geom_point(shape = 21, fill = 'white', size = 2)

  # Aesthetics  
  + labs(x = "", y = "", fill = "", title ="Number of commits per time of day")
  + coord_polar()
  + scale_x_continuous(limits = c(0,24), breaks = seq(0, 23, 1),
                       minor_breaks = NULL, labels = NULL)
  + scale_y_continuous(breaks = NULL, minor_breaks = NULL, labels = NULL)
  + scale_fill_manual(
    values = c(
      night = nord_theme$deep_blue,
      morning = nord_theme$yellow,
      afternoon = nord_theme$green,
      evening = nord_theme$orange
    )
  )
  + theme_minimal()
  + theme(
    # Theme elements messed up in polar plot,
    # so disable all and use geom_segments
    panel.grid.major.y = element_blank(),
    # axis.line = element_blank()
    # axis.text.x = element_text(vjust = -5)
    text = element_text(family = font_family, size = 24),
    legend.position = "bottom",
    legend.text = element_text(size = 28),
    legend.box.spacing = unit(-1, 'line'),
    plot.title = element_text(hjust = 0.5, size = 48),
    plot.margin = margin(b=1.5, t=1, unit = 'line'),
    panel.spacing = unit(0, 'line'),
  )
)

print(p)
ggsave(here::here("commit-clock.png"),
       plot = p,
       width = 6.5,
       height = 6.5,
       bg = nord_theme$snow_storm_0)
