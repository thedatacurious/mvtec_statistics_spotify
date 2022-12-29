library(tidyverse)
library(janitor)
library(styler)

## In the original data, the "key" column uses integers (e.g. 0 = C) to map to pitches.
## Create a new data-frame to show the actual Pitch Class notation

key_alpha <- c("C", "C#/Db", "D", "D#/Eb", "E", "F", "F#/Gb", "G", "G#/Ab", "A", "A#/Bb", "B")

key_map <- data.frame(
  key = c(0:11),
  key_alpha = key_alpha
)

data <- read_csv("Downloads/spotifyData.csv") %>%
  clean_names() %>%
  full_join(key_map, "key") %>%
  mutate(
    mode = str_replace(as.character(mode), "0", "minor"),
    mode = str_replace(as.character(mode), "1", "major"),
    time_signature = as.character(time_signature),
    ## Replace "key" column with version that shows actual Pitch Class notation
    key = key_alpha,
    ## Add a column to indicate whether the track is by one artist entity or multiple entities (A band counts as one entity)
    ## True for multiple, false if otherwise
    multiple_artists = grepl(";", artists)
  ) %>%
  ## Drop the column which only shows row numbers) and the extra "key" column from the table join
  select(-1, -22) %>%
  ## Remove duplicate tracks based on track-artist combo
  ## Some tracks appear in multiple albums (e.g. released again as a single)
  distinct(track_name, artists, .keep_all = TRUE)

## Filter data by artists with at least 20 tracks
## Most tracks should be 10 minutes or shorter in duration

popular_artists <- data %>%
  group_by(artists) %>%
  summarize(count = n()) %>%
  filter(count >= 20)

filtered <- data %>%
  filter(artists %in% popular_artists$artists) %>%
  filter(duration_ms <= 600000)

## Ensure that our observations aren't missing values
nrow(filtered) == nrow(filtered[complete.cases(filtered),])


## Further processing for PCA Analysis

# tempo <- read.csv("tempo.csv") %>% clean_names()
#
# filtered <- filtered  %>%
#   ## Remove the track_id, title, artists, albums which are not needed.
#   select(5:21) %>%
#   mutate(
#     tempo_cat = cut(tempo,
#       breaks = c(0, 20, 40, 60, 66, 76, 108, 120, 168, 176, 200, 1000),
#       labels = c("Larghissimo", "Grave", "Lento/Largo", "Larghetto", "Adagio", "Andante", "Moderato", "Allegro", "Vivace", "Presto", "Prestissimo")
#     )
#   )

