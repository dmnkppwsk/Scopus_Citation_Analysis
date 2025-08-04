# === Scopus Author Analysis Script ===
# Author: Dominik Popowski & ChatGPT (OpenAI)
# Purpose: Analyze publication frequency and trends from Scopus-exported data
# Output: Top authors, filtered lists (excl. Poland), plots saved to "plots/"
#
# === Instructions for data export from Scopus ===
# 1. Go to Export → CSV
# 2. Select:
#    [x] Citation information
#    [x] Bibliographical information
# 3. Save the exported file(s) to your working directory (set via setwd()).
# 4. If your author has more than 5000 publications, Scopus will NOT automatically split the export.
#    You need to manually export the data in separate files (e.g., by selecting different year ranges).
#

library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(readr)
library(purrr)
library(stringi)
library(ggpubr)

# === SETUP ===
if (interactive()) {
  cat("Please choose any file inside the target working directory...
")
  dummy_file <- file.choose()
  wd <- dirname(dummy_file)
  message("Using working directory: ", wd)
  setwd(wd)
} else {
  wd <- getwd()
  message("Using working directory: ", wd)
  setwd(wd)
}
dir.create("plots", showWarnings = FALSE)

# === READ AND MERGE ALL SCOPUS FILES IN DIRECTORY ===
csv_files <- list.files(pattern = "\\.csv$")
df_list <- lapply(csv_files, read_csv, col_types = cols(.default = "c"))
df <- bind_rows(df_list) %>% distinct()

# === AUTHOR NAME CLEANING FUNCTION ===
extract_names <- function(fullnames) {
  names <- unlist(str_split(fullnames, ";"))
  clean_names <- str_trim(str_remove(names, "\\s*\\(.*?\\)"))
  return(clean_names)
}

# === TOP 100 AUTHORS (ALL RECORDS) ===
all_names <- df %>%
  filter(!is.na(`Author full names`)) %>%
  pull(`Author full names`) %>%
  map(extract_names) %>%
  unlist()

top_100 <- as.data.frame(table(all_names)) %>%
  arrange(desc(Freq)) %>%
  slice(1:100)

p1 <- ggplot(top_100, aes(x = reorder(all_names, Freq), y = Freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 100 authors (all records)", x = "Author", y = "Number of publications") +
  theme_minimal(base_size = 10)

print(p1)
ggsave("plots/top_100_authors.png", plot = p1, width = 10, height = 12)

# === FILTER OUT POLISH AFFILIATIONS ===
pl_keywords <- c("Poland", "Polska", "Warsaw", "Wroclaw", "Krakow", "Lodz", "Poznan",
                 "Gdansk", "Katowice", "Lublin", "Bialystok", "Szczecin", "Opole", "Torun", "Rzeszow")

df_no_pl <- df %>%
  filter(!str_detect(tolower(Affiliations), paste(tolower(pl_keywords), collapse = "|")))

# === TOP 100 AUTHORS (WITHOUT POLAND) ===
names_no_pl <- df_no_pl %>%
  filter(!is.na(`Author full names`)) %>%
  pull(`Author full names`) %>%
  map(extract_names) %>%
  unlist()

top_100_no_pl <- as.data.frame(table(names_no_pl)) %>%
  arrange(desc(Freq)) %>%
  slice(1:100)

p2 <- ggplot(top_100_no_pl, aes(x = reorder(names_no_pl, Freq), y = Freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 100 authors (excluding Polish affiliations)", x = "Author", y = "Number of publications") +
  theme_minimal(base_size = 10)

print(p2)
ggsave("plots/top_100_authors_no_poland.png", plot = p2, width = 10, height = 12)

write_csv(top_100_no_pl, "plots/top_100_authors_no_poland.csv")

# === PUBLICATION TRENDS FOR AUTHORS WITH ≥ N RECORDS ===
n <- 20
plot_list <- list()

top_authors_nplus <- top_100_no_pl %>%
  filter(Freq >= n) %>%
  pull(names_no_pl)

yearly_counts <- df_no_pl %>%
  filter(!is.na(`Author full names`), !is.na(Year)) %>%
  rowwise() %>%
  mutate(Author = list(extract_names(`Author full names`))) %>%
  tidyr::unnest_longer(Author) %>%
  filter(Author %in% top_authors_nplus) %>%
  mutate(Year = as.integer(Year)) %>%
  group_by(Author, Year) %>%
  summarise(Publications = n(), .groups = "drop")

for (author in top_authors_nplus) {
  plot_data <- yearly_counts %>% filter(Author == author)
  start_year <- min(plot_data$Year, na.rm = TRUE)
  end_year <- max(plot_data$Year, na.rm = TRUE)
  plot_data <- complete(plot_data, Year = seq(start_year, end_year), fill = list(Publications = 0))
  
  p <- ggplot(plot_data, aes(x = Year, y = Publications)) +
    geom_line() +
    geom_point() +
    labs(title = paste("Annual publications:", author),
         x = "Year", y = "Number of publications") +
    theme_minimal(base_size = 10)
  
  plot_list[[author]] <- p
  
  filename_safe <- str_replace_all(author, "[ ,]", "_") %>%
    stringi::stri_trans_general("Latin-ASCII")
  
  ggsave(filename = paste0("plots/author_", filename_safe, ".png"),
         plot = p, width = 6, height = 4)
  print(p)
}

# === SAVE COMBINED PDF WITH ALL AUTHOR TRENDS ===
pdf("plots/author_trends_combined.pdf", width = 8, height = 5)
for (p in plot_list) print(p)
dev.off()
