library(jsonlite)
library(dplyr)
library(ggplot2)

# ---------- CONFIG ----------
username <- "U1Kemp"
output_file <- "images/languages.png"

# ---------- FETCH REPOS ----------
repos_url <- paste0(
  "https://api.github.com/users/",
  username,
  "/repos?per_page=100"
)

repos <- fromJSON(repos_url)

# ---------- FETCH LANGUAGES ----------
lang_list <- lapply(repos$languages_url, function(url) {
  if (is.na(url)) return(NULL)
  fromJSON(url)
})

lang_df <- bind_rows(lapply(lang_list, function(x) {
  if (is.null(x)) return(NULL)
  tibble(
    language = names(x),
    bytes = as.numeric(x)
  )
}))

# ---------- AGGREGATE ----------
lang_summary <- lang_df %>%
  group_by(language) %>%
  summarise(bytes = sum(bytes), .groups = "drop") %>%
  mutate(percent = bytes / sum(bytes) * 100) %>%
  arrange(desc(percent))

# ---------- PLOT ----------
p <- ggplot(lang_summary, aes(
  x = reorder(language, percent),
  y = percent,
  fill = language
)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(round(percent, 3), "%")),
            hjust = -0.1,
            size = 3.2,
            color = "white") +
  scale_y_continuous(limits = c(0, 85), expand = expansion(mult = c(0, 0.05))) +
  coord_flip() +
  labs(
    title = "",
    x = NULL,
    y = "Percentage (%)"
  ) +
  theme_dark(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "#0d1117", color = NA),
    panel.background = element_rect(fill = "#0d1117", color = NA),
    text = element_text(color = "#c9d1d9"),
    axis.text = element_text(color = "#c9d1d9"),
    plot.title = element_text(color = "#ff5733", face = "bold")
  )

dir.create("images", showWarnings = FALSE)

ggsave(
  filename = output_file,
  plot = p,
  width = 6,
  height = 4,
  dpi = 150
)
