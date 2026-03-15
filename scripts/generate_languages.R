library(jsonlite)
library(dplyr)
library(ggplot2)
library(httr)

# ---------- EXTENSION TO LANGUAGE MAP AND FUNCTION ----------
ext_to_lang <- list(
  "py" = "Python",
  "js" = "JavaScript",
  "ts" = "TypeScript",
  "java" = "Java",
  "cpp" = "C++",
  "c" = "C",
  "cs" = "C#",
  "rb" = "Ruby",
  "go" = "Go",
  "rs" = "Rust",
  "php" = "PHP",
  "swift" = "Swift",
  "kt" = "Kotlin",
  "scala" = "Scala",
  "r" = "R",
  "m" = "MATLAB",
  "jl" = "Julia",
  "sh" = "Shell",
  "pl" = "Perl",
  "dart" = "Dart",
  "lua" = "Lua",
  "hs" = "Haskell",
  "html" = "HTML",
  "css" = "CSS",
  "json" = "JSON",
  "xml" = "XML",
  "ipynb" = "Jupyter Notebook",
  "tex" = "TeX"
)

get_language <- function(filename) {
  ext <- tolower(tools::file_ext(filename))
  if (ext %in% names(ext_to_lang)) {
    ext_to_lang[[ext]]
  } else {
    NA_character_
  }
}


# ---------- CONFIG ----------
username <- "U1Kemp"
output_file <- "images/languages.png"

# ---------- FETCH REPOS ----------
repos_url <- paste0(
  "https://api.github.com/users/",
  username,
  "/repos?per_page=100"
)

# Read GitHub token from environment (set in Actions as GITHUB_TOKEN)
token <- Sys.getenv("GITHUB_TOKEN", "")

# Helper to fetch JSON from GitHub with optional auth
fetch_json <- function(url) {
  if (is.null(url) || is.na(url) || url == "") return(NULL)
  headers <- c(`User-Agent` = "R generate_languages script")
  if (nzchar(token)) headers <- c(headers, Authorization = paste("token", token))
  resp <- GET(url, add_headers(.headers = headers))
  stop_for_status(resp)
  txt <- content(resp, as = "text", encoding = "UTF-8")
  fromJSON(txt)
}

repos <- fetch_json(repos_url)

# Helper to fetch file tree for a repo (recursive)
fetch_tree <- function(owner, repo) {
  url <- paste0("https://api.github.com/repos/", owner, "/", repo, "/git/trees/HEAD?recursive=1")
  fetch_json(url)
}

# Get all file paths for all repos
file_langs <- list()
for (i in seq_len(nrow(repos))) {
  repo_name <- repos$name[i]
  tree <- fetch_tree(username, repo_name)
  if (!is.null(tree$tree)) {
    files <- tree$tree[tree$tree$type == "blob", ]
    if (nrow(files) > 0) {
      langs <- sapply(files$path, get_language)
      file_langs[[repo_name]] <- langs[!is.na(langs)]
    }
  }
}

# Flatten and count
all_langs <- unlist(file_langs)
lang_count <- as.data.frame(table(all_langs), stringsAsFactors = FALSE)
colnames(lang_count) <- c("language", "files")
lang_count <- lang_count %>% arrange(desc(files))
lang_count <- lang_count %>% filter(!is.na(language))
# ---------- FETCH LANGUAGES ----------
lang_list <- lapply(repos$languages_url, function(url) {
  if (is.na(url)) return(NULL)
  fetch_json(url)
})

lang_df <- bind_rows(lapply(lang_list, function(x) {
  if (is.null(x)) return(NULL)
  tibble(
    language = names(x),
    bytes = as.numeric(x)
  )
}))


# ---------- AGGREGATE ----------
lang_summary <- lang_count %>%
  mutate(percent = files / sum(files) * 100) %>%
  filter(percent >= 0.01)


# ---------- PLOT ----------
p <- ggplot(lang_summary, aes(
  x = reorder(language, percent),
  y = percent,
  fill = language
)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(round(percent,2),"%")),
            hjust = -0.1,
            size = 3.2,
            color = "white") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5))) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Files (%)"
  ) +
  theme_dark(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "#0d1117", color = NA),
    panel.background = element_rect(fill = "#0d1117", color = NA),
    text = element_text(color = "#c9d1d9"),
    axis.text = element_text(color = "#c9d1d9")
  )

dir.create("images", showWarnings = FALSE)

ggsave(
  filename = output_file,
  plot = p,
  width = 5,
  height = 3.5,
  dpi = 150
)
