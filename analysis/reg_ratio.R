# devtools::install_github("slarge/fedregs")
library(dplyr)
library(fedregs)
# library(quanteda)
# library(tidyr)
library(ggplot2)

# reg <- purrr::map_df(1996:2017, ~ cfr_text(year = .x,
#                                           title_number = 50,
#                                           chapter = 6,
#                                           part = 648,
#                                           return_tidytext = FALSE,
#                                           verbose = TRUE))

# saveRDS(reg, "analysis/title50_1996-2017.rds")
reg <- readRDS("analysis/title50_1996-2017.rds")
bad_words <- c("to", "is", "a", "the", "that", "this", "it","its","itself",
               "these", "those", "am", "were", "are", "was", "and", "but", "an",
               "if", "or", "because", "as", "of", "at", "in", "for", "be", "by",
               "with", "on", "any", "than", "all", "been", "each", "such")

bad_words <- paste(bad_words, collapse = "|")

bad_words <- paste0("\\b(", bad_words, ")\\b")

stop_words <- quanteda::stopwords("english")

bad_words[bad_words %in% stop_words]
raw_words <- stop_words[!stop_words %in% bad_words]


td <- unique(word_regs$word[word_regs$word %in% raw_words])

clean_regs <- reg %>%
  tidyr::unnest() %>%
  mutate(TEXT = gsub("^c\\(", "", TEXT),
         TEXT = gsub("([/-])|[[:punct:]]", "\\1", TEXT), # remove any remaining punctuation
         TEXT = gsub("[[:digit:]]", "", TEXT),
         TEXT = gsub("—", "", TEXT),
         TEXT = gsub("[^[:ascii:]]", "", TEXT, perl = TRUE),
         # TEXT = gsub(bad_words, "", TEXT),
         NULL)

word_regs <- clean_regs %>%  # remove digits (e.g., 1st, 1881a, 15th, etc)
  mutate(TEXT = gsub("\\bmay not\\b", "weenieroast", TEXT)) %>%
  tidytext::unnest_tokens(output = word, input = TEXT, token = "words") %>%
  filter(is.na(as.numeric(word)),
         !grepl("^m{0,4}(cm|cd|d?c{0,3})(xc|xl|l?x{0,3})(ix|iv|v?i{0,3})$",
                word), # adios Roman Numerals
         !grepl("\\b[a-z]{1}\\b", word), # get rid of one letter words
         !grepl("\\bwww*.", word)) %>%
  dplyr::mutate(word = quanteda::char_wordstem(word),
                word = case_when(word == "weenieroast" ~ "may not",
                                 TRUE ~ word))

count_regs <- word_regs %>%
  group_by(year) %>%
  summarize(n = n())

reg_words <- c("shall", "must", "may not", "prohibited", "required")

ratio_regs <- word_regs %>%
  filter(word %in% reg_words) %>%
  group_by(year) %>%
  summarize(reg_words = n()) %>%
  left_join(count_regs, by = c("year")) %>%
  mutate(ratio = reg_words/n)



count_regs %>%
  arrange(-words) %>%
  head(100) %>%
  pull(word)





count_words <- clean_words %>%
  group_by(word) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(-n) %>%
  top_n(n = 25, wt = n) %>%
  mutate(word = reorder(word, n))

ggplot(ratio_regs, aes(x = year, ratio)) +
  geom_line() +
  labs(xlab = NULL,
       title = "Code of Federal Regulations (2017)",
       subtitle = "Title 50, Chapter VI, Part 648",
       caption = sprintf("Data accessed on %s from:\n https://www.gpo.gov/fdsys/browse/collectionCfr.action?collectionCode=CFR",
                         format(Sys.Date(), "%d %B %Y"))) +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1),
  #       legend.direction = "horizontal",
  #       legend.position = "bottom",
  #       text = element_text(size = 8)) +
  # coord_flip() +
  theme_minimal()
