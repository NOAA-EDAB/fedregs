devtools::install_github("slarge/fedregs")
library(dplyr)
library(fedregs)

## Word counts
total_words <- function(year, title_number, chapter, part){

  tt <- cfr_text(year = year,
                 title_number = title_number,
                 chapter = chapter,
                 part = part,
                 return_tidytext = TRUE,
                 verbose = FALSE)

  stop_words <- dplyr::data_frame(word = quanteda::stopwords("english"))

  clean_words <- tt %>%
    tidyr::unnest() %>%
    dplyr::mutate(word = gsub("[[:punct:]]", "", word), # remove punctuation
                  word = gsub("^[[:digit:]]*", "", word)) %>%  # remove digits (e.g., 1st, 1881a, etc)
    dplyr::anti_join(stop_words, by = "word") %>%  # remove "stop words"
    dplyr::filter(is.na(as.numeric(word)),
                  !grepl("^m{0,4}(cm|cd|d?c{0,3})(xc|xl|l?x{0,3})(ix|iv|v?i{0,3})$",
                         word),
                  !grepl("\\b[a-z]{1}\\b", word),
                  !grepl("\\bwww*.", word)) %>%
    dplyr::mutate(word = quanteda::tokens(word),
                  word = quanteda::tokens_wordstem(word),
                  word = as.character(word))

  count_words <- clean_words %>%
    summarise(year = unique(year),
              n = n())

  return(count_words)

}

# td <- lapply(2012:2013, function(x) total_words(year = x, title_number = 50, chapter = 6, part = 648))
tt <- purrr::map_df(1996:2017, ~ cfr_text(year = .x,
               title_number = 50,
               chapter = 6,
               part = 648,
               return_tidytext = TRUE,
               verbose = TRUE))

td <- purrr::map_df(1996:2017, ~ total_words(year = .x,
                                             title_number = 50,
                                             chapter = 6,
                                             part = 648))


library(ggplot2)
ggplot(td, aes(x = year, y = n)) +
  geom_line() +
  theme_minimal()

# td will be a data.frame of year and total number of words



## #3 look at the bind_tf_idf
## work through: https://www.tidytextmining.com/tfidf.html#the-bind_tf_idf-function

year_words <- purrr::map_df(2016:2017, ~ cfr_text(year = .x,
                                              title_number = 50,
                                              chapter = 6,
                                              part = 648,
                                              return_tidytext = TRUE,
                                              verbose = FALSE))
# year_words <- bind_rows(cfr_text(year = 2015,
#                                  title_number = 50,
#                                  chapter = 6,
#                                  part = 648,
#                                  return_tidytext = TRUE,
#                                  verbose = FALSE),
#                         cfr_text(year = 2016,
#                                  title_number = 50,
#                                  chapter = 6,
#                                  part = 648,
#                                  return_tidytext = TRUE,
#                                  verbose = FALSE))

book_words <- year_words %>%
  tidyr::unnest() %>%
  count(year, word, sort = TRUE) %>%
  ungroup()


book_words <- book_words %>%
  bind_tf_idf(word, book, n)
book_words
