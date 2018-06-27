## devtools::install_github("hrbrmstr/xmlview")
# library(xmlview) ## highly optional for this example!

# cfr_url_list <- readRDS("data/cfr_url_list.RDS")
# devtools::use_data(cfr_url_list, overwrite = TRUE)

tt <- cfr_text(year = 2010,
               title_number = 50,
               chapter = 6,
               part = 648,
               return_tidytext = TRUE,
               verbose = TRUE)

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
  group_by(SUBPART_NAME, word) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(-n)

  # dplyr::count(SUBPART_NAME, word, sort = TRUE)

td <- bind_rows(count_words %>%
                  group_by(word) %>%
                  summarise(SUBPART_NAME = "TOTAL",
                            n = sum(n)) %>%
                  ungroup() %>%
                  arrange(-n) %>%
                  top_n(n = 20, wt = n),
                count_words %>%
                  group_by(SUBPART_NAME) %>%
                  arrange(-n) %>%
                  top_n(n = 20, wt = n))

ggplot(td, aes(word, n, fill = SUBPART_NAME)) +
  geom_col() +
  xlab(NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.direction = "horizontal", legend.position = "bottom") +
  coord_flip() +
  facet_wrap(.~SUBPART_NAME, scales = "free_y")

year = 2015
title_number = 50

chapter = 6
part = 648
subpart = NULL
verbose = TRUE
#%>%
# purrr::map(~ gsub("^\u00A7*\u2009", "", .))


td <- all_parts %>%
  dplyr::filter(grepl("Subpart A", SUBPART_NAME)) %>%
  tidyr::unnest(data)

tp <-  tidytext::unnest_tokens(tbl = td, output = word, input = TEXT, token = "words")


data(stop_words, package = "tidytext")

tidy_books <- tp %>%
  anti_join(stop_words)

tidy_books %>%
  count(word, sort = TRUE)

td <- TEXT[[1]]
tp <- td[1]
toks <- readRDS("subpart_a.rds")
# text <- "An example of preprocessing techniques"
# toks <- quanteda::tokens(tp)  # tokenize into unigrams
# toks
toks <- quanteda::tokens_tolower(toks)
toks <- quanteda::tokens_wordstem(toks)
sw <- quanteda::stopwords("english")
toks <- tokens_remove(toks, sw)
# saveRDS(toks, "subpart_a.rds")
str(td)


toks <- tokens(text)  # tokenize into unigrams
toks

names(SUBJECT) <- SECTNO

SECTNO = bind_cols(SECTNO = subpart %>%
                  purrr::map(~ xml2::xml_find_all(., ".//SECTNO")) %>%
                  purrr::map(~ xml2::xml_text(.)) %>%
                  purrr::map(~ gsub("^\u00A7*\u2009", "", .)) %>%
                  unlist,
                SUBJECT = subpart %>%
                  purrr::map(~ xml2::xml_find_all(., ".//SUBJECT")) %>%
                  purrr::map(~ xml2::xml_text(.)) %>%
                  unlist)

TEXT = subpart %>%
  purrr::map(~ xml2::xml_find_all(., ".//P")) %>%
  purrr::map(~ xml2::xml_text(.))

str(all_parts)


section <- xml2::read_xml(httr::content(ne_url, as = "text", encoding = "UTF-8")) %>%
  xml2::xml_find_all(sprintf(".//PART/HD[contains(text(), '%s')]/following-sibling::SUBPART/SECTION", 648)) %>%
  xml2::xml_find_all(".//SUBPART")





section <- tt %>%
  # subpart %>%
  mutate(SECTNO = nodeset %>%
           purrr::map(~ xml2::xml_find_all(., ".//SECTNO")) %>%
           purrr::map(~ xml2::xml_text(.)),
         SUBJECT = nodeset %>%
           purrr::map(~ xml2::xml_find_all(., ".//SUBJECT")) %>%
           purrr::map(~ xml2::xml_text(.)),
         TEXT = nodeset %>%
           purrr::map(~ xml2::xml_find_all(., ".//P")) %>%
           purrr::map(~ xml2::xml_text(.)))



tt <- data_frame(row = seq_along(subpart),
                 nodeset = subpart)


cells_df <- tt %>%
    mutate(col_name_raw = nodeset %>% purrr::map(~ xml2::xml_name(.)),
           cell_text = nodeset %>% purrr::map(~ xml2::xml_text(.)),
           i = nodeset %>% purrr::map(~ seq_along(.))) %>%
    select(row, i, col_name_raw, cell_text) %>%
    tidyr::unnest(cell_text)

str(cells_df)

part_text[1]

xml_view(part_text)


tt <- readLines(con = part_text)


library(stringi)
x <- stri_trim(tt)                               # strip surrounding whitespace
x <- stri_trans_tolower(x)

library(quanteda)
text <- "An example of preprocessing techniques"
toks <- tokens(text)
