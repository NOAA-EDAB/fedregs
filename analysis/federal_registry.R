# devtools::install_github("slarge/fedregs")
# install.packages("federalregister")
library(dplyr)
#library(federalregister)
library(fedregs)
library(httr)


r <- do.call("GET", list("www.federalregister.gov/api/v1/documents.json?fields%5B%5D=action&fields%5B%5D=citation&fields%5B%5D=publication_date&fields%5B%5D=page_length&fields%5B%5D=pdf_url&fields%5B%5D=title&fields%5B%5D=type&per_page=2000&order=newest&conditions%5Bcfr%5D%5Btitle%5D=50&conditions%5Bcfr%5D%5Bpart%5D=648"))


response <- httr::content(r, "text")
out <- jsonlite::fromJSON(response, flatten = TRUE)
tt <- data.frame(out, stringsAsFactors = FALSE)

td <- cfr_text(year = 2017, title_number = 50, chapter = 6, part = 648)

subparts <-td$subpart

str(dates)
tl <- tt %>%
  mutate(results.title = tolower(results.title)) %>%
  filter(grepl("fisheries of the northeastern united states", results.title)) %>%
  mutate(subpart = case_when(grepl("general provisions", results.title)~ "Subpart A",
                             grepl("atlantic mackerel|squid|butterfish", results.title)~ "Subpart B",
                             grepl("salmon", results.title)~ "Subpart C",
                             grepl("sea scallop", results.title)~ "Subpart D",
                             grepl("surf clam|ocean quahog", results.title)~ "Subpart E",
                             grepl("multispecies|monkfish|groundfish", results.title)~ "Subpart F",
                             grepl("summer flounder", results.title)~ "Subpart G",
                             grepl("scup", results.title)~ "Subpart H",
                             grepl("black sea bass", results.title)~ "Subpart I",
                             grepl("bluefish", results.title)~ "Subpart J",
                             grepl("herring", results.title)~ "Subpart K",
                             grepl("spiny dogfish", results.title)~ "Subpart L",
                             grepl("deep-sea|red crab", results.title)~ "Subpart M",
                             grepl("tilefish", results.title)~ "Subpart N",
                             grepl("skate|complex", results.title)~ "Subpart O",
                             #grepl("")
                             TRUE ~ results.title),
         month_date = format(as.Date(results.publication_date), "%Y")) %>%
  filter(!grepl("Subpart [A-Z]{1}", subpart))


dates <- data_frame(subpart = paste0("Subpart ", LETTERS[1:15])) %>%
  tidyr::crossing(month_date = format(seq(ymd('1997-01-01'), ymd('2018-12-31'), by = 'years'), "%Y"))

tm <- tl %>%
  right_join(dates) %>%
  mutate(subpart = subpart,
         month_date = as.factor(month_date)) %>%
  group_by(subpart, month_date) %>%
  summarize(sums = sum(results.page_length, na.rm = FALSE)) %>%
  ungroup() %>%
  # arrange(subpart) %>%
  mutate(subpart = factor(subpart, levels = rev(paste0("Subpart ", LETTERS[1:15])),
                          ordered = TRUE))
subparts

gg <- ggplot(tm, aes(x = month_date, y = subpart)) +
 geom_tile(aes(fill = sums), color="white", size=0.1) +
 scale_fill_viridis_c(name = "# Pages/Year", na.value="grey90") +
 geom_text(aes(label = round(sums, 1)), color = "grey50") +
 coord_equal() +
 labs(x = NULL, y = NULL,
      title = "Federal Register actions affecting CFR:",
      subtitle = "Title 50, Chapter VI, Subpart 648",
      caption = sprintf("Data accessed on %s from:\n https://www.federalregister.gov",
                       format(Sys.Date(), "%d %B %Y"))) +
#  theme_tufte(base_family="Helvetica")
 theme(plot.title=element_text(hjust=0)) +
 theme(axis.ticks=element_blank()) +
 theme(axis.text=element_text(size=7)) +
 theme(legend.title=element_text(size=8)) +
 theme(legend.text=element_text(size=6))
gg

ggsave(plot = gg, filename = "analysis/federal_register.pdf", dpi = "retina",  width = 30, height = 20, units = "cm")
