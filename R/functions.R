#' cfr_urls
#'
#' @param year
#' @param title
#' @param check_url
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun{cfr_url_list <- expand.grid(years = 2017:2018,
#' title = 50,
#' KEEP.OUT.ATTRS = FALSE,
#' stringsAsFactors = FALSE) %>%
#' mutate(url = purrr::map2(years, title, cfr_urls, check_url = TRUE))
#' head(cfr_url_list)}
#'
#'
cfr_urls <- function(year, title, check_url = TRUE, verbose = FALSE) {

  url_head <- "https://www.gpo.gov/fdsys/"
  url <- sprintf("%s/bulkdata/CFR/%s/title-%s", url_head, year, title)


  url_list <- purrr::possibly( ~.x %>% xml2::read_html() %>%    # Try to take a URL, read it,
                                 rvest::html_nodes('a') %>%
                                 rvest::html_attr("href"),
                               NA)(url) %>%
    grep(pattern =  sprintf("title%s.*?.xml", title),  value = TRUE)

  url_df <- data.frame(URL = sprintf("%s%s", url_head, url_list),
                       stringsAsFactors = FALSE)

  Sys.sleep(sample(seq(1, 3, by = 0.001), 1))

  if(nrow(url_df) != 0) {
    if(check_url == TRUE){
      url_df$STATUS <- sapply(url_df$URL, httr::http_error, httr::config(followlocation = 0L), USE.NAMES = FALSE)
      if(any(url_df$STATUS == TRUE) &
         verbose){
        message("The following urls result in an http error:", url_df$URL[url_df$STATUS == TRUE])
      }
      if(all(url_df$STATUS == FALSE) &
         verbose) {
        message(sprintf("All urls for title %s in %s should be fine.\n", title, year))
      }
    }
    return(as.character(url_df$URL))
  }
  if(nrow(url_df) == 0 &
     verbose){
    message(sprintf("There aren't any regulations for title %s in %s.\n", title, year))
    return(NA)
  }
}


#' cfr_part
#'
#' @param url
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun{data(cfr_url_list)
#'
#' cfr_part_list <- cfr_url_list %>%
#' dplyr::filter(!is.na(url)) %>%
#' tidyr::unnest() %>%
#' dplyr::mutate(all_cfr = purrr::map(url, cfr_part))}
#'
#'
cfr_part <- function(url, verbose = FALSE){

  res <- httr::GET(url)
  parts <- httr::content(res, as = "parsed", encoding = "UTF-8") %>%
    xml2::xml_find_all(sprintf("FMTR/TITLEPG/PARTS")) %>%
    xml2::xml_text()

  chapters <- httr::content(res, as = "parsed", encoding = "UTF-8") %>%
    xml2::xml_find_all(sprintf("FMTR/TOC/TITLENO/CHAPTI/SUBJECT")) %>%
    xml2::xml_text()

  if(length(chapters) == 0){
    chapters <- httr::content(res, as = "parsed", encoding = "UTF-8") %>%
      xml2::xml_find_all(sprintf("TOC/TITLENO/CHAPTI/SUBJECT")) %>%
      xml2::xml_text()
  }

  Sys.sleep(sample(seq(1, 3, by = 0.001), 1))

  if(verbose) {
    message(sprintf("Pulling the chapter, part, and volume information from:\n%s.\n", url))
  }

  return(data.frame(year = gsub(".*CFR-(.*)-title(.*)-vol(.*).xml", "\\1", url),
                    title = gsub(".*CFR-(.*)-title(.*)-vol(.*).xml", "\\2", url),
                    vol = gsub(".*CFR-(.*)-title(.*)-vol(.*).xml", "\\3", url),
                    chapters = chapters,
                    parts = parts,
                    url = url,
                    stringsAsFactors = FALSE))
}



#' numextract
#'
#' @param string
#' @param return
#'
#' @return
#'
#' @keywords internal
#' @examples
#'
#' \dontrun{data(all_cfr)
#'
#' NE_cfr <- all_cfr %>%
#' dplyr::mutate(min_parts = purrr::map(parts, numextract, "min"),
#' max_parts = purrr::map(parts, numextract, "max")) %>%
#' dplyr::filter(grepl("Chapter VI", chapters),
#' min_parts <= 648,
#' max_parts > 648)}
#'
numextract <- function(string, return = c("min", "max")){
  ## need to find out what the max for each title might be
  num_vec <- unlist(regmatches(string,
                               gregexpr("[[:digit:]]+\\.*[[:digit:]]*", string)))
  if(length(num_vec) == 1){
    num_vec <- c(num_vec, Inf)
  }
  if(return == "min"){
    return(num_vec[1])
  }
  if(return == "max"){
    return(max(num_vec))
  }
}

#' section_function
#'
#' @param data
#' @param subpart_number
#'
#' @return
#' @export
#'
#' @keywords internal
#'
#' @examples
section_function <- function(data, subpart_number){
  xml2::xml_find_all(data,
                     sprintf(".//SECTNO[text()='%s']/following-sibling::P",
                             subpart_number)) %>%
    xml2::xml_text(.)
}




#' cfr_text
#'
#' @param year
#' @param title_number
#' @param chapter
#' @param part
#' @param subpart
#' @param return_tidytext
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples


cfr_text <- function(year, title_number, chapter, part, subpart = NULL, return_tidytext = TRUE,
                     verbose = FALSE) {

  if(!year %in% seq(1996, 2017)){
    stop("Year must be between 1996 and 2017.\n")
  }

  if(!title_number %in% seq(1, 50)){
    stop("Title must be a numeric value between 1 and 50.\n")
  }

  if(is.numeric(chapter)){
    chapter <- as.character(as.roman(chapter))
  }

  cfr_url_list <- cfr_urls(year = year, title = title_number, check_url = TRUE, verbose = verbose)

  if(all(is.na(cfr_url_list) == TRUE)){
    stop(sprintf("There aren't any regulations for title %s in %s.\n", title_number, year))
  }

  cfr_part_df<- dplyr::data_frame(all_cfr =
                                    purrr::map(cfr_url_list,
                                               purrr::possibly(~ cfr_part(., verbose),
                                                               otherwise = NA,
                                                               quiet = FALSE)))

  cfr_part <- cfr_part_df %>%
    tidyr::unnest() %>%
    dplyr::mutate(min_parts = purrr::map(parts, numextract, "min"),
                  max_parts = purrr::map(parts, numextract, "max")) %>%
    dplyr::filter(grepl(sprintf("Chapter %s", chapter), chapters),
                  min_parts <= part,
                  max_parts > part)

  cfr_subpart <- cfr_part %>%
    dplyr::select(url) %>%
    dplyr::pull() %>%
    httr::GET() %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    xml2::read_xml() %>%
    xml2::xml_find_all(sprintf(".//PART/HD[contains(text(), '%s')]/following-sibling::SUBPART", part))

  subpart_names <- cfr_subpart %>%
    purrr::map(~ xml2::xml_find_all(., sprintf(".//HD[contains(text(), '%s')]", "Subpart"))) %>%
    purrr::map(~ xml2::xml_text(.)) %>%
    unlist()

  reserved_names <- cfr_subpart %>%
    purrr::map(~ xml2::xml_find_all(., sprintf(".//RESERVED[contains(text(), '%s')]", "Subpart"))) %>%
    purrr::map(~ xml2::xml_text(.)) %>%
    unlist()

  subpart_names <- sort(c(subpart_names, reserved_names))

  section_numbers <- cfr_subpart %>%
    purrr::map(~ xml2::xml_find_all(., ".//SECTNO")) %>%
    purrr::map(~ xml2::xml_text(.))

  names(section_numbers) <- subpart_names
  section_data <- dplyr::data_frame(SUBPART_NAME = rep(names(section_numbers),
                                                       sapply(section_numbers, length)),
                                    SECTION_NUMBER = unlist(section_numbers))


  section_text <- section_data %>%
    dplyr::mutate(year = year,
                  title_number = title_number,
                  chapter = chapter,
                  part = part,
                  TEXT = purrr::map(.x = SECTION_NUMBER,
                                    ~ section_function(data = cfr_subpart,
                                                       subpart_number = .x)),
                  TEXT = stringi::stri_trim(TEXT),
                  TEXT = stringi::stri_trans_tolower(TEXT)) %>%
    dplyr::rename(subpart = SUBPART_NAME)

  if(return_tidytext){
    return(section_text %>%
             tidytext::unnest_tokens(word, TEXT) %>%
             dplyr::group_by(year, title_number, chapter, part, subpart) %>%
             tidyr::nest())
  }

  if(!return_tidytext){
    return(section_text %>%
             dplyr::group_by(year, title_number, chapter, part, subpart) %>%
             tidyr::nest())
  }
}
