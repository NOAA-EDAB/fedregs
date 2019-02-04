#' cfr_urls
#'
#' @title URLs for .xml Code of Federal Regulations.
#'
#' @description \code{cfr_urls} returns a character string of valid URLs associated with a year and CFR title.
#'
#' @details The Code of Federal Regulations (CFR) is divided into titles, chapters, parts, subparts, and sections. Each title within the CFR is divided into volumes. Unfortunately, each chapter isn't consistently in the same volume so \code{cfr_urls} function scrapes up all the valid URLs for a given title/year combination.
#'
#' @param year numeric (YYYY) between 1996 and 2018.
#' @param title_number numeric between 1 and 50.
#' @param check_url logical. Should the URLs be tested using \code{httr::http:error()}.
#' @param verbose logical. Will return "helpful" messages regarding the status of the URL.
#'
#' @return Valid URLs are returned as a vector of character strings. Invalid URLs are returned as `NA`'s.
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' \donttest{library(dplyr)
#' url_list <- expand.grid(years = 2015:2018,
#'   title = 50,
#'   KEEP.OUT.ATTRS = FALSE,
#'   stringsAsFactors = FALSE) %>%
#'   mutate(url = purrr::map2(years, title, cfr_urls, check_url = TRUE))
#'   head(url_list)}
#'
#'
cfr_urls <- function(year, title_number, check_url = TRUE, verbose = FALSE) {

  if(!year %in% seq(1996, 2018)){
    stop("Year must be between 1996 and 2018.\n")
  }

  if(!title_number %in% seq(1, 50)){
    stop("Title must be a numeric value between 1 and 50.\n")
  }

  # url_head <- "https://www.gpo.gov/fdsys/"
  # url <- sprintf("%s/bulkdata/CFR/%s/title-%s", url_head, year, title_number)
  # url_list <- purrr::possibly( ~.x %>% xml2::read_html() %>%    # Try to take a URL, read it,
  #                                rvest::html_nodes('a') %>%
  #                                rvest::html_attr("href"),
  #                              NA)(url) %>%
  #   grep(pattern =  sprintf("title%s.*?.xml", title_number),  value = TRUE)


  url_head <- "https://www.govinfo.gov/bulkdata/CFR"
  url <- sprintf("%s/%s/title-%s", url_head, year, title_number)

  url_list <- purrr::possibly(~.x %>% httr::GET() %>%
                                httr::content(as = "text", encoding = "utf-8") %>%
                                xml2::read_xml() %>%
                                xml2::xml_find_all(".//displayLabel") %>%
                                xml2::xml_text(),
                              NA)(url)

  ## remove the zip files
  url_list <- url_list[!grepl("*.zip", url_list)]
  ## remove NAs
  url_list <- url_list[!is.na(url_list)]

  url_df <- data.frame(URL = sprintf("%s/%s/title-%s/%s", url_head, year, title_number, url_list),
                       stringsAsFactors = FALSE)

  Sys.sleep(sample(seq(1, 3, by = 0.001), 1))

  if(nrow(url_df) != 0) {
    if(check_url == TRUE){
      url_df$STATUS <- sapply(url_df$URL, httr::http_error, httr::config(followlocation = 0L), USE.NAMES = FALSE)
      if(any(url_df$STATUS == TRUE)){
        if(verbose){
          message("The following urls result in an http error:", url_df$URL[url_df$STATUS == TRUE])
        }
      }
      if(all(url_df$STATUS == FALSE)) {
        if(verbose){
          message(sprintf("All urls for title %s in %s should be fine.\n", title_number, year))
        }
      }
    }
    return(as.character(url_df$URL))
  }
  if(nrow(url_df) == 0){
    if(verbose){
      message(sprintf("There aren't any regulations for title %s in %s.\n", title_number, year))
    }
    return(NA)
  }
}


#' cfr_part
#'
#' @title Parse the Relevant Details for CFR urls.
#' @description \code{cfr_part} returns a data_frame year, title, volume, chapters, parts, and URL for each url
#' @details Since we're after more refined data than a single volume, we need to figure out what chapters and parts are associated with each volume. This function parses the xml and scrapes the Table of Contents for the information held in each volume.
#'
#' @param url A valid url for .xml CFR volumes. Ideally, from \code{cfr_urls}.
#' @param verbose logical. Will return "helpful" messages regarding the status of the URL.
#'
#' @return Numeric (year, title, volume, and chapters) and characters (parts and URL).
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' \donttest{part_vec <- cfr_urls(year = 2018, title_number = 50)
#' cfr_part(part_vec[1])}
#'
#'
cfr_part <- function(url, verbose = FALSE){

  ## add a better test ##
  if(is.na(url)){
    stop("NA is not a valid url.")
  }

  if(httr::http_error(httr::GET(url))){
    stop("The URL is not valid.")
  }

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

  return(data.frame(year = as.numeric(gsub(".*CFR-(.*)-title(.*)-vol(.*).xml", "\\1", url)),
                    title = as.numeric(gsub(".*CFR-(.*)-title(.*)-vol(.*).xml", "\\2", url)),
                    vol = as.numeric(gsub(".*CFR-(.*)-title(.*)-vol(.*).xml", "\\3", url)),
                    chapters = chapters,
                    parts = parts,
                    url = url,
                    stringsAsFactors = FALSE))
}



#' numextract
#'
#' @title Extract the Part Numbers
#' @description \code{numextract} takes the part numbers from \code{cfr_part} output.
#'
#' @details Each CFR chapter has multiple parts that often span volumes. To facilitate targeting a specific part, it's necessary to evaluate which parts are in each volume (e.g., Parts 18 to 199). The CFR sometimes uses terms like "END" or "end" to denote the maximum part in each chapter. \code{numextract} simply returns the max as `Inf` in these situations.
#'
#' @param string ideally \code{cfr_part()}$parts
#' @param return min or max, default is "min"
#'
#' @return numeric value from 1 to `Inf``
#'
#' @export
#'
#' @keywords internal
#' @examples
#' \donttest{part_vec <- cfr_urls(year = 2018, title_number = 50)
#' parts <- cfr_part(part_vec[1])
#' numextract(parts$parts, return = "max")}
#'
numextract <- function(string, return = c("min", "max")[1]){

  if(!grepl("part*", tolower(string))){
    stop("Make sure you are providing a valid 'part'.")
  }

  num_vec <- unlist(regmatches(string,
                               gregexpr("[[:digit:]]+\\.*[[:digit:]]*", string)))

  if(length(num_vec) == 0) {
    stop("Make sure string is a numeric value.")
  }

  if(length(num_vec) == 1){
    num_vec <- c(num_vec, Inf)
  }
  if(return == "min"){
    return(as.numeric(num_vec[1]))
  }
  if(return == "max"){
    return(as.numeric(max(num_vec)))
  }
}

#' cfr_text
#'
#' @title Extract the Text for a Given Year, Title, Chapter, and Part
#' @description \code{cfr_text} returns a tibble of CFR text
#' @details This function is the main function of the \code{fedregs} package. It takes the title, chapter, part, and year and returns a tibble of raw text (\code{return_tidytext = FALSE}) or \href{https://www.tidytextmining.com/tidytext.html}{tidytext} text (\code{return_tidytext = TRUE}). N.b., it has not been extensively tested on titles and chapters other than Title 50 chapter VI and part 648.
#'
#' @param year numeric between 1996 and 2018.
#' @param title_number numeric between 1 and 50.
#' @param chapter numeric or roman numeral.
#' @param part numeric.
#' @param token character. Unit for tokenizing. Currently
#' @param return_tidytext logical. TRUE = tidytext, FALSE = raw data
#' @param verbose logical. Will return "helpful" messages regarding the status of the URL.
#' @param ... Extra arguments passed on to tokenizers, such as n and k for "ngrams" and "skip_ngrams"
#'
#' @return a tibble with year, title_number, chapter, part, and text nested by subpart
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' \donttest{regs <- cfr_text(year = 2018,
#' title_number = 50,
#' chapter = 6,
#' part = 648,
#' return_tidytext = TRUE,
#' token = "words",
#' verbose = TRUE)
#' head(regs)}
#'

cfr_text <- function(year, title_number, chapter, part, token = "words", return_tidytext = TRUE,
                     verbose = FALSE, ...) {

  if(!year %in% seq(1996, 2018)){
    stop("Year must be between 1996 and 2018.\n")
  }

  if(!title_number %in% seq(1, 50)){
    stop("Title must be a numeric value between 1 and 50.\n")
  }

  if(!is.numeric(chapter)){
    stop("Chapter must be a numeric value, not a Roman Numeral.\n")
  }
  if(is.numeric(chapter)){
    chapter <- as.character(utils::as.roman(chapter))
  }

  if(!is.numeric(part)){
    stop("Part must be a numeric value.\n")
  }

  if(token == "ngram" & !exists("n", mode = "integer")) {
    stop("For ngram tokens, please include the 'n' argument.")
  }

  cfr_url_list <- cfr_urls(year = year,
                           title_number = title_number,
                           check_url = TRUE,
                           verbose = verbose)

  if(all(is.na(cfr_url_list) == TRUE)){
    stop(sprintf("There aren't any regulations for title %s in %s.\n", title_number, year))
  }

  cfr_part_df<- dplyr::data_frame(all_cfr =
                                    purrr::map(cfr_url_list,
                                               purrr::possibly(~ cfr_part(., verbose),
                                                               otherwise = NA,
                                                               quiet = FALSE)))

  cfr_select_part <- cfr_part_df %>%
    tidyr::unnest() %>%
    dplyr::mutate(min_parts = purrr::map(parts, numextract, "min"),
                  max_parts = purrr::map(parts, numextract, "max")) %>%
    dplyr::filter(grepl(sprintf("Chapter %s", chapter), chapters),
                  min_parts <= part,
                  max_parts > part)

  cfr_xml <- cfr_select_part %>%
    dplyr::select(url) %>%
    dplyr::pull() %>%
    httr::GET() %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    xml2::read_xml()

  subpart_names <- cfr_xml %>%
    xml2::xml_find_all(sprintf("//PART/HD[contains(text(), '%s')]/following-sibling::CONTENTS/SUBPART/HD",
                               part)) %>%
    xml2::xml_text(.) %>%
    unlist()

  if(length(subpart_names) == 0L){
    stop("Check that ", cfr_select_part$url, " has the proper XML format.")
  }

  section_all <- dplyr::data_frame(subpart_names) %>%
    dplyr::mutate(values = purrr::map(subpart_names, function(x) xml2::xml_find_all(x = cfr_xml,
                                                              sprintf("//PART/HD[contains(text(), '%s')]/following-sibling::CONTENTS/SUBPART/HD[contains(text(), '%s')]/following-sibling::SECTNO",
                                                                      part,
                                                                      x)) %>%
                                 xml2::xml_text(.))) %>%
    tidyr::unnest()

  cfr_subpart <- cfr_xml %>%
    xml2::xml_find_all(sprintf("//PART/HD[contains(text(), '%s')]/following-sibling::SUBPART", part))

  if(length(cfr_subpart) == 0L){
    stop("Check that ", cfr_select_part$url, " has the proper XML format.")
  }

  section_numbers <- cfr_subpart %>%
    purrr::map(~ xml2::xml_find_all(., "//SECTION/SECTNO")) %>%
    purrr::map(~ xml2::xml_text(.)) %>%
    unlist()

  section_names <- cfr_subpart %>%
    purrr::map(~ xml2::xml_find_all(., "//SECTION/SUBJECT|//SECTION/RESERVED")) %>%
    purrr::map(~ xml2::xml_text(.)) %>%
    unlist()

  section_text <- dplyr::data_frame(SECTION_NAME = section_names,
                                    SECTION_NUMBER = section_numbers,
                                    values = gsub("[^[:digit:].]", "", section_numbers)) %>%
    dplyr::filter(grepl(sprintf("%s[\\.]", part), SECTION_NUMBER)) %>%
    dplyr::distinct() %>%
    dplyr::left_join(section_all, by = "values") %>%
    dplyr::mutate(TEXT = purrr::map(SECTION_NUMBER, function(x) xml2::xml_find_all(cfr_subpart,
                                                                            sprintf("//SECTNO[text()='%s']/following-sibling::P",
                                                                                    x)) %>%
                               xml2::xml_text(.)),
           year = year,
           title_number = title_number,
           chapter = chapter,
           part = part,
           # TEXT = stringi::stri_trim(TEXT),
           # TEXT = stringi::stri_trans_tolower(TEXT)) %>%
           TEXT = tolower(TEXT)) %>%
    dplyr::rename(subpart = subpart_names)

  if(return_tidytext){
    out <- section_text %>%
             tidytext::unnest_tokens(word, TEXT, token, ...) %>%
             dplyr::group_by(year, title_number, chapter, part, subpart) %>%
             tidyr::nest()
  }

  if(!return_tidytext){
    out <- section_text %>%
             dplyr::group_by(year, title_number, chapter, part, subpart) %>%
             tidyr::nest()
  }
  return(out)
}
