context("test-fedregs.R")

test_that("We get the best CFR URLs.", {

  good_year <- 2000
  good_title_number <- 15

  bad_year <- 1993
  bad_title_number <- 55

  na_year <- 1996
  na_title_number <- 1

    expect_error(cfr_urls(year = bad_year,
                          title_number = good_title_number,
                            check_url = TRUE), "Year must be between 1996 and 2017.\n")

    expect_error(cfr_urls(year = good_year,
                          title_number = bad_title_number,
                          check_url = TRUE), "Title must be a numeric value between 1 and 50.\n")

    expect_message(cfr_urls(year = na_year,
                            title_number = na_title_number,
                            verbose = TRUE,
                            check_url = FALSE), sprintf("There aren't any regulations for title %s in %s.",
                                                        na_title_number,
                                                        na_year))

    expect_true(is.na(cfr_urls(year = na_year,
                         title_number = na_title_number,
                         verbose = FALSE,
                         check_url = FALSE)))

    cfr <- cfr_urls(year = good_year,
                    title_number = good_title_number,
                    check_url = FALSE)

    expect_true(all(grepl("*..xml$", cfr)))

    expect_that(cfr, is_a("character"))
})

test_that("We can parse some parts.", {

  good_year <- 2000
  good_title_number <- 15

  na_url <- cfr_urls(year = na_year, title_number = na_title_number)
  good_url <- cfr_urls(year = good_year, title_number = good_title_number)
  bad_url <- sprintf("https://www.gpo.gov/fdsys/bulkdata/CFR/%s/title-%s/CFR-%s-title%s-vol1.xml",
                     bad_year,
                     bad_title_number,
                     bad_year,
                     bad_title_number)
  url <- bad_url
  cfr_part("https://www.gpo.gov/fdsys/bulkdata/CFR/1993/title-55/CFR-1993-title55-vol1.xml")


  bad_year <- 1993
  bad_title_number <- 55

  na_year <- 1996
  na_title_number <- 1

  expect_error(cfr_urls(year = bad_year,
                        title_number = good_title_number,
                        check_url = TRUE), "Year must be between 1996 and 2017.\n")

  expect_error(cfr_urls(year = good_year,
                        title_number = bad_title_number,
                        check_url = TRUE), "Title must be a numeric value between 1 and 50.\n")

  expect_message(cfr_urls(year = na_year,
                          title_number = na_title_number,
                          verbose = TRUE,
                          check_url = FALSE), sprintf("There aren't any regulations for title %s in %s.",
                                                      na_title_number,
                                                      na_year))

  expect_true(is.na(cfr_urls(year = na_year,
                             title_number = na_title_number,
                             verbose = FALSE,
                             check_url = FALSE)))

  cfr <- cfr_urls(year = good_year,
                  title_number = good_title_number,
                  check_url = FALSE)

  expect_true(all(grepl("*..xml$", cfr)))

  expect_that(cfr, is_a("character"))
})

