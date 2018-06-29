
suppressMessages(library(magrittr))



testthat::context(":: SibilantFricatives internal data set")

testthat::test_that("Variable names are correct", {
  testthat::expect_identical(
    object = names(phoneigen::SibilantFricatives()),
    expected = c("SessionDate", "Session", "Adult", "Participant",
                 "Age", "Female", "MAE", "StimulusSet", "Trial",
                 "Orthography", "Target", "Transcription", "Rating",
                 "ExcitationPattern")
  )
})

testthat::test_that("Each Participant code is the prefix of the corresponding Session code", {
  testthat::expect_identical(
    object = phoneigen::SibilantFricatives()$Participant,
    expected = substring(phoneigen::SibilantFricatives()$Session, 1, 4)
  )
})

testthat::test_that("Children younger than 33 months of age received StimulusSet 1", {
  .object <- dplyr::filter(phoneigen::SibilantFricatives(), Age < 33)$StimulusSet
  testthat::expect_equal(object = .object, expected = rep(1, length(.object)))
})

testthat::test_that("Children older than 33 months of age received StimulusSet 2", {
  .object <- dplyr::filter(phoneigen::SibilantFricatives(), Age > 33)$StimulusSet
  testthat::expect_equal(object = .object, expected = rep(2, length(.object)))
})

testthat::test_that("Target consonants are all sibilants", {
  testthat::expect_match(
    object = phoneigen::SibilantFricatives()$Target,
    regexp = "^s|S$"
  )
})

testthat::test_that("Transcriptions are well-formed", {
  testthat::expect_match(
    object = phoneigen::SibilantFricatives()$Transcription,
    regexp = "^s|s:S|S:s|S$"
  )
})

testthat::test_that("Productions by adults have no Rating", {
  .rated_adults <-
    dplyr::filter(phoneigen::SibilantFricatives(), Adult & !is.na(Rating))
  testthat::expect_equal(object = nrow(.rated_adults), expected = 0)
})

testthat::test_that("All productions by children have a Rating", {
  .unrated_children <-
    dplyr::filter(phoneigen::SibilantFricatives(), !Adult & is.na(Rating))
  testthat::expect_equal(object = nrow(.unrated_children), expected = 0)
})

testthat::test_that("Each excitation pattern is a 361-dimensional real-valued non-negative vector", {
  .is_361_dim <- function(x) {length(x) == 361}
  .is_psd <- function(x) {purrr::reduce(x >= 0, `&`)}
  .eps <- phoneigen::SibilantFricatives()$ExcitationPattern
  .well_formed <-
    purrr::map_lgl(.eps, .is_361_dim) &
    purrr::map_lgl(.eps, is.numeric) &
    purrr::map_lgl(.eps, .is_psd)
  purrr::walk(.well_formed, testthat::expect_true)
})

