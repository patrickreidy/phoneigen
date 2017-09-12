
suppressMessages(library(magrittr))



testthat::context(":: SibilantFricatives internal data set")

testthat::test_that("Variable names are correct", {
  testthat::expect_identical(
    object = names(phoneigen::SibilantFricatives()),
    expected = c("File", "Participant", "Session", "SessionCompleted",
                 "TaggingCompleted", "Trial", "Orthography", "TargetC",
                 "TargetV", "TaggedOnset", "TaggedVOT", "Onset", "VOT",
                 "ExcitationPattern")
  )
})

testthat::test_that("990 observations are present", {
  testthat::expect_equal(
    object = nrow(phoneigen::SibilantFricatives()),
    expected = 990
  )
})

testthat::test_that("$File values are correctly formatted", {
  phoneigen::SibilantFricatives() %>%
    dplyr::mutate(File2 = purrr::pmap_chr(
      list(Participant, Session, Trial, Orthography),
      function(.p, .s, .t, .o) {
        stringr::str_interp("${.p}_${.s}_${.t}_${.o}.WAV")
      }
    )) %>%
    dplyr::mutate(Test = purrr::walk2(File, File2, testthat::expect_identical))
})

testthat::test_that("$Participant values are correctly formatted", {
  testthat::expect_match(
    object = phoneigen::SibilantFricatives()$Participant,
    regexp = stringr::str_c("A", 50:70, "N") %>% stringr::str_c(collapse = "|")
  )
})

testthat::test_that("$Participant values are prefix of $Session values", {
  testthat::expect_identical(
    object = phoneigen::SibilantFricatives()$Participant,
    expected = substring(phoneigen::SibilantFricatives()$Session, 1, 4)
  )
})

testthat::test_that("$Session values are correctly formatted", {
  testthat::expect_match(
    object = phoneigen::SibilantFricatives()$Session,
    regexp = "A[0-9]{2}N[0-9]{2}[MF][AS][23]"
  )
})

testthat::test_that("$Trial values are all Test trials", {
  testthat::expect_match(
    object = phoneigen::SibilantFricatives()$Trial,
    regexp = "Test"
  )
})

testthat::test_that("$TargetC values are sibilant fricatives", {
  testthat::expect_match(
    object = phoneigen::SibilantFricatives()$TargetC,
    regexp = "^s|S$"
  )
})

testthat::test_that("$TargetV values are vowel transcriptions", {
  testthat::expect_match(
    object = phoneigen::SibilantFricatives()$TargetV,
    regexp = "^@|^|A|aI|aU|E|ei|I|i:|oU|u|U$"
  )
})

testthat::test_that("$TaggedOnset precedes $TaggedVOT", {
  phoneigen::SibilantFricatives() %>%
    dplyr::pull(.data$TaggedOnset) %>%
    is.finite() %>%
    purrr::walk(testthat::expect_true)
  phoneigen::SibilantFricatives() %>%
    dplyr::pull(.data$TaggedVOT) %>%
    is.finite() %>%
    purrr::walk(testthat::expect_true)
  phoneigen::SibilantFricatives() %>%
    dplyr::mutate(Object = .data$TaggedOnset < .data$TaggedVOT) %>%
    dplyr::pull(.data$Object) %>%
    purrr::walk(testthat::expect_true)
})

testthat::test_that("$Onset precedes $VOT", {
  phoneigen::SibilantFricatives() %>%
    dplyr::pull(.data$Onset) %>%
    (function(.o) {.o == 0.020}) %>%
    purrr::walk(testthat::expect_true)
  phoneigen::SibilantFricatives() %>%
    dplyr::pull(.data$VOT) %>%
    is.finite() %>%
    purrr::walk(testthat::expect_true)
  phoneigen::SibilantFricatives() %>%
    dplyr::mutate(Object = .data$Onset < .data$VOT) %>%
    dplyr::pull(.data$Object) %>%
    purrr::walk(testthat::expect_true)
})

testthat::test_that("The same duration is spanned by $VOT-$Onset as by $TaggedVOT-$TaggedOnset", {
  phoneigen::SibilantFricatives() %>%
    dplyr::mutate(Diff = .data$VOT - .data$Onset,
                  TaggedDiff = .data$TaggedVOT - .data$TaggedOnset) %>%
    dplyr::mutate(Object = abs(.data$Diff - .data$TaggedDiff) < .Machine$double.eps) %>%
    dplyr::pull(.data$Object) %>%
    purrr::walk(testthat::expect_true)
})










testthat::context(":: StopBursts internal data set")

testthat::test_that("Variable names are correct", {
  testthat::expect_identical(
    object = names(phoneigen::StopBursts()),
    expected = c("File", "Participant", "Session", "SessionCompleted",
                 "TaggingCompleted", "Trial", "Orthography", "TargetC",
                 "TargetV", "TaggedBurst", "TaggedVOT", "Burst", "VOT",
                 "ExcitationPattern")
  )
})

testthat::test_that("1179 observations are present", {
  testthat::expect_equal(
    object = nrow(phoneigen::StopBursts()),
    expected = 1020
  )
})

testthat::test_that("$File values are correctly formatted", {
  phoneigen::StopBursts() %>%
    dplyr::mutate(File2 = purrr::pmap_chr(
      list(Participant, Session, Trial, Orthography),
      function(.p, .s, .t, .o) {
        stringr::str_interp("${.p}_${.s}_${.t}_${.o}.WAV")
      }
    )) %>%
    dplyr::mutate(Test = purrr::walk2(File, File2, testthat::expect_identical))
})

testthat::test_that("$Participant values are correctly formatted", {
  testthat::expect_match(
    object = phoneigen::StopBursts()$Participant,
    regexp = stringr::str_c("A", 50:70, "N") %>% stringr::str_c(collapse = "|")
  )
})

testthat::test_that("$Participant values are prefix of $Session values", {
  testthat::expect_identical(
    object = phoneigen::StopBursts()$Participant,
    expected = substring(phoneigen::StopBursts()$Session, 1, 4)
  )
})

testthat::test_that("$Session values are correctly formatted", {
  testthat::expect_match(
    object = phoneigen::StopBursts()$Session,
    regexp = "A[0-9]{2}N[0-9]{2}[MF][AS][23]"
  )
})

testthat::test_that("$Trial values are all Test trials", {
  testthat::expect_match(
    object = phoneigen::StopBursts()$Trial,
    regexp = "Test"
  )
})

testthat::test_that("$TargetC values are sibilant fricatives", {
  testthat::expect_match(
    object = phoneigen::StopBursts()$TargetC,
    regexp = "^th|kh$"
  )
})

testthat::test_that("$TargetV values are vowel transcriptions", {
  testthat::expect_match(
    object = phoneigen::StopBursts()$TargetV,
    regexp = "^@|^|A|aI|aU|E|eI|I|i:|oU|u|U$"
  )
})

testthat::test_that("$TaggedBurst precedes $TaggedVOT", {
  phoneigen::StopBursts() %>%
    dplyr::pull(.data$TaggedBurst) %>%
    is.finite() %>%
    purrr::walk(testthat::expect_true)
  phoneigen::StopBursts() %>%
    dplyr::pull(.data$TaggedVOT) %>%
    is.finite() %>%
    purrr::walk(testthat::expect_true)
  phoneigen::StopBursts() %>%
    dplyr::mutate(Object = .data$TaggedBurst < .data$TaggedVOT) %>%
    dplyr::pull(.data$Object) %>%
    purrr::walk(testthat::expect_true)
})

testthat::test_that("$Burst precedes $VOT", {
  phoneigen::StopBursts() %>%
    dplyr::pull(.data$Burst) %>%
    (function(.o) {.o == 0.020}) %>%
    purrr::walk(testthat::expect_true)
  phoneigen::StopBursts() %>%
    dplyr::pull(.data$VOT) %>%
    is.finite() %>%
    purrr::walk(testthat::expect_true)
  phoneigen::StopBursts() %>%
    dplyr::mutate(Object = .data$Burst < .data$VOT) %>%
    dplyr::pull(.data$Object) %>%
    purrr::walk(testthat::expect_true)
})

testthat::test_that("The same duration is spanned by $VOT-$Burst as by $TaggedVOT-$TaggedBurst", {
  phoneigen::StopBursts() %>%
    dplyr::mutate(Diff = .data$VOT - .data$Burst,
                  TaggedDiff = .data$TaggedVOT - .data$TaggedBurst) %>%
    dplyr::mutate(Object = abs(.data$Diff - .data$TaggedDiff) < .Machine$double.eps) %>%
    dplyr::pull(.data$Object) %>%
    purrr::walk(testthat::expect_true)
})
