
# This code depends on a private package: `l2trwr`.
# It will run only on machines that have this package installed.




# Attach `magrittr` to make visible the forward-pipe operator.
library(magrittr)

# Source functions for loading data from `l2trwr`.
source("data-raw/l2t-data.R")



onset_buffer <- 0.02
offset_buffer <- 0.06
wav_source <- file.path("~", "Learning2Talk", "l2trwr",
                        "data-raw", "audio", "adult-norm")



# Extract the sibilant fricative productions.
extract_sibilants <-
  l2tSibilants(onsetBuffer = onset_buffer) %>%
  dplyr::mutate(SE = purrr::pmap(
    list(Session, TaggedOnset, TaggedVOT, File),
    function(.session, .onset, .vot, .file) {
      list.files(path = wav_source, pattern = .session, full.names = TRUE) %>%
        speechr::ReadWAV(channel = "left",
                         from = .onset - onset_buffer,
                         to = .vot + offset_buffer) %>%
        speechr::Export(file = file.path("data-raw", "wav-sibilants", .file))
    }
  ))



# Extract the stop productions
extract_stops <-
  l2tStops(onsetBuffer = onset_buffer) %>%
  dplyr::mutate(SE = purrr::pmap(
    list(Session, TaggedBurst, TaggedVOT, File),
    function(.session, .burst, .vot, .file) {
      list.files(path = wav_source, pattern = .session, full.names = TRUE) %>%
        speechr::ReadWAV(channel = "left",
                         from = .burst - onset_buffer,
                         to = .vot + offset_buffer) %>%
        speechr::Export(file = file.path("data-raw", "wav-stops", .file))
    }
  ))
