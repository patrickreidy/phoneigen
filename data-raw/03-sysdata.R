


# Attach `magrittr` to make visible the forward-pipe operator.
library(magrittr)


# # Package version 0.0.1 and earlier:
#
# # Source auxiliary code that defines classes and functions for computing
# # an excitation pattern from an acoustic waveform.
# c("Waveform.R", "Spectrum.R", "DPSS.R", "Multitaper.R",
#   "ERB.R", "Gammatone.R", "AuditoryModel.R", "ExcitationPattern.R") %>%
#   (function(.f) {file.path("data-raw", "auxiliary", .f)}) %>%
#   purrr::walk(source)
# source(file.path("data-raw", "01-l2t-data.R"))
#
#
# # Build the auditory model. The auditory periphery is modeled by a bank of
# # 361 bandpass filters. Each filter is a fourth-order, zero-phase gammatone
# # filter. The center frequencies of the filters are uniformly spaced from
# # 3 to 39 (i.e., every 0.1) along the ERB-rate scale. The bandwidth of each
# # filter is proportional to its center frequency; hence, the filters are wider
# # at higher frequencies. These features model the frequency tuning and
# # differential frequency selectivity of the basilar membrane.
# auditoryModel <-
#   FrequencySelectivity(gammatone = TRUE,
#                        erb = seq(from = 3, to = 39, by = 0.1),
#                        n = 44100, sampleRate = 44100) %>%
#   AuditoryModel(sensitivity = "flat")
#
#
#
# # Compute an excitation pattern for each sibilant fricative production.
# # 1. Extract the middle half of the sibilant frication interval with a
# #    rectangular window.
# # 2. Compute an 8th-order multitaper spectrum from this window.
# # 3. Apply the `auditoryModel` to the multitaper spectrum.
# sibilantFricatives <-
#   l2tSibilants() %>%
#   dplyr::mutate(ExcitationPattern = purrr::pmap(
#     list(File, Onset, VOT),
#     function(.file, .onset, .vot) {
#       file.path("data-raw", "wav-sibilants", .file) %>%
#         Waveform(from = .onset + (1/4)*(.vot-.onset),
#                  to = .onset + (3/4)*(.vot-.onset)) %>%
#         PreEmphasize(alpha = 0.95) %>%
#         ZeroPad(lengthOut = 44100) %>%
#         Multitaper(k = 8, nw = 4) %>%
#         ExcitationPattern(auditoryModel = auditoryModel) %>%
#         excitation()
#     }
#   ))
#
#
#
# # Compute an excitation pattern for each stop production.
# # 1. Extract the stop burst with a 25-ms rectangular window whose left
# #    edge is positioned 5 ms prior to the stop burst.
# # 2. Compute an 8th-order multitaper spectrum from this window.
# # 3. Apply the `auditoryModel` to the multitaper spectrum.
# stopBursts <-
#   l2tStops() %>%
#   dplyr::filter(Participant %in% stringr::str_c("A", 50:65, "N")) %>%
#   dplyr::mutate(ExcitationPattern = purrr::pmap(
#     list(File, Burst, VOT),
#     function(.file, .burst, .vot) {
#       file.path("data-raw", "wav-stops", .file) %>%
#         Waveform(from = .burst - 0.005,
#                  to = .burst + 0.02) %>%
#         PreEmphasize(alpha = 0.95) %>%
#         ZeroPad(lengthOut = 44100) %>%
#         Multitaper(k = 8, nw = 4) %>%
#         ExcitationPattern(auditoryModel = auditoryModel) %>%
#         excitation()
#     }
#   ))
#
#
#
#
# # Internalize the `sibilantFricatives` and `stopBursts` data sets.
# devtools::use_data(
#   sibilantFricatives, stopBursts,
#   internal = TRUE, overwrite = TRUE
# )










# Package versions later than 0.0.1 (import data from package interspeech2017.1607):

# Import the interspeech2017.1607::sibilants data set and tidy it.
sibilantFricatives <-
  interspeech2017.1607::sibilants %>%
  tibble::as_tibble() %>%
  dplyr::rename(Session = ID) %>%
  dplyr::mutate(Adult = substring(Session, 1, 1) == "A") %>%
  dplyr::mutate(Age = ifelse(Adult, NA_integer_, as.integer(substring(Session, 5, 6)))) %>%
  dplyr::mutate(Female = substring(Session, 7, 7) == "F") %>%
  dplyr::mutate(MAE = substring(Session, 8, 8) == "S") %>%
  dplyr::mutate(StimulusSet = as.integer(substring(Session, 9, 9))) %>%
  dplyr::rename(Target = Target1) %>%
  dplyr::select(SessionDate, Session, Adult, Participant, Age, Female, MAE,
                StimulusSet, Trial, Orthography, Target, Transcription, Rating,
                ExcitationPattern)

devtools::use_data(sibilantFricatives, internal = TRUE, overwrite = TRUE)





