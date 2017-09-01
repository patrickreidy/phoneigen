# This code depends on a private package: `l2trwr`.
# It will run only on machines that have this package installed.




# Attach `magrittr` to make visible the forward-pipe operator.
library(magrittr)




l2tSibilants <- function(onsetBuffer = 0.02) {
  l2trwr::SibilantFricatives() %>%
    tidyr::unnest(SibilantProductions) %>%
    dplyr::filter(Study == "AdultNorm1") %>%
    dplyr::filter(ConsonantType == "Sibilant fricative") %>%
    dplyr::filter(!is.na(TurbulenceOnset) & !is.na(VOT)) %>%
    dplyr::filter(TurbulenceOnset < VOT) %>%
    dplyr::rename(Participant = Subject) %>%
    dplyr::rename(Session = ResearchID) %>%
    dplyr::rename(TaggingCompleted = TurbulenceTaggingCompleted) %>%
    dplyr::rename(TargetC = Target1) %>%
    dplyr::rename(TargetV = Target2) %>%
    dplyr::rename(TaggedOnset = TurbulenceOnset) %>%
    dplyr::rename(TaggedVOT = VOT) %>%
    dplyr::mutate(Onset = onsetBuffer) %>%
    dplyr::mutate(VOT = TaggedVOT - TaggedOnset + onsetBuffer) %>%
    dplyr::mutate(Orthography = stringr::str_replace(Orthography, " ", "-")) %>%
    dplyr::mutate(File = purrr::pmap_chr(
      list(Participant, Session, Trial, Orthography),
      function(.part, .sess, .trial, .ortho) {
        "${.part}_${.sess}_${.trial}_${.ortho}.WAV" %>%
          stringr::str_interp()
      }
    )) %>%
    dplyr::select(
      File, Participant, Session, SessionCompleted, TaggingCompleted,
      Trial, Orthography, TargetC, TargetV, TaggedOnset, TaggedVOT, Onset, VOT
    )
}




l2tStops <- function(onsetBuffer = 0.02) {
  l2trwr::SingletonStops() %>%
    tidyr::unnest(StopProductions) %>%
    dplyr::filter(Study == "AdultNorm1") %>%
    dplyr::filter(ConsonantType == "Stop") %>%
    dplyr::filter(!is.na(Burst) & !is.na(VOT)) %>%
    dplyr::filter(Burst < VOT) %>%
    dplyr::filter(VOT-Burst > 0.02) %>%
    dplyr::rename(Participant = Subject) %>%
    dplyr::rename(Session = ResearchID) %>%
    dplyr::rename(TaggingCompleted = BurstTaggingCompleted) %>%
    dplyr::rename(TargetC = Target1) %>%
    dplyr::rename(TargetV = Target2) %>%
    dplyr::rename(TaggedBurst = Burst) %>%
    dplyr::rename(TaggedVOT = VOT) %>%
    dplyr::mutate(Burst = onsetBuffer) %>%
    dplyr::mutate(VOT = TaggedVOT - TaggedBurst + onsetBuffer) %>%
    dplyr::mutate(Orthography = stringr::str_replace(Orthography, " ", "-")) %>%
    dplyr::mutate(File = purrr::pmap_chr(
      list(Participant, Session, Trial, Orthography),
      function(.part, .sess, .trial, .ortho) {
        "${.part}_${.sess}_${.trial}_${.ortho}.WAV" %>%
          stringr::str_interp()
      }
    )) %>%
    dplyr::select(
      File, Participant, Session, SessionCompleted, TaggingCompleted,
      Trial, Orthography, TargetC, TargetV, TaggedBurst, TaggedVOT, Burst, VOT
    )
}
