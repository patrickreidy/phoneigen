
#' Sibilant Fricatives
#'
#' Access the internal data set for productions of target sibilant fricatives
#' by 16 adult native speakers and 69 two- to three-year-old native learners
#' of American English, who participated in the Learning to Talk Project.
#' The participants' productions were elicited in word-initial pre-vocalic
#' position during a real word repetition task.
#'
#' @return A \code{\link[tibble]{tibble}} with 2475 rows (i.e., target sibilant
#'   fricative productions) and 14 columns (i.e., variables):
#'   \enumerate{
#'     \item \code{SessionDate}: The date on which the participant completed the
#'       session, in \code{YYYY-MM-DD} format.
#'     \item \code{Session}: An alphanumeric code for the session. Each adult
#'       completed two sessions; each child completed one.
#'     \item \code{Adult}: A logical vector indicating whether the participant
#'       is an adult (= \code{TRUE}) or a child (= \code{FALSE}).
#'     \item \code{Participant}: An alphanumeric code for the participant.
#'     \item \code{Age}: An integer vector, the ages, in months, of the children
#'       and \code{NA_integer_} for the adults.
#'     \item \code{Female}: A logical vector indicating whether the participant
#'       is female (= \code{TRUE}) or male (= \code{FALSE}).
#'     \item \code{MAE}: A logical vector indicating whether the audio prompts
#'       in the session were presented in Mainstream American English
#'       (= \code{TRUE}) or African American English (= \code{FALSE}).
#'     \item \code{StimulusSet}: An integer vector indicating which (multi)set
#'       of sibilant-initial words were elicited during the session. Adults
#'       completed two sessions with stimulus sets 2 and 3. Children completed
#'       one session with either stimulus set 1 or 2, depending on their age:
#'       children 32 months and younger completed set 1; children 34
#'       months and older completed set 2.
#'     \item \code{Trial}: The trial number within the session when the
#'       production was elicited.
#'     \item \code{Orthography}: The orthographic transcription of the word
#'       presented during the \code{Trial}, used to elicit a production of a
#'       sibilant fricative.
#'     \item \code{Target}: The WorldBet transcription of the target sibilant
#'       fricative.
#'     \item \code{Transcription}: A broad WorldBet transcription of the
#'       produced sibilant fricative. Note: \code{s:S} denotes a produced
#'       sibilant whose place of articulation was judged to be intermediate
#'       between \code{s} and \code{S}, but closer to \code{S}; and conversely
#'       for \code{S:s}.
#'     \item \code{Rating}: A numeric vector, \code{NA_real_} for productions
#'       by adults; the mean rating along a visual analog scale for productions
#'       by children. See "Visual Analog Scale Ratings" section below.
#'     \item \code{ExcitationPattern}: A list-column of 361-component numeric
#'       vectors, each of which represents the values of an excitation pattern
#'       computed from the production. These values are associated to the
#'       vector of center frequencies, on the ERB scale:
#'       \code{seq(from = 3, to = 39, by = 0.1)}.
#'       See "Excitation Patterns" section below.
#'   }
#'
#' @section Excitation Patterns:
#'   An excitation pattern is a type of psychoacoustic spectrum that represents
#'   the distribution of auditory excitation across auditory filters. To compute
#'   an excitation pattern, the auditory periphery was modeled by a bank of 361
#'   bandpass filters. Each filter was a fourth-order, zero-phase gammatone filter.
#'   The center frequencies of the filters were uniformly spaced from 3 to 39
#'   along the ERB scale (i.e., 0.1 inter-filter spacing). The bandiwidth of
#'   each filter was proportional to its center frequency; hence, the filters
#'   were wider at high frequencies. These features model how the basilar
#'   membrane compresses the frequency scale logarithmically, and is
#'   differentially tuned to different frequency components. The excitation
#'   pattern of an acoustic waveform is computed by filtering it through the
#'   gammatone bank, summing the energy at the output of each filter, and
#'   associating the output energy of each filter to its center frequency.
#'
#'   See \code{data-raw/03-excitation-patterns} in the source package for the
#'   code used to compute the vectors in the list-column \code{ExcitationPattern}
#'   of this data set.
#'
#' @section Visual Analog Scale Ratings:
#'   Each production by a child was used as a stimulus in a visual-analog-scale
#'   perceptual rating task. From the recording of each of these whole-word
#'   productions, the initial CV sequence was extracted, beginning 5 ms prior
#'   to the onset of sibilant frication and ending 150 ms after the onset of
#'   voicing for the vowel. Batches of these extracted sequences were then
#'   presented to 70 listeners who were all native monolingual speakers of
#'   American English between the ages of 18 and 50 years and who reported
#'   no current or previous speech, language, or hearing disorder.
#'
#'   On each trial in the perceptual rating task, the listener saw a
#'   double-headed arrow anchored by the text "the 's' sound" at one end and
#'   "the 'sh' sound" at the other. The stimulus was played once, and the
#'   listener was asked to rate where the initial consonant fell on this
#'   visual analog scale by clicking at an appropriate location along the
#'   arrow. The click location in pixels was logged automatically, and the
#'   pixel locations have been normalized to fall within the [0,1] range, where
#'   0 denotes an ideal /s/, and 1 denotes an ideal /S/.
#'
#'   Listeners were given no explicit instructions on what criteria they should
#'   use to judge the fricative. They were encouraged to use their 'gut instinct'.
#'   Each stimulus was rated by at least 15 listeners. The mean normalized
#'   rating (i.e., within the [0,1] range) was computed for each stimulus, and
#'   this mean rating is the number that appears in the \code{Rating} column
#'   of this data set.
#'
#' @source See \url{learningtotalk.org} for more information about the
#'   Learning to Talk Project.
#'
#' @export
SibilantFricatives <- function() {
  tibble::as_tibble(sibilantFricatives)
}





