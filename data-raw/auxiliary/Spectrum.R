# Author: Patrick Reidy
# Email:  <patrick.francis.reidy@gmail.com>





################################################################################
# Class definition                                              Spectrum class #
################################################################################

setClass(
  Class = 'Spectrum',
  contains = c(),
  slots    = c(values   = 'numeric',
               binWidth = 'numeric',
               nyquist  = 'numeric')
  )




################################################################################
# Methods                                                     Spectrum methods #
################################################################################

#############################################################################
# @binWidth get-method                                             binWidth #
#############################################################################

# binWidth
if (! isGeneric('binWidth'))
  setGeneric(
    name = 'binWidth',
    def  = function(x) standardGeneric('binWidth'))

setMethod(
  f   = 'binWidth',
  sig = c(x = 'Spectrum'),
  def = function(x) x@binWidth)


#############################################################################
# Centroid frequency of a spectrum                                 centroid #
#############################################################################

# centroid
if (! isGeneric('centroid'))
  setGeneric(
    name = 'centroid',
    def  = function(x, ...) standardGeneric('centroid'))

setMethod(
  f   = 'centroid',
  sig = c(x = 'Spectrum'),
  def = function(x, minHz = 0, maxHz = Inf, scale = 'linear') {
    .freqs <- frequencies(x)[which(minHz <= freq(x) & freq(x) <= maxHz)]
    .vals  <- values(x)[which(minHz <= freq(x) & freq(x) <= maxHz)]
    if (scale == 'dB' | scale == 'decibel')
      .vals <- 10*log10(.vals/min(.vals))
    sum(.freqs * (.vals/sum(.vals)))
  })


#############################################################################
# Variance (second moment) of a spectrum                           variance #
#############################################################################

# variance
if (! isGeneric('variance'))
  setGeneric(
    name = 'variance',
    def  = function(x, ...) standardGeneric('variance')
  )
setMethod(
  f   = 'variance',
  sig = c(x = 'Spectrum'),
  def = function(x, minHz = 0, maxHz = Inf, scale = 'linear') {
    .inds  <- which(minHz <= frequencies(x) & frequencies(x) <= maxHz)
    .freqs <- frequencies(x)[.inds]
    .vals  <- values(x)[.inds]
    if (tolower(scale) %in% c('db', 'decibel'))
      .vals <- 10 * log10(.vals / min(.vals))
    sum((.vals / sum(.vals)) * ((.freqs - centroid(x, minHz, maxHz, scale))^2))
  }
)


#############################################################################
# Skewness (third moment) of a spectrum                            skewness #
#############################################################################

# skewness
if (! isGeneric('skewness'))
  setGeneric(
    name = 'skewness',
    def  = function(x, ...) standardGeneric('skewness')
  )
setMethod(
  f   = 'skewness',
  sig = c(x = 'Spectrum'),
  def = function(x, minHz = 0, maxHz = Inf, scale = 'linear') {
    .inds  <- which(minHz <= frequencies(x) & frequencies(x) <= maxHz)
    .freqs <- frequencies(x)[.inds]
    .vals  <- values(x)[.inds]
    if (tolower(scale) %in% c('db', 'decibel'))
      .vals <- 10 * log10(.vals / min(.vals))
    .top <- sum((.vals / sum(.vals)) * ((.freqs - centroid(x, minHz, maxHz, scale))^3))
    .bottom <- variance(x, minHz, maxHz, scale)^(3/2)
    .top / .bottom
  }
)


#############################################################################
# Kurtosis (fourth moment) of a spectrum                           kurtosis #
#############################################################################

# kurtosis
if (! isGeneric('kurtosis'))
  setGeneric(
    name = 'kurtosis',
    def  = function(x, ...) standardGeneric('kurtosis')
  )
setMethod(
  f   = 'kurtosis',
  sig = c(x = 'Spectrum'),
  def = function(x, minHz = 0, maxHz = Inf, scale = 'linear', excess = FALSE) {
    .inds  <- which(minHz <= frequencies(x) & frequencies(x) <= maxHz)
    .freqs <- frequencies(x)[.inds]
    .vals  <- values(x)[.inds]
    if (tolower(scale) %in% c('db', 'decibel'))
      .vals <- 10 * log10(.vals / min(.vals))
    .top <- sum((.vals / sum(.vals)) * ((.freqs - centroid(x, minHz, maxHz, scale))^4))
    .bottom <- variance(x, minHz, maxHz, scale)^2
    (.top / .bottom) - ifelse(excess, yes = 3, no = 0)
  }
)

#############################################################################
# Fourier frequencies of a spectrum                            fourierFreqs #
#############################################################################

# frequencies
if (! isGeneric('frequencies'))
  setGeneric(
    name = 'frequencies',
    def  = function(x) standardGeneric('frequencies'))

setMethod(
  f   = 'frequencies',
  sig = c(x = 'Spectrum'),
  def = function(x) 
    seq(from = 0, by = binWidth(x), length.out = length(values(x))))

# freq
if (! isGeneric('freq'))
  setGeneric(
    name = 'freq',
    def  = function(x) standardGeneric('freq'))

setMethod(
  f   = 'freq',
  sig = c(x = 'Spectrum'),
  def = function(x) frequencies(x))


#############################################################################
# Maximum amplitude                                           max amplitude #
#############################################################################

# maxAmp
if (! isGeneric('maxAmp'))
  setGeneric(
    name = 'maxAmp',
    def  = function(x, ...) standardGeneric('maxAmp'))

setMethod(
  f   = 'maxAmp',
  sig = c(x = 'Spectrum'),
  def = function(x, minHz = 0, maxHz = Inf, scale = 'dB', ref = 1) {
    .values <- values(x)
    if (tolower(scale) %in% c('decibel', 'db'))
      .values <- 10 * log10(.values / ref)
    .indices <- which(minHz <= frequencies(x) & frequencies(x) <= maxHz)
    max(.values[.indices])
  })


#############################################################################
# Minimum amplitude                                           min amplitude #
#############################################################################

# minAmp
if (! isGeneric('minAmp'))
  setGeneric(
    name = 'minAmp',
    def  = function(x, ...) standardGeneric('minAmp'))

setMethod(
  f   = 'minAmp',
  sig = c(x = 'Spectrum'),
  def = function(x, minHz = 0, maxHz = Inf, scale = 'dB', ref = 1) {
    .values <- values(x)
    if (tolower(scale) %in% c('decibel', 'db'))
      .values <- 10 * log10(.values / ref)
    .indices <- which(minHz <= frequencies(x) & frequencies(x) <= maxHz)
    min(.values[.indices])
  })


#############################################################################
# @nyquist get-method                                               nyquist #
#############################################################################

# nyquist
if (! isGeneric('nyquist'))
  setGeneric(
    name = 'nyquist',
    def  = function(x) standardGeneric('nyquist'))

setMethod(
  f   = 'nyquist',
  sig = c(x = 'Spectrum'),
  def = function(x) x@nyquist)


#############################################################################
# Peak frequency of a spectrum                                       peakHz #
#############################################################################

# peakHz
if (! isGeneric('peakHz'))
  setGeneric(
    name = 'peakHz',
    def  = function(x, ...) standardGeneric('peakHz'))

setMethod(
  f   = 'peakHz',
  sig = c(x = 'Spectrum'),
  def = function(x, minHz = 0, maxHz = Inf) {
    .inds <- which(minHz <= frequencies(x) & frequencies(x) <= maxHz)
    .freq <- frequencies(x)[.inds]
    .vals <- values(x)[.inds]
    .freq[which.max(.vals)]
  })


#############################################################################
#  show                                                                show #
#############################################################################

setMethod(
  f   = 'show',
  sig = c(object = 'Spectrum'),
  def = function(object) {
    .spec = data.frame(Frequency = frequencies(object),
                       Amplitude = 10*log10(values(object)/max(values(object))))
    print(
      ggplot(data = .spec, aes(x = Frequency, y = Amplitude)) +
        geom_path(colour = 'black') + theme_bw() +
        xlab('Frequency (Hz)') + ylab('Amplitude (dB)')
    )
  })


#############################################################################
# @values get-method                                                 values #
#############################################################################

# values
if (! isGeneric('values'))
  setGeneric(
    name = 'values',
    def  = function(x) standardGeneric('values'))

setMethod(
  f   = 'values',
  sig = c(x = 'Spectrum'),
  def = function(x) x@values)

# ordinates
if (! isGeneric('ordinates'))
  setGeneric(
    name = 'ordinates',
    def  = function(x) standardGeneric('ordinates'))

setMethod(
  f   = 'ordinates',
  sig = c(x = 'Spectrum'),
  def = function(x) values(x))





# peakBandwidth
if (! isGeneric('peakBandwidth'))
  setGeneric(
    name = 'peakBandwidth',
    def  = function(x, ...) standardGeneric('peakBandwidth'))
setMethod(
  f   = 'peakBandwidth',
  sig = c(x = 'Spectrum'),
  def = function(x, minHz = 0, maxHz = Inf, dBdrop = 3) {
    #.peak_hz <- peakERB(x, minERB, maxERB)
    .peak_hz <- peakHz(x, minHz, maxHz)
    #.peak.ind  <- which(erb(x) == .peak.erb)
    .peak_ind <- which(frequencies(x) == .peak_hz)
    #.dB.excite <- 10 * log10(excitation(x) / max(excitation(x)))
    .dB_values <- 10 * log10(values(x) / max(values(x)))
    #.peak.amp <- .dB.excite[which(erb(x) == .peak.erb)]
    .peak_dB <- .dB_values[.peak_ind]
    # Forward.
    .forward <- .peak_ind
    .forward_drop <- .peak_dB - .dB_values[.forward]
    while (.forward < length(frequencies(x)) & .forward_drop <= dBdrop) {
      .forward <- .forward + 1
      .forward_drop <- .peak_dB - .dB_values[.forward]
    }
    .forward_hz <- frequencies(x)[.forward]
    # Backward.
    .backward <- .peak_ind
    .backward_drop <- .peak_dB - .dB_values[.backward]
    while (.backward > 1 & .backward_drop <= dBdrop) {
      .backward <- .backward - 1
      .backward_drop <- .peak_dB - .dB_values[.backward]
    }
    .backward_hz <- frequencies(x)[.backward]
    .bandwidth <- .forward_hz - .backward_hz
    return(.bandwidth)
  })




# A method for determining the amplitude drop across a higher and lower
# frequency band within an excitation pattern.
if (! isGeneric('ampDrop'))
  setGeneric(
    name = 'ampDrop',
    def  = function(x, ...) standardGeneric('ampDrop')
  )
setMethod(
  f   = 'ampDrop',
  sig = c(x = 'Spectrum'),
  def = function(x, lowerMinHz = 500, lowerMaxHz = 3000, upperMinHz = 3000, upperMaxHz = Inf, scale = 'decibel', ref = 1) {
    .upper_max <- maxAmp(x = x, minHz = upperMinHz, maxHz = upperMaxHz, scale = scale, ref = ref)
    .lower_min <- minAmp(x = x, minHz = lowerMinHz, maxHz = lowerMaxHz, scale = scale, ref = ref)
    return(.upper_max - .lower_min)
  }
)
