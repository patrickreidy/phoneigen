# Author:       Patrick Reidy
# Email:        <patrick.francis.reidy@gmail.com>





################################################################################
# ExcitationPattern class.                                                     #
################################################################################
# Class definition.
setClass(
  Class = 'ExcitationPattern',
  contains = c(),
  slots    = c(excitation = 'numeric',
               erb = 'numeric'))

# Object construction.
setGeneric(
  name = 'ExcitationPattern',
  def  = function(spectrum, auditoryModel) 
    standardGeneric('ExcitationPattern'))
setMethod(
  f   = 'ExcitationPattern',
  sig = c(spectrum = 'Spectrum', auditoryModel = 'AuditoryModel'),
  def = function(spectrum, auditoryModel)
    new(Class = 'ExcitationPattern',
        excitation = as.numeric(values(spectrum) %*% auditoryModel@response),
        erb = auditoryModel@erb))





################################################################################
# Methods for ExcitationPattern objects                                        #
################################################################################
# Hidden method: .as_pmf, a function for normalizing excitation levels to 
# sum to one, so that the excitation pattern can be treated as a discrete
# probability mass function.
if (! isGeneric('.as_pmf'))
  setGeneric(
    name = '.as_pmf',
    def  = function(x, ...) standardGeneric('.as_pmf'))
setMethod(
  f   = '.as_pmf',
  sig = c(x = 'ExcitationPattern'),
  def = function(x, minERB = -Inf, maxERB = Inf, scale = 'decibel') {
    # Grab ERB numbers from ExcitationPattern [x].
    .erbs <- erb(x)
    # Partition the ERB number scale into left, center, and right; center is
    # band whose excitation levels will be normalized to a pmf.
    .left_i <- which(.erbs < minERB)
    .center_i <- which(minERB <= .erbs & .erbs <= maxERB)
    .right_i <- which(maxERB < .erbs)
    # Grab the excitation levels from ExcitationPattern [x].
    .excit <- excitation(x)
    .center_excit <- .excit[.center_i]
    if (tolower(scale) %in% c('db', 'decibel')) {
      .center_excit <- 10 * log10(.center_excit / (min(.center_excit)/2))
    }
    .center_excit <- .center_excit / sum(.center_excit)
    # Return a new ExcitationPattern object.
    new(Class = 'ExcitationPattern',
        excitation = c(rep(0, length(.left_i)),   # left
                       .center_excit,             # center
                       rep(0, length(.right_i))), # right
        erb = .erbs)
  })

# @excitation get method
if (! isGeneric('excitation'))
  setGeneric(
    name = 'excitation',
    def  = function(x, ...) standardGeneric('excitation'))
setMethod(
  f   = 'excitation',
  sig = c(x = 'ExcitationPattern'),
  def = function(x) x@excitation)

# @erb get method.
if (! isGeneric('erb'))
  setGeneric(
    name = 'erb',
    def  = function(x, ...) standardGeneric('erb'))
setMethod(
  f   = 'erb',
  sig = c(x = 'ExcitationPattern'),
  def = function(x) x@erb)

# ampD
if (! isGeneric('ampD'))
  setGeneric(
    name = 'ampD',
    def  = function(x, ...) standardGeneric('ampD'))
setMethod(
  f   = 'ampD',
  sig = c(x = 'ExcitationPattern'),
  def = function(x, lowerBandMin = ERBscale(550), lowerBandMax = ERBscale(3000),
                 upperBandMin = ERBscale(3000), upperBandMax = ERBscale(15000)) {
    .excitation <- 10 * log10(excitation(x) / max(excitation(x)))
    .lower <- which(lowerBandMin <= erb(x) & erb(x) <= lowerBandMax)
    .upper <- which(upperBandMin <= erb(x) & erb(x) <= upperBandMax)
    max(.excitation[.upper]) - min(.excitation[.lower])
  })

# compactness index
if (! isGeneric('compactness'))
  setGeneric(
    name = 'compactness',
    def  = function(x, ...) standardGeneric('compactness'))
setMethod(
  f   = 'compactness',
  sig = c(x = 'ExcitationPattern'),
  def = function(x, minERB = 0, maxERB = Inf, widthERB = 3, scale = 'decibel', adaptive = FALSE, dBdown = 3) {
    .peak_erb  <- peakERB(x, minERB, maxERB)
    .peak_ind  <- which(erb(x) == .peak_erb)
    .scale <- scale
    if (adaptive & !is.null(dBdown)) {
      .scale <- 'decibel'
      .dB_excite <- 10 * log10(excitation(x) / max(excitation(x)))
      .peak_dB <- .dB_excite[which(erb(x) == .peak_erb)]
      # Find the .lower_ind
      .lower_ind <- .peak_ind
      .lower_drop <- .peak_dB - .dB_excite[.lower_ind]
      while (.lower_ind > 1 & .lower_drop <= dBdown) {
        .lower_ind <- .lower_ind - 1
        .lower_drop <- .peak_dB - .dB_excite[.lower_ind]
      }
      # Find the .upper_bound
      .upper_ind <- .peak_ind
      .upper_drop <- .peak_dB - .dB_excite[.upper_ind]
      while (.upper_ind < length(erb(x)) & .upper_drop <= dBdown) {
        .upper_ind <- .upper_ind + 1
        .upper_drop <- .peak_dB - .dB_excite[.upper_ind]
      }
#       message(sprintf('Lower ERB: %.1f', erb(x)[.lower_ind]))
#       message(sprintf('Peak ERB: %.1f', .peak_erb))
#       message(sprintf('Upper ERB: %.1f', erb(x)[.upper_ind]))
    } else {
      .lower_erb <- max(c(min(erb(x)), .peak_erb - (widthERB/2)))
      .upper_erb <- min(c(max(erb(x)), .peak_erb + (widthERB/2)))
      if (.lower_erb == min(erb(x))) .upper_erb <- .lower_erb + widthERB
      if (.upper_erb == max(erb(x))) .lower_erb <- .upper_erb - widthERB
      .lower_ind <- which.min(abs(erb(x) - .lower_erb))
      .upper_ind <- which.min(abs(erb(x) - .upper_erb))
    }
    .ex_levels <- excitation(x)
    if (tolower(.scale) %in% c('decibel', 'db')) {
      .ex_levels <- 10*log10(.ex_levels / min(.ex_levels))
    } else if (tolower(.scale) %in% c('linear', 'lin')) {
      .ex_levels <- .ex_levels - min(.ex_levels)
    }
    .compactness <- sum(.ex_levels[.lower_ind:.upper_ind]) / sum(.ex_levels)
    return(.compactness)
  })


# Kullback-Leibler divergence (symmetric, nonzero)
if (! isGeneric('KLdivergence'))
  setGeneric(
    name = 'KLdivergence',
    def  = function(x, y, ...) standardGeneric('KLdivergence'))
setMethod(
  f   = 'KLdivergence',
  sig = c(x = 'ExcitationPattern', y = 'ExcitationPattern'),
  def = function(x, y, minERB = -Inf, maxERB = Inf, scale = 'decibel') {
    # Compute KL divergence only if [x] and [y] are defined on the same
    # ERB numbers.
    if (identical(erb(x), erb(y))) {
      # Define non-symmetric KL divergence function.
      .KLdiv <- function(x, y) sum(x * log(x/y))
      .x_ex <- excitation(.as_pmf(x, minERB, maxERB, scale))
      .y_ex <- excitation(.as_pmf(y, minERB, maxERB, scale))
      .kl_div <- .KLdiv(.x_ex, .y_ex) + .KLdiv(.y_ex, .x_ex)
    } else {
      message('KL divergence not computed: [x] and [y] defined on different ERB numbers.')
      .kl_div <- NULL
    }
    return(.kl_div)
  })

# peakBandwidth
if (! isGeneric('peakBandwidth'))
  setGeneric(
    name = 'peakBandwidth',
    def  = function(x, ...) standardGeneric('peakBandwidth'))
setMethod(
  f   = 'peakBandwidth',
  sig = c(x = 'ExcitationPattern'),
  def = function(x, minERB = 0, maxERB = Inf, dBdrop = 3, scale = 'erb') {
    .peak.erb  <- peakERB(x, minERB, maxERB)
    .peak.ind  <- which(erb(x) == .peak.erb)
    .dB.excite <- 10 * log10(excitation(x) / max(excitation(x)))
    .peak.amp <- .dB.excite[which(erb(x) == .peak.erb)]
    # Forward.
    .forward <- .peak.ind
    .forward.drop <- .peak.amp - .dB.excite[.forward]
    while (.forward < length(erb(x)) & .forward.drop <= dBdrop) {
      .forward <- .forward + 1
      .forward.drop <- .peak.amp - .dB.excite[.forward]
    }
    .forward.erb <- erb(x)[.forward]
    # Backward.
    .backward <- .peak.ind
    .backward.drop <- .peak.amp - .dB.excite[.backward]
    while (.backward > 1 & .backward.drop <= dBdrop) {
      .backward <- .backward - 1
      .backward.drop <- .peak.amp - .dB.excite[.backward]
    }
    .backward.erb <- erb(x)[.backward]
    if (tolower(scale) == 'erb')
      .forward.erb - .backward.erb
    else if (tolower(scale) == 'hz')
      Hz(.forward.erb) - Hz(.backward.erb)
  })

# peakERB.
if (! isGeneric('peakERB'))
  setGeneric(
    name = 'peakERB',
    def  = function(x, ...) standardGeneric('peakERB'))
setMethod(
  f   = 'peakERB',
  sig = c(x = 'ExcitationPattern'),
  def = function(x, minERB = 0, maxERB = Inf) {
    .inds <- which(minERB <= erb(x) & erb(x) <= maxERB)
    .excs <- excitation(x)[.inds]
    .erbs <- erb(x)[.inds]
    .erbs[which.max(.excs)]
  })

# peakAmplitude.
if (! isGeneric('peakAmplitude'))
  setGeneric(
    name = 'peakAmplitude',
    def = function(x, ...) standardGeneric('peakAmplitude')
  )
setMethod(
  f = 'peakAmplitude',
  sig = c(x = 'ExcitationPattern'),
  def = function(x, minERB = 0, maxERB = Inf, decibel = TRUE) {
    .peak_erb <- peakERB(x = x, minERB = minERB, maxERB = maxERB)
    .peak_amp <- excitation(x)[which(erb(x) == .peak_erb)]
    if (decibel) {
      .peak_amp <- 10 * log10(.peak_amp)
    }
    return(.peak_amp)
  }
)

# # peakProportion
# if (! isGeneric('peakProportion'))
#   setGeneric(
#     name = 'peakProportion',
#     def  = function(x, ...) standardGeneric('peakProportion'))
# setMethod(
#   f   = 'peakProportion',
#   sig = c(x = 'ExcitationPattern'),
#   def = function(x, minERB = 0, maxERB = Inf, dBdrop = 3) {
#     .peak.erb  <- peakERB(x, minERB, maxERB)
#     .peak.ind  <- which(erb(x) == .peak.erb)
#     .dB.excite <- 10 * log10(excitation(x) / min(excitation(x)))
#     .peak.amp <- .dB.excite[which(erb(x) == .peak.erb)]
#     # Forward.
#     .forward <- .peak.ind
#     .forward.drop <- .peak.amp - .dB.excite[.forward]
#     while (.forward < length(erb(x)) & .forward.drop <= dBdrop) {
#       .forward <- .forward + 1
#       .forward.drop <- .peak.amp - .dB.excite[.forward]
#     }
#     .forward.erb <- erb(x)[.forward]
#     # Backward.
#     .backward <- .peak.ind
#     .backward.drop <- .peak.amp - .dB.excite[.backward]
#     while (.backward > 1 & .backward.drop <= dBdrop) {
#       .backward <- .backward - 1
#       .backward.drop <- .peak.amp - .dB.excite[.backward]
#     }
#     .backward.erb <- erb(x)[.backward]
#     sum(.dB.excite[.backward:.forward]) / sum(.dB.excite)
#   })

# show.
setMethod(
  f   = 'show',
  sig = c(object = 'ExcitationPattern'),
  def = function(object) {
    .spec = data.frame(Frequency = erb(object),
                       Excitation = 10*log10(excitation(object)/max(excitation(object))))
    print(
      ggplot(data = .spec, aes(x = Frequency, y = Excitation)) +
        geom_path(colour = 'black') + theme_bw() +
        xlab('Frequency (ERB)') + ylab('Excitation (dB)')
    )
  })

# highlight.
if (! isGeneric('highlight'))
  setGeneric(
    name = 'highlight',
    def  = function(object, xMin, xMax, ...) standardGeneric('highlight')
  )
setMethod(
  f   = 'highlight',
  sig = c(object = 'ExcitationPattern', xMin = 'numeric', xMax = 'numeric'),
  def = function(object, xMin, xMax) {
    .InRange <- function(x, lim1, lim2) lim1 <= x & x <= lim2
    .erbs        <- erb(object)
    .excitations <- 10*log10(excitation(object)/min(excitation(object)))
    .in_range <- as.logical(Reduce(`+`, Map(.InRange, x = list(.erbs), lim1 = xMin, lim2 = xMax)))
    .highlight   <- ifelse(.in_range, yes = .excitations, no = NA)
    .background  <- data.frame(Frequency = .erbs, Excitation = .excitations)
    .foreground  <- data.frame(Frequency = .erbs, Excitation = .highlight)
    suppressWarnings(
      print(
        ggplot(data = .background, aes(x = Frequency, y = Excitation)) +
          theme_bw() +
          xlab('Frequency (ERB number)') +
          ylab('Excitation (dB)') +
          geom_line(colour = 'gray', alpha = 0.5, size = 1) +
          geom_line(data = .foreground, colour = '#e41a1c', size = 2)
      )
    )
  }
)

# centroid
if (! isGeneric('centroid'))
  setGeneric(
    name = 'centroid',
    def  = function(x, ...) standardGeneric('centroid'))
setMethod(
  f   = 'centroid',
  sig = c(x = 'ExcitationPattern'),
  def = function(x, minERB = 0, maxERB = Inf, scale = 'linear') {
    .erbs <- erb(x)[which(minERB <= erb(x) & erb(x) <= maxERB)]
    .vals <- excitation(x)[which(minERB <= erb(x) & erb(x) <= maxERB)]
    if (scale == 'dB' | scale == 'decibel')
      .vals <- 10*log10(.vals/min(.vals))
    sum(.erbs * (.vals/sum(.vals)))
  })

# variance
if (! isGeneric('variance'))
  setGeneric(
    name = 'variance',
    def  = function(x, ...) standardGeneric('variance')
  )
setMethod(
  f   = 'variance',
  sig = c(x = 'ExcitationPattern'),
  def = function(x, minERB = 0, maxERB = Inf, scale = 'linear') {
    .inds <- which(minERB <= erb(x) & erb(x) <= maxERB)
    .erbs <- erb(x)[.inds]
    .vals <- excitation(x)[.inds]
    if (scale %in% c('db', 'decibel'))
      .vals <- 10 * log10(.vals / min(.vals))
    sum((.vals / sum(.vals)) * ((.erbs - centroid(x, minERB, maxERB, scale))^2))
  }
)

# skewness
if (! isGeneric('skewness'))
  setGeneric(
    name = 'skewness',
    def  = function(x, ...) standardGeneric('skewness')
  )
setMethod(
  f   = 'skewness',
  sig = c(x = 'ExcitationPattern'),
  def = function(x, minERB = 0, maxERB = Inf, scale = 'linear') {
    .inds <- which(minERB <= erb(x) & erb(x) <= maxERB)
    .erbs <- erb(x)[.inds]
    .vals <- excitation(x)[.inds]
    if (scale %in% c('db', 'decibel'))
      .vals <- 10 * log10(.vals / min(.vals))
    .top <- sum((.vals / sum(.vals)) * ((.erbs - centroid(x, minERB, maxERB, scale))^3))
    .bottom <- variance(x, minERB, maxERB, scale)^(3/2)
    .top / .bottom
  }
)

# kurtosis
if (! isGeneric('kurtosis'))
  setGeneric(
    name = 'kurtosis',
    def  = function(x, ...) standardGeneric('kurtosis')
  )
setMethod(
  f   = 'kurtosis',
  sig = c(x = 'ExcitationPattern'),
  def = function(x, minERB = 0, maxERB = Inf, scale = 'linear', excess = FALSE) {
    .inds <- which(minERB <= erb(x) & erb(x) <= maxERB)
    .erbs <- erb(x)[.inds]
    .vals <- excitation(x)[.inds]
    if (scale %in% c('db', 'decibel'))
      .vals <- 10 * log10(.vals / min(.vals))
    .top <- sum((.vals / sum(.vals)) * ((.erbs - centroid(x, minERB, maxERB, scale))^4))
    .bottom <- variance(x, minERB, maxERB, scale)^2
    (.top / .bottom) - ifelse(excess, yes = 3, no = 0)
  }
)



# A method for determining the maximum excitation level within a given band
# of an excitation pattern.
if (! isGeneric('maxExcitation'))
  setGeneric(
    name = 'maxExcitation',
    def  = function(x, ...) standardGeneric('maxExcitation'))
setMethod(
  f   = 'maxExcitation',
  sig = c(x = 'ExcitationPattern'),
  def = function(x, minERB = 0, maxERB = Inf, scale = 'decibel', ref = 1) {
    .excitation <- excitation(x)
    if (tolower(scale) %in% c('decibel', 'db'))
      .excitation <- 10 * log10(.excitation / ref)
    .indices <- which(minERB <= erb(x) & erb(x) <= maxERB)
    max(.excitation[.indices])
  })
# A method for determining the minimum excitation level within a given band
# of an excitation pattern.
if (! isGeneric('minExcitation'))
  setGeneric(
    name = 'minExcitation',
    def  = function(x, ...) standardGeneric('minExcitation'))
setMethod(
  f   = 'minExcitation',
  sig = c(x = 'ExcitationPattern'),
  def = function(x, minERB = 0, maxERB = Inf, scale = 'decibel', ref = 1) {
    .excitation <- excitation(x)
    if (tolower(scale) %in% c('decibel', 'db'))
      .excitation <- 10 * log10(.excitation / ref)
    .indices <- which(minERB <= erb(x) & erb(x) <= maxERB)
    min(.excitation[.indices])
  })
# A method for determining the excitation drop across a higher and lower
# frequency band within an excitation pattern.
if (! isGeneric('excitationDrop'))
  setGeneric(
    name = 'excitationDrop',
    def  = function(x, ...) standardGeneric('excitationDrop')
    )
setMethod(
  f   = 'excitationDrop',
  sig = c(x = 'ExcitationPattern'),
  def = function(x, lowerMinERB = ERBscale(500), lowerMaxERB = ERBscale(3000), upperMinERB = ERBscale(3000), upperMaxERB = Inf, scale = 'decibel', ref = 1) {
    .upper_max <- maxExcitation(x = x, minERB = upperMinERB, maxERB = upperMaxERB, scale = scale, ref = ref)
    .lower_min <- minExcitation(x = x, minERB = lowerMinERB, maxERB = lowerMaxERB, scale = scale, ref = ref)
    return(.upper_max - .lower_min)
  }
)

