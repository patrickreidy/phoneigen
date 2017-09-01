# Author:       Patrick Reidy
# Email:        <patrick.francis.reidy@gmail.com>





################################################################################
# FrequencySelectivity class.                                                  #
# -- A FrequencySelectivity object models the auditory system's differential   #
#    frequency selectivity as a bank of filters, whose bandwidths increase     #
#    with the filters' center frequencies.                                     #
################################################################################
# Class definition.
setClass(
  Class = 'FrequencySelectivity',
  contains = c(),
  slots    = c(response = 'matrix',
               erb      = 'numeric',
               filter   = 'character')
  )

# Object construction.
setGeneric(
  name = 'FrequencySelectivity',
  def  = function(gammatone, erb, n, sampleRate, ...)
    standardGeneric('FrequencySelectivity'))
setMethod(
  f   = 'FrequencySelectivity',
  sig = c(gammatone = 'Gammatone', n = 'numeric', sampleRate = 'numeric', erb = 'missing'),
  def = function(gammatone, n, sampleRate)
    new(Class = 'FrequencySelectivity',
        response = FrequencyResponse(filter = gammatone, rate = sampleRate, n = n),
        erb      = ERBscale(centerFrequency(gammatone)),
        filter   = 'gammatone')
  )
setMethod(
  f   = 'FrequencySelectivity',
  sig = c(gammatone = 'logical', erb = 'numeric', n = 'numeric', sampleRate = 'numeric'),
  def = function(gammatone = TRUE, erb, n, sampleRate) {
    if (gammatone) {
      .channels <- length(erb)
      .gtbank <- Gammatone(amplitude = rep(1, .channels),
                           centerFrequency = Hz(erb),
                           bandwidth = 1.019 * ERB(Hz(erb)),
                           phase = rep(0, .channels),
                           order = rep(4, .channels))
      FrequencySelectivity(gammatone = .gtbank, n = n, sampleRate = sampleRate)
    }
  })
  



################################################################################
# AuditoryModel class.                                                         #
################################################################################
# Class definition.
setClass(
  Class = 'AuditoryModel',
  contains = c(),
  slots    = c(response = 'matrix',
               erb = 'numeric')
  )

# Object construction.
setGeneric(
  name = 'AuditoryModel',
  def  = function(selectivity, sensitivity) standardGeneric('AuditoryModel'))
setMethod(
  f   = 'AuditoryModel',
  sig = c(selectivity = 'FrequencySelectivity',
          sensitivity = 'character'),
  def = function(selectivity, sensitivity) {
    if (sensitivity %in% c('flat', 'uniform'))
      .scalars <- apply(X = selectivity@response,
                        MARGIN = 2, # apply to columns
                        FUN = max)
    else if (sensitivity %in% c('normalize', 'sum'))
      .scalars <- colSums(selectivity@response)
    .scalars <- matrix(data = rep(.scalars, each = nrow(selectivity@response)),
                       nrow = nrow(selectivity@response))
    new(Class = 'AuditoryModel',
        response = selectivity@response / .scalars,
        erb = selectivity@erb)
  })

