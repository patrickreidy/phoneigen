# Author:       Patrick Reidy
# Email:        <patrick.francis.reidy@gmail.com>





################################################################################
# Gammatone class.                                                             #
# -- A Gammatone object represents a bank of gammatone filters with their      #
#    values for the parameters: amplitude, center frequency, bandwidth, phase  #
#    and order.
################################################################################
# Class definition.
setClass(
  Class = 'Gammatone',
  contains = c(),
  slots    = c(amplitude       = 'numeric',
               centerFrequency = 'numeric',
               bandwidth       = 'numeric',
               phase           = 'numeric',
               order           = 'numeric')
  )

# Object construction.
setGeneric(
  name = 'Gammatone',
  def  = function(amplitude, centerFrequency, bandwidth, phase, order)
    standardGeneric('Gammatone'))
setMethod(
  f   = 'Gammatone',
  sig = c(amplitude       = 'numeric',
          centerFrequency = 'numeric',
          bandwidth       = 'numeric',
          phase           = 'numeric',
          order           = 'numeric'),
  def = function(amplitude, centerFrequency, bandwidth, phase, order)
    new(Class = 'Gammatone',
        amplitude       = amplitude,
        centerFrequency = centerFrequency,
        bandwidth       = bandwidth,
        phase           = phase,
        order           = order))

# Attribute get method: amplitude
if (! isGeneric('amplitude'))
  setGeneric(
    name = 'amplitude',
    def  = function(x, ...) standardGeneric('amplitude'))
setMethod(
  f   = 'amplitude',
  sig = c(x = 'Gammatone'),
  def = function(x) x@amplitude)

# Attribute get method: bandwidth
if (! isGeneric('bandwidth'))
  setGeneric(
    name = 'bandwidth',
    def  = function(x, ...) standardGeneric('bandwidth'))
setMethod(
  f   = 'bandwidth',
  sig = c(x = 'Gammatone'),
  def = function(x) x@bandwidth)

# Attribute get method: centerFrequency
if (! isGeneric('centerFrequency'))
  setGeneric(
    name = 'centerFrequency',
    def  = function(x, ...) standardGeneric('centerFrequency'))
setMethod(
  f   = 'centerFrequency',
  sig = c(x = 'Gammatone'),
  def = function(x) x@centerFrequency)

# Attribute get method: order
if (! isGeneric('filterOrder'))
  setGeneric(
    name = 'filterOrder',
    def  = function(x, ...) standardGeneric('filterOrder'))
setMethod(
  f   = 'filterOrder',
  sig = c(x = 'Gammatone'),
  def = function(x) x@order)

# Attribute get method: phase
if (! isGeneric('phase'))
  setGeneric(
    name = 'phase',
    def  = function(x, ...) standardGeneric('phase'))
setMethod(
  f   = 'phase',
  sig = c(x = 'Gammatone'),
  def = function(x) x@phase)



################################################################################
# ImpulseResponse generic function.                                            #
################################################################################
.angular <- function(x) 2 * pi * x
.GTimp <- function(amp, cf, bw, ord, ph, x)
  amp * x^(ord-1) * cos(.angular(cf)*x + .angular(ph)) * exp(-1*.angular(bw)*x)

# Generic definition.
setGeneric(
  name = 'ImpulseResponse',
  def  = function(filter, x, n, rate, ...) standardGeneric('ImpulseResponse'))
setMethod(
  f   = 'ImpulseResponse',
  sig = c(filter = 'Gammatone', x = 'numeric', n = 'missing', rate = 'missing'),
  def = function(filter, x)
    matrix(data = Reduce(c, Map(.GTimp, 
                                amplitude(filter), centerFrequency(filter),
                                bandwidth(filter), filterOrder(filter),
                                phase(filter), list(x))),
           nrow = length(x))
  )
setMethod(
  f   = 'ImpulseResponse',
  sig = c(filter = 'Gammatone', n = 'numeric', rate = 'numeric', x = 'missing'),
  def = function(filter, n, rate)
    ImpulseResponse(filter, seq(from = 0, by = 1/rate, length = n))
  )



################################################################################
# FrequencyResponse generic function.                                          #
################################################################################
# Generic definition.
setGeneric(
  name = 'FrequencyResponse',
  def  = function(filter, n, rate, binWidth, nyquist, ...)
    standardGeneric('FrequencyResponse'))
setMethod(
  f   = 'FrequencyResponse',
  sig = c(filter = 'Gammatone', n = 'numeric', rate = 'numeric', binWidth = 'missing', nyquist = 'missing'),
  def = function(filter, n, rate) {
    .nyquist <- rate / 2
    .binWidth <- rate / n
    .frequencies <- seq(from = 0, to = .nyquist, by = .binWidth)
    .impulse <- ImpulseResponse(filter, n = n, rate = rate)
    abs(mvfft(.impulse))[1:length(.frequencies), ]
  })
setMethod(
  f   = 'FrequencyResponse',
  sig = c(filter = 'Gammatone', n = 'missing', rate = 'missing', binWidth = 'numeric', nyquist = 'numeric'),
  def = function(filter, binWidth, nyquist)
    FrequencyResponse(filter, (nyquist*2)/binWidth, nyquist*2)
  )