# Author:       Patrick Reidy
# Email:        <patrick.francis.reidy@gmail.com>





################################################################################
# AuditoryBandwidth class.                                                     #
# -- An AuditoryBandwith object is a list of the two numeric parameters that   #
#    determine how a Hz value relates to either an ERB or an ERB-scale value.  #
################################################################################
# Class definition
setClass(
  Class = 'AuditoryBandwidth',
  contains = c(),
  slots    = c(resolution   = 'numeric',
               minBandwidth = 'numeric')
  )

# Object construction
setGeneric(
  name = 'AuditoryBandwidth',
  def  = function(resolution, minBandwidth) standardGeneric('AuditoryBandwidth'))
setMethod(
  f   = 'AuditoryBandwidth',
  sig = c(resolution = 'missing', minBandwidth = 'missing'),
  def = function()
    new(Class = 'AuditoryBandwidth',
        resolution   = 1.0 / (24.7*0.00437),
        minBandwidth = 24.7))
setMethod(
  f   = 'AuditoryBandwidth',
  sig = c(resolution = 'numeric', minBandwidth = 'numeric'),
  def = function(resolution, minBandwidth)
    new(Class = 'AuditoryBandwidth',
        resolution   = resolution,
        minBandwidth = minBandwidth))

# Attribute get method: minBandwidth
if (! isGeneric('minBandwidth'))
  setGeneric(
    name = 'minBandwidth',
    def  = function(x) standardGeneric('minBandwidth'))
setMethod(
  f   = 'minBandwidth',
  sig = c(x = 'AuditoryBandwidth'),
  def = function(x) x@minBandwidth)
# Attribute get method: resolution.
if (! isGeneric('resolution'))
  setGeneric(
    name = 'resolution',
    def  = function(x) standardGeneric('resolution'))
setMethod(
  f   = 'resolution',
  sig = c(x = 'AuditoryBandwidth'),
  def = function(x) x@resolution
  )



################################################################################
# ERB generic function.                                                        #
# -- ERB is a function of center frequency (hz), parameterized by an           #
#    AuditoryBandwidth object.                                                 #
################################################################################
if (! isGeneric('ERB'))
  setGeneric(
    name = 'ERB',
    def  = function(hz, auditoryBandwidth) standardGeneric('ERB'))
# Method signature: hz = numeric; auditoryBandwidth = AuditoryBandwidth
setMethod(
  f   = 'ERB',
  sig = c(hz = 'numeric', 
          auditoryBandwidth = 'AuditoryBandwidth'),
  def = function(hz, auditoryBandwidth) 
    (hz / resolution(auditoryBandwidth)) + minBandwidth(auditoryBandwidth))
# Method signature: hz = numeric; auditoryBandwidth = missing
# -- Calling this method uses the default AuditoryBandwidth() parameter values.
setMethod(
  f   = 'ERB',
  sig = c(hz = 'numeric', 
          auditoryBandwidth = 'missing'),  
  def = function(hz) ERB(hz, AuditoryBandwidth()))



################################################################################
# ERBscale generic function.                                                   #
# -- The ERBscale is a function of center frequency (hz) and is parameterized  #
#    by an AuditoryBandwidth object.                                           #
################################################################################
if (! isGeneric('ERBscale'))
  setGeneric(
    name = 'ERBscale',
    def  = function(hz, auditoryBandwidth) standardGeneric('ERBscale'))
setMethod(
  f   = 'ERBscale',
  sig = c(hz = 'numeric', auditoryBandwidth = 'AuditoryBandwidth'),
  def = function(hz, auditoryBandwidth)
    resolution(auditoryBandwidth) * log((hz / (resolution(auditoryBandwidth) * minBandwidth(auditoryBandwidth))) + 1))
setMethod(
  f   = 'ERBscale',
  sig = c(hz = 'numeric', auditoryBandwidth = 'missing'),
  def = function(hz) ERBscale(hz, AuditoryBandwidth()))



################################################################################
# Hz generic function.                                                         #
# -- Hz maps a value on the ERB-scale to a frequency value, and is             #
#    parameterized by an AuditoryBandwidth object.                             #
################################################################################
if (! isGeneric('Hz'))
  setGeneric(
    name = 'Hz',
    def  = function(erb, auditoryBandwidth) standardGeneric('Hz'))
setMethod(
  f   = 'Hz',
  sig = c(erb = 'numeric', auditoryBandwidth = 'AuditoryBandwidth'),
  def = function(erb, auditoryBandwidth)
    (exp(erb / resolution(auditoryBandwidth)) - 1.0) * resolution(auditoryBandwidth) * minBandwidth(auditoryBandwidth))
setMethod(
  f   = 'Hz',
  sig = c(erb = 'numeric', auditoryBandwidth = 'missing'),
  def = function(erb) Hz(erb, AuditoryBandwidth()))
