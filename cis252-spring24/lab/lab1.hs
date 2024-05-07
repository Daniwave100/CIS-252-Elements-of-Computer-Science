-- Daniel Canhedo (docanhed@syr.edu)

-- Convert celsius temp to Fahrenheit
celsiusToFahr :: Float -> Float
celsiusToFahr c = c*9/5 + 32

-- Is Celsius temperature greater than 80F?
isTooHot :: Float -> Bool
isTooHot t = celsiusToFahr t > 80 