`is.number` <-
function(x) (is.numeric(x) || is.complex(x)) & !is.na(x)

