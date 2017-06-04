weekdayNumeric <- function(x) {
   (as.numeric(as.Date(x, origin="1970-01-01")) - 4) %%7 + 1
  }


#I think this one could be sped up, the main bottlemneck
createLagVar <- function(input, lag_freq) {
  out <- data.frame(matrix(NA, nrow = nrow(input), ncol = 1))
  for (i in max(1, 1+lag_freq):min(nrow(input), nrow(input) + lag_freq)) {
    out[i,]=input[i-lag_freq, ]
  }
  out
}


#the create lags seem redundant
# createLagMaxVar <- function(input, lag_freq) {
# #only works for 1column series
#   out <- data.frame(matrix(NA, nrow = nrow(input), ncol = 1))
#   for (i in max(1, 1+lag_freq):min(nrow(input), nrow(input) + lag_freq)) {
#     out[i,]=max(input[i:i-lag_freq, ])
#   }
#   out
# }
# 
# createLagMinVar <- function(input, lag_freq) {
#   #only works for 1column series
#   out <- data.frame(matrix(NA, nrow = nrow(input), ncol = 1))
#   for (i in max(1, 1+lag_freq):min(nrow(input), nrow(input) + lag_freq)) {
#     out[i,]=min(input[i-lag_freq:i, ])
#   }
#   out
# }



minPeriod <- function(input, lag_freq) {
  #taken based on close value
  out <- data.frame(matrix(NA, nrow = nrow(input), ncol = 1))
  if (lag_freq<0) {
    for (i in max(1, 1+lag_freq):min(nrow(input), nrow(input) + lag_freq)) {
      out[i,]=min(input[(i+1):(i-lag_freq), ])
    }
  }
  else {
    for (i in max(1, 1+lag_freq):min(nrow(input), nrow(input) + lag_freq)) {
      out[i,]=min(input[(i):(i-lag_freq), ])
    }
  }
  out
}

maxPeriod <- function(input, lag_freq) {
  out <- data.frame(matrix(NA, nrow = nrow(input), ncol = 1))
  if (lag_freq<0) {
    for (i in max(1, 1+lag_freq):min(nrow(input), nrow(input) + lag_freq)) {
      out[i,]=max(input[(i+1):(i-lag_freq), ])
    }
  }
  else {
    for (i in max(1, 1+lag_freq):min(nrow(input), nrow(input) + lag_freq)) {
      out[i,]=max(input[(i):(i-lag_freq), ])
    }
  }
  out
}


# last observation moved forward
# replaces all NA values with last non-NA values
na.lomf <- function(x) {
  
  na.lomf.0 <- function(x) {
    non.na.idx <- which(!is.na(x))
    if (is.na(x[1L])) {
      non.na.idx <- c(1L, non.na.idx)
    }
    rep.int(x[non.na.idx], diff(c(non.na.idx, length(x) + 1L)))
  }
  
  dim.len <- length(dim(x))
  
  if (dim.len == 0L) {
    na.lomf.0(x)
  } else {
    apply(x, dim.len, na.lomf.0)
  }
}


cleanMem <- function(n=20) { for (i in 1:n) gc() }
