#' Performs a restrict interval optimization
#'
#' This function performs a optimization testing within an interval defined
#' by the user using starting point and an alpha value. Since it is designed
#' for more accurate searching, it does not allow the generation of the
#' data frame for plotting.
#'
#' @param functions An array of functions
#' @param desirabilityModel A desirability overallD model
#' @param startPoint An array with the references (mid-points) for the optimization
#' @param step The ammount of each increment in the optimization
#' @param alpha Defines the range of the seach, as startPoint +- alpha for each x value
#' @return A list containg the data regarding the maximum desirability found
#' @export
mixtureFineOptim <- function(functions, desirabilityModel, startPoint, step = 0.001, alpha = 0.02) {

  misturaRecursao <- function(functions, desirabilityModel, step, level, value, plot) {
    value[level] = startPoint[level] + alpha
    if(level == length(value)) {
      value[level] = 0.0;
      gap <- 1.0 - sum(value)
      if(gap >= 0) {
        value[level] = gap
        #print(value)
        valores = rep(0.0, length(functions))
        for(i in 1:length(functions)) {
          valores[i] = functions[[i]](value)
        }
        tmp <- predict(desirabilityModel, as.data.frame(matrix(valores, nrow=1)))
        if(tmp > best) {
          best <<- tmp
          bestValues <<- value
        }
      }
    } else {
      while(value[level] >= startPoint[level] - alpha) {
        misturaRecursao(functions, desirabilityModel, step, level + 1, value, plot)
        value[level] = value[level] - step
      }
    }

  }

  best <- 0
  bestValues <- startPoint
  cat("Optimizing values.. 0 %.. ")
  if(length(startPoint) > 0) {
    vetor <- startPoint - alpha;
    count <- 0
    steps = (alpha*2)/(step)
    while(vetor[1] <= startPoint[1] + alpha) {
      misturaRecursao(functions, desirabilityModel, step, 2, vetor, plot)
      vetor[1] = vetor[1] + step
      count = count + 1
      perc = (count * 100.0) / steps
      if(perc %% 10 == 0) {
        cat(paste(perc,"%.. "))
      } else {
        #cat(".")
      }
    }
  }
  retorno <- list("maxDesirability" = best, "bestComposition" = bestValues)
  return(retorno)
}
