#' Performs a full interval optimization
#'
#' This function performs a full interval optimization (0-1 on each x variable).
#' It allows the creation of the data frame used for plotting.
#'
#' @param functions An array of functions
#' @param desirabilityModel A desirability overallD model
#' @param xCount The amount of x variables used in the functions
#' @param step The ammount of each increment in the optimization
#' @param plot Define is the data frame that can be used for the desirabilityPlot function will be create. Strongly affects performance
#' @return A list containg the data regarding the maximum desirability found
#' @export
mixtureOptim <- function(functions, desirabilityModel, xCount, step = 0.01, plot = T) {

  misturaRecursao <- function(functions, desirabilityModel, step, level, value, plot) {
    gap <- 1.0 - sum(value)
    value[level] = gap;
    if(level == length(value)) {
      #print(value)
      valores = rep(0.0, length(functions))
      for(i in 1:length(functions)) {
        valores[i] = functions[[i]](value)
      }
      tmp <- predict(desirabilityModel, as.data.frame(matrix(valores, nrow=1)))
      if(plot)
        grafico <<- rbind(grafico, c(value, tmp, valores))
      if(tmp > best) {
        best <<- tmp
        bestValues <<- value
      }
    } else {
      while(value[level] >= 0.0) {
        misturaRecursao(functions, desirabilityModel, step, level + 1, value, plot)
        value[level] = value[level] - step
      }
    }

  }

  best <- 0
  if(plot) {
    grafico <- data.frame()
  }
  bestValues <- rep(0, xCount)
  cat("Optimizing values.. 0 % ")
  if(xCount > 0) {
    vetor <- rep(0, xCount)
    count <- 0
    while(vetor[1] <= 1.0) {
      misturaRecursao(functions, desirabilityModel, step, 2, vetor, plot)
      vetor[1] = vetor[1] + step
      count = count + 1
      if(count %% 10 == 0) {
        cat(paste(count,"% "))
      } else {
        cat(".")
      }
    }
  }
  retorno <- list("maxDesirability" = best, "bestComposition" = bestValues)
  if(plot) {
    retorno$plotData <- grafico
  }
  return(retorno)
}
