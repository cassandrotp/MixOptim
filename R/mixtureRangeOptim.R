#' Performs a specific range optimization
#'
#' This function performs a optimization testing within an interval defined
#' by the user using alpha values for each middle point. It allows the
#' generation of the data frame for plotting.
#'
#' @param functions An array of functions
#' @param desirabilityModel A desirability overallD model
#' @param midPoints An array with the references (mid-points) for the optimization
#' @param alpha Defines the range of the seach, as startPoint +- alpha for each x value
#' @param step The ammount of each increment in the optimization
#' @param plot Define is the data frame that can be used for the desirabilityPlot function will be create. Strongly affects performance
#' @return A list containg the data regarding the maximum desirability found
#' @export
mixtureRangeOptim <- function(functions, desirabilityModel, midPoints, alpha, step = 0.01, plot = T) {

    misturaRecursao <- function(functions, desirabilityModel, step, level, value, plot) {
    value[level] = midPoints[level] + alpha[level]
    if(level == length(value)) {
      #print(value)
      value[level] = 0.0;
      gap <- 1.0 - sum(value)
      if(gap >= midPoints[level] - alpha[level]) {
        value[level] = gap
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
      }
    } else {
      while(value[level] >= midPoints[level] - alpha[level]) {
        misturaRecursao(functions, desirabilityModel, step, level + 1, value, plot)
        value[level] = value[level] - step
      }
    }

  }

  best <- 0
  if(plot) {
    grafico <- data.frame()
  }
  bestValues <- midPoints
  cat("Optimizing values.. 0 %.. ")
  if(length(midPoints) > 0) {
    vetor <- midPoints - alpha
    #print(vetor)
    count <- 0
    steps = (alpha[1]*2)/(step)
    while(vetor[1] <= midPoints[1] + alpha[1]) {
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
  if(plot) {
    retorno$plotData <- grafico
  }
  return(retorno)
}
