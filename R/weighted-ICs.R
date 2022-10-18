# *** weighted model probabilities ***
library(here)
library(roxygen2)


#' Get the weighted probabilities of different models for each participant
#'
#' @description Takes the raw model comparison information criterion values (e.g., AIC, BIC, LOOIC)
#' for each model (columns) and participant (rows) and then converts them into a relitive probability
#' of that model being the best performing model according to that model comparison criterion.
#'
#' @param ICs a numeric matrix/array/data frame containing the raw model comparison criterion values (e.g., AIC, BIC, LOOIC) for each model (columns) and participant (rows).
#'
#' @return A matrix of probabilities corresponding to each participant (rows) and model (columns). To plot this output, see ?plotWeightedICs() and ?compareMM-ICs().
#' @export
#'
#' @examples
#' weightedICs(exampleData)

weightedICs = function(ICs){

  getWeights=function(x) {
    useX = x*(-0.5) # transform model criterion to a chi square distribution
    if (mean(is.na(useX)) == 1) {
      return(NA)
    }
    maxLogDens=max(useX) # calculate the maximum log density
    if (maxLogDens > 700) {
      densTransform=maxLogDens-700
      useX=useX-densTransform
    } else if (maxLogDens < -710) {
      densTransform=maxLogDens-700
      useX=useX-densTransform
    } else {
      densTransform=0
    }
    exp(useX)/sum(exp(useX)) # convert to relative probabilities
  }

  nSubj = length(ICs[,1])
  nModels = length(ICs[1,])
  ICweights=array(NA,c(nSubj,nModels))
  for (s in 1:nSubj) {
    ICweights[s,]= getWeights(as.matrix(ICs[s,]))
  }
  colnames(ICweights) = colnames(ICs)
  return(ICweights) #' @returns a matrix of probabilities
}


