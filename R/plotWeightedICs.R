### Plot
#' Plot weighted model comparisons
#'
#' @description Takes a matrix/array/data frame that contains model probabilities
#' for each model (columns) and participant (rows) (which can be created made using the "weightedICs" function) and turns it into a stacked bar
#' plot.
#' @param ICweights A numeric data frame/matrix/array that contains the model probabilities of each participant (rows) and each model (column).
#' @param main Title of plot.
#' @param xlab x axis label.
#' @param ylab y axis label.
#' @param groupNames Character vector of length 2 describing the names of the two groups of models (to be shown on the plot legend).
#' @param inset Determines the position of the plot legend.
#' @param colours,seed Character vector of base r colours. Default generates colours for each variable randomly,  You can also try different numeric values for "seed" to try out different random colour combinations.
#' @param cex Controls the size of the plot legend.
#'
#' @return A stacked bar plot.
#' @export
#'
#' @examples plotWeightedICs(weightedICs(exampleData))
#' @examples plotWeightedICs(weightedICs(exampleData), colours = (c("darkblue","darkgreen","lightgreen","lightblue")))
#' @examples plotWeightedICs(weightedICs(exampleData), cex = 1, colours = (c("darkblue","darkgreen","lightgreen","lightblue")))
#'
plotWeightedICs = function(ICweights,
                           main = "Weighted Model Comparison",
                           ylab = "Probability",
                           xlab = "Participant",
                           colours = "random",
                           cex = .5,
                           seed = FALSE,
                           inset = c(-0.5,-.34)) {
  nModels = length(ICweights[1,])
  nSubj = length(ICweights[, 1])

  if (is.numeric(seed)) {
    set.seed(seed) # keep any random colours that are generated constant
  }

  set.seed(seed)

  # choose colours for each model in the barplot
  if (colours == "random") {
    colours = sample(colors(), size = nModels, replace = FALSE) # select random colours if user does not want any particular colours
  } else {
    colours = colours # must be a vector of valid base r colours of length nModels
    if (length(colours) != nModels) {
      stop("Colours must be a vector of the same length as the number of models")
    } else if (is.vector(colours) == FALSE) {
      stop("Colours must be a vector")
    }
  }

  plot(
    x = 100,
    y = 100,
    xlim = c(0, nSubj+0.5),
    ylim = c(0, 1),
    xlab = "",
    ylab = "",
    main = "",
    xaxt = "n",
    yaxt = "n"
  )

  for (i in 1:nSubj) {
    use.i = i
    sumThing = 0
    for (j in 1:nModels) {
      col = colours[j] # Purple
      rect(i-0.5,sumThing,i+0.5,sumThing+ICweights[use.i,j], border = col, col = col)
      sumThing = sumThing + ICweights[use.i, j]
    }
  }

  title(main = main, xlab = xlab, ylab = ylab, line = 0.2)

  axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
  axis(side=1, at=seq(0,nSubj,nSubj), labels=seq(0,nSubj,nSubj), cex.axis=1.5)

  par(xpd=TRUE) # lets plot legend be drawn outside of plot area

  legend("bottom",
         legend = colnames(ICweights),
         col = colours,
         pch = 15,
         #horiz = T,
         cex = cex,
         inset=inset,
         ncol = 4)

}


# Other things for  this package:
# 1. number of participants best fit by each model
# 2. overall percent probability of each model collapsed across participants + plot for that?
