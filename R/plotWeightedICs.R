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
                           colours = c("#800000" ,  "#0000FF", "#800080", "#FFC0CB","#00FF00", "#00FFFF", "#FF00FF", "#FFFF00", "#FFA500", "#008080", "#FF0000", "#00FF00", "#4B0082", "#000080", "#808000", "#00FFFF", "#C0C0C0", "#808080", "#A52A2A", "#FFD700"),
                           cex = .5,
                           position = "bottom",
                           seed = FALSE,
                           inset = c(-0.5,-.1)) {

  if(is.vector(ICweights)){
    nSubj = 1
    nModels = length(ICweights)
  } else{
    nSubj = length(ICweights[, 1])
    nModels = length(ICweights[1,])
  }

  if (is.numeric(seed)) {
    set.seed(seed) # keep any random colours that are generated constant
  }

  set.seed(seed)

  # choose colours for each model in the barplot
  if (length(colours) < nModels) {
    colours = sample(colors(), size = nModels, replace = FALSE) # select random colours if user does not want any particular colours
    print("More colours than the default colour pallette (20). Colours selected randomly, set the seed argument to have a consistent colour scheme or choose your own vector of colours.")
  } else {
    colours = colours[1:nModels]
  }



  if (is.vector(ICweights)){

    plot(
      x = 100,
      y = 100,
      xlim = c(0, nSubj),
      ylim = c(0, 1),
      xlab = "",
      ylab = "",
      main = "",
      xaxt = "n",
      yaxt = "n"
    )

    sumThing = 0
    i = 1
    use.i = 1
    for (j in 1:nModels) {
      col = colours[j]
      rect(i-1,sumThing,i,sumThing+ICweights[j], border = col, col = col)
      sumThing = sumThing + ICweights[j]
    }
    legend(position,
           legend = names(ICweights),
           col = colours,
           pch = 15,
           #horiz = T,
           cex = cex,
           inset=inset,
           ncol = 4)
  } else{

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
      legend("bottom",
             legend = colnames(ICweights),
             col = colours,
             pch = 15,
             #horiz = T,
             cex = cex,
             inset=inset,
             ncol = 4)
    }

  }



  title(main = main, xlab = xlab, ylab = ylab, line = 2)

  axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
  axis(side=1, at=seq(0,nSubj,nSubj), labels=seq(0,nSubj,nSubj), cex.axis=1.5)

  par(xpd=TRUE) # lets plot legend be drawn outside of plot area



}



# Other things for  this package:
# 1. number of participants best fit by each model
# 2. overall percent probability of each model collapsed across participants + plot for that?

