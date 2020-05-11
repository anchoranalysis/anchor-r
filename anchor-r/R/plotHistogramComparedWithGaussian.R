#
#  Copyright (C) 2019 F. Hoffmann-La Roche Ltd
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

plotHistogramComparedWithGaussian <- function( title, featureVals, gaussianMean, gaussianSd, breaks=20, xlim=NULL ) {
  #
  # Plots a histogram of feature-values, with a fitted Gaussian (blue), and an explictly-defined Gaussian (red)
  #
  # Args:
  #   title: title of graph
  #   featureVals: feature-value
  #   gaussianMean: the mean of a Gaussian-curve
  #   gaussianSd: the std-dev of a Gaussian-curve
  #   breaks: like in hist()  
  #   xlim: if non-NULL, like in hist(), otherwise ignored  
  #
  # Returns:
  #   nothing
  #
  
  # http://www.statmethods.net/graphs/density.html
  h = hist(featureVals,main=title,xlab="",xlim=xlim,breaks=breaks);
  
  # Normal curve for existing data
  xfit<-seq(min(featureVals),max(featureVals),length=40);
  yfit<-dnorm(xfit,mean=mean(featureVals),sd=sd(featureVals));
  yfit <- yfit*diff(h$mids[1:2])*length(featureVals);
  lines(xfit, yfit, col="blue", lwd=4);
  
  
  yfitModel<-dnorm(xfit,mean=gaussianMean,sd=gaussianSd)
  yfitModel <- yfitModel*diff(h$mids[1:2])*length(featureVals);
  lines(xfit, yfitModel, col="red", lwd=2);
}


plotHistogramComparedWithGaussianLegend <- function() {
  #
  # Plots the legend for the plotHistogramComparedWithGaussian chart
  #
  # Returns:
  #  nothing
  #
  plot(1, type = "n", axes=FALSE, xlab="", ylab="")
  plot_colors <- c("blue","red")
  legend(x = "top",inset = 0,
         legend = c("Annot.", "Model"), 
         col=plot_colors, lwd=5, cex=2, horiz = FALSE)
}


plotHistogramComparedWithGaussianForEachGroup <- function( groups, listParams, listFeatures, featureName, numCols=4, addLegend=TRUE, titleFUN=NULL, breaks=20, xlim=range(breaks) ) {
  #
  # Runs plotHistogramComparedWithGaussian for each group
  #
  # Args:
  #   groups: a set of groups
  #   listParams: a list of params-object, where the key of each list item is the group-identifier.
  #               Within each params-object, a Gaussian can be constructed from featureName + '_fitted_normal_mean' and '_fitted_normal_sd'
  #   listFeatures: a list of feature-values, where the key of each list item is the group-identifier
  #   featureName: the feature to plot
  #   numCols: how many columns in the plot
  #   addLegend: if TRUE, a legend is added.
  #   titleFUN: if non-NULL, the function is used to generate a string-title from a group-identifier (the function takes the group-identifier as an argument)
  #   breaks: like in hist()
  #   xlim: if non-NULL, like in hist(), otherwise ignored
  #
  # Returns:
  #   nothing
  #
  
  numGroups = length(groups);
  
  # If we add a legend, it takes up the space of one additional plot
  if (addLegend) {
    numGroups = numGroups + 1;
  }
  
  numRows = ceiling(numGroups / numCols);
  
  #pdf(sprintf("figureOut/nucModel%s.pdf",featureName), height=4, width=8);
  
  par(mfrow=c(numRows,numCols),oma = c(0, 0, 3, 0),mar = c(3,3,3,1));
  
  for( set in groups ) {
    
    paramsModel = listParams[[set]]; # paramsFromXml(pathFolderModel, set);
    
    featureVals = listFeatures[[set]][[featureName]];
    
    featureVals <- na.omit(featureVals);
    
    # Determine title
    if (!is.null(titleFUN)) {
      title = titleFUN(set);
    } else {
      title = set;
    }
    
    
    featureMean = paste(featureName,'_fitted_normal_mean',sep='');
    featureSd = paste(featureName,'_fitted_normal_sd',sep='');
    
    gaussianMean = paramsModel[[featureMean]];
    gaussianSd = paramsModel[[featureSd]];
    
    p = plotHistogramComparedWithGaussian( shorten(set), featureVals, gaussianMean, guassianSd, breaks=breaks, xlim=xlim );
  }
  
  # Add Legend
  if (addLegend) {
    plotHistogramComparedWithGaussianLegend();
  }
  
  # Add an overall title of identifying which feature we are using
  mtext(featureName, outer = TRUE, cex = 1.5)
  
  par(mfrow=c(1,1));
  
  #dev.off();
}
