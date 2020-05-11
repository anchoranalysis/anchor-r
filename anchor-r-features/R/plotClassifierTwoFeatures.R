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

plotClassifierTwoFeatures <- function( classifier, trainingSet, featureNameX, featureNameY ) {
  #
  # Plots two features against each other from the classification results
  #
  # The labels are shown as a red and blue (0 and 1) respectively
  #
  # Correct predictions are marked with a zero. Incorrect predictions have an X.
  #
  # Args:
  #   classifier:    output of a create classifier function (e.g. createNucleiMergeClassifier)
  #   featureNameX:  feature on X-axis
  #   featureNameY:  feature on Y-axis
  #
  
  # We adjust the labels so they match our original 0 and 1s
  labelsAdj = classifier$model$labels[classifier$label];
  
  colors = c('red','blue')[labelsAdj];
  pointType = c(4,1);
  cexSize = c(1,2);
  
  
  # If it's a SVM, we make the support-vectors larger
  if('svm' %in% class(classifier$model)) {
    cexVec = 1:nrow(trainingSet$featuresTraining) %in% classifier$model$index;
    cexVec = cexSize[ ifelse(cexVec,2,1) ];
  } else {
    cexVec = rep(1, nrow(trainingSet$featuresTraining));
  }
  
  pointTypeVec = c(4,1)[ ifelse(classifier$correct,2,1) ];
  plot( trainingSet$features[,featureNameX], trainingSet$features[,featureNameY], pch=pointTypeVec, col=colors, xlab=featureNameX, ylab=featureNameY, cex=cexVec );
  
  legend('topright', legend=c('false 0','true 0','false 1','true 1'), col=c('red','red','blue','blue'), pch=c(pointType[1],pointType[2],pointType[1],pointType[2]) );
}