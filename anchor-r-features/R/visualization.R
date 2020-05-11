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

displayFeaturesFor <- function( nameSet ) {
  indices = which(row.names(featuresTraining) %in% nameSet)
  featuresTraining[indices,] 
}


# Displays a particular feature, in order of its feature Values, with the label as a name
displayFeatureWithLabels <- function( featuresTraining, featureName ) {
  feat = featuresTraining[, featureName];
  indx = order(feat)
  
  feat = feat[indx]
  
  names(feat) = label[indx]
  print(feat);
}


# Displays two features, in order of its featureName1, and also shows the label and if it was correctly predicted
displayTwoFeaturesWithLabels <- function( trainingSet, featureName1, featureName2, correct ) {
  feat1 = trainingSet$featuresTraining[, featureName1];
  feat2 = trainingSet$featuresTraining[, featureName2];
  
  indx = order(feat1)
  
  out = data.frame(
    featureName1=feat1[indx],
    featureName2=feat2[indx],
    label=trainingSet$label[indx],
    correct=correct[indx]
  );
  
  # Use proper featureNames, but these are often long and hterefore annoying
  #names(out)[1:2] =  c(featureName1, featureName2)
  
  row.names(out) = row.names(trainingSet$featuresTraining)[indx]
  print(out);
}

# Limits for xlim or ylim, overriding if maxVal is non-NULL, otherwise taking the range
.limitsForPlot <- function( maxVal, dataVector ) {
  if (!is.null(maxVal)) {
    c(0,maxVal);
  } else {
    range( dataVector );
  }
}

# A trainingFeatures + label
.labelledFeatures <- function( trainingSet ) {
  data = trainingSet$featuresTraining
  data$label = trainingSet$label
  data
}

# Plots a SVM model (e1071) results on two features. Also prints the feature-values to the display ordered by featureNameX
plotSvm <- function( classifier, trainingSet, featureNameX, featureNameY, maxX=NULL, maxY=NULL ) {
  
  formula = as.formula( sprintf("%s ~ %s", featureNameY, featureNameX ) );
  
  displayFeatureWithLabels( trainingSet, featureNameX, featureNameY, classifier$correct );
  
  data = .labelledFeatures(trainingSet);
  
  plot(
    classifier$model,
    data=data,
    formula=formula,
    xlim=.limitsForPlot(maxX, data[,featureNameX]),
    ylim=.limitsForPlot(maxY, data[,featureNameY])    
  )
}