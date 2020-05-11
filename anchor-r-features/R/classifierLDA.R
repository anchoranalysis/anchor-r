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

# Linear Discriminant analysis
trainLDA <- function( features, label ) {
  #
  # Trains a LDA classifier
  #
  # A custom-object class 'anchorlda' is used, as the predict()
  #  behaviour needs to be overridden to return only the classification
  #
  # Args:
  #   features: feature-data for training
  #   label: a labeling for the features
  #
  # Returns:
  #   a classifier-model
  #
  stopifnot( !has.nan(features) );
  
  model = lda( label ~ ., data=features, CV=FALSE, prior=c(0.5,0.5) );
  out = list(submodel=model);
  class(out) = 'anchorlda';
  out;
}


explicitPredict.anchorlda <- function( model, data ) {
  #
  # Does the same as predict.anchorlda (assumes the model is a binary classifier with prior (0.5,0.5))
  #
  # See:
  #   http://stats.stackexchange.com/questions/15983/lda-cutoff-decision-boundary-value
  #
  # Args:
  #   model: model of class anchorlda
  #   data: feature-table
  #
  # Returns:
  #   predicted-label (1 and 0)
  #
  ldaCoef = coef(model);
  featureNames = rownames(ldaCoef);
  featureCoefficients = ldaCoef[,1];
  
  matM = as.matrix(trainingSet$features[,featureNames]) %*% featureCoefficients;
  thrshld = sum( featureCoefficients * model$submodel$means[2,] + featureCoefficients * model$submodel$means[1,])/2;
  ifelse(matM >= thrshld, 1, 0);
}


# START delegate methods

predict.anchorlda <- function( model, data ) {
  predict(model$submodel, data)$class;
}

coef.anchorlda <- function( model, data ) {
  coef(model$submodel);
}

# END delegate methods