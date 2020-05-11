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

trainSVM_Radial <- function( data, label ) {
  #
  # Trains a SVM classifier (radial)
  #
  #
  # Args:
  #   features: feature-data for trainign
  #   label: a labeling for the features
  #
  # Returns:
  #   a classifier-model
  #
  stopifnot( class(label)=="factor" );
  
  # linear
  svm( label ~ ., data=data, kernel="radial", scale=FALSE );
}


#
# Trains a SVM classifier (linear) using the e1071 package
#
#
# Args:
#   cost: the cost-value (C) in the SVM training algorithm
#
# Returns:
#   a function that builds a classifier-model
#
trainSVM_Linear <- function(cost=1) {
  
  function( data, label ) {

    stopifnot( class(label)=="factor" );
    
    # linear
    svm( label ~ ., data=data, kernel="linear", scale=FALSE, cost=cost );
  }
}

predictSimple <- function( model, data, label ) {
  #
  #  Makes a prediction from the model on data using the predict() function
  #
  # Args:
  #   model: classifier
  #   data:  predictor-data
  #   label: ignored
  #
  # Returns:
  #   predictions as outputted by the predict() function
  #    
  predict( model, data );
}