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

.setupCaretTrain <- function() {
  trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
  set.seed(3233)
  trctrl
}

.addLabelToData <- function( data, label ) {
  dataWithLabel = data;
  dataWithLabel$label = label;
  dataWithLabel
}

.trainCaret <- function( data, label, tuneGrid ) {
  stopifnot( class(label)=="factor" );
  
  trctrl <- .setupCaretTrain()
  
  train(label ~., data=.addLabelToData(data,label), method = "svmLinear", trControl=trctrl, tuneGrid=tuneGrid, tuneLength = 10)
}

trainSVM_Linear_Caret <- function( data, label ) {
  #
  # Trains a SVM classifier (linear) using the Caret Package
  #
  #
  # Args:
  #   features: feature-data for training
  #   label: a labeling for the features
  #
  # Returns:
  #   a classifier-model
  #
  .trainCaret( data, label, NULL)
}

trainSVM_Linear_Caret_Grid <- function( data, label ) {
  #
  # Trains a SVM classifier (linear)
  #
  #
  # Args:
  #   features: feature-data for training
  #   label: a labeling for the features
  #
  # Returns:
  #   a classifier-model
  #
  grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 5, 10, 50, 100, 500, 1000, 5000, 10000))
  .trainCaret( data, label, grid)
}