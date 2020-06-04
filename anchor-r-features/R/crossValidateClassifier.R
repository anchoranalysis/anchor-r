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

crossValidateClassifier <- function( features, label, createClassifierFunc, predictFunc=predictSimple ) {
  #
  # Perform 10 fold cross validation
  #
  #
  # Args:
  #   features:  features to be cross-validated
  #   label:     labelling for features
  #   createClassifierFunc:  a function of form function( data, label ) that trains a classifier, that can later be used with predict()
  #   predictFunc: a function for predicting the result, defaults to predict()
  #
  # Returns:
  #   list with the predictions and the (re-ordred) labeleling (ground truth)
  #
  indexesRandomlyMapped = sample(1:nrow(features));
  
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(features)),breaks=10,labels=FALSE)

  allPredictions = label;
  for(i in 1:10) {
    # Split data into training and test
    testIndexes = indexesRandomlyMapped[which(folds==i,arr.ind=TRUE)];
    testData = features[testIndexes, ]
    trainData = features[-testIndexes, ]

    labelTest = label[testIndexes];
    labelTrain = label[-testIndexes];

    # Train the classifier on the training data
    classifier = createClassifierFunc( trainData, labelTrain );
    
    # Make predictions on the test data
    predictions = predictFunc(classifier, testData, label);
    
    printRatioCorrectPredictions( labelTest, predictions );

    allPredictions[testIndexes] = predictions;
  }
  
  stopifnot( any(is.na(allPredictions))==FALSE );

  allPredictions
}