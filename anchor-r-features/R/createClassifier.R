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

.calcCorrect <- function( predictions, label ) {
  correct = ifelse(predictions==label,1,0);
  stopifnot( length(predictions)==length(correct) );
  correct
}

.printClassificationStats <- function( label, predictions, featuresTraining ) {
  stopifnot( any(is.na(predictions))==FALSE );
  printRatioCorrectPredictions( label, predictions );
  printConfusionMatrix( label, predictions );
  printMislabelled( row.names(featuresTraining), label, predictions );
}


.applyCrossValidation <- function( trainingSet, trainFunc, predictFunc, label, features ) {
  cat('With cross-validation...\n');
  predictions = crossValidateClassifier( features, label, trainFunc, predictFunc );
  
  cat('Total cross-validation\n');
  .printClassificationStats( label, predictions, features )
  predictions
}


createClassifier <- function( trainingSet, trainFunc=trainLDA, predictFunc=predictSimple, crossValidate=TRUE ) {
  #
  # Creates a classifier for from trainingSet data
  #
  # Args:
  #   trainingSet: an object of class('anchorTrainingSet')
  #   trainFunc: a function for training the classifier, defaults to LDA
  #   predictFunc: a function for predicting from the classifier, defaults to predict
  #   crossValidation: if TRUE, a cross-validation procedure is applied for the training
  #
  # Returns:
  #   list(
  #      model:    a classifier-model (can be used with predict) for nuclei-merges
  #      predict:  predictions from cross-validation
  #      correct:  which predictions were correct, and which were not
  #   )
  
  cat('Without cross-validation...\n');
  stopifnot( !has.nan(trainingSet$featuresTraining) );

  # Train model and make predictions  
  model = trainFunc( trainingSet$featuresTraining, trainingSet$label );
  predictions <- predict(model,trainingSet$featuresTraining);
  
  .printClassificationStats( trainingSet$label, predictions, trainingSet$featuresTraining )
  
  # If we are in cross-validation mode, then we apply it
  # (sometimes this has already occurred in the trainer, and is redundant at this point)
  if (crossValidate) {
    predictions = .applyCrossValidation(
      trainingSet,
      trainFunc,
      predictFunc,
      trainingSet$label,
      trainingSet$featuresTraining
    )
  }

  list(
    model=model,
    predict=predictions,
    label=trainingSet$label,
    correct=.calcCorrect( predictions, trainingSet$label )
  );
}
