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


.removeUnhelpfulFeatures <- function(features, cutoffCorrelation) {
  features = features %>%
    removeNonNumericFeatures() %>%
    removeFeaturesWithNA %>% 
    removeFeaturesWithNAN %>% 
    removeFeaturesWithZeroSd
  
  removeFeaturesHighlyCorrelated( features, cutoff=cutoffCorrelation );
}

.addLabelToFeatures <- function( features, label ) {
  featuresLabel = features;
  featuresLabel$label = features$label;
  featuresLabel
}

.checkTrainingFeatures <- function( featuresTraining, minSizeTrainingSet ) {
  
  stopifnot( !has.nan(featuresTraining) );

  if( nrow(featuresTraining)==0 ) {
    stop( sprintf('There are %d objects left in the training set. At least %d are required.',0,minSizeTrainingSet) );
  }
  
  if( nrow(featuresTraining)<minSizeTrainingSet ) {
    stop( sprintf('There are %d objects left in the training set. At least %d are required.',nrow(featuresTraining),minSizeTrainingSet) );
  }
  
  print( sprintf('There are %d features left in the training set.',ncol(featuresTraining) ) );
  
  featuresTraining
}

#
# Creates AnchorTrainingSet, a form needed for other training functions
#
# Args:
#   features: all feature-values including ids etc.
#   rebalance: if TRUE, the smaller class is upsampled to be as large as the bigger class
#   label: labels associated with the features
#   featuresTraining: a subset of feature-values that will be used for training
#   cutoffCorrelation: threshold above which features will be removed if sufficiently correlated with others
#
# Returns (after maybe rebalancing):
#  list(
#    featuresTraining    feature-values for training
#    label               labelling of the feature-values 1 or 0
#    features            all-features,
#    featuresLabel       cbind(features,label)
#    minSizeTrainingSet   minimum number of rows needed in training set
#    pp: preprocessing routine for data (to include center and scale). if NULL it is calculated from the training data
#    rebalance: if TRUE, over-samples the minority class to each parity
#  )
#
createTrainingSet <- function(
  features,
  label,
  featuresTraining,
  cutoffCorrelation=0.9,
  minSizeTrainingSet=10,
  pp=NULL,
  rebalance=TRUE
) {

  stopifnot( nrow(features)==nrow(label) );
  stopifnot( nrow(features)==nrow(featuresTraining) );
  
  # If necessary, create pre-processing routine
  if (is.null(pp)) {
    pp = preProcess(featuresTraining, method=c("center", "scale"));
  }
  
  # Apply pre-processing routine
  featuresTraining = predict(pp, featuresTraining);
  
  
  # If there's major imbalance we oversample from the under-represented class, so there are as many samples
  #  as the large class
  if (rebalance) {
    list[features, label, featuresTraining] = rebalanceLabelledFeatures(
      features,
      label,
      3,
      additionalFeatures=featuresTraining
    );
  }

  
  # Removing features which won't help our classifier
  featuresTraining = .removeUnhelpfulFeatures(featuresTraining, cutoffCorrelation);
  
  .checkTrainingFeatures(featuresTraining,minSizeTrainingSet )
  
  out = list(
    featuresTraining=featuresTraining,
    label=label,
    features=features,
    featuresLabel=.addLabelToFeatures(features,label),
    scaling=data.frame(mean=pp$mean,scale=pp$std),   # The mean-and-scale from the preprocessing
    pp=pp   # Caret preprocessing routine, making it easy to apply again to other datasets
  );
  class(out) = 'anchorTrainingSet';
  out
}