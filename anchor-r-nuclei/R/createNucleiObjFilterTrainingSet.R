#
#  Copyright (C) 2016 ETH Zurich, University of Zurich, Owen Feehan
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

createNucleiObjFilterTrainingSet <- function( features, minSizeTrainingSet=10, minOverlap=0.9 ) {
  #
  # Takes features from the nucei:
  #   1. Adds labels for whether to DISCARD or KEEP
  #   2. Adds some additional features
  #   3. Remove unsuitable features for training
  #   4. (optionally) standardizes the features by converting to zscores
  #
  # Args:
  #   features: features
  #   standardize: if TRUE, each feature-column is converted into its z-score
  #   minSizeTrainingSet: the minimum number of rows in the training se
  #   minOverlap: a threshold on overlap, determining if an object is labelled DISCARD or KEEP
  #
  # Returns:
  #  list(
  #    featuresTraining    features for training
  #    label               labelling of the features
  #    features            all-features
  #  )
  #features$maxOverlapWithAllAnnotations = features$maxOverlapWithAllPositiveAnnotations + features$maxOverlapWithAllNegativeAnnotations;
  
  # We parition all our examples into two labelled-categories.
  #   0 - DISCARD
  #   1 - KEEP
  #
  # Let's remove any features with a deltaOverlap of less than 0.1.... these are UNSURE features
  
  # We want to keep anything which has an overlap>=0.9 with a specific positive annotation, the rest we label as unwanted
  combinedLabelVal = features$maxOverlapWithPositiveAnnotation - minOverlap;
  label = createLabelPositiveNegative( combinedLabelVal );
  
  # We calculate scaling information, before we remove other objects, as this is more accura  
  scaling = centerScaleFrame( features );
  
  # We remove features we don't want included in the traning
  featuresTraining = removeFeaturesForTraining(features);

  featuresTraining = removeFeaturesWithNA(featuresTraining);
  featuresTraining = removeFeaturesWithNAN(featuresTraining);
  featuresTraining = removeFeaturesWithZeroSd(featuresTraining);
  
  stopifnot( !has.nan(featuresTraining) );

  # If there's major imbalance we oversample from the under-represented class, so there are as many samples
  #  as the large class
  rebalanced = rebalanceLabelledFeatures( features, label, 3, additionalFeatures=featuresTraining );
  
  # We add a label
  featuresLabel = rebalanced$features;
  featuresLabel$label = rebalanced$label;
  featuresTraining = rebalanced$additionalFeatures;
  label = rebalanced$label;
  features = rebalanced$features;
  
  stopifnot( !has.nan(featuresTraining) );
  
  if( nrow(featuresTraining)==0 ) {
    stop( sprintf('There are %d features in the training set. At least %d are required.',0,minSizeTrainingSet) );
  }
  
  if( nrow(featuresTraining)<minSizeTrainingSet ) {
    stop( sprintf('There are %d features in the training set. At least %d are required.',nrow(features),minSizeTrainingSet) );
  }
  
  
  
  out = list(featuresTraining=featuresTraining,label=label,features=features,featuresLabel=featuresLabel,scaling=scaling);
  
  if( 'csvID' %in% colnames(features)) {
    out[['csvID']] = features$csvID;
  }
  
  class(out) = 'anchorTrainingSet';
  out;
}