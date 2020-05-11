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

# Creates labels MERGE (1) and DO_NOT_MERGE (0) from overlap features
.createLabelFromOverlap <- function( features ) {
  cominedLabelVal = pmin(features$span.deltaOverlap,features$pair.numTouchingVoxels - 10);
  
  # And create a label from the rest
  label = createLabelPositiveNegative( cominedLabelVal );
  
  # If there's a large change in the feature pairMin.ratioAddedVoxelsAfterClosing.4 we determine
  #  that although the merge covers more of the annotated-object, there is probably a hole in between
  #  so we don't consider it a desirable merge
  stopIfFeaturesAreMissing(features,'diffMax.ratioAddedVoxelsAfterClosing.4');
  
  label[ features$pair.numTouchingVoxelFacesOverMin > 0.4 ] = 1;
  label[ features$pair.numTouchingVoxelFacesOverMin < 0.05 ] = 0;
  
  label
}



createNucleiMergeTrainingSet <- function( features, standardize=TRUE, minSizeTrainingSet=10, cutoffCorrelation=0.9 ) {
  #
  # Takes features from the nucei:
  #   1. Adds labels for whether to MERGE (1) or DO_NOT_MERGE (0)
  #   2. Adds some additional features
  #   3. Remove unsuitable features for training
  #   4. (optionally) standardizes the features by converting to zscores
  #
  # Args:
  #   features: features
  #   standardize: if TRUE, each feature-column is converted into its z-score
  #
  # Returns:
  #  list(
  #    featuresTraining    features for training
  #    label               labelling of the features
  #    features            all-features,
  #    featuresLabel       cbind(features,label)
  #  )
  
  # We create some additional features
  features = createAdditionalOverlapFeatures(features);

  features = createAdditionalDiffFeatures(features);
  features = createAdditionalPairFeatures(features);
  
  features$merged.distScoreTo1 = abs( 1 - features$merged.score_maxSliceArea );

  # We calculate scaling information, before we remove other objects, as this is more accura  
  scaling = centerScaleFrame( features );
  
  # We parition all our examples into three labelled-categories.
  #   MERGE - clearly, these objects should be merged (1)
  #   DO NOT MERGE - do not merge these objects (0)
  #   UNSURE - it is not clear whether to merge or not.
  #
  # Let's remove any features with a deltaOverlap of less than 0.1.... these are UNSURE features
  features = removeObjectsWithAbsoluteValueLessThan( features, 'span.deltaOverlap', 0.1 );
  #features = removeObjectsWithinRange( features, 'pair.numTouchingVoxels', 10, 40 );
  features = removeObjectsWithinRange( features, 'first.numOverlapWithAnnotation', 2, 100000000000 );
  features = removeObjectsWithinRange( features, 'second.numOverlapWithAnnotation', 2, 100000000000 );
  features = removeObjectsWithinRange( features, 'merged.numOverlapWithPositiveAnnotation', 0, 0 );

  label = .createLabelFromOverlap( features );

  # We remove features we don't want included
  featuresTraining = removeFeaturesForTraining(features);
  
  # printFeatureNames(features)
  
  out = createTrainingSet(
    features,
    label,
    featuresTraining,
    scaling,
    cutoffCorrelation,
    minSizeTrainingSet,
    standardize,
    rebalance=TRUE
  );
  
  if( 'csvID' %in% colnames(features)) {
    out[['csvID']] = features$csvID;
  }
  
  stopifnot(nrow(out$featuresTraining)==length(out$label));
  
  out;
}



