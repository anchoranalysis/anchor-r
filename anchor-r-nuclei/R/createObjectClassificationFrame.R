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

createObjectClassificationFrame <- function( features, classifier, pairwise=FALSE ) {
  #
  # Creates a data.frame describing the classification of pairs of objects, containing:
  #   1. an arbitrary point for each object that is guaranteed to be inside the object-mask
  #   2. the label for each object-pair
  #   3. the prediction for each object-pair
  #   4. whether the prediction matches the label (correct)
  #
  #  Expects the following features to be present to uniquely identify objects
  #   image
  #   group
  #   unique_pixel_in_object
  #
  # Args:
  #   features: feature-table
  #   classifier: output from createNucleiMergeClassifier
  #   pairwise: if TRUE, looks for pairwise-features (first.numVoxels second.numVoxels etc.). Otherwise looks for features as is (numVoxels etc.)
  #
  # Returns:
  #   data-frame
  stopifnot(nrow(features)==length(classifier$label));
  
  # Let's make a CSV file that describes the objects for anchor
  colNames = c('image','group','unique_pixel_in_object')

  stopIfFeaturesAreMissing( features, colNames );
  
  # Correct decisions are 0
  # Negative-labels that have been marked as positives (false-positives) ==1
  # Positive-labels that have been marked as negatives (false-negatives) ==2

  incorrectCode = ifelse(classifier$label==1,2,1);
  incorrectCode[classifier$correct==1] = 0;
  
  stopifnot( length(classifier$correct)==length(classifier$predict) );
    
  out = cbind(features[,colNames],classifier$label,classifier$predict,classifier$correct,incorrectCode);
  
  colNames = c(colNames,'label','predict','correct','incorrectCode');
  colnames(out) = colNames;
  out;
}
