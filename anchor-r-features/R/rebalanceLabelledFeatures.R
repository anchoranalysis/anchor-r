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


# if necessary, we also oversample the additional-features  
.maybeOverSampleAdditional <- function(additionalFeatures,label,labelMajority,labelMinority, oversampleIndexes) {
  if (!is.null(additionalFeatures)) {
    majorityRows = additionalFeatures[label==labelMajority,];
    minorityRows = additionalFeatures[label==labelMinority,];
    return( rbind( majorityRows, minorityRows[oversampleIndexes,] ) );
  } else {
    return(NULL);
  }
}

# After figuring out a minority/majority labels, and how many we would like of each, we oversample
.doOversampling <- function(features, label,labelMajority,labelMinority, numMajority, numMinority, additionalFeatures) {
  
  majorityRows = features[label==labelMajority,];
  minorityRows = features[label==labelMinority,];
  oversampleIndexes = sample(numMinority,numMajority,replace=TRUE);
  
  # We combine our majorityClass with oversampled data
  featuresOut = rbind( majorityRows, minorityRows[oversampleIndexes,] )
  labelOut = factor( c( rep(labelMajority, numMajority ), rep(labelMinority, numMajority) ) );
  
  additionalFeaturesOut = .maybeOverSampleAdditional(additionalFeatures,label,labelMajority,labelMinority, oversampleIndexes)
  
  list(features=featuresOut,label=labelOut,additionalFeatures=additionalFeaturesOut);  
}


rebalanceLabelledFeatures <- function( features, label, minimumImbalanceRatio=0.0, additionalFeatures=NULL ) {
  #
  # Takes a labelled data.frame of features, and corrects for imbalanced labelled data
  #
  # Args:
  #  features: features
  #  label: binary-labelling (0 and 1)  
  #  minimumImbalanceRatio: only re-balance if the imbalance-ratio is greater than this threshold (imbalance ratio = numMajority/numMinority)
  #  additionalFeatures: if non-NULL, an additional feature-table with the same number of rows as features. Rebalanced in the same way and order.
  #
  # Returns:
  #  list(
  #    features: rebalanced-features
  #    label: rebalanced-label (factors),
  #    additionalFeatures: rebalanced-additionalFeatures (if additionalFeatures is non-null) or NULL
  #  )
  #
  
  if (!is.null(additionalFeatures)) {
    stopifnot( nrow(features)==nrow(additionalFeatures) );
  }
  
  stopifnot( !is.null(label) );
  
  numZero = sum(label==0);
  numOne = sum(label==1);

  # Determine which label is undersampled
  if (numOne>numZero) {
    labelMajority = 1;
    labelMinority = 0;
    numMajority = numOne;
    numMinority = numZero;
  } else {
    labelMajority = 0;
    labelMinority = 1;
    numMajority = numZero;
    numMinority = numOne;
  }

  imbalanceRatio = numMajority/numMinority
  
  # Exit early, if our imbalanceRatio is too low
  if (imbalanceRatio<minimumImbalanceRatio) {
    list(features=features,label=label,additionalFeatures=additionalFeatures);
  } else {
    .doOversampling(features, label,labelMajority, labelMinority, numMajority, numMinority, additionalFeatures)  
  }
}
