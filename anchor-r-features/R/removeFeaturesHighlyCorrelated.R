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

removeFeaturesHighlyCorrelated <- function( features, cutoff=0.9, verbose=FALSE, maxFeatureNamesDisplay=10 ) {
  # Removes highly-correlated from a feature-table, so that only feature is left for each group of highly-correlated
  #   features
  #
  # Uses the caret::findCorrelation() function
  #
  # Args:
  #   features: feature-table
  #   cutoff: minimum cutoff value, above which features are considered highly-correlated
  #   verbose: a boolean for printing the details
  #   maxFeatureNamesDisplay: the maximum-number of feature-names that will be printed to the screen, after which '[and more...]' is appended
  #
  # Returns:
  #   a feature-table without the removed columns
  c = cor(features, use="p")
  featureIndices = findCorrelation(c, cutoff=cutoff, verbose=verbose);
  
  if (length(featureIndices)>0) {
    cat( sprintf('Removing %d features that are highly-correlated with others:\n', length(featureIndices) ) );
    featureNames = colnames(features)[featureIndices];
    if (length(featureIndices)<=maxFeatureNamesDisplay) {
      print( featureNames  );
    } else {
      print( c(featureNames[1:maxFeatureNamesDisplay],'[and more...]') );
    }
  }
  
  if (length(featureIndices)>0) {
    out = features[,-featureIndices,drop=FALSE];
    stopifnot( length(findCorrelation(cor(out, use="p"),cutoff=cutoff))==0 );
  } else {
    out = features;
  }
  out;
}