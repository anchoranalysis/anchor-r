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

plotFeatureHistogram <- function( features, numBins, featureName=NULL, stackFeatureName=NULL, objSet=NULL, minAxis=-Inf, maxAxis=+Inf, extraFun=NULL, binWithinRange=TRUE ) {
  #
  # Display a feature-histogram for each group alongside each other
  #
  #
  # Args:
  #   features: a data.frame of features (e.g. from readFeaturesCSV() )
  #   numBins: how many bins (for all the feature values, before being grouped)
  #   featureName: if non-NULL, a string indicating which feature to display
  #   stackFeatureName: if non-NULL, a string indicating which feature to use to create 'stacks' in the histogram
  #   objSet: if non-NULL, a string selecting an objSet
  #   minAxis: the minimum value for the x-axis (if -Inf, then its the minimum of all feature values).
  #   maxAxis: the maximum value for the x-axis (if +Inf, then its the minimum of all feature values).
  #   binWithinRange: if TRUE, the binning only occurs for values within the range (minAxis,maxAxis). if FALSE, binning occurs on all the samples.
  #
  
  if (!is.null(objSet)) {
    features = features[features$objSetName==objSet,];
  }
  
  if (is.null(featureName)) {
    vals = features;  # Assume it's a vector of values
  } else {
    vals = features[,c(featureName)];
  }
  
  # If we are only concerned about our range, we trim the data-values to only lie within the range
  if (binWithinRange==TRUE) {
    indicesToKeep = vals>=minAxis & vals <= maxAxis;
    vals=vals[indicesToKeep];
    features=features[indicesToKeep,];
  }
  
  if (!is.null(stackFeatureName)) {
    fill = factor( features[,c(stackFeatureName)] );
  }
  
  # We use this to calculate the breaks, as it also gets the midpoints nicely, and is apparently efficient
  breaksMid = hist(vals, numBins, plot = FALSE);
  
  # Bins up the feature-values
  valFactors = cut(vals, breaksMid$breaks);

  mid = breaksMid$mid;
  
  # a table, where each column is a group, and each row is a bin (from valFactors), and each cell has a count
  # if stacked we also include the fill factor
  if (!is.null(stackFeatureName)) {
    count = table(valFactors,fill);
    fillID = rep( levels(fill), each=length(mid) );
    freqTable = data.frame(mid, as.vector(count), fillID );
  } else {
    count = table(valFactors);
    freqTable = data.frame(mid, as.vector(count) );
  }
  
  # Determine axis min and max  
  if (minAxis==-Inf) {
    minAxis = min(vals);  
  }
  
  if (maxAxis==+Inf) {
    maxAxis = max(vals);  
  }
  
  if (!is.null(stackFeatureName)) {
    p = plotHistogramFromFrequencyTableStacked( freqTable, maxAxis, minAxis, featureName, featureName, extraFun=extraFun);
  } else {
    # Create non-stacked
    p = plotHistogramFromFrequencyTable( freqTable, maxAxis, minAxis, featureName, featureName, extraFun=extraFun);
  }
  
  print(p);
}