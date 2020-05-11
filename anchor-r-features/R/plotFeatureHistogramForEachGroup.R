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

plotFeatureHistogramForEachGroup <- function( features, numBins, featureName=NULL, objSet=NULL, minAxis=-1, maxAxis=-1, firstPageOnly=FALSE, maxPerPage=16 ) {
  #
  # Display a feature-histogram for each group alongside each other
  #
  #
  # Args:
  #   features: a data.frame of features (e.g. from readFeaturesCSV() )
  #   numBins: how many bins (for all the feature values, before being grouped)
  #   featureName: if non-NULL, a string indicating which feature to display
  #   objSet: if non-NULL, a string selecting an objSet
  #   minAxis: the minimum value for the x-axis (if -1, then its the minimum of all feature values). Doesn't affect binning.
  #   maxAxis: the maximum value for the x-axis (if -1, then its the minimum of all feature values). Doesn't affect binning.
  #   firstPageOnly: shows only the first-page of any multi-paged plot and then exists
  #   maxPerPage: max number of plots per page
  #
  
  featuresObjSet = features[features$objSetName==objSet,];
  vals = featuresObjSet[,c(featureName)];
  
  # We use this to calculate the breaks, as it also gets the midpoints nicely, and is apparently efficient
  breaksMid = hist(vals, numBins, plot = FALSE);
  
  # Bins up the feature-values
  valFactors = cut(vals, breaksMid$breaks);

  # a table, where each column is a group, and each row is a bin (from valFactors), and each cell has a count
  countsAll = table(valFactors,featuresObjSet$group);
  
  groups = colnames(countsAll);
  
  # How many groups, and how we divide them into rows and columns?
  numGroups = length(groups);
  numCols = floor(sqrt(numGroups));
  numRows = ceiling(numGroups / numCols);

  # Determine axis min and max  
  if (minAxis==-1) {
    minAxis = min(vals);  
  }
  
  if (maxAxis==-1) {
    maxAxis = max(vals);  
  }

  # Record a plot for each group  
  mid = breaksMid$mid;
  plots = vector(mode="list", length=length(numGroups));
  for( i in 1:numGroups ) {
    
    count = countsAll[,i];

    freqTable = data.frame(mid, count );
    p = plotHistogramFromFrequencyTable( freqTable, maxAxis, minAxis, groups[i], featureName );
    
    plots[[i]] = p;
  }
  
  # Displays the plots alongside each other, perhaps one more than one page
  multiplotPaged( plotlist=plots, cols = 4, maxPerPage=maxPerPage, firstPageOnly=firstPageOnly );
}