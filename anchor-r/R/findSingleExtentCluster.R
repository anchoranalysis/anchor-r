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

findSingleExtentCluster <- function(features, featureNamesCluster, featureNameMin, maxG=2, showPlot=FALSE ) {
  #
  # Builds a model of the approximate extent of an ellipsoidal object, from a set of features
  #   representing 1 or more clumped objects (and maybe outliers)
  #
  #  This is done by Gaussian-Mixture-Modelling clustering with at least 2 items, and then searching
  #    for the cluster with maximum
  #
  # Args:
  #   features: a data.frame of features (e.g. from readFeaturesCSV() )
  #   featureNamesCluster: the names of the features that are used in the clustering-step
  #   featureNameMin: the mean of this feature is calculated for each cluster. The minimum-value determines the single-item.
  #
  # Returns:
  #   the subset of features, corresponding to the single items
  #
  fChosen =  features[,featureNamesCluster];
  
  mBoth=Mclust( fChosen, G=2:maxG );
  stopifnot(mBoth$G>=2);
  
  clusters = factor(mBoth$classification);
  
  agg = aggregate(features[,featureNameMin],by=list(clusters),FUN=mean);
  means = agg$x;

  chosenID = which.min(means);

  chosenIndices = mBoth$classification!=chosenID;
  
  featuresSub = features[ chosenIndices, ];
  
  if (showPlot==TRUE) {
    plot(mBoth,what=c("classification"));
  }
  return(featuresSub);	
}





