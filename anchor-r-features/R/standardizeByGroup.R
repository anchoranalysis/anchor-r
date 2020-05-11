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

standardizeByGroup <- function( features, label ) {
  # Standardizes feature-values in each column, based upon group-labeling
  #  such that:
  #      each group has mean 0
  #      each group has var 1
  #
  # Args:
  #   features: feature-table
  #   label: a factor describing group membership
  #
  # Returns:
  #   an identical feature-table except with standardized-values
  
  withinGroupSd = apply(features,2,function(x) tapply(x,label,sd));
  withinGroupMean = apply(features,2,function(x) tapply(x,label,mean));
  
  for(l in levels(label)) {
    groupIndex = rownames(withinGroupMean)==l;
    for(i in 1:length(colnames(features))) {
      features[label==l,i] <- (features[label==l,i] - withinGroupMean[groupIndex,i]) / withinGroupSd[groupIndex,i];
    }
  }
  features
}
