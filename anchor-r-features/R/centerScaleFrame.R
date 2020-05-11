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

centerScaleFrame <- function(features) {
  #
  # Builds a data.frame with center and scale (sd) values for each feature in a table
  #
  # For non-numeric features, center and scale are recorded as NA
  #
  # Args:
  #   features: features
  #
  # Returns:
  #   data.frame with center and scale
  #

  meanFunc <- function(x) {
    if(class(x)=='numeric') {
      return( mean(x,na.rm=TRUE) );
    } else {
      return( NA );
    }
  };
  
  sdFunc <- function(x) {
    if(class(x)=='numeric') {
      return( sd(x,na.rm=TRUE) );
    } else {
      return( NA );
    }      
  };
  
  meanVals = sapply(features, meanFunc );
  sdVals = sapply(features, sdFunc );
  
  data.frame( mean=meanVals, scale=sdVals );
}