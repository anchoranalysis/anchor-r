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

removeObjectsWithinRange <- function( features, featureName, minThreshold, maxThreshold ) {
  #
  # Removes objects (rows in feature table) with a values within a certain range
  #
  # Args:
  #   features: features
  #   featureName: the particular feature that is thresholded
  #   minThreshold: minimum-allowed value that is removed from feature table
  #   maxThreshold: maximum-allowed value that is removed from feature table (maxThreshold>=minThreshold)
  #
  # Returns:
  #   modified features
  #
  toRemove = features[,featureName] >= minThreshold & features[,featureName] <= maxThreshold;
  features[!toRemove,];
}