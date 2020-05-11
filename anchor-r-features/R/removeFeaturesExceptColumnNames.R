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

removeFeaturesExceptColumnNames <- function( features, featureNames ) {
  # Removes features from a feature-table except those with specific featureNames
  #
  # Args:
  #   features: feature-table
  #   featureNames: names of features to keep (character-vector)
  #
  # Returns:
  #   a feature-table with only the specified featureNames
  foundIn = featureNames %in% colnames(features);
  if (!all(foundIn)) {
    stop(sprintf('Missing features: %s\n', featureNames[foundIn==FALSE] ) );
  }
  
  features[,featureNames];
}
