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

createLabelPositiveNegative <- function( featureVals ) {
  #
  # Creates a label for a feature-table based upon if feature values or positive or negative
  #
  # The label is 0 for negative features, and 1 for positive features
  #
  # Args:
  #   featureVals: the values-of a feature
  #
  # Returns:
  #   0 and 1 as factors
  #
  label = sign( featureVals );
  label[label==-1] = 0;
  # Make the label a factor
  factor(label);
}