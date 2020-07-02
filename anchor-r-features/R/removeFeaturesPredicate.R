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

removeFeaturesPredicate <- function( features, predicate ) {
  # Removes any feature-columns that match a predicate
  #
  # Args:
  #   features: feature-table
  #   predicate: a condition on a feature-column which determines whether a column is removed
  #
  # Returns:
  #   a feature-table without the removed columns 
  select_if(features, Negate(predicate))
}