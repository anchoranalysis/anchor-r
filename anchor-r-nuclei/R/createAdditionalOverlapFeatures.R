#
#  Copyright (C) 2016 ETH Zurich, University of Zurich, Owen Feehan
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

createAdditionalOverlapFeatures <- function( features ) {
  #
  # Creates additional features about overlap of objects with annotations
  #
  # Args:
  #   features:  features (data.frame)
  #
  # Requires features:
  #   first.maxOverlapWithPositiveAnnotation
  #   second.maxOverlapWithPositiveAnnotation
  #   merged.maxOverlapWithPositiveAnnotation
  #
  #   first.maxOverlapWithNegativeAnnotation
  #   second.maxOverlapWithNegativeAnnotation
  #   merged.maxOverlapWithNegativeAnnotation
  #
  # Creates features:
  #   pair.maxOverlapWithAnnotation
  #   merged.maxOverlapWithAnnotation
  #   span.deltaOverlap
  #
  # Returns:
  #   modified features
  #
  features$first.maxOverlapWithAnnotation = pmax( features$first.maxOverlapWithPositiveAnnotation, features$first.maxOverlapWithNegativeAnnotation );
  features$second.maxOverlapWithAnnotation = pmax( features$second.maxOverlapWithPositiveAnnotation, features$second.maxOverlapWithNegativeAnnotation );
  
  features$first.minOverlapWithAnnotation = pmax( features$first.minOverlapWithPositiveAnnotation, features$first.minOverlapWithNegativeAnnotation );
  features$second.minOverlapWithAnnotation = pmax( features$second.minOverlapWithPositiveAnnotation, features$second.minOverlapWithNegativeAnnotation );
  
  features$first.numOverlapWithAnnotation = features$first.numOverlapWithPositiveAnnotation + features$first.numOverlapWithNegativeAnnotation;
  features$second.numOverlapWithAnnotation = features$second.numOverlapWithPositiveAnnotation + features$second.numOverlapWithNegativeAnnotation;
  features$merged.numOverlapWithAnnotation = features$merged.numOverlapWithPositiveAnnotation + features$merged.numOverlapWithNegativeAnnotation;
  
  features$pair.maxOverlapWithAnnotation = mapply(mean, features$first.maxOverlapWithAnnotation, features$second.maxOverlapWithAnnotation);
  features$pair.minOverlapWithAnnotation = mapply(max, features$first.minOverlapWithAnnotation, features$second.minOverlapWithAnnotation);
  features$pair.minOverlapWithPositiveAnnotation = mapply(max, features$first.minOverlapWithPositiveAnnotation, features$second.minOverlapWithPositiveAnnotation);
  features$pair.maxOverlapWithPositiveAnnotation = mapply(mean, features$first.maxOverlapWithPositiveAnnotation, features$second.maxOverlapWithPositiveAnnotation);
  
  features$merged.maxOverlapWithAnnotation = pmax( features$merged.maxOverlapWithPositiveAnnotation, features$merged.maxOverlapWithNegativeAnnotation );
  features$merged.minOverlapWithAnnotation = pmax( features$merged.minOverlapWithPositiveAnnotation, features$merged.minOverlapWithNegativeAnnotation );
  
  features$span.deltaOverlap = features$merged.minOverlapWithAnnotation - features$pair.minOverlapWithAnnotation;
  #features$span.deltaOverlap = features$merged.minOverlapWithPositiveAnnotation - features$pair.minOverlapWithPositiveAnnotation;
  features;
}