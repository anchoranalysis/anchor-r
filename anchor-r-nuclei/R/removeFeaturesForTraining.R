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

removeFeaturesForTraining <- function( features ) {
  #
  # Removes certain features (annotation-related, image and group name) that should not be included in training-data
  #
  # Args:
  #   features: features (data.frame)
  #
  # Returns:
  #   modified-features
  #
  featuresReduced = features;
  featuresReduced = removeFeaturesMatch(featuresReduced, 'id');
  featuresReduced = removeFeaturesMatch(featuresReduced, 'image');
  featuresReduced = removeFeaturesMatch(featuresReduced, 'unique_pixel_in_object');
  featuresReduced = removeFeaturesMatch(featuresReduced, 'group');
  featuresReduced = removeFeaturesMatch(featuresReduced, 'image\\.');
  featuresReduced = removeFeaturesColumnNames(featuresReduced, c('objSetName','group') );
  featuresReduced = removeFeaturesMatch(featuresReduced, 'annotation');
  featuresReduced = removeFeaturesMatch(featuresReduced, 'path');
  featuresReduced = removeFeaturesMatch(featuresReduced, 'csvID');
  featuresReduced = removeFeaturesMatch(featuresReduced, 'cog');
  featuresReduced = removeFeaturesMatch(featuresReduced, 'insidePnt');
  featuresReduced = removeFeaturesMatch(featuresReduced, 'scene');
  featuresReduced = removeFeaturesMatch(featuresReduced, 'atBorder');
  featuresReduced = removeFeaturesMatch(featuresReduced, 'first.');
  featuresReduced = removeFeaturesMatch(featuresReduced, 'second.');
  featuresReduced = removeFeaturesMatch(featuresReduced, 'Closing');
  featuresReduced = removeFeaturesWithNA(featuresReduced);
  featuresReduced = removeFeaturesWithNAN(featuresReduced);
  featuresReduced = removeFeaturesWithZeroSd(featuresReduced);
  featuresReduced = removeFeaturesMatch(featuresReduced, 'deltaOverlap');
  
  stopifnot(ncol(featuresReduced)>0);

  featuresReduced;
}