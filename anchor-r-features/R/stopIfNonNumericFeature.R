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

stopIfNonNumericFeature <- function( features ) {
  #
  # Checks that all all features have class=="numeric". Stops with an error message otherwise
  #
  # Args:
  #  features: features
  #
  # Returns:
  #  nothing
  #
  vec = sapply(trainingSet$features,class);
  
  indicesNonNumeric = which(vec!="numeric");
  
  if ( length(indicesNonNumeric)>0 ) {
    featureNames = colnames(features)[indicesNonNumeric];
    featureNamesSingleString = paste(featureNames,collapse=',');
    stop( sprintf("Features %s are non-numeric. Only numeric features are allowed.\n", featureNamesSingleString ) );
  }
}