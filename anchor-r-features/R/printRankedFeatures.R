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

printRankedFeatures <- function( coefModel ) {
  #
  #  Print features in the order of their contribution to the model
  #   i.e. ordered by the absolute value of their coefficient
  #
  #  It assumes features were normalized before the model was trained
  #    so that the coefficients reflect the relative-contribution
  #
  #  Args:
  #   coefModel:   the object created by training function (e.g. an lda model) for which coef() returns meaningful valuess
  #
  #  Returns:
  #    nothing
  #
  
  # Features ranked
  featureContrib = abs(coef(coefModel)); #g$scaling[,1];
  indexOrder = order( featureContrib, decreasing=TRUE );
  orderedFeatures = as.data.frame( featureContrib[indexOrder,] );
  
  #cat('Ordered Features:\n');
  print( orderedFeatures);
}