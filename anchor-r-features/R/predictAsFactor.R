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

predictAsFactor <- function( model, data, label ) {
  #
  #  Makes a prediction from the model on data
  #
  #  we need to specify the existing label, so a factor can be recreated with identical labels to the input
  #
  #  as the predict associated with the svm function() returns a factor 1,2,3 we need to convert this back
  #   into the same factor-levels as our label
  #
  # Args:
  #   model: classifier (must come from a trainSVM_ function)
  #   data:  predictor-data
  #   label: the labelling used for training the classifier
  #
  # Returns:
  #   predictions as a factor with identical levels to label
  #  
  p = predict( model, data );
  p = factor( as.numeric(p)-1, levels=levels(label) );
  p;
}