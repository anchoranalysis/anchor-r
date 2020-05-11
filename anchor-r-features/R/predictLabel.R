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

# Predict label on all images
predictLabel <- function( classifier, featuresToPredict, labelTraining, pp ) {
  
  # Perform necessary pre-processing
  featuresToPredict = predict(pp, featuresToPredict)
  
  # Make prediction
  labelPredict = predictAsFactor( classifier$model, featuresToPredict, labelTraining );
  
  # Make a data-frame that has just the image-identifier and the label
  featuresToPredict$label = labelPredict;
  data.frame(image=row.names(featuresToPredict),label=labelPredict)
}
