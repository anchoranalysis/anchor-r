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

writeLDAModelKeyValueParams <- function( folderPath, model, additionalCoefficient=NULL ) {
  #
  #  Writes an LDA model to a KeyValueParams xml format.
  #
  #  Each feature is written as a property-value combination, with it's name and coefficient
  #
  #  The classification-threshold is also written as a property-value combination, with the name '__ldaThreshold'
  #
  #  Assumes binary-classifier with priors c(0.5,0.5)
  #
  #  Args:
  #    folderPath: the folder which the mode is written to (with name 'ldaModel.xml')
  #    model: a model of class anchorlda
  #    additionalCoefficient: if non-NULL, an additional coefficient in the form list(featureName=string,coefficient=double)
  #
  #  Returns:
  #    none
  #
  
  # The feature coefficients
  ldaCoef = coef(model);
  featureNames = rownames(ldaCoef);
  featureCoefficients = ldaCoef[,1];
  
  top = newXMLNode("properties")
  
  # Write XML property for each feature and its coefficient
  for( name in featureNames ) {
    addFeature(name,featureCoefficients[name],top);
  }
  
  if (!is.null(additionalCoefficient)) {
    # HARD-CODED FOR NOW
    # We disallowing touching pairs
    addFeature(additionalCoefficient$featureName,additionalCoefficient$coefficient,top);
  }
  
  
  
  # the classification threshold
  thrshld = sum( featureCoefficients * model$submodel$means[2,] + featureCoefficients * model$submodel$means[1,])/2;
  addFeature('__ldaThreshold',thrshld,top);
  
  writeKeyValueParamsFromNode(folderPath,'ldaModel.xml',top);
}