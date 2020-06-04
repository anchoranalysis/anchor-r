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

# Private
createPairFunctionFeature <- function( features, featureNameNew, featureName, aggFunction ) {
  #
  #  Creates a feature that is a functio applied do the first and second feature
  #
  #  Args:
  #    features:  features (data.frame)
  #
  #  Returns:
  #    modified-features
  #
  pairName = sprintf('%s.%s', featureNameNew, featureName );
  firstName = sprintf('first.%s', featureName );
  secondName = sprintf('second.%s', featureName );

  if (!firstName %in% colnames(features)) {
    stop( sprintf("Cannot find feature '%s'", firstName));
  }
  
  if (!secondName %in% colnames(features)) {
    stop( sprintf("Cannot find feature '%s'", secondName));
  }

  features[,pairName] = mapply( aggFunction, features[,firstName], features[,secondName] );
  features;
}

# Private
# Minimum of the ratio x:y and y:x
minRatio <- function( x, y ) {
  min( c(x/y,y/x) );
}

# Private
# Maximum of the ratio x:y and y:x
maxRatio <- function( x, y ) {
  max( c(x/y,y/x) );
}

# Private
# Maximum of the ratio x:y and y:x
meanRatio <- function( x, y ) {
  mean( c(x/y,y/x) );
}


# Private
createPairFeaturesForName <- function( features, featureName ) {
  #
  #  Creates a features that relate the first and second feature, for a specific feature
  #
  #  Args:
  #    features:  features (data.frame)
  #    featureName: specificFeature to create
  #
  #  Returns:
  #    modified-features
  #
  features = createPairFunctionFeature( features, 'pairMean', featureName, mean );
  features = createPairFunctionFeature( features, 'pairMax', featureName, max );
  features = createPairFunctionFeature( features, 'pairMin', featureName, min );
  features = createPairFunctionFeature( features, 'pairMinRatio', featureName, minRatio );
  features = createPairFunctionFeature( features, 'pairMaxRatio', featureName, maxRatio );
  features = createPairFunctionFeature( features, 'pairMeanRatio', featureName, meanRatio );
  features;
}

createAdditionalPairFeatures <- function( features ) {
  #
  #  Creates a features that relate the first and second feature
  #
  # Args:
  #   features:  features (data.frame)
  #
  # Requires features:
  #   first.x
  #   second.x
  #
  # Creates features:
  #   pair.x
  #
  # for each x described in the code
  #
  # Returns:
  #   modified features
  #
  features = createPairFeaturesForName( features, 'axisRatioEllipsoid' );
  features = createPairFeaturesForName( features, 'axisRatioEllipsoidAll' );
  features = createPairFeaturesForName( features, 'axisRatioMoments' );
  
  features = createDiffFeaturesForName( features, 'axisEccentricity' );
  
  features = createPairFeaturesForName( features, 'axisWidthMIP.0' );
  features = createPairFeaturesForName( features, 'axisWidthMIP.1' );
  
  features = createPairFeaturesForName( features, 'axisWidthMIPPhysical.0' );
  features = createPairFeaturesForName( features, 'axisWidthMIPPhysical.1' );
  
  features = createPairFeaturesForName( features, 'objectShellMeanIntensity.2' );
  features = createPairFeaturesForName( features, 'objectShellMeanIntensity.3' );
  features = createPairFeaturesForName( features, 'objectShellMeanIntensity.4' );
  
  features = createPairFeaturesForName( features, 'objectShellMeanIntensityEdge.2' );
  features = createPairFeaturesForName( features, 'objectShellMeanIntensityEdge.3' );
  features = createPairFeaturesForName( features, 'objectShellMeanIntensityEdge.4' );
  features = createPairFeaturesForName( features, 'objectShellMeanIntensityEdge.5' );
  
  features = createPairFeaturesForName( features, 'score_objectShellMeanIntensityEdge.2' );
  features = createPairFeaturesForName( features, 'score_objectShellMeanIntensityEdge.3' );
  features = createPairFeaturesForName( features, 'score_objectShellMeanIntensityEdge.4' );
  features = createPairFeaturesForName( features, 'score_objectShellMeanIntensityEdge.5' );
  
  features = createPairFeaturesForName( features, 'ellipticity' );
  features = createPairFeaturesForName( features, 'ellipsoidicity' );
  features = createPairFeaturesForName( features, 'ellipsoidicityAll' );
  
  features = createPairFeaturesForName( features, 'score_maxSliceArea' );
  features = createPairFeaturesForName( features, 'maxSliceNumVoxelsOnContour' );
  features = createPairFeaturesForName( features, 'maxSliceArea_Pixels' );
  
  features = createPairFeaturesForName( features, 'maxSliceArea' );
  
  features = createPairFeaturesForName( features, 'bboxExtnt.x' );
  features = createPairFeaturesForName( features, 'bboxExtnt.y' );
  features = createPairFeaturesForName( features, 'bboxExtnt.z' );
  
  features = createPairFeaturesForName( features, 'shapeRegularityCenterSlice' );
  features = createPairFeaturesForName( features, 'objectRadiusStdDev' );
  features = createPairFeaturesForName( features, 'objectRadiusCov' );

  features;
}