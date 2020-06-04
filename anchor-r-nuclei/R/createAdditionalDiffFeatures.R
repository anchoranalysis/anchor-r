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
createDiffFunctionFeature <- function( features, featureNameNew, featureName, aggFunction ) {
  #
  #  Creates a feature that is the difference between the merged feature and the mean of the first and second
  #
  #  Args:
  #    features:  features (data.frame)
  #
  #  Returns:
  #    modified-features
  #
  diffName = sprintf('%s.%s', featureNameNew, featureName );
  firstName = sprintf('first.%s', featureName );
  secondName = sprintf('second.%s', featureName );
  mergedName = sprintf('merged.%s', featureName );
  
  if (!firstName %in% colnames(features)) {
    stop( sprintf("Cannot find feature '%s'", firstName));
  }
  
  if (!secondName %in% colnames(features)) {
    stop( sprintf("Cannot find feature '%s'", secondName));
  }
  
  if (!mergedName %in% colnames(features)) {
    stop( sprintf("Cannot find feature '%s'", mergedName));
  }

  features[,diffName] = features[,mergedName] - mapply( aggFunction, features[,firstName], features[,secondName] );
  features;
}

# Private
createDiffFeaturesForName <- function( features, featureName ) {
  #
  #  Creates a features that relate the merged feature to the first and second feature, for a specific feature
  #
  #  Args:
  #    features:  features (data.frame)
  #    featureName: specificFeature to create
  #
  #  Returns:
  #    modified-features
  #
  features = createDiffFunctionFeature( features, 'diffMean', featureName, mean );
  features = createDiffFunctionFeature( features, 'diffMax', featureName, max );
  features = createDiffFunctionFeature( features, 'diffMin', featureName, min );
  features;
}

createAdditionalDiffFeatures <- function( features ) {
  #
  #  Creates a features that relate the merged feature to the first and second feature
  #
  # Args:
  #   features:  features (data.frame)
  #
  # Requires features:
  #   first.x
  #   second.x
  #   merged.x
  #
  # Creates features:
  #   diff.x
  #
  # for each x described in the code
  #   
  #
  # Returns:
  #   modified features
  #
  features = createDiffFeaturesForName( features, 'axisEccentricity' );
  
  features = createDiffFeaturesForName( features, 'axisRatioEllipsoid' );
  features = createDiffFeaturesForName( features, 'axisRatioEllipsoidAll' );
  features = createDiffFeaturesForName( features, 'axisRatioMoments' );
  
  features = createDiffFeaturesForName( features, 'axisWidthMIP.0' );
  features = createDiffFeaturesForName( features, 'axisWidthMIP.1' );

  features = createDiffFeaturesForName( features, 'axisWidthMIPPhysical.0' );
  features = createDiffFeaturesForName( features, 'axisWidthMIPPhysical.1' );

  features = createDiffFeaturesForName( features, 'objectShellMeanIntensity.2' );
  features = createDiffFeaturesForName( features, 'objectShellMeanIntensity.3' );
  features = createDiffFeaturesForName( features, 'objectShellMeanIntensity.4' );
  
  features = createDiffFeaturesForName( features, 'objectShellMeanIntensityEdge.2' );
  features = createDiffFeaturesForName( features, 'objectShellMeanIntensityEdge.3' );
  features = createDiffFeaturesForName( features, 'objectShellMeanIntensityEdge.4' );
  features = createDiffFeaturesForName( features, 'objectShellMeanIntensityEdge.5' );
  
  features = createDiffFeaturesForName( features, 'score_maxSliceArea' );
  features = createDiffFeaturesForName( features, 'score_objectShellMeanIntensityEdge.2' );
  features = createDiffFeaturesForName( features, 'score_objectShellMeanIntensityEdge.3' );
  features = createDiffFeaturesForName( features, 'score_objectShellMeanIntensityEdge.4' );
  features = createDiffFeaturesForName( features, 'score_objectShellMeanIntensityEdge.5' );
  
  features = createDiffFeaturesForName( features, 'ellipticity' );
  features = createDiffFeaturesForName( features, 'ellipsoidicity' );
  features = createDiffFeaturesForName( features, 'ellipsoidicityAll' );
  
  features = createDiffFeaturesForName( features, 'maxSliceNumVoxelsOnContour' );
  features = createDiffFeaturesForName( features, 'maxSliceArea_Pixels' );
  features = createDiffFeaturesForName( features, 'maxSliceArea' );
  
  features = createDiffFeaturesForName( features, 'bboxExtnt.x' );
  features = createDiffFeaturesForName( features, 'bboxExtnt.y' );
  features = createDiffFeaturesForName( features, 'bboxExtnt.z' );
  
  features = createDiffFeaturesForName( features, 'shapeRegularityCenterSlice' );
  features = createDiffFeaturesForName( features, 'objectRadiusStdDev' );
  features = createDiffFeaturesForName( features, 'objectRadiusCov' );
  
  features = createDiffFeaturesForName( features, 'ratioAddedVoxelsAfterClosing.4' );
  
  features;
}