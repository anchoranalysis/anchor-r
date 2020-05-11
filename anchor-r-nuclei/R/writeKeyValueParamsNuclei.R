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

determineKeyValueParamsNucleiOutputPath <- function( pathFolderOut, filename, subFolder ) {
  #
  #  Determines the output path for writeKeyValueParamsNuclei, creating directories if necessary
  #
  #  Args:
  #    pathFolderOut: the path to the output folder
  #    filename: the filename for the params with explicitly specified prefix (usually ends in .xml)  
  #    subFolder: if non-NULL, an additional subfolder is appended to pathFolderOut
  #
  #  Returns:
  #    a string that is the path to the KeyValueParams file
  #

  # Determining output folder and filename
  if (!is.null(subFolder)) {
    outFilePath_folder = paste(pathFolderOut,"/",subFolder,sep='');
  } else {
    outFilePath_folder = pathFolderOut;
  }
  outFilePath = paste(outFilePath_folder,"/",filename,sep='');
  dir.create( outFilePath_folder, recursive=TRUE, showWarnings=FALSE );
  
  return(outFilePath);
}


addFeatureDivideByTwo <- function( fnPrefix, number, top ) {
  #
  #  Adds a feature fitted gaussian for a featureName of form "feature.NUM", but creating a name "feature.div2.NUMOUT"
  #      where NUMOUT = NUM * 2
  #
  featureNameIn = sprintf("%s.%d", fnPrefix, ceiling(number/2) );
  featureNameOut = sprintf("%s.div2.%d", fnPrefix, number);
  
  if( !(featureNameIn %in% colnames(features)) ) {
    stop( sprintf("Feature '%s' not in data.frame", featureNameIn));
  }
  
  addFeatureFittedGaussian(featureNameOut,features[,featureNameIn],"normal",top);
  addFeatureGroup(featureNameOut,features[featureNameIn],top);
}

writeKeyValueParamsNuclei <- function( pathFolderOut, filename='params.xml', features, subFolder=NULL, flatMode=FALSE ) {
  #
  #  Writes feature-statistics for Nucleus into a KeyValueParams-XML file for Anchor
  #
  #  Args:
  #    pathFolderOut: the path to the output folder
  #    filename: the filename for the params with explicitly specified prefix (usually ends in .xml)  
  #    features: an Anchor-style featuers table
  #    subFolder: if non-NULL, an additional subfolder is appended to pathFolderOut
  #    flatMode: a parameter that is also written to the file indicating if the dataset should be treated as a flat set of nuclei (lying on the same plane)
  #
  #  Returns:
  #    nothing
  #
  
  featureNamesGaussian = c(
    'meanIntensity',
    'meanIntensityGradient',
    'texture',
    'objectRadiusStdDev',
    'objectRadiusCov',
    'volume_CubicMicrons',
    'maxSliceArea',
    'sumIntensity',
    'bboxExtnt.x',
    'bboxExtnt.y',
    'bboxExtnt.z',
    'ellipsoidicity',
    'ellipticity',
    'shapeRegularityCenterSlice',
    'objectShellMeanIntensityEdge.1',
    'objectShellMeanIntensityEdge.2',
    'objectShellMeanIntensityEdge.3',
    'objectShellMeanIntensityEdge.4',
    'objectShellMeanIntensityEdge.5',
    'objectShellMeanIntensityEdge.6',
    'objectShellMeanIntensityEdge.7',
    'objectShellMeanIntensity.2',
    'objectShellMeanIntensity.3',
    'objectShellMeanIntensity.4',
    'objectEdgeDifference2',
    'objectShellMeanIntensityEdgeInverse.2',
    'objectShellMeanIntensityEdgeInverse.4',
    'bboxExtnt_Pixels.x',
    'bboxExtnt_Pixels.y',
    'bboxExtnt_Pixels.z',
    'maxSliceNumVoxelsOnContour',
    'axisRatioMoments',
    'objectDifferenceShellMeanIntensity'
  )

  outFilePath = determineKeyValueParamsNucleiOutputPath( pathFolderOut, filename, subFolder );
  
  cat( 'WRITING: ', outFilePath,'\n' );  
  
  top = newXMLNode("properties")

  # Number of Features
  addFeature('size',nrow(features),top);
  
  # Fitting a Gaussian to features
  for ( fn in featureNamesGaussian ) {
    
    if (fn %in% colnames(features)) {
      addFeatureFittedGaussian(fn,features[,fn],"normal",top);
      addFeatureGroup(fn,features[,fn],top);
    } else {
      cat( sprintf("Missing feature '%s'\n", fn) );
    }
  }
  
  for( i in 1:7 ) {
    addFeatureDivideByTwo('objectShellMeanIntensityEdge',i, top);
  }

  addFeature( 'flatMode',ifelse(flatMode,1,0),top);
  
  fileOut <- file(outFilePath, "w");
  cat( '<?xml version="1.0" encoding="UTF-8" standalone="no"?>\n', file=fileOut);
  cat( '<!DOCTYPE properties SYSTEM "http://java.sun.com/dtd/properties.dtd">\n', file=fileOut);
  cat( saveXML(top), file=fileOut );
  close(fileOut);
}
