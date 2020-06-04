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

addFeature <- function(key,val,parent) {
  #
  # Adds a simple key-value parameter to the XML-object
  #
  # Args:
  #    key: key
  #    val: value (as string)
  #    parent: parent XML node
  #
  # Returns:
  #    return value of newXMLNode
  newXMLNode("entry", val, attrs = c(key=key), parent=parent);
}



addFeatureFittedGaussian <- function(featureName,featureValues,distributionName,parent) {
  featureValues = as.matrix(featureValues);
  na.omit(featureValues);
  featureValues = featureValues[!is.na(featureValues)];

  if (length(featureValues)>0) {
    f = fitdistr(featureValues, distributionName); #"normal"
    est_mean = f$estimate[1]
    est_sd = f$estimate[2]
  } else {
    est_mean = NaN
    est_sd = NaN
  }
  
  addFeature( paste(featureName,"_fitted_", distributionName, "_mean",sep=''),est_mean,parent);
  addFeature( paste(featureName,"_fitted_", distributionName, "_sd",sep=''),est_sd,parent);
}


addFeatureGroup <- function(featureName,featureValues,parent) {
  featureValues = as.matrix(featureValues);
  
  featureValues = na.omit(featureValues);
  
  addFeature( paste(featureName,"_mean",sep=''),mean(featureValues),parent);
  addFeature( paste(featureName,"_median",sep=''),median(featureValues),parent);
  addFeature( paste(featureName,"_sd",sep=''),sd(featureValues),parent);
  addFeature( paste(featureName,"_min",sep=''),min(featureValues),parent);
  addFeature( paste(featureName,"_max",sep=''),max(featureValues),parent);
}


addFeatureRange <- function(featureName,featureRangeMin,featureRangeMax,featureRangeTop,parent) {
  addFeature( paste(featureName,"_min",sep=''),featureRangeMin,parent);
  addFeature( paste(featureName,"_max",sep=''),featureRangeMax,parent);
  addFeature( paste(featureName,"_top",sep=''),featureRangeTop,parent);
}

addFeatureGaussianCluster <- function(featureName,cluster,parent,index=1) {
  addFeature( paste(featureName,"_gaussian_mean",sep=''),cluster$selMean[index],parent);
  addFeature( paste(featureName,"_gaussian_sd",sep=''),cluster$selStdDev[index],parent);
  addFeature( paste(featureName,"_gaussian_numClusters",sep=''),cluster$numClusters,parent);
}

addFeatureGaussianVector <- function(featureName,vec,parent,shiftIndex=0) {
  addFeature( paste(featureName,"_gaussian_mean",sep=''),vec[1+shiftIndex],parent);
  addFeature( paste(featureName,"_gaussian_sd",sep=''),vec[2+shiftIndex],parent);
}

addFeatureSingleGaussianClusterToIncludeAllData <- function(featureName,featureValues,parent) {
  browser();
  addFeature( paste(featureName,"_gaussian_mean",sep=''),cluster$selMean[index],parent);
  addFeature( paste(featureName,"_gaussian_sd",sep=''),cluster$selStdDev[index],parent);
  addFeature( paste(featureName,"_gaussian_numClusters",sep=''),1,parent);
}


addFeatureIntensityClusterRange <- function(featureName,intensityClusterRange,parent) {
  stopifnot( is.finite(intensityClusterRange$min) );
  stopifnot( is.finite(intensityClusterRange$max) );
  
  addFeature( paste(featureName,"_min",sep=''),intensityClusterRange$min,parent);
  addFeature( paste(featureName,"_max",sep=''),intensityClusterRange$max,parent);
  addFeature( paste(featureName,"_mean",sep=''),intensityClusterRange$mean,parent);
  addFeature( paste(featureName,"_sd",sep=''),intensityClusterRange$sd,parent);
}


writeKeyValueParamsFromNode <- function( folderPath, fileName, xmlNode ) {
  #
  #  Writes the KeyValueParams to a file from an XML node
  #
  #  Args:
  #    folderPath:  path to the folder to write to
  #    fileName:    file-name to write to (appended to the folderPath)
  #    xmlNode:     the top-level XML node to write
  #
  outFilePath = paste(folderPath,"\\", fileName, sep='');
  
  dir.create( folderPath, recursive=TRUE, showWarnings=FALSE );
  
  fileOut <- file(outFilePath, "w");
  cat( '<?xml version="1.0" encoding="UTF-8" standalone="no"?>\n', file=fileOut);
  cat( '<!DOCTYPE properties SYSTEM "http://java.sun.com/dtd/properties.dtd">\n', file=fileOut);
  cat( saveXML(xmlNode), file=fileOut );
  close(fileOut);
}



writeKeyValueParamsRange <- function( pathFolderOut, dataset, intensityClusterRangeHigh, intensityClusterRangeLow, intensityClusterRangeMean, intensityClusterRangeMeanMax,overexposure ) {
  
  cat( 'WRITING: ', dataset,'\n' );
  #printf("rangeHigh\t\tmin=%f\tmax=%f\ttop=%f\n", rangeMin, rangeMax, rangeTop );

  top = newXMLNode("properties");
  
  addFeatureIntensityClusterRange("intensityHigh",intensityClusterRangeHigh, top );
  addFeatureIntensityClusterRange("intensityLow",intensityClusterRangeLow, top );
  addFeatureIntensityClusterRange("intensityMean",intensityClusterRangeMean, top );
  addFeatureIntensityClusterRange("intensityMeanMax",intensityClusterRangeMeanMax, top );
  addFeature("overexposure",overexposure,top);

  outFilePath_folder = paste(pathFolderOut,"\\",dataset,sep='');
  writeKeyValueParamsFromNode(outFilePath_folder,'paramsGroupAgg.xml',top);
}


writeKeyValueParamsRangeSingle <- function( pathFolderOut, dataset, clusterName, cluster) {
  
  cat( 'WRITING: ', dataset,'\n' );

  top = newXMLNode("properties");
  
  addFeatureIntensityClusterRange(clusterName,cluster, top );
  
  outFilePath_folder = paste(pathFolderOut,"\\",dataset,sep='');
  writeKeyValueParamsFromNode(outFilePath_folder,'paramsGroupAgg.xml',top);
}


writeKeyValueParamsIntensityRangeAndExtnt <- function(
  pathFolderOut,
  dataset,
  intensityClusterRangeHigh,
  intensityClusterRangeLow,
  intensityClusterRangeMean,
  intensityClusterRangeScale,
  gaussianExtnt
) {
  
  cat( 'WRITING: ', dataset,'\n' );

  top = newXMLNode("properties");
  
  addFeatureIntensityClusterRange("intensityHigh",intensityClusterRangeHigh, top );
  addFeatureIntensityClusterRange("intensityLow",intensityClusterRangeLow, top );
  addFeatureIntensityClusterRange("intensityMean",intensityClusterRangeMean, top );
  addFeatureIntensityClusterRange("intensityScale",intensityClusterRangeScale, top );
  
  addFeatureGaussianVector('volume',gaussianExtnt,top);
  addFeatureGaussianVector('maxSliceArea',gaussianExtnt,top,shiftIndex=4);
  addFeatureGaussianVector('sumIntensity',gaussianExtnt,top);
  
  outFilePath_folder = paste(pathFolderOut,"\\",dataset,sep='');
  writeKeyValueParamsFromNode(outFilePath_folder,'paramsGroupAgg.xml',top);
}


writeKeyValueParams <- function( dataset, pathFolderOut, features, cluster_meanIntensity, clusterResults_extnt ) {
  
  printf <- function(...) invisible(cat(sprintf(...)))
  
  cat( 'WRITING: ', dataset,'\n' );
  printf("meanIntensitySd\t\tmean=%f\tsd=%f\n", median(features$meanIntensity), sd(features$meanIntensity) );
  
  printf("meanIntensity       \t\tmean=%f\tsd=%f\n", cluster_meanIntensity$selMean, cluster_meanIntensity$selStdDev );
  printf("volume       \t\tmean=%f\tsd=%f\n", clusterResults_extnt$cluster$selMean[1], clusterResults_extnt$cluster$selStdDev[1] );
  printf("maxSliceArea  \t\tmean=%f\tsd=%f\n", clusterResults_extnt$cluster$selMean[2], clusterResults_extnt$cluster$selStdDev[2] );
  printf("sumIntensity  \t\tmean=%f\tsd=%f\n", clusterResults_extnt$cluster$selMean[3], clusterResults_extnt$cluster$selStdDev[3] );

  top = newXMLNode("properties")
  addFeature('size',nrow(features),top);
  
  if (!is.null(cluster_meanIntensity)) {
    addFeatureGaussianCluster('meanIntensity',cluster_meanIntensity,top);
  } else {
    addFeatureSingleGaussianClusterToIncludeAllData('meanIntensity',features$meanIntensity,top);
  }
  addFeatureGaussianCluster('volume',clusterResults_extnt$cluster,top,index=1);
  addFeatureGaussianCluster('maxSliceArea',clusterResults_extnt$cluster,top,index=2);
  addFeatureGaussianCluster('sumIntensity',clusterResults_extnt$cluster,top,index=3);
  
  addFeatureFittedGaussian('meanIntensity',features$meanIntensity,"normal",top);
  addFeatureFittedGaussian('volume',features$volume,"normal",top);
  addFeatureFittedGaussian('maxSliceArea',features$maxSliceArea,"normal",top);
  addFeatureFittedGaussian('sumIntensity',features$sumIntensity,"normal",top);
  
  addFeatureGroup('meanIntensity',features$meanIntensity,top);
  addFeatureGroup('volume',features$volume,top);
  addFeatureGroup('maxSliceArea',features$maxSliceArea,top);
  addFeatureGroup('sumIntensity',features$sumIntensity,top);
  
  outFilePath_folder = paste(pathFolderOut,"\\",dataset,sep='');
  writeKeyValueParamsFromNode(outFilePath_folder,'paramsGroupAgg.xml',top);
}
