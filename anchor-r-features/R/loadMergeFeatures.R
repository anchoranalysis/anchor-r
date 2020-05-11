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

.findRowsWithAnyNA <- function( features ) {
  funFind = function(a) any(is.na(a));
  all = apply( features, 1, funFind );
  features[all,];
}



# Reads the image-label from an aggregated CSV
.readAnnotationsImageLabel <- function( datasetName, prependImageName=FALSE ) {
  pathAnnotations = root$pathExp('annotationImageClassifier',datasetName,'1.0','annotationsAgg.csv');
  annotations = readFeaturesCSV( pathAnnotations, colClasses=c(id='character') );
  colnames(annotations) <- c('image', 'label')
  if (prependImageName) {
    annotations$image = sprintf("%s/%s", datasetName, annotations$image)
  }
  annotations
}


.addAnnotationsToFeatures <- function( features, dataSetName, csvPathConvertStrings ) {
  annotations = .readAnnotationsImageLabel(dataSetName);
  
  annotations$label = convertStringsToFactor(annotations$label, csvPathConvertStrings)
  
  merged = merge(annotations,features,by="image", all.y=TRUE);
  row.names(merged) = merged$image;
  merged;
}

.removeFeaturesForTraining <- function( features ) {
  features = removeFeaturesMatch(features, 'group');
  features = .removeLabelFeatures(features);
  features = removeFeaturesWithZeroSd(features);
  
  stopifnot(ncol(features)>0);
  
  features
}

.removeIdentifierFeatures <- function( features ) {
  features = removeFeaturesMatch(features, 'image');
  stopifnot(ncol(features)>0);
  features
}

.removeLabelFeatures <- function( features ) {
  features = removeFeaturesMatch(features, 'label');
  stopifnot(ncol(features)>0);
  features
}






#
# load the features from the file-system and removes any features we don't want
#
# Args:
#    dataSetName: name of the dataset
#    experimentName: name of the experiment
#    experimentVersion: version of the experiment
#
loadFeatures <- function( dataSetName, experimentName='imageFeatures', experimentVersion='1.0' ) {
  
  pathFeatures = root$pathExp(experimentName, dataSetName, experimentVersion, 'csvAll.csv');
  features = readFeaturesCSV( pathFeatures, colClasses=c(image='character') );
  row.names(features) = features$image;
  
  # Remove features we don't want in training, but leave the identifier feature, as it's useful
  # for merging with annotations
  features = .removeFeaturesForTraining(features);
  
  stopifnot(
    nrow(.findRowsWithAnyNA(features))==0
  );
  
  features;
}



#
# merge annotations with features, seperating the rows which have a valid label
#  from those that don't have a valid label
#
# Args:
#   features: feature-table (without annotation label)
#   dataSetName: used to load the annotations
#   csvPathConvertStrings: a CSV file which maps the labels, so as to create a valid factor
#
# Returns:
#   list(
#     validFeatures: features with a label ONLY (but already stripped of the label)
#     label: labels corresponding to validFeatures
#   )
#
mergeFeaturesWithAnnotations <- function( features, dataSetName, csvPathConvertStrings ) {
  
  features = .addAnnotationsToFeatures( features, dataSetName, csvPathConvertStrings )
  features = .removeIdentifierFeatures(features);
  
  # Only rows with a valid label
  features = features[!is.na(features$label),]
  
  list(
    validFeatures=.removeLabelFeatures(features),
    label=features$label
  )
}