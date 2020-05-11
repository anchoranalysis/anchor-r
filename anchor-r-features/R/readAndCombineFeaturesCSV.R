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

# PRIVATE
printFeatureDiff <- function( featureNamesPrevious, featureNamesToAdd ) {
  #
  #  Prints the difference in names between previous and toAdd
  #
  #  Returns:
  #    nothing
  #
  featuresDiffMissing = c( setdiff(featureNamesPrevious,featureNamesToAdd) );
  featuresDiffExtra = c( setdiff(featureNamesToAdd,featureNamesPrevious) );
  
  if( length(featuresDiffMissing)>0 ) {
    cat( sprintf("The following features are missing:\n") );
    print(featuresDiffMissing);
  }
  
  if( length(featuresDiffExtra)>0 ) {
    cat( sprintf("The following additional-features are found:\n") );
    print(featuresDiffExtra);
  }
}


# PRIVATE
addPathToFeatures <- function( feature, path ) {
  #
  #  Adds a path column to the data.frame
  #
  feature[,'path'] = rep(path, nrow(feature) );
  feature;
}

# PRIVATE
addIdentifierToFeatures <- function( feature, id ) {
  #
  #  Adds a path column to the data.frame
  #
  feature[,'csvID'] = rep(id, nrow(feature) );
  feature;
}


readAndCombineFeaturesCSV <- function( paths, addPath=FALSE, addCSVIdentifier=FALSE ) {
  #
  #  Reads one or more feature-CSVs, and combines them into a single feature-table
  #
  #  An error is thrown if they do not have identical columns (features)
  #
  #  Args:
  #    paths: character-vector of paths
  #    addPath: if TRUE, an additional feature 'path' is added with the path to the original file
  #    addCSVIdentifier: if TRUE, adds an integer number as a factor for each separate CSV file as feature 'csvID'
  #
  #  Returns:
  #    feature-table with the features from all the paths
  #
  first = TRUE;
  for( i in 1:length(paths) ) {
    
    path = paths[i];
    featuresSingle = readFeaturesCSV(path);

    if (addPath) {
      featuresSingle = addPathToFeatures(featuresSingle,path);
    }
    
    if (addCSVIdentifier) {
      featuresSingle = addIdentifierToFeatures(featuresSingle,i);
    }
    
    if (i==1) {
      # If it's the first
      features = featuresSingle;
    } else {
      
      # If it's subsequent, then we try to combine
      if (!identical(colnames(featuresSingle),colnames(features))) {
        
        printFeatureDiff( colnames(features), colnames(featuresSingle) );
        
        stop( sprintf("Features for '%s' are not identical to previous features", path ) );
      }
      
      features = rbind( features, featuresSingle );
      
    }
  }
  
  if (addCSVIdentifier) {
    # Turn the ID into a factor  
    features$csvID = factor(features$csvID);
  }
  
  features;  
}