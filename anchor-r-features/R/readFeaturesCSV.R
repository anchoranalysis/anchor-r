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

#
# Reads a anchor features-output from a .csv file that contains one feature per row
#
# Args:
#   csvpath: path to the csv file
#
# Returns:
#   a data frame of features, where each row is a value, and special columns (group, object-sets)
#   have been converted into factors
#
readFeaturesCSV <- function( csvpath, colClasses=NA ) {
  
  printf <- function(...) invisible(cat(sprintf(...)));
  
  features = read.csv( csvpath, stringsAsFactors=FALSE, colClasses=colClasses );  

  printf("Loaded %d rows from CSV\n", nrow(features));
  
  # Initially we take each column as a feature, then we remove
  #   special columns for group or objSetName
  allFeatureNames = colnames(features);
  
  if ("group" %in% colnames(features) ) {
    features$group = factor(features$group);
    
    printf("There are %d groups\n", length(levels(features$group)) );
    
    allFeatureNames = setdiff( allFeatureNames, 'group' )
  }
  
  if ("objSetName" %in% colnames(features) ) {
    features$objSetName = factor(features$objSetName);
    
    groupLevels = levels(features$objSetName);
    printf("There are %d object-sets\n", length(groupLevels) );
    print(groupLevels);
    
    allFeatureNames = setdiff( allFeatureNames, 'objSetName' )
  }
  
  printf("There are %d features\n", length(allFeatureNames) );
  #print(allFeatureNames);

  features
}