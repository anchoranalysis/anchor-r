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

# Maps a vector of strings to int using a mapping that is found in a two-column CSV (with headernames) at pathLabelMapping
convertStringsToInt <- function(label, pathLabelMapping) {
  
  mapping = read.csv( pathLabelMapping, sep=",", row.names=1 );
  
  valueForLabel <- function( lab ) {
    indices = which(row.names(mapping)==lab)
    if( length(indices)==0 ) {
      stop( sprintf("Cannot find a mapping for %s", lab) );
    } else if( length(indices)==1 ) {
      return(mapping[indices,1]);
    } else {
      stop("indices should always be of length 0 or 1");
    }
  }
  
  labelReplaced = sapply( label, valueForLabel, USE.NAMES=F )
  
  if (any( is.na(labelReplaced) )) {
    missingLabels = unique( label[is.na(labelReplaced)] )
    missingLabelsAsStr = paste(missingLabels, collapse=", ")
    msg = sprintf(
      "Cannot find label-values for %s in %s",
      missingLabelsAsStr,
      pathLabelMapping
    )
    stop(msg)
  }
  
  labelReplaced
}

convertStringsToFactor <- function(label, pathLabelMapping) {
  label = convertStringsToInt(label, pathLabelMapping);
  label = as.factor(label);
  return(label);
}