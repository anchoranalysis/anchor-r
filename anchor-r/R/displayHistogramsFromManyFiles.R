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

displayHistogramsFromManyFiles <- function( dirpath, pattern, distFromRoot, minHistBin, maxHistBin, firstPageOnly=FALSE, maxPerPage=20 ) {
  #
  # Reads a histogram from many .csv file outputted by Anchor and displays them together
  #
  # Only considers files in some directory which match a pattern and which have identical distances to the root-directory
  #   where:
  #     dist(SOMRDIR/X/X/SOMEFILE)=2
  #     dist(SOMRDIR/SOMEFILE)=0
  #     etc.
  #
  # Args:
  #   dirpath: path to the root-directory
  #   pattern: a regular-expression to match against files (this will be removed from the filepath to form a name)
  #   distFromRoot: an integer, indicating the exact distance to the root-directory that is considered
  #   minHistBin: the minimum intensity value for the histogram (no guessing, must be specified)
  #   maxHistBin: the maximum intensity value for the histogram (no guessing, must be specified)
  #   firstPageOnly: shows only the first-page of any multi-paged plot and then exists
  #   maxPerPage: max number of plots per page
  #

  
  # Find the files recursively
  files = list.files(dirpath,pattern=pattern,recursive=TRUE);
  
  if( length(files)==0 ) {
    stop('No histogram files found');
  }
  
  # Calculate distFromRoot for each file
  distances = str_count(files,'/') ;
  	
  # Narrow the chosen files, to only include those with the correct distFromRoot
  files = files[which(distances==distFromRoot)];
  
  if( length(files)==0 ) {
    stop( sprintf('No histogram files found at distance %d', distFromRoot) );
  }
  
  # We guess a column width
  numFiles = length(files);
    numCols = floor(sqrt(numFiles));
  numRows = ceiling(numFiles / numCols);
  
  # We replace the pattern with empty to make the names look nicer
  niceNames = gsub( pattern, '', files);
  
  plots = vector(mode="list", length=length(numFiles));
  for( i in 1:numFiles ) {
  
    filepath = paste(dirpath,files[i],sep="/");
  
    p = createHistogramPlot(filepath,100,maxHistBin,minHistBin,name=niceNames[i],allowGreaterIntensity=TRUE,allowLesserIntensity=TRUE);
    plots[[i]] = p;
  }
  
  multiplotPaged( plotlist=plots, cols = 4, maxPerPage=maxPerPage, firstPageOnly=firstPageOnly );
}
