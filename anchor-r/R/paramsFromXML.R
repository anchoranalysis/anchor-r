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

paramsFromXml <- function( pathFolder1, pathFolder2 ) {
  
  path = paste(pathFolder1,pathFolder2,'paramsGroupAgg.xml',sep='\\');
  
  if (!file.exists(path)) {
    return(NULL);
  }
  
  xml = xmlParse( path );
  
  entryList = xmlToList(xml);
  
  params = list();
  for( i in 1:length(entryList)) {
    
    # Checks if there is actual text for the entry.
    if ("text" %in% names(entryList[i]$entry)) {
      params[[entryList[i]$entry$.attrs]]=as.double(entryList[i]$entry$text);
    } else {
      # If there's no text set set it as NaN
      #params[[entryList[i]$entry$.attrs]]= NaN;
    }
  }
  
  #print(params);
  
  return(params);
}
