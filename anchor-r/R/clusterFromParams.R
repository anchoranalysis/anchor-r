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

clusterFromParams <- function( params, prefix ) {
  clusterOut = c();
  
  extractIndParam <- function( params, prefix, name ) {
    params[[ paste(prefix,'_',name,sep='') ]];		
  }
  
  clusterOut$max = extractIndParam(params,prefix,'max');
  clusterOut$min = extractIndParam(params,prefix,'min');
  clusterOut$peakMax = extractIndParam(params,prefix,'peakMax');
  clusterOut$peakMode = extractIndParam(params,prefix,'peakMode');
  clusterOut$mean = extractIndParam(params,prefix,'mean');
  clusterOut$sd = extractIndParam(params,prefix,'sd');
  
  clusterOut;
}
