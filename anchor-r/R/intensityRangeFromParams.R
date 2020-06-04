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

intensityRangeFromParams <- function( params ) {
  high = clusterFromParams( params, "intensityHigh");
  highFixed = clusterFromParams( params, "intensityHighFixed");
  low = clusterFromParams( params, "intensityLow");
  mean = clusterFromParams( params, "intensityMean");
  meanMax = clusterFromParams( params, "intensityMeanMax");
  scale = clusterFromParams( params, "intensityScale");
  shellDiff = clusterFromParams(params,"intensityClusterRangeShellDiff");
  
  # We record these two clusters
  return( list(
    high=high,
    highFixed=highFixed,
    low=low,
    mean=mean,
    meanMax=meanMax,
    scale=scale,
    shellDiff=shellDiff
  ));
}





