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

plotFeatureCorrelationHeatmap <- function( features ) {
  #
  # Displays a heat-map with the correlations between feeatures in the feature table
  #
  # Args:
  #   features: a data.frame of features (e.g. from readFeaturesCSV() )
  #
  c = cor(features, use="p");
  p = qplot(x=Var1, y=Var2, data=melt(c), fill=value, geom="tile") + scale_fill_gradient2(limits=c(-1, 1));
  print(p);
}