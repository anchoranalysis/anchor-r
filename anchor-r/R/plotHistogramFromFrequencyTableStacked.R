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

plotHistogramFromFrequencyTableStacked <- function( freqTable, maxHistBin, minHistBin, name, xAxisLabel, extraFun=NULL ) {
  # Plots a histogram from a frequency table.
  #
  #
  # Args:
  #   freqTable: data.frame or matrix. assumes first column are values, second column are counts, third column are factors to be stacked.
  #   maxHistBin: if -1, it is ignored, otherwise it is the value of the maximum-intensity-bin in the histogram
  #   minHistBin: It is the value of the minimum-intensity-bin in the histogram, defaulting to 0
  #   name: a name for the histogram
  #   xAxisLabel: a label for the xaxis
  #   extraFun: an extra-function that is plotted on top of the histogram
  #
  # Returns:
  #   the plot object from ggplot2
  
  # Lets make sure we have the right column names
  colnames(freqTable)[1] = 'val';
  colnames(freqTable)[2] = 'count';  
  colnames(freqTable)[3] = 'fill';

  stopifnot(!is.na(freqTable));
  p = (ggplot(data=freqTable,mapping=aes(x=val,y=count,fill=fill))
       + geom_histogram(stat='identity')  
       + coord_cartesian(xlim = c(minHistBin, maxHistBin))
       + ggtitle(name)
       + scale_y_continuous(labels = scientific)
       + xlab(xAxisLabel)
       + theme(plot.title = element_text(lineheight=.8, face="bold")));
  if (!is.null(extraFun)) {
    p = p + list(stat_function(fun = extraFun, colour = "blue"))
  }
  return(p);
}