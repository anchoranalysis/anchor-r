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

guessImageDepth <- function( maxObservedIntensity ) {
  # Guesses the bit depth of the image image from the maxObservedIntensity in the available histogram counts.
  #
  # Assumes image is one of the following types:
  #          unsigned 8-bit (8)
  #          unsigned 16-bit (16)
  #          unsigned 32-bit (32)
  #
  # Args:
  #   maxObservedIntensity: the maximum-intensity-value that has been observed
  #
  # Returns:
  #   the bit depth (8,16,32 etc.) as per above.
  
  if (maxObservedIntensity < 2^8 ) {
    return(8);
  } else if (maxObservedIntensity < 2^12 ) {
    return(12);
  } else if (maxObservedIntensity < 2^16 ) {
    return(16);
  }
}

readHistogram <- function( filepath ) {
  #  Reads an Anchor-outputted csv for a histogram, and returns a frequency-table
  #
  # Args:
  #   filepath: path to a CSV file
  read.csv(filepath);
}


createHistogramPlot <- function( filepath, numBins=256, maxHistBin=-1, minHistBin=0, name='Untitled', allowGreaterIntensity=FALSE, allowLesserIntensity=FALSE, xAxisLabel='intensity' ) {
  # Reads an Anchor-outputted csv for a histogram, and creates a plot
  #
  # Args:
  #   filepath: path to a CSV file
  #   numBins: the approximate number of bins for the histogram
  #   maxHistBin: if -1, it is ignored, otherwise it is the value of the maximum-intensity-bin in the histogram
  #   minHistBin: It is the value of the minimum-intensity-bin in the histogram, defaulting to 0
  #   name: a name for the histogram
  #   allowGreaterIntensity: if TRUE, we ignore situations where there is intensity greater than maxHistBin, if FALSE, we throw an error when it occurs
  #   allowLesserIntensity: if TRUE, we ignore situations where there is intensity lower than minHistBin, if FALSE, we throw an error when it occurs
  #
  # Returns:
  #   the plot object from ggplot2
	
  # Frequency table
  freqTable = readHistogram(filepath);
	
  # Get a maximum-intensity
  maxIntensity = max(freqTable$intensity)
  minIntensity = min(freqTable$intensity)
	
  # We guess an upper intensity for the histogram, if none is passed
  maxHistBin = .guessHistBinIfNeeded(maxHistBin, maxIntensity);
	
  # Error check to and trimming for intensity values above the maximum and below the minimum
  freqTable = .trimMaxFreqTable(freqTable, maxIntensity, maxHistBin, allowGreaterIntensity);
  freqTable = .trimMinFreqTable(freqTable, minIntensity, minHistBin, allowLesserIntensity);

  if (nrow(freqTable)==0) {
    return( .createEmptyPlot(minHistBin, maxHistBin, name) );
  }

  freqTable$bin = .createBins( freqTable$intensity, maxHistBin, numBins ); 

  plotHistogramFromFrequencyTable(freqTable, maxHistBin, minHistBin, name, xAxisLabel=xAxisLabel);
}

.createBins <- function( intensity, maxHistBin, numBins ) {
  binWidthFloat = (maxHistBin+1)/numBins;
  binWidth = ceiling( binWidthFloat );
  
  floor(intensity/binWidth); 
}

.guessHistBinIfNeeded <- function(maxHistBin, maxIntensity) {
  if (maxHistBin==-1) {
    guessedImageDepth = guessImageDepth( maxIntensity );
    (2^guessedImageDepth)-1;
  } else {
    maxHistBin;
  }
}

.trimMaxFreqTable <- function(freqTable, maxIntensity, maxHistBin, allowGreaterIntensity) {
  if (maxIntensity>maxHistBin) {
    
    if (allowGreaterIntensity) {
      freqTable[ freqTable$intensity<= maxHistBin, ];
    } else {
      stop( sprintf("maxIntensity is %d but maxHistBin is %d", maxIntensity, maxHistBin) );
    }
    
  } else {
    # The freqTable isunchanged
    freqTable;
  }
}
 
.trimMinFreqTable <- function(freqTable, minIntensity, minHistBin, allowLesserIntensity) {
  
  if (minIntensity<minHistBin) {
    
    if (allowLesserIntensity) {
      freqTable = freqTable[ freqTable$intensity>= minHistBin, ];
    } else {
      stop( sprintf("minIntensity is %d but minHistBin is %d", minIntensity, minHistBin) );
    }
  } else {
    # The freqTable isunchanged
    freqTable;
  }
}

.createEmptyPlot <- function(minHistBin, maxHistBin, name) {
  # We simply return an empty plot, if there's nothing to show
  df <- data.frame()
  p = ggplot2::ggplot(df) + geom_point() + xlim(minHistBin, maxHistBin) + ylim(0, 100)  + ggtitle(name) + scale_y_continuous(labels = scientific) +
    theme(plot.title = element_text(lineheight=.8, face="bold"));
  return(p);
}


