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

multiplot <- function(..., plotlist=NULL, cols) {
  # Displays many plots side-by-side in a grid
  #
  # Args:
  #   ...: saved-plot objects
  #   plotlist: a list of saved-plots
  #   cols: the number of columns in the grid
  #
  # Returns:
  #   nothing
  #
  # Based upon:
  # http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/9

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
	
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)

  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
}



multiplotPaged <- function(plotlist=NULL, cols, maxPerPage=20, firstPageOnly=FALSE) {
  # Displays many plots side-by-side in a grid
  #
  # Args:
  #   maxPerPage: maximum number of objects in a single plot
  #   plotlist: a list of saved-plots
  #   cols: the number of columns in the grid
  #   firstPageOnly: only shows the first page of plots, ignores any subsequent plots
  #
  # Returns:
  #   nothing
  #

  numNextPage = min( length(plotlist), maxPerPage );	
  while( TRUE ) {
	
    plotsSub = plotlist[1:numNextPage];
    plotlist[1:numNextPage] = NULL;
	 	
    multiplot( plotlist=plotsSub, cols=cols );
		
    numNextPage = min( length(plotlist), maxPerPage );
	  if( numNextPage == 0 || firstPageOnly==TRUE) {
		  break;
	  }    
    
    cat ("Press [enter] to continue");
    line <- readline();
  }
}
