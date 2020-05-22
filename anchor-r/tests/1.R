test.createhistogram <- function()
{
  	path = file.path( "data/hist1.csv" );
  	
  	hist = readHistogram( path );
  	
    checkEqualsNumeric(hist$counts[400], 1125);
    checkEqualsNumeric(round(hist$density[400],digits=7), 2.71e-05);
    checkEquals(class(hist),"histogram");
    checkException(extractHist( path, 100 ), 'Unable to extractHist with a maxBinSize of 100')
}
 
test.guessimagedepth <- function() {
	checkEqualsNumeric( guessImageDepth( 234 ), 8 );
	checkEqualsNumeric( guessImageDepth( 256 ), 12 );
	checkEqualsNumeric( guessImageDepth( 12256 ), 16 );
}

test.clusteringMeanShift <- function() {
  # We create artificial data
  all = load('data/mixtureOfGaussians.ser');
  
  res = clusterMeanShift( all, h=0.1 );
  
  checkEqualsNumeric( length( res$cluster_centers ), 2 );
  c1 = res$cluster_centers[1];
  c2 = res$cluster_centers[2];
  
  checkTrue( c1 > 2.5 );
  checkTrue( c1 < 3.5 );
  
  checkTrue( c2 > 8.5 );
  checkTrue( c2 < 9.5 );
}