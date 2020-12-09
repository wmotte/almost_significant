#!/usr/bin/env Rscript
# Map prevalence of phrases.
############################

library( 'stringr' )

###
# Get intput xmls
##
get_input_xmls <- function( indir )
{
	infile <- dir( indir, pattern = "*.tei.xml" )
	pmid <- stringr::str_split_fixed( infile, '.tei.xml', 2 )[ , 1 ]
	path <- paste0( indir, '/', infile )
	
	out <- data.frame( pmid = pmid, infile = infile, path = path )
	return( out )
}

###
# Extract info
##
extract_info <- function( pmid, data, phrase, extra_chars_length )
{
	# extract phrase
	total_length <- str_length( data )
	
	# get all locations
	locations <- data.frame( str_locate_all( data, phrase ) )
	
	if( nrow( locations ) == 0 )
		return( locations )
	
	locations$extracted <- '<insufficient data error>'
	
	for( i in 1:nrow( locations ) )
	{
		location <- locations[ i, ]
		
		if( ( location[ 1 ] + extra_chars_length ) < total_length ) {
			locations[ i, 'extracted' ] <- str_sub( data, location[ 2 ] + 1, location[ 2 ] + extra_chars_length )
		}
	}
	
	locations$pmid <- pmid
	locations$phrase <- phrase
	locations$start <- NULL
	locations$end <- NULL
	
	return( locations )	
}

##############################
# END FUNCTIONS
##############################

# extract extra words after detected phrase.
extra_chars_length <- 100

# output directory
outdir <- 'out.prev'
dir.create( outdir, showWarnings = FALSE )

# get phrases
phrases_file <- 'phrases_505.csv'
phrases <- stringr::str_trim( tolower( read.csv( phrases_file, sep = ';' )$phrases ) )
#ophrases <- get_phrases()

head( phrases )
tail( phrases )

# input directory
indir <- 'input.xmls'

# input xmls files
input <- get_input_xmls( indir )


# extract info 
for( i in 1:nrow( input ) )
{
	pmid <- input[ i, 'pmid' ]
	path <- as.character( input[ i, 'path' ] )

	print( path )
	# read text file into single char and convert to lower case
	data <- tolower( paste( readLines( file( path ), warn = FALSE ), collapse = " " ) )
	
	all <- NULL
	for( j in 1:length( phrases ) )
	{
		phrase <- phrases[ j ]
		tmp <- extract_info( pmid, data, phrase, extra_chars_length )
		if( nrow( tmp ) != 0 )
			all <- rbind( all, tmp )
	}
	
	# sort columns
	all <- all[ ,c( 'pmid', 'phrase', 'extracted' ) ]

	if( !is.null( all ) ) {	
		outfile <- paste0( outdir, '/extracted_', pmid, '.csv' )
		write.csv( all, outfile )
	}
}
	
