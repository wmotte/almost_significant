#!/usr/bin/env Rscript

library( 'readr' )		# for read_csv()
library( 'purrr' )		# for map(), reduce()
library( 'plyr' )		# for summarise()
library( 'ggplot2' )

###
# Get proportional CI.
##
get.prop.ci <- function( k, n, lower = TRUE )
{
	if( k > n ) stop( "*** ERROR ***: could not calculate proportional CI as k > n!" )
	
	p <- prop.test( x = k, n = n )
	
	if( lower )
	{
		return( p$conf.int[ 1 ] )
	} else {
		return( p$conf.int[ 2 ] )
	}
}

###
# Get proportions.
##
get_prop_all <- function( df )
{
	comb <- ddply( df, c( "group" ), summarise, K = length( phrase ) )
	comb$N <- sum( comb$K )
	comb$prop <- round( comb$K / comb$N, 3 )
	
	comb$lower <- NA
	comb$upper <- NA
	
	for( i in 1:nrow( comb ) )
	{
		comb[ i, 'lower' ] <- round( get.prop.ci( comb[ i, 'K' ], comb[ i, 'N' ], lower = TRUE ), 3 )
		comb[ i, 'upper' ] <- round( get.prop.ci( comb[ i, 'K' ], comb[ i, 'N' ], lower = FALSE ), 3 )
	}
	
	return( comb )
}

###
# Get median P value.
##
get_median_p <- function( df )
{
	comb <- ddply( df, c( "phrase" ), summarise, median = median( highest_pvalue, na.rm = TRUE ) )
	comb$median <- round( comb$median, 3 )
	return( comb )
}

###
#
##
get_prop_phrases <- function( df )
{
	# get prevalences per phrase
	comb1 <- ddply( df, c( "phrase", "group" ), summarise, K = length( phrase ) )
	comb2 <- ddply( df, c( "phrase" ), summarise, N = length( phrase ) )
	comb <- merge( comb1, comb2 )
	comb$prop <- comb$K / comb$N

	comb$lower <- NA
	comb$upper <- NA
	
	for( i in 1:nrow( comb ) )
	{
		comb[ i, 'lower' ] <- get.prop.ci( comb[ i, 'K' ], comb[ i, 'N' ], lower = TRUE )
		comb[ i, 'upper' ] <- get.prop.ci( comb[ i, 'K' ], comb[ i, 'N' ], lower = FALSE )
	}
	
	return( comb )
}

################################################################################
# END FUNCTIONS
################################################################################


# read all manual labelled P values
dim( df <- read.csv( 'data.manual/all_P_values.csv', row.names = 1 ) )

# get proportions and 95% CIs
props_all <- get_prop_all( df )

# output dir
outdir <- 'out.05.process'
dir.create( outdir, showWarnings = FALSE )

# density plot [all]
p_d <- ggplot( data = df, aes( x = highest_pvalue ) ) + 
	geom_density( adjust = 1.3, size = 0.8 ) + 
	geom_vline( xintercept = c( 0.01 ), linetype = "dotted" ) + 
	geom_vline( xintercept = c( 0.05 ), linetype = "dotted" ) + 
	scale_x_continuous( breaks = c( 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5 ), limits = c( 0, 0.5 ) ) +
	scale_y_continuous( breaks = c( 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50 ), limits = c( 0, 25 ) ) + 
	ylab( 'Extracted (%)' ) + 
	xlab( "P value" ) +
	theme( strip.text.x = element_text( size = 6, face = 'bold' ),
		   axis.text.x = element_text( angle = 45, hjust = 1 ),
		   axis.text = element_text( size = 8, colour = 'gray50', angle = 0 ),
		   axis.title = element_text( size = 10, colour = 'gray50', face = 'bold' ) ) 
# write to file	
outfile <- paste0( outdir, '/density_P_values__all.png' )
ggsave( file = outfile, plot = p_d, height = 5, width = 4, dpi = 200 )
outfile_csv <- paste0( outdir, '/density_P_values__all.csv' )
write.csv( props_all, file = outfile_csv )

######################################

###
props_phrases <- get_prop_phrases( df )

# get median P values per phrase
meds <- get_median_p( df )
props_phrases <- merge( props_phrases, meds )

props_phrases[ props_phrases$group != '>=0.05-0.15', 'median' ] <- NA

# remove all phrases with <100 P values
dim( props_phrases_100 <- props_phrases[ props_phrases$N >= 100, ] )
dim( props_phrases_30_99 <- props_phrases[ props_phrases$N >= 30 & props_phrases$N < 100, ] )

### 100 ##############
# bar plot [all]
p_bar_phrase_100 <- ggplot( props_phrases_100, aes( x = group, y = 100 * prop, ymin = 100 * lower, ymax = 100 * upper, fill = group ) ) + 
	geom_col( color = 'gray40' ) +
	geom_errorbar( color = 'gray40', width = 0.5 ) +
	geom_label( aes( label = median, fontface = 2 ), x = 1, y = 96, color = 'gray60', fill = NA, size = 1.5 ) +
	xlab( "Category" ) +
	ylab( "Extracted (%)" ) + 
	scale_fill_manual( values = c( "#999999", "#E69F00", "#56B4E9" ) ) +
	scale_y_continuous( breaks = c( 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100 ) ) + 
	facet_wrap( ~phrase, labeller = label_wrap_gen( width = 20 ), ncol = 5 ) +
	theme( legend.position = 'none', strip.text.x = element_text( size = 6, face = 'bold' ),
		   axis.text.x = element_text( angle = 45, hjust = 1 ),
		   axis.text = element_text( size = 8, colour = 'gray50', angle = 0 ),
		   axis.title = element_text( size = 10, colour = 'gray50', face = 'bold' ) )

# write to file	
outfile <- paste0( outdir, '/range_P_values__phrases_100.png' )
ggsave( file = outfile, plot = p_bar_phrase_100, height = 9, width = 5, dpi = 300 )
outfile_csv <- paste0( outdir, '/range_P_values__phrases_100.csv' )
write.csv( props_phrases, file = outfile_csv )


### 30-99 ##############
# bar plot [all]
p_bar_phrase_30_99 <- ggplot( props_phrases_30_99, aes( x = group, y = 100 * prop, ymin = 100 * lower, ymax = 100 * upper, fill = group ) ) + 
	geom_col( color = 'gray40' ) +
	geom_errorbar( color = 'gray40', width = 0.5 ) +
	geom_label( aes( label = median, fontface = 2 ), x = 1, y = 96, color = 'gray60', fill = NA, size = 1.5 ) +
	xlab( "Category" ) +
	ylab( "Extracted (%)" ) + 
	scale_fill_manual( values = c( "#999999", "#E69F00", "#56B4E9" ) ) +
	scale_y_continuous( breaks = c( 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100 ) ) + 
	facet_wrap( ~phrase, labeller = label_wrap_gen( width = 20 ), ncol = 5 ) +
	theme( legend.position = 'none', strip.text.x = element_text( size = 6, face = 'bold' ),
		   axis.text.x = element_text( angle = 45, hjust = 1 ),
		   axis.text = element_text( size = 8, colour = 'gray50', angle = 0 ),
		   axis.title = element_text( size = 10, colour = 'gray50', face = 'bold' ) )

# write to file	
outfile <- paste0( outdir, '/range_P_values__phrases_30_99.png' )
ggsave( file = outfile, plot = p_bar_phrase_30_99, height = 9, width = 5, dpi = 300 )
outfile_csv <- paste0( outdir, '/range_P_values__phrases_30_99.csv' )
write.csv( props_phrases, file = outfile_csv )

