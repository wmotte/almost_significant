#!/usr/bin/env Rscript

library( 'plyr' )
library( 'stringr' ) 
library( 'ggplot2' )
library( 'cowplot' )

############################

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
# Get overall prevalence
##
get_prev_overall <- function( sum_phrases, sum_trials )
{
    ( nhits <- sum( sum_phrases$K ) )  
    ( ntrials <- sum( sum_trials$N ) )  
    ( nhits / ntrials ) * 100        
    
    prev.avg <- round( 100 * ( nhits / ntrials ), 2 )
    prev.lower <- round( 100 * get.prop.ci( k = nhits, n = ntrials, lower = TRUE ), 2 )
    prev.upper <- round( 100 * get.prop.ci( k = nhits, n = ntrials, lower = FALSE ), 2 )
    
    prev.overall <- data.frame( N = sum( sum_trials$N ), K = sum( sum_phrases$K ), 
        prev.avg = prev.avg, prev.lower = prev.lower, prev.upper = prev.upper )

    return( prev.overall )
}

###
# hist. n trials.
##
plot_n_trials <- function( df, outdir )
{
	# Change the width of bins
    p <- ggplot( df, aes( x = year ) ) + 
        geom_histogram( binwidth = 1, color = 'gray40', fill = 'orange' ) +
        xlab( "Year of publication" ) +
        ylab( "Count (trials)" ) + 
        scale_y_continuous( breaks = seq( 0, 30000, 2500 ) ) +
        scale_x_continuous( breaks = c( 1990, 1995, 2000, 2005, 2010, 2015, 2020 ) ) +
        theme( strip.text.x = element_text( size = 6, face = 'bold' ),
            axis.text = element_text( size = 8, colour = 'gray50', angle = 0 ),
            axis.title = element_text( size = 10, colour = 'gray50', face = 'bold' ) )
    
    return( p )
}

###
# plot hist of phrases
##
plot_n_phrases <- function( m, outdir )
{
	# remove duplicates
	m$tmp <- paste0( m$phrase, m$pmid )
	m_no_dupl <-  m[ !duplicated( m$tmp ), ]
	
	   # Change the width of bins
    p <- ggplot( m_no_dupl, aes( x = year ) ) + 
        geom_histogram( binwidth = 1, color = 'gray40', fill = 'orange' ) +
        xlab( "Year of publication" ) +
        ylab( "Phrase-positive trials (N)" ) + 
        scale_y_continuous( breaks = seq( 0, 2800, 250 ) ) +
        scale_x_continuous( breaks = c( 1990, 1995, 2000, 2005, 2010, 2015, 2020 ) ) +
        theme( strip.text.x = element_text( size = 6, face = 'bold' ),
            axis.text = element_text( size = 8, colour = 'gray50', angle = 0 ),
            axis.title = element_text( size = 10, colour = 'gray50', face = 'bold' ))

    return( p )
}

###
# Prevalences
##
plot_prevalence <- function( df, m, outdir )
{
    s1 <- ddply( df, c( "year" ), summarise, N = length( pmid ) )
    s2 <- ddply( m, c( "year" ), summarise, K = length( unique( pmid ) ) )

    comb <- merge( s1, s2 )
    comb$perc <- 100 * ( comb$K / comb$N )
    
    comb$lower <- NA
    comb$upper <- NA
    
    for( i in 1:nrow( comb ) )
    {
        comb[ i, 'lower' ] <- 100 * get.prop.ci( comb[ i, 'K' ], comb[ i, 'N' ], lower = TRUE )
        comb[ i, 'upper' ] <- 100 * get.prop.ci( comb[ i, 'K' ], comb[ i, 'N' ], lower = FALSE )
    }
    
    p <- ggplot( comb, aes( x = year, y = perc, ymin = lower, ymax = upper) ) + 
        geom_errorbar( color = 'gray40', width = 0.6 ) +
        geom_col( color = 'gray40', fill = '#31a354' ) +
        xlab( "Year of publication" ) +
        ylab( "Prevalence (%)" ) + 
        scale_y_continuous( breaks = c( 0, 2, 4, 6, 8, 10, 12, 14 ) ) + 
        scale_x_continuous( breaks = c( 1990, 1995, 2000, 2005, 2010, 2015, 2020 ) ) +
        theme( strip.text.x = element_text( size = 6, face = 'bold' ),
            axis.text = element_text( size = 8, colour = 'gray50', angle = 0 ),
            axis.title = element_text( size = 10, colour = 'gray50', face = 'bold' ) )

    return( p )
}

###
# Plot combined
##
plot_combined_figure <- function( df, m, outdir, p_N, p_K, p_R )
{
    # adjust dims
    p_N2 <- p_N + coord_cartesian( xlim = c( 1990, 2020 ), ylim = c( 0, 33000 ) )
    p_K2 <- p_K + coord_cartesian( xlim = c( 1990, 2020 ), ylim = c( 0, 3100 ) )
    p_R2 <- p_R + coord_cartesian( xlim = c( 1990, 2020 ), ylim = c( 0, 13 ) )
    
    # arrange the three plots in a single row
    p <- plot_grid(
    	p_N2 + theme( legend.position = "none", axis.title = element_blank() ) + 
            annotate("text", -Inf, Inf, label = "  Full texts (N)", hjust = 0, vjust = 2, size = 4, fontface = 2, colour = 'gray40' ),
    	p_K2 + theme( legend.position = "none", axis.title = element_blank() ) + 
            annotate("text", -Inf, Inf, label = "  Phrase-positive trials (N)", hjust = 0, vjust = 2, size = 4, fontface = 2, colour = 'gray40' ),
    	p_R2 + theme( legend.position = "none", axis.title = element_blank() ) + 
            annotate("text", -Inf, Inf, label = "  Prevalence (%)", hjust = 0, vjust = 2, size = 4, fontface = 2, colour = 'gray40' ),
    	
    	align = 'vh',
    	labels = c( "A", "B", "C" ),
    	label_size = 10,
    	hjust = -1,
        vjust = 2,
    	nrow = 3
    )

    # save to disk
    save_plot( paste0( outdir, "/Figure_1.png" ), p, base_height = NULL, base_width = 6, base_asp = 0.8 )  
}

#############################
# END FUNCTIONS
#############################

outdir <- 'out.01.process'
dir.create( outdir, showWarnings = FALSE )

# get trial data
dim( df <- read.csv( 'data.xmls/data_rcts.csv.gz', row.names = 1, stringsAsFactors = FALSE ) )

# phrases
dim( phrases <- read.csv( 'data.xmls/data_phrases.csv.gz', row.names = 1, stringsAsFactors = FALSE ) )

# merge with trials
dim( m <- merge( df, phrases ) )

################################

# stats, N phrases
sum_phrases <- ddply( m, c( "labels", "phrase" ), summarise, K = length( phrase ) )

# stats, N pmids
sum_phrases_pmids <- ddply( m, c( "labels", "phrase" ), summarise, K = length( unique( pmid ) ) )

# N pmids [total]
sum_pmids <- ddply( df, c( "labels" ), summarise, N = length( unique( pmid ) ) )

# get overall prevalence and 95% proportional confidence interval
prev.overall <- get_prev_overall( sum_phrases_pmids, sum_pmids )
write.csv( prev.overall, file = paste0( outdir, '/overall_prevalence__pmids.csv' ) )


# prevalence per phrase
out <- merge( sum_phrases_pmids, sum_pmids )
out$prev <- 100 * ( out$K / out$N )

# write to file
write.csv( sum_phrases, file = paste0( outdir, '/sum_phrases.csv' ) )
write.csv( sum_phrases_pmids, file = paste0( outdir, '/sum_phrases_pmids.csv' ) )
write.csv( sum_pmids, file = paste0( outdir, '/sum_pmids.csv' ) )
write.csv( out, file = paste0( outdir, '/sum_data.csv' ) )

############################
# plots

# plot hist of n-trials
p_N <- plot_n_trials( df, outdir )
    
# plot hist of phrases
p_K <- plot_n_phrases( m, outdir )
    
# plot prevalence
p_R <- plot_prevalence( df, m, outdir )

# combine
plot_combined_figure( df, m, outdir, p_N, p_K, p_R )

