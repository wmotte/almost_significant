#!/usr/bin/env Rscript

library( 'ggplot2' )
library( 'plyr' )

############################

###
# Get data, given BF threshold
##
get_data <- function( df, bf, lmin, lmax )
{
    bf_thres <- bf[ ( bf$BayesFactor >= lmin ) & ( bf$BayesFactor < lmax ), ]
    
    # select sentences
    sel <- df[ df$phrase %in% bf_thres$phrase, ]
    sel$phrase <- paste0( '"', sel$phrase, '"' )    
    
    return( sel )
}

####################

# output dir
outdir <- 'out.04.plots'
dir.create( outdir, showWarnings = FALSE )

# prevalence input
df <- read.csv( 'out.01.process/sum_data.csv', row.names = 1 )

# select sentences for main figure
bf <- read.csv( 'out.03.stats/BF_fits.csv', row.names = 1 )

# select hightest Bayes Factor
sel_1 <- get_data( df, bf, 100, 100000000 )
sel_2 <- get_data( df, bf, 10, 100 )
#sel_3 <- get_data( df, bf, 3.2, 10 )

get_wraper <- function(width) {
    function(x) {
        lapply(strwrap(x, width = width, simplify = FALSE), paste, collapse="\n")
    }
}

# main figure
p_1 <- ggplot( data = sel_1, aes( x = labels - 1, y = prev ) ) + 
            geom_point( alpha = 0.2 ) + 
            stat_smooth( method = 'lm', formula = 'y ~ x', se = TRUE, colour = 'orange' ) +
            scale_x_continuous( breaks = c( 1990, 2000, 2010, 2020 ), limits = c( NA, 2020 ) ) +
            ylab( "Prevelance (%)") +
            xlab( 'Publication year' ) +
            facet_wrap( ~phrase, labeller = label_wrap_gen( width = 20 ), ncol = 5, scale = 'free_y' ) +
            theme( strip.text.x = element_text( size = 8, face = 'bold' ),
                    axis.text = element_text( size = 8, colour = 'gray50', angle = 0 ),
                    axis.title = element_text( size = 10, colour = 'gray50', face = 'bold' ) )

# save to disk
outfile <- paste0( outdir, '/Scatterplot__bf-100-1000.png' )
ggsave( file = outfile, plot = p_1, height = 4, width = 8, dpi = 200 )

############ 10 - 100

# main figure
p_2 <- ggplot( data = sel_2, aes( x = labels - 1, y = prev ) ) + 
            geom_point( alpha = 0.2 ) + 
            stat_smooth( method = 'lm', formula = 'y ~ x', se = TRUE, colour = 'orange' ) +
            scale_x_continuous( breaks = c( 1990, 2000, 2010, 2020 ), limits = c( NA, 2020 ) ) +
            ylab( "Prevelance (%)") +
            xlab( 'Publication year' ) +
            facet_wrap( ~phrase, labeller = label_wrap_gen( width = 30 ), ncol = 5, scale = 'free_y' ) +
            theme( strip.text.x = element_text( size = 6, face = 'bold' ),
                    axis.text = element_text( size = 8, colour = 'gray50', angle = 0 ),
                    axis.title = element_text( size = 10, colour = 'gray50', face = 'bold' ) )


# save to disk
outfile <- paste0( outdir, '/Scatterplot__bf-10-100.png' )
ggsave( file = outfile, plot = p_2, height = 7, width = 9, dpi = 200 )




