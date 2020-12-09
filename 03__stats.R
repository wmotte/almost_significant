#!/usr/bin/env Rscript

library( 'BayesFactor' )
library( 'ggplot2' )

##################################
# FUNCTIONS
##################################

###
# Get linear regression output
##
get_out <- function( s, phrase )
{
    # fit lm
    sum <- summary( model <- lm( prev ~ labels, data = s ) )
    
    out1 <- data.frame( sum$coefficients )
    out2 <- data.frame( confint( model ) )
    out <- cbind( out1, out2 )
    out$phrase <- phrase
    
    colnames( out ) <- c( 'Estimate', 'SE', 'T', 'P', 'CI_lower', 'CI_upper', 'phrase' )
    out <- out[ 2, ]
    
    return( out )
}

###
# Get bayes factor of model with slope versus model without slope
##
get_BF <- function( s, phrase )
{
    s$int <- 1
    bf_null = regressionBF( prev ~ int, data = s )
    bf_slope = regressionBF( prev ~ labels, data = s )

    bf_out <- bf_slope / bf_null  
    bf <- exp( bf_out@bayesFactor$bf )
    out <- data.frame( BayesFactor = bf, phrase = phrase )
}

##################################
# END FUNCTIONS
##################################

set.seed( 555 )

# output dir
outdir <- 'out.03.stats'
dir.create( outdir, showWarnings = FALSE )

# prevalence input
df <- read.csv( 'out.01.process/sum_data.csv', row.names = 1 )

all <- NULL
all_bf <- NULL

length( phrases <- unique( df$phrase ) )
length( phrases <- phrases[ phrases != 'quite significant' ] ) # sub-phrase

for( phrase in phrases )
{
    df.sub <- df[ df$phrase %in% phrase, ]

    if( nrow( df.sub ) >= 5 )
    {
        print( phrase )
        # get data
        out.sub <- get_out( df.sub, phrase )
        all <- rbind( all, out.sub )
        
        out.sub.bf <- get_BF( df.sub, phrase )
        all_bf <- rbind( all_bf, out.sub.bf )
        
    }
}

# write all to file
write.csv( all, file = paste0( outdir, '/lm_fits.csv' ) )

# create table
tab <- all_bf[ sort.int( all_bf$BayesFactor, index = TRUE, decreasing = TRUE )$ix, ]
rownames( tab ) <- NULL
tab$BayesFactor <- round( tab$BayesFactor, 3 )
write.csv( tab, file = paste0( outdir, '/BF_fits.csv' ) )

 
