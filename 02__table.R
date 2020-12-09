#!/usr/bin/env Rscript

library( 'ggplot2' )
library( 'plyr' )

############################

# output dir
outdir <- 'out.02.table'
dir.create( outdir, showWarnings = FALSE )

# prevalence input
df <- read.csv( 'out.01.process/sum_data.csv', row.names = 1 )

# determine average prevalence and group accordingly
avg <- ddply( df, c( "phrase" ), summarise, total_trials = sum( K ) )

# create table
tab <- avg[ sort.int( avg$total_trials, index = TRUE, decreasing = TRUE )$ix, ]
rownames( tab ) <- NULL
colnames( tab ) <- c( 'Phrase', 'Total trials' )

# write to disk
write.csv( tab, file = paste0( outdir, '/identified_phrases_K.csv' ) )

 
