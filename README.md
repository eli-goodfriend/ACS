This is a collection of R scripts I've used to play with the American Community Survey dataset. To use them, you'll need to download the 2015 PUMS dataset and, if you want to make maps, the associated shapefiles.

preprocess contains scripts I use to preprocess PUMS and shapefile data so it is useful to me
	saving_ACS_data.R reduces the enormous PUMS files to a single dataframe of data I want
	merge_shapefiles.R reduces the by-state PUMA shapefiles to a single dataframe for plotting

use_plot_by_PUMA.R calls plot_by_PUMA.R to make maps of the percent of people in a region who meet some criteria specified in use_plot_by_PUMA.R

ANOVA_of_income.R is a learning exercise in statistical testing


unimportant folders:
archive contains scripts that are broken but I'm hanging onto for sentimental / completeness reasons. 
half-bakery contains scripts that are broken because they aren't done yet, but which I am not actively working on
