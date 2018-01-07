if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )
chis_username <- Sys.getenv( "chis_username" )
chis_password <- Sys.getenv( "chis_password" )
library(lodown)
lodown( "chis" , output_dir = file.path( getwd() ) , 
	your_username = chis_username , 
	your_password = chis_password )
library(lodown)
# examine all available CHIS microdata files
chis_cat <-
	get_catalog( "chis" ,
		output_dir = file.path( getwd() ) , 
		your_username = chis_username , 
		your_password = chis_password )

# 2014 only
chis_cat <- subset( chis_cat , year == 2014 )
# download the microdata to your local computer
lodown( "chis" , chis_cat , 
	your_username = chis_username , 
	your_password = chis_password )

options( survey.replicates.mse = TRUE )

library(survey)

child <- readRDS( file.path( getwd() , "2014 child.rds" ) )

child$ak7_p1 <- child$ak10_p <- NA
child$agecat <- "1 - child"
child$no_usual_source_of_care <- as.numeric( child$cd1 == 2 )

# four-category srhs (excellent / very good / good / fair+poor)
child$hlthcat <- child$ca6_p1

# load adolescents ages 12-17
teen <- readRDS( file.path( getwd() , "2014 teen.rds" ) )

teen$ak7_p1 <- teen$ak10_p <- NA
teen$agecat <- "2 - adolescent"
teen$no_usual_source_of_care <- as.numeric( teen$tf1 == 2 )

# four-category srhs (excellent / very good / good / fair+poor)
teen$hlthcat <- teen$tb1_p1

# load adults ages 18+
adult <- readRDS( file.path( getwd() , "2014 adult.rds" ) )

adult$agecat <- ifelse( adult$srage_p1 >= 65 , "4 - senior" , "3 - adult" )
adult$no_usual_source_of_care <- as.numeric( adult$ah1 == 2 )

# four-category srhs (excellent / very good / good / fair+poor)
adult$hlthcat <- c( 1 , 2 , 3 , 4 , 4 )[ adult$ab1 ]

# construct a character vector with only the variables needed for the analysis
vars_to_keep <- 
	c( grep( "rakedw" , names( adult ) , value = TRUE ) , 
		'hlthcat' , 'agecat' , 'ak7_p1' , 'ak10_p' ,
		'povgwd_p' , 'no_usual_source_of_care' )

chis_df <- 
	rbind( 
		child[ vars_to_keep ] , 
		teen[ vars_to_keep ] , 
		adult[ vars_to_keep ] 
	)

# remove labelled classes
labelled_cols <- 
	sapply( 
		chis_df , 
		function( w ) class( w ) == 'labelled' 
	)

chis_df[ labelled_cols ] <- 
	sapply( 
		chis_df[ labelled_cols ] , 
		as.numeric
	)

chis_design <- 
	svrepdesign( 
		data = chis_df , 
		weights = ~ rakedw0 , 
		repweights = "rakedw[1-9]" , 
		type = "other" , 
		scale = 1 , 
		rscales = 1 , 
		mse = TRUE 
	)
chis_design <- 
	update( 
		chis_design , 
		one = 1 ,
		hlthcat = 
			factor( hlthcat , 
				labels = c( 'excellent' , 'very good' , 'good' , 'fair or poor' ) 
			)
	)
sum( weights( chis_design , "sampling" ) != 0 )

svyby( ~ one , ~ hlthcat , chis_design , unwtd.count )
svytotal( ~ one , chis_design )

svyby( ~ one , ~ hlthcat , chis_design , svytotal )
svymean( ~ povgwd_p , chis_design )

svyby( ~ povgwd_p , ~ hlthcat , chis_design , svymean )
svymean( ~ agecat , chis_design )

svyby( ~ agecat , ~ hlthcat , chis_design , svymean )
svytotal( ~ povgwd_p , chis_design )

svyby( ~ povgwd_p , ~ hlthcat , chis_design , svytotal )
svytotal( ~ agecat , chis_design )

svyby( ~ agecat , ~ hlthcat , chis_design , svytotal )
svyquantile( ~ povgwd_p , chis_design , 0.5 )

svyby( 
	~ povgwd_p , 
	~ hlthcat , 
	chis_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE 
)
svyratio( 
	numerator = ~ ak10_p , 
	denominator = ~ ak7_p1 , 
	chis_design ,
	na.rm = TRUE
)
sub_chis_design <- subset( chis_design , agecat == "4 - senior" )
svymean( ~ povgwd_p , sub_chis_design )
this_result <- svymean( ~ povgwd_p , chis_design )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ povgwd_p , 
		~ hlthcat , 
		chis_design , 
		svymean 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( chis_design )
svyvar( ~ povgwd_p , chis_design )
# SRS without replacement
svymean( ~ povgwd_p , chis_design , deff = TRUE )

# SRS with replacement
svymean( ~ povgwd_p , chis_design , deff = "replace" )
svyciprop( ~ no_usual_source_of_care , chis_design ,
	method = "likelihood" )
svyttest( povgwd_p ~ no_usual_source_of_care , chis_design )
svychisq( 
	~ no_usual_source_of_care + agecat , 
	chis_design 
)
glm_result <- 
	svyglm( 
		povgwd_p ~ no_usual_source_of_care + agecat , 
		chis_design 
	)

summary( glm_result )
library(srvyr)
chis_srvyr_design <- as_survey( chis_design )
chis_srvyr_design %>%
	summarize( mean = survey_mean( povgwd_p ) )

chis_srvyr_design %>%
	group_by( hlthcat ) %>%
	summarize( mean = survey_mean( povgwd_p ) )
stopifnot( round( coef( svytotal( ~ one , chis_design ) ) , -3 ) == 37582000 )
( total_population_ex_vg_good <- svymean( ~ hlthcat , chis_design ) )

# confirm these match
stopifnot( 
	identical( 
		as.numeric( round( coef( total_population_ex_vg_good ) * 100 , 1 )[ 1:3 ] ) ,
		c( 23.2 , 31.4 , 28.4 )
	)
)
( total_pop_ci <- confint( total_population_ex_vg_good , df = degf( chis_design ) ) )

# confirm these match
stopifnot(
	identical(
		as.numeric( 
			round( total_pop_ci * 100 , 1 )[ 1:3 , ] 
		) ,
		c( 22.1 , 30.1 , 27.1 , 24.2 , 32.7 , 29.6 )
	)
)
