if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )
chis_username <- Sys.getenv( "chis_username" )
chis_password <- Sys.getenv( "chis_password" )
library(lodown)
# examine all available CHIS microdata files
chis_cat <-
	get_catalog( "chis" ,
		output_dir = file.path( getwd() ) , 
		your_username = chis_username , 
		your_password = chis_password )

# 2015 only
chis_cat <- subset( chis_cat , year == 2015 )
# download the microdata to your local computer
stopifnot( nrow( chis_cat ) > 0 )



options( survey.replicates.mse = TRUE )

library(survey)

child <- readRDS( file.path( getwd() , "2015 child.rds" ) )

child$ak3_p1 <- child$ak10_p <- NA
child$agecat <- "1 - child"
child$no_usual_source_of_care <- as.numeric( child$cd1 == 2 )

# four-category srhs (excellent / very good / good / fair+poor)
child$hlthcat <- child$ca6_p1

# load adolescents ages 12-17
teen <- readRDS( file.path( getwd() , "2015 teen.rds" ) )

teen$ak3_p1 <- teen$ak10_p <- NA
teen$agecat <- "2 - adolescent"
teen$no_usual_source_of_care <- as.numeric( teen$tf1 == 2 )

# four-category srhs (excellent / very good / good / fair+poor)
teen$hlthcat <- teen$tb1_p1

# load adults ages 18+
adult <- readRDS( file.path( getwd() , "2015 adult.rds" ) )

adult$agecat <- ifelse( adult$srage_p1 >= 65 , "4 - senior" , "3 - adult" )
adult$no_usual_source_of_care <- as.numeric( adult$ah1 == 2 )

# four-category srhs (excellent / very good / good / fair+poor)
adult$hlthcat <- c( 1 , 2 , 3 , 4 , 4 )[ adult$ab1 ]

# construct a character vector with only the variables needed for the analysis
vars_to_keep <- 
	c( grep( "rakedw" , names( adult ) , value = TRUE ) , 
		'hlthcat' , 'agecat' , 'ak3_p1' , 'ak10_p' ,
		'povll2_p' , 'no_usual_source_of_care' )

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
svymean( ~ povll2_p , chis_design )

svyby( ~ povll2_p , ~ hlthcat , chis_design , svymean )
svymean( ~ agecat , chis_design )

svyby( ~ agecat , ~ hlthcat , chis_design , svymean )
svytotal( ~ povll2_p , chis_design )

svyby( ~ povll2_p , ~ hlthcat , chis_design , svytotal )
svytotal( ~ agecat , chis_design )

svyby( ~ agecat , ~ hlthcat , chis_design , svytotal )
svyquantile( ~ povll2_p , chis_design , 0.5 )

svyby( 
	~ povll2_p , 
	~ hlthcat , 
	chis_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE 
)
svyratio( 
	numerator = ~ ak10_p , 
	denominator = ~ ak3_p1 , 
	chis_design 
)
sub_chis_design <- subset( chis_design , agecat == "4 - senior" )
svymean( ~ povll2_p , sub_chis_design )
this_result <- svymean( ~ povll2_p , chis_design )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ povll2_p , 
		~ hlthcat , 
		chis_design , 
		svymean 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( chis_design )
svyvar( ~ povll2_p , chis_design )
# SRS without replacement
svymean( ~ povll2_p , chis_design , deff = TRUE )

# SRS with replacement
svymean( ~ povll2_p , chis_design , deff = "replace" )
svyciprop( ~ no_usual_source_of_care , chis_design ,
	method = "likelihood" )
svyttest( povll2_p ~ no_usual_source_of_care , chis_design )
svychisq( 
	~ no_usual_source_of_care + agecat , 
	chis_design 
)
glm_result <- 
	svyglm( 
		povll2_p ~ no_usual_source_of_care + agecat , 
		chis_design 
	)

summary( glm_result )
library(srvyr)
chis_srvyr_design <- as_survey( chis_design )
chis_srvyr_design %>%
	summarize( mean = survey_mean( povll2_p ) )

chis_srvyr_design %>%
	group_by( hlthcat ) %>%
	summarize( mean = survey_mean( povll2_p ) )

