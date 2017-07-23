if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )
chis_username <- Sys.getenv( "chis_username" )
chis_password <- Sys.getenv( "chis_password" )
library(lodown)
# examine all available CHIS microdata files
chis_cat <-
	get_catalog( "chis" ,
		output_dir = file.path( getwd() ) )

# 2015 only
chis_cat <- subset( chis_cat , year == 2015 )
# download the microdata to your local computer
stopifnot( nrow( chis_cat ) > 0 )

options( survey.replicates.mse = TRUE )

library(survey)

child <- readRDS( file.path( getwd() , "2015/child.rds" )

child$agecat <- "1 - child"

# rename the four-category (excellent / very good / good / fair+poor) variable over to `hlthcat`
child$hlthcat <- child$ca6_p1

# load adolescents ages 12-17
teen <- readRDS( file.path( getwd() , "2015/teen.rds" )

teen$agecat <- "2 - adolescent"

# rename the four-category (excellent / very good / good / fair+poor) variable over to `hlthcat`
teen$hlthcat <- teen$tb1_p1

# load adults ages 18+
adult <- readRDS( file.path( getwd() , "2015/adult.rds" )

adult$agecat <- ifelse( adult$srage_p1 >= 65 , "4 - senior" , "3 - adult" )

# recode the five-category variable into four categories (condensing fair+poor)
adult$hlthcat <- c( 1 , 2 , 3 , 4 , 4 )[ adult$ab1 ]

# construct a character vector with only the variables needed for the analysis
vars_to_keep <- c( grep( "rakedw" , names( adult ) , value = TRUE ) , 'hlthcat' , 'agecat' )

chis_df <- 
	rbind( 
		child[ vars_to_keep ] , 
		teen[ vars_to_keep ] , 
		adult[ vars_to_keep ] 
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
svymean( ~ bmipct , chis_design , na.rm = TRUE )

svyby( ~ bmipct , ~ hlthcat , chis_design , svymean , na.rm = TRUE )
svymean( ~ agecat , chis_design )

svyby( ~ agecat , ~ hlthcat , chis_design , svymean )
svytotal( ~ bmipct , chis_design , na.rm = TRUE )

svyby( ~ bmipct , ~ hlthcat , chis_design , svytotal , na.rm = TRUE )
svytotal( ~ agecat , chis_design )

svyby( ~ agecat , ~ hlthcat , chis_design , svytotal )
svyquantile( ~ bmipct , chis_design , 0.5 , na.rm = TRUE )

svyby( 
	~ bmipct , 
	~ hlthcat , 
	chis_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ ever_tried_to_quit_cigarettes , 
	denominator = ~ smoked_cigarettes_past_year , 
	chis_design ,
	na.rm = TRUE
)
sub_chis_design <- subset( chis_design , qn41 == 1 )
svymean( ~ bmipct , sub_chis_design , na.rm = TRUE )
this_result <- svymean( ~ bmipct , chis_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ bmipct , 
		~ hlthcat , 
		chis_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( chis_design )
svyvar( ~ bmipct , chis_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ bmipct , chis_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ bmipct , chis_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ never_rarely_wore_bike_helmet , chis_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( bmipct ~ never_rarely_wore_bike_helmet , chis_design )
svychisq( 
	~ never_rarely_wore_bike_helmet + agecat , 
	chis_design 
)
glm_result <- 
	svyglm( 
		bmipct ~ never_rarely_wore_bike_helmet + agecat , 
		chis_design 
	)

summary( glm_result )
library(srvyr)
chis_srvyr_design <- as_survey( chis_design )
chis_srvyr_design %>%
	summarize( mean = survey_mean( bmipct , na.rm = TRUE ) )

chis_srvyr_design %>%
	group_by( hlthcat ) %>%
	summarize( mean = survey_mean( bmipct , na.rm = TRUE ) )

unwtd.count( ~ never_rarely_wore_bike_helmet , yrbss_design )

svytotal( ~ one , subset( yrbss_design , !is.na( never_rarely_wore_bike_helmet ) ) )
 
svymean( ~ never_rarely_wore_bike_helmet , yrbss_design , na.rm = TRUE )

svyciprop( ~ never_rarely_wore_bike_helmet , yrbss_design , na.rm = TRUE , method = "beta" )

