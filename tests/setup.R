if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )
chis_username <- Sys.getenv( "chis_username" )
chis_password <- Sys.getenv( "chis_password" )
library(lodown)
lodown( "chis" , output_dir = file.path( getwd() ) , 
	your_username = chis_username , 
	your_password = chis_password )
