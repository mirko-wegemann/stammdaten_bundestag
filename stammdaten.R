require("XML")
require("xml2")
require("dplyr")
require("stringr")

getwd()


# parse the "cleaned" XML and store it into a dataframe
stammdaten <- read_xml("mdb_stammdaten.xml")
mdb_data <- stammdaten %>%
  xml_find_all('//MDB') %>% 
  as_list()


# create empty matrix
mdb_bio_matrix <- matrix(ncol=65, nrow=length(mdb_data))

# fill matrix with values from xml file
start_time <- Sys.time()
for(i in 1:length(mdb_data)){
  mdb_bio_matrix[i,1] <- ifelse(is.null(unlist(mdb_data[[i]][[2]][[1]][[6]])), "", unlist(mdb_data[[i]][[2]][[1]][[6]])) # title
  mdb_bio_matrix[i,2] <- unlist(mdb_data[[i]][[2]][[1]][[2]]) # first name
  mdb_bio_matrix[i,3] <- unlist(mdb_data[[i]][[2]][[1]][[1]]) # last name
  mdb_bio_matrix[i,4] <- paste(ifelse(is.null(unlist(mdb_data[[i]][[2]][[1]][[6]])), "", unlist(mdb_data[[i]][[2]][[1]][[6]])), 
                               unlist(mdb_data[[i]][[2]][[1]][[2]]), unlist(mdb_data[[i]][[2]][[1]][[1]])) # full name
  mdb_bio_matrix[i,5] <- unlist(mdb_data[[i]][[3]][[5]]) # gender
  mdb_bio_matrix[i,6] <- unlist(mdb_data[[i]][[3]][[1]]) # date of birth
  mdb_bio_matrix[i,7] <- ifelse(is.null(unlist(mdb_data[[i]][[3]][[2]])), NA, unlist(mdb_data[[i]][[3]][[2]])) # place of birth
  mdb_bio_matrix[i,8] <- ifelse(is.null(unlist(mdb_data[[i]][[3]][[3]])), NA, unlist(mdb_data[[i]][[3]][[3]])) # country of birth
  mdb_bio_matrix[i,9] <- ifelse(is.null(unlist(mdb_data[[i]][[3]][[4]])), NA, unlist(mdb_data[[i]][[3]][[4]])) # date of death
  mdb_bio_matrix[i,10] <- ifelse(is.null(unlist(mdb_data[[i]][[3]][[7]])), NA, unlist(mdb_data[[i]][[3]][[7]])) # religion
  mdb_bio_matrix[i,11] <- ifelse(is.null(unlist(mdb_data[[i]][[3]][[6]])), NA, unlist(mdb_data[[i]][[3]][[6]])) # family status
  mdb_bio_matrix[i,12] <- ifelse(is.null(unlist(mdb_data[[i]][[3]][[8]])), NA, unlist(mdb_data[[i]][[3]][[8]])) # occupation
  mdb_bio_matrix[i,13] <- ifelse(is.null(unlist(mdb_data[[i]][[3]][[9]])), NA,  unlist(mdb_data[[i]][[3]][[9]])) # party
  wp_duration <- length(mdb_data[[i]][[4]])
  for(j in 1:wp_duration){
    mdb_bio_matrix[i,(10+(j*4))] <- ifelse(is.null(unlist(mdb_data[[i]][[4]][[j]][[1]])), NA, unlist(mdb_data[[i]][[4]][[j]][[1]])) # legislature
    mdb_bio_matrix[i,(11+(j*4))] <- ifelse(is.null(unlist(mdb_data[[i]][[4]][[j]][[4]])), NA, unlist(mdb_data[[i]][[4]][[j]][[4]])) # electoral district
    mdb_bio_matrix[i,(12+(j*4))] <- ifelse(is.null(unlist(mdb_data[[i]][[4]][[j]][[9]][[1]][[2]])), NA, unlist(mdb_data[[i]][[4]][[j]][[9]][[1]][[2]])) # faction
    mdb_bio_matrix[i,(13+(j*4))] <- ifelse(is.null(unlist(mdb_data[[i]][[4]][[j]][[8]])), NA, unlist(mdb_data[[i]][[4]][[j]][[8]]))# mandate legislature 
  }
  }
end_time <- Sys.time()
end_time-start_time

# create colnames and assign to matrix
legislature_list <- c(paste0("legislature_", 1:13))
e_distr_list <- c(paste0("elect_distr_", 1:13))
faction_list <- c(paste0("faction_", 1:13))
mandate_type_list <- c(paste0("mandate_type_", 1:13))
leg_mand_list <- mapply(list, legislature_list, e_distr_list, faction_list, mandate_type_list, SIMPLIFY = F) %>% unlist()
colnames(mdb_bio_matrix) <- c("title", "first_name", "last_name", "full_name", "gender", "date_birth", "place_birth", "country_birth", "date_death", "religion", "family_stat", "occupation", "party", leg_mand_list)

# create data frame 
mdb_df <- data.frame(mdb_bio_matrix)
mdb_df$full_name <- str_trim(mdb_df$full_name, side = c("left"))

# reshape to long-format 
mdb_data_long <- reshape(mdb_df, direction = "long", varying = list(c(legislature_list),c(e_distr_list),c(faction_list),c(mandate_type_list)), 
                         idvar=c("full_name", "date_birth"), 
                         new.row.names = NULL, v.names = c("legislature", "elect_distr", "faction", "mandate_type"))

rownames(mdb_data_long) <- NULL
# drop rows with empty observations 
mdb_data_long <- subset(mdb_data_long, !is.na(legislature))
