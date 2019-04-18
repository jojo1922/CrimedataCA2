library(readr) # called reader library for reading csv files
library(dplyr) # grammer of data manupulation.

files <- list.files(path ="D:/datascience project/CrimedataCA2"
                    ,pattern = "*.csv",full.names = T)
# listing the csv files full

All_crime_data <- sapply(files,read_csv,simplify = FALSE) %>% bind_rows(.id="id")
# bind all the files to one file called All_Crime_data
nrow(All_crime_data) # number of rows
count(All_crime_data) # count of data set
All_crime_data_new <- subset(All_crime_data , select = -c(1,2,4,5,9,10,12,13))
# remove the following attribute CrimeID, Reported by,
#Falls within, LSOA code, LSOA name, last outcome and context
All_crime_data_new # save to All_crime_data_new
str(All_crime_data_new) # structure of modified file
crime_type <- factor(All_crime_data_new$`Crime type`, ordered = TRUE) 
# factorise crime type attribute.
crime_type
str(crime_type) # structure of crime type modified
str(All_crime_data_new$Location) # structure of Location attribute
remove_words <- c("On or near") # remove words "On or near" from Location Column
All_crime_data_new$Location <- gsub(paste0(remove_words, collapse = "|"),
                                    "",All_crime_data_new$Location)
All_crime_data_new$Location <- toupper(All_crime_data_new$Location)
All_crime_data_new$Location   
na.omit(All_crime_data_new)
view()  

#write.csv(All_crime_data_new, "randomcrime.csv") # saved to randomcrime.csv
randomcrime <- read_csv("randomcrime.csv")
View(randomcrime)
is.na(randomcrime)
clean_1 <- na.omit(randomcrime)# clean the data

cleaned_na<- subset(clean_1, select = -c(1))# delete the X1 column
cleaned_na
Random_Crime_sample <- head(cleaned_na,1000)
# selected the 1000 vales 
# saved the NA values to Random_Crime_sample.
View( Random_Crime_sample)   
# writing function to the post code
find_a_postcode <- function(Location) {
 
 data_filter <- filter(NIPostcodes,NIPostcodes$`Primary Thorfare` == Location)# filter the data based on Postcode location
  
result <- names(which.max(table(data_filter$Postcode)))
return(result)  
  
}
 postcodes_function <- lapply(toupper(Random_Crime_sample$Location), find_a_postcode)
 
 head(postcodes_function)
 View(postcodes_function)
 is.na(postcodes_function)
 str(postcodes_function)
 

# saving the post code function to Random_Crime_sample. 
postcodes_function <- sapply(postcodes_function, paste0, collapse = "")
head(postcodes_function)
na.omit(postcodes_function)# clean the na values in postcode
Random_Crime_sample$Postcode <- postcodes_function

head(Random_Crime_sample)
 View(Random_Crime_sample)
str(Random_Crime_sample)# structure after creating random crime sample
count(Random_Crime_sample) # count the number of records

# save the Random_crime_sample to csv file.


write.csv(Random_Crime_sample, "Random_Crime_sample.csv")
View(Random_Crime_sample)
updated_random_sample <-data.frame(Random_Crime_sample)
updated_random_sample <- Random_Crime_sample # saved to updated random sample
str(updated_random_sample)
updated_random_sample
Random_Crime_sample <- read_csv("Random_Crime_sample.csv")

Random_Crime_sample <- na.omit(Random_Crime_sample)# omit na values 
  
chart_data <- subset(Random_Crime_sample, select = -c(1))
# eliminate the unwanted column and saved to chart_data
chart_data<- data.frame(chart_data) 
#View(chart_data)
na.omit(chart_data)
 

chart_data <- chart_data[order(chart_data$Postcode),]
# sorted the values based on Postcode BT1
View(chart_data)  
summary(chart_data)


        
        
        
        
        
