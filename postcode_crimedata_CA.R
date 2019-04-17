getwd()
setwd(dir = "D:/datascience project/postcode_crimedata_CA2")
library(readr)

NIPostcodes <- read_csv("NIPostcodes.csv") # import the csv file 

View(NIPostcodes)

str(NIPostcodes)    # structure of the NIPostcodes

names(NIPostcodes) <- c("Organisation Name","Sub-building Name",
                        "Building Name","Number","Primary Thorfare","Alt Thorfare",
                        "Secondary Thorfare","Locality",
                        "Townland","Town","County","Postcode"
                        ,"x-coordinates","y-coordinates",
                        "Primary Key (identifier)")
# Adding suitable title for each attribute in the column
names(NIPostcodes)
NIPostcodes

nrow(NIPostcodes) # total number of rows

str(NIPostcodes) # structure of NIPostcode dataset
head(NIPostcodes,10) # Head first 10 rows
is.na(NIPostcodes$`Organisation Name`)
#na_vec <- c(which(complete.cases(NIPostcodes$`Organisation Name`)))
#na_vec
sum_of_na_organisation<- sum( is.na(NIPostcodes$`Organisation Name`))
#sum of missing values in Organisation name col
sum_of_na_organisation
#NIPostcodes[is.na(NIPostcodes)]=0
#View(NIPostcodes) # replace NA value with 0
mean_organisation <- mean(is.na(NIPostcodes$`Organisation Name`))
#Mean of missing value of organisation name

mean_organisation
sum_of_na_subbulding<- sum( is.na(NIPostcodes$`Sub-building Name`))
# sum of missing values in Organisation name col

sum_of_na_subbulding
mean_of_na_subbulding<- mean(is.na(NIPostcodes$`Sub-building Name`)) 
#Mean of missing value of organisation name

mean_of_na_subbulding
sum_of_na_subbulding <- sum(is.na(NIPostcodes$`Sub-building Name`)) 
#sum of missing values of sub building name

sum_of_na_subbulding
mean_of_na_bulding_name<- mean(is.na(NIPostcodes$`Building Name`)) 
#mean of na values of Bulding name

mean_of_na_bulding_name
sum_of_bulding_name <- sum(is.na(NIPostcodes$`Building Name`)) 
#sum pf mean values of Bulding name
sum_of_bulding_name


names(NIPostcodes) <- c("Organisation Name","Sub-building Name","Building Name","Number",
                        "Primary Thorfare","Alt Thorfare","Secondary Thorfare","Locality",
                        "Townland","Town","Categorising Factor","Postcode",
                        "x-coordinates","y-coordinates","Primary Key (identifier)")


# Modify the County attribute to be a categorising factor. 
names(NIPostcodes)
NIPostcodes<-NIPostcodes[,c(15,1:14)]
# move the last column named Primary key to the first column
NIPostcodes
 
new_data_1 <- subset(NIPostcodes, Town== 'LIMAVADY')
#extract the dataframe contaning the town name LIMAVADY.
new_data_1


Limavady_data <-subset(new_data_1, select = c(9,10,11))
#create a dataset called Limavady_data with locality,townland and town 
Limavady_data


write.csv(Limavady_data,"Limavady.csv") #save data as Limavady.csv

 
write.csv(Limavady_data,"CleanNIPostcodeData.csv")
# Save the modified NIPostcode dataset in a csv file called CleanNIPostcodeData.

str(Limavady_data)

