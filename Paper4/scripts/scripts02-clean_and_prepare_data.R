#### Preamble ####
# Purpose: Clean and prepare the data for further analysis. Csv file could be found in the output of scripts01-gather_data.R.
# Author: Haocheng Xu
# Data: 1998, Philippines.
# Contact: haocheng.xu@mail.utoronto.ca
# License: MIT
# Before start: 
# Here we use the Table 8.12 "Vaccinations by background characteristics" to do further analysis.This table is in the page 156 of the pdf: https://dhsprogram.com/pubs/pdf/FR103/FR103.pdf.

#### Clean the table ####
#Input the dataC:/Users/Asus/Desktop/Paper4/
rawdata <- read.csv("C:/Users/Asus/Desktop/Paper4/raw_data.csv",  header=TRUE)

#Put the first column into the rownames
row.names(rawdata) <- rawdata[, 1]
rawdata <- rawdata[, -1]

# Drop the NA rows 
rawdata <- rawdata[-c(3, 8, 11, 28, 32), ]
rawdata

#Edit some rownames manually
row.names(rawdata)[3] <- "Birthorder1"
row.names(rawdata)[4] <- "Birthorder2-3"
row.names(rawdata)[5] <- "Birthorder4-5"
row.names(rawdata)[6] <- "Birthorder6+"

# Now we could see the the cleaned table
View(rawdata)

####Check the table ####
library(pointblank)
library(dplyr) #Rename function is in the dplyr package

#Check if 'number of children' columns are all integers;
#Check if the value in the 'Percentage.of.a.card' columns are in the range of 0.0 to 100.0;

column_names_as_contracts <- 
  rawdata|> 
  rename(
    "int_noc" = "Number.of.Children",
    "bet_poc"="Percentage.of.a.card",
  )

agent <-
  create_agent(tbl = column_names_as_contracts) |>
  col_is_integer(columns = vars(int_noc)) |>
  col_vals_between(columns = vars(bet_poc), 0.0, 100.0) |> # Functions found in https://cran.r-project.org/web/packages/pointblank/pointblank.pdf. 
  interrogate()

#Check if the sum of numbers of children in different categories are equal to the total value: gender, birth order,and residence.
#Male and female
noc1 <- rawdata[c(1,2),c(12)]
sum(noc1)
#Birth order
noc2 <- rawdata[c(3,4,5,6),c(12)]
sum(noc2)
#Residence
noc3 <- rawdata[c(7,8),c(12)]
sum(noc3)
#The total value is 1474 shown in the original table. The category total is very close to 1474, but two of them is 1475.
#I have checked the origin. It seems that there is a count error in the survey.


####Now we can output the data.
View(rawdata)
write.csv(rawdata,"C:/Users/Asus/Desktop/Paper4/cleaned_data.csv", row.names = TRUE)