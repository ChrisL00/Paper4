#### Preamble ####
# Purpose: Gather the Philippines survey data and convert the PDF into a tidy dataset that could be analyzed in the later process. 
# Author: Haocheng Xu
# Data: 1998, Philippines.
# Contact: haocheng.xu@mail.utoronto.ca
# License: MIT
# Before start: Here we use the Table 8.12 "Vaccinations by background characteristics" to do further analysis.This table is in the page 156 of the pdf.


#### Workspace setup ####
library(janitor)
library(pdftools)
library(purrr)
library(tidyverse)
library(stringi)

#### Download the original pdf
#If this download failed, please use Internet to download the Philippines Demographic and Health Survey(1998) at:https://dhsprogram.com/pubs/pdf/FR103/FR103.pdf. 
#Then save it at a local folder.Replace "FR103.pdf" in line 21 with the local address of the file. 
download.file("https://dhsprogram.com/pubs/pdf/FR103/FR103.pdf", "FR103.pdf",mode="wb")
all_content <- pdf_text("FR103.pdf") 


#### Convert the page to strings
page156 <- all_content[[156]] 
table812 <- stri_split_lines(page156)[[1]] 
table812 <- table812[table812 != ""]


# Grab the name of the table
tablename <- table812[1] |> str_squish()
tablename <- str_to_title(tablename)


# Grab the explanation of table
tablexplain <- table812[2:4] |> str_squish()
tablexplain <- paste(tablexplain[1],tablexplain[2],tablexplain[3], sep=" ", collapse=NULL)


# Get rid of the table name, explanations and row names.
table812_no_header <- table812[10:length(table812)] 

# Get rid of the bottom matter
table812_no_header_no_footer <- table812_no_header[1:34] 
table812_no_header_no_footer

#Correct the decimal points
table812_no_header_no_footer <- str_replace_all(table812_no_header_no_footer, ",", ".")
table812_no_header_no_footer <- str_replace_all(table812_no_header_no_footer, "~", ".")

#Delete some extra spaces in numbers manually
table812_no_header_no_footer <- str_replace(table812_no_header_no_footer, "9 1 . 4", "91.4")
table812_no_header_no_footer <- str_replace(table812_no_header_no_footer, "8 9 . 2", "89.2")
table812_no_header_no_footer <- str_replace(table812_no_header_no_footer, "8 5 . 7", "85.7")
table812_no_header_no_footer <- str_replace(table812_no_header_no_footer, "8 6 . 4", "86.4")
table812_no_header_no_footer <- str_replace(table812_no_header_no_footer, "8 8 . 3", "88.3")
table812_no_header_no_footer <- str_replace(table812_no_header_no_footer, "8 9 . 8", "89.8")
table812_no_header_no_footer <- str_replace(table812_no_header_no_footer, "9 0 . 9", "90.9")
table812_no_header_no_footer <- str_replace(table812_no_header_no_footer, "8 0 . 7", "80.7")
table812_no_header_no_footer <- str_replace(table812_no_header_no_footer, "9 4 . 8", "94.8")
table812_no_header_no_footer <- str_replace(table812_no_header_no_footer, "9 2 . 0", "92.0")
table812_no_header_no_footer <- str_replace(table812_no_header_no_footer, "9 2 . 2", "92.2")
table812_no_header_no_footer <- str_replace(table812_no_header_no_footer, "8 6 . 2", "86.2")
table812_no_header_no_footer <- str_replace(table812_no_header_no_footer, "8 2 . 4", "82.4")
table812_no_header_no_footer <- str_replace(table812_no_header_no_footer, "8 4 . 3", "84.3")
table812_no_header_no_footer <- str_replace(table812_no_header_no_footer, "4 6 . 8", "46.8")
table812_no_header_no_footer <- str_replace(table812_no_header_no_footer, "4 3 . 0", "43.0")
table812_no_header_no_footer <- str_replace(table812_no_header_no_footer, "8 2 . 6", "82.6")
table812_no_header_no_footer <- str_replace(table812_no_header_no_footer, "9 3 . 8", "93.8")
table812_no_header_no_footer <- str_replace(table812_no_header_no_footer, "9 1 . 3", "91.3")
table812_no_header_no_footer <- str_replace(table812_no_header_no_footer, "9 6 . 5", "96.5")
table812_no_header_no_footer <- str_replace(table812_no_header_no_footer, "9 3 . 5", "93.5")
table812_no_header_no_footer <- str_replace(table812_no_header_no_footer, "9 0 . 3", "90.3")
table812_no_header_no_footer <- str_replace(table812_no_header_no_footer, "8 7 . 0", "87.0")
table812_no_header_no_footer <- str_replace(table812_no_header_no_footer, "1.474", "1474")
table812_no_header_no_footer <- str_replace(table812_no_header_no_footer, "90.i", "90.1")

# Make the row names stay in the first column 
table812_no_header_no_footer <-gsub("([A-Za-z])\\s+([A-Za-z])", "\\1\\2",table812_no_header_no_footer)
table812_no_header_no_footer <-gsub("([A-Za-z]).\\s+([A-Za-z])", "\\1.\\2",table812_no_header_no_footer)
table812_no_header_no_footer


# Convert the adjusted strings into a tibble
demography_data <- tibble(all = table812_no_header_no_footer)

# Split columns
demography_data <-
  demography_data |>
  mutate(all = str_squish(all)) |> # Any space more than two spaces is reduced
  separate(col =all,
           into = c("Background Characteristic","BCG", "DPT1", "DPT2", "DPT3+", "Polio1", "Polio2", "Polio3+", "Measles","All","None","Percentage of a card","Number of Children"),
           sep = " ",
           remove = TRUE,
           fill = "right",
           extra = "drop"
  )

view(demography_data)
write.csv(demography_data,"C:/Users/Asus/Desktop/Paper4/raw_data.csv", row.names = FALSE)
## We have made the pdf page a matrix. Now the next step is to clean it.



