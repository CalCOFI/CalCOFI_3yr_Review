###Combine various sheets to unnderstand which speecies are being sampled by CalCOFI


#install packages
library(readxl) 
library(here)


rm(list=ls())

#function for excel sheets
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


mysheets <- read_excel_allsheets(here("CalCOFI_3yr_data/FINALSPECIESLISTCALCOFI.xlsx"))

#View(mysheets)

#make all sheets dfs
list2env(mysheets,envir=.GlobalEnv)

#add scientific names to CalCOFI fish
colnames(FinalCalCOFI)
colnames(ImpFishInfo)
FinalCalCOFI1 <- merge(FinalCalCOFI, ImpFishInfo[,c("SCIENTIFIC NAME", "COMMON NAME")], by.x = ("Scientific_Name"), by.y = ("SCIENTIFIC NAME"), all=T)
#View(FinalCalCOFI1)

#replace with common name if na
FinalCalCOFI1$'Common_Name' <- ifelse(is.na(FinalCalCOFI1$'Common_Name'), FinalCalCOFI1$'COMMON NAME', FinalCalCOFI1$'Common_Name')

View(FinalCalCOFI1)
colnames(FinalCalCOFI1)


test1 <- merge(FinalCalCOFI1, FedFishSp[,c('Federal Fished Species', "Common_Name_a", "Scientific_Name", "Broad grouping_a")], by= "Scientific_Name", all= T)

test2 <- merge(test1, FedMMSp[,c('Federal Marine Mammal Species', "Common_Name_b", "Scientific_Name", "Broad grouping_b")], by= "Scientific_Name", all= T)

test3 <- merge(test2, TESpecies[,c('Threatened & Endangered Species', "Common_Name_c", "Scientific_Name", "Broad grouping_c")], by= "Scientific_Name", all= T)


#put common names where they should be
test3$'Broad grouping' <- ifelse(is.na(test3$'Broad grouping'), test3$'Broad grouping_a', test3$'Broad grouping')
test3$'Broad grouping' <- ifelse(is.na(test3$'Broad grouping'), test3$'Broad grouping_b', test3$'Broad grouping')
test3$'Broad grouping' <- ifelse(is.na(test3$'Broad grouping'), test3$'Broad grouping_c', test3$'Broad grouping')


test3$Common_Name <- ifelse(is.na(test3$Common_Name), test3$Common_Name_a, test3$Common_Name)
test3$Common_Name <- ifelse(is.na(test3$Common_Name), test3$Common_Name_b, test3$Common_Name)
test3$Common_Name <- ifelse(is.na(test3$Common_Name), test3$Common_Name_c, test3$Common_Name)

colnames(test3)

FinalFederalCalCOFI <- test3[,c("Broad grouping", "Scientific_Name", "Common_Name", "CalCOFI sampled species", "Federal Fished Species", "Federal Marine Mammal Species", "Threatened & Endangered Species")]

#View(FinalFederalCalCOFI)

#State links
head(StateCommSp)
head(StateRecSp)

colnames(StateCommSp)
colnames(FinalFederalCalCOFI)

class(StateCommSp)
class(FinalFederalCalCOFI)

class(StateCommSp$Common_Name)
class(FinalFederalCalCOFI$Common_Name)

test4 <- merge(x= FinalFederalCalCOFI, y= StateCommSp[,c("State Commercial Species", "Broad_grouping_d", "Common_Name")], by.x = c("Common_Name"), by.y = c("Common_Name"), all = T)

colnames(StateRecSp)
test5 <- merge(x= test4, y= StateRecSp[,c("State Rec Species", "Broad_grouping_e", "Common_Name")], by.x = c("Common_Name"), by.y = c("Common_Name"), all = T)

colnames(test5)

test5$'Broad grouping' <- ifelse(is.na(test5$'Broad grouping'), test5$'Broad_grouping_d', test5$'Broad grouping')
test5$'Broad grouping' <- ifelse(is.na(test5$'Broad grouping'), test5$'Broad_grouping_e', test5$'Broad grouping')

#View(test5)
colnames(test5)

finalCalCOFIStateFedSp <- test5[,c("Broad grouping", "Scientific_Name", "Common_Name", "CalCOFI sampled species", "Federal Fished Species", "Federal Marine Mammal Species", "Threatened & Endangered Species", "State Commercial Species", "State Rec Species")]

#View(finalCalCOFIStateFedSp)

#replace so all the same
finalCalCOFIStateFedSp$`Broad grouping` <- gsub("Invertebrates", "Inverts" , finalCalCOFIStateFedSp$`Broad grouping`)
finalCalCOFIStateFedSp$`Broad grouping` <- gsub("Invertebrate", "Inverts" , finalCalCOFIStateFedSp$`Broad grouping`)
finalCalCOFIStateFedSp$`Broad grouping` <- gsub("Invert", "Inverts" , finalCalCOFIStateFedSp$`Broad grouping`)
finalCalCOFIStateFedSp$`Broad grouping` <- gsub("Fishes", "Fish" , finalCalCOFIStateFedSp$`Broad grouping`)
finalCalCOFIStateFedSp$`Broad grouping` <- gsub("Phytoplankton", "Algae/Phytoplankton" , finalCalCOFIStateFedSp$`Broad grouping`)
finalCalCOFIStateFedSp$`Broad grouping` <- gsub("Algae", "Algae/Phytoplankton" , finalCalCOFIStateFedSp$`Broad grouping`)
finalCalCOFIStateFedSp$`Broad grouping` <- gsub("Marine mammal/Seabirds", "Marine mammals/Seabirds/Turtles" , finalCalCOFIStateFedSp$`Broad grouping`)
finalCalCOFIStateFedSp$`Broad grouping` <- gsub("Inverts", "Inverts/Zooplankton" , finalCalCOFIStateFedSp$`Broad grouping`)
finalCalCOFIStateFedSp$`Broad grouping` <- gsub("Zooplankton", "Inverts/Zooplankton" , finalCalCOFIStateFedSp$`Broad grouping`)
finalCalCOFIStateFedSp$`Broad grouping` <- gsub("Inverts/Inverts/Zooplankton", "Inverts/Zooplankton" , finalCalCOFIStateFedSp$`Broad grouping`)
finalCalCOFIStateFedSp$`Broad grouping` <- gsub("Inverts/Inverts/Zooplanktons", "Inverts/Zooplankton" , finalCalCOFIStateFedSp$`Broad grouping`)
finalCalCOFIStateFedSp$`Broad grouping` <- gsub("Inverts/Zooplanktons", "Inverts/Zooplankton" , finalCalCOFIStateFedSp$`Broad grouping`)
finalCalCOFIStateFedSp$`Broad grouping` <- gsub("Algae/Phytoplankton/Phytoplankton", "Algae/Phytoplankton" , finalCalCOFIStateFedSp$`Broad grouping`)

unique(finalCalCOFIStateFedSp$`Broad grouping`)



write.csv(finalCalCOFIStateFedSp, 'FinalALLCalCOFIStateFedSppList.csv')

#subset to only include zooplankton/fish/mm

##get percentages of each (so times that CalCOFI data )

colnames(finalCalCOFIStateFedSp)
indx <- combn(colnames(finalCalCOFIStateFedSp)[-c(1:3)],2)
res <- sapply(split(indx, col(indx)), function(x) 
  sum(!rowSums(is.na(finalCalCOFIStateFedSp[,x]))))

names(res) <- apply(indx,2, paste, collapse="_")
resdf <- as.data.frame(res)
View(resdf)

colnames(finalCalCOFIStateFedSp)
sum(!is.na(finalCalCOFIStateFedSp$`CalCOFI sampled species`)) #753 all CalCOFI
sum(!is.na(finalCalCOFIStateFedSp$`Federal Fished Species`)) #109 federal fished species
sum(!is.na(finalCalCOFIStateFedSp$`Federal Marine Mammal Species`)) #36 MM  species
sum(!is.na(finalCalCOFIStateFedSp$`Threatened & Endangered Species`)) #36 TE  species
sum(!is.na(finalCalCOFIStateFedSp$`State Commercial Species`)) #390 state comm fished species
sum(!is.na(finalCalCOFIStateFedSp$`State Rec Species`)) #219 state rec fished species

#CalCOFI sampled species_Federal Fished Species	18
18/109 #17% of federal fished species
#CalCOFI sampled species_Federal Marine Mammal Species	28
28/36 #78% of federal MM species
#CalCOFI sampled species_Threatened & Endangered Species	16
16/36 #44 of TE species
#CalCOFI sampled species_State Commercial Species	14
14/390 #4% of State Commercial spp.
#CalCOFI sampled species_State Rec Species	13
13/219 #6% of State Rec species

    