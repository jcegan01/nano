#Author: Jeff Cegan
#Project: Nano
#Date: 12/20/2018


##############
### setup ###
############

#load libraries
lapply(c("tidyverse", "reshape2", "plyr"), require, character.only = TRUE)

#set working directory
setwd("C:/Users/xadmin/projects/nano")


#############
### read ###
###########

############################## CREATE TEXT FILES ##############################


dir_path <- list.dirs(path = "data/pdfs", full.names = TRUE, recursive = TRUE)
filenames <- list.files(dir_path,pattern = ".pdf",full.names=TRUE)
exe <- "C:\\Users\\xadmin\\projects\\code\\erdc\\exe\\xpdfbin-win-3.04\\bin32\\pdftotext.exe"  
for (f in 1:length(filenames)){
filepath <- paste0(filenames[f])
system(paste("\"", exe, "\" \"", filepath, "\"", sep = ""), wait = F) 
}


