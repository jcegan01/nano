############################## SETUP ##############################

#set options
options(stringsAsFactors=FALSE)

#load packages
packages <- c("stringr","data.table","tm","SnowballC","plyr","devtools","ggplot2","wordcloud","RColorBrewer","cluster","fpc","class","RWeka")
lapply(packages, require, character.only = TRUE)

#set working directory
setwd("C:\\Users\\u4eprjcc\\Documents\\SynBio\\")





############################## CREATE OUTPUT TABLE ##############################

dir_path <- "02_Articles\\txt\\"
filenames <- list.files(dir_path,pattern = ".txt",full.names=FALSE)

#initialize output table
numrows <- length(filenames)
res <- data.frame(matrix(NA,numrows,0))

for (f in c(1:numrows)){ 
  
  res[f,"File_Name"] <- unlist(strsplit(filenames[f], "/"))
  #gsub("(?<=\\s)i+", " ", "akui i ii", perl=T)
  res[f,"Author_Name"] <- gsub("\\s.*","", filenames[f])  
  res[f,"Author_Name"] <- gsub(",.*","", res[f,"Author_Name"])
  
  res[f,"Title_Name"] <- strsplit(filenames[f],' - ',fixed=TRUE)[[1]][3] 
  res[f,"Title_Name"] <- gsub("\\.txt","", res[f,"Title_Name"])
  title_wordlist <-strsplit(res[f,"Title_Name"], "\\W+", perl=TRUE)
  title_wordvector <- unlist(title_wordlist)
  res[f,"Title_Keyword1"] <- tolower(title_wordvector[nchar(title_wordvector)==max(nchar(title_wordvector))][[1]])
  res[f,"Title_Keyword2"] <- tolower(title_wordvector[nchar(title_wordvector)==max(nchar(title_wordvector)[nchar(title_wordvector)!=max(nchar(title_wordvector))])][[1]])
  
  #grab year
  year <- as.character(1970:2017) 
  
  reflist <-strsplit(res[f,"File_Name"], "\\W+", perl=TRUE)
  refvector <- unlist(reflist)
  for (n in 1:1000){
    for (y in 1:length(year)){
      if (refvector[n] == year[y]){
        res[f,"Publication_Year"] <- refvector[n]
        break
      }}
    if (refvector[n] == year[y]){break}
    if (n == length(refvector)){break}      
  }
}

#create unique ids
res$ID <- paste(res$Author_Name,res$Publication_Year,res$Title_Keyword1,res$Title_Keyword2,sep="_")


#QA: no year
res[is.na(res$Publication_Year)==TRUE,]     

#QA: duplicate articles
nrow(res[duplicated(res$ID),])  

#delete duplicates
res<-res[!duplicated(res$ID),]
numrowscols <- nrow(res)

#print results table for Emily's crosswalk
outputpath <- c("03_Data\\Crosswalk_setup.csv") 

###
# write.table(res, file = paste(outputpath),sep=",",col.names = TRUE,append=FALSE,row.names=FALSE)	
###

#turn results table into matrix
column_pushover <- 6        
citm <- data.frame(matrix(NA,numrowscols,numrowscols+column_pushover))  
names(citm) <- c("File_Name","Author_Name","Publication_Year","ID","Title_Keyword1","Title_Keyword2",res$ID)    
citm$File_Name <- res$File_Name
citm$Author_Name <- res$Author_Name 
citm$Publication_Year <- res$Publication_Year
citm$Title_Keyword1 <- res$Title_Keyword1
citm$Title_Keyword2 <- res$Title_Keyword2                   
citm$ID <- res$ID

#copy text files into special folder for corpus creation (incase duplicates were deleted)

###	
# files2keep <- unique(citm$File_Name)        
# startpath <- c("C:\\Users\\u4eprjcc\\Documents\\SynBio\\02_Articles\\txt\\")
# dir.create("C:\\Users\\u4eprjcc\\Documents\\SynBio\\02_Articles\\txt\\tm\\",showWarnings=FALSE)             
# file.copy(paste0(startpath,files2keep), paste("C:\\Users\\u4eprjcc\\Documents\\SynBio\\02_Articles\\txt\\tm\\",sep=""),overwrite=TRUE)  
###



############################## CREATE CORPUS ##############################

#words to ignore
w2i <- read.csv("C:\\Users\\u4eprjcc\\Documents\\Code\\MCDA\\words2ignore.csv",header=FALSE,colClasses="character")
w2i <-strsplit(tolower(w2i), "\\W+", perl=TRUE)
w2ivector <- unlist(w2i)

#clean corpus function
cleanCorpus <- function(corpus){
  tx.tmp <- tm_map(corpus, stripWhitespace)
  tx.tmp <- tm_map(tx.tmp, content_transformer(tolower))  
  #tx.tmp <- tm_map(tx.tmp, removePunctuation)
  #tx.tmp <- tm_map(tx.tmp, removeWords, w2ivector)       
  tx.tmp <- tm_map(tx.tmp, removeWords, stopwords("english"))
  #tx.tmp <- tm_map(tx.tmp, stemDocument, language="english")
  #tx.tmp <- tm_map(tx.tmp, removeNumbers)            
  #tx.tmp <- tm_map(tx.tmp, PlainTextDocument)    
  return(tx.tmp) 
}

#compile text
s.dir <- "C:\\Users\\u4eprjcc\\Documents\\SynBio\\02_Articles\\txt\\"
s.cor <- Corpus(DirSource(directory=s.dir),readerControl = list(language = "lat"))
s.cor.cl <- cleanCorpus(s.cor)
ref_all <- s.cor.cl




############################## TEXT MINING SCAN ##############################

#run through files (f) and check for word counts    
fend <- nrow(citm)

for (f in 1:fend){  
  #read and convert to string 
  ref <- as.character(ref_all[[f]]) #old version: readLines(filenames[f],warn=FALSE);ref <- tolower(ref);         
  reflist <-strsplit(ref, "\\W+", perl=TRUE)
  refvector <- unlist(reflist)
  refline <- paste(refvector, collapse = " ")
  #if(nchar(refline) == 0) next()
  
  #delete "-" 
  refline <- gsub("-", "", refline, fixed=TRUE)
  
  #convert all other punctuation to space
  punct <- '[]\\?!\"\'#$%&(){}+*/:;,._`|~\\[<=>@\\^]'     
  refline <- gsub(punct, " ", refline, fixed=TRUE)    
  
  
  #output file stats  
  x_author <- tolower(citm$Author_Name)
  x_keyword1 <- tolower(citm$Title_Keyword1)
  x_keyword2 <- tolower(citm$Title_Keyword2)      
  x_year <- citm$Publication_Year
  
  #function: reduce refline to author and possible dates 
  keep_authors_years <- function(text,keep_list) {
    stop_regex <- paste(keep_list, collapse = "\\b|\\b")
    stop_regex <- paste("\\b", stop_regex, "\\b", sep = "")
    tmp <- strsplit(text, " ")[[1]]
    idx <- grepl(stop_regex, tmp)
    txt <- paste(tmp[idx], collapse = " ")
    return(txt)
  }
  
  #search reduced refline for author/date match
  for (k in 1:(length(x_author))){
    keep_list <- append(x_author[k],as.character(1900:2018))    
    keep_list <- append(keep_list,x_keyword1[k])    
    keep_list <- append(keep_list,x_keyword2[k])
    
    temp1 <- keep_authors_years(refline,keep_list)
    search_whole_word_1 <- paste(x_author[k],x_keyword1[k],x_keyword2[k],x_year[k],sep=" ") #author - keyword1 - keyword2 - year
    search_whole_word_2 <- paste(x_author[k],x_keyword2[k],x_keyword1[k],x_year[k],sep=" ") #author - keyword2 - keyword1 - year                        
    search_whole_word_3 <- paste(x_author[k],x_year[k],x_keyword1[k],x_keyword2[k],sep=" ") #author - year - keyword1 - keyword2
    search_whole_word_4 <- paste(x_author[k],x_year[k],x_keyword2[k],x_keyword1[k],sep=" ") #author - year - keyword2 - keyword1
    
    citm[f,k+column_pushover] <- str_count(temp1, pattern= search_whole_word_1) + str_count(temp1, pattern= search_whole_word_2) + str_count(temp1, pattern= search_whole_word_3) + str_count(temp1, pattern= search_whole_word_4)
    
    if(citm[f,k+column_pushover] > 1) citm[f,k+column_pushover] <- 1
  }
  
  #set title author as NA
  citm[f,f+column_pushover] <- 0
  
  #bypass error   
  #if(is.null(refvector) == TRUE) next()
  
  #remove
  rm(ref); gc();
  
  #print output
  outputpath <- c("03_Data\\Outputs\\CitNet_170609.csv") 
  if(f == 1)  write.table(citm[f,], file = paste(outputpath),sep=",",col.names = TRUE,append=FALSE,row.names=FALSE)
  if(f > 1)   write.table(citm[f,], file = paste(outputpath),sep=",",col.names = FALSE,append=TRUE,row.names=FALSE)
}   


