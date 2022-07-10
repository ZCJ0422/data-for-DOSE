library(stringr)

#read HumanDO.obo file and select data
f1<-data.table::fread("./HumanDO.obo",encoding = "UTF-8",fill = T,skip = 28,header = F)
locate <-which(!is.na(str_match(f1$V1,"\\[Term\\]"))|!is.na(str_match(f1$V1,"id: DOID"))|!is.na(str_match(f1$V1,"name: "))|!is.na(str_match(f1$V1,"synonym:")))
f1<-f1[locate,]

#extract data into a dataframe
n = nrow(f1)
doid <- c(1:n)
Term <- c(NA)
Synonym <- c(NA)
Secondary <- c(NA)
df <- cbind(doid,Term,Synonym,Secondary)
df <- as.data.frame(df)

i=1
for (i in 1:n){
  if(!is.na(str_match(f1$V1[i],"\\[Term\\]"))){
    df$doid[i] <- f1[i+1];
    df$Term[i] <- f1[i+2];
    j=1
  while (is.na(str_match(f1$V1[i+2+j],"\\[Term\\]" )) & j < 50) {
    if(!is.na(str_match(f1$V1[i+2+j],"synonym:"))){
      df$Synonym[i] <- f1[i+2+j];
    }
    else if(!is.na(str_match(f1$V1[i+2+j],"alt_id:"))){
      df$Secondary[i] <- f1[i+2+j];
       }
    j=j+1;
    }
  }
  i=i+2+j #turn to next term
}

#process data
locate <-which(!is.na(str_match(df$doid,"id: DOID")))
df <- df[locate,]
df$doid <- gsub("id: ","",df$doid)
df$Term <- gsub("name: ","",df$Term)
df$Synonym <- gsub("synonym: ","",df$Synonym)
df$Secondary <- gsub("alt_id: ","",df$Secondary)

#save data
write.csv(df, file = "./DOTERM.csv")

