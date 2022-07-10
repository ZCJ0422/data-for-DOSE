
library(stringr)

#read HumanDO.obo file and select data
f1<-data.table::fread("./HumanDO.obo",encoding = "UTF-8",fill = T,skip = 28,header = F)
locate <-which(!is.na(str_match(f1$V1,"\\[Term\\]"))|!is.na(str_match(f1$V1,"id: DOID"))|!is.na(str_match(f1$V1,"is_a:")))
f1<-f1[locate,]
locate <- which(is.na(str_match(f1$V1,"alt_id: DOID")))
f1 <- f1[locate,]
m <- as.matrix(f1)

#extract data into a dataframe
n = nrow(f1)
doid <- c(1:n)
is_a <- c(NA)
df <- cbind(doid,is_a) %>% as.data.frame()
i=1
for (i in 1:n) {
  if (!is.na(str_match(m[i],"\\[Term\\]")) & !is.na(str_match(m[i+3],"\\[Term\\]"))){
    df$doid[i]<-m[i+1];
    df$is_a[i]<-m[i+2];
    i=i+3
  }
  else {
    df$doid[i]<-m[i+1];
    df$is_a[i]<-"is_a: NA";
    i=i+2
  }
}
locate <-which(!is.na(str_match(df$doid,"id: DOID")))
df <- df[locate,]

#process data
df$doid <- gsub("id:","",df$doid)
df$is_a <- str_extract(df$is_a,"(?<=\\is_a:).+?(?=\\!)")

#the last data should be added manually
n = nrow(df)
df$is_a[n] <- "DOID:12930"

#save data
write.csv(df, file = "./PARENTS.csv")

