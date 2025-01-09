

#setwd('~/Downloads/')
#docs <- fread('v3form.csv', header=T)

colnames(docs)

library(splitstackshape)
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)

#column names for each tag
tags = unique(trimws(unlist(strsplit(docs$'2.1. What type of flow is it?', split=", "))))
for(tag in tags) {
  set(docs, j=tag, value=F)
  set(docs, i=grep(tag, docs$'2.1. What type of flow is it?'), j=tag, value=T)
}

setcolorder(docs, 
            neworder=c("Sociocultural",
                       "Human movement",
                       "Biophysical",
                       "Biotic"
                      ),
            after = "2.1. What type of flow is it?")
#write.csv(docs, 'newdocjan7.csv')
