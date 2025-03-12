## Systematic review ##


# results and stats for internal review deadline ####

stats <- read.csv('sysstats.csv', header=T)
docs <- read.csv('sysdocs.csv', header=T)
abs <- read.csv('abstats.csv', header=T)
exclude <- read.csv('exclude.csv', header=T)

head(stats)
head(docs)
head(abs)

length(unique(stats$Reviewer.A))
length(unique(abs$Reviewer.A))

sa <- stats$Reviewer.A
sb <- stats$Reviewer.B
aa <- abs$Reviewer.A
ab <- abs$Reviewer.B

sc <- c(sa, sb)
ac <- c(aa, ab)

length(unique(sc))
length(unique(ac))

mean(abs$Proportionate.Agreement)
sd(abs$Proportionate.Agreement)
mean(abs$Random.Agreement.Probability)
sd(abs$Random.Agreement.Probability)
abs$Cohen.s.Kappa <- as.numeric(abs$Cohen.s.Kappa)
mean(abs$Cohen.s.Kappa, na.rm=T)
sd(abs$Cohen.s.Kappa, na.rm=T)

mean(stats$Proportionate.Agreement)
sd(stats$Proportionate.Agreement)
mean(stats$Random.Agreement.Probability)
sd(stats$Random.Agreement.Probability)
stats$Cohen.s.Kappa <- as.numeric(stats$Cohen.s.Kappa)
mean(stats$Cohen.s.Kappa, na.rm=T)
sd(stats$Cohen.s.Kappa, na.rm=T)

library(splitstackshape)

docnew <- cSplit(docs, 'Tags', ";")
nrow(docnew[docnew$Tags_1 == 'Biotic',])
nrow(docnew[docnew$Tags_2 == 'Biotic',])
nrow(docnew[docnew$Tags_3 == 'Biotic',])
nrow(docnew[docnew$Tags_4 == 'Biotic',])
nrow(docnew[docnew$Tags_5 == 'Biotic',])
nrow(docnew[docnew$Tags_6 == 'Biotic',])

nrow(docnew[docnew$Tags_1 == 'Human movement',])
nrow(docnew[docnew$Tags_2 == 'Human movement',])
nrow(docnew[docnew$Tags_3 == 'Human movement',])
nrow(docnew[docnew$Tags_4 == 'Human movement',])
nrow(docnew[docnew$Tags_5 == 'Human movement',])
nrow(docnew[docnew$Tags_6 == 'Human movement',])

nrow(docnew[docnew$Tags_1 == 'Biophysical',])
nrow(docnew[docnew$Tags_2 == 'Biophysical',])
nrow(docnew[docnew$Tags_3 == 'Biophysical',])
nrow(docnew[docnew$Tags_4 == 'Biophysical',])
nrow(docnew[docnew$Tags_5 == 'Biophysical',])
nrow(docnew[docnew$Tags_6 == 'Biophysical',])


nrow(docnew[docnew$Tags_1 == 'Sociocultural',])
nrow(docnew[docnew$Tags_2 == 'Sociocultural',])
nrow(docnew[docnew$Tags_3 == 'Sociocultural',])
nrow(docnew[docnew$Tags_4 == 'Sociocultural',])
nrow(docnew[docnew$Tags_5 == 'Sociocultural',])
nrow(docnew[docnew$Tags_6 == 'Sociocultural',])

head(docnew)

exnew <- cSplit(exclude, 'Tags', ';')
head(exnew)
nrow(exnew[exnew$Tags_1 == 'Transboundary unclear',])
nrow(exnew[exnew$Tags_2 == 'Transboundary unclear',])
nrow(exnew[exnew$Tags_3 == 'Transboundary unclear',])
nrow(exnew[exnew$Tags_4 == 'Transboundary unclear',])
nrow(exnew[exnew$Tags_5 == 'Transboundary unclear',])
nrow(exnew[exnew$Tags_6 == 'Transboundary unclear',])

nrow(exnew[exnew$Tags_1 == 'Human movement',])
nrow(exnew[exnew$Tags_2 == 'Human movement',])
nrow(exnew[exnew$Tags_3 == 'Human movement',])
nrow(exnew[exnew$Tags_4 == 'Human movement',])
nrow(exnew[exnew$Tags_5 == 'Human movement',])
nrow(exnew[exnew$Tags_6 == 'Human movement',])

nrow(exnew[exnew$Tags_1 == 'Biophysical',])
nrow(exnew[exnew$Tags_2 == 'Biophysical',])
nrow(exnew[exnew$Tags_3 == 'Biophysical',])
nrow(exnew[exnew$Tags_4 == 'Biophysical',])
nrow(exnew[exnew$Tags_5 == 'Biophysical',])
nrow(exnew[exnew$Tags_6 == 'Biophysical',])


nrow(exnew[exnew$Tags_1 == 'Sociocultural',])
nrow(exnew[exnew$Tags_2 == 'Sociocultural',])
nrow(exnew[exnew$Tags_3 == 'Sociocultural',])
nrow(exnew[exnew$Tags_4 == 'Sociocultural',])
nrow(exnew[exnew$Tags_5 == 'Sociocultural',])
nrow(exnew[exnew$Tags_6 == 'Sociocultural',])

# random sampling for coding of various flows ####

setwd('~/Downloads/')


docs <- read.csv('november1codingextraction.csv', header=T)

library(splitstackshape)
library(dplyr)
library(tidyr)
library(reshape2)

docnew <- cSplit(docs, 'Tags', ";")
nrow(docnew[docnew$Tags_1 == 'Biotic',])
nrow(docnew[docnew$Tags_2 == 'Biotic',])
nrow(docnew[docnew$Tags_3 == 'Biotic',])
nrow(docnew[docnew$Tags_4 == 'Biotic',])
nrow(docnew[docnew$Tags_5 == 'Biotic',])
nrow(docnew[docnew$Tags_6 == 'Biotic',])

nrow(docnew[docnew$Tags_1 == 'Human movement',])
nrow(docnew[docnew$Tags_2 == 'Human movement',])
nrow(docnew[docnew$Tags_3 == 'Human movement',])
nrow(docnew[docnew$Tags_4 == 'Human movement',])
nrow(docnew[docnew$Tags_5 == 'Human movement',])
nrow(docnew[docnew$Tags_6 == 'Human movement',])

nrow(docnew[docnew$Tags_1 == 'Biophysical',])
nrow(docnew[docnew$Tags_2 == 'Biophysical',])
nrow(docnew[docnew$Tags_3 == 'Biophysical',])
nrow(docnew[docnew$Tags_4 == 'Biophysical',])
nrow(docnew[docnew$Tags_5 == 'Biophysical',])
nrow(docnew[docnew$Tags_6 == 'Biophysical',])


nrow(docnew[docnew$Tags_1 == 'Sociocultural',])
nrow(docnew[docnew$Tags_2 == 'Sociocultural',])
nrow(docnew[docnew$Tags_3 == 'Sociocultural',])
nrow(docnew[docnew$Tags_4 == 'Sociocultural',])
nrow(docnew[docnew$Tags_5 == 'Sociocultural',])
nrow(docnew[docnew$Tags_6 == 'Sociocultural',])

head(docnew)
colnames(docnew)[13] <- 'CovID'

# set.seed(7)
# samples <- docnew %>% group_by(Tags_1) %>% sample_frac(.5)
# head(samples)
# nrow(samples[samples$value=='Biophysical',])
# nrow(samples[samples$value=='Biotic',])
# nrow(samples[samples$value=='Human movement',])
# nrow(samples[samples$value=='Sociocultural',])
# head(samples)

docmelt <- docnew[,c(13,16:20)]
docmolted=melt(docmelt,id.vars=c("CovID"))
head(docmolted)
docmolted <- docmolted %>% drop_na(value)  
docmolted <- docmolted[,c(1,3)]
  
docnew <- docnew[,c(1:15)]

mergemolt <- left_join(docmolted, docnew, by='CovID')
mergemolt <- distinct(mergemolt)

#testsamples <- subset(mergemolt, Title %in% c("Loss and Damage in the Rapidly Changing Arctic",
                               #                "A summary of the effects of climate change on Ontario's aquatic ecosystems",
                               #                "Adaptive strategies and life history characteristics in a warming climate: Salmon in the Arctic?",
                               # "The use of environmental data in descriptive and predictive models of vector-borne disease in North America", 
                               # "Synthesis of habitat models for management of wolverine<i> (Gulo</i><i> gulo):</i> Identifying key habitat and snow refugia in the Columbia and Rocky Mountains, Canada",
                               # "Migration matters: How migration is critical to contemporary human-environment geography", 
                               # "Climate-related migration and the climate-security-migration nexus in the Central American Dry Corridor",
                               # "Climate impacts on migration in the Arctic North America: existing evidence and research recommendations", 
                               # "The rise of phase-out as a critical decarbonisation approach: a systematic review",
                               # "Deliberative framing: opening up discussions for local-level public engagement on climate change", 
                               # "Impacts of colonization on Indigenous food systems in Canada and the United States: a scoping review",
                               # "A primer on potential impacts, management priorities, and future directions for <i>Elodea</i> spp. in high latitude systems: learning from the Alaskan experience",
                               # "A review of Canadian Arctic killer whale (Orcinus orca) ecology","Evidence for widespread changes in the structure, composition, and fire regimes of western North American forests"))
mergemolt$test <- 'NA'
mergemolt$test[mergemolt$Title == "Loss and Damage in the Rapidly Changing Arctic" & mergemolt$value == 'Biophysical'] <- 'test'
mergemolt$test[mergemolt$Title == "A summary of the effects of climate change on Ontario's aquatic ecosystems" & mergemolt$value == 'Biophysical'] <- 'test'
mergemolt$test[mergemolt$Title == "Adaptive strategies and life history characteristics in a warming climate: Salmon in the Arctic?" & mergemolt$value == 'Biotic'] <- 'test'
mergemolt$test[mergemolt$Title == "The use of environmental data in descriptive and predictive models of vector-borne disease in North America" & mergemolt$value == 'Biotic'] <- 'test'
mergemolt$test[mergemolt$Title == "Synthesis of habitat models for management of wolverine<i> (Gulo</i><i> gulo):</i> Identifying key habitat and snow refugia in the Columbia and Rocky Mountains, Canada" & mergemolt$value == 'Biotic'] <- 'test'
mergemolt$test[mergemolt$Title == "Migration matters: How migration is critical to contemporary human-environment geography" & mergemolt$value == 'Human movement'] <- 'test'
mergemolt$test[mergemolt$Title == "Climate-related migration and the climate-security-migration nexus in the Central American Dry Corridor" & mergemolt$value == 'Human movement'] <- 'test'
mergemolt$test[mergemolt$Title == "The rise of phase-out as a critical decarbonisation approach: a systematic review" & mergemolt$value == 'Sociocultural'] <- 'test'
mergemolt$test[mergemolt$Title == "Deliberative framing: opening up discussions for local-level public engagement on climate change" & mergemolt$value == 'Sociocultural'] <- 'test'
mergemolt$test[mergemolt$Title == "Impacts of colonization on Indigenous food systems in Canada and the United States: a scoping review" & mergemolt$value == 'Sociocultural'] <- 'test'
mergemolt$test[mergemolt$Title == "A primer on potential impacts, management priorities, and future directions for <i>Elodea</i> spp. in high latitude systems: learning from the Alaskan experience" & mergemolt$value == 'Biotic'] <- 'test'
mergemolt$test[mergemolt$Title == "A review of Canadian Arctic killer whale (Orcinus orca) ecology" & mergemolt$value == 'Biotic'] <- 'test'
mergemolt$test[mergemolt$Title == "Evidence for widespread changes in the structure, composition, and fire regimes of western North American forests" & mergemolt$value == 'Biophysical'] <- 'test'
mergemolt$test[mergemolt$Title == "Climate impacts on migration in the Arctic North America: existing evidence and research recommendations" & mergemolt$value == 'Human movement'] <- 'test'

#write.csv(mergemolt, 'reshapednovember1extract.csv')

#mergemolt <- anti_join(mergemolt, testsamples, by="Title")

set.seed(7)
samples <- mergemolt[mergemolt$test !='test',] %>% group_by(value) %>% sample_frac(.5)
head(samples)
nrow(samples[samples$value=='Biophysical',])
nrow(samples[samples$value=='Biotic',])
nrow(samples[samples$value=='Human movement',])
nrow(samples[samples$value=='Sociocultural',])

sampleswithtest <- rbind(mergemolt[mergemolt$test =='test',], samples)
nrow(sampleswithtest[sampleswithtest$value=='Biophysical',])
nrow(sampleswithtest[sampleswithtest$value=='Biotic',])
nrow(sampleswithtest[sampleswithtest$value=='Human movement',])
nrow(sampleswithtest[sampleswithtest$value=='Sociocultural',])

#write.csv(sampleswithtest, 'newsamples_november1.csv')

soc <- mergemolt[mergemolt$value == 'Sociocultural',]
#write.csv(soc, 'sociocultural_oct16.csv')
