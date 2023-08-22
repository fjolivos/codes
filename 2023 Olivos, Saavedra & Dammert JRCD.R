#   Code for 
#   Olivos, F., Saavedra, P., & Dammert, L. (2022). Citizen complaints as an accountability: 
#   Uncovering patterns using topic modeling. Journal of Research in Crime and Delinquency.


#:::::::::::::::::Police complaints

library(tidytext)
library(dplyr)
library(tm)
library(stringr)
library(quanteda)
library(corpus)
library(tidyverse)
library(stm)
library(qdap)

#Loading 
police <- readRDS("C:/Users/Francisco/Dropbox/Data/Scrapped/carabineros.rds")
police <- subset(police, select = -c(Metadata) )
police2 <- readRDS("C:/Users/Francisco/Dropbox/Data/Scrapped/carabineros_upd.rds")

police2020 <- rbind(police2, police)
police2020 <- police2020[-c(301:427), ] 
police<- police2020 %>% 
  separate(Information, c("City", "Province", "Day", "Month"), sep = ",", remove = FALSE)


#check the errors

police<- police %>% 
  separate(Month, c("M_Y", "Other"), sep = "Número", remove = TRUE)

police<- police %>% 
  separate(M_Y, c("NA", "Month", "Year"), sep = " ", remove = TRUE)

police<- police %>% 
  separate(Day, c("NA", "ChileDay", "Num"), sep = " ", remove = TRUE)

police<- police %>% 
  separate(ChileDay, c("NA2","Day"), sep = "Chile", remove = TRUE)


#Descriptives
library(arsenal)
table_one <- tableby(~ Year, data = police)
summary(table_one, title = "Year")

table_two <- tableby(~ zone, data = police)
summary(table_two, title = "zone")

#Creation of average word lenght as proxy of edu level

tchar <- nchar(gsub(" ", "",police$Complain)) # number of characters, exc space

tword <- lengths(gregexpr("\\w+",police$Complain)) #number of words (spaces)

police <- transform(police, alength = tchar / tword)

alength

str_count(police$Complain, c("á", "é", "í", "ó", "ú"))

police$Complain.of.a <- str_count(police$Complain, "á")
police$Complain.of.e <- str_count(police$Complain, "é")
police$Complain.of.i <- str_count(police$Complain, "í")
police$Complain.of.o <- str_count(police$Complain, "ó")
police$Complain.of.u <- str_count(police$Complain, "ú")

police <- transform(police, ctones = Complain.of.a + Complain.of.e + Complain.of.i +
                      Complain.of.o + Complain.of.u)

police <- transform(police, ptones = tword /  ctones )

police$ptones[police$ctones==0] <-0

police$nse4 <- ntile(police$ptones, 4) 

police$nse4[police$nse4==1] <-"Q1"
police$nse4[police$nse4==2] <-"Q2"
police$nse4[police$nse4==3] <-"Q3"
police$nse4[police$nse4==4] <-"Q4"

head(police$Complain, 6)

#Tokenization
tidy_police <- police %>% 
  mutate(line = row_number()) %>% # register the complain to which the word belong
  unnest_tokens(word, Complain)

#Identify most common words
tidy_police %>%
  count(word) %>%
  arrange(desc(n))


#Remove stopwords 
custom_stop_words <- bind_rows(stop_words,
                               data_frame(word = tm::stopwords("spanish"),
                                          lexicon = "custom"))
tidy_police<-tidy_police %>%
  anti_join(custom_stop_words)


##Remove numbers
tidy_police<-tidy_police[-grep("\\b\\d+\\b", tidy_police$word),]

##Remove empty spaces
tidy_police$word <- gsub("\\s+","",tidy_police$word)

#Remove other words
common <- c("carabineros", "carabinero", "carabineros", "si", "mas", "hacia", "mismo", "así", "asi", "cada", "aun", "tan", "dos", "solo")
common <- as.data.frame(common)
colnames(common) <- "word"
tidy_police<-tidy_police %>%
  anti_join(common)

#Identify most common words
tidy_police %>%
  count(word) %>%
  arrange(desc(n))

##Plot frequency

tidy_police_top_words<-
  tidy_police %>%
  count(word) %>%
  arrange(desc(n))

tidy_police_top_words

#Selecting most frequent word
top_20<-tidy_police_top_words[1:20,]

#Vector to order by frequency
tidy_police_top_words$word <- factor(tidy_police_top_words$word, levels = tidy_police_top_words$word[order(tidy_police_top_words$n,decreasing=TRUE)])


top_20$word <- factor(top_20$word, levels = top_20$word[order(top_20$n,decreasing=TRUE)])


# Figure 3
library(ggplot2)
ggplot(top_20, aes(x=word, y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Frequency of words")+
  xlab("")+
  guides(fill=FALSE) +
  scale_x_discrete(breaks=c("años","auto","calle","casa", "chile", "comisaria", "día", "forma", "funcionario", "hace", "hacer", "lugar", "mal", "momento", "parte", "persona", "reclamo", "ser", "trabajo", "vehículo"),
                  labels=c("years", "car", "street", "house", "Chile", "precint", "day", "way", "officer", "does", "to do", "place","bad", "moment", "ticket", "person", "complaint", "to be", "job", "vehicle"))


##Topic model

#Few cases in 2012 and 2021
police<-police[!(police$Year=="2012\n"),]
police<-police[!(police$Year=="2021\n"),]
table(police$Year)

#Average number of words

wc(police$Complain, FALSE)/nrow(police)

#Cleaning of complain column
processed <- textProcessor(police$Complain, metadata = police, language = "sp")

#Additional words 
processed <- textProcessor(police$Complain, 
                           metadata = police, stem = FALSE, language = "sp",
                           customstopwords = c("aun","mas", "solo", "reclamo", "asi", "ahi", "ahí", "según", "así", "veces", "aqui", "atras", "vez", "dos",
                                               "luego", "mientras","anterior", "ningun","tambien", "ademas", "segun","despues", "frente"))

#Pre-processing and thresholds 
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 15, upper.thresh = 400)

docs <- out$documents
vocab <- out$vocab
meta <-out$meta

#Topic model for exploration
police_TM1 <- stm(documents = docs, vocab = vocab,
                 K = 25, prevalence =~ Province + Year + Month + nse4,
                 max.em.its = 1, data = meta,
                 init.type = "Spectral", verbose = FALSE)

par(mfrow=c(1,1))

#Basic visualization
plot(police_TM1)

#More detail of word per topic
summary(police_TM1)

#Selection of the K

findingk <- searchK(out$documents, out$vocab, K = c(3:30),
                    prevalence =~ Province + Year + Month + nse4,
                    data = meta, verbose=FALSE) 
plot(findingk)

police_TM5 <- stm(documents = docs, vocab = vocab,
                  K = 5, prevalence =~ Province + Year + Month + nse4,
                  max.em.its = 100, data = meta,
                  init.type = "Spectral", verbose = FALSE)

police_TM6 <- stm(documents = docs, vocab = vocab,
                  K = 6, prevalence =~ zone + Year + Month + nse4,
                  max.em.its = 100, data = meta,
                  init.type = "Spectral", verbose = FALSE)

police_TM7 <- stm(documents = docs, vocab = vocab,
                  K = 7, prevalence =~ zone + Year + Month + nse4,
                  max.em.its = 100, data = meta,
                  init.type = "Spectral", verbose = FALSE)
#Chosen solution
police_TM8 <- stm(documents = docs, vocab = vocab,
                  K = 8, prevalence =~ Province + Year + Month + nse4,
                  max.em.its = 100, data = meta,
                  init.type = "Spectral", verbose = FALSE)
#Figure 4
police_TM8


proportionsTM8 <- make.dt(police_TM8, meta = NULL)

prTM8_T1 <- mean(proportionsTM8$Topic1)
prTM8_T2 <- mean(proportionsTM8$Topic2)
prTM8_T3 <- mean(proportionsTM8$Topic3)
prTM8_T4 <- mean(proportionsTM8$Topic4)
prTM8_T5 <- mean(proportionsTM8$Topic5)
prTM8_T6 <- mean(proportionsTM8$Topic6)
prTM8_T7 <- mean(proportionsTM8$Topic7)
prTM8_T8 <- mean(proportionsTM8$Topic8)

Topic <- 1:8
Label <- c("T1: Employee","T2: Traffic","T3: Control","T4: Noise",
           "T5: Household","T6: Parking","T7: Certificates","T8: Generalized")
Proportion <- c(prTM8_T1,prTM8_T2,prTM8_T3,prTM8_T4,
                prTM8_T5,prTM8_T6,prTM8_T7,prTM8_T8)
prTM8_df <- data.frame(Topic, Label, Proportion)

#Figure 5
ggplot(prTM8_df, aes(x=Label, y=Proportion)) +
      geom_bar(stat= "identity") + xlab("Topics") + ylab("Average prevalence")

print(proportionsT8)

police_TM20 <- stm(documents = docs, vocab = vocab,
                  K = 20, prevalence =~ zone + Year + Month + nse4,
                  max.em.its = 100, data = meta,
                  init.type = "Spectral", verbose = FALSE)
#Iteration changed only for exploration
police_TM25 <- stm(documents = docs, vocab = vocab,
                  K = 25, prevalence =~ Province + Year + Month + nse4,
                  max.em.its = 100, data = meta,
                  init.type = "Spectral", verbose = FALSE)

plot(police_TM5, xlim = c(0, 1))
plot(police_TM8, xlim = c(0, .8))
summary(police_TM25)


#Representative documents

thougts1<-findThoughts(police_TM8, texts = police$Complain, n = 3, topics = 1)
thougts1

thougts2<-findThoughts(police_TM8, texts = police$Complain, n = 3, topics = 2)
thougts2

thougts3<-findThoughts(police_TM8, texts = police$Complain, n = 3, topics = 3)
thougts3

thougts4<-findThoughts(police_TM8, texts = police$Complain, n = 3, topics = 4)
thougts4

thougts5<-findThoughts(police_TM8, texts = police$Complain, n = 3, topics = 5)
thougts5

thougts6<-findThoughts(police_TM8, texts = police$Complain, n = 3, topics = 6)
thougts6

thougts7<-findThoughts(police_TM8, texts = police$Complain, n = 3, topics = 7)
thougts7

thougts8<-findThoughts(police_TM8, texts = police$Complain, n = 3, topics = 8)
thougts8

#Representative words

labelTopics(police_TM8, c(1, 2, 3, 4, 5, 6, 7, 8), n=15)

plot(police_TM5)
plot(police_TM6)
plot(police_TM8)
plot(police_TM20)

plot(police_TM25)

labelTopics(police_TM25, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 
                           12, 13, 14, 15, 16, 17, 18, 19, 20,
                           21, 22, 23, 24, 25), n=10)

#Correlation for high dimensional TM

mod8.out.corr <- topicCorr(police_TM8)
mod8.out.corr
plot(mod8.out.corr)

mod20.out.corr <- topicCorr(police_TM20)
mod20.out.corr
plot(mod20.out.corr)

mod25.out.corr <- topicCorr(police_TM25)
mod25.out.corr
plot(mod25.out.corr, weighted=TRUE)

library(stmCorrViz)

#Hierarchical Topics (appendix)

stmCorrViz(police_TM8, "corrviz.html", documents_raw=NULL, documents_matrix=NULL,
           title="STM Model", clustering_threshold=FALSE,
           search_options = list(range_min=.05, range_max=5, step=.05),
           labels_number=7, display=TRUE, verbose=FALSE)

stmCorrViz(police_TM20, "corrviz.html", documents_raw=NULL, documents_matrix=NULL,
           title="STM Model", clustering_threshold=FALSE,
           search_options = list(range_min=.05, range_max=5, step=.05),
           labels_number=7, display=TRUE, verbose=FALSE)

stmCorrViz(police_TM25, "corrviz.html", documents_raw=NULL, documents_matrix=NULL,
           title="STM Model", clustering_threshold=FALSE,
           search_options = list(range_min=.05, range_max=5, step=.05),
           labels_number=7, display=TRUE, verbose=FALSE)



#Explaining topics

police_TM8b <- stm(documents = docs, vocab = vocab,
                  K = 8, #prevalence =~ zone + Year + Month,
                  max.em.its = 100, data = meta,
                  init.type = "Spectral", verbose = FALSE)

predict_topics<-estimateEffect(formula = 1:8 ~ Province + Year + Month + nse4,
                               stmobj = police_TM8, metadata = out$meta, 
                               uncertainty = "Global")
print(predict_topics)

out$meta$Year <- as.factor(out$meta$Year)
out$meta$zone <- as.factor(out$meta$zone)

predict_topics<-estimateEffect(formula = 1:20 ~ Year + Province + Month,
                               stmobj = police_TM20, metadata = out$meta, 
                               uncertainty = "Global")

#Tables for appendix (RR)

summary(predict_topics)
summary(predict_topics, topics=1)
summary(predict_topics, topics=2)
summary(predict_topics, topics=3)
summary(predict_topics, topics=4)
summary(predict_topics, topics=5)
summary(predict_topics, topics=6)
summary(predict_topics, topics=7)
summary(predict_topics, topics=8)

library(stminsights)

effects <- get_effects(estimates = predict_topics,
                       variable = 'Year',
                       type = 'pointestimate')
summary(effects)

#Figure 6


effects %>% filter (topic == 1) %>%
  ggplot(aes(x = proportion, y = value)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.15, size = 1) +
  coord_flip() + theme_light() + labs(title = 'Topic 1: Employee', y = '', x = 'Prevalence') +
  xlim(-.10, .40)

effects %>% filter (topic == 2) %>%
  ggplot(aes(x = proportion, y = value)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.15, size = 1) +
  coord_flip() + theme_light() + labs(title = 'Topic 2: Traffic', y = '', x = '') +
  xlim(-.10, .40)


effects %>% filter (topic == 3) %>%
  ggplot(aes(x = proportion, y = value)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.15, size = 1) +
  coord_flip() + theme_light() + labs(title = 'Topic 3: Control', y = '', x = '') +
  xlim(-.10, .40)


effects %>% filter (topic == 4) %>%
  ggplot(aes(x = proportion, y = value)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.15, size = 1) +
  coord_flip() + theme_light() + labs(title = 'Topic 4: Noise', y = '', x = '') +
  xlim(-.10, .40)


effects %>% filter (topic == 5) %>%
  ggplot(aes(x = proportion, y = value)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.15, size = 1) +
  coord_flip() + theme_light() + labs(title = 'Topic 5: Household', y = 'Year', x = 'Prevalence') +
  xlim(-.10, .40)


effects %>% filter (topic == 6) %>%
  ggplot(aes(x = proportion, y = value)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.15, size = 1) +
  coord_flip() + theme_light() + labs(title = 'Topic 6: Parking', y = 'Year', x = '') +
  xlim(-.10, .40)


effects %>% filter (topic == 7) %>%
  ggplot(aes(x = proportion, y = value)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.15, size = 1) +
  coord_flip() + theme_light() + labs(title = 'Topic 7: Certificates', y = 'Year', x = '') +
  xlim(-.10, .40)


effects %>% filter (topic == 8) %>%
  ggplot(aes(x = proportion, y = value)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.15, size = 1) +
  coord_flip() + theme_light() + labs(title = 'Topic 8: Generalized', y = 'Year', x = '') +
  xlim(-.10, .40)


#Figure 7 

effects <- get_effects(estimates = predict_topics,
                       variable = 'nse4',
                       type = 'pointestimate')

effects %>% filter (topic == 1) %>%
  ggplot(aes(x = proportion, y = value)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.15, size = 1) +
  coord_flip() + theme_light() + labs(title = 'Topic 1: Employee', y = '', x = 'Prevalence') +
  xlim(-0.1, .40)

effects %>% filter (topic == 2) %>%
  ggplot(aes(x = proportion, y = value)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.15, size = 1) +
  coord_flip() + theme_light() + labs(title = 'Topic 2: Traffic', y = '', x = '') +
  xlim(-0.1, .40)

effects %>% filter (topic == 3) %>%
  ggplot(aes(x = proportion, y = value)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.15, size = 1) +
  coord_flip() + theme_light() + labs(title = 'Topic 3: Control', y = '', x = '') +
  xlim(-0.1, .40)

effects %>% filter (topic == 4) %>%
  ggplot(aes(x = proportion, y = value)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.15, size = 1) +
  coord_flip() + theme_light() + labs(title = 'Topic 4: Noise', y = '', x = '') +
  xlim(-0.1, .40)

effects %>% filter (topic == 5) %>%
  ggplot(aes(x = proportion, y = value)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.15, size = 1) +
  coord_flip() + theme_light() + labs(title = 'Topic 5: Household', y = 'Educational Level', x = 'Prevalence') +
  xlim(-0.1, .40)

effects %>% filter (topic == 6) %>%
  ggplot(aes(x = proportion, y = value)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.15, size = 1) +
  coord_flip() + theme_light() + labs(title = 'Topic 6: Parking', y = 'Educational Level', x = '') +
  xlim(-0.1, .40)

effects %>% filter (topic == 7) %>%
  ggplot(aes(x = proportion, y = value)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.15, size = 1) +
  coord_flip() + theme_light() + labs(title = 'Topic 7: Certificates', y = 'Educational Level', x = '') +
  xlim(-0.1, .40)

effects %>% filter (topic == 8) %>%
  ggplot(aes(x = proportion, y = value)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.15, size = 1) +
  coord_flip() + theme_light() + labs(title = 'Topic 8: Generalized', y = 'Educational Level', x = '') +
  xlim(-0.1, .40)



##Reviwer 3 asked for physical and verbal behavior of officers
# We can search specific words:

lab <- sageLabels(police_TM8, n=100)
findTopic(lab, c("dijo"), type = c("prob", "frex", "lift", "score"))
findTopic(lab, c("dice"), type = c("prob", "frex", "lift", "score"))
findTopic(lab, c("dise"), type = c("prob", "frex", "lift", "score"))
findTopic(lab, c("grito"), type = c("prob", "frex", "lift", "score"))
findTopic(lab, c("grita"), type = c("prob", "frex", "lift", "score"))
findTopic(lab, c("procedimiento"), type = c("prob", "frex", "lift", "score"))
findTopic(lab, c("insulto"), type = c("prob", "frex", "lift", "score"))
findTopic(lab, c("fuerza"), type = c("prob", "frex", "lift", "score"))
findTopic(lab, c("violencia"), type = c("prob", "frex", "lift", "score"))
findTopic(lab, c("golpe"), type = c("prob", "frex", "lift", "score"))
findTopic(lab, c("golpear"), type = c("prob", "frex", "lift", "score"))
findTopic(lab, c("golpeo"), type = c("prob", "frex", "lift", "score"))
findTopic(lab, c("golpio"), type = c("prob", "frex", "lift", "score"))
findTopic(lab, c("pego"), type = c("prob", "frex", "lift", "score"))






