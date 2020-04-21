#################################################################
# Preliminary analysis of COVID-19 academic information patterns: 
# A call for open science in the times of closed borders
# Homolak, Kodvanj, Virag
#################################################################


searchF <- function(search_topic_TopicSpecified){
  search_query <- EUtilsSummary(search_topic_TopicSpecified,  retmax = 9999)
  summary(search_query)
  Sys.sleep(3)
  records <- EUtilsGet(search_query)
  return(records)
}

sptime <- function(pm){
  
  pm <- pm[!is.na(pm$YMD_Accepted),]
  pm <- pm[!is.na(pm$YMD_Received),]
  pm <- pm %>%  mutate(InReview =  YMD_Accepted - YMD_Received)
  
  pm$JournalCount <- table(pm$Journal)[pm$Journal]  
  
  return(pm)
}

rismedextract <- function(records){
  pubmed_data_full <- data.frame('PMID'=as.character(PMID(records)),
                                 'DayAccepted' = DayAccepted(records),
                                 'MonthAccepted' = MonthAccepted(records),
                                 'YearAccepted' = YearAccepted(records),
                                 'DayReceived' = DayReceived(records),
                                 'MonthReceived' = MonthReceived(records),
                                 'YearReceived' = YearReceived(records),
                                 'Journal' = Title(records),
                                 'PublicationStatus' = PublicationStatus(records),
                                 'Country' = Country(records),
                                 'Language' = Language(records)
                
  )
  pubmed_data_full <- pubmed_data_full %>%
    mutate(YMD_Accepted = ymd(paste(YearAccepted,MonthAccepted, DayAccepted))) %>%
    mutate(YMD_Received = ymd(paste(YearReceived,MonthReceived, DayReceived))) 
  return(pubmed_data_full)
}


AuthorCountF <- function(x){
  c <- c()
  for (i in 1:length(x)){
    c[i] <- length(unlist(str_split(x[[i]], ";")))
  }
  return(c)
}

AffiliationCountF <- function(x){
  c <- c()
  for (i in 1:length(x)){
    c[i] <- length(unlist(str_split(x[[i]], ";")))
  }
  return(c)
}

countryF <- function(x){
  for (i in 1:dim(x)[1]){
    af <- unlist(str_split(x$AU_UN[i], ";"))
    x$duplicated[i] <- TRUE %in% duplicated(af)
    for (j in af){
      
    }
  }
  
  
  
}





library(ggplot2)
library(lubridate)
library(dplyr)
library(ggsci)
library(RISmed)
library(stringr)
library(devEMF)
library(plotly)
library(rjson)
library(bibliometrix)
library(pubmedR)


tema <- theme_bw() + theme( plot.subtitle = element_text(vjust = 1), 
                            plot.caption = element_text(vjust = 1), 
                            panel.grid.major = element_blank(), 
                            panel.grid.minor = element_blank(), 
                            axis.title = element_text(size = 9), 
                            axis.text = element_text(size = 8, colour = "black"), 
                            panel.background = element_rect(fill = NA),
                            panel.border = element_rect(linetype = "solid", fill = NA, size = 1)
                            
)


theme_set(tema)






search1 <- '(COVID-19) AND (Case Reports[Publication Type] OR English Abstract[Publication Type] OR Guideline[Publication Type] OR Journal Article[Publication Type] OR Multicenter Study[Publication Type] OR Review[Publication Type])'

search1atime <- Sys.time()
records1 <- searchF(search1)

search1btime <- Sys.time()
pubmed1 <- pmApiRequest(search1, limit=8000, api_key = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx") ## insert API key
df1 <- pmApi2df(pubmed1, format = "bibliometrix")


search2 <- 'COVID-19'

search2atime <- Sys.time()
records2 <- searchF(search2)

search2btime <- Sys.time()
pubmed2 <- pmApiRequest(search2, limit=8000, api_key = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx") ## insert API key
df2 <- pmApi2df(pubmed2, format = "bibliometrix")

# Top
pm2 <- rismedextract(records2)
pm2 <- sptime(pm2)
x <- pm2 %>%
  filter(JournalCount > 15)
popis <- levels(factor(x$Journal))
searchp <- paste('$',popis[1],'$[Journal]', sep = "")
for (i in 2:length(popis)) {
  searchp <- paste(searchp, paste('$', popis[i], '$',"[Journal]", sep = ""), sep = " OR ")
}
write.csv(searchp, "searchphrase.csv")


search3 <- '("2019/12/01"[Date - Publication] : "2020/12/12"[Date - Publication]) AND (COVID-19) AND ("International journal of antimicrobial agents"[Journal] OR "International journal of infectious diseases : IJID : official publication of the International Society for Infectious Diseases"[Journal] OR "Journal of clinical medicine"[Journal] OR "Journal of Korean medical science"[Journal] OR "Journal of medical virology"[Journal] OR "Journal of microbiology, immunology, and infection = Wei mian yu gan ran za zhi"[Journal] OR "Journal of the American Academy of Dermatology"[Journal] OR "Lancet (London, England)"[Journal] OR "The Journal of hospital infection"[Journal] OR "The Journal of infection"[Journal] OR "The Lancet. Infectious diseases"[Journal] OR "The Lancet. Public health"[Journal] OR "The Lancet. Respiratory medicine"[Journal] OR "Travel medicine and infectious disease"[Journal])'

search3atime <- Sys.time()
records3 <- searchF(search3)


search3btime <- Sys.time()
pubmed3 <- pmApiRequest(search3, limit=8000, api_key = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx") ## insert API key
df3 <- pmApi2df(pubmed3, format = "bibliometrix")



search4 <- '("2018/12/01"[Date - Publication] : "2019/04/11"[Date - Publication]) AND ("International journal of antimicrobial agents"[Journal] OR "International journal of infectious diseases : IJID : official publication of the International Society for Infectious Diseases"[Journal] OR "Journal of clinical medicine"[Journal] OR "Journal of Korean medical science"[Journal] OR "Journal of medical virology"[Journal] OR "Journal of microbiology, immunology, and infection = Wei mian yu gan ran za zhi"[Journal] OR "Journal of the American Academy of Dermatology"[Journal] OR "Lancet (London, England)"[Journal] OR "The Journal of hospital infection"[Journal] OR "The Journal of infection"[Journal] OR "The Lancet. Infectious diseases"[Journal] OR "The Lancet. Public health"[Journal] OR "The Lancet. Respiratory medicine"[Journal] OR "Travel medicine and infectious disease"[Journal])'
search4atime <- Sys.time()
records4 <- searchF(search4)

search4btime <- Sys.time()
pubmed4 <- pmApiRequest(search4, limit=5000, api_key = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx") ## insert API key
df4 <- pmApi2df(pubmed4, format = "bibliometrix")


search5 <- '("International journal of antimicrobial agents"[Journal] OR "International journal of infectious diseases : IJID : official publication of the International Society for Infectious Diseases"[Journal] OR "Journal of clinical medicine"[Journal] OR "Journal of Korean medical science"[Journal] OR "Journal of medical virology"[Journal] OR "Journal of microbiology, immunology, and infection = Wei mian yu gan ran za zhi"[Journal] OR "Journal of the American Academy of Dermatology"[Journal] OR "Lancet (London, England)"[Journal] OR "The Journal of hospital infection"[Journal] OR "The Journal of infection"[Journal] OR "The Lancet. Infectious diseases"[Journal] OR "The Lancet. Public health"[Journal] OR "The Lancet. Respiratory medicine"[Journal] OR "Travel medicine and infectious disease"[Journal]) AND ("2019/12/01"[Date - Publication] : "2020/12/12"[Date - Publication]) NOT (COVID-19)'

search5atime <- Sys.time()
records5 <- searchF(search5)

search5btime <- Sys.time()
pubmed5 <- pmApiRequest(search5, limit=9999, api_key = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx") ## insert API key
df5 <- pmApi2df(pubmed5, format = "bibliometrix")






#SP time 
pm1 <- rismedextract(records1)
pm1sp <- sptime(pm1)

pm2 <- rismedextract(records2)
pm2sp <- sptime(pm2)

pm3 <- rismedextract(records3)
pm3sp <- sptime(pm3)

pm4 <- rismedextract(records4)
pm4sp <- sptime(pm4)

pm5 <- rismedextract(records5)
pm5sp <- sptime(pm5)


#Author count
au1 <- AuthorCountF(df1$AU)
au2 <- AuthorCountF(df2$AU)
au3 <- AuthorCountF(df3$AU)
au4 <- AuthorCountF(df4$AU)
au5 <- AuthorCountF(df5$AU)

x <- df3 %>% filter(DT == "JOURNAL ARTICLE")
au3A <- AuthorCountF(x$AU)

y <- df4 %>% filter(DT == "JOURNAL ARTICLE")
au4A <- AuthorCountF(y$AU)

z <- df5 %>% filter(DT == "JOURNAL ARTICLE")
au5A <- AuthorCountF(z$AU)


#Affiliation count
af1 <- AffiliationCountF(df1$AU_UN)
af2 <- AffiliationCountF(df2$AU_UN)
af3 <- AffiliationCountF(df3$AU_UN)
af4 <- AffiliationCountF(df4$AU_UN)
af5 <- AffiliationCountF(df5$AU_UN)

x <- df3 %>% filter(DT == "JOURNAL ARTICLE")
af3A <- AffiliationCountF(x$AU_UN)

y <- df4 %>% filter(DT == "JOURNAL ARTICLE")
af4A <- AffiliationCountF(y$AU_UN)

z <- df5 %>% filter(DT == "JOURNAL ARTICLE")
af5A <- AffiliationCountF(z$AU_UN)

# ACCEPTED RECEIVED
AcceptedP <- function(pm){
  plotAccepted <- ggplot(pm) + 
    geom_bar(aes(x = YMD_Accepted), inherit.aes = FALSE, fill = "Red" ,color = "Red", alpha = 0.4) + 
    scale_x_date(limits = c(ymd("2020-01-15"),ymd("2020-03-21"))) + xlab("Date Accepted") + ylab("Count") 
}
ReceivedP <- function(pm){
  plotReceived <- ggplot(pm) + 
    geom_bar(aes(x = YMD_Received), inherit.aes = FALSE, fill = "Blue", color = "Blue", alpha = 0.4) + 
    scale_x_date(limits = c(ymd("2020-01-15"),ymd("2020-03-21"))) + xlab("Date Received") + ylab("Count")
  
}

plotAcceptedP <- AcceptedP(pm2sp)
plotReceivedP <- ReceivedP(pm2sp)
emf("0 Accepted.emf", width = 3.5, height = 3)
print(plotAcceptedP)
dev.off()
emf("0 Received.emf", width = 3.5, height = 3)
print(plotReceivedP)
dev.off()

#LANGUAGE
languageP <- function(pm){
  pm$Language <- plyr::revalue(pm$Language, 
                  c("chi"="Chinese", "eng"="English", "fre" = "French", "ger" = "German", "ita" = "Italian", "spa" = "Spanish", "por" = "Portuguese", "tur" = "Turkish", "ice" = "Icelandic"))
  pm %>%  ggplot(aes(x = Language, fill = Language)) + geom_bar()  + coord_flip()
  
}
plotLanguage1 <- languageP(pm1)
plotLanguage2 <- languageP(pm2)
plotLanguage5 <- languageP(pm5)

emf("0 Language.emf", width = 3.5, height = 3)
print(plotLanguage2)
dev.off()


# LANGUAGE & COUNTRY
pmx <- pm2
pmx$Language <- plyr::revalue(pmx$Language, 
                             c("chi"="Chinese", "eng"="English", "fre" = "French", "ger" = "German", "ita" = "Italian", "spa" = "Spanish", "por" = "Portuguese", "tur" = "Turkish", "ice" = "Icelandic"))
pmx$CountryCount <- table(pmx$Country)[pmx$Country]  

langcountry <- ggplot(pmx, aes(x=reorder(Country, CountryCount), fill = Language, order=CountryCount)) + geom_bar() + coord_flip() +
  scale_fill_simpsons()


emf("publisherCountryLanguage.emf", width = 5, height = 5)
print(langcountry)
dev.off()

#SP TIME
pl2 <- pm2sp %>% 
  filter(JournalCount > 15) %>%
  ggplot(aes(x = Journal, y = InReview, fill = Journal)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(position = "jitter", size = 0.1) +
  theme(legend.position = "None") + coord_flip() +
  ylab("SP time [days]") + theme(axis.title.y = element_blank()) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) + 
  scale_y_continuous(limits = c(0,365)) + scale_fill_rickandmorty(alpha = 0.7)
pl3 <- pm3sp %>% 
  filter(JournalCount > 15) %>%
  ggplot(aes(x = Journal, y = InReview, fill = Journal)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(position = "jitter", size = 0.1) +
  theme(legend.position = "None") + coord_flip() +
  ylab("SP time [days]") + theme(axis.title.y = element_blank()) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) + 
  scale_y_continuous(limits = c(0,365)) + scale_fill_rickandmorty(alpha = 0.7)
pl4 <- pm4sp %>% 
  filter(JournalCount > 15) %>%
  ggplot(aes(x = Journal, y = InReview, fill = Journal)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(position = "jitter", size = 0.1) +
  theme(legend.position = "None") + coord_flip() +
  ylab("SP time [days]") + theme(axis.title.y = element_blank()) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) + 
  scale_y_continuous(limits = c(0,365)) + scale_fill_rickandmorty(alpha = 0.7)
pl5 <- pm5sp %>% 
  filter(JournalCount > 15) %>%
  ggplot(aes(x = Journal, y = InReview, fill = Journal)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(position = "jitter", size = 0.1) +
  theme(legend.position = "None") + coord_flip() +
  ylab("SP time [days]") + theme(axis.title.y = element_blank()) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) + 
  scale_y_continuous(limits = c(0,365)) + scale_fill_rickandmorty(alpha = 0.7)

emf("3 sp3.emf", width = 5, height = 5)
print(pl3)
dev.off()
emf("3 sp4.emf", width = 5, height = 5)
print(pl4)
dev.off()
emf("3 sp5.emf", width = 5, height = 5)
print(pl5)
dev.off()

count <- pm1sp %>%
  group_by(Journal) %>%
  summarise(count = n())
summary_sp <- pm1sp %>%
  filter(InReview < 1) %>%
  group_by(Journal) %>%
  summarise(n = n()) %>%
  left_join(count, by = "Journal") %>% 
  mutate(perc = n / count * 100)
colnames(summary_sp) <-  c("Journal", "Peer Review < 24h", "Total Article count", "Percentage")

# AFFILIATION/AUTOR COUNT
AffiliationCount <- rbind(data.frame("AffiliationCount" = af3, "Time" = "COVID-19"),
                          data.frame("AffiliationCount" = af5, "Time" = "NOT COVID-19"),
                          data.frame("AffiliationCount" = af4, "Time" = "Y18/19"))
afp <- ggplot(AffiliationCount, aes(x = AffiliationCount, fill = Time, color = Time)) + 
  geom_density(alpha = 0.1, size = 0.8) + 
  scale_x_continuous(limits = c(NA, 40)) +
  theme(axis.title.y = element_blank(), legend.title = element_blank()) + 
  scale_fill_startrek() + 
  xlab("Affiliation Count Distribution")
SummaryAffiliationCount <- AffiliationCount %>%
  group_by(Time) %>%
  summarise(N = n(), median = median(AffiliationCount), iqr = IQR(AffiliationCount))
emf("Affditr COVID vs NOTCOVID.emf", width = 4, height = 3)
print(afp)
dev.off()


AffiliationCountA <- rbind(data.frame("AffiliationCount" = af3A, "Time" = "COVID-19"),
                           data.frame("AffiliationCount" = af4A, "Time" = "Y18/19"),
                           data.frame("AffiliationCount" = af5A, "Time" = "NOT COVID-19"))
afpA <- ggplot(AffiliationCountA, aes(x = AffiliationCount, fill = Time, color = Time)) + 
  geom_density(alpha = 0.1, size = 0.8) + 
  scale_x_continuous(limits = c(NA, 40)) +
  theme(axis.title.y = element_blank(), legend.title = element_blank()) + 
  scale_fill_startrek() + 
  xlab("Affiliation Count Distribution")
SummaryAffiliationCountA <- AffiliationCountA %>%
  group_by(Time) %>%
  summarise(N = n(), median = median(AffiliationCount), iqr = IQR(AffiliationCount))
emf("Affditr COVID vs NOTCOVID only ARTICLE JOURNAL.emf", width = 4, height = 3)
print(afpA)
dev.off()


AuthorCount <- rbind(data.frame("AuthorCount" = au3, "Time" = "COVID-19"),
                     data.frame("AuthorCount" = au4, "Time" = "Y18/19"),
                     data.frame("AuthorCount" = au5, "Time" = "NOT COVID-19"))
aup <- ggplot(AuthorCount, aes(x = AuthorCount, fill = Time, color = Time)) + 
  geom_density(alpha = 0.1, size = 0.8) + 
  scale_x_continuous(limits = c(NA, 40)) +
  theme(axis.title.y = element_blank(), legend.title = element_blank()) + 
  scale_fill_startrek() +
  xlab("Author Count Distribution")
SummaryAuthorCount <- AuthorCount %>%
  group_by(Time) %>%
  summarise(N = n(), median = median(AuthorCount), iqr = IQR(AuthorCount))
emf("AUT COVID vs NOTCOVID.emf", width = 4, height = 3)
print(aup)
dev.off()

AuthorCountA <- rbind(data.frame("AuthorCount" = au3A, "Time" = "COVID-19"),
                      data.frame("AuthorCount" = au4A, "Time" = "Y18/19"),
                     data.frame("AuthorCount" = au5A, "Time" = "NOT COVID-19"))
aupA <- ggplot(AuthorCountA, aes(x = AuthorCount, fill = Time, color = Time)) + 
  geom_density(alpha = 0.1, size =0.8) + 
  scale_x_continuous(limits = c(NA, 40)) +
  theme(axis.title.y = element_blank(), legend.title = element_blank()) + 
  scale_fill_startrek() +
  xlab("Author Count Distribution")
SummaryAuthorCountA <- AuthorCountA %>%
  group_by(Time) %>%
  summarise(N = n(), median = median(AuthorCount), iqr = IQR(AuthorCount))
emf("AUT COVID vs NOTCOVID only ARTICLE JOURNAL.emf", width = 4, height = 3)
print(aupA)
dev.off()


# PUBLICATION STATUS
pubstatus <- rbind(
  data.frame("PubStatus" = pm3$PublicationStatus, "Time" = "COVID-19"),
  data.frame("PubStatus" = pm5$PublicationStatus, "Time" = "NOT COVID-19")
)


pubstatus$PubStatus <- plyr::revalue(pubstatus$PubStatus, 
                                  c("aheadofprint"="Ahead of Print", "epublish"="E-publish", "ppublish" = "P-publish"))
pubstatusp <- ggplot(pubstatus, aes(x = Time, fill = PubStatus)) +geom_bar(position = "fill", width = 0.7) + 
  ylab("%") + theme(axis.title.x = element_blank()) + scale_fill_jco() + scale_y_continuous(labels = scales::percent) +
  theme(axis.title = element_blank(), legend.title = element_blank(), legend.position = "top", legend.direction = "horizontal")

emf("PubStatus.emf", width = 3.5, height = 3)
print(pubstatusp)
dev.off()


## bibliometrix package - country extraction
df1c <- metaTagExtraction(df1, Field = "AU1_CO", sep = ";")
df1c <- data.frame("AU1_CO" = df1c$AU1_CO) 
successfulnessCOExtract <- table(is.na(df1c))
df1c <- na.omit(df1c)
df1c$Count <- table(df1c$AU1_CO)[df1c$AU1_CO]  
df1c$AU1_CO <- factor(df1c$AU1_CO)

df1c %>%
  ggplot(aes(x = AU1_CO, fill = AU1_CO)) + geom_bar() + theme(legend.position = "Na") + coord_flip() +
  aes(x=reorder(AU1_CO,Count,sum))
 




## rXiv 

# load JSON, output data from python script
json <- fromJSON(file = "article_list.json")


dfrxiv <- data.frame("rxiv_date" = c(NA), "injournal" = c(NA), "rxiv_site" = c(NA), 
                 "J_received" = c(NA), "J_accepted" = c(NA), "J_published" = c(NA),
                 "journal" = c(NA), "doi" = c(NA))


for (i in json){
  if(is.null(i$journal_date_received)){
    NA
  } else {  i$journal_date_received }
  dfrxiv <- rbind(dfrxiv, data.frame("rxiv_date" =  i$rxiv_date, "injournal" = !is.null(i$journal_doi),
                             "rxiv_site" = i$rxiv_site, 
                             "J_received" =   if(is.null(i$journal_date_received)){NA} else { i$journal_date_received }, 
                             "J_accepted" = if(is.null(i$journal_date_accepted)){NA} else { i$journal_date_accepted }, 
                             "J_published" = if(is.null(i$journal_date_published)){NA} else { i$journal_date_published },
                             "journal" = if(is.null(i$journal)){NA} else { i$journal },
                             "doi" = if(is.null(i$journal_doi)){NA} else { i$journal_doi }))
}

dfrxiv<- dfrxiv[-1,]

dfrxiv$rxiv_date <- lubridate::ymd(dfrxiv$rxiv_date)

dfrxiv$injournal <- factor(dfrxiv$injournal)
dfrxiv$injournal <- plyr::revalue(dfrxiv$injournal, 
                             c("TRUE"="Published in Journal", "FALSE"="Not Published in Journal"))


pl7 <- ggplot(dfrxiv, aes(x = rxiv_date, fill = injournal)) + geom_bar() + 
  scale_x_date(limits = c(ymd("2020-01-10"), NA)) + scale_fill_startrek() +
  theme(legend.title = element_blank(), legend.position = "top", legend.direction = "horizontal") + xlab("Date") + ylab("Count")

emf("7 rXiv.emf", width = 3.5, height = 3.5)
print(pl7)
dev.off()




## Bibliometrix
# based on search results for search phrase: 
# importing bib file from scopus
# Search phrase on scopus ( ( "COVID-19"  OR  "COVID19"  OR  "COVID"  OR  "severe acute respiratory syndrome coronavirus 2"  OR  "2019-nCoV"  OR  "2019nCoV"  OR  "SARS-CoV-2"  OR  "SARS-CoV2"  OR  "SARS2"  OR  "coronavirus disease 2019"  OR  "coronavirus disease-19" ) ) 
# Accessed on 2020-04-11 15:45 CET




M <- convert2df("scopus20411.bib", dbsource = "scopus", format = "bibtex")

M <- M %>%
  filter(DT == "ARTICLE")

results_bib <- biblioAnalysis(M, sep = ";")

options(width=100)

S <- summary(object = results_bib, k = 20, pause = FALSE)


plot_list <- plot(x = results_bib, k = 10, pause = FALSE)

pl8 <- plot_list$MostProdCountries + scale_fill_startrek() + theme_bw() +
  theme(legend.title = element_blank(), plot.title = element_blank(),
         axis.title.y = element_blank(),
         plot.caption = element_blank(),
         plot.subtitle = element_text(vjust = 1),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.text = element_text(size = 8, colour = "black"),
         panel.background = element_rect(fill = NA)) +
  ylab("Published papers")

emf("8 Scopus_countryproductivity.emf", width = 3.5, height = 3.5)
print(pl8)
dev.off()




## used to create a map.
biblioshiny()


usporedba <- rbind(data.frame("DB" = "rXiv", "N" = dim(dfrxiv)[1]),
                   data.frame("DB" = "PubMed", "N" = dim(df1)[1]),
                   data.frame("DB" = "Scopus", "N" = dim(M)[1]))

usporedbap <- ggplot(usporedba, aes(x = DB, y = N, fill = DB)) + geom_col() + scale_fill_startrek() +
  ylab("Count") + theme(axis.title.x = element_blank(), legend.title = element_blank())


emf("baze.emf", width = 3.5, height = 3.5)
print(usporedbap)
dev.off()
