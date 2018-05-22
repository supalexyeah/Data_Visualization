#For final project 
library(tidyverse)
library(leaflet)
library(tidytext)
library(lubridate)
library(wordcloud)
library(igraph)
library(ggraph)
library(DT)
library(tm)
library(caret)
####Preparation: dataset processing####
FIR = read.csv("~/desktop/ProjData/FoodInspection_17R.csv")
FIR_Network <- FIR[,c('Inspection.ID','Risk','Results','Violations')]
#text data processing 
FIR_Network$Violations <- tolower(FIR_Network$Violations)
FIR_Network$Violations <- str_replace_all(FIR_Network$Violations, "[[:punct:]]", "")
FIR_Network$Violations <- str_replace_all(FIR_Network$Violations, "[[:digit:]]", "")
gsub(paste0(StopWordsCustom,collapse = "|"),"", FIR_Network$Violations)
FIR_Network$Violations<- gsub(paste0(StopWordsCustom,collapse = "|"),"", FIR_Network$Violations)

write.csv(FIR_Network, "~/desktop/ProjData/FIR_Network.csv")

#### Main Graph Making ######
fillColor = "#FFA07A"
fillColor2 = "#F1C40F"
FoodInspections = read_csv("~/desktop/ProjData/FIR_Network.csv")
FoodInspectionsReduced = FoodInspections %>%
  mutate(InspectionID = `Inspection.ID`) %>%
  select(InspectionID,Violations,Results)
FoodInspectionWords <- FoodInspectionsReduced %>%
  unnest_tokens(word, Violations) %>%
  filter(!word %in% stop_words$word) %>%
  count(Results, word, sort = TRUE) %>%
  ungroup()

#Term Frequency of Words (TF)
FoodInspectionResultsWords <- FoodInspectionWords
TotalWordsPerResult <- FoodInspectionResultsWords %>% 
  group_by(Results) %>% 
  summarize(total = sum(n))
FoodInspectionResultsWords <- left_join(FoodInspectionResultsWords, TotalWordsPerResult)
FoodInspectionResultsWords = FoodInspectionResultsWords %>% filter (!is.na(Results))
ggplot(FoodInspectionResultsWords, aes(n/total, fill = Results)) +
  geom_histogram(bins = 30, show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~Results, ncol = 2, scales = "free_y") + theme_bw()

#TF-IDF of Unigrams (One Word)
FoodInspectionWords_TF_IDF <- FoodInspectionWords  %>%
  bind_tf_idf(word, Results, n)
#Choose words with low IDF
LowIDF = FoodInspectionWords_TF_IDF %>%
  arrange((idf)) %>%
  select(word,idf)
#Get the Unique Words with LowIDF
UniqueLowIDF = unique(LowIDF$word)
plot_FoodInspectionWords_TF_IDF <- FoodInspectionWords_TF_IDF %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

#plot 1: Word cloud for UniGrams
plot_FoodInspectionWords_TF_IDF2 = plot_FoodInspectionWords_TF_IDF %>% top_n(100)
plot_FoodInspectionWords_TF_IDF2 %>%
  with(wordcloud(word, tf_idf, max.words = 50,colors=brewer.pal(8, "Dark2")))

#TF-IDF Bigrams
FoodInspectionWordsBiGram <- FoodInspectionsReduced %>%
  unnest_tokens(bigram, Violations, token = "ngrams", n = 2)
bigrams_separated <- FoodInspectionWordsBiGram %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigram_tf_idf <- bigrams_united %>%
  count(Results, bigram) %>%
  bind_tf_idf(bigram, Results, n) 
plot_FoodInspectionWords_TF_IDF <- bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram))))

#plot 2: Force-directed graph for relationship among words
count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, Violations, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}
visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    labs(title = "Relationship Between Keywords of Restaurant Violations")+
    theme_void()
}
visualize_bigrams_individual <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a,end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}
FoodInspectionsReduced_Bigrams <- FoodInspectionsReduced %>%
  count_bigrams()
FoodInspectionsReduced_Bigrams %>%
  filter(n > 1500) %>%
  visualize_bigrams()

####Other Exploratory Graphs####
#Plot 3: Bar graph for Top 20 bad-performing restaurants(with high risk)
cData%>%
  select(DBA.Name,Risk,Latitude,Longitude)%>%
  group_by(DBA.Name,Risk,Latitude,Longitude)%>%
  summarise(Tot=n())->res_by_high_risk

res_by_high_risk<-arrange(res_by_high_risk,-Tot)
write.csv(res_by_high_risk,'~/desktop/rbhr.csv',sep=",", row.names=FALSE)
# use tableau to graph 

#Plot 4:choropleth for discribtion of high risk restaurants by zip code
# use tableau to graph 

#plot 5: line graph for time series about inspection frequency
hchart(tseries, name = "test") %>%
  hc_add_theme(hc_theme_538()) %>%
  hc_credits(enabled = TRUE, style = list(fontSize = "13px")) %>%
  hc_title(text = "line graph for time series about inspection frequency in 2015-2017") %>%
  hc_legend(enabled = TRUE)

#Plot 6:leaflet map of food place broken down by results(pass and fail)
ResultsPassORFail = c("Pass","Fail")
factpal <- colorFactor(c("gray","red","purple","yellow","orange","green","blue"), 
                       FoodInspections$Results)
FoodInspectionsSubSet = FoodInspections %>%
  sample_n(8e3) %>%
  filter(Results %in%  ResultsPassORFail) 
leaflet(FoodInspectionsSubSet) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircles(lng = ~Longitude, lat = ~Latitude,radius = 1, 
             color = ~factpal(Results))  %>%
  addLegend("bottomright", pal = factpal, values = ~Results,
            title = "Locations of Food Places in Chicago",
            opacity = 1)