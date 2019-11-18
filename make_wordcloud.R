rm(list=ls())
setwd("")   #set path
library(readtext)
library(tidyverse)
library(ggwordcloud)
library(textclean)
library(tm)

stub = "official"   #BBC translations (MVs only)
#stub = "extra"   #personal translations (full discography)

words_to_drop = c(stopwords("en"),
                "like",
                "will",
                "oh",
                "even",
                "get",
                "can",
                "gets", 
                "take",
                "let")  #edit this list as needed

cutoff = 5  #minimum number of times a word has to be used to appear in plot

# Read and clean lyrics ----------------------------------------------------------

## Some simple text cleaning
Cleanup = function(str) {
  cleanstr = replace_contraction(str) %>%
    tolower() %>%
    removeWords(words_to_drop) %>%
    str_replace_all("\n", " ") %>%
    str_replace_all("[[:punct:]]", " ")
}

files = list.files(pattern = paste0(stub, ".txt"))  #grab all the .txt files of the right type (official or extra)
lyrictable = readtext(files) %>%
  mutate(doc_id = str_remove(doc_id, paste0("_", stub, ".txt"))) %>%
  rename(group = doc_id) %>%
  mutate(cleantext = Cleanup(text)) %>%
  select(-text)

## Create a table of unigrams for each member/unit

SplitWords = function(x) {
  unigrams = strsplit(lyrictable[[x,2]], " +")[[1]]
  unigrams_tibble = 
    enframe(unigrams) %>%
    select(value) %>%
    rename(word = value) %>%
    mutate(group = lyrictable[[x,1]])
}

wordslist = lapply(1:16, SplitWords)


## Append to one table, count and drop duplicates

wordtable = bind_rows(wordslist)
plottable = wordtable %>%
  filter(word != "") %>%
  group_by(group) %>%
  mutate(wordcount_group = n()) %>%   #number of words by each member/unit
  ungroup() %>%
  group_by(word, group) %>%
  mutate(frequency_withingroup = n()) %>%   #word frequency for each member/unit
  ungroup() %>%
  mutate(freq_normalized = frequency_withingroup / wordcount_group) %>%
  group_by(word) %>%
  mutate(frequency = n()) %>%   #frequency across everyone
  arrange(desc(frequency), desc(freq_normalized)) %>%
  mutate(assignTo = group[which.max(freq_normalized)]) %>%    #who used this word the most
  ungroup() %>%
  select(word, frequency, assignTo) %>%
  distinct() %>%  #drop duplicates
  mutate(assignTo = str_replace(assignTo, "1_3", "1/3"))


# Draw the wordcloud ----------------------------------------------------------

set.seed(12)

colors = c("heejin" = "deeppink",
           "hyunjin" = "yellow2",
           "haseul" = "green3",
           "yeojin" = "orangered",
           "vivi" = "lightpink",
           "kimlip" = "red2",
           "jinsoul" = "blue", 
           "choerry" = "purple", 
           "yves" = "darkred",
           "chuu" = "sandybrown",
           "gowon" = "lightgreen",
           "olivia" = "gray50",
           "1/3" = "plum",
           "oec" = "lightblue",
           "yyxy" = "yellowgreen",
           "loona" = "black")

wordcloud = plottable %>%
  filter(frequency >= cutoff) %>%
  mutate(angle = 45 * sample(-1:1, n(), replace = TRUE, prob = c(1, 3, 1))) %>%
  ggplot(aes(label = word, size = frequency, color = assignTo, angle = angle)) +
  geom_text_wordcloud(area_corr = FALSE, show.legend = TRUE) +
  scale_color_manual(values = colors) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text  = element_text(size = 5),
        legend.key.size = unit(0.5, "lines"),
        legend.key = element_blank(),
        legend.box.margin = margin(-10, -10, -10, -10),
        panel.background = element_blank()) +
  guides(size = FALSE, color = guide_legend(nrow = 2, override.aes = list(size=3)))
  
ggsave(paste0("wordcloud_", stub, ".png"), wordcloud, width = 5.2, height = 3.1, units = c("in"))

