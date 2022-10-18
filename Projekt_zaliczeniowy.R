#-------------------------------------------------------#
#                     Text Mining                       #
#                        CDV                            #
#                Projekt zaliczeniowy                   #
#                  Marek Miłosierny                     #
#-------------------------------------------------------#

library(tidyverse)
library(tidytext)
library(wordcloud)
library(stringr)
library(lubridate)
library(tm)
library(hunspell)
library(ggplot2)

load("Projekt_zal_dane.RData")

# Pobranie numerów działów i numerów artykułów jako id
all_articles <- articles %>% 
  mutate(dzial = str_sub(url, 43, 48)) %>% 
  mutate(id = str_sub(url, 50, 57))

# Przekształcenie daty na osobne kolumny
all_articles <- all_articles %>% 
  mutate(day = day(date),
         month = month(date),
         year = year(date))

# Timeseries dla ogółu danych
all_articles %>% 
  count(year, month) %>% 
  ggplot() +
  geom_col(aes(make_date(year, month, 1), n)) +
  xlab("Data") +
  ylab("Ilość artykułów") +
  scale_x_date(date_breaks = "1 months", date_labels = "%m.%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Wypisanie nr działów
unique(all_articles$dzial)

# Przypisanie działom nazw
all_articles <- all_articles %>% 
  mutate(dzial = case_when(.$dzial == "114881" ~ "Świat",
                           .$dzial == "114884" ~ "Polityka",
                           .$dzial == "114883" ~ "Polska",
                           .$dzial == "173952" ~ "Koronawirus",
                           .$dzial == "157710" ~ "Lotto",
                           .$dzial == "127561" ~ "Deutsche Welle",
                           .$dzial == "114871" ~ "Najnowsze",
                           .$dzial == "156046" ~ "Edukacja",
                           .$dzial == "143907" ~ "Wybory",
                           .$dzial == "166611" ~ "Wiadomości dnia",
  ))


# Liczba artykułów dla działu w miesiącu

all_articles %>% 
  count(dzial, year, month) %>% 
  mutate(date = make_date(year, month)) %>% 
  ggplot() +
  geom_col(aes(date, n, fill = dzial), position = position_stack(reverse = TRUE)) +
  scale_x_date(date_breaks = "1 months", date_labels = "%m.%Y") +
  labs(fill = "Działy") +
  xlab("Data") +
  ylab("Ilość artykułów") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        legend.position = "top")

# Wybranie badanego okresu i działu

articles <- all_articles %>% 
  filter(date >= "2019-06-01", date < "2019-07-01") %>% 
  filter(dzial == "Świat")

# Liczba artykułów w wybranym dziale w miesiącu

articles %>%
  count(dzial, year, month) %>% 
  mutate(date = make_date(year, month, 1)) %>% 
  ggplot() +
  geom_col(aes(date, n, fill = dzial), position = position_stack(reverse = TRUE)) +
  scale_x_date(date_breaks = "1 months", date_labels = "%m.%Y") +
  labs(fill = "Dział") +
  xlab("Data") +
  ylab("Ilość artykułów") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), legend.position = "bottom")

# Lematyzatyzacja

pl_stopwords <- read_lines("stop_words_polish.txt")

lead_words <- articles %>% 
  unnest_tokens(word, lead, token = "words") %>%  
  filter(nchar(word) > 3) %>% 
  filter(!word %in% pl_stopwords)

lead_words <- lead_words %>%
  mutate(stem_word = hunspell_stem(word, dict = dictionary('pl_PL')))
lead_words$stem_word <- unlist(lapply(lead_words$stem_word, function(x) x[1]))

# Top 30 słów dla lead

lead_words %>% 
  filter(!is.na(stem_word)) %>% 
  count(stem_word) %>% 
  top_n(30, n) %>% 
  ggplot() +
  geom_bar(aes(reorder(stem_word, -n), n), stat = "identity", color='skyblue',fill='steelblue')+
  xlab("Słowa") +
  ylab("Ilość słów") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Wordcloud

lead_words_count <- lead_words %>% 
  count(stem_word)

wordcloud(lead_words_count$stem_word, lead_words_count$n, max.words = 100, scale = c(50, 0.5),
          colors=brewer.pal(8, "Dark2"))

# Pojedycze słowa dla body

body_words <- articles %>% 
  unnest_tokens(word, body, token = "words") %>% 
  filter(nchar(word) > 3) %>%
  filter(!word %in% pl_stopwords)

body_words <- body_words %>% 
  select(id, word)

# Usunięcie znaków powodujących zawieszanie się funkcji hunspell_stem

body_words$word <- gsub("’", "", body_words$word)
body_words$word <- gsub("ø", "o", body_words$word)


body_words <- body_words %>%
  mutate(stem_word = hunspell_stem(word, dict = dictionary('pl_PL')))

body_words$stem_word <- unlist(lapply(body_words$stem_word, function(x) x[1]))

# Filtrowanie body ze zbędnych słów

body_words <- body_words %>% 
  filter(!is.na(stem_word)) %>%
  filter(!stem_word %in% c("null", "slot", "headline", "http", "image", "logo", "logos", "persona", "reklama", "wideo"))

# Top 30 słów dla body

body_words %>% 
  count(stem_word) %>% 
  top_n(30, n) %>% 
  ggplot() +
  geom_bar(aes(reorder(stem_word, -n), n), stat = "identity", color='skyblue',fill='steelblue') +
  xlab("Słowa") +
  ylab("Ilość słów") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Wordcloud

body_word_count <- body_words %>% 
  count(stem_word)

wordcloud(body_word_count$stem_word, body_word_count$n, max.words = 100, scale = c(4, 0.5),
          colors=brewer.pal(8, "Dark2"))

# Analiza sentymentu

sent_dict <- read.csv('slownikWydzwieku01.csv', sep = '\t', encoding = 'UTF-8', header = F)

names(sent_dict) <- c('slowo', 'V2' ,'czy_emocja', 'sent_bin', 'sent_dysk', 'sent_so_pmi') 

word_sentiment <- inner_join(body_words, sent_dict, by = c('stem_word' = 'slowo'))


word_sentiment <- word_sentiment %>% 
  group_by(id) %>% 
  summarise(sent_bin = sum(sent_bin),
            sent_dysk = sum(sent_dysk),
            sent_so_pmi = sum(sent_so_pmi)) # %>%

ggplot(data = word_sentiment, aes(x = id, y = sent_bin)) +
  geom_point() +
  xlab("Id artykułu") +
  ylab("Sentyment") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

ggplot(data = word_sentiment, aes(x = id, y = sent_dysk)) +
  geom_point() +
  xlab("Id artykułu") +
  ylab("Sentyment") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

ggplot(data = word_sentiment, aes(x = id, y = sent_so_pmi)) +
  geom_point() +
  xlab("Id artykułu") +
  ylab("Sentyment SO-PMI") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))
