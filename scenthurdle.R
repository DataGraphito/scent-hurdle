#Load Packages
#R version 3.6.3 
library(Rcrawler) #0.1.9-1
library(tidyverse) #1.3.0
library(rvest) #0.3.6
library(tidytext) #0.3.1
library(proustr) #0.4.0


###### Web Crawling
#Scrape scenthurdle.com and save html pages


# Prep - set WD to specific folder
setwd(paste0(getwd(), "/Documents/ScentHurdle"))


# Start webcrawling at a blog post because the homepage does not have links
Rcrawler(Website = "http://scenthurdle.com/bruno-fazzolari-unsettled-2017/", 
         DIR = paste0(getwd()),
         ignoreAllUrlParams = TRUE)


# Prep returned dataframe to create new file names
INDEX <- INDEX %>% 
  # Create column with duplicated posts, but with different URL structures
  mutate(Url2 = str_replace(Url, "http://www.", "http://")) %>% 
  # Create column with blog titles from cleaned page name
  mutate(fileName = str_sub(Url2, 24, -1)) %>% 
  mutate(fileName = str_replace_all(fileName, regex("/$"), ""))%>% 
  mutate(fileName = str_replace_all(fileName, "/", "--"))


# Get list of files from the folder Rcrawler created -
# the file names are numeric, which aren't very informative
oldFiles <- list.files(path = paste0(getwd(), "/scenthurdle.com-092159/"),
                       full.names = TRUE)


# Create new directory for new files
dir.create(paste0(getwd(), "/newFiles"))


# Create function to copy html pages to a new location 
# with the page name as the file name
replaceFileNames <- function(x){
  
  fileLookup <- str_extract(x, "[//][0-9]+") %>% 
    str_remove("/") 
  
  fileReplacement <- INDEX %>% 
    filter(Id == fileLookup) %>% 
    pull(fileName)
  
  file.copy(from = x,
            to = paste0(getwd(), "/newFiles/", fileReplacement, ".html"), 
            overwrite = FALSE)
}


# Apply replaceFileNames function to webpages to copy posts with more informative names
oldFiles %>% 
  map(replaceFileNames)




##### Data Cleaning


# Get hmtl files with new names
webVersions <- list.files(path = paste0(getwd(), "/newFiles"),
                          full.names = TRUE)


# Remove pages of only blog tags
webVersions <- webVersions %>%
  discard(~ str_detect(.x, "category--")) %>%
  discard(~ str_detect(.x, "author--"))


# Create function to get relevant information from text posts
webToText <- function(x) {
  
  # Get post url
  pAddress <- read_html(x) %>% 
    html_nodes(xpath = "//link[@rel='canonical']") %>% 
    html_attr("href")
  
  # Get post title
  pTitle <- read_html(x) %>% 
    html_node("h1.entry-title") %>% 
    html_text() %>% 
    str_remove_all("[/]")
  
  # Get post text
  pText <- read_html(x) %>% 
    html_node("div.entry-content") %>% 
    html_text() 
  
  # Get post tags
  pTags <- read_html(x) %>% 
    html_node("div.cat-links") %>% 
    html_children() %>% 
    html_attr("href") %>% 
    str_remove(pattern = regex("^.*/category/")) %>% 
    str_replace(pattern = regex("/$"), "") %>% 
    as_tibble() %>% 
    separate(value, into = c("category", "value"), sep = "/") %>% 
    group_by(category) %>% 
    mutate(category = paste0(category, row_number())) %>% 
    pivot_wider(names_from = "category", names_repair = "unique")
  
  # Combine data into dataframe
  DF <- tibble(postUrl = pAddress,
               postTitle = pTitle,
               postText = pText) %>% 
    bind_cols(pTags) %>% 
    select(-matches("value"))
  
  return(DF)
} 


# Apply webToText function to webpages and save as dataframe
postsDF1 <- map_dfr(webVersions, webToText)


# Remove duplicates and whitespace
postsDF1 <- postsDF1 %>% 
  distinct() %>% 
  mutate(postText = str_replace_all(postText, "\\n", " ")) %>% 
  mutate(postText = str_remove_all(postText, "\\t")) %>% 
  mutate(postText = str_trim(postText)) %>% 
  mutate_at(vars(starts_with("year")), as.double) #630 posts


# Fix tags for years 2013 and 2015 and add decades
postsDF1 <- postsDF1  %>% 
  mutate_at(vars(starts_with("year")), 
            ~ case_when(.x == 2103 ~ 2013,
                        .x == 2105 ~ 2015,
                        TRUE ~ .x)) %>% 
  mutate(decade = plyr::round_any(year1, 10, f = floor)) %>% 
  select(postUrl, postTitle, postText, decade,
         starts_with("year"), starts_with("brand"), starts_with("perfumer"))


##### Vintage Collecting
# Standard theme for ggplot graphs
graphTheme <- theme_minimal() +
  theme(plot.title = element_text(color = "#595959"),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#F4F7F6", color = "#F4F7F6"),
        panel.background = element_rect(fill = "#FCFCFC", color = "#595959"))


# Make plot of post frequency
ggplot(postsDF1, aes(decade)) + 
  geom_bar(fill = "#72AD2D") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1,
            color = "#595959") +
  scale_x_continuous(breaks = seq(from = min(postsDF1$decade, na.rm = TRUE), 
                                  to = max(postsDF1$decade, na.rm = TRUE), 
                                  by = 10)) +
  ylim(0, 200) + 
  labs(title = "Post Frequency by Perfume Launch Decade") +
  graphTheme


# Create DF to separate vintage vs non-vintage
wordCountDF <- postsDF1 %>% 
  filter(!is.na(decade)) %>% 
  select(postTitle, decade, postText) %>% 
  unnest_tokens(word, postText) %>% 
  filter(!word %in% tidytext::stop_words$word) %>% 
  mutate(isVintage = ifelse(decade < 2000,
                            "Vintage", 
                            "Not Vintage")) %>% 
  group_by(postTitle, isVintage) %>% 
  count()


# Are the post lengths statistically different?
wilcox.test(n ~ isVintage, data = wordCountDF) 
#p-value = 0.01462


# Make plot of vintage vs non-vintage densities
ggplot(wordCountDF, aes(n, group = isVintage, fill = isVintage, color = isVintage)) +
  geom_density(alpha = 0.2) +
  # Vintage is yellow
  scale_fill_manual(values = c("#9C1C2D", "#FABC2A")) +
  scale_color_manual(values = c("#9C1C2D", "#FABC2A")) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Word Count per Post") +
  graphTheme +
  theme(legend.position = "none")


# Get basic stats
wordCountDF %>% 
  group_by(isVintage) %>% 
  summarise(medWordCount = median(n),
            meanWordCount = mean(n))




##### Scent Preferences
# Create vector of 10 common perfume notes
perfumeNotes <- tibble(Note = c("iris", "rose", "chypre", "lily", 
                                     "vanilla","musk", "jasmine", "galbanum", 
                                     "narcissus", "leather"),
                       searchTerms = c("iris", "rose", "(chypre|oakmoss)", "(lily|muguet|lilies)", 
                                       "vanill","(musk|musc)", "jasmin", "galbanum", 
                                       "narcissus", "leather"))


# Create function to count mentions of a word
countFunction <- compose(~ sum(.x), 
                         ~str_count(postsDF1$postText, 
                                    pattern = regex(paste0("\\s", .x), 
                                                    ignore_case = TRUE)))


# Note Mentions to CSV
noteCountDF <- perfumeNotes %>% 
  mutate(`Note Mentions` = map_dbl(searchTerms, countFunction)) %>% 
  select(-searchTerms)


noteCountDF %>% 
  mutate(`Note Mentions / Total Posts` = `Note Mentions` / nrow(postsDF1)) %>% 
  mutate(Note = str_to_title(Note)) %>% 
  arrange(-`Note Mentions`) %>% 
  mutate_if(is.double, scales::number, accuracy = 0.001) %>% 
  write.csv(file = "totalNoteFrequency.csv",
            row.names = FALSE)


# Note Term Freq - Inverse Document Freq to CSV
postsDF1 %>% 
  select(postTitle, postText) %>% 
  unnest_tokens(word, postText) %>% 
  filter(!word %in% tidytext::stop_words$word) %>% 
  filter(!word %in% proustr::stop_words$word) %>% 
  mutate(word = ifelse(word %in% c("lily", "muguet", "lilies"),
                       "lily",
                       word),
         word = ifelse(word %in% c("jasmin", "jasmines"),
                       "jasmine",
                       word),
         word = ifelse(word %in% c("cuir", "leathers", "cuirs", "leathery"),
                       "leather",
                       word),
         word = ifelse(word %in% c("musc", "musks", "musky"),
                       "musk",
                       word),
         word = ifelse(word %in% c("chypres", "oakmoss"),
                       "chypre",
                       word),
         word = ifelse(word == "roses",
                       "rose",
                       word)) %>% 
  count(postTitle, word) %>% 
  bind_tf_idf(word, postTitle, n) %>% 
  filter(word %in% perfumeNotes$Note) %>% 
  arrange(-tf_idf) %>% 
  filter(row_number() < 11) %>% 
  mutate(word = str_to_title(word)) %>% 
  mutate_if(is.double, scales::number, accuracy = 0.001) %>% 
  rename(`Post Title` = postTitle,
         Note = word,
         Count = n,
         `Term Frequency` = tf,
         `Inverse Document Frequency` = idf,
         `TF-IDF` = tf_idf) %>% 
  write.csv(file = "noteTfIdF.csv",
            row.names = FALSE)




##### Chanel
# (I made the Chanel graphic in Google Sheets)
# Chanel Post Counts
postsDF1 %>% 
  filter(str_detect(postText, "Chanel")) %>%
  select(postUrl, postTitle) %>% 
  nrow() #95


# N.5
postsDF1 %>% 
  filter(str_detect(postText, regex("chanel\\s+[no0]*.{0,2}5", ignore_case = TRUE))) %>%
  select(postUrl, postTitle) %>% 
  nrow() #19


# N.19
postsDF1 %>% 
  filter(str_detect(postText, regex("chanel\\s+[no0]*.{0,2}19", ignore_case = TRUE))) %>%
  select(postUrl, postTitle) %>% 
  nrow() #21




# Chanel Term Counts
str_count(postsDF1$postText, pattern = regex("chanel", ignore_case = TRUE)) %>% 
  sum() #165


# N.5
str_count(postsDF1$postText, pattern = regex("chanel\\s+[no0]*.{0,2}5", ignore_case = TRUE)) %>% 
  sum() #33
# N.19
str_count(postsDF1$postText, pattern = regex("chanel\\s+[no0]*.{0,2}19", ignore_case = TRUE)) %>% 
  sum() # 21






#
