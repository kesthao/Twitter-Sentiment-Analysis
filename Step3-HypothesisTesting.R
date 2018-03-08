# Import required libraries
library(ggplot2)
library(wordcloud)
library(tm)
library(RCurl)
library(RColorBrewer)
library(stringi)
library(ggthemes)

# Read tweet.csv into R
tweet.df <- read.csv('tweet_df.csv')

# Subset observations (users) which have been classified as "Dem" or "Rep"
sentiment.df <- subset(tweet.df, Political_Affiliation != "", 
                           select = c(Political_Affiliation, Text_Label))

########################
#--HYPTHESIS TESTING---#
########################
# Two-Sample Test for Difference of Proportions
dem_neg <- length(which(sentiment.df$Political_Affiliation == "Dem" & 
                          sentiment.df$Text_Label == "neg"))
dem <- length(which(sentiment.df$Political_Affiliation == "Dem"))
rep_neg <-length(which(sentiment.df$Political_Affiliation == "Rep" & 
                         sentiment.df$Text_Label == "neg"))
rep <- length(which(sentiment.df$Political_Affiliation == "Rep"))
# Create a DataFrame to contain groups and counts
prop.df <- data.frame(Groups = c("Democrats", "Republicans", "Dem Neg", "Rep Neg"),
                      Proportion = c(dem, rep, dem_neg, rep_neg))
# 2-Sample Z-test
prop_test <- prop.test(x = c(dem_neg, rep_neg), n = c(dem, rep), alternative = "greater")

########################
#--------BAR PLOT------#
########################
# Create a bar plot
ggplot(prop.df, aes(x = Groups, y = Proportion)) + 
  geom_bar(colour = "black", fill = c("coral", "violet", "light blue", "goldenrod"), 
           width=.35, stat="identity", position="dodge") +   
  ylab("Counts") + ggtitle("Group Counts")  + theme_wsj()

########################
#------WORD CLOUD------#
########################
#Create a word cloud of the most common words associated with 'tax'

# Encode to ASCII
text.df <- data.frame(tweet.df$Text_Words)
text2.df <- apply(text.df, 2, stri_enc_toascii)

pal <- brewer.pal(9, "Set1")

# Create word cloud
wordcloud(text2.df, random.order = F, max.words = 50, scale = c(3.5, .5),
          colors = pal)
