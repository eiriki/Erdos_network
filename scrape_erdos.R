#This script will scrape the data from the given set and store it
library(rvest)
library(stringr)
library(reader)

#before we calculate influence, we need to collect information on Erdos' coauthors
#start to scrape the Erdos1 data set: This is the link they provided 
link = "https://files.oakland.edu/users/grossman/enp/Erdos1.html"
doc <- read_html(link)

long_text <- doc %>% html_text()
#save the text object
write(long_text, './data/Erdos1.txt')

text <- reader('./data/Erdos1.txt')
text[1:30]
#we can take out the first 24 header lines
textVect <- text[25:length(text)]
textVect

#make a function that will loop through the text and fill a 511x511 matrix 
#we import project_data.xlsx just for the author name vector
data <- read_xlsx('./data/project data.xlsx',col_names = F)
colnames(data) <- c("Name","Year","Count")
frame <- matrix(data= 0, nrow = 511, ncol = 511)
colnames(frame) <- data$Name
rownames(frame) <- data$Name

#lazy loop- functionalize later
i = 1
while(i < length(textVect)){
  #get the author and clean it up
  rowIDX = textVect[i]
  rowIDX <- str_extract(rowIDX,'.*?(?=\\s\\s)')
  rowIDX <- str_trim(rowIDX)
  author = "hello" # just initializing 
  j = i
  while(author != ""){
    author = textVect[j+1]
    author = str_remove(author, '\t')
    if(author %in% colnames(frame)){
      frame[rowIDX,author] = 1
      frame[author,rowIDX] = 1
      print(author)
    }
    j= j +1
  }
  i = j +1
}
#save the resulting coauthor matrix
saveRDS(frame, './data/coauthor.RDS')



