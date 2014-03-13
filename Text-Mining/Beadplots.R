# Text Mining Gun Deaths

In this post I will explore public data being collected by <a href="http://www.slate.com/articles/news_and_politics/crime/2012/12/gun_death_tally_every_american_gun_death_since_newtown_sandy_hook_shooting.html">Slate</a>.

This data began collection as a response to the shooting at Sandy Hook Elementary in December of 2012 in an attempt to create a database of all of the deaths as a result of guns in the US.  Since its creation, the database has expanded to a list of over 12,000 deaths in a little over a year.

In this post I will explore a little of that database as well scrape the web for additional data from the articles listed on the database.

# Let's load the raw data:
gun.deaths <- read.csv("http://slate-interactives-prod.elasticbeanstalk.com/gun-deaths/getCSV.php")

tail(gun.deaths)
summary(gun.deaths)

# We can see that the vast majority of gun deaths are among men with 10,153 
# and only 1,850 among women.  The mean age is 33.34 while the median age is
# a little lower are 30.  Interestingly the maximum age is 107. What 
# information is not provided though would be interesting would be cause of
# death such murder, suicide, accident, etc.

library(XML)

# Read and parse HTML file
gun.deaths.text <- list()

html.last <- ""

# The following code will grab all 12k+ article text.  I have a somewhat slow internet

# This will allow you to gather some of the text and come back to it at a future point
# if you decide not to wait for it to ping all 12k websites.

for (i in (length(gun.deaths.text)+1):nrow(gun.deaths)) {
  print(paste("reading html #", i))
  
  # The following code I borrow from a post on Quantum Forest.
  # It grabs the text between paragrahs from HTML documents.
  # http://www.quantumforest.com/2011/10/reading-html-pages-in-r-for-text-processing/
  try(doc.html <- htmlTreeParse(gun.deaths$url[i],
                           useInternal = TRUE))
    # I have added a few 'try' error handling functions so that the web scraping
    # loop does not stop when there is a missing URL.
  if (is.null(doc.html)) doc.html <- html.last
  doc.text = unlist(try(xpathApply(doc.html, '//p', xmlValue)))
  doc.text = gsub('\\n', ' ', doc.text)
  doc.text = paste(doc.text, collapse = ' ')
  
  if (identical(html.last, doc.html)) doc.text <- "ERROR Source DROPPED"
    
  gun.deaths.text[i] <- list(doc.text)
  
  # Save the last html read as the current so that dropped documents are not counted twice.
  html.last <- doc.html
}

length(gun.deaths.text)
# for the following results I only collect the first ~3000 results

# I suggest saving the data after you have downloaded it all.
save(gun.deaths.text, file="gun.deaths.text.Rdata")
load("gun.deaths.text.Rdata")

# We will use the text mining library
library(tm)

# We first turn our list of articles into a corpus
gun.deaths.corpus <- Corpus(VectorSource(gun.deaths.text))
# Then we lowercase the words in that list.
gun.deaths.corpus <- tm_map(gun.deaths.corpus, tolower)

# This will create a matrix that lists all of the words
# and how frequently they appear in each article.
# It can be very long.
dtm <- DocumentTermMatrix(gun.deaths.corpus)

freqTerms <- findFreqTerms(dtm, 550)

dtmDic <- as.data.frame(inspect(DocumentTermMatrix(gun.deaths.corpus,
  list(dictionary = sort(c("suspect",  "suspects", "gunman", 
                      "fatally", "slaying","witnesses", 
                      "victim" , "victims", "homicide",  
                      "drug",   "crime", "accidentally",
                      "multiple", "suicide",
                      "accidental", "killed","children",
                      "student", "teacher", "charged",
                      "arrested"))))))
ndict <- ncol(dtmDic)
nobs  <- nrow(dtmDic)

# Let's drop the information about frequency of word use and just ask whether
# different words were used.
bimydf <- as.data.frame(dtmDic*0+1*(dtmDic>0))

# Let's see some word frequencies plotted

# First we want to count probability of word use for each article
perc <- apply(bimydf,2,mean)

# I will now create my first bead plot to be saved to the hard drive
png("2013-03-13GunDeaths1.png", width = 650, height = 400)

# Adjust the margins
par(mar=c(5,2,3,1))
  
  # Plot the beads
  plot(perc, xaxt = "n", xlab="", ylab="%", cex=1.5,
       main="Percent of Articles Using Word",pch=19)
  
  # Add guide lines
  for (i in 1:ndict) abline(v=i, col=gray(.9-.1*(i %% 4)), lwd=1)
  
  # Add text to identify each bead
  text(cex=1, x=1:length(perc)+.5, y=-.015, names(perc), 
       xpd=TRUE, srt=60, pos=2)

dev.off()

# This is interesting.  Homocide and crime are common while accidently and suicide
# are quite low.

# Let's creating a valiable that is 1 to count how frequently a word such as homocide,
# gunman, victim, etc appears.
violent <- bimydf$homicide+bimydf$gunman+bimydf$victim+
  bimydf$victims+bimydf$victims+bimydf$crime+bimydf$suspect+
  bimydf$suspects+bimydf$slaying

# The average number of references to any of the above terms per article
# is 2.
summary(violent[doc.text != "ERROR Source DROPPED"])

violent.bi <- as.numeric(violent>0)
summary(violent.bi[doc.text != "ERROR Source DROPPED"])

# 58% of the articles seem to have some reference to violence or crime

with(bimydf,
     cor(data.frame(violent.bi, 
               suicide, 
               accidental, 
               accidentally, 
               multiple,
               children,
               drug)))

# Looking at a correlation matrix we find more results.  Our violent crime 
# variable is negatively correlated with suicide, accidental, and accidently while
# strongly correlated with multiple, children, and drug.

# Next I will plot out our data over time with each death being mapped.
library("RColorBrewer")

# I use color brewer to mix a brew of colors with blues representing the youngest
# aged victims and darkred representing the oldest victims.
collist <- 
  colorRampPalette(c("blue", "darkred")) (50)

# Prepare to save as png
png("2013-03-13GunDeaths2.png", width = 650, height = 1200)

  # I adjust the margins.
  par(mar=c(5,1,3,1))
  
  # Open a plot window
  plot(0,0, xlim=c(.75,ndict-.5), ylim=c(.03,.97), 
       type="n", xaxt = "n", xlab="", 
       yaxt = "n", ylab="Account",
       main="Wordcount Beadplot-US Gun Deaths Articles")
  
  # Name the columns
  text(cex=1, x=1:ndict, y=-.015, colnames(dtmDic), 
       xpd=TRUE, srt=60, pos=2)
  
  # Insert column guides
  for (i in 1:ndict) abline(v=i-.25, col=gray(.9-.1*(i %% 4)), lwd=4)
  
  # Insert a small horizontal line for each word associated with each article
  for (i in 1:ndict) for (ii in 1:nobs)
    if (dtmDic[ii,i]>0) lines(c(i-.75,i+.15),c(ii/nobs,ii/nobs),
                    col=collist[min(gun.deaths$age[ii],50)], lwd=.5)
dev.off()
