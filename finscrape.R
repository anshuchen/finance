# By Anshu Chen (a.chen@yale.edu)
# Requires installing "pdftools" and "stringr"
# Last modified August 1, 2016
# Scrapes HBS finance case PDF and extracts case title, author, description, product#, discipline, and format

# I first merged all the folder pdfs together into "merged.pdf" using an online
# pdf merger.

library(pdftools)
# Converts pdf into character vector, where each component is a page
txt <- pdf_text("C:/Users/Anshu/Box Sync/summer 2016/Fed summer 2016/finance/merged.pdf")

# Combines the page components into one long string
x <- cbind(txt)
library(stringr)
y <- str_c(x, collapse = "")

# Splits file into individual cases
z <- unlist(strsplit(y, "CASE\\s"))
# First case only contains filler, so remove it
z <- z[-1]

# Cleans extraneous elements
z <- gsub("\r\n", "", z, fixed=TRUE)
z <- gsub("[0-9]{1,2}\\s{1}of\\s{1}[0-9]{2}\\s+7/26/2016 [0-9]:[0-9]{2} AM\\s*Folder\\s+https://cb.hbsp.harvard.edu/cbmp/context/folders/51214014", "", z, perl=TRUE)
# Drops unnecessary observations (supplements, module notes, course overviews)
z <- gsub("SUPPLEMENT[^S].*$", "", z, perl=TRUE)
z <- gsub("MODULE NOTE.*$", "", z, perl=TRUE)
z <- gsub("COURSE OVERVIEW.*$", "", z, perl=TRUE)
# More cleaning
z <- gsub("SUPPLEMENTS", "", z, fixed=TRUE)
z <- gsub("AUDIO SAMPLE", "", z, fixed=TRUE)
z <- gsub(".{1}\\s{1}Free Trials", "", z)
z <- gsub("TEACHING NOTE", "", z, fixed=TRUE)
z <- gsub("EDUCATOR COPY", "", z, fixed=TRUE)
z <- gsub("TEACHING PLAN", "", z, fixed=TRUE)
z <- gsub(".{1}\\s{1}Details", "", z)
z <- gsub("NVF2013\\s+[0-9]{2}", "", z)
z <- gsub("SELECT ALL.*$", "", z)
z <- gsub("[^[:ascii:]]", "", z, perl=TRUE)

# trim all leading white space
z <- str_trim(z)

# Extracts case title into "case" vector
case <- sub("\\s{2,}.*$", "", z)
maybe <- list()
# Fix cut-off titles
for (i in 1:length(z)) {
  maybe[i] <- strsplit(z[i], "\\s{2,}", perl = TRUE)
  if (grepl("^Added.*$", maybe[[i]][3], perl = TRUE) == FALSE) {
    case[i] <- paste(case[i], maybe[[i]][2], sep = " ") 
 }
}
                             
# Extracts discipline into "discipline" vector
discipline <- (str_extract(z, "Discipline.*[:].*Source\\s{2}"))
discipline <- gsub("Discipline.*:", "", discipline)
discipline <- gsub("\\s+Source", "", discipline)
discipline <- str_trim(discipline)

# Extracts authors into "author" vector
author <- c()
for (i in 1:length(z)) {
  if (grepl("^Added.*$", maybe[[i]][3], perl = TRUE) == TRUE) {
    author[i] <- maybe[[i]][2]
  }
  else {
    author[i] <- maybe[[i]][3]
  }
}

# Extracts case summaries into "description" vector.
# Remove white space (str_trim only removes leading and trailing WS, not WS in body)
description <- gsub("^.*IndeStudy2016Finance", "", z)
description <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", str_trim(description), perl=TRUE)
description <- gsub("To maximize their effectiveness, color cases should be printed in color[.]\\s", "", description)
description <- gsub("Software for this case is available.*$", "", description)
description <- gsub("^.*reveal key case details[.]\\s", "", description)
description <- gsub("Includes color exhibits[.]", "", description)
description <- gsub("Video Supplement.*$", "", description)

# Extracts product ID from cases

product <- str_extract(z, "Product\\s[#].*[:].*Length")
product <- gsub("Product\\s[#]\\s+:", "", product)
product <- gsub("\\s+Length", "", product)
product <- str_trim(product)

# Collects above 5 variables into a dataframe
df <- data.frame(product, case, author, description, discipline)

# Creates indicator variable for audio-type
df$isaud <- str_count(df$case, "audio")

# Rank cases with audio and without audio: first assign audio-inclusive rank
df$aud_rank <- c(1:1085) 

# We will drop audio versions, so we assign ranks including audio cases to audio cases' PDF duplicates.
# Creates new data frame with all audio cases
g <- df[df$isaud == 1,]
# Finds g's matches in the PDF cases, then subs in audio case rank.
g$case <- gsub("\\s[(]audio version[)]", "", as.character(g$case))
for (i in 1:1085) {
  if (df$isaud[i] == 0 & any(g$case == df$case[i])) {
    df$aud_rank[i] <- g$aud_rank[g$case == df$case[i]]
  }
}

# Drop audio versions
df <- df[df$isaud == 0,]
df$pdf_rank <- c(1:1063)

# Orders variables
df <- df[,c("aud_rank", "pdf_rank", "case", "author", "discipline", "product", "description", "isaud")]

# Drop isaud
df <- df[,-8]

write.csv(df, file = "C:/Users/Anshu/Box Sync/summer 2016/Fed summer 2016/finance/fincases.csv", row.names=FALSE)
