##
# 运行第4行到第310行的代码，这些是方程和一些必要的包
# 运行第318行到第320行的代码, 读取数据
#
# 运行第315行到第376 以及 第418到第452行的代码，得到结果，结果存储在final_dn里面
library(readr)
library(reshape2)
library(dplyr)
library(tidyr)
library(plyr)
library(data.table)
library(parallel)

#>------------------------------INTERNAL FUNCION START------------------------------<
  
  # this function is used to make a char frequency table
  # parameter "data": "mydata_t2h" for drug cleaned data and "news" for news data
  # It return is 
  onecharFreq <- function(data) {
    tem <- strsplit(data,"")[[1]]
    temFeq <- as.data.frame(table(tem))
    temFeq <- temFeq[order(temFeq$Freq, decreasing = T),]
    colnames(temFeq) <- c('character','freq')
    rownames(temFeq) <- NULL
    temFeq$pct <- with(temFeq, freq/sum(temFeq$freq))
    
    return(temFeq)
  }
  
  # get word frequenct with n character from realword data
  # for the function "new_two_char <- ncharFreq()"
  # split large news data into several chunks
  ncharFreq2 <- function(chunkNumber, data, x, charFeq = NULL) {
    
    split_value <- length(data$article) %/% chunkNumber
    chunkSteps <- 1:chunkNumber
    
    for(i in chunkSteps) {
      cat(">>>Start running chunk number: ",i, "out of", chunkNumber, "\n")
      print(Sys.time())
      if(i==1) { # generate an original frequenct table with the news data in first chunk
        tempnews <- paste(data$article[i:split_value], collapse = "")
        final_char <<- ncharFreq(tempnews,x,charFeq)
      } else if(i == chunkNumber) { 
        # the length of data in the last chunk is diff from the previous, 
        # so we get the data from split_value*(i-1)+1:length(data$article)
        tempnews <- paste(data$article[split_value*(i-1)+1:length(data$article)], collapse = "")
        tempfinal_char <- ncharFreq(tempnews,x,charFeq) 
      } else { 
        # data between the first and last chunk
        tempnews <- paste(data$article[split_value*(i-1)+1:split_value*i], collapse = "")
        tempfinal_char <- ncharFreq(tempnews,x,charFeq) 
      }
      if(i != 1) {
        final_char <<- merge(final_char, tempfinal_char ,by = "character", all = TRUE)
        final_char[is.na(final_char)] <<- 0
        final_char$freq <<- with(final_char, freq.x+freq.y)
        final_char <<- final_char[,c("character","freq")]
        final_char$pct <<- with(final_char, freq/sum(final_char$freq))
      } else {
        next
      }
    }
    # calcualte character frequency in each word
    if(x == 2) {
      temp_df <- do.call(rbind,sapply(as.character(final_char$character), FUN = function(x) strsplit(x,"")))
      for(j in 1:x) {
        # create new column to store single character
        final_char[,LETTERS[j]]<<- temp_df[,j] 
        #replace the single char by its global frequency
        final_char[,LETTERS[j]] <<- unlist(lapply(final_char[,LETTERS[j]], 
                                                  FUN = function(x) charFeq[charFeq$character == x,]$pct))
      }
    }
    return(final_char)
  }
  
  
  
  
  # this function create a table of word frequency table with n characters.
  # parameter "data"   : "mydata_t2h" for drug cleaned data and "news" for news data
  #           "x"      : the number of parameter of the combined word.
  #           "charfeq": the result of the onecharFreq function. 
  ncharFreq <- function(data, x, charfeq = NULL) {
    
    #split raw data
    data <- strsplit(data,"")[[1]]
    tem <- data
    
    # create word list with "n" character
    # eg:   BEFROE: ABCDEFGH
    #       AFTER : AB BC CD DE EF FG GH
    for(i in 1:(x-1)) {
      tem<- paste0(tem,data[i+1:length(data)])
    }
    tem <- tem[1:length(data)]
    temFeq <- as.data.frame(table(tem))
    
    # calculate word frequency and sort the value in descending order
    # reset column names and index
    temFeq <- temFeq[order(temFeq$Freq, decreasing = T),]
    colnames(temFeq) <- c('character','freq')
    rownames(temFeq) <- NULL
    temFeq$pct <- with(temFeq, freq/sum(temFeq$freq))
    
    # drop row with non-chinese character
    temFeq <- temFeq[temFeq$character %in% 
                       regmatches(as.character(temFeq$character), 
                                  regexpr("^[\u4e00-\u9fa5]{0,}$", 
                                          as.character(temFeq$character), 
                                          perl = TRUE)),]
    
    if(x == 2 & identical(charfeq, one_char)) {
      # calcualte character frequency in each word
      temp_df <- do.call(rbind,sapply(as.character(temFeq$character), FUN = function(x) strsplit(x,"")))
      for(j in 1:x) {
        temFeq[,LETTERS[j]]<- temp_df[,j] # create new column to store single character
        temFeq[,LETTERS[j]] <- unlist(lapply(temFeq[,LETTERS[j]], 
                                             FUN = function(x) charfeq[charfeq$character == x,]$pct))
        #replace the single char by its global frequency
      }
    } 
    return(temFeq)
  }
  
  
  # this function works as a filter. The input value are userchar and realword,
  # which is the word with n character of user-input data and real-world data
  # It returns a mergedData with word means the log threshold.
  charFilter <- function(userchar, realword) {
    # Left merge default and realworld dataframe
    mergedData <- left_join(userchar, realword, by = c("character"))
    # if the word contains 2 characters.
    # then we calculate its MPMI(Modified Point Mutual Infomation)
    # else 
    if (length(strsplit(as.character(userchar$character[1]),"")[[1]]) == 2) {
      unmatchedData <- mergedData[is.na(mergedData$freq.y),]
      mergedData <- mergedData[complete.cases(mergedData), ]
      mergedData$log <- with(mergedData, log2((pct.x/(A.x*B.x))/(pct.y/(A.y*B.y))))
      unmatchedData$log <- with(unmatchedData, log2(pct.x/(A.x*B.x)))
      
      mergedData <- mergedData[mergedData$log >= as.double(summary(mergedData$log)[4]),]
      unmatchedData <- unmatchedData[unmatchedData$log >= as.double(summary(unmatchedData$log)[4]),]
      mergedData <- rbind(mergedData, unmatchedData)
      mergedData <- mergedData[c("character", "log")]
      mergedData <- mergedData[order(mergedData$log, decreasing = T),]
    }
    rownames(mergedData) <- NULL
    return(mergedData)
  }
  
  
  # inner entropy calculator
  innerH <- function(two_char, segWords) {
    h_l_r <- feq_counter_R(strsplit(two_char,"")[[1]][1], segWords)
    h_r_l <- feq_counter_L(strsplit(two_char,"")[[1]][2], segWords)
    return(min(h_l_r,h_r_l))
  }
  
  # outer entropy calculator
  outerH <- function(two_char,segWords) {
    h_r <- feq_counter_R(strsplit(two_char,"")[[1]][2], segWords)
    h_l <- feq_counter_L(strsplit(two_char,"")[[1]][1], segWords)
    return(min(h_r,h_l))
  }
  
  
  nw_counter <- function(cand_word, segwords) {
    word_list <<- c()
    
    for (j in seq_along(segWords)) {
      if (cand_word == segWords[j])
        word_list <<- c(word_list, segWords[j+1])
      else
        next
    } 
    char_table <- as.data.frame(table(word_list))
    char_table <- char_table[order(char_table$Freq, decreasing = T),]
    char_table$pct <- with(char_table, Freq/sum(Freq))
    rownames(char_table) <- NULL
    
    return(char_table)
  }
  ``
  # Method: 
  #         feq_counter
  # Param: 
  #         type      : character
  #         cand_word : input candidate character
  # Return: 
  #         type: data.frame
  #         char_table: find the fequenct of appearance of the next character of the candi word
  feq_counterR <- function(cand_word, segWords) {
    
    word_list <<- c()
    
    for (j in seq_along(segWords)) {
      if (cand_word == segWords[j])
        word_list <<- c(word_list, segWords[j+1])
      else
        next
    } 
    char_table <- as.data.frame(table(word_list))
    char_table <- char_table[order(char_table$Freq, decreasing = T),]
    rownames(char_table) <- NULL
    
    #char_table$pct <<- format(round(with(char_table, 
    #                                    Freq/sum(char_table$Freq)), 3), 
    #                         nsmall = 3) 
    char_table$pct <- with(char_table, Freq/sum(char_table$Freq))
    char_table$global_fequency <- with(char_table, Freq/length(segWords))
    entropy <<- sum(unlist(lapply(char_table$global_fequency, function(x) -x*log2(x))))
    return(char_table)
  }
  
  feq_counter_R <- function(cand_word, segWords) {
    feq_counterR(cand_word, segWords)
    return(entropy)
  }
  
  feq_counterL <- function(cand_word, segWords) {
    
    word_list <<- c()
    
    for (j in seq_along(segWords)) {
      if (j == 1)
        next
      if (cand_word == segWords[j])
        word_list <<- c(word_list, segWords[j-1])
      else
        next
    } 
    char_table <- as.data.frame(table(word_list))
    char_table <- char_table[order(char_table$Freq, decreasing = T),]
    rownames(char_table) <- NULL
    
    #char_table$pct <<- format(round(with(char_table, 
    #                                    Freq/sum(char_table$Freq)), 3), 
    #                         nsmall = 3) 
    char_table$pct <- with(char_table, Freq/sum(char_table$Freq))
    char_table$global_fequency <- with(char_table, Freq/length(segWords))
    return(char_table)
  }
  
  feq_counter_L <- function(cand_word, segWords) {
    feq_counterL(cand_word, segWords)
    return(entropy)
  }
  
  textfeq <- function(myfile) {
    seg_words <- strsplit(myfile, "")
    tableWord <- table(seg_words) 
    tableWord <- as.data.frame(tableWord)
    tableWord <- tableWord[order(tableWord$Freq, decreasing = T),]
    rownames(tableWord) <-NULL
    
    segWords <<- seg_words[[1]][-1]
    uniqueWords <- unique(seg_words[[1]][-1])
    steps <- 1:length(uniqueWords)
    
    #tableWord$pct <<- format(round(with(tableWord, 
    #                                     Freq/sum(tableWord$Freq)), 3), 
    #                          nsmall = 3)
    
    #tableWord$pct <- with(tableWord, Freq/sum(tableWord$Freq))
    cat("Character fequency table generazation complete!\n")
    return(tableWord)
  }
  
  nextword_miner <- function(n_char_final, num_char, threshold) {
    # initialize output list
    cl <- list("cont" = NULL, "stop" = NULL)
    # Split word with n characters 
    # Combine the result into n_char_final dataframe
    temp_df <- do.call(rbind,sapply(as.character(n_char_final$character), 
                                    FUN = function(x) strsplit(x,"")))
    for(j in 1:num_char) {
      n_char_final[,LETTERS[j]]<- temp_df[,j]
    }
    
    tail_char <- n_char_final[,LETTERS[num_char]] # the last character of the given word
    head_char <- n_char_final[,LETTERS[1]]        # the first character of the given word
    sapply(strsplit("a", " "), length)
    steps <- 1:length(n_char_final$character) # create step for loop by traversal unique last character
    for(i in steps) { 
      # if the last word "A" appear in the first word pool
      tail_char <- n_char_final[n_char_final$character == as.character(n_char_final$character[i]),][,LETTERS[num_char]]
      if(tail_char %in% head_char) {
        candi_table <- nw_counter(tail_char, mydata_split)
        candi_char <- as.character(candi_table[candi_table$pct >= threshold,]$word_list)
        # get all word start with character "A"
        if(candi_char %in% head_char) {
          next_char <- n_char_final[n_char_final$A == candi_char,]$character
        }
        tem_char <- n_char_final[n_char_final[,LETTERS[1]] == unique(tail_char)[i],]$character
        # Get all characters before "A"
        tem_first_char <- n_char_final[rownames(
          n_char_final[n_char_final[,LETTERS[num_char]] == 
                         unique(tail_char)[i],]),][,LETTERS[num_char-1]]
        cl$cont <- append(cl$cont,unlist(lapply(tem_first_char, 
                                                function(tem_first_char) paste0(
                                                  tem_first_char,tem_char
                                                ))))
      } else {
        cl$stop <- append(cl$stop, 
                          as.character(n_char_final[n_char_final[,LETTERS[num_char]] == unique(tail_char)[i],]$character))
      }
    }
    return(cl)
  }
  # >------------------------------INTERNAL FUNCION END------------------------------<

  

# >------------------------------START------------------------------<
main <- function(){}

# loading a series of .csv file
files = list.files(pattern=".csv")# for multiple files in the same folder
news = rbindlist(mclapply(files, read.csv), fill = TRUE)
news <- as.data.frame(news)


# splitting news file and create the character frequency table of real world
# news file which named `news` comes from the previous result
for(i in 1:length(files)){
  cat("> Reading file: ", files[i], "\n")
  mytempFile <- read.csv(files[i])
  mytempFile <- as.data.frame(mytempFile)
  if("content" %in% colnames(mytempFile)) {colnames(mytempFile) <- c("id", "article")}
  mytempFile <- paste(mytempFile$article, collapse = "")
  tempnewsFreq <- textfeq(mytempFile)
  if(i == 1) {
    newsFreq <- tempnewsFreq
  } else {
    newsFreq <- merge(newsFreq, tempnewsFreq ,by = "seg_words", all = TRUE)
    newsFreq[is.na(newsFreq)] <- 0
    newsFreq$Freq <- with(newsFreq, Freq.x+Freq.y)
  }
  newsFreq <- newsFreq[,c("seg_words","Freq")]
}

newsFreq$pct <- with(newsFreq, Freq/sum(newsFreq$Freq))
colnames(newsFreq) <- c("character", "freq", "pct")
newsFreq <- newsFreq[order(newsFreq$freq, decreasing = T),]
write.csv(newsFreq, "result/newsFeq", row.names = FALSE)

# the end of the privious process
newsFreq <- read.csv("result/news frequency.csv")

# Input user default data
mydata <- read.delim2("drugs_all.txt",encoding = 'UTF-8',header = F)
mydata <- as.data.frame(mydata)
colnames(mydata) <- c("drugs_name")
mydata_t2h <- paste(mydata$drugs_name, collapse = "")

# get character pool for both user default and news data
# news stand for the real world data. 
# We call the character frequency table created based on "news" 
# Chinese Character andfrequency table
mydata_split <- strsplit(mydata_t2h, "")[[1]]

news_segword <- newsFreq$seg_words
mydata_table <- textfeq(mydata_t2h)
# mydata_segword <- segWords

# get word frequenct with n character from user data
one_char <- onecharFreq(mydata_t2h)
two_char <- ncharFreq(mydata_t2h, 2, one_char)
two_char$log <- with(two_char, log2(pct/(A*B)))
tri_char <- ncharFreq(mydata_t2h, 3)
qua_char <- ncharFreq(mydata_t2h, 4)
five_char <- ncharFreq(mydata_t2h, 5)
six_char <- ncharFreq(mydata_t2h, 6)
sev_char <- ncharFreq(mydata_t2h, 7)


new_two_char <- fread("result/news 2charfeq.csv")
new_tri_char <- fread("result/news 3charfeq.csv")
new_qua_char <- fread("result/news 4charfeq.csv")
new_five_char <- fread("result/news 5charfeq.csv")
news_6char <- fread("result/news 6charfeq.csv")
news_7char <- fread("result/news 7charfeq.csv")

# here we have to apply `ncharFreq2` function for the huge news data
new_two_char <- ncharFreq2(10,news,2,newsFreq)
new_tri_char <- ncharFreq2(50,news,3)
new_tri_char <- new_tri_char[new_tri_char$freq >= 3,]
new_qua_char <- ncharFreq2(100,news,4)
new_qua_char <- new_qua_char[new_qua_char$freq >= 3,]
new_five_char <- ncharFreq2(50,news,5)
news_6char <- ncharFreq2(100,news,6)
news_7char <- ncharFreq2(100,news,7)
new_two_char <- new_two_char[,-1]


# new word mining preparation
two_char_filter <- charFilter(two_char,new_two_char5)
tri_char_filter <- charFilter(tri_char,new_tri_char)
qua_char_filter <- charFilter(qua_char,new_qua_char)
five_char_filter <- charFilter(five_char,new_five_char)
six_char_filter <- charFilter(six_char,news_6char)
sev_char_filter <- charFilter(sev_char,news_7char)
two_char_final <- two_char_filter[,c("character","log")]

# calculate information entropy between two word
two_char_filter$innerEntropy  <- unlist(lapply(two_char_filter$character, 
                                                 FUN =  function(x) innerH(x, mydata_split)))

two_char_filter$outerEntropy  <- unlist(lapply(two_char_filter$character, 
                                                 FUN =  function(x) outerH(x, mydata_split)))

# calculate word score. 
# word with higher score would be taken to the further step
weight_lambda <- 0.3
two_char_filter$score <- with(two_char_filter, (1-lambda)log-lambda(innerEntropy+outerEntropy))


# the following process is used for getting word frequency table with n characters
# mining word length: 3
cl <- nextword_miner(two_char_filter,2)
cl$cont <- unique(cl$cont[cl$cont %in% tri_char_filter$character == TRUE])
tri_char_final <- tri_char_filter[tri_char_filter$character %in% cl$cont,]

# mining word length: 4
cl2 <- nextword_miner(tri_char_final,3)
cl2$cont <- unique(cl2$cont[cl2$cont %in% qua_char_filter$character == TRUE])
qua_char_final <- qua_char_filter[qua_char_filter$character %in% cl2$cont,]

# mining word length: 5
cl3 <- nextword_miner(qua_char_final,4)
cl3$cont <- unique(cl3$cont[cl3$cont %in% five_char_filter$character == TRUE])
five_char_final <- five_char_filter[five_char_filter$character %in% cl3$cont,]


# mining word length: 6
cl4 <- nextword_miner(five_char_final,5)
cl4$cont <- unique(cl4$cont[cl4$cont %in% six_char_filter$character == TRUE])
six_char_final <- six_char_filter[six_char_filter$character %in% cl4$cont,]

cl5 <- nextword_miner(six_char_final,6)
cl5$cont <- unique(cl5$cont[cl5$cont %in% six_char_filter$character == TRUE])
sev_char_final <- sev_char_filter[sev_char_filter$character %in% cl5$cont,]

# generate final list and save
final_dn <- list()
final_dn <- append(final_dn,cl$stop)
final_dn <- append(final_dn,cl2$stop)
final_dn <- append(final_dn,cl3$stop)
final_dn <- append(final_dn,cl4$stop)
final_dn <- append(final_dn,cl5$stop)
final_dn <- unlist(final_dn)
final_dn

write.csv(final_dn, "drug name.csv", row.names = FALSE)
# >------------------------------END------------------------------<

# others
#standard <- as.data.frame(read_csv("tymc.csv"))
#result <- read.delim2("drug name.txt",encoding = 'UTF-8',header = F)
#result <- as.data.frame(result)


## 75% of the sample size
#mp_size <- floor(0.8 * nrow(mydata))

## set the seed to make your partition reproducible
#set.seed(123)
#train_ind <- sample(seq_len(nrow(mydata)), size = smp_size)

#test <- mydata[train_ind, ]
#train <-mydata[-train_ind, ]

#mydata$drugs_name <- str_replace_all(mydata$drugs_name, "[[:punct:]]", "")
#mydata$drugs_name <- str_replace_all(mydata$drugs_name, "[[:digit:]]", "")

#for (i in length(result$V1):1) {
#  candidate <-  as.character(mydata$drugs_name[which(grepl(result$V1[i], mydata$drugs_name))])
#  a <- as.data.frame(table(grepl("yw", candidate)))
#  if (length(candidate != 0) & a[a$Var1 == F,]$Freq == length(candidate)) {
#   replacement <- paste0("{", result$V1[i],"||yw}")
#    mydata$drugs_name <- str_replace_all(mydata$drugs_name, as.character(result$V1[i]), replacement)
#  }
#  else
#    next
#}

#annotated <- list()
#pure <- list()
#for (i in 1:length(result$V1)) {
#  candidate <-  as.character(mydata$drugs_name[which(grepl(result$V1[i], mydata$drugs_name))])
#  a <- as.data.frame(table(grepl("yw", candidate)))
#  if (length(candidate != 0)) {
#    drug <- paste0("{", result$V1[i],"||yw}")
#    annotated <- append(annotated, grep("yw",str_replace_all(mydata$drugs_name, as.character(result$V1[i]), drug), value = T))
#  }
#  else
#    next
#}
#annotated <- mydata[which(grepl("yw",mydata$drugs_name)),]
#write(annotated, "annotated_a.txt")

#output <- read.delim2("Output_data.txt",encoding = 'UTF-8',header = F)
#output <- as.data.frame(output)
