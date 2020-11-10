library(data.table)
library(tidyverse)
library(tidytext)
library(Matrix)

rm(list = ls())
filenames = list.files("data/LASSO")
data = data.frame()
for(name in filenames)
{
  temp = read.csv(paste("data/LASSO/",name,sep=''))
  data = rbind(data,temp)
}

str(data)

# Delete the date with missing journal
# data<-data[data$journal!="",]


text_df <- tibble(paper = 1:nrow(data), abstract = data$abstract)
tt  = text_df %>% unnest_tokens(word, abstract)
A = cast_sparse(tt, paper, word)
str(A)
dim(A)
hist(rowSums(A))
cs = colSums(A)
# hist(log(cs[cs>1]))


library(vsp)
fa = vsp(A, rank = 5)

abstract = text_df$abstract


topPapers = 5
# just run the next code chunk...

topDoc = fa$Z %>% 
  apply(2,
        function(x) which(rank(-x, ties.method = "random") <= topPapers)
  )
for(j in 1:ncol(topDoc)){
  paste("topic", j, "\n \n") %>% cat
  data$title[topDoc[,j]] %>% print
  paste("\n \n \n") %>% cat
}
plot(fa$d)
plot_varimax_z_pairs(fa, 1:5)