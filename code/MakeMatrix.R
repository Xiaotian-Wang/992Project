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

data<-data[data$journal!="",]

text_df <- tibble(paper = 1:nrow(data), journal = data$journal)
tt = text_df
A = cast_sparse(tt, paper, journal)
str(A)
dim(A)
hist(rowSums(A))
cs = colSums(A)
# hist(log(cs[cs>1]))


library(vsp)
fa = vsp(A, rank = 3)

journal = text_df$journal
apply(fa$Y,2, function(x) journal[order(-x)[1:20]])%>%View

plot_varimax_z_pairs(fa, 1:3)