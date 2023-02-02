#########################################################
###########    ALL DATA LENGTH   ########################
#########################################################

library(rpart)
library(rpart.plot)
library(readxl)
library(rstudioapi) #confort pour chemin des rep et fichiers
library(R.utils)    #confort pour chemin des rep et fichiers
library(dplyr)
library(ggplot2)

DIR = dirname(rstudioapi::getSourceEditorContext()$path) # C'est le rep o? il y a le fichier de script
setwd(DIR)



ALL0 <- read_excel("12_good_scales.xlsx")
View(ALL0)
arbre_ALL0 <- rpart(ALL0, method="class", minsplit=10)
rpart.plot(arbre_ALL0)



ALL1 <- read_excel("13_climatological.xlsx")
View(ALL1)
arbre_ALL1 <- rpart(ALL1, minsplit=15)
rpart.plot(arbre_ALL1)



ALL3 <-  read_excel("14_summer.xlsx")
View(ALL3)
arbre_ALL3 <- rpart(ALL3)
rpart.plot(arbre_ALL3)


ALL3 <-  read_excel("12_event.xlsx")
View(ALL3)
arbre_ALL3 <- rpart(ALL3)
rpart.plot(arbre_ALL3)

meanvar.rpart(arbre_ALL3)
plotcp(arbre_ALL3)
post.rpart(arbre_ALL3)
predict.rpart(arbre_ALL3)
print.rpart(arbre_ALL3)
printcp(arbre_ALL3)
prune.rpart(arbre_ALL3, "METHOD")
residuals.rpart(arbre_ALL3)
rsq.rpart(arbre_ALL3)
snip.rpart(arbre_ALL3)
summary.rpart(arbre_ALL3)




png(file="test.png", width = 30, height = 30, units = "cm", res = 600)
arbre_ALL1 <- rpart(ALL1, minbucket = 1)
rpart.plot(arbre_ALL1)

dev.off()




allscales <- read_excel("13_climatological.xlsx")
png(file="arbre_allscales.png", width = 30, height = 30, units = "cm", res = 600)
arbre_allscales <- rpart(allscales)
rpart.plot(arbre_allscales, roundint=FALSE, type = 4, extra = 1, box.palette=list("pink", "lightblue", "lightgray"))
dev.off()
printcp(arbre_allscales)

df <- data.frame(imp = arbre_allscales$variable.importance)
df2 <- df %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))

ggplot2::ggplot(df2) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = imp), 
               size = 1.5, alpha = 0.7) +
  geom_point(aes(x = variable, y = imp, col = variable), 
             size = 4, show.legend = F) +
  coord_flip() +
  theme_bw()





png(file="arbre_allscales.png", width = 30, height = 30, units = "cm", res = 600)
allscales <- read_excel("14_summer.xlsx")
arbre_allscales <- rpart(allscales)
rpart.plot(arbre_allscales, roundint=FALSE, type = 4, extra = 1, box.palette=list("pink", "lightblue", "lightgray"))
dev.off()
printcp(arbre_allscales)

df <- data.frame(imp = arbre_allscales$variable.importance)
df2 <- df %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))

ggplot2::ggplot(df2) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = imp), 
               size = 1.5, alpha = 0.7) +
  geom_point(aes(x = variable, y = imp, col = variable), 
             size = 4, show.legend = F) +
  coord_flip() +
  theme_bw() +
  ggtitle("(a) All dataset without yy")

summary(arbre_allscales, cp = 0.06)





png(file="clim_tree.png", width = 30, height = 30, units = "cm", res = 600)
allscales <- read_excel("13_climatological.xlsx")
arbre_allscales <- rpart(allscales)
rpart.plot(arbre_allscales, roundint=FALSE, type = 4, extra = 1, box.palette=list("pink", "lightblue", "lightgray"))
dev.off()
printcp(arbre_allscales)

df <- data.frame(imp = arbre_allscales$variable.importance)
df2 <- df %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))

ggplot2::ggplot(df2) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = imp), 
               size = 1.5, alpha = 0.7) +
  geom_point(aes(x = variable, y = imp, col = variable), 
             size = 4, show.legend = F) +
  coord_flip() +
  theme_bw() +
  ggtitle("(a) All dataset without yy")

summary(arbre_allscales, cp = 0.06)








allscales <- read_excel("12_good_scales.xlsx")
arbre_allscales <- rpart(allscales)
rpart.plot(arbre_allscales, roundint=FALSE, type = 4, extra = 1, box.palette=list("pink", "lightblue", "lightgray"))

printcp(arbre_allscales)

df <- data.frame(imp = arbre_allscales$variable.importance)
df2 <- df %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))

ggplot2::ggplot(df2) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = imp), 
               size = 1.5, alpha = 0.7) +
  geom_point(aes(x = variable, y = imp, col = variable), 
             size = 4, show.legend = F) +
  coord_flip() +
  theme_bw() +
  ggtitle("(a) All dataset without yy")

summary(arbre_allscales, cp = 0.06)

