################################################################################
###############    PLOT FIGURES ARTICLE   ######################################
################################################################################

# Charge libraries
library(readxl)
library(rstudioapi) #confort pour chemin des rep et fichiers
library(R.utils)    #confort pour chemin des rep et fichiers
library(rpart)
library(rpart.plot)
library(ggplot2)
library(magrittr)
library(ggpubr)

################################################################################
DIR = dirname(rstudioapi::getSourceEditorContext()$path) # script's repository
setwd(DIR) # define the working repository
################################################################################


# Plot and save figure 2 (climate) #--------------------------------------------

df_climate <- read_excel("climate.xlsx", col_names = FALSE)
colnames(df_climate) <- c('climate', 'experiments')

png(file="Figure_2.png", width = 30, height = 10, units = "cm", res = 600) # initiate creation of png file
Figure_2<-ggplot(data=df_climate, aes(x=climate, y=experiments)) +
  geom_bar(stat="identity", fill="darkgrey")+
  geom_text(aes(label=experiments), vjust=-0.3, size=3.5)+
  theme_bw()+ylab('number of experiments')+xlab('climates')
Figure_2
dev.off() # end creation of png file

# Plot and save figure 3 (population) #-----------------------------------------

df_pop <- read_excel("population.xlsx") # Charge excel file 

png(file="Figure_3.png", width = 30, height = 10, units = "cm", res = 600) # initiate creation of png file
hist(df_pop$Population, main = "", xlab= 'Population in thousands', 
     ylab = 'number of experiments', breaks = 30) # plot population histogram
dev.off() # end creation of png file

# Plot and save figure 4 (coast distance and altitude) #------------------------

df_coast <- read_excel("coastdistance.xlsx") # Charge excel file 
df_alt <- read_excel("altitude.xlsx") # Charge excel file 

png(file="Figure_4.png", width = 30, height = 20, units = "cm", res = 600)# initiate creation of png file
par(mfrow=2:1) # two plots in one png, two on same column
hist(df_coast$`COAST DISTANCE`, xlab= 'Distance to the coast in kilometers', 
     ylab = 'number of experiments', breaks = 30, 
     main = "(a) Distribution of experiments'distance to the coast") # plot coast distance histogram
hist(df_alt$ALTITUDE, xlab= 'Altitude above sea level in meters', 
     ylab = 'number of experiments', breaks = 30 , 
     main = "(b) Distribution of experiments'altitude") # plot altitude histogram
dev.off() # end creation of png file

# Plot and save figure 5 (main conclusions) #-----------------------------------

png(file="Figure_5.png", width = 35, height = 10, units = "cm", res = 600)
plot <- par(mfrow = c(1, 2))

df_results <- read_excel("results_experiments.xlsx", 
                  col_types = c("text", "numeric"))
barplot(height = df_results$number, 
        names.arg = df_results$impact,
        axisnames = TRUE ,
        xlab = "(a) all the experiments", ylab = "Number of results ", 
        ylim = c(0, 140),
        cex.names=1.5,
        cex.lab = 1.7)
df_summer <- read_excel("results_summer.xlsx", 
                     col_types = c("text", "numeric"))
barplot(height = df_summer$number, 
        names.arg = df_summer$impact,
        axisnames = TRUE ,
        xlab = "(b) summer experiments", ylab = "Number of results ", 
        ylim = c(0, 140),
        cex.names=1.5,
        cex.lab =1.7)
dev.off()

# Plot and save figure 6 (classification trees) #-------------------------------

#par(mfrow=1:2)
png(file="Figure_6.png", width = 40, height = 10, units = "cm", res = 600)
par(mfrow=1:2)
experiments <- read_excel("experiments.xlsx")
tree_experiments <- rpart(experiments)
rpart.plot(tree_experiments, roundint=FALSE, type = 4, extra = 1, 
           box.palette=list("pink", "lightblue", "lightgray"), 
           main = '(a) Classification tree of all studies')
summer <- read_excel("summer.xlsx")
tree_summer <- rpart(summer)
rpart.plot(tree_summer, roundint=FALSE, type = 4, extra = 1, 
           box.palette=list("pink", "lightblue", "lightgray"), 
           main = '(b) Classification tree of summer studies')
dev.off()

# Plot and save figure 7 (variables importance in the classification trees) #---

png(file="Figure_7.png", width = 40, height = 10, units = "cm", res = 600)
par(mfrow=1:2)

df_var <- data.frame(imp = tree_experiments$variable.importance)
df_var_exp <- df_var %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))

variables1 <- ggplot2::ggplot(df_var_exp) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = imp), 
               size = 1.5, alpha = 0.7) +
  coord_flip() +
  theme_bw() +
  ggtitle("(a) from classification tree of all experiments results figure 6")+
  ylab('Variable importance')+xlab('')

df_var <- data.frame(imp = tree_summer$variable.importance)
df_var_summer <- df_var %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))

variables2 <- ggplot2::ggplot(df_var_summer) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = imp), 
               size = 1.5, alpha = 0.7) +
  coord_flip() +
  theme_bw() +
  ggtitle("(b) from classification tree of summer experiment results figure 6")+
  ylab('Variable importance')+xlab('')

figure7 <- ggarrange(variables1, variables2,
                    #labels = c("A", "B", "C"),
                    ncol = 1, nrow = 2)
figure7
dev.off()

# Plot and save figure 9 (data used per year) #---------------------------------

df_data <- read_excel("data_evolution.xlsx")
df_data <- t(df_data);
png(file="Figure_9.png", width = 30, height = 12, units = "cm", res = 600)
barplot(df_data[2:5,1:50],df_data[1,1:50],
        names.arg = df_data[1,1:50],
        col = c("aquamarine","coral","darkblue","azure"),
        beside = TRUE, xlab = "Publication's years", 
        ylab = "Number of experiments", las=2);
legend(x = 1975,
       y = 37,
       c("Rain Gauge","Radar","Satellite","Modeled"),
       fill = c("aquamarine","coral","darkblue","azure"));
dev.off()
