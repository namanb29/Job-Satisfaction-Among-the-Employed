#setting working directory
setwd("M:/EC969/Term Paper")

remove(list=ls())
#creating log file
sink("TermPaperLog.log", split = T)

#installing packages 
install.packages(c("tidyverse","logr", "dplyr", "naniar", "haven", "survey", "labelled","Rmisc","expss","ggplot2", "AER","estimatr","margins","ggeffects", "plm", "ivreg", "fastDummies", "pglm", "MASS", "multiwayvcov", "stargazer", "htmltools", "magrittr", "writexl"))


#activating the packages 
packages <-c("tidyverse","logr", "dplyr", "naniar", "haven", "survey", "labelled","Rmisc","expss","ggplot2", "AER","estimatr","margins","ggeffects","ivreg", "plm", "fastDummies", "pglm", "MASS", "multiwayvcov", "stargazer", "htmltools", "magrittr", "writexl")
lapply(packages, library, character.only = T)

#creating data frame
nwaves <- 7  
variables <- list("pid","sex" ,"age" , "hlstat", "qfedhi", "jbsat", "jbsat1", "jbsat2", "jbsat4", "jbsat7")
df <- data.frame()  

#importing BHPS data set
for (i in 1:7){    
  letter <- letters[i]  
  vars <- paste(letter, variables[-1], sep = "") 
  setwd(paste0("//sernt2/ec969$/UKDA-5151-stata/stata/stata13/bhps_w",i)) 
  data <- read_dta(file = paste0(letter,"indresp.dta")) %>% 
    dplyr::select(pid, vars)
  colnames(data) <- variables   
  data$wave <- i 
  df <- bind_rows(df, data)  
}
#removing unwanted variables from environment
rm(variables,nwaves, data, vars, letter)

table(df$jbsat)

#cleaning data by getting rid of negative values
df$jbsat[df$jbsat==-9 |df$jbsat==-8 |df$jbsat==-7 | df$jbsat==-2|  df$jbsat==-1]<-NA
df$hlstat[df$hlstat==-9 |df$hlstat==-8 |df$hlstat==-7 | df$hlstat==-2|  df$hlstat==-1]<-NA
df$jbsat1[df$jbsat1==-9 |df$jbsat1==-8 |df$jbsat1==-7 | df$jbsat1==-2|  df$jbsat1==-1]<-NA
df$jbsat2[df$jbsat2==-9 |df$jbsat2==-8 |df$jbsat2==-7 | df$jbsat2==-2|  df$jbsat2==-1]<-NA
df$jbsat4[df$jbsat4==-9 |df$jbsat4==-8 |df$jbsat4==-7 | df$jbsat4==-2|  df$jbsat4==-1]<-NA
df$jbsat7[df$jbsat7==-9 |df$jbsat7==-8 |df$jbsat7==-7 | df$jbsat7==-2|  df$jbsat7==-1]<-NA
df$qfedhi[df$qfedhi==-9 |df$qfedhi==-8 |df$qfedhi==-7 | df$qfedhi==-2|  df$qfedhi==-1]<-NA

table_1 <- table(df$jbsat, df$wave, df$sex)
table_1

#renaming coloumn labels in the table
colnames(table_1) = c("1991", "1992", "1993", "1994", "1995", "1996", "1997")
table_1

#creating a sub dataframe of data relating only to females
df_females <- df
df_females <- df_females %>%
  filter(df_females$sex == 2)

#creating a sub dataframe of data relating only to males
df_males <- df
df_males <- df_males %>%
  filter(df_males$sex == 1)

#importing excel files of the sub data frames
write_xlsx(df_females,"M:/EC969/Term Paper/table_females.xlsx")
write_xlsx(df_males,"M:/EC969/Term Paper/table_males.xlsx")



#Let us now work on the female only dataframe

#creating dummy education qualification varaibles
df_females<-df_females %>%   
  mutate(Q1 = ifelse((qfedhi==1 | qfedhi==2), 1, 0),
         Q2 = ifelse(qfedhi==3, 1, 0),
         Q3 = ifelse(qfedhi==4, 1, 0), 
         Q4 = ifelse(qfedhi==5, 1, 0), 
         Q5 = ifelse(qfedhi==6, 1, 0), 
         Q6 = ifelse(qfedhi==7, 1, 0))

#adding labels to the dummy variables
var_lab(df_females$Q1) <- "1st degree or higher"
var_lab(df_females$Q2) <- "hnd,hnc,teaching"
var_lab(df_females$Q3) <- "a level"
var_lab(df_females$Q4) <- "o level"
var_lab(df_females$Q5) <- "cse"
var_lab(df_females$Q6) <- "none of these qualif"

#creating dummy health status variables
df_females<-df_females %>%   
  mutate(H1 = ifelse((hlstat==1 | hlstat==2), 1, 0),
         H2 = ifelse(hlstat==3, 1, 0),
         H3 = ifelse((hlstat==4|hlstat==5), 1, 0)) 

#adding labels to the dummy variables
var_lab(df_females$H1) <- "Health - Excellent/Good"
var_lab(df_females$H2) <- "Health - Fair"
var_lab(df_females$H3) <- "Health - Poor/Very Poor"

#filtering dataset to include females of working age only
df_females<-df_females %>%
  filter(age >= 16 & age <= 64)

#cleaning data set to avoid any missing values like NA
df_females$age2<-df_females$age*df_females$age
df_females <- df_females %>%
  filter((hlstat >= 1 | hlstat <= 5), (jbsat >= 1 | jbsat <= 7), (jbsat1 >= 1 | jbsat1 <= 7), (jbsat2 >= 1 | jbsat2 <= 7), (jbsat4 >= 1 | jbsat4 <= 7), (jbsat7 >= 1 | jbsat7 <= 7))

#convering variable into orderd factor from levels 1 to 7
df_polr_females <- data.frame(df_females)
df_polr_females$jbsat <- factor(df_polr_females$jbsat, levels=1:7, ordered=T)
df_polr_females$jbsat_num <- as.numeric(df_polr_females$jbsat)

#cleaning data set to avoid any missing values like NA
df_polr_females <- df_polr_females%>%
  filter((Q1 >= 0 | Q1 <= 1), (Q2 >= 0 | Q2 <= 1), (Q3 >= 0 | Q3 <= 1), (Q4 >= 0 | Q4 <= 1), (Q5 >= 0 | Q5 <= 1), (Q6 >= 0 | Q6 <= 1), (H1 >= 0 | H1 <= 0), (H2 >= 0 | H2 <= 0), (H3 >= 0 | H3 <= 0), (jbsat >= 1 | jbsat<= 7), (jbsat1 >= 1 | jbsat1 <= 7), (jbsat2 >= 1 | jbsat2 <= 7), (jbsat4 >= 1 | jbsat4 <= 7), (jbsat7 >= 1 | jbsat7 <= 7))


#checking if there are still any missing values
which(is.na(df_polr_females))


#setting up a probit linear regression function
probit_linear_female <- glm(jbsat ~ age + age2  + jbsat1 + jbsat2 + jbsat4 + jbsat7 + Q1+ Q2 + Q3 + Q4 + Q5 + Q6 + H1 + H2 + H3, family = binomial(link = "probit"), data = df_polr_females)  
summary(probit_linear_female)

#setting up an ordered probit regression function
ordered_probit_female <- polr(jbsat ~ age+ age2 + jbsat1 + jbsat2 + jbsat4 + jbsat7 + Q1+ Q2 + Q3 + Q4 + Q5 + Q6 + H1 + H2 + H3 , data = df_polr_females, method= "probit")  
summary(ordered_probit_female)

#this probit model leads to an error due to its complexity
#we drop a coefficient and try again
ordered_probit_female <- polr(jbsat ~ age+ age2 + jbsat1 + jbsat2 + jbsat4 + jbsat7 + Q1+ Q2 + Q3 + Q4 + Q5 + Q6 + H1 + H3 , data = df_polr_females, method= "probit")  
summary(ordered_probit_female)

#Now, let us turn to the male only data set

#setting educational qualification dummy variables
df_males<-df_males %>%   
  mutate(Q1 = ifelse((qfedhi==1 | qfedhi==2), 1, 0),
         Q2 = ifelse(qfedhi==3, 1, 0),
         Q3 = ifelse(qfedhi==4, 1, 0), 
         Q4 = ifelse(qfedhi==5, 1, 0), 
         Q5 = ifelse(qfedhi==6, 1, 0), 
         Q6 = ifelse(qfedhi==7, 1, 0))

#adding labels to the dummy variables
var_lab(df_males$Q1) <- "1st degree or higher"
var_lab(df_males$Q2) <- "hnd,hnc,teaching"
var_lab(df_males$Q3) <- "a level"
var_lab(df_males$Q4) <- "o level"
var_lab(df_males$Q5) <- "cse"
var_lab(df_males$Q6) <- "none of these qualif"

#setting health status dummy variables
df_males<-df_males %>%   
  mutate(H1 = ifelse((hlstat==1 | hlstat==2), 1, 0),
         H2 = ifelse(hlstat==3, 1, 0),
         H3 = ifelse((hlstat==4|hlstat==5), 1, 0)) 

#adding labels to the dummy variables
var_lab(df_males$H1) <- "Health - Excellent/Good"
var_lab(df_males$H2) <- "Health - Fair"
var_lab(df_males$H3) <- "Health - Poor/Very Poor"

#filtering dataset to include males of working age only
df_males<-df_males %>%
  filter(age >= 16 & age <= 64)

#cleaning data set to avoid any missing values like NA
df_males$age2<-df_males$age*df_males$age
df_males <- df_males %>%
  filter((hlstat >= 1 | hlstat <= 5), (jbsat >= 1 | jbsat <= 7), (jbsat1 >= 1 | jbsat1 <= 7), (jbsat2 >= 1 | jbsat2 <= 7), (jbsat4 >= 1 | jbsat4 <= 7), (jbsat7 >= 1 | jbsat7 <= 7))

#convering variable into orderd factor from levels 1 to 7
df_polr_males <- data.frame(df_males)
df_polr_males$jbsat<-factor(df_polr_males$jbsat, levels=1:7, ordered=T)
df_polr_males$jbsat_num<-as.numeric(df_polr_males$jbsat)

#cleaning data set to avoid any missing values like NA
df_polr_males <- df_polr_males%>%
  filter((Q1 >= 0 | Q1 <= 1), (Q2 >= 0 | Q2 <= 1), (Q3 >= 0 | Q3 <= 1), (Q4 >= 0 | Q4 <= 1), (Q5 >= 0 | Q5 <= 1), (Q6 >= 0 | Q6 <= 1), (H1 >= 0 | H1 <= 0), (H2 >= 0 | H2 <= 0), (H3 >= 0 | H3 <= 0), (jbsat >= 1 | jbsat<= 7), (jbsat1 >= 1 | jbsat1 <= 7), (jbsat2 >= 1 | jbsat2 <= 7), (jbsat4 >= 1 | jbsat4 <= 7), (jbsat7 >= 1 | jbsat7 <= 7))

#checking if there are still any missing values
which(is.na(df_polr_males))
table(df_polr_males$jbsat)

#setting up a probit linear regression function
probit_linear_male <- glm(jbsat ~ age + age2  + jbsat1 + jbsat2 + jbsat4 + jbsat7 + Q1+ Q2 + Q3 + Q4 + Q5 + Q6 + H1 + H2 + H3, family = binomial(link = "probit"), data = df_polr_males)  
summary(probit_linear_male)

#setting up an ordered probit regression function
ordered_probit_male <-polr(jbsat ~ age+ age2+ jbsat1 + jbsat2 + jbsat4 + jbsat7 + Q1+ Q2 + Q3 + Q4 + Q5 + Q6 + H1 + H2 + H3, data=df_polr_males,  method="probit")  

#once again, the same error pops up.
#we drop the same coefficient and try again
ordered_probit_male <-polr(jbsat ~ age+ age2+ jbsat1 + jbsat2 + jbsat4 + jbsat7 + Q1+ Q2 + Q3 + Q4 + Q5 + Q6 + H1 + H2, data=df_polr_males,  method="probit")  
summary(ordered_probit_male)

sink()


