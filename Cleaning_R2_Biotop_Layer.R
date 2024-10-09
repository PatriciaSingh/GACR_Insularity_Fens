
# Cleaning_R2_Biotop_Attribute_Table --------------------------------------

# Loading Attribute Table -------------------------------------------------
r_biotop <- read.csv("C:/Users/patad/PATA/UNI_MUNI/RESEARCH/GACR_Insularity_Fens/Selected_R2_Biotops_2.csv") 

# Loading libraries -------------------------------------------------------
library(dplyr)
library(tidyverse)
library(tidyr)
library(reshape2)

# Split text --------------------------------------------------------------

## Step 1: separete biotops in column BIOTOP_SEZ (max 6 biotopes per cell) to separate columns ----
df <- r_biotop %>% separate(BIOTOP_SEZ, c("Biotop1", "Biotop2", "Biotop3", "Biotop4", "Biotop5", "Biotop6"), ",")

## Step 2: separete biotop name and percentage value to separate columns ----
df1 <- df %>% separate(Biotop1, c("Biotop1_Descrip", "Biotop1_Percent"), " " )
df2 <- df1 %>% separate(Biotop2, c("none2", "Biotop2_Descrip", "Biotop2_Percent"), " " )# there is extra space in the first place of the cell - solve by adding column none, which will be deleted later on 
df3 <- df2 %>% separate(Biotop3, c("none3", "Biotop3_Descrip", "Biotop3_Percent"), " " )
df4 <- df3 %>% separate(Biotop4, c("none4", "Biotop4_Descrip", "Biotop4_Percent"), " " )
df5 <- df4 %>% separate(Biotop5, c("none5", "Biotop5_Descrip", "Biotop5_Percent"), " " )
df6 <- df5 %>% separate(Biotop6, c("none6", "Biotop6_Descrip", "Biotop6_Percent"), " " )

## Step 3: remove parentheses from Biotop_Percent columns ----
df6_Biotop1_Percent_gsub <- as.data.frame(gsub("[()]", "", df6$Biotop1_Percent))
df6_Biotop2_Percent_gsub <- as.data.frame(gsub("[()]", "", df6$Biotop2_Percent))
df6_Biotop3_Percent_gsub <- as.data.frame(gsub("[()]", "", df6$Biotop3_Percent))
df6_Biotop4_Percent_gsub <- as.data.frame(gsub("[()]", "", df6$Biotop4_Percent))
df6_Biotop5_Percent_gsub <- as.data.frame(gsub("[()]", "", df6$Biotop5_Percent))
df6_Biotop6_Percent_gsub <- as.data.frame(gsub("[()]", "", df6$Biotop6_Percent))

## Step 4: rename columns names ----
names(df6_Biotop1_Percent_gsub)[names(df6_Biotop1_Percent_gsub) == 'gsub(\"[()]\", \"\", df6$Biotop1_Percent)'] <- 'Biotop1_Percent_ok'
names(df6_Biotop2_Percent_gsub)[names(df6_Biotop2_Percent_gsub) == 'gsub(\"[()]\", \"\", df6$Biotop2_Percent)'] <- 'Biotop2_Percent_ok'
names(df6_Biotop3_Percent_gsub)[names(df6_Biotop3_Percent_gsub) == 'gsub(\"[()]\", \"\", df6$Biotop3_Percent)'] <- 'Biotop3_Percent_ok'
names(df6_Biotop4_Percent_gsub)[names(df6_Biotop4_Percent_gsub) == 'gsub(\"[()]\", \"\", df6$Biotop4_Percent)'] <- 'Biotop4_Percent_ok'
names(df6_Biotop5_Percent_gsub)[names(df6_Biotop5_Percent_gsub) == 'gsub(\"[()]\", \"\", df6$Biotop5_Percent)'] <- 'Biotop5_Percent_ok'
names(df6_Biotop6_Percent_gsub)[names(df6_Biotop6_Percent_gsub) == 'gsub(\"[()]\", \"\", df6$Biotop6_Percent)'] <- 'Biotop6_Percent_ok'

## Step 5: add lists with removed parenthesis back to df6 ----
df6_ok <- cbind(df6_Biotop1_Percent_gsub,
         df6_Biotop2_Percent_gsub,
         df6_Biotop3_Percent_gsub,
         df6_Biotop4_Percent_gsub,
         df6_Biotop5_Percent_gsub,
         df6_Biotop6_Percent_gsub,
         df6)


# Calculate percentage cover of R2_2, R2_3 nd R2_4 biotop -----------------

## Step 1: remove not wanted columns ----
df6_remove <-dplyr::select(df6_ok, -c("none2", "none3", "none4", "none5", "none6"))
df6_remove2 <-dplyr::select(df6_remove, -c("Biotop1_Percent", "Biotop2_Percent", "Biotop3_Percent", "Biotop4_Percent", "Biotop5_Percent", "Biotop6_Percent"))

## Step 2: turn it from wide format to long format ----
long_df6_3 <- reshape(df6_remove2, dir = 'long', varying = lapply(c('Percent_ok','Descrip'), grep, names(df6_remove2)), timevar = 'Order')
names(long_df6_3)[names(long_df6_3) == 'Biotop1_Percent_ok'] <- 'Perc_Cover'
names(long_df6_3)[names(long_df6_3) == 'Biotop1_Descrip'] <- 'Biotop'
names(long_df6_3)

## Step 3: calculate percentage cover of R2_2, R2_3 nd R2_4 biotop ----
long_df6_3[13] <- lapply(long_df6_3[13], as.numeric)

long_df6_3$Perc_Cover[is.na(long_df6_3$Perc_Cover)] <- 0

R2_sum <- long_df6_3 %>%
  group_by(id) %>%
  filter(Biotop == c("R2.2","R2.3","R2.4")) %>%
  summarise(R2_Perc_Cover = sum(Perc_Cover))
  
R2_merged <- merge(long_df6_3, R2_sum, all = TRUE)


# Make histogram ----------------------------------------------------------

ggplot(R2_merged, aes(R2_Perc_Cover)) +
  geom_histogram(bins = 10) +
  stat_bin(aes(y=after_stat(count), label=..count..), geom="text", vjust=-.5, bins = 10) 


library(dplyr)
df6_remove2[1:6] <- lapply(df6_remove2[1:6], as.numeric)

df6_total <- df6_remove2 %>% mutate(Total = rowSums(pick(Biotop1_Percent_ok:Biotop6_Percent_ok), na.rm = TRUE))


names(df6_remove)

df6_ok %>%
  gather(Biotop, Percentage) %>%
  mutate(Biotop = parse_number(Biotop)
         , Percentage = parse_number(Percentage))



r_biotop_count<- r_biotop %>% 
  count(BIOTOP_SEZ)

write.csv(r_biotop_count, "C:/Users/patad/PATA/UNI_MUNI/RESEARCH/PoorFenProject/R_Biotops_Counts.csv")


