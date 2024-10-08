r_biotop <- read.csv("C:/Users/patad/PATA/UNI_MUNI/RESEARCH/PoorFenProject/Selected_R2_Biotops_2.csv") 

library(dplyr)


r_biotop_count<- r_biotop %>% 
  count(BIOTOP_SEZ)

write.csv(r_biotop_count, "C:/Users/patad/PATA/UNI_MUNI/RESEARCH/PoorFenProject/R_Biotops_Counts.csv")


# Split text --------------------------------------------------------------
library(tidyr)
# separete biotops in column BIOTOP_SEZ (max 6 biotopes per cell) to separate columns
df <- r_biotop %>% separate(BIOTOP_SEZ, c("Biotop1", "Biotop2", "Biotop3", "Biotop4", "Biotop5", "Biotop6"), ",")

# separete biotop name and percentage value to separate columns
df1 <- df %>% separate(Biotop1, c("Biotop1_Descrip", "Biotop1_Percent"), " " )
df2 <- df1 %>% separate(Biotop2, c("none2", "Biotop2_Descrip", "Biotop2_Percent"), " " )# there is extra space in the first place of the cell - solve by adding column none, which will be deleted later on 
df3 <- df2 %>% separate(Biotop3, c("none3", "Biotop3_Descrip", "Biotop3_Percent"), " " )
df4 <- df3 %>% separate(Biotop4, c("none4", "Biotop4_Descrip", "Biotop4_Percent"), " " )
df5 <- df4 %>% separate(Biotop5, c("none5", "Biotop5_Descrip", "Biotop5_Percent"), " " )
df6 <- df5 %>% separate(Biotop6, c("none6", "Biotop6_Descrip", "Biotop6_Percent"), " " )

# remove parentheses from Biotop_Percent columns
df6_Biotop1_Percent_gsub <- as.data.frame(gsub("[()]", "", df6$Biotop1_Percent))
df6_Biotop2_Percent_gsub <- as.data.frame(gsub("[()]", "", df6$Biotop2_Percent))
df6_Biotop3_Percent_gsub <- as.data.frame(gsub("[()]", "", df6$Biotop3_Percent))
df6_Biotop4_Percent_gsub <- as.data.frame(gsub("[()]", "", df6$Biotop4_Percent))
df6_Biotop5_Percent_gsub <- as.data.frame(gsub("[()]", "", df6$Biotop5_Percent))
df6_Biotop6_Percent_gsub <- as.data.frame(gsub("[()]", "", df6$Biotop6_Percent))

# rename columns names
names(df6_Biotop1_Percent_gsub)[names(df6_Biotop1_Percent_gsub) == 'gsub(\"[()]\", \"\", df6$Biotop1_Percent)'] <- 'Biotop1_Percent_ok'
names(df6_Biotop2_Percent_gsub)[names(df6_Biotop2_Percent_gsub) == 'gsub(\"[()]\", \"\", df6$Biotop2_Percent)'] <- 'Biotop2_Percent_ok'
names(df6_Biotop3_Percent_gsub)[names(df6_Biotop3_Percent_gsub) == 'gsub(\"[()]\", \"\", df6$Biotop3_Percent)'] <- 'Biotop3_Percent_ok'
names(df6_Biotop4_Percent_gsub)[names(df6_Biotop4_Percent_gsub) == 'gsub(\"[()]\", \"\", df6$Biotop4_Percent)'] <- 'Biotop4_Percent_ok'
names(df6_Biotop5_Percent_gsub)[names(df6_Biotop5_Percent_gsub) == 'gsub(\"[()]\", \"\", df6$Biotop5_Percent)'] <- 'Biotop5_Percent_ok'
names(df6_Biotop6_Percent_gsub)[names(df6_Biotop6_Percent_gsub) == 'gsub(\"[()]\", \"\", df6$Biotop6_Percent)'] <- 'Biotop6_Percent_ok'

# add lists with removed parenthesis back to df6

df6_ok <- cbind(df6_Biotop1_Percent_gsub,
         df6_Biotop2_Percent_gsub,
         df6_Biotop3_Percent_gsub,
         df6_Biotop4_Percent_gsub,
         df6_Biotop5_Percent_gsub,
         df6_Biotop6_Percent_gsub,
         df6)


df1<-separate_wider_delim(
  data = df,
  cols = c(Biotop1,Biotop2),
  delim = " ",
  names = " ",
  names_sep = "_Percent",
  names_repair = "check_unique",
  cols_remove = FALSE
)

df1<-separate_wider_delim(
  data = df,
  cols = c(Biotop1,Biotop2,Biotop3,Biotop4,Biotop5,Biotop6),
  delim = " ",
  names = " ",
  names_sep = "_Percent",
  names_repair = "check_unique",
  cols_remove = FALSE
)
