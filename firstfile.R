library(readxl)
library(ggplot2)
library(purrr)
library(tidyverse, quietly = TRUE)
data_intelligence_all <- read_excel("~/Downloads/data_intelligence_all.xlsx")

#data_intelligence_all %>% mutate_all(~ replace_na(.x, 0))


data_intelligence_alldata = data.frame(data_intelligence_all)


#Attribution of intelligence (3 factors)
#Scenario: lockbox (scenarios 1-6), navigation (scenarios 7-12)
#Strategy: representation (scenarios 1, 4, 7, 10), linear search (scenarios 2, 5, 8, 11), random guess (3, 6, 9, 12)
#Embodiment: embodied (scenarios 1, 3, 5, 7, 9, 11), not embodied (scenarios 2, 4, 6, 8, 10, 12)
#Scatter/line plot: intelligence score on Y axis, strategy on X axis, 
#4 curves depicting scenario X embodiment interaction (dashed/solid line for embodied/not embodied and color1/color2 for lockbox/navigation), 
#error bars displaying Standard Error of the Mean (SEM)

lockbox1 =  mean(as.numeric(c(data_intelligence_alldata$V1_01,data_intelligence_alldata$V2_01, data_intelligence_alldata$V3_01, data_intelligence_alldata$V4_01)),  na.rm = TRUE)
lockbox1sd =  sd(as.numeric(c(data_intelligence_alldata$V1_01,data_intelligence_alldata$V2_01, data_intelligence_alldata$V3_01, data_intelligence_alldata$V4_01)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_01,data_intelligence_alldata$V2_01, data_intelligence_alldata$V3_01, data_intelligence_alldata$V4_01)))
lockbox2 =  mean(as.numeric(c(data_intelligence_alldata$V1_02,data_intelligence_alldata$V2_02, data_intelligence_alldata$V3_02, data_intelligence_alldata$V4_02)),  na.rm = TRUE)
lockbox2sd =  sd(as.numeric(c(data_intelligence_alldata$V1_02,data_intelligence_alldata$V2_02, data_intelligence_alldata$V3_02, data_intelligence_alldata$V4_02)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_02,data_intelligence_alldata$V2_02, data_intelligence_alldata$V3_02, data_intelligence_alldata$V4_02)))
lockbox3 =  mean(as.numeric(c(data_intelligence_alldata$V1_03,data_intelligence_alldata$V2_03, data_intelligence_alldata$V3_03, data_intelligence_alldata$V4_03)),  na.rm = TRUE)
lockbox3sd =  sd(as.numeric(c(data_intelligence_alldata$V1_03,data_intelligence_alldata$V2_03, data_intelligence_alldata$V3_03, data_intelligence_alldata$V4_03)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_03,data_intelligence_alldata$V2_03, data_intelligence_alldata$V3_03, data_intelligence_alldata$V4_03)))
lockbox4 =  mean(as.numeric(c(data_intelligence_alldata$V1_04,data_intelligence_alldata$V2_04, data_intelligence_alldata$V3_04, data_intelligence_alldata$V4_04)),  na.rm = TRUE)
lockbox4sd =  sd(as.numeric(c(data_intelligence_alldata$V1_04,data_intelligence_alldata$V2_04, data_intelligence_alldata$V3_04, data_intelligence_alldata$V4_04)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_04,data_intelligence_alldata$V2_04, data_intelligence_alldata$V3_04, data_intelligence_alldata$V4_04)))
lockbox5 =  mean(as.numeric(c(data_intelligence_alldata$V1_05,data_intelligence_alldata$V2_05, data_intelligence_alldata$V3_05, data_intelligence_alldata$V4_05)),  na.rm = TRUE)
lockbox5sd =  sd(as.numeric(c(data_intelligence_alldata$V1_05,data_intelligence_alldata$V2_05, data_intelligence_alldata$V3_05, data_intelligence_alldata$V4_05)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_05,data_intelligence_alldata$V2_05, data_intelligence_alldata$V3_05, data_intelligence_alldata$V4_05)))
lockbox6 =  mean(as.numeric(c(data_intelligence_alldata$V1_06,data_intelligence_alldata$V2_06, data_intelligence_alldata$V3_06, data_intelligence_alldata$V4_06)),  na.rm = TRUE)
lockbox6sd =  sd(as.numeric(c(data_intelligence_alldata$V1_06,data_intelligence_alldata$V2_06, data_intelligence_alldata$V3_06, data_intelligence_alldata$V4_06)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_06,data_intelligence_alldata$V2_06, data_intelligence_alldata$V3_06, data_intelligence_alldata$V4_06)))
lockbox7 =  mean(as.numeric(c(data_intelligence_alldata$V1_07,data_intelligence_alldata$V2_07, data_intelligence_alldata$V3_07, data_intelligence_alldata$V4_07)),  na.rm = TRUE)
lockbox7sd =  sd(as.numeric(c(data_intelligence_alldata$V1_07,data_intelligence_alldata$V2_07, data_intelligence_alldata$V3_07, data_intelligence_alldata$V4_07)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_07,data_intelligence_alldata$V2_07, data_intelligence_alldata$V3_07, data_intelligence_alldata$V4_07)))
lockbox8 =  mean(as.numeric(c(data_intelligence_alldata$V1_08,data_intelligence_alldata$V2_08, data_intelligence_alldata$V3_08, data_intelligence_alldata$V4_08)),  na.rm = TRUE)
lockbox8sd =  sd(as.numeric(c(data_intelligence_alldata$V1_08,data_intelligence_alldata$V2_08, data_intelligence_alldata$V3_08, data_intelligence_alldata$V4_08)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_08,data_intelligence_alldata$V2_08, data_intelligence_alldata$V3_08, data_intelligence_alldata$V4_08)))
lockbox9 =  mean(as.numeric(c(data_intelligence_alldata$V1_09,data_intelligence_alldata$V2_09, data_intelligence_alldata$V3_09, data_intelligence_alldata$V4_09)),  na.rm = TRUE)
lockbox9sd =  sd(as.numeric(c(data_intelligence_alldata$V1_09,data_intelligence_alldata$V2_09, data_intelligence_alldata$V3_09, data_intelligence_alldata$V4_09)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_09,data_intelligence_alldata$V2_09, data_intelligence_alldata$V3_09, data_intelligence_alldata$V4_09)))
lockbox10 =  mean(as.numeric(c(data_intelligence_alldata$V1_10,data_intelligence_alldata$V2_10, data_intelligence_alldata$V3_10, data_intelligence_alldata$V4_10)),  na.rm = TRUE)
lockbox10sd =  sd(as.numeric(c(data_intelligence_alldata$V1_10,data_intelligence_alldata$V2_10, data_intelligence_alldata$V3_10, data_intelligence_alldata$V4_10)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_10,data_intelligence_alldata$V2_10, data_intelligence_alldata$V3_10, data_intelligence_alldata$V4_10)))
lockbox11 =  mean(as.numeric(c(data_intelligence_alldata$V1_11,data_intelligence_alldata$V2_11, data_intelligence_alldata$V3_11, data_intelligence_alldata$V4_11)),  na.rm = TRUE)
lockbox11sd =  sd(as.numeric(c(data_intelligence_alldata$V1_11,data_intelligence_alldata$V2_11, data_intelligence_alldata$V3_11, data_intelligence_alldata$V4_11)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_11,data_intelligence_alldata$V2_11, data_intelligence_alldata$V3_11, data_intelligence_alldata$V4_11)))
lockbox12 =  mean(as.numeric(c(data_intelligence_alldata$V1_12,data_intelligence_alldata$V2_12, data_intelligence_alldata$V3_12, data_intelligence_alldata$V4_12)),  na.rm = TRUE)
lockbox12sd =  sd(as.numeric(c(data_intelligence_alldata$V1_12,data_intelligence_alldata$V2_12, data_intelligence_alldata$V3_12, data_intelligence_alldata$V4_12)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_12,data_intelligence_alldata$V2_12, data_intelligence_alldata$V3_12, data_intelligence_alldata$V4_12)))

# Create a first line



embodied_lockbox = c(lockbox1, lockbox3, lockbox5)
notembodied_lockbox = c(lockbox2, lockbox4, lockbox6)
embodied_navigation = c(lockbox7, lockbox9, lockbox11)
notembodied_navigation = c(lockbox8, lockbox10, lockbox12)
Strategy = c("Representation", "Linear Search", "Random Guess")

plot(1:3, embodied_lockbox , ylab="Strategies", xaxt = "none", xlab = "tenor", type="o", col="red", lty=2,ann=FALSE)+
  lines(1:3,notembodied_lockbox, type="o", col="red", ann=FALSE)+
  lines(1:3,embodied_navigation, type="o", col="blue",lty=2, ann=FALSE)+
  lines(1:3,notembodied_navigation, type="o", col="blue", ann=FALSE)
axis(side = 1, at = 1:3, labels = Strategy)
legend(x="topright", 1, legend=c("Lockbox embodied", "Lockbox not embodied", "Navigation embodied", "Navigation not embodied"),   col=c("red", "red", "blue", "blue"), lty=2:1, cex=0.8)
arrows(1, lockbox1-lockbox1sd, 1, lockbox1+lockbox1sd, length=0.05, angle=90, code=3)
arrows(1, lockbox2-lockbox2sd, 1, lockbox2+lockbox2sd, length=0.05, angle=90, code=3)
arrows(2, lockbox3-lockbox3sd, 2, lockbox3+lockbox3sd, length=0.05, angle=90, code=3)
arrows(2, lockbox4-lockbox4sd, 2, lockbox4+lockbox4sd, length=0.05, angle=90, code=3)
arrows(3, lockbox5-lockbox5sd, 3, lockbox5+lockbox5sd, length=0.05, angle=90, code=3)
arrows(3, lockbox6-lockbox6sd, 3, lockbox6+lockbox6sd, length=0.05, angle=90, code=3)
arrows(1, lockbox7-lockbox7sd, 1, lockbox7+lockbox7sd, length=0.05, angle=90, code=3)
arrows(1, lockbox8-lockbox8sd, 1, lockbox8+lockbox8sd, length=0.05, angle=90, code=3)
arrows(2, lockbox9-lockbox9sd, 2, lockbox9+lockbox9sd, length=0.05, angle=90, code=3)
arrows(2, lockbox10-lockbox10sd, 2, lockbox10+lockbox10sd, length=0.05, angle=90, code=3)
arrows(3, lockbox11-lockbox11sd, 3, lockbox11+lockbox11sd, length=0.05, angle=90, code=3)
arrows(3, lockbox12-lockbox12sd, 3, lockbox12+lockbox12sd, length=0.05, angle=90, code=3)




##################### Social Intelligence ###################
#6 items (scenarios: item1 = 13-15; item2 = 16-18; item3 = 19-21; item4 = 22-24; item5 = 25-27; item6 = 28-30)
#Presence: present (scenarios 13, 16, 19, 22, 25, 28), absent (scenarios 14, 17, 20, 23, 26, 29), neutral (scenarios 15, 18, 21, 24, 27, 30)



StandardMethodChla <- data.frame(stringsAsFactors=FALSE,
                                 Dimension = c(rep("Item 1" , 3) , rep("Item 2" , 3) , rep("Item 3" , 3) , rep("Item 4" , 3) , rep("Item 5" , 3)  , rep("Item 6" , 3)),
                                 Presence = rep(c("present" , "absent" , "neutral") , 6),
                                 IntelligenceScore= c(
                                   mean(as.numeric(c(data_intelligence_alldata$V1_13,data_intelligence_alldata$V2_13, data_intelligence_alldata$V3_13, data_intelligence_alldata$V4_13)),  na.rm = TRUE),
                                   mean(as.numeric(c(data_intelligence_alldata$V1_14,data_intelligence_alldata$V2_14, data_intelligence_alldata$V3_14, data_intelligence_alldata$V4_14)),  na.rm = TRUE),
                                   mean(as.numeric(c(data_intelligence_alldata$V1_15,data_intelligence_alldata$V2_15, data_intelligence_alldata$V3_15, data_intelligence_alldata$V4_15)),  na.rm = TRUE),
                                   mean(as.numeric(c(data_intelligence_alldata$V1_16,data_intelligence_alldata$V2_16, data_intelligence_alldata$V3_16, data_intelligence_alldata$V4_16)),  na.rm = TRUE),
                                   mean(as.numeric(c(data_intelligence_alldata$V1_17,data_intelligence_alldata$V2_17, data_intelligence_alldata$V3_17, data_intelligence_alldata$V4_17)),  na.rm = TRUE),
                                   mean(as.numeric(c(data_intelligence_alldata$V1_18,data_intelligence_alldata$V2_18, data_intelligence_alldata$V3_18, data_intelligence_alldata$V4_18)),  na.rm = TRUE),
                                   mean(as.numeric(c(data_intelligence_alldata$V1_19,data_intelligence_alldata$V2_19, data_intelligence_alldata$V3_19, data_intelligence_alldata$V4_19)),  na.rm = TRUE),
                                   mean(as.numeric(c(data_intelligence_alldata$V1_20,data_intelligence_alldata$V2_20, data_intelligence_alldata$V3_20, data_intelligence_alldata$V4_20)),  na.rm = TRUE),
                                   mean(as.numeric(c(data_intelligence_alldata$V1_21,data_intelligence_alldata$V2_21, data_intelligence_alldata$V3_21, data_intelligence_alldata$V4_21)),  na.rm = TRUE),
                                   mean(as.numeric(c(data_intelligence_alldata$V1_22,data_intelligence_alldata$V2_22, data_intelligence_alldata$V3_22, data_intelligence_alldata$V4_22)),  na.rm = TRUE),
                                   mean(as.numeric(c(data_intelligence_alldata$V1_23,data_intelligence_alldata$V2_23, data_intelligence_alldata$V3_23, data_intelligence_alldata$V4_23)),  na.rm = TRUE),
                                   mean(as.numeric(c(data_intelligence_alldata$V1_24,data_intelligence_alldata$V2_24, data_intelligence_alldata$V3_24, data_intelligence_alldata$V4_24)),  na.rm = TRUE),
                                   mean(as.numeric(c(data_intelligence_alldata$V1_25,data_intelligence_alldata$V2_25, data_intelligence_alldata$V3_25, data_intelligence_alldata$V4_25)),  na.rm = TRUE),
                                   mean(as.numeric(c(data_intelligence_alldata$V1_26,data_intelligence_alldata$V2_26, data_intelligence_alldata$V3_26, data_intelligence_alldata$V4_26)),  na.rm = TRUE),
                                   mean(as.numeric(c(data_intelligence_alldata$V1_27,data_intelligence_alldata$V2_27, data_intelligence_alldata$V3_27, data_intelligence_alldata$V4_27)),  na.rm = TRUE),
                                   mean(as.numeric(c(data_intelligence_alldata$V1_28,data_intelligence_alldata$V2_28, data_intelligence_alldata$V3_28, data_intelligence_alldata$V4_28)),  na.rm = TRUE),
                                   mean(as.numeric(c(data_intelligence_alldata$V1_29,data_intelligence_alldata$V2_29, data_intelligence_alldata$V3_29, data_intelligence_alldata$V4_29)),  na.rm = TRUE),
                                   mean(as.numeric(c(data_intelligence_alldata$V1_30,data_intelligence_alldata$V2_30, data_intelligence_alldata$V3_30, data_intelligence_alldata$V4_30)),  na.rm = TRUE)
                                 ),
                                 soc_int_sd = c(
                                   sd(as.numeric(c(data_intelligence_alldata$V1_13,data_intelligence_alldata$V2_13, data_intelligence_alldata$V3_13, data_intelligence_alldata$V4_13)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_13,data_intelligence_alldata$V2_13, data_intelligence_alldata$V3_13, data_intelligence_alldata$V4_13))),
                                   sd(as.numeric(c(data_intelligence_alldata$V1_14,data_intelligence_alldata$V2_14, data_intelligence_alldata$V3_14, data_intelligence_alldata$V4_14)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_13,data_intelligence_alldata$V2_13, data_intelligence_alldata$V3_13, data_intelligence_alldata$V4_13))),
                                   sd(as.numeric(c(data_intelligence_alldata$V1_15,data_intelligence_alldata$V2_15, data_intelligence_alldata$V3_15, data_intelligence_alldata$V4_15)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_13,data_intelligence_alldata$V2_13, data_intelligence_alldata$V3_13, data_intelligence_alldata$V4_13))),
                                   sd(as.numeric(c(data_intelligence_alldata$V1_16,data_intelligence_alldata$V2_16, data_intelligence_alldata$V3_16, data_intelligence_alldata$V4_16)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_13,data_intelligence_alldata$V2_13, data_intelligence_alldata$V3_13, data_intelligence_alldata$V4_13))),
                                   sd(as.numeric(c(data_intelligence_alldata$V1_17,data_intelligence_alldata$V2_17, data_intelligence_alldata$V3_17, data_intelligence_alldata$V4_17)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_13,data_intelligence_alldata$V2_13, data_intelligence_alldata$V3_13, data_intelligence_alldata$V4_13))),
                                   sd(as.numeric(c(data_intelligence_alldata$V1_18,data_intelligence_alldata$V2_18, data_intelligence_alldata$V3_18, data_intelligence_alldata$V4_18)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_13,data_intelligence_alldata$V2_13, data_intelligence_alldata$V3_13, data_intelligence_alldata$V4_13))),
                                   sd(as.numeric(c(data_intelligence_alldata$V1_19,data_intelligence_alldata$V2_19, data_intelligence_alldata$V3_19, data_intelligence_alldata$V4_19)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_13,data_intelligence_alldata$V2_13, data_intelligence_alldata$V3_13, data_intelligence_alldata$V4_13))),
                                   sd(as.numeric(c(data_intelligence_alldata$V1_20,data_intelligence_alldata$V2_20, data_intelligence_alldata$V3_20, data_intelligence_alldata$V4_20)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_13,data_intelligence_alldata$V2_13, data_intelligence_alldata$V3_13, data_intelligence_alldata$V4_13))),
                                   sd(as.numeric(c(data_intelligence_alldata$V1_21,data_intelligence_alldata$V2_21, data_intelligence_alldata$V3_21, data_intelligence_alldata$V4_21)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_13,data_intelligence_alldata$V2_13, data_intelligence_alldata$V3_13, data_intelligence_alldata$V4_13))),
                                   sd(as.numeric(c(data_intelligence_alldata$V1_22,data_intelligence_alldata$V2_22, data_intelligence_alldata$V3_22, data_intelligence_alldata$V4_22)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_13,data_intelligence_alldata$V2_13, data_intelligence_alldata$V3_13, data_intelligence_alldata$V4_13))),
                                   sd(as.numeric(c(data_intelligence_alldata$V1_23,data_intelligence_alldata$V2_23, data_intelligence_alldata$V3_23, data_intelligence_alldata$V4_23)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_13,data_intelligence_alldata$V2_13, data_intelligence_alldata$V3_13, data_intelligence_alldata$V4_13))),
                                   sd(as.numeric(c(data_intelligence_alldata$V1_24,data_intelligence_alldata$V2_24, data_intelligence_alldata$V3_24, data_intelligence_alldata$V4_24)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_13,data_intelligence_alldata$V2_13, data_intelligence_alldata$V3_13, data_intelligence_alldata$V4_13))),
                                   sd(as.numeric(c(data_intelligence_alldata$V1_25,data_intelligence_alldata$V2_25, data_intelligence_alldata$V3_25, data_intelligence_alldata$V4_25)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_13,data_intelligence_alldata$V2_13, data_intelligence_alldata$V3_13, data_intelligence_alldata$V4_13))),
                                   sd(as.numeric(c(data_intelligence_alldata$V1_26,data_intelligence_alldata$V2_26, data_intelligence_alldata$V3_26, data_intelligence_alldata$V4_26)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_13,data_intelligence_alldata$V2_13, data_intelligence_alldata$V3_13, data_intelligence_alldata$V4_13))),
                                   sd(as.numeric(c(data_intelligence_alldata$V1_27,data_intelligence_alldata$V2_27, data_intelligence_alldata$V3_27, data_intelligence_alldata$V4_27)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_13,data_intelligence_alldata$V2_13, data_intelligence_alldata$V3_13, data_intelligence_alldata$V4_13))),
                                   sd(as.numeric(c(data_intelligence_alldata$V1_28,data_intelligence_alldata$V2_28, data_intelligence_alldata$V3_28, data_intelligence_alldata$V4_28)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_13,data_intelligence_alldata$V2_13, data_intelligence_alldata$V3_13, data_intelligence_alldata$V4_13))),
                                   sd(as.numeric(c(data_intelligence_alldata$V1_29,data_intelligence_alldata$V2_29, data_intelligence_alldata$V3_29, data_intelligence_alldata$V4_29)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_13,data_intelligence_alldata$V2_13, data_intelligence_alldata$V3_13, data_intelligence_alldata$V4_13))),
                                   sd(as.numeric(c(data_intelligence_alldata$V1_30,data_intelligence_alldata$V2_30, data_intelligence_alldata$V3_30, data_intelligence_alldata$V4_30)),  na.rm = TRUE)/sqrt(length(c(data_intelligence_alldata$V1_13,data_intelligence_alldata$V2_13, data_intelligence_alldata$V3_13, data_intelligence_alldata$V4_13)))
                                 )
)

StandardMethodChla %>% 
  group_by(Presence) %>% 
  mutate(se = sd(IntelligenceScore)/sqrt(length(IntelligenceScore))) %>% 
  ggplot(aes(x = Dimension, y = IntelligenceScore , fill = Presence)) + 
  labs(y="Intelligence Score")+
  geom_bar(stat="identity", alpha=0.5, 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=IntelligenceScore-soc_int_sd, ymax=IntelligenceScore+soc_int_sd), width=.2, colour="black", 
                position=position_dodge(.9))


##############################Demographics###############################################################

##Education#[EN06]

education_intelligence_score <- function(int_table_education){
  sum_list1 = list()
  catch <- grep("^V" , names(data_intelligence_alldata))
  sumi <- 0
  do_append <- FALSE
  for(i in 1:length(data_intelligence_alldata)){
    sumi <- 0
    do_append <- FALSE
    for(j in 1:length(catch)){
      if(catch[j] == i){
        listo <- int_table_education[, catch[j]]
        listo <- as.numeric(listo)
        meanu <- mean(listo[!is.na(listo)], na.rm = TRUE)
        sumi <- sumi + sum(listo[!is.na(listo)], na.rm = TRUE)
        mean_length = length(which(!is.na(listo)))
        do_append <- TRUE
      }
    }
    
    if(do_append){
      sum_list1 = c(sum_list1, list(meanu))
    }
  } 
  sum_list1 <- as.numeric(sum_list1)
  result <-  mean(sum_list1[!is.na(sum_list1)], na.rm = TRUE)
  semi <- sd(sum_list1[!is.na(sum_list1)], na.rm = TRUE)/sqrt(length(sum_list1[!is.na(sum_list1)]))
  result_data = data.frame(Mean = result, SEM = semi)
  return(result_data)
  
}
get_value_education_list <- function(){
  education_list = list(intelligence_score_education_1 <-data_intelligence_alldata[data_intelligence_alldata[["EN06"]]==1,],
                        intelligence_score_education_2 <-data_intelligence_alldata[data_intelligence_alldata[["EN06"]]==2,],
                        intelligence_score_education_3 <-data_intelligence_alldata[data_intelligence_alldata[["EN06"]]==3,],
                        intelligence_score_education_4 <-data_intelligence_alldata[data_intelligence_alldata[["EN06"]]==4,],
                        intelligence_score_education_5 <-data_intelligence_alldata[data_intelligence_alldata[["EN06"]]==5,],
                        intelligence_score_education_6 <-data_intelligence_alldata[data_intelligence_alldata[["EN06"]]==6,],
                        intelligence_score_education_7 <-data_intelligence_alldata[data_intelligence_alldata[["EN06"]]==7,]
  )
  
  
  sem_list = list()
  mean_list = list()
  for(item in education_list){
    value_education = education_intelligence_score(item)
    mean_list = c(mean_list, value_education$Mean)
    sem_list = c(sem_list, value_education$SEM)
  }
  mean_list <- as.numeric(mean_list)
  sem_list <- as.numeric(sem_list)
  res_frame <-data.frame(Mean = mean_list, SEM = sem_list)
  return(res_frame)
}

value_education_frame = get_value_education_list()
value_education = value_education_frame$Mean
print(value_education)
sem_education = value_education_frame$SEM
diff_education = value_education-sem_education
diffo_education = value_education+sem_education

my.labels <- c("High school \n or equivalent","Apprenticeship, \n technical \n or occupational certificate","Bachelor’s degree","Master’s degree","PhD","Other","I prefer not to answer") # first create labels, add \n where appropriate.
# = get_value_education_list()
data_education <- data.frame(
  Education=c("High school or equivalent","Apprenticeship/technical or occupational certificate","Bachelor’s degree","Master’s degree","PhD","Other","I prefer not to answer") ,  
yukj=value_education
)
ggplot(data_education, aes(x=Education, y=yukj)) + ylim(0,6)+
  geom_bar(stat = "identity", fill="light green")+  scale_x_discrete(labels= my.labels)+
  geom_errorbar(aes(ymin=diff_education, ymax=diffo_education), width=.2, colour="black", 
                position=position_dodge(.9))

###Gender##[EN04]


gender_intelligence_score <- function(int_table_age){
  
  sum_list1 = list()
  catch <- grep("^V" , names(data_intelligence_alldata))
  sumi <- 0
  do_append <- FALSE
  for(i in 1:length(data_intelligence_alldata)){
    sumi <- 0
    do_append <- FALSE
    for(j in 1:length(catch)){
      if(catch[j] == i){
        mean_count <- mean_count + 1
        listo <- int_table_age[, catch[j]]
        listo <- as.numeric(listo)
        meanu <- mean(listo[!is.na(listo)], na.rm = TRUE)
        sumi <- sumi + sum(listo[!is.na(listo)], na.rm = TRUE)
        mean_length = length(which(!is.na(listo)))
        do_append <- TRUE
      }
    }
    
    if(do_append){
      sum_list1 = c(sum_list1, list(meanu))
      #print(sum_list1)
    }
  } 
  #print(sum_list1)
  sum_list1 <- as.numeric(sum_list1)
  result <-  mean(sum_list1[!is.na(sum_list1)], na.rm = TRUE)
  semi <- sd(sum_list1[!is.na(sum_list1)], na.rm = TRUE)/sqrt(length(sum_list1[!is.na(sum_list1)]))
  
  result_data = data.frame(Mean = result, SEM = semi)
  return(result_data)
  
}
get_value_gender_list <- function(){
  gender_list = list(intelligence_score_gender_1 <-data_intelligence_alldata[data_intelligence_alldata[["EN04"]]==1,],
                     intelligence_score_gender_2 <-data_intelligence_alldata[data_intelligence_alldata[["EN04"]]==2,],
                     intelligence_score_gender_3 <-data_intelligence_alldata[data_intelligence_alldata[["EN04"]]==3,],
                     intelligence_score_gender_4 <-data_intelligence_alldata[data_intelligence_alldata[["EN04"]]==4,]
  )
  
  
  mean_list = list()
  sem_list = list()
  for(item in gender_list){
    value_gender = gender_intelligence_score(item)
    mean_list = c(mean_list, value_gender$Mean)
    sem_list = c(sem_list, value_gender$SEM)
  }
  mean_list <- as.numeric(mean_list)
  sem_list <- as.numeric(sem_list)
  res_frame <-data.frame(Mean = mean_list, SEM = sem_list)
  return(res_frame)
}

value_gender_frame = get_value_gender_list()
value_gender = value_gender_frame$Mean
sem_gender = value_gender_frame$SEM
diff = value_gender-sem_gender
diffo = value_gender+sem_gender

data_gender <- data.frame(
  Gender=c("female","male","diverse","I prefer not to answer") ,  
  Value=value_gender
)

ggplot(data_gender, aes(x=Gender, y=Value)) +ylim(0,6)+ 
  geom_bar(stat = "identity", fill = "#FF6666")+geom_errorbar(aes(ymin=diff, ymax=diffo), width=.2, colour="black", 
                                                              position=position_dodge(.9))

###Age##[EN03]

age_intelligence_score <- function(int_table_age){
  
  sum_list1 = list()
  #print(length(data_intelligence_alldata))
  catch <- grep("^V" , names(data_intelligence_alldata))
  sumi <- 0
  mean_count <- 0
  do_append <- FALSE
  for(i in 1:length(data_intelligence_alldata)){
    sumi <- 0
    do_append <- FALSE
    for(j in 1:length(catch)){
      #sumi <- 0
      if(catch[j] == i){
        # print(j)
        mean_count <- mean_count + 1
        listo <- int_table_age[, catch[j]]
        #print(intelligence_score_gender_female_1[, catch[j]])
        listo <- as.numeric(listo)
        #print(sum(listo[!is.na(listo)], na.rm = TRUE))
        meanu <- mean(listo[!is.na(listo)], na.rm = TRUE)
        sumi <- sumi + sum(listo[!is.na(listo)], na.rm = TRUE)
        mean_length = length(which(!is.na(listo)))
        do_append <- TRUE
      }
    }
    
    if(do_append){
      sum_list1 = c(sum_list1, list(meanu))
      #print(sum_list1)
    }
  } 
  #print(sum_list1)
  sum_list1 <- as.numeric(sum_list1)
  result <-  mean(sum_list1[!is.na(sum_list1)], na.rm = TRUE)
  semi <- sd(sum_list1[!is.na(sum_list1)], na.rm = TRUE)/sqrt(length(sum_list1[!is.na(sum_list1)]))
  result_data = data.frame(Mean = result, SEM = semi)
  return(result_data)
  
}
get_value_age_list <- function(){
  age_list = list(intelligence_score_age_1 <-data_intelligence_alldata[data_intelligence_alldata[["EN03"]]==1,],
                  intelligence_score_age_2 <-data_intelligence_alldata[data_intelligence_alldata[["EN03"]]==2,],
                  intelligence_score_age_3 <-data_intelligence_alldata[data_intelligence_alldata[["EN03"]]==3,],
                  intelligence_score_age_4 <-data_intelligence_alldata[data_intelligence_alldata[["EN03"]]==4,],
                  intelligence_score_age_5 <-data_intelligence_alldata[data_intelligence_alldata[["EN03"]]==5,],
                  intelligence_score_age_6 <-data_intelligence_alldata[data_intelligence_alldata[["EN03"]]==6,]
                  #intelligence_score_age_7 <-data_intelligence_alldata[data_intelligence_alldata[["EN03"]]==7,],
                  #intelligence_score_age_8 <-data_intelligence_alldata[data_intelligence_alldata[["EN03"]]==8,]
                  #intelligence_score_age_6 <-data_intelligence_alldata[data_intelligence_alldata[["EN03"]]==9,]
  )
  
  mean_list = list()
  sem_list = list()
  for(item in age_list){

    value_age = age_intelligence_score(item)
    
    mean_list = c(mean_list, value_age$Mean)
    mean_list <- c(mean_list)
    sem_list = c(sem_list, value_age$SEM)
    sem_list = c(sem_list)
    print(sem_list)
  }

  mean_list <- as.numeric(c(mean_list,list(0, 0, 0)))
  sem_list <- as.numeric(c(sem_list, list(0, 0, 0)))
  res_frame <-data.frame(Mean = mean_list, SEM = sem_list)
  return(res_frame)
}

value_age_frame = get_value_age_list()
value_age = value_age_frame$Mean
sem_age = value_age_frame$SEM
diff_age = value_age-sem_age
diffo_age = value_age+sem_age
data_age <- data.frame(
  Age=c("[18-25]","[26-35]","[36-45]","[46-55]","[56-65]",
        "[66-75]","[76-85]", "[85+]", "I prefer not to answer") ,
  Value= value_age
)
ggplot(data_age, aes(x=Age, y=Value)) +
  ylim(0,6)+
  geom_bar(stat = "identity",fill = "light blue")+
  geom_errorbar(aes(ymin=diff_age, ymax=diffo_age), width=.2, colour="black", 
                position=position_dodge(.9))


