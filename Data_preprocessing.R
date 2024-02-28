####PREPARATION

#Install libraries
libraries <- c("plyr", "MASS", "haven", "stringr", "tidyverse", "dplyr", 
               "tibble", "stats", "vtable", "ppcor", "rio", "stringi", 
               "tidyr", "lubridate", "ggplot2", "pscl", "boot", "epiDisplay", 
               "DescTools", "pastecs", "qwraps2", "sjmisc", "ggpubr", "effsize", 
               "fitdistrplus")

to_install <- libraries[!libraries %in% installed.packages()]

if (length(to_install) > 0) {
    install.packages(to_install)
} else {
    print("All required packages are already installed.")
}



#Load libraries
library(plyr)
library(MASS)
library(haven)
library(stringr)
library(tidyverse)
library(dplyr)
library(tibble)
library(stats)
library(vtable)
library(ppcor)
library(rio)
library(stringi)
library(tidyr)
library(lubridate)
library(ggplot2)
library(pscl)
library(boot)
library(lattice)
library(foreign)
library(survival)
library(MASS)
library(nnet)
library(epiDisplay)
library(DescTools)
library(pastecs)
library(dplyr)
library(qwraps2)
library(sjmisc)
library(ggpubr)
library(effsize)
library(rstatix)
library(fitdistrplus)

####DATA PREPROCESSING

#Import survey data and paradata

convert("GGP2021_WAVE1_EE_EST_V_0_0.dta", "dt_survey_EST.csv")
convert("GGP2021_WAVE1_EE_RUS_V_0_0.dta", "dt_survey_RUS.csv")
convert("Paradata_EST.dta", "prdt_EST.csv")
convert("Paradata_RUS.dta", "prdt_RUS.csv")
prdt_rus<- read_dta(file = "Paradata_RUS.dta")
prdt_est<- read_dta(file = "Paradata_EST.dta")
Paradata_EST <- import("Paradata_EST.dta")
paradata_est <- read_csv("paradata_est.csv")


#Get True/False where "previous" appears

Paradata_RUS$previous <- str_detect(Paradata_RUS$v4, "PreviousPage()")
paradata_est$previous <- str_detect(paradata_est$v4, "PreviousPage()")


#Make a subset of only actions paradata
paradata_est$updatepage <- str_detect(paradata_est$v4, "UpdatePageEvent")
paradata_est_onlyactions <- subset(paradata_est, updatepage!="TRUE")


#Filter by respid

para_rusfltr <- dplyr::semi_join(Paradata_RUS, dt_survey_RUS, "respid")
length(unique(para_rusfltr$respid))
para_estfltr <- dplyr::semi_join(paradata_est, dt_survey_EST, "respid")
length(unique(para_estfltr$respid))

table(para_rusfltr$previous)
table(para_estfltr$previous)
mean(para_rusfltr$previous)
mean(para_estfltr$previous)

#Get mean proportion of "previous" actions

para_rusfltr_aggr <- aggregate(para_rusfltr, by=list(para_rusfltr$respid),FUN=mean, na.rm=TRUE)
para_estfltr_aggr <- aggregate(para_estfltr, by=list(para_estfltr$respid),FUN=mean, na.rm=TRUE)
summary(para_rusfltr_aggr$previous)
summary(para_estfltr_aggr$previous)

#Number of respondents with at least one "previous"

nrow(para_rusfltr_aggr[para_rusfltr_aggr$previous > 0,])/nrow(para_rusfltr_aggr)
nrow(para_estfltr_aggr[para_estfltr_aggr$previous > 0,])/nrow(para_estfltr_aggr)

#Order paradata by respid, time and order variables

para_rusfltr_ordered <- para_rusfltr[
    with(para_rusfltr, order(para_rusfltr$respid, para_rusfltr$v3, para_rusfltr$order)),
]
para_estfltr_ordered <- para_estfltr[
    with(para_estfltr, order(para_estfltr$respid, para_estfltr$v3, para_estfltr$order)),
] 

#For each UpdatePage event get the page number

para_rusfltr_ordered$pagenum <-  stri_sub(para_rusfltr_ordered$v4, -16)
para_rusfltr_ordered$pagenum <-  extract_numeric(para_rusfltr_ordered$pagenum)
para_estfltr_ordered$pagenum <-  stri_sub(para_estfltr_ordered$v4, -16)
para_estfltr_ordered$pagenum <-  extract_numeric(para_estfltr_ordered$pagenum)


##Drop rows with StartSession (StartSessionEvent)
para_rusfltr_ordered$startsession <- str_detect(para_rusfltr_ordered$v4, "StartSessionEvent")
para_rusfltr_ordered_1 <-para_rusfltr_ordered[(para_rusfltr_ordered$startsession=="FALSE"),]

para_estfltr_ordered$startsession <- str_detect(para_estfltr_ordered$v4, "StartSessionEvent")
para_estfltr_ordered_1 <-para_estfltr_ordered[(para_estfltr_ordered$startsession=="FALSE"),]

#Order paradata by respid, time and order variables

para_rusfltr_ordered_fin <- para_rusfltr_ordered_1[
    with(para_rusfltr_ordered_1, order(para_rusfltr_ordered_1$respid, para_rusfltr_ordered_1$v3, para_rusfltr_ordered_1$order)),
]
para_estfltr_ordered_fin <- para_estfltr_ordered_1[
    with(para_estfltr_ordered_1, order(para_estfltr_ordered_1$respid, para_estfltr_ordered_1$v3, para_estfltr_ordered_1$order)),
] 

#Adding three variables (from which page (previous - 1) "before", to which page (previous + 1) "to", page after next (previous + 3) "after")

para_rusfltr_ordered_fin$before <- lag(para_rusfltr_ordered_fin$pagenum, 1)
para_estfltr_ordered_fin$before <- lag(para_estfltr_ordered_fin$pagenum, 1) 

para_rusfltr_ordered_fin$to <- lead(para_rusfltr_ordered_fin$pagenum, 1)
para_estfltr_ordered_fin$to <- lead(para_estfltr_ordered_fin$pagenum, 1)

para_rusfltr_ordered_fin$after <- lead(para_rusfltr_ordered_fin$pagenum, 3)
para_estfltr_ordered_fin$after <- lead(para_estfltr_ordered_fin$pagenum, 3)


#Computing a difference between before and after?

para_rusfltr_ordered_fin$difference <- as.numeric(para_rusfltr_ordered_fin$after) - as.numeric(para_rusfltr_ordered_fin$before)
para_estfltr_ordered_fin$difference <- as.numeric(para_estfltr_ordered_fin$after) - as.numeric(para_estfltr_ordered_fin$before)

para_rusfltr_ordered_fin$diff_present <- (para_rusfltr_ordered_fin$after != para_rusfltr_ordered_fin$before)
para_estfltr_ordered_fin$diff_present <- (para_estfltr_ordered_fin$after != para_estfltr_ordered_fin$before)



#Add indication of pages with immediate filters, non-immediate filters, looping questions
filter_pageIndex <-c(7, 13, 16, 26, 29, 34, 38, 40, 43, 44, 47, 48, 53, 54, 55, 56, 61, 64, 65, 66, 67, 69, 340, 
                     341, 343, 348, 359, 360, 5, 28, 731, 809, 811, 813, 817, 816, 827, 829, 830, 837, 839, 840, 
                     843, 844, 846, 848, 848, 852, 857, 860, 864, 865, 866, 868, 870, 873, 12, 75, 78, 79, 80, 81, 
                     83, 89, 92, 93, 94, 95, 97, 103, 106, 107, 108, 109, 111, 117, 120, 121, 122, 123, 125, 131, 134, 
                     135, 136, 137, 139, 145, 148, 149, 150, 151, 153, 159, 162, 163, 164, 165, 167, 173, 176, 177, 178, 
                     179, 181, 187, 190, 191, 192, 193, 195, 201, 204, 205, 206, 207, 209, 215, 218, 219, 220, 221, 223, 
                     229, 232, 233, 234, 235, 237, 243, 246, 247, 249, 248, 251, 257, 260, 261, 262, 263, 265, 271, 274, 
                     275, 276, 277, 279, 285, 288, 289, 290, 291, 293, 299, 302, 303, 304, 305, 307, 313, 316, 317, 318, 
                     319, 321, 327, 330, 331, 332, 333, 335, 361, 370, 378, 379, 379, 388, 396, 397, 397, 406, 414, 415, 
                     415, 424, 432, 433, 433, 442, 450, 451, 451, 460, 468, 469, 469, 478, 486, 487, 487, 496, 504, 505, 
                     514, 505, 522, 523, 523, 532, 540, 541, 541, 550, 558, 559, 559, 568, 576, 577, 577, 586, 594, 595, 
                     595, 604, 612, 613, 613, 622, 630, 631, 631, 640, 648, 649, 649, 658, 666, 667, 667, 676, 684, 685,
                     685, 694, 702, 703, 9, 52, 36, 57, 352, 364, 366, 367, 707, 718, 719, 721, 723, 801, 805, 835, 842, 
                     853, 872, 882, 886, 895, 893, 903, 914, 919, 921, 927, 936, 952, 63, 902, 897, 941, 956, 1, 908, 931, 
                     888, 861, 715, 725, 885, 727, 39, 706, 363, 962, 963, 965, 706, 717, 711, 708)
loop_pageIndex <- c(56, 342, 342, 342, 731, 940)
filter_or_loop_pageIndex <- c (7, 13, 16, 26, 29, 34, 38, 40, 43, 44, 47, 48, 53, 54, 55, 56, 61, 64, 65, 66, 67, 69, 340, 
                               341, 343, 348, 359, 360, 5, 28, 731, 809, 811, 813, 817, 816, 827, 829, 830, 837, 839, 840, 
                               843, 844, 846, 848, 848, 852, 857, 860, 864, 865, 866, 868, 870, 873, 12, 75, 78, 79, 80, 81, 
                               83, 89, 92, 93, 94, 95, 97, 103, 106, 107, 108, 109, 111, 117, 120, 121, 122, 123, 125, 131, 134, 
                               135, 136, 137, 139, 145, 148, 149, 150, 151, 153, 159, 162, 163, 164, 165, 167, 173, 176, 177, 178, 
                               179, 181, 187, 190, 191, 192, 193, 195, 201, 204, 205, 206, 207, 209, 215, 218, 219, 220, 221, 223, 
                               229, 232, 233, 234, 235, 237, 243, 246, 247, 249, 248, 251, 257, 260, 261, 262, 263, 265, 271, 274, 
                               275, 276, 277, 279, 285, 288, 289, 290, 291, 293, 299, 302, 303, 304, 305, 307, 313, 316, 317, 318, 
                               319, 321, 327, 330, 331, 332, 333, 335, 361, 370, 378, 379, 379, 388, 396, 397, 397, 406, 414, 415, 
                               415, 424, 432, 433, 433, 442, 450, 451, 451, 460, 468, 469, 469, 478, 486, 487, 487, 496, 504, 505, 
                               514, 505, 522, 523, 523, 532, 540, 541, 541, 550, 558, 559, 559, 568, 576, 577, 577, 586, 594, 595, 
                               595, 604, 612, 613, 613, 622, 630, 631, 631, 640, 648, 649, 649, 658, 666, 667, 667, 676, 684, 685,
                               685, 694, 702, 703, 9, 52, 36, 57, 352, 364, 366, 367, 707, 718, 719, 721, 723, 801, 805, 835, 842, 
                               853, 872, 882, 886, 895, 893, 903, 914, 919, 921, 927, 936, 952, 56, 342, 342, 342, 731, 940, 63, 902, 
                               897, 941, 956, 1, 908, 931, 888, 861, 715, 725, 885, 727, 39, 706, 363, 962, 963, 965, 706, 717, 711, 708)
immediate_filters_pageIndex <- c(7, 13, 26, 29, 34, 38, 40, 43, 44, 47, 48, 53, 54, 55, 56, 61, 64, 65, 66, 69, 340, 341, 348, 359, 
                                 360, 731, 809, 811, 813, 827, 829, 830, 837, 839, 843, 840, 846, 848, 852, 857, 860, 864, 865, 866, 
                                 868, 870, 873, 75, 78, 79, 80, 83, 89, 92, 93, 94, 97, 103, 106, 107, 108, 111, 117, 120, 121, 122, 
                                 125, 131, 134, 135, 136, 139, 145, 148, 149, 150, 153, 159, 162, 163, 164, 167, 173, 176, 177, 178, 
                                 181, 187, 190, 191, 192, 195, 201, 204, 205, 206, 209, 215, 218, 219, 220, 223, 229, 232, 233, 234, 
                                 237, 243, 246, 247, 248, 251, 257, 260, 261, 262, 265, 271, 274, 275, 276, 279, 285, 288, 289, 290, 
                                 293, 299, 302, 303, 304, 307, 316, 313, 317, 318, 321, 327, 330, 331, 332, 335, 370, 378, 379, 388, 
                                 396, 397, 406, 414, 415, 424, 432, 433, 442, 450, 451, 460, 468, 469, 478, 486, 487, 496, 504, 505, 
                                 514, 522, 523, 532, 540, 541, 550, 558, 559, 568, 576, 577, 586, 594, 595, 604, 612, 613, 622, 630, 
                                 631, 640, 648, 649, 658, 666, 667, 676, 684, 685, 694, 702, 703, 9, 36, 52, 57, 364, 352, 366, 367, 
                                 707, 718, 719, 721, 723, 801, 805, 835, 842, 853, 872, 882, 886, 893, 895, 903, 914, 919, 921, 927, 
                                 936, 952, 63, 902, 897, 941, 956, 1, 908, 931, 888, 861, 715, 725, 885, 727, 39, 706, 363, 962, 963, 
                                 965, 706, 717, 711, 708)
nonimmediate_filters_pageIndex <- c(16, 67, 343, 5, 28, 816, 817, 844, 848, 12, 81, 95, 109, 123, 137, 151, 
                                    165, 179, 193, 207, 221, 235, 249, 263, 277, 291, 305, 319, 333, 361, 
                                    379, 397, 415, 433, 469, 451, 487, 505, 523, 541, 559, 577, 595, 613, 
                                    631, 649, 667, 685)


para_rusfltr_ordered_fin %>%
    mutate(to_loop = ifelse(to %in% loop_pageIndex, 'TRUE', 'FALSE')) -> para_rusfltr_ordered_fin

para_rusfltr_ordered_fin %>%
    mutate(to_filter = ifelse(to %in% filter_pageIndex,'TRUE', 'FALSE')) -> para_rusfltr_ordered_fin

para_rusfltr_ordered_fin %>%
    mutate(to_filter_or_loop = ifelse(to %in% filter_or_loop_pageIndex, 'TRUE', 'FALSE')) -> para_rusfltr_ordered_fin

para_rusfltr_ordered_fin %>%
    mutate(to_immediate_filter = ifelse(to %in% immediate_filters_pageIndex, 'TRUE', 'FALSE')) -> para_rusfltr_ordered_fin

para_rusfltr_ordered_fin %>%
    mutate(to_nonimmediate_filter = ifelse(to %in% nonimmediate_filters_pageIndex, 'TRUE', 'FALSE')) -> para_rusfltr_ordered_fin

para_rusfltr_ordered_fin %>%
    mutate(after_same = ifelse(previous == 'TRUE', ifelse (difference == 0, 'TRUE', 'FALSE'), 'NoPrev')) -> para_rusfltr_ordered_fin

para_rusfltr_ordered_fin %>%
    mutate(after_later = ifelse(previous == 'TRUE', ifelse(difference > 0, 'TRUE', 'FALSE'), 'NoPrev')) -> para_rusfltr_ordered_fin

para_rusfltr_ordered_fin %>%
    mutate(after_earlier = ifelse(previous == 'TRUE', ifelse(difference < 0, 'TRUE', 'FALSE'), 'NoPrev')) -> para_rusfltr_ordered_fin

para_rusfltr_ordered_fin %>%
    mutate(to_regular_1 = ifelse(to %in% filter_or_loop_pageIndex, 'FALSE', 'TRUE')) -> para_rusfltr_ordered_fin

para_estfltr_ordered_fin %>%
    mutate(to_loop = ifelse(previous == 'TRUE', ifelse(to %in% loop_pageIndex, 'TRUE', 'FALSE'), 'NoPrev')) -> para_estfltr_ordered_fin

para_estfltr_ordered_fin %>%
    mutate(to_filter = ifelse(to %in% filter_pageIndex,'TRUE', 'FALSE')) -> para_estfltr_ordered_fin

para_estfltr_ordered_fin %>%
    mutate(to_filter_or_loop = ifelse(to %in% filter_or_loop_pageIndex, 'TRUE', 'FALSE')) -> para_estfltr_ordered_fin

para_estfltr_ordered_fin %>%
    mutate(to_immediate_filter = ifelse(previous == 'TRUE', ifelse(to %in% immediate_filters_pageIndex, 'TRUE', 'FALSE'), 'NoPrev')) -> para_estfltr_ordered_fin

para_estfltr_ordered_fin %>%
    mutate(to_nonimmediate_filter = ifelse(previous == 'TRUE', ifelse(to %in% nonimmediate_filters_pageIndex, 'TRUE', 'FALSE'), 'NoPrev')) -> para_estfltr_ordered_fin

para_estfltr_ordered_fin %>%
    mutate(after_same = ifelse(previous == 'TRUE', ifelse(difference == 0, 'TRUE', 'FALSE'), 'NoPrev')) -> para_estfltr_ordered_fin

para_estfltr_ordered_fin %>%
    mutate(after_later = ifelse(previous == 'TRUE', ifelse(difference > 0, 'TRUE', 'FALSE'), 'NoPrev')) -> para_estfltr_ordered_fin

para_estfltr_ordered_fin %>%
    mutate(after_earlier = ifelse(previous == 'TRUE', ifelse(difference < 0, 'TRUE', 'FALSE'), 'NoPrev')) -> para_estfltr_ordered_fin

para_estfltr_ordered_fin %>%
    mutate(to_regular_2 = ifelse(previous == 'TRUE', ifelse(to %in% filter_or_loop_pageIndex, 'FALSE', 'TRUE'), 'NoPrev')) -> para_estfltr_ordered_fin

##Adding indication of filters/loops without consistent going backs

para_rusfltr_ordered_fin %>%
    mutate(to_loop_withoutback = ifelse(after_earlier == 'FALSE', ifelse(to %in% loop_pageIndex, 'TRUE', 'FALSE'), 'FALSE')) -> para_rusfltr_ordered_fin

para_rusfltr_ordered_fin %>%
    mutate(to_filter_withoutback = ifelse(after_earlier == 'FALSE', ifelse(to %in% filter_pageIndex,'TRUE', 'FALSE'), 'FALSE')) -> para_rusfltr_ordered_fin

para_rusfltr_ordered_fin %>%
    mutate(to_filter_or_loop_withoutback = ifelse(after_earlier == 'FALSE', ifelse(to %in% filter_or_loop_pageIndex, 'TRUE', 'FALSE'), 'FALSE')) -> para_rusfltr_ordered_fin

para_rusfltr_ordered_fin %>%
    mutate(to_immediate_filter_withoutback = ifelse(after_earlier == 'FALSE', ifelse(to %in% immediate_filters_pageIndex, 'TRUE', 'FALSE'), 'FALSE')) -> para_rusfltr_ordered_fin

para_rusfltr_ordered_fin %>%
    mutate(to_nonimmediate_filter_withoutback = ifelse(after_earlier == 'FALSE', ifelse(to %in% nonimmediate_filters_pageIndex, 'TRUE', 'FALSE'), 'FALSE')) -> para_rusfltr_ordered_fin


para_estfltr_ordered_fin %>%
    mutate(to_loop_withoutback = ifelse(after_earlier == 'FALSE', ifelse(to %in% loop_pageIndex, 'TRUE', 'FALSE'), 'FALSE')) -> para_estfltr_ordered_fin

para_estfltr_ordered_fin %>%
    mutate(to_filter_withoutback = ifelse(after_earlier == 'FALSE', ifelse(to %in% filter_pageIndex,'TRUE', 'FALSE'), 'FALSE')) -> para_estfltr_ordered_fin

para_estfltr_ordered_fin %>%
    mutate(to_filter_or_loop_withoutback = ifelse(after_earlier == 'FALSE', ifelse(to %in% filter_or_loop_pageIndex, 'TRUE', 'FALSE'), 'FALSE')) -> para_estfltr_ordered_fin

para_estfltr_ordered_fin %>%
    mutate(to_immediate_filter_withoutback = ifelse(after_earlier == 'FALSE', ifelse(to %in% immediate_filters_pageIndex, 'TRUE', 'FALSE'), 'FALSE')) -> para_estfltr_ordered_fin

para_estfltr_ordered_fin %>%
    mutate(to_nonimmediate_filter_withoutback = ifelse(after_earlier == 'FALSE', ifelse(to %in% nonimmediate_filters_pageIndex, 'TRUE', 'FALSE'), 'FALSE')) -> para_estfltr_ordered_fin


##Adding count of going back to filters/loops/either to survey dataset

counts_to_loop <- para_rusfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_loop = sum(str_count(to_loop, pattern = 'TRUE')))

dt_survey_RUS_1 <- dt_survey_RUS_1 %>%
    left_join(counts_to_loop, by = 'respid')

counts_to_filter <- para_rusfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_filter = sum(str_count(to_filter, pattern = 'TRUE')))

dt_survey_RUS_1 <- dt_survey_RUS_1 %>%
    left_join(counts_to_filter, by = 'respid')

counts_to_filter_or_loop <- para_rusfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_filter_or_loop = sum(str_count(to_filter_or_loop, pattern = 'TRUE')))

dt_survey_RUS_1 <- dt_survey_RUS_1 %>%
    left_join(counts_to_filter_or_loop, by = 'respid')

counts_to_immediate_filter <- para_rusfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_immediate_filter = sum(str_count(to_immediate_filter, pattern = 'TRUE')))

dt_survey_RUS_1 <- dt_survey_RUS_1 %>%
    left_join(counts_to_immediate_filter, by = 'respid')

counts_to_nonimmediate_filter <- para_rusfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_nonimmediate_filter = sum(str_count(to_nonimmediate_filter, pattern = 'TRUE')))

dt_survey_RUS_1 <- dt_survey_RUS_1 %>%
    left_join(counts_to_nonimmediate_filter, by = 'respid')

counts_to_regular_1 <- para_rusfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_regular_1 = sum(str_count(to_regular_1, pattern = 'TRUE')))

dt_survey_RUS_1 <- dt_survey_RUS_1 %>%
    left_join(counts_to_regular_1, by = 'respid')

counts_after_same_1 <- para_rusfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_after_same_1 = sum(str_count(after_same, pattern = 'TRUE')))

dt_survey_RUS_1 <- dt_survey_RUS_1 %>%
    left_join(counts_after_same_1, by = 'respid')

counts_after_later_1 <- para_rusfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_after_later_1 = sum(str_count(after_later, pattern = 'TRUE')))

dt_survey_RUS_1 <- dt_survey_RUS_1 %>%
    left_join(counts_after_later_1, by = 'respid')

counts_after_earlier_1 <- para_rusfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_after_earlier_1 = sum(str_count(after_earlier, pattern = 'TRUE')))

dt_survey_RUS_1 <- dt_survey_RUS_1 %>%
    left_join(counts_after_earlier_1, by = 'respid')


counts_to_loop <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_loop_1 = sum(str_count(to_loop, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_to_loop, by = 'respid')

counts_to_filter <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_filter = sum(str_count(to_filter, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_to_filter, by = 'respid')

counts_to_filter_or_loop <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_filter_or_loop = sum(str_count(to_filter_or_loop, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_to_filter_or_loop, by = 'respid')

counts_to_immediate_filter <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_immediate_filter_1 = sum(str_count(to_immediate_filter, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_to_immediate_filter, by = 'respid')

counts_to_nonimmediate_filter <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_nonimmediate_filter_1 = sum(str_count(to_nonimmediate_filter, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_to_nonimmediate_filter, by = 'respid')

para_estfltr_ordered_fin$to_regular_2 <- ifelse(para_estfltr_ordered_fin$previous == 'TRUE', 
                                                ifelse(para_estfltr_ordered_fin$to_filter_or_loop == 'FALSE', 'TRUE', 'FALSE'), 'NotPrev')

counts_to_regular_2 <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_regular_3 = sum(str_count(to_regular_2, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_to_regular_2, by = 'respid')

counts_after_same_1 <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_after_same_1 = sum(str_count(after_same, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_after_same_1, by = 'respid')

counts_after_later_1 <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_after_later_1 = sum(str_count(after_later, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_after_later_1, by = 'respid')

counts_after_earlier_1 <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_after_earlier_1 = sum(str_count(after_earlier, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_after_earlier_1, by = 'respid')

##Adding count of going back to filters/loops/either to survey dataset without consistent going backs
counts_to_loop_withoutbacks <- para_rusfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_loop_withoutbacks = sum(str_count(to_loop_withoutback, pattern = 'TRUE')))

dt_survey_RUS_1 <- dt_survey_RUS_1 %>%
    left_join(counts_to_loop_withoutbacks, by = 'respid')

counts_to_filter_withoutbacks <- para_rusfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_filter_withoutbacks = sum(str_count(to_filter_withoutback, pattern = 'TRUE')))

dt_survey_RUS_1 <- dt_survey_RUS_1 %>%
    left_join(counts_to_filter_withoutbacks, by = 'respid')

counts_to_filter_or_loop_withoutbacks <- para_rusfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_filter_or_loop_withoutbacks = sum(str_count(to_filter_or_loop_withoutback, pattern = 'TRUE')))

dt_survey_RUS_1 <- dt_survey_RUS_1 %>%
    left_join(counts_to_filter_or_loop_withoutbacks, by = 'respid')

counts_to_immediate_filter_withoutbacks <- para_rusfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_immediate_filter_withoutbacks = sum(str_count(to_immediate_filter_withoutback, pattern = 'TRUE')))

dt_survey_RUS_1 <- dt_survey_RUS_1 %>%
    left_join(counts_to_immediate_filter_withoutbacks, by = 'respid')

counts_to_nonimmediate_filter_withoutbacks <- para_rusfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_nonimmediate_filter_withoutbacks = sum(str_count(to_nonimmediate_filter_withoutback, pattern = 'TRUE')))

dt_survey_RUS_1 <- dt_survey_RUS_1 %>%
    left_join(counts_to_nonimmediate_filter_withoutbacks, by = 'respid')


counts_to_loop_withoutbacks <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_loop_withoutbacks = sum(str_count(to_loop_withoutback, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_to_loop_withoutbacks, by = 'respid')

counts_to_filter_withoutbacks <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_filter_withoutbacks = sum(str_count(to_filter_withoutback, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_to_filter_withoutbacks, by = 'respid')

counts_to_filter_or_loop_withoutbacks <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_filter_or_loop_withoutbacks = sum(str_count(to_filter_or_loop_withoutback, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_to_filter_or_loop_withoutbacks, by = 'respid')

counts_to_immediate_filter_withoutbacks <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_immediate_filter_withoutbacks = sum(str_count(to_immediate_filter_withoutback, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_to_immediate_filter_withoutbacks, by = 'respid')

counts_to_nonimmediate_filter_withoutbacks <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_nonimmediate_filter_withoutbacks = sum(str_count(to_nonimmediate_filter_withoutback, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_to_nonimmediate_filter_withoutbacks, by = 'respid')

##Adding variable for going back to filer/loop/either and TRUE for difference before/after

para_rusfltr_ordered_fin$filter_change <- +(as.logical(para_rusfltr_ordered_fin$to_filter) & para_rusfltr_ordered_fin$diff_present)
para_rusfltr_ordered_fin$loop_change <- +(as.logical(para_rusfltr_ordered_fin$to_loop) & para_rusfltr_ordered_fin$diff_present)
para_rusfltr_ordered_fin$filter_or_loop_change <- +(as.logical(para_rusfltr_ordered_fin$to_filter_or_loop) & para_rusfltr_ordered_fin$diff_present)

para_estfltr_ordered_fin$filter_change <- +(as.logical(para_estfltr_ordered_fin$to_filter) & para_estfltr_ordered_fin$diff_present)
para_estfltr_ordered_fin$loop_change <- +(as.logical(para_estfltr_ordered_fin$to_loop) & para_estfltr_ordered_fin$diff_present)
para_estfltr_ordered_fin$filter_or_loop_change <- +(as.logical(para_estfltr_ordered_fin$to_filter_or_loop) & para_estfltr_ordered_fin$diff_present)

para_estfltr_ordered_fin$immediate_filter_aftsame <- ifelse(para_estfltr_ordered_fin$to_immediate_filter == 'TRUE' & para_estfltr_ordered_fin$after_same == 'TRUE',
                                                            'TRUE', 'FALSE')
para_estfltr_ordered_fin$immediate_filter_aftlater <- ifelse(para_estfltr_ordered_fin$to_immediate_filter == 'TRUE' & para_estfltr_ordered_fin$after_later == 'TRUE',
                                                             'TRUE', 'FALSE')
para_estfltr_ordered_fin$immediate_filter_aftearlier <- ifelse(para_estfltr_ordered_fin$to_immediate_filter == 'TRUE' & para_estfltr_ordered_fin$after_earlier == 'TRUE',
                                                               'TRUE', 'FALSE')

para_estfltr_ordered_fin$to_regular_aftsame <- ifelse(para_estfltr_ordered_fin$to_regular_2 == 'TRUE' & para_estfltr_ordered_fin$after_same == 'TRUE',
                                                      'TRUE', 'FALSE')
para_estfltr_ordered_fin$to_regular_aftlater <- ifelse(para_estfltr_ordered_fin$to_regular_2 == 'TRUE' & para_estfltr_ordered_fin$after_later == 'TRUE',
                                                       'TRUE', 'FALSE')
para_estfltr_ordered_fin$to_regular_aftearlier <- ifelse(para_estfltr_ordered_fin$to_regular_2 == 'TRUE' & para_estfltr_ordered_fin$after_earlier == 'TRUE',
                                                         'TRUE', 'FALSE')

para_estfltr_ordered_fin$to_loop_aftsame <- ifelse(para_estfltr_ordered_fin$to_loop == 'TRUE' & para_estfltr_ordered_fin$after_same == 'TRUE',
                                                   'TRUE', 'FALSE')
para_estfltr_ordered_fin$to_loop_aftlater <- ifelse(para_estfltr_ordered_fin$to_loop == 'TRUE' & para_estfltr_ordered_fin$after_later == 'TRUE',
                                                    'TRUE', 'FALSE')
para_estfltr_ordered_fin$to_loop_aftearlier <- ifelse(para_estfltr_ordered_fin$to_loop == 'TRUE' & para_estfltr_ordered_fin$after_earlier == 'TRUE',
                                                      'TRUE', 'FALSE')

para_estfltr_ordered_fin$nonimmediate_filter_aftsame <- ifelse(para_estfltr_ordered_fin$to_nonimmediate_filter == 'TRUE' & para_estfltr_ordered_fin$after_same == 'TRUE',
                                                               'TRUE', 'FALSE')
para_estfltr_ordered_fin$nonimmediate_filter_aftlater <- ifelse(para_estfltr_ordered_fin$to_nonimmediate_filter == 'TRUE' & para_estfltr_ordered_fin$after_later == 'TRUE',
                                                                'TRUE', 'FALSE')
para_estfltr_ordered_fin$nonimmediate_filter_aftearlier <- ifelse(para_estfltr_ordered_fin$to_nonimmediate_filter == 'TRUE' & para_estfltr_ordered_fin$after_earlier == 'TRUE',
                                                                  'TRUE', 'FALSE')

#Adding - by type of question to which came back - satisficing/optimizing/consequent back
counts_immediate_filter_aftsame <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_immediate_filter_aftsame = sum(str_count(immediate_filter_aftsame, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_immediate_filter_aftsame, by = 'respid')

counts_immediate_filter_aftlater <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_immediate_filter_aftlater = sum(str_count(immediate_filter_aftlater, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_immediate_filter_aftlater, by = 'respid')

counts_immediate_filter_aftearlier <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_immediate_filter_aftearlier = sum(str_count(immediate_filter_aftearlier, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_immediate_filter_aftearlier, by = 'respid')

counts_to_regular_aftsame <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_regular_aftsame = sum(str_count(to_regular_aftsame, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_to_regular_aftsame, by = 'respid')

counts_to_regular_aftlater <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_regular_aftlater = sum(str_count(to_regular_aftlater, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_to_regular_aftlater, by = 'respid')

counts_to_regular_aftearlier <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_regular_aftearlier = sum(str_count(to_regular_aftearlier, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_to_regular_aftearlier, by = 'respid')

counts_to_loop_aftsame <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_loop_aftsame = sum(str_count(to_loop_aftsame, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_to_loop_aftsame, by = 'respid')

counts_to_loop_aftlater <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_loop_aftlater = sum(str_count(to_loop_aftlater, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_to_loop_aftlater, by = 'respid')

counts_to_loop_aftearlier <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_to_loop_aftearlier = sum(str_count(to_loop_aftearlier, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_to_loop_aftearlier, by = 'respid')

counts_nonimmediate_filter_aftsame <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_nonimmediate_filter_aftsame = sum(str_count(nonimmediate_filter_aftsame, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_nonimmediate_filter_aftsame, by = 'respid')

counts_nonimmediate_filter_aftlater <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_nonimmediate_filter_aftlater = sum(str_count(nonimmediate_filter_aftlater, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_nonimmediate_filter_aftlater, by = 'respid')

counts_nonimmediate_filter_aftearlier <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_nonimmediate_filter_aftearlier = sum(str_count(nonimmediate_filter_aftearlier, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_nonimmediate_filter_aftearlier, by = 'respid')


##Adding aggregated number of going backs to filer/loop/either and TRUE for difference before/after

counts_filter_change_rus <- setNames(aggregate(x = para_rusfltr_ordered_fin$filter_change,
                                               by = list(para_rusfltr_ordered_fin$respid),
                                               FUN = sum), c("respid", "filter_change"))
counts_filter_change_rus[is.na(counts_filter_change_rus)] <- 0

counts_loop_change_rus <- setNames(aggregate(x = para_rusfltr_ordered_fin$loop_change,
                                             by = list(para_rusfltr_ordered_fin$respid),
                                             FUN = sum), c("respid", "loop_change"))
counts_loop_change_rus[is.na(counts_loop_change_rus)] <- 0

counts_filter_or_loop_change_rus <- setNames(aggregate(x = para_rusfltr_ordered_fin$filter_or_loop_change,
                                                       by = list(para_rusfltr_ordered_fin$respid),
                                                       FUN = sum), c("respid", "filter_or_loop_change"))
counts_filter_or_loop_change_rus[is.na(counts_filter_or_loop_change_rus)] <- 0

dt_survey_RUS_1 <- dt_survey_RUS_1 %>%
    left_join(counts_filter_change_rus, by = 'respid') %>%
    left_join(counts_loop_change_rus, by = 'respid') %>%
    left_join(counts_filter_or_loop_change_rus, by = 'respid')

counts_filter_change_est <- setNames(aggregate(x = para_estfltr_ordered_fin$filter_change,
                                               by = list(para_estfltr_ordered_fin$respid),
                                               FUN = sum), c("respid", "filter_change"))
counts_filter_change_est[is.na(counts_filter_change_est)] <- 0

counts_loop_change_est <- setNames(aggregate(x = para_estfltr_ordered_fin$loop_change,
                                             by = list(para_estfltr_ordered_fin$respid),
                                             FUN = sum), c("respid", "loop_change"))
counts_loop_change_est[is.na(counts_loop_change_est)] <- 0

counts_filter_or_loop_change_est <- setNames(aggregate(x = para_estfltr_ordered_fin$filter_or_loop_change,
                                                       by = list(para_estfltr_ordered_fin$respid),
                                                       FUN = sum), c("respid", "filter_or_loop_change"))
counts_filter_or_loop_change_est[is.na(counts_filter_or_loop_change_est)] <- 0

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_filter_change_est, by = 'respid') %>%
    left_join(counts_loop_change_est, by = 'respid') %>%
    left_join(counts_filter_or_loop_change_est, by = 'respid')

##Adding number of pages (UpdatePage actions) for each respondent
counts_updatepage <- para_rusfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(count_updatepage = sum(str_count(v4, pattern = 'UpdatePageEvent')))

dt_survey_RUS_1 <- dt_survey_RUS_1 %>%
    left_join(counts_updatepage, by = 'respid')

counts_updatepage_est <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(count_updatepage = sum(str_count(v4, pattern = 'UpdatePageEvent')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_updatepage_est, by = 'respid')

#Add variable - filled in one sitting

para_estfltr_ordered_fin_fordate <- para_estfltr_ordered_fin

para_estfltr_ordered_fin_fordate %>%
    mutate(v3 = ymd_hms(v3)) %>%
    summarise(min = min(v3),
              max = max(v3))

para_estfltr_ordered_fin_maxdate <- para_estfltr_ordered_fin_fordate %>% 
    arrange(respid, desc(v3)) %>% 
    distinct(respid, .keep_all = T)

para_estfltr_ordered_fin_mindate <- para_estfltr_ordered_fin_fordate %>% 
    arrange(respid, v3) %>% 
    distinct(respid, .keep_all = T)

para_estfltr_ordered_fin_maxdate$v3_max <- para_estfltr_ordered_fin_maxdate$v3
para_estfltr_ordered_fin_mindate$v3_min <- para_estfltr_ordered_fin_mindate$v3

para_estfltr_ordered_fin_datediff <- merge(para_estfltr_ordered_fin_maxdate, para_estfltr_ordered_fin_mindate, by = c("respid", "respid")) 
para_estfltr_ordered_fin_datediff$datediff <- difftime(para_estfltr_ordered_fin_datediff$v3_max, para_estfltr_ordered_fin_datediff$v3_min, units = "mins")



para_estfltr_ordered_fin_datediff_1 <- subset(para_estfltr_ordered_fin_datediff, select = c(respid,v3_max,v3_min,datediff) )

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(para_estfltr_ordered_fin_datediff_1, by = 'respid')

dt_survey_EST_1$datediff < na.omit(dt_survey_EST_1$datediff)

dt_survey_EST_1$filled_in_one_sitting <- ifelse(dt_survey_EST_1$datediff > 180, 'FALSE', 'TRUE')
max(as.numeric(na.omit(dt_survey_EST_1$datediff)))
max(as.numeric(dt_survey_EST_1$datediff))

View(subset(dt_survey_EST_1, select = c(respid,datediff,filled_in_one_sitting)))

#with startpage

counts_startsession_est <- para_estfltr_ordered %>%
    group_by(respid) %>%
    summarise(count_startsession = sum(str_count(v4, pattern = 'StartSessionEvent')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_startsession_est, by = 'respid')

table(dt_survey_EST_1$count_startsession)
summary(dt_survey_EST_1$count_startsession)

dt_survey_EST_1$filled_in_one_sitting_updtd <- ifelse(dt_survey_EST_1$count_startsession > 1, 0, 1)
table(dt_survey_EST_1$filled_in_one_sitting_updtd)

#Comparison:

para_rusfltr_ordered_onlyprevious <-para_rusfltr_ordered[(para_rusfltr_ordered$previous=="TRUE"),]
para_estfltr_ordered_onlyprevious <-para_estfltr_ordered[(para_estfltr_ordered$previous=="TRUE"),]

counts_changed_est <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_changed = sum(str_count(diff_present, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_changed_est, by = 'respid')

counts_previous_est <- para_estfltr_ordered_fin %>%
    group_by(respid) %>%
    summarise(counts_previous = sum(str_count(previous, pattern = 'TRUE')))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    left_join(counts_previous_est, by = 'respid')

dt_survey_EST_withchanges <- dt_survey_EST_1 %>%
    left_join(counts_changed_est, by = 'respid')


dt_survey_EST_1 <- dt_survey_EST_1 %>%
    add_column(previous_present = 
                   if_else(.$counts_previous > 0, TRUE, FALSE))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    add_column(change_filter_or_loop_present = 
                   if_else(.$filter_or_loop_change > 0, TRUE, FALSE))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    add_column(change_immediate_filter_1 = 
                   if_else(.$counts_to_immediate_filter.y > 0, TRUE, FALSE))

dt_survey_EST_1 <- dt_survey_EST_1 %>%
    add_column(change_loop_1 = 
                   if_else(.$counts_to_loop > 0, TRUE, FALSE))

