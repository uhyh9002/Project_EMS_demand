library(Rssa)
source("C:/R_pro/changepointA/bin_data.txt")

# Data pre-processing --------
temporal_dat12 <- read.csv("C:/R_pro/changepointA/12_daejeon_sorted_filtered2.csv")
temporal_dat11 <- read.csv("C:/R_pro/changepointA/11_daejeon_sorted_filtered.csv")
temporal_dat10 <- read.csv("C:/R_pro/changepointA/10_daejeon_sorted_filtered.csv")

temporal_dat12 = temporal_dat12[temporal_dat12$transfer=="이송",]
temporal_dat11 = temporal_dat11[temporal_dat11$transfer=="이송",]
temporal_dat10 = temporal_dat10[temporal_dat10$transfer=="이송",]

dj_MCI = temporal_dat12[duplicated(temporal_dat12$jaenan_sn),]
dj_MCI = dj_MCI[!duplicated(dj_MCI$jaenan_sn),]
temporal_dat12 = temporal_dat12[!(temporal_dat12$jaenan_sn %in% dj_MCI$jaenan_sn),]
dj_MCI = temporal_dat11[duplicated(temporal_dat11$jaenan_sn),]
dj_MCI = dj_MCI[!duplicated(dj_MCI$jaenan_sn),]
temporal_dat11 = temporal_dat11[!(temporal_dat11$jaenan_sn %in% dj_MCI$jaenan_sn),]
dj_MCI = temporal_dat10[duplicated(temporal_dat10$jaenan_sn),]
dj_MCI = dj_MCI[!duplicated(dj_MCI$jaenan_sn),]
temporal_dat10 = temporal_dat10[!(temporal_dat10$jaenan_sn %in% dj_MCI$jaenan_sn),]

# Regular user delete ---------
name_bin = binning(temporal_dat12,"name","string")
a = vector()
for(i in 1:length(name_bin)){
  if (length(name_bin[[i]])>5){
    a = append(a,names(name_bin)[i],length(a))
  }
}
temporal_dat12.name = temporal_dat12[!temporal_dat12$name%in%a,]
name_bin = binning(temporal_dat11,"name","string")
a = vector()
for(i in 1:length(name_bin)){
  if (length(name_bin[[i]])>5){
    a = append(a,names(name_bin)[i],length(a))
  }
}
temporal_dat11.name = temporal_dat11[!temporal_dat11$name%in%a,]
name_bin = binning(temporal_dat10,"name","string")
a = vector()
for(i in 1:length(name_bin)){
  if (length(name_bin[[i]])>5){
    a = append(a,names(name_bin)[i],length(a))
  }
}
temporal_dat10.name = temporal_dat10[!temporal_dat10$name%in%a,]
temporal_dat12 = temporal_dat12.name
temporal_dat11 = temporal_dat11.name
temporal_dat10 = temporal_dat10.name
remove(temporal_dat12.name)
remove(temporal_dat11.name)
remove(temporal_dat10.name)

write.csv(temporal_dat12,"dj_12_original.csv")
write.csv(temporal_dat11,"dj_11_original.csv")
write.csv(temporal_dat10,"dj_10_original.csv")

# Data construction --------
time_arrival = matrix(0,((365*3+1)*24),21+24)
j = 0
past_time = 23
for(i in 1:nrow(temporal_dat10)){
  current_time = floor(temporal_dat10[i,]$call_t/100)
  if(current_time!=past_time){
    diff = (current_time - past_time)%%24
    j = j+diff
    time_arrival[j,1] = time_arrival[j,1] + 1
    time_arrival[j,(current_time+2)] = 1              # Time index
    time_arrival[j,(temporal_dat10[i,]$dow+1+24)] = 1 # Day of week index
    month = floor((temporal_dat10[i,]$call_d%%10000)/100)
    time_arrival[j,(month+8+24)] = 1                  # Month index
    time_arrival[j,21+24] = temporal_dat10[i,]$call_d
  }else{
    time_arrival[j,1] = time_arrival[j,1] + 1
  }
  past_time = current_time
}
j = 365*24
for(i in 1:nrow(temporal_dat11)){
  current_time = floor(temporal_dat11[i,]$call_t/100)
  if(current_time!=past_time){
    diff = (current_time - past_time)%%24
    j = j+diff
    time_arrival[j,1] = time_arrival[j,1] + 1
    time_arrival[j,(current_time+2)] = 1              # Time index
    time_arrival[j,(temporal_dat11[i,]$dow+1+24)] = 1 # Day of week index
    month = floor((temporal_dat11[i,]$call_d%%10000)/100)
    time_arrival[j,(month+8+24)] = 1                  # Month index
    time_arrival[j,21+24] = temporal_dat11[i,]$call_d
  }else{
    time_arrival[j,1] = time_arrival[j,1] + 1
  }
  past_time = current_time
}
j = 365*2*24
for(i in 1:nrow(temporal_dat12)){
  current_time = floor(temporal_dat12[i,]$call_t/100)
  if(current_time!=past_time){
    diff = (current_time - past_time)%%24
    j = j+diff
    time_arrival[j,1] = time_arrival[j,1] + 1
    time_arrival[j,(current_time+2)] = 1              # Time index
    time_arrival[j,(temporal_dat12[i,]$dow+1+24)] = 1 # Day of week index
    month = floor((temporal_dat12[i,]$call_d%%10000)/100)
    time_arrival[j,(month+8+24)] = 1                  # Month index
    time_arrival[j,21+24] = temporal_dat12[i,]$call_d
  }else{
    time_arrival[j,1] = time_arrival[j,1] + 1
  }
  past_time = current_time
}

# for(error_prevent in 1:5){
#   # Data Interpolation iterate until no zeroes
#   for(i in 2:(nrow(time_arrival)-1)){
#     if(time_arrival[i,1]==0){
#       time_index = 0
#       dow_index = 0
#       month_index = 0
#       for(j in 1:24){
#         time_index = time_index+time_arrival[(i-1),(j+1)]*j
#       }
#       if(time_index == 24){
#         time_arrival[i,(1+1)] = 1
#       }else{
#         time_arrival[i,(time_index+1+1)] = 1
#       }
#       for(j in 1:7){
#         dow_index = dow_index+time_arrival[(i-1),(j+1+24)]*j
#       }
#       if(time_index == 24){
#         time_arrival[i,(dow_index+1+1+24)] = 1
#       }else{
#         time_arrival[i,(dow_index+1+24)] = 1
#       }
#       month_index = ((floor((time_arrival[(i-1),45])/100)%%100)+(floor((time_arrival[(i+1),45])/100)%%100))/2
#       
#       if(time_index == 24 && month_index%%1>0){
#         time_arrival[i,(month_index+1+1+24+7)] = 1
#       }else{
#         time_arrival[i,(month_index+1+24+7)] = 1
#       }
#     }
#   }
# }

# write.csv(time_arrival,"time_arrival_dj.csv")
# write.csv(time_arrival,"time_arrival_dj_single.csv")
# write.csv(time_arrival,"time_arrival_dj_name.csv")
write.csv(time_arrival,"time_arrival_dj_origin_2.csv")

time_arrival = time_arrival[,2:ncol(time_arrival)]
day_arrival_origin = matrix(0,((365*3+1)),21)
for(i in 1:nrow(day_arrival_origin)){
  day_arrival_origin[i,1] = sum(time_arrival[((i-1)*24+1):(i*24),1])
  for(j in 1:20){
    day_arrival_origin[i,(j+1)] = max(time_arrival[((i-1)*24+1):(i*24),(25+j)])
  }
}
write.csv(day_arrival_origin,"day_arrival_dj_origin_2.csv"