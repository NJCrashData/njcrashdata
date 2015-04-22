# 04 Mapping.r

library(ggmap)

DT.Acc_code[match_type != "Exact" & Municipality_Code == 35]




2011132211-3785
output_accidents_to_csv(DT.Accidents)


## FOR GOOGLE FUSION
# file_out <- output_accidents_to_csv(DT.Accidents, file_name = "NJ_Accidents_MnmthEsx_2011_2012_2013.csv", split=FALSE)
file_outs <- output_accidents_to_csv(DT.Accidents, file_name = "NJ_Accidents_MnmthEsx_2011_2012_2013.csv", split=TRUE)
.cc(file_out)
subl(file_out)



  ============================

