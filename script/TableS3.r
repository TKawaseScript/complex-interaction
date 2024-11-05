library(dplyr)

Pos_pos_data<-read.csv("Pos_pos_data.csv",header=T)

# rmatch_type と match_type が "No Match" でない行を抽出
TableS3_Pos_pos <- Pos_pos_data %>%
    filter(rmatch_type != "No Match" & match_type != "No Match")

# 結果を出力
write.csv(TableS3_Pos_pos,"TableS3_Pos_pos.csv")

Neg_neg_data<-read.csv("Neg_neg_data.csv",header=T)

# rmatch_type と match_type が "No Match" でない行を抽出
TableS3_Neg_neg <- Neg_neg_data %>%
    filter(rmatch_type != "No Match" & match_type != "No Match")

# 結果を出力
write.csv(TableS3_Neg_neg,"TableS3_Neg_neg.csv")

Pos_none_data<-read.csv("Pos_none_data.csv",header=T)

# rmatch_type と match_type が "No Match" でない行を抽出
TableS3_Pos_none <- Pos_none_data %>%
    filter(rmatch_type != "No Match" & match_type != "No Match")

# 結果を出力
write.csv(TableS3_Pos_none,"TableS3_Pos_none.csv")

Neg_none_data<-read.csv("Pos_none_data.csv",header=T)

# rmatch_type と match_type が "No Match" でない行を抽出
TableS3_Neg_none <- Neg_none_data %>%
    filter(rmatch_type != "No Match" & match_type != "No Match")

# 結果を出力
write.csv(TableS3_Neg_none,"TableS3_Neg_none.csv")