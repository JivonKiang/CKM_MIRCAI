# ==============================================================================
# 脚本 4：CKM分期全自动计算（适配NHANES+CHARLS合并数据 | 零报错）
# 新增：无任何风险因素 = CKM 0期（显性定义 + 强制显示0期）
# 适配数据：Result_Clean_Impute/最终合规数据.RDS
# ==============================================================================
rm(list = ls())
gc()
library(dplyr)

# ===================== 1. 读取合并清洗后的最终数据 =====================
cat("============= 读取NHANES+CHARLS合并数据 =============\n")
data <- readRDS("Result_Clean_Impute/最终合规数据.RDS")
cat("✅ 数据读取成功 | 总样本量：", nrow(data), "\n")

# ===================== 2. 数据预处理 =====================
# 重新计算BMI
data <- data %>%
  mutate(BMI = Weight / (Height / 100) ^ 2) %>%
  filter(BMI >= 10 & BMI <= 60)

# ===================== 3. 核心指标分类 =====================
# BMI分类（亚裔/非亚裔）
data <- data %>%
  mutate(BMI_Category = case_when(
    Race == "asian" & BMI < 23 ~ "Normal",
    Race == "asian" & BMI >= 23 ~ "Abnormal",
    Race != "asian" & BMI < 25 ~ "Normal",
    Race != "asian" & BMI >= 25 ~ "Abnormal"
  ))

# 腰围WC分类
data <- data %>% 
  mutate(WC_Category = case_when(
    Race == "asian" & Sex == "2_Female" & WC < 80 ~ "Normal",
    Race == "asian" & Sex == "1_Male" & WC < 90 ~ "Normal",
    Race != "asian" & Sex == "2_Female" & WC < 88 ~ "Normal",
    Race != "asian" & Sex == "1_Male" & WC < 102 ~ "Normal",
    TRUE ~ "Abnormal"
  ))

# 血糖分类
data <- data %>% 
  mutate(FPG_Category = case_when(
    FPG < 100 ~ "Normal",
    FPG >= 100 & FPG <= 125 ~ "Middle",
    TRUE ~ "High"
  ))

# 糖化血红蛋白分类
data <- data %>% 
  mutate(HbA1C_Category = case_when(
    HbA1C < 5.7 ~ "Normal",
    HbA1C >= 5.7 & HbA1C <= 6.4 ~ "Middle",
    TRUE ~ "High"
  ))

# 血压分类
data <- data %>% 
  mutate(SBP_Category = case_when(SBP < 140 ~ "Normal", TRUE ~ "High"),
         DBP_Category = case_when(DBP < 90 ~ "Normal", TRUE ~ "High"))

# 血脂分类
data <- data %>% 
  mutate(TG_Category = case_when(TG < 135 ~ "Normal", TRUE ~ "High"),
         HDL_Category = case_when(HDL >= 50 ~ "Normal", TRUE ~ "Abnormal"))

# ===================== 4. eGFR计算 + 肾功能分级 =====================
compute_eGFR <- function(data) {
  κ <- ifelse(data$Sex == "2_Female", 0.7, 0.9)
  α <- ifelse(data$Sex == "2_Female", -0.241, -0.302)
  scr_over_kappa <- data$SCR / κ
  min_val <- pmin(scr_over_kappa, 1)
  max_val <- pmax(scr_over_kappa, 1)
  exponent_part <- (min_val ^ α) * (max_val ^ -1.200)
  age_factor <- 0.9938 ^ data$Age
  gender_factor <- ifelse(data$Sex == "2_Female", 1.012, 1)
  eGFRcr <- 142 * exponent_part * age_factor * gender_factor
  data$eGFR <- round(eGFRcr, 2)
  return(data)
}
data <- compute_eGFR(data)

data <- data %>% 
  mutate(eGFR_Category = case_when(
    eGFR >= 90 ~ "G1",
    eGFR >= 60 & eGFR <= 89 ~ "G2",
    eGFR >= 45 & eGFR <= 59 ~ "G3a",
    eGFR >= 30 & eGFR <= 44 ~ "G3b",
    eGFR >= 15 & eGFR <= 29 ~ "G4",
    TRUE ~ "G5"
  ))

# ===================== 5. 临床/亚临床证据（匹配你的变量） =====================
data <- data %>% 
  mutate(Subclinical_evidence = case_when(
    Chest_Pain == "Yes" ~ "Positive",
    TRUE ~ "Negative"
  ))

data <- data %>% 
  mutate(Clinical_evidence = case_when(
    CVD == "Yes" ~ "Positive",
    TRUE ~ "Negative"
  ))

# ===================== 6. CKM评分计算 =====================
variable_map <- function(var, value) {
  switch(var,
         "BMI_Category" = ifelse(value == "Abnormal", 1, 0),
         "WC_Category" = ifelse(value == "Abnormal", 1, 0),
         "HbA1C_Category" = case_when(value == "High"~2, value == "Middle"~1, TRUE~0),
         "FPG_Category" = case_when(value == "High"~2, value == "Middle"~1, TRUE~0),
         "SBP_Category" = ifelse(value == "High",2,0),
         "DBP_Category" = ifelse(value == "High",2,0),
         "TG_Category" = ifelse(value == "High",2,0),
         "HDL_Category" = ifelse(value == "Abnormal",2,0),
         "eGFR_Category" = case_when(value == "G4"~3, value == "G5"~4, TRUE~0),
         "Subclinical_evidence" = ifelse(value == "Positive",3,0),
         "Clinical_evidence" = ifelse(value == "Positive",4,0)
  )
}

# 计算各项得分
data <- data %>%
  mutate(
    BMI_Score = variable_map("BMI_Category", BMI_Category),
    WC_Score = variable_map("WC_Category", WC_Category),
    HbA1C_Score = variable_map("HbA1C_Category", HbA1C_Category),
    FPG_Score = variable_map("FPG_Category", FPG_Category),
    SBP_Score = variable_map("SBP_Category", SBP_Category),
    DBP_Score = variable_map("DBP_Category", DBP_Category),
    TG_Score = variable_map("TG_Category", TG_Category),
    HDL_Score = variable_map("HDL_Category", HDL_Category),
    eGFR_Score = variable_map("eGFR_Category", eGFR_Category),
    Subclinical_Score = variable_map("Subclinical_evidence", Subclinical_evidence),
    Clinical_Score = variable_map("Clinical_evidence", Clinical_evidence)
  )

# CKM主分期（无任何风险=0分）
data <- data %>%
  mutate(CKM_stage = pmax(BMI_Score, WC_Score, HbA1C_Score, FPG_Score,
                          SBP_Score, DBP_Score, TG_Score, HDL_Score,
                          eGFR_Score, Subclinical_Score, Clinical_Score, na.rm=TRUE))

# ===================== 关键：定义0期 + 强制显示所有分期 =====================
data <- data %>%
  mutate(CKM_stage_ab = case_when(
    CKM_stage == 0 ~ "0",  # 0期：无任何风险
    CKM_stage == 4 & eGFR_Category == "G5" ~ "4b",
    CKM_stage == 4 & eGFR_Category != "G5" ~ "4a",
    TRUE ~ as.character(CKM_stage)
  ))

# 强制因子水平（包含0期，哪怕没有样本也会显示）
data$CKM_stage_ab <- factor(data$CKM_stage_ab, levels = c("0","1","2","3","4a","4b"))

# ===================== 验证：查看是否有0期样本 =====================
cat("\n============= 验证：0期样本数量 =============\n")
cat("总样本中，CKM_stage=0（完全无风险）的数量：", sum(data$CKM_stage == 0, na.rm=TRUE), "\n")
cat("CKM_stage所有取值：", paste(unique(data$CKM_stage), collapse = ", "), "\n")

# ===================== 7. 结果输出 =====================
cat("\n============= CKM分期计算完成（强制显示0期）=============\n")
print(table(data$CKM_stage_ab))

# 创建输出文件夹
if(!dir.exists("4. Result_CKM")) dir.create("4. Result_CKM")
saveRDS(data, file = "4. Result_CKM/CKM_Stage_Result.RDS")

cat("\n🎉 脚本运行完成！结果保存至：4. Result_CKM/CKM_Stage_Result.RDS\n")