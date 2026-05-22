# ===================== 0. 环境初始化 =====================
rm(list = ls())
gc()
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(openxlsx)

dir.create("Result_Merged", showWarnings = FALSE)

# ==============================================================================
# 统一规范
# 1. 缩写全大写：BMI SBP DBP FPG TC HDL LDL TG CRP HbA1C UA BUN SCR PLT WC
# 2. 普通变量首字母大写
# 3. 胸痛合并：双队列任一Yes=Yes，全No=No
# 4. NHANES糖尿病：双变量任一Yes=Yes
# 5. 已删除所有教育水平变量
# ==============================================================================

# ===================== 1. 读取数据 =====================
NHANES <- readRDS("original data/NHANES/清洗但未插补的数据.RDS")
CHARLS <- readRDS("original data/CHARLS/清洗但未插补的数据.RDS")

# ==============================================================================
# 【一】CHARLS 标准化（胸痛合并：Chest_Pain + Chest_Pain_Exert）
# ==============================================================================
Charls_Clean <- CHARLS %>%
  mutate(Height = Height * 100) %>%
  rename(
    ID = ID, Status = Status, Time = Time, Age = Age, Height = Height, Weight = Weight, BMI = BMI, WC = Waist,
    SBP = SBP, DBP = DBP, Sex = Sex, Marital = Marital.status, Smoking = Smoking, Drinking = Drinking,
    Diabetes = Diabetes, Cancer = Cancer, Stroke = Stroke, Hypertension = Hypertension, Dyslipidemia = Dyslipidemia,
    Kidney_Disease = Kidney.Disease, Chest_Pain = Chest.pain, Chest_Pain_Exert = Chest.Pains.When.Climbing,
    HbA1C = HbA1C, TC = TC, HDL = HDL, LDL = LDL, CRP = CRP, TG = TG, FPG = FPG, UA = UA, BUN = BUN, SCR = Scr, PLT = PLT
  ) %>%
  mutate(across(where(is.character), ~str_remove(., "^\\d+_"))) %>%
  # ✅ 核心修正：胸痛合并（任一Yes=Yes）
  mutate(
    Chest_Pain = case_when(
      Chest_Pain == "Yes" | Chest_Pain_Exert == "Yes" ~ "Yes",
      Chest_Pain == "No" & Chest_Pain_Exert == "No" ~ "No",
      TRUE ~ NA_character_
    ),
    CVD = case_when(Heart.disease == "Yes" ~ 1, Heart.disease == "No" ~ 0)
  ) %>%
  # 删除冗余变量+教育变量
  select(-c(Hukou.status, Sleep.health, Intensive.PA, Moderate.PA, Light.PA, Chest_Pain_Exert, Heart.disease, Education.level)) %>%
  mutate(Dataset = "CHARLS", Race = "Non-Hispanic Asian") %>%
  mutate(across(where(is.numeric), ~round(., 2)))

# ==============================================================================
# 【二】NHANES 标准化（糖尿病合并 + 胸痛合并：Chest_Pain + Shortness_Breath）
# ==============================================================================
# 糖尿病合并：双变量任一Yes=Yes
NHANES <- NHANES %>%
  mutate(
    Diabetes = case_when(
      diabetes == "1_Yes" | Diabetes == "1_Yes" ~ "1_Yes",
      diabetes == "2_No" & Diabetes == "2_No" ~ "2_No",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-diabetes, -hyperten)

Nhanes_Clean <- NHANES %>%
  rename(
    ID = SEQN, Status = Status, Time = Time, Age = Age, Sex = Sex, Marital = `Marital status`, Race = Race,
    Smoking = Smoking, Drinking = Drinking, Diabetes = Diabetes, Cancer = Cancer, Stroke = Stroke,
    Hypertension = Hypertension, Dyslipidemia = Hyperlipidemia, Kidney_Disease = `Kidney disease`,
    Chest_Pain = `Chest pain`, Shortness_Breath = `Shortness of breath`, Height = Height, Weight = Weight, BMI = BMI, WC = WC,
    SBP = SBP, DBP = DBP, HbA1C = HbA1C, TC = TC, HDL = HDL, LDL = LDL, CRP = CRP, TG = TG, FPG = FPG, UA = UA, BUN = BUN, SCR = Scr, PLT = PLT,
    CHD = `Coronary heart disease`, HF = `Congestive heart failure`, MI = `Myocardial infarction`, Angina = `Angina pectoris`
  ) %>%
  mutate(across(where(is.character), ~str_remove(., "^\\d+_"))) %>%
  mutate(across(where(is.character), ~ifelse(.x %in% c("Refused","Don't know"), NA, .x))) %>%
  # ✅ 核心修正：胸痛合并（任一Yes=Yes）
  mutate(
    Chest_Pain = case_when(
      Chest_Pain == "Yes" | Shortness_Breath == "Yes" ~ "Yes",
      Chest_Pain == "No" & Shortness_Breath == "No" ~ "No",
      TRUE ~ NA_character_
    ),
    CVD = case_when(
      CHD == "Yes"|HF == "Yes"|MI == "Yes"|Angina == "Yes" ~1,
      CHD == "No"&HF == "No"&MI == "No"&Angina == "No" ~0
    )
  ) %>%
  # 删除冗余变量+教育变量
  select(-c(eligstat, permth_int, permth_exm, `Sleep health`, `Vigorous work activity`, `Moderate work activity`, `Mild work activity`,
            Shortness_Breath, `Severe chest pain`, CHD, HF, MI, Angina, `Education level`)) %>%
  mutate(Dataset = "NHANES", ID = as.character(ID)) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

# ==============================================================================
# 【三】合并数据 + 临床异常值清洗
# ==============================================================================
Merged_Final <- bind_rows(Charls_Clean, Nhanes_Clean) %>%
  mutate(
    Age = ifelse(Age < 18 | Age > 110, NA, Age),
    Height = ifelse(Height < 120 | Height > 210, NA, Height),
    FPG = ifelse(FPG > 450, NA, FPG),
    BMI = ifelse(BMI < 10 | BMI > 60, NA, BMI)
  )

# ==============================================================================
# 【四】统计报表
# ==============================================================================
# 缺失值报告
Missing_Report <- Merged_Final %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Missing_Count") %>%
  mutate(Total = nrow(Merged_Final), Missing_Rate = round(Missing_Count / Total * 100, 2)) %>%
  arrange(desc(Missing_Rate))

# 临床异常值报告
Outlier_Report <- tibble(
  Variable = c("Age","Height","BMI","FPG"),
  Unit = c("岁","cm","kg/m²","mg/dL"),
  Abnormal_Rule = c("<18或>110","<120或>210","<10或>60",">450"),
  Abnormal_Count = c(
    sum(Merged_Final$Age <18 | Merged_Final$Age>110, na.rm=T),
    sum(Merged_Final$Height<120 | Merged_Final$Height>210, na.rm=T),
    sum(Merged_Final$BMI<10 | Merged_Final$BMI>60, na.rm=T),
    sum(Merged_Final$FPG>450, na.rm=T)
  )
)

# ==============================================================================
# 【五】可视化 + 输出
# ==============================================================================
p1 <- head(Missing_Report,15) %>%
  ggplot(aes(x=reorder(Variable,Missing_Rate),y=Missing_Rate)) +
  geom_bar(stat="identity",fill="#E74C3C") + coord_flip() + labs(title="缺失值TOP15") + theme_bw()

p2 <- Merged_Final %>% ggplot(aes(Age,fill=Dataset)) +
  geom_histogram(bins=40,alpha=0.6) + labs(title="年龄分布") + theme_bw()

ggsave("Result_Merged/合并数据可视化.png",grid.arrange(p1,p2,ncol=1),width=10,height=10,dpi=300)

saveRDS(Merged_Final, "Result_Merged/CHARLS_NHANES_统一合并数据.RDS")
write.xlsx(Merged_Final, "Result_Merged/Merged_Final.xlsx")
write.csv(Missing_Report, "Result_Merged/缺失值统计报表.csv", row.names = F)
write.csv(Outlier_Report, "Result_Merged/临床异常值统计报表.csv", row.names = F)

cat("\n✅ 合并完成！胸痛/糖尿病/命名/单位 全部按要求统一！\n")
cat("📊 总样本：",nrow(Merged_Final)," | 变量：",ncol(Merged_Final),"\n")