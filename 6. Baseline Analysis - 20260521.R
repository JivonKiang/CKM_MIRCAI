# ==============================================================================
# 脚本6：CKM分期基线特征分析（纯基线分析·无数据集划分）
# 核心：CKM分期组间比较 + 亚组分析 + 差异变量UpSet图 + 趋势可视化
# 🔴 关键：严格沿用脚本4原始分期命名：1,2,3,4a,4b | 同步脚本5全量代谢指标
# 适配：你的自定义变量 | 全英文输出 | 无报错版本
# ==============================================================================
rm(list=ls())
gc()

# 加载全部依赖包（新增UpSetR，替换Venn图）
packages <- c("tidyverse","moonBook","rrtable","openxlsx","ggplot2","gridExtra","VennDiagram","RColorBrewer","UpSetR")
for (p in packages) {
  if (!require(p, character.only=TRUE)) install.packages(p, dependencies = TRUE)
  library(p, character.only=TRUE)
}
set.seed(123)
options(warn=-1)

# ===================== 1. 数据加载与清洗 =====================
cat("============= 1. Data Loading and Cleaning =============\n")
data1 <- readRDS("5. Result_Indicator/代谢指标计算完成数据.RDS")
data1 <- as.data.frame(data1)
names(data1) <- gsub(" ", "_", names(data1))

# ===================== 🔥 同步脚本5：全量变量列表（无缺失） =====================
basic_vars <- c("Age","Sex","Race","Marital", 
                "Smoking", "Drinking", 
                "Diabetes", "Cancer", "Stroke", "Hypertension", "Dyslipidemia", "KidneyDisease", "Chest_Pain",
                "Height","Weight","BMI","WC","WHtR","ABSI","BRI","CI",
                "SBP","DBP",
                "TG","FPG","HbA1C","HDL","UA","CRP")

# ===================== 【固定不动】最新全套代谢指标：9核心 + 144衍生 =====================
core_indicators <- c("TyG", "UHR", "AIP", "CMI", "HCHR", "METS_IR", "SHR", "eGDR", "eGFR")
derivation_vars <- c("WC", "WHtR", "BMI", "ABSI", "BRI", "CI", "CRP", "HDL")
derived_multi <- paste0(rep(core_indicators, each=length(derivation_vars)), "_", derivation_vars)
derived_div <- paste0(rep(core_indicators, each=length(derivation_vars)), "_", derivation_vars, "_div")
MCI_vars <- c(core_indicators, derived_multi, derived_div)

all_vars <- c("Status","CKM_stage_ab","Time", basic_vars, MCI_vars)

# ===================== 🔴 严格绑定原始分期命名 =====================
data1 <- data1[, intersect(all_vars, names(data1))]
data1$CKM_stage_ab <- factor(data1$CKM_stage_ab, 
                             levels = c("1","2","3","4a","4b"),
                             ordered = TRUE)
data1 <- data1 %>% drop_na()

# ===================== 2. 创建输出文件夹 =====================
out_dir <- "6_Baseline_Analysis_Results"
dir.create(out_dir, recursive=T, showWarnings=F)
cat("============= 2. Baseline Comparison Between CKM Stages =============\n")

# ===================== 3. 基线分析：CKM分期组间比较 =====================
mb_total <- mytable(CKM_stage_ab ~ ., data=data1, digits=2)
table2docx(mb_total$res, paste0(out_dir,"/6_Overall_Baseline_Comparison.docx"))
write.xlsx(mb_total$res, paste0(out_dir,"/6_Overall_Baseline_Comparison.xlsx"))

# ===================== 4. CKM亚组分析 =====================
mb_sub <- mytable(CKM_stage_ab + Status ~ ., data=data1, digits=2)
table2docx(mb_sub, paste0(out_dir,"/6_CKM_Subgroup_Analysis.docx"))

# Excel分工作表保存亚组表
wb <- createWorkbook()
stage_names <- levels(data1$CKM_stage_ab)
for(i in 1:length(mb_sub)){
  addWorksheet(wb, paste0("Stage_", stage_names[i]))
  writeData(wb, paste0("Stage_", stage_names[i]), mb_sub[[i]]$res)
}
saveWorkbook(wb, paste0(out_dir,"/6_CKM_Subgroup_Analysis.xlsx"), overwrite=T)

# ===================== 5. 提取各分期差异变量 =====================
cat("============= 3. Extract Significant Differential Variables =============\n")
diff_vars_list <- list()
stage_labels <- paste0("CKM_", levels(data1$CKM_stage_ab))

for(i in 1:length(mb_sub)){
  res <- mb_sub[[i]]$res
  var_names <- str_trim(res$Status)        
  p_values <- as.numeric(res$p)            
  valid_idx <- which(p_values < 0.05 & !grepl("^\\s|-", var_names) & !is.na(p_values))
  sig_vars <- unique(var_names[valid_idx])
  
  diff_vars_list[[i]] <- sig_vars
  names(diff_vars_list)[i] <- stage_labels[i]
}

# 输出差异变量 + 共有变量
print("Significant variables in each CKM stage:")
print(diff_vars_list)
common_vars <- Reduce(intersect, diff_vars_list)
cat("✅ Common significant variables in all stages:", length(common_vars), "\n")
write.xlsx(data.frame(Common_Differential_Variables = common_vars), 
           paste0(out_dir,"/6_Common_Differential_Variables.xlsx"))

# ===================== 6. 绘制UpSet图 =====================
cat("============= 4. Plot UpSet Plot =============\n")
png(paste0(out_dir,"/6_UpSetPlot_Differential_Variables.png"), width=2500, height=1800, res=300)
upset(
  fromList(diff_vars_list),
  nsets = length(stage_labels),       
  nintersects = 40,         
  order.by = "freq",        
  main.bar.color = "#2E86AB",
  sets.bar.color = "#A23B72",
  matrix.color = "#F18F01",
  text.scale = 1.3           
)
dev.off()

# ===================== 7. 趋势可视化（分面绘图） =====================
cat("============= 5. Visualization of Common Variables =============\n")
common_vars <- intersect(common_vars, colnames(data1))
continuous_vars <- common_vars[sapply(data1[,common_vars, drop=F], is.numeric)]
categorical_vars <- common_vars[sapply(data1[,common_vars, drop=F], function(x) !is.numeric(x))]

# 1. 连续变量趋势图
if(length(continuous_vars)>0){
  plot_data <- data1 %>%
    group_by(CKM_stage_ab) %>%
    summarise(across(all_of(continuous_vars), median, na.rm=T)) %>%
    pivot_longer(cols=-CKM_stage_ab, names_to="Variable", values_to="Median")
  
  p_line <- ggplot(plot_data, aes(x=CKM_stage_ab, y=Median, group=1)) +
    geom_line(linewidth=1.2, color="#2E86AB") + 
    geom_point(size=2.5, color="#A23B72") +
    facet_wrap(~Variable, scales="free_y", ncol=6) +
    labs(title="Trends of Continuous Variables Across CKM Stages",
         x="CKM Stage", y="Median Value") +
    theme_bw() + 
    theme(plot.title=element_text(hjust=0.5, size=14),
          strip.text=element_text(size=10, face="bold"),
          axis.text=element_text(size=9))
  
  ggsave(paste0(out_dir,"/6_Continuous_Variables_Trend_Facet.png"), p_line, width=10, height=10, dpi=300)
  write.xlsx(plot_data, paste0(out_dir,"/6_Continuous_Variables_Median_by_Stage.xlsx"))
}

# 2. 分类变量堆叠图
if(length(categorical_vars)>0){
  plot_data <- data1 %>%
    select(CKM_stage_ab, all_of(categorical_vars)) %>%
    pivot_longer(cols=-CKM_stage_ab, names_to="Variable", values_to="Category") %>%
    count(CKM_stage_ab, Variable, Category) %>%
    group_by(CKM_stage_ab, Variable) %>%
    mutate(Percentage = n/sum(n)*100) %>%
    ungroup()
  
  p_bar <- ggplot(plot_data, aes(x=CKM_stage_ab, y=Percentage, fill=Category)) +
    geom_col(position="stack", width=0.7) +
    facet_wrap(~Variable, scales="free_y", ncol=3) +
    labs(title="Prevalence of Categorical Variables Across CKM Stages",
         x="CKM Stage", y="Percentage (%)") +
    scale_y_continuous(limits=c(0,100)) +
    theme_bw() +
    theme(plot.title=element_text(hjust=0.5, size=14),
          strip.text=element_text(size=10, face="bold"),
          legend.position="bottom",
          axis.text=element_text(size=9))
  
  ggsave(paste0(out_dir,"/6_Categorical_Variables_Stacked_Bar_Facet.png"), p_bar, width=8, height=5, dpi=300)
  write.xlsx(plot_data, paste0(out_dir,"/6_Categorical_Variables_Prevalence_by_Stage.xlsx"))
}

# ===================== 分析完成 =====================
cat("\n🎉 Script 6 Completed Successfully! All results saved in:", out_dir, "\n")