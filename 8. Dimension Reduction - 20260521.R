# ==============================================================================
# 脚本8：基线差异与双效指标整合筛选【最终版·散点图轴范围自定义+自动P值+引线】
# ==============================================================================
rm(list=ls())
gc()

# 加载依赖包（新增rstatix用于更稳定的统计计算）
packages <- c("tidyverse","survival","MASS","openxlsx","car","pROC","UpSetR",
              "pheatmap","ggpubr","fpc","gridExtra","rstatix")
for (p in packages) {
  if (!require(p, character.only=TRUE)) install.packages(p, dependencies = TRUE)
  library(p, character.only=TRUE)
}
set.seed(123)
options(warn = -1)

# ===================== 🔥 自定义散点图坐标轴范围（仅需修改这里！） =====================
# X轴 = AUC (Stage_AUC)
SCATTER_X_MIN <- 0.5   # X轴最小值
SCATTER_X_MAX <- 0.65   # X轴最大值
# Y轴 = C-index (Prognosis_Cindex)
SCATTER_Y_MIN <- 0.5   # Y轴最小值
SCATTER_Y_MAX <- 0.85   # Y轴最大值
# ====================================================================================

# ===================== 变量定义 =====================
basic_vars <- c("Age","Sex","Race","Marital","Smoking","Drinking","Diabetes","Cancer","Stroke",
                "Hypertension","Dyslipidemia","KidneyDisease","Chest_Pain",
                "Height","Weight","BMI","WC","WHtR","ABSI","BRI","CI","SBP","DBP","TG","FPG","HbA1C","HDL","UA","CRP")
# ===================== 【固定不动】最新全套代谢指标：9核心 + 144衍生 =====================
core_indicators <- c("TyG", "UHR", "AIP", "CMI", "HCHR", "METS_IR", "SHR", "eGDR", "eGFR")
derivation_vars <- c("WC", "WHtR", "BMI", "ABSI", "BRI", "CI", "CRP", "HDL")
derived_multi <- paste0(rep(core_indicators, each=length(derivation_vars)), "_", derivation_vars)
derived_div <- paste0(rep(core_indicators, each=length(derivation_vars)), "_", derivation_vars, "_div")
metabolic_vars <- c(derived_multi, derived_div)

# ===================== 核心函数 =====================
vif_filter <- function(df, vars) {
  if(length(vars) <= 1) return(vars)
  tryCatch({
    fit <- lm(reformulate(vars[-1], vars[1]), data = df)
    vif_val <- vif(fit)
    return(names(vif_val[vif_val < 5]))
  }, error = function(e) vars)
}
select_best_metrics <- function(df, vars, score_df, score_col) {
  if(length(vars)==0) return(c())
  vars_ordered <- score_df %>% filter(variable %in% vars) %>% arrange(desc(!!sym(score_col))) %>% pull(variable)
  selected <- c()
  for(v in vars_ordered){ temp <- c(selected, v); clean <- vif_filter(df, temp); if(v %in% clean) selected <- temp }
  return(selected)
}

# ===================== 数据读取 =====================
cat("========== 1. 数据加载 ==========\n")
data <- readRDS("5. Result_Indicator/代谢指标计算完成数据.RDS")
data$CKM_stage_ab <- factor(data$CKM_stage_ab, levels = c("1","2","3","4a","4b"), ordered = TRUE)
data <- data %>% drop_na(Time, Status, CKM_stage_ab)

baseline_vars <- read.xlsx("6_Baseline_Analysis_Results/6_Common_Differential_Variables.xlsx")[[1]]
dual_vars <- read.xlsx("7_Dual_Effect_Results/Final_Dual_Effect_Variables.xlsx")$Dual_Effect_Biomarker
dual_vars <- dual_vars[!is.na(dual_vars)]

# ===================== 筛选与效能计算 =====================
integrated_vars <- intersect(baseline_vars, dual_vars)
perf_df <- data.frame()
for (v in integrated_vars) {
  rho <- tryCatch(cor(as.numeric(data$CKM_stage_ab), data[[v]], method = "spearman"), error=function(e) NA)
  auc <- tryCatch({ 
    fit <- polr(reformulate(v, "CKM_stage_ab"), data = data, Hess = T)
    pred_prob <- predict(fit, type = "probs")
    raw_auc <- multiclass.roc(data$CKM_stage_ab, pred_prob)$auc
    ifelse(!is.na(rho) && rho < 0 && raw_auc < 0.5, 1 - raw_auc, raw_auc) 
  }, error=function(e) NA)
  cindex <- tryCatch({ 
    fit <- coxph(Surv(Time, Status) ~ get(v), data = data)
    concordance(fit)$concordance 
  }, error=function(e) NA)
  perf_df <- rbind(perf_df, data.frame(Variable = v, Stage_AUC = round(auc,3), Prognosis_Cindex = round(cindex,3)))
}
perf_df <- perf_df %>% drop_na() %>% filter(Stage_AUC >= 0.5)
optimal_vars <- select_best_metrics(data, perf_df$Variable, perf_df %>% rename(variable=Variable, auc=Stage_AUC), "auc")
optimal_data <- perf_df %>% filter(Variable %in% optimal_vars)

# ===================== 标记指标类型 =====================
optimal_data <- optimal_data %>%
  mutate(Type = case_when(
    Variable %in% basic_vars ~ "Clinical",
    Variable %in% metabolic_vars ~ "Metabolic",
    TRUE ~ "Other"
  ))

# ===================== 聚类分析 =====================
cluster_mat <- optimal_data %>% dplyr::select(Stage_AUC, Prognosis_Cindex) %>% scale()
hc <- hclust(dist(cluster_mat), method = "ward.D2")
optimal_data$k2 <- as.factor(cutree(hc, k=2))
optimal_data$k3 <- as.factor(cutree(hc, k=3))
optimal_data$k4 <- as.factor(cutree(hc, k=4))

# ===================== 统一主题 =====================
my_theme <- theme_bw() + 
  theme(plot.title=element_text(hjust=0.5,face="bold"), 
        legend.position="bottom",
        axis.text = element_text(size=10),
        axis.title = element_text(size=11))

# ==============================================================================
# 🔥 核心修复：全自动P值+完整引线+强制显示ns
# ==============================================================================
plot_data <- optimal_data %>% pivot_longer(c(Stage_AUC,Prognosis_Cindex), names_to = "Metric", values_to = "Value")

# 定义显著性符号规则（强制显示ns）
symnum.args <- list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                    symbols = c("***", "**", "*", "ns"))

# ==============================================================================
# 绘制子图：自动引线+完整标注 + 自定义坐标轴范围
# ==============================================================================
# -------------------- k=2 散点图 + 箱线图 --------------------
p2_scatter <- ggplot(optimal_data, aes(x=Stage_AUC, y=Prognosis_Cindex, color=k2, shape=Type)) +
  geom_point(size=4) + geom_text(aes(label=Variable), vjust=-1.2, size=3, check_overlap = T) +
  scale_color_brewer(palette="Dark2") + labs(title="k=2 Clustering", x="AUC", y="C-index") + 
  xlim(SCATTER_X_MIN, SCATTER_X_MAX) + ylim(SCATTER_Y_MIN, SCATTER_Y_MAX) +  # 自定义轴范围
  my_theme

p2_box <- ggplot(plot_data, aes(x=Metric, y=Value, fill=k2)) +
  geom_boxplot(width=0.6, alpha=0.7, outlier.shape = NA, position = position_dodge(0.8)) + 
  geom_jitter(size=2, shape=21, color="black", stroke=0.2, position=position_jitterdodge(0.1)) +
  scale_fill_brewer(palette="Dark2") +
  labs(title="k=2 Comparison", x="", y="Value") + my_theme + theme(legend.position="none") +
  # 🎯 自动 Wilcoxon + 完整引线 + 强制显示ns
  stat_compare_means(
    method = "wilcox.test", label = "p.signif",
    hide.ns = FALSE,  # 强制显示不显著
    symnum.args = symnum.args,
    tip.length = 0.03,  # 引线长度
    bracket.size = 0.5,  # 引线粗细
    comparisons = list(list(c("1","2")), list(c("1","2"))),
    label.y = c(0.82, 0.68),  # C-index和AUC的标注高度
    size = 4, fontface = "bold"
  )

p2_row <- ggarrange(p2_scatter+theme(legend.position="none"), p2_box, ncol=2, common.legend=T, legend="bottom")

# -------------------- k=3 散点图 + 箱线图 --------------------
p3_scatter <- ggplot(optimal_data, aes(x=Stage_AUC, y=Prognosis_Cindex, color=k3, shape=Type)) +
  geom_point(size=4) + geom_text(aes(label=Variable), vjust=-1.2, size=3, check_overlap = T) +
  scale_color_brewer(palette="Dark2") + labs(title="k=3 Clustering", x="AUC", y="C-index") + 
  xlim(SCATTER_X_MIN, SCATTER_X_MAX) + ylim(SCATTER_Y_MIN, SCATTER_Y_MAX) +  # 自定义轴范围
  my_theme

p3_box <- ggplot(plot_data, aes(x=Metric, y=Value, fill=k3)) +
  geom_boxplot(width=0.6, alpha=0.7, outlier.shape = NA, position = position_dodge(0.8)) + 
  geom_jitter(size=2, shape=21, color="black", stroke=0.2, position=position_jitterdodge(0.1)) +
  scale_fill_brewer(palette="Dark2") +
  labs(title="k=3 Comparison", x="", y="Value") + my_theme + theme(legend.position="none") +
  # 🎯 三组两两比较 + 完整引线 + 强制显示ns
  stat_compare_means(
    method = "wilcox.test", label = "p.signif",
    hide.ns = FALSE,
    symnum.args = symnum.args,
    comparisons = list(list(c("1","2")), list(c("1","3")), list(c("2","3")), list(c("1","2")), list(c("1","3")), list(c("2","3"))),
    tip.length = rep(0.01,6), bracket.size = 0.5, step.increase = 0.05,
    label.y = c(0.84, 0.87, 0.90, 0.69, 0.72, 0.75),
    size = 3.5, fontface = "bold"
  )

p3_row <- ggarrange(p3_scatter+theme(legend.position="none"), p3_box, ncol=2, common.legend=T, legend="bottom")

# -------------------- k=4 散点图 + 箱线图 --------------------
p4_scatter <- ggplot(optimal_data, aes(x=Stage_AUC, y=Prognosis_Cindex, color=k4, shape=Type)) +
  geom_point(size=4) + geom_text(aes(label=Variable), vjust=-1.2, size=3, check_overlap = T) +
  scale_color_brewer(palette="Dark2") + labs(title="k=4 Clustering", x="AUC", y="C-index") + 
  xlim(SCATTER_X_MIN, SCATTER_X_MAX) + ylim(SCATTER_Y_MIN, SCATTER_Y_MAX) +  # 自定义轴范围
  my_theme

p4_box <- ggplot(plot_data, aes(x=Metric, y=Value, fill=k4)) +
  geom_boxplot(width=0.6, alpha=0.7, outlier.shape = NA, position = position_dodge(0.8)) + 
  geom_jitter(size=2, shape=21, color="black", stroke=0.2, position=position_jitterdodge(0.1)) +
  scale_fill_brewer(palette="Dark2") +
  labs(title="k=4 Comparison", x="", y="Value") + my_theme + theme(legend.position="none") +
  # 🎯 四组两两比较 + 完整引线 + 强制显示ns
  stat_compare_means(
    method = "wilcox.test", label = "p.signif",
    hide.ns = FALSE,
    symnum.args = symnum.args,
    comparisons = list(list(c("1","2")), list(c("1","3")), list(c("1","4")), list(c("2","3")), list(c("2","4")), list(c("3","4")),
                       list(c("1","2")), list(c("1","3")), list(c("1","4")), list(c("2","3")), list(c("2","4")), list(c("3","4"))),
    tip.length = rep(0.01,12), bracket.size = 0.4, step.increase = 0.04,
    label.y = seq(0.85, 1.1, 0.02),
    size = 3, fontface = "bold"
  )

p4_row <- ggarrange(p4_scatter+theme(legend.position="none"), p4_box, ncol=2, common.legend=T, legend="bottom")

# ===================== 拼接大图 =====================
combined_plot <- ggarrange(p2_row, p3_row, p4_row, nrow=3)

# ===================== 输出 =====================
out_dir <- "8_Integrated_Screening_Results"
dir.create(out_dir, showWarnings = F, recursive = T)
ggsave(file.path(out_dir, "8_Cluster_Final_GraphPad.png"), combined_plot, width=8, height=10, dpi=300)

# UpSet图 + 热图 + 结果导出（原功能完整保留）
png(file.path(out_dir, "8_Intersection_UpSetPlot.png"), width=1000, height=800, res=300)
upset(fromList(list(Baseline = baseline_vars, Dual_Effect = dual_vars, Optimal = optimal_vars)), order.by = "freq")
dev.off()

heat_data <- optimal_data %>% column_to_rownames("Variable")
png(file.path(out_dir, "8_Pure_Cluster_Heatmap.png"), width=1600, height=1200, res=300)
pheatmap(heat_data[,c("Stage_AUC","Prognosis_Cindex")], scale = "none", cluster_rows = T, cluster_cols = F, display_numbers = T, main = "Optimal Variables Performance")
dev.off()

write.xlsx(
  list(Optimal_Vars = data.frame(Variables=optimal_vars), 
       Cluster_Result = optimal_data %>% dplyr::select(Variable,Stage_AUC,Prognosis_Cindex,Type,k2,k3,k4)), 
  file.path(out_dir, "8_Integrated_Screening_Results.xlsx")
)

cat("\n✅ 绘制完成！散点图轴范围自定义+自动P值+完整引线已全部生效！\n")