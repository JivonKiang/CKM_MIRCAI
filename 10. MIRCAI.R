# 加载必备包
library(ggplot2)
library(ggalluvial)
library(dplyr)

# ===================== 1. 基础指标定义 =====================
core_list <- c("TyG","UHR","AIP","CMI","HCHR","METS_IR","SHR","eGDR","eGFR")
derived_list <- c(
  "Waist\nCircumference", "Waist-to-Height\nRatio", "Body Mass\nIndex", 
  "A Body Shape\nIndex", "Body Roundness\nIndex", "Conicity\nIndex", 
  "C-Reactive\nProtein", "HDL-Cholesterol"
)

# 生成完整数据
full_data <- expand.grid(core = core_list, derived = derived_list) %>% 
  mutate(freq = 1)

# 拆分：乘法数据(72条)、除法数据(72条)
data_mul <- full_data  # 乘法：红色
data_div <- full_data  # 除法：蓝色

# ===================== 2. 神经网络风格绘图函数 =====================
plot_nn <- function(data, color, title, filename){
  p <- ggplot(data, aes(y = freq, axis1 = core, axis2 = derived)) +
    # 神经网络流线：纯色柔和线条，无杂乱感
    geom_alluvium(
      fill = color, color = "white", alpha = 0.8, lwd = 0.4, knot.pos = 0.4
    ) +
    # 节点样式：纯白底色，细边框，模拟神经网络神经元
    geom_stratum(fill = "white", color = "#EEEEEE", lwd = 0.5, width = 0.35) +
    # 超大加粗文字，图小字大
    geom_text(
      stat = "stratum", aes(label = after_stat(stratum)),
      size = 7.5, fontface = "bold", color = "#222222"
    ) +
    # 双栏标题（神经网络层）
    scale_x_discrete(limits = c("Core\nIndicators", "Derived\nIndicators"), expand = c(0.05, 0.05)) +
    # 极简主题：纯黑背景/纯白背景二选一（默认纯白，更高级）
    theme_void() +
    theme(
      plot.title = element_text(size = 22, face = "bold", hjust = 0.5, margin = margin(b=20)),
      plot.margin = margin(30, 60, 30, 60)
    ) +
    labs(title = title)
  
  # 保存高清图片
  ggsave(filename, p, width=14, height=10, dpi=300, bg="white")
  return(p)
}

# ===================== 3. 分别绘制乘法(红色)、除法(蓝色)图 =====================
# 1️⃣ 乘法图（红色 - 神经网络风格）
plot_nn(
  data = data_mul,
  color = "#E74C3C",  # 高级红
  title = "MIRCAI Multiplication Logic (72 Derived Indicators)",
  filename = "MIRCAI_Multiplication_NN.png"
)

# 2️⃣ 除法图（蓝色 - 神经网络风格）
plot_nn(
  data = data_div,
  color = "#3498DB",  # 高级蓝
  filename = "MIRCAI_Division_NN.png",
  title = "MIRCAI Division Logic (72 Derived Indicators)"
)

cat("✅ 两张神经网络风格图表已保存完成！\n",
    "1. 乘法图：MIRCAI_Multiplication_NN.png\n",
    "2. 除法图：MIRCAI_Division_NN.png\n",
    "保存路径：", getwd())