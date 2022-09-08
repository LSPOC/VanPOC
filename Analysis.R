# Libraries

library(car)
library(ggplot2)
library(dplyr)
library(HH)
library(Metrics)
library(patchwork)
library(pastecs)

# Tables

VanPOCLl <- read.csv("VanPOCRLlopis.csv")
VanPOCR <- read.csv("VanPOCRRevilla.csv")
VanPOCG <- read.csv("VanPOCRGoti.csv")
VanPOCAV <- read.csv("VanPOCAV.csv")

# Summary

stat.desc(VanPOCLl)
stat.desc(VanPOCR)
stat.desc(VanPOCG)

# Shapiro–Wilk Test (test of normality)

shapiro.test(VanPOCLl$TDM)
shapiro.test(VanPOCLl$Estimated)
shapiro.test(VanPOCR$Estimated)
shapiro.test(VanPOCG$Estimated)
shapiro.test(VanPOCLl$Predicted)
shapiro.test(VanPOCR$Predicted)
shapiro.test(VanPOCG$Predicted)

# Wilcox Test

wilcox.test(x = VanPOCLl$TDM, y = VanPOCLl$Estimated, alternative = "two.sided", mu = 0, paired = FALSE, conf.int = 0.95)
wilcox.test(x = VanPOCR$TDM, y = VanPOCR$Estimated, alternative = "two.sided", mu = 0, paired = FALSE, conf.int = 0.95)
wilcox.test(x = VanPOCG$TDM, y = VanPOCG$Estimated, alternative = "two.sided", mu = 0, paired = FALSE, conf.int = 0.95)
wilcox.test(x = VanPOCLl$TDM, y = VanPOCLl$Predicted, alternative = "two.sided", mu = 0, paired = FALSE, conf.int = 0.95)
wilcox.test(x = VanPOCR$TDM, y = VanPOCR$Predicted, alternative = "two.sided", mu = 0, paired = FALSE, conf.int = 0.95)
wilcox.test(x = VanPOCG$TDM, y = VanPOCG$Predicted, alternative = "two.sided", mu = 0, paired = FALSE, conf.int = 0.95)

# Box Plot
Group <- VanPOCAV$GROUP
Test <- VanPOCAV$TEST
data <- data.frame(Group, Test)
ch <- ggplot(data = data, aes(x = Group, y = Test, fill=Group)) +
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="none")+
  labs(title = "Figure 1. Boxplot for estimates, predictions and trough concentration.", x = NULL, y = "mg/L", caption = "\n (E): Estimates; (P): Predictions.")

ch + scale_fill_manual(values=c("#13649b",
                                "#13649b",
                                "#6e298e",
                                "#6e298e",
                                "#00ab56",
                                "#00ab56",
                                "gray"))

# Bland-Altman plot for estimates.

bias <- ((VanPOCLl$Estimated - VanPOCLl$TDM)/((VanPOCLl$Estimated + VanPOCLl$TDM) / 2))*100
mean <- (VanPOCLl$Estimated + VanPOCLl$TDM) / 2
data <- data.frame(VanPOCLl$Estimated, VanPOCLl$TDM, bias, mean)
# summary(data)
BA_plot <- ggplot(data = data, aes(x = mean, y = bias)) +
  geom_point(pch = 1, size = 1.5, col = "black") +
  labs(title = NULL, x = NULL, y = "Bias %") +
  ylim(-200, 200) +
  xlim(0, 40) +
  # Bias line
  geom_hline(yintercept = mean(data$bias), lwd = 1) +
  # Line in y=0
  geom_hline(yintercept = 0, lwd = 0.5, lty = 5, col = "green") +
  # Limits of Agreement
  geom_hline(yintercept = mean(data$bias) + 1.96 * sd(data$bias), lty = 2, col = "firebrick") +
  geom_hline(yintercept = mean(data$bias) - 1.96 * sd(data$bias), lty = 2, col = "firebrick") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(label = paste(round(mean(data$bias),2), "%"), x = max(data$mean)+5, y = mean(data$bias) + 20, size = 3, colour = "black") +
  geom_text(label = "+1.96 SD", x = max(data$mean)+5, y = (mean(data$bias) + 1.96 * sd(data$bias)) + 20, size = 3, colour = "firebrick") +
  geom_text(label = "-1.96 SD", x = max(data$mean)+5, y = (mean(data$bias) - 1.96 * sd(data$bias)) + 20, size = 3, colour = "firebrick") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

LlopisE <- BA_plot + geom_smooth(method = "lm", se = TRUE, fill = "lightgrey", lwd = 0.1, lty = 5)

bias <- ((VanPOCR$Estimated - VanPOCR$TDM)/((VanPOCR$Estimated + VanPOCR$TDM) / 2))*100
mean <- (VanPOCR$Estimated + VanPOCR$TDM) / 2
data <- data.frame(VanPOCR$Estimated, VanPOCR$TDM, bias, mean)
BA_plot <- ggplot(data = data, aes(x = mean, y = bias)) +
  geom_point(pch = 1, size = 1.5, col = "black") +
  labs(title = NULL, x = NULL, y = NULL) +
  ylim(-200, 200) +
  xlim(0, 40) +
  # Bias line
  geom_hline(yintercept = mean(data$bias), lwd = 1) +
  # Line in y=0
  geom_hline(yintercept = 0, lwd = 0.5, lty = 5, col = "green") +
  # Limits of Agreement
  geom_hline(yintercept = mean(data$bias) + 1.96 * sd(data$bias), lty = 2, col = "firebrick") +
  geom_hline(yintercept = mean(data$bias) - 1.96 * sd(data$bias), lty = 2, col = "firebrick") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(label = paste(round(mean(data$bias),2), "%"), x = max(data$mean)+5, y = mean(data$bias) + 20, size = 3, colour = "black") +
  geom_text(label = "+1.96 SD", x = max(data$mean)+5, y = (mean(data$bias) + 1.96 * sd(data$bias)) + 20, size = 3, colour = "firebrick") +
  geom_text(label = "-1.96 SD", x = max(data$mean)+5, y = (mean(data$bias) - 1.96 * sd(data$bias)) + 20, size = 3, colour = "firebrick") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

RevillaE <- BA_plot + geom_smooth(method = "lm", se = TRUE, fill = "lightgrey", lwd = 0.1, lty = 5)

bias <- ((VanPOCG$Estimated - VanPOCG$TDM)/((VanPOCG$Estimated + VanPOCG$TDM) / 2))*100
mean <- (VanPOCG$Estimated + VanPOCG$TDM) / 2
data <- data.frame(VanPOCG$Estimated, VanPOCG$TDM, bias, mean)
BA_plot <- ggplot(data = data, aes(x = mean, y = bias)) +
  geom_point(pch = 1, size = 1.5, col = "black") +
  labs(title = NULL, x = NULL, y = NULL) +
  ylim(-200, 200) +
  xlim(0, 40) +
  # Bias line
  geom_hline(yintercept = mean(data$bias), lwd = 1) +
  # Line in y=0
  geom_hline(yintercept = 0, lwd = 0.5, lty = 5, col = "green") +
  # Limits of Agreement
  geom_hline(yintercept = mean(data$bias) + 1.96 * sd(data$bias), lty = 2, col = "firebrick") +
  geom_hline(yintercept = mean(data$bias) - 1.96 * sd(data$bias), lty = 2, col = "firebrick") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(label = paste(round(mean(data$bias),2), "%"), x = max(data$mean)+5, y = mean(data$bias) + 20, size = 3, colour = "black") +
  geom_text(label = "+1.96 SD", x = max(data$mean)+5, y = (mean(data$bias) + 1.96 * sd(data$bias)) + 20, size = 3, colour = "firebrick") +
  geom_text(label = "-1.96 SD", x = max(data$mean)+5, y = (mean(data$bias) - 1.96 * sd(data$bias)) + 20, size = 3, colour = "firebrick") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

GotiE <- BA_plot + geom_smooth(method = "lm", se = TRUE, fill = "lightgrey", lwd = 0.1, lty = 5)

accuracy <- (VanPOCLl$Estimated/VanPOCLl$TDM)*100
mean <- (VanPOCLl$Estimated + VanPOCLl$TDM) / 2
data <- data.frame(VanPOCLl$Estimated, VanPOCLl$TDM, accuracy, mean)
# summary(data)
BA_plot <- ggplot(data = data, aes(x = mean, y = accuracy)) +
  geom_point(pch = 1, size = 1.5, col = "black") +
  labs(title = "Llopis et al.", x = NULL, y = "Accuracy %") +
  ylim(-70, 350) +
  xlim(0, 40) +
  # Bias line
  geom_hline(yintercept = mean(data$accuracy), lwd = 1) +
  # Line in y=0
  geom_hline(yintercept = 100, lwd = 0.5, lty = 5, col = "green") +
  # Limits of Agreement
  geom_hline(yintercept = mean(data$accuracy) + 1.96 * sd(data$accuracy), lty = 2, col = "firebrick") +
  geom_hline(yintercept = mean(data$accuracy) - 1.96 * sd(data$accuracy), lty = 2, col = "firebrick") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(label = paste(round(mean(data$accuracy),2), "%"), x = max(data$mean)+5, y = mean(data$accuracy) + 20, size = 3, colour = "black") +
  geom_text(label = "+1.96 SD", x = max(data$mean)+5, y = (mean(data$accuracy) + 1.96 * sd(data$accuracy)) + 20, size = 3, colour = "firebrick") +
  geom_text(label = "-1.96 SD", x = max(data$mean)+5, y = (mean(data$accuracy) - 1.96 * sd(data$accuracy)) + 20, size = 3, colour = "firebrick") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

LlopisEa <- BA_plot + geom_smooth(method = "lm", se = TRUE, fill = "lightgrey", lwd = 0.1, lty = 5)

accuracy <- (VanPOCR$Estimated/VanPOCR$TDM)*100
mean <- (VanPOCR$Estimated + VanPOCR$TDM) / 2
data <- data.frame(VanPOCR$Estimated, VanPOCR$TDM, accuracy, mean)
# summary(data)
BA_plot <- ggplot(data = data, aes(x = mean, y = accuracy)) +
  geom_point(pch = 1, size = 1.5, col = "black") +
  labs(title = "Revilla et al.", x = NULL, y = NULL) +
  ylim(-70, 350) +
  xlim(0, 40) +
  # Bias line
  geom_hline(yintercept = mean(data$accuracy), lwd = 1) +
  # Line in y=0
  geom_hline(yintercept = 100, lwd = 0.5, lty = 5, col = "green") +
  # Limits of Agreement
  geom_hline(yintercept = mean(data$accuracy) + 1.96 * sd(data$accuracy), lty = 2, col = "firebrick") +
  geom_hline(yintercept = mean(data$accuracy) - 1.96 * sd(data$accuracy), lty = 2, col = "firebrick") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(label = paste(round(mean(data$accuracy),2), "%"), x = max(data$mean)+5, y = mean(data$accuracy) + 20, size = 3, colour = "black") +
  geom_text(label = "+1.96 SD", x = max(data$mean)+5, y = (mean(data$accuracy) + 1.96 * sd(data$accuracy)) + 20, size = 3, colour = "firebrick") +
  geom_text(label = "-1.96 SD", x = max(data$mean)+5, y = (mean(data$accuracy) - 1.96 * sd(data$accuracy)) + 20, size = 3, colour = "firebrick") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

RevillaEa <- BA_plot + geom_smooth(method = "lm", se = TRUE, fill = "lightgrey", lwd = 0.1, lty = 5)

accuracy <- (VanPOCG$Estimated/VanPOCG$TDM)*100
mean <- (VanPOCG$Estimated + VanPOCG$TDM) / 2
data <- data.frame(VanPOCG$Estimated, VanPOCG$TDM, accuracy, mean)
# summary(data)
BA_plot <- ggplot(data = data, aes(x = mean, y = accuracy)) +
  geom_point(pch = 1, size = 1.5, col = "black") +
  labs(title = "Goti et al.", x = NULL, y = NULL) +
  ylim(-70, 350) +
  xlim(0, 40) +
  # Bias line
  geom_hline(yintercept = mean(data$accuracy), lwd = 1) +
  # Line in y=0
  geom_hline(yintercept = 100, lwd = 0.5, lty = 5, col = "green") +
  # Limits of Agreement
  geom_hline(yintercept = mean(data$accuracy) + 1.96 * sd(data$accuracy), lty = 2, col = "firebrick") +
  geom_hline(yintercept = mean(data$accuracy) - 1.96 * sd(data$accuracy), lty = 2, col = "firebrick") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(label = paste(round(mean(data$accuracy),2), "%"), x = max(data$mean)+5, y = mean(data$accuracy) + 20, size = 3, colour = "black") +
  geom_text(label = "+1.96 SD", x = max(data$mean)+5, y = (mean(data$accuracy) + 1.96 * sd(data$accuracy)) + 20, size = 3, colour = "firebrick") +
  geom_text(label = "-1.96 SD", x = max(data$mean)+5, y = (mean(data$accuracy) - 1.96 * sd(data$accuracy)) + 20, size = 3, colour = "firebrick") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

GotiEa <- BA_plot + geom_smooth(method = "lm", se = TRUE, fill = "lightgrey", lwd = 0.1, lty = 5)

wrap_plots(LlopisEa, RevillaEa, GotiEa, LlopisE, RevillaE, GotiE) + plot_annotation(title = "Figure 2. Bland-Altman plot for estimates.")

# Bland-Altman plot for predictions.

bias <- ((VanPOCLl$Predicted - VanPOCLl$TDM)/((VanPOCLl$Predicted + VanPOCLl$TDM) / 2))*100
mean <- (VanPOCLl$Predicted + VanPOCLl$TDM) / 2
data <- data.frame(VanPOCLl$Predicted, VanPOCLl$TDM, bias, mean)
# summary(data)
BA_plot <- ggplot(data = data, aes(x = mean, y = bias)) +
  geom_point(pch = 1, size = 1.5, col = "black") +
  labs(title = NULL, x = NULL, y = "Bias %") +
  ylim(-200, 200) +
  xlim(0, 40) +
  # Bias line
  geom_hline(yintercept = mean(data$bias), lwd = 1) +
  # Line in y=0
  geom_hline(yintercept = 0, lwd = 0.5, lty = 5, col = "green") +
  # Limits of Agreement
  geom_hline(yintercept = mean(data$bias) + 1.96 * sd(data$bias), lty = 2, col = "firebrick") +
  geom_hline(yintercept = mean(data$bias) - 1.96 * sd(data$bias), lty = 2, col = "firebrick") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(label = paste(round(mean(data$bias),2), "%"), x = max(data$mean)+5, y = mean(data$bias) + 20, size = 3, colour = "black") +
  geom_text(label = "+1.96 SD", x = max(data$mean)+5, y = (mean(data$bias) + 1.96 * sd(data$bias)) + 20, size = 3, colour = "firebrick") +
  geom_text(label = "-1.96 SD", x = max(data$mean)+5, y = (mean(data$bias) - 1.96 * sd(data$bias)) + 20, size = 3, colour = "firebrick") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

LlopisE <- BA_plot + geom_smooth(method = "lm", se = TRUE, fill = "lightgrey", lwd = 0.1, lty = 5)

bias <- ((VanPOCR$Predicted - VanPOCR$TDM)/((VanPOCLl$Predicted + VanPOCR$TDM) / 2))*100
mean <- (VanPOCR$Predicted + VanPOCR$TDM) / 2
data <- data.frame(VanPOCR$Predicted, VanPOCLl$TDM, bias, mean)
BA_plot <- ggplot(data = data, aes(x = mean, y = bias)) +
  geom_point(pch = 1, size = 1.5, col = "black") +
  labs(title = NULL, x = NULL, y = NULL) +
  ylim(-200, 200) +
  xlim(0, 40) +
  # Bias line
  geom_hline(yintercept = mean(data$bias), lwd = 1) +
  # Line in y=0
  geom_hline(yintercept = 0, lwd = 0.5, lty = 5, col = "green") +
  # Limits of Agreement
  geom_hline(yintercept = mean(data$bias) + 1.96 * sd(data$bias), lty = 2, col = "firebrick") +
  geom_hline(yintercept = mean(data$bias) - 1.96 * sd(data$bias), lty = 2, col = "firebrick") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(label = paste(round(mean(data$bias),2), "%"), x = max(data$mean)+5, y = mean(data$bias) + 20, size = 3, colour = "black") +
  geom_text(label = "+1.96 SD", x = max(data$mean)+5, y = (mean(data$bias) + 1.96 * sd(data$bias)) + 20, size = 3, colour = "firebrick") +
  geom_text(label = "-1.96 SD", x = max(data$mean)+5, y = (mean(data$bias) - 1.96 * sd(data$bias)) + 20, size = 3, colour = "firebrick") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

RevillaE <- BA_plot + geom_smooth(method = "lm", se = TRUE, fill = "lightgrey", lwd = 0.1, lty = 5)

bias <- ((VanPOCG$Predicted - VanPOCG$TDM)/((VanPOCG$Predicted + VanPOCG$TDM) / 2))*100
mean <- (VanPOCG$Predicted + VanPOCG$TDM) / 2
data <- data.frame(VanPOCLl$Predicted, VanPOCLl$TDM, bias, mean)
BA_plot <- ggplot(data = data, aes(x = mean, y = bias)) +
  geom_point(pch = 1, size = 1.5, col = "black") +
  labs(title = NULL, x = NULL, y = NULL) +
  ylim(-200, 200) +
  xlim(0, 40) +
  # Bias line
  geom_hline(yintercept = mean(data$bias), lwd = 1) +
  # Line in y=0
  geom_hline(yintercept = 0, lwd = 0.5, lty = 5, col = "green") +
  # Limits of Agreement
  geom_hline(yintercept = mean(data$bias) + 1.96 * sd(data$bias), lty = 2, col = "firebrick") +
  geom_hline(yintercept = mean(data$bias) - 1.96 * sd(data$bias), lty = 2, col = "firebrick") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(label = paste(round(mean(data$bias),2), "%"), x = max(data$mean)+5, y = mean(data$bias) + 20, size = 3, colour = "black") +
  geom_text(label = "+1.96 SD", x = max(data$mean)+5, y = (mean(data$bias) + 1.96 * sd(data$bias)) + 20, size = 3, colour = "firebrick") +
  geom_text(label = "-1.96 SD", x = max(data$mean)+5, y = (mean(data$bias) - 1.96 * sd(data$bias)) + 20, size = 3, colour = "firebrick") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

GotiE <- BA_plot + geom_smooth(method = "lm", se = TRUE, fill = "lightgrey", lwd = 0.1, lty = 5)

accuracy <- (VanPOCLl$Predicted/VanPOCLl$TDM)*100
mean <- (VanPOCLl$Predicted + VanPOCLl$TDM) / 2
data <- data.frame(VanPOCLl$Predicted, VanPOCLl$TDM, accuracy, mean)
# summary(data)
BA_plot <- ggplot(data = data, aes(x = mean, y = accuracy)) +
  geom_point(pch = 1, size = 1.5, col = "black") +
  labs(title = "Llopis et al.", x = NULL, y = "Accuracy %") +
  ylim(-70, 350) +
  xlim(0, 40) +
  # Bias line
  geom_hline(yintercept = mean(data$accuracy), lwd = 1) +
  # Line in y=0
  geom_hline(yintercept = 100, lwd = 0.5, lty = 5, col = "green") +
  # Limits of Agreement
  geom_hline(yintercept = mean(data$accuracy) + 1.96 * sd(data$accuracy), lty = 2, col = "firebrick") +
  geom_hline(yintercept = mean(data$accuracy) - 1.96 * sd(data$accuracy), lty = 2, col = "firebrick") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(label = paste(round(mean(data$accuracy),2), "%"), x = max(data$mean)+5, y = mean(data$accuracy) + 20, size = 3, colour = "black") +
  geom_text(label = "+1.96 SD", x = max(data$mean)+5, y = (mean(data$accuracy) + 1.96 * sd(data$accuracy)) + 20, size = 3, colour = "firebrick") +
  geom_text(label = "-1.96 SD", x = max(data$mean)+5, y = (mean(data$accuracy) - 1.96 * sd(data$accuracy)) + 20, size = 3, colour = "firebrick") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

LlopisEa <- BA_plot + geom_smooth(method = "lm", se = TRUE, fill = "lightgrey", lwd = 0.1, lty = 5)

accuracy <- (VanPOCR$Predicted/VanPOCR$TDM)*100
mean <- (VanPOCR$Predicted + VanPOCR$TDM) / 2
data <- data.frame(VanPOCR$Predicted, VanPOCR$TDM, accuracy, mean)
# summary(data)
BA_plot <- ggplot(data = data, aes(x = mean, y = accuracy)) +
  geom_point(pch = 1, size = 1.5, col = "black") +
  labs(title = "Revilla et al.", x = NULL, y = NULL) +
  ylim(-70, 350) +
  xlim(0, 40) +
  # Bias line
  geom_hline(yintercept = mean(data$accuracy), lwd = 1) +
  # Line in y=0
  geom_hline(yintercept = 100, lwd = 0.5, lty = 5, col = "green") +
  # Limits of Agreement
  geom_hline(yintercept = mean(data$accuracy) + 1.96 * sd(data$accuracy), lty = 2, col = "firebrick") +
  geom_hline(yintercept = mean(data$accuracy) - 1.96 * sd(data$accuracy), lty = 2, col = "firebrick") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(label = paste(round(mean(data$accuracy),2), "%"), x = max(data$mean)+5, y = mean(data$accuracy) + 20, size = 3, colour = "black") +
  geom_text(label = "+1.96 SD", x = max(data$mean)+5, y = (mean(data$accuracy) + 1.96 * sd(data$accuracy)) + 20, size = 3, colour = "firebrick") +
  geom_text(label = "-1.96 SD", x = max(data$mean)+5, y = (mean(data$accuracy) - 1.96 * sd(data$accuracy)) + 20, size = 3, colour = "firebrick") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

RevillaEa <- BA_plot + geom_smooth(method = "lm", se = TRUE, fill = "lightgrey", lwd = 0.1, lty = 5)

accuracy <- (VanPOCG$Predicted/VanPOCG$TDM)*100
mean <- (VanPOCG$Predicted + VanPOCG$TDM) / 2
data <- data.frame(VanPOCG$Predicted, VanPOCG$TDM, accuracy, mean)
# summary(data)
BA_plot <- ggplot(data = data, aes(x = mean, y = accuracy)) +
  geom_point(pch = 1, size = 1.5, col = "black") +
  labs(title = "Goti et al.", x = NULL, y = NULL) +
  ylim(-70, 350) +
  xlim(0, 40) +
  # Bias line
  geom_hline(yintercept = mean(data$accuracy), lwd = 1) +
  # Line in y=0
  geom_hline(yintercept = 100, lwd = 0.5, lty = 5, col = "green") +
  # Limits of Agreement
  geom_hline(yintercept = mean(data$accuracy) + 1.96 * sd(data$accuracy), lty = 2, col = "firebrick") +
  geom_hline(yintercept = mean(data$accuracy) - 1.96 * sd(data$accuracy), lty = 2, col = "firebrick") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(label = paste(round(mean(data$accuracy),2), "%"), x = max(data$mean)+5, y = mean(data$accuracy) + 20, size = 3, colour = "black") +
  geom_text(label = "+1.96 SD", x = max(data$mean)+5, y = (mean(data$accuracy) + 1.96 * sd(data$accuracy)) + 20, size = 3, colour = "firebrick") +
  geom_text(label = "-1.96 SD", x = max(data$mean)+5, y = (mean(data$accuracy) - 1.96 * sd(data$accuracy)) + 20, size = 3, colour = "firebrick") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

GotiEa <- BA_plot + geom_smooth(method = "lm", se = TRUE, fill = "lightgrey", lwd = 0.1, lty = 5)

wrap_plots(LlopisEa, RevillaEa, GotiEa, LlopisE, RevillaE, GotiE) + plot_annotation(title = "Figure 3. Bland-Altman plot for predictions.")

