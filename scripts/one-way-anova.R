#load data
my_data <- PlantGrowth

	
# Show a random sample
set.seed(1234)
dplyr::sample_n(my_data, 10)

#calculate descriptive statistics
library(dplyr)
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )


# Box plots
# ++++++++++++++++++++
# Plot weight by group and color by group
library("ggpubr")
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "tort", "trt2"),
          ylab = "Weight", xlab = "Treatment")

# Mean plots
# ++++++++++++++++++++
# Plot weight by group
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")
ggline(my_data, x = "group", y = "weight", 
       add = c("mean_se", "jitter"), 
       order = c("ctrl", "trt1", "trt2"),
       ylab = "Weight", xlab = "Treatment")


# Compute the analysis of variance
res.aov <- aov(weight ~ group, data = my_data)

# Summary of the analysis
summary(res.aov)

#post-hoc tests
TukeyHSD(res.aov)

pairwise.t.test(my_data$weight, my_data$group,
                 p.adjust.method = "BH")

# 1. Homogeneity of variances
plot(res.aov, 1)

leveneTest(weight ~ group, data = my_data)

oneway.test(weight ~ group, data = my_data)

pairwise.t.test(my_data$weight, my_data$group,
                 p.adjust.method = "BH", pool.sd = FALSE)

# 2. Normality
plot(res.aov, 2)

# Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

kruskal.test(weight ~ group, data = my_data)