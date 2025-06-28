library(ggpol)
library(patchwork)

m1 <- ggplot(data = group_results_filtered, aes(x = session_type, y = mratio, fill = session_type)) +
  geom_boxjitter(outlier.shape = NA, width = 0.5, shape = 21, alpha = 0.3) + 
  theme_custom()

m2 <- ggplot(data = group_results_filtered, aes(x = session_type, y = auroc, fill = session_type)) +
  geom_boxjitter(outlier.shape = NA, width = 0.5, shape = 21, alpha = 0.3) + 
  theme_custom()

d<-ggplot(data = group_results_filtered, aes(x = session_type, y = mean_accuracy, fill = session_type)) +
  geom_boxjitter(outlier.shape = NA, width = 0.5, shape = 21, alpha = 0.3) + 
  theme_custom()

c<-ggplot(data = group_results_filtered, aes(x = session_type, y = mean_confidence, fill = session_type)) +
  geom_boxjitter(outlier.shape = NA, width = 0.5, shape = 21, alpha = 0.3) + 
  theme_custom()

b<-ggplot(data = group_results_filtered, aes(x = session_type, y = estimated_threshold, fill = session_type)) +
  geom_boxjitter(outlier.shape = NA, width = 0.5, shape = 21, alpha = 0.3) + 
  theme_custom()

a<-ggplot(data = group_results_filtered, aes(x = session_type, y = estimated_slope, fill = session_type)) +
  geom_boxjitter(outlier.shape = NA, width = 0.5, shape = 21, alpha = 0.3) + 
  theme_custom()

(c + d) / (b + a) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

m1 + m2 + plot_layout(guides = "collect") & theme(legend.position = 'bottom')