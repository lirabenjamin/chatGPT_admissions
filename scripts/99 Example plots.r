# Example plots

# Make a plot that shows that human ratings correlate with chatbot generated ratings

# Make a plot that has number of raters in the x axis, and reliability in the y axis. reliability should increase logarithmically.
library(ggplot2)

library(ggplot2)

# create a data frame with number of raters and reliability values for 7 constructs
df <- data.frame(num_raters = rep(1:10, each = 7)) %>% 
mutate(reliability = .36 + log(num_raters)/5 + rnorm(70,0, sd = .01),
                 construct = rep(paste0("Construct ", 1:7), times = 10))
                 

# create the plot with 7 lines
ggplot(data = df, aes(x = num_raters, y = reliability, color = construct)) + 
  geom_line() + 
  scale_color_discrete(guide = guide_legend(title = "Construct")) + 
  labs(x = "Number of Raters", y = "Reliability", 
       title = "Relationship between Number of Raters and Reliability") + 
  ylim(0, 1) + 
  scale_x_continuous(breaks = 1:10)
ggsave("reliability.png", width = 6, height = 4, units = "in", dpi = 300)

library(ggplot2)

library(ggplot2)

# create a sample data frame with 8 demographic dimensions, correlation coefficients, and coder type
df <- data.frame(demographic = rep(c("Gender", "Race", "Age", "Education", "Income", "Occupation", "Region", "Religion"), each = 2*7*2), 
                 coder = rep(rep(c("ChatGPT", "Human Coder"), each = 2*7), 8), 
                 construct = rep(rep(paste0("Construct ", 1:7), each = 2*2), 8), 
                 correlation = rnorm(8*7*2*2, mean = 0.5, sd = 0.1))

df = expand_grid(demographic = c("Gender", "Race", "Age", "Education", "Income", "Occupation", "Region", "Religion"),
coder = c("ChatGPT", "Human Coder"),
construct = paste0("Construct ", 1:7)) %>% 
mutate(correlation = rnorm(nrow(.), mean = 0, sd = 0.1))

# create a color palette with red and blue
my_colors <- c("red", "blue")

# create the plot with facets for each construct
ggplot(data = df, aes(x = correlation, y = demographic, color = coder)) + 
  geom_point(position = position_dodge2(preserve = "single", width = 0.5), size = 2.5) + 
  geom_errorbar(aes(xmin = correlation - 0.1, xmax = correlation + 0.1), position = position_dodge(preserve = "single", width = 0.5), width = 0.2) + 
  facet_wrap(~ construct, nrow = 1) + 
  scale_color_manual(values = my_colors, breaks = c("ChatGPT", "Human Coder"), labels = c("ChatGPT", "Human Coder"), name = "Coder Type") + 
  scale_fill_manual(values = my_colors) +
  labs(x = "Correlation Coefficient", y = "", title = "Comparison of Correlation Coefficients between ChatGPT and Human Coder Ratings by Demographic Dimension") + 
  theme_bw() + 
  theme(panel.spacing = unit(0.5, "lines"), strip.background = element_blank(), strip.text = element_text(face = "bold"), legend.position = "bottom")
ggsave("demographics.png", width = 6, height = 4, units = "in", dpi = 300)


df = expand_grid(outcome = c("College Graduation (4yr)", "College Graduation (6yr)"),
coder = c("ChatGPT", "Human Coder"),
construct = paste0("Construct ", 1:7)) %>% 
mutate(correlation = rnorm(nrow(.), mean = 0.4, sd = 0.1))

# create a color palette with red and blue
my_colors <- c("red", "blue")

# create the plot with facets for each construct
ggplot(data = df, aes(x = correlation, y = construct, color = coder)) + 
  geom_point(position = position_dodge2(preserve = "single", width = 0.5), size = 2.5) + 
  geom_errorbar(aes(xmin = correlation - 0.1, xmax = correlation + 0.1), position = position_dodge(preserve = "single", width = 0.5), width = 0.2) + 
  facet_wrap(~ outcome, nrow = 1) + 
  scale_color_manual(values = my_colors, breaks = c("ChatGPT", "Human Coder"), labels = c("ChatGPT", "Human Coder"), name = "Coder Type") + 
  scale_fill_manual(values = my_colors) +
  labs(x = "Correlation Coefficient", y = "", title = "Comparison of Correlation Coefficients between ChatGPT and Human Coder Ratings to Outcomes") + 
  theme_bw() + 
  coord_flip() +
  theme(axis.text.x  = element_text(angle = 15, hjust = 1))+
  theme(panel.spacing = unit(0.5, "lines"), strip.background = element_blank(), strip.text = element_text(face = "bold"), legend.position = "bottom")
ggsave("outcomes.png", width = 6, height = 4, units = "in", dpi = 300)
