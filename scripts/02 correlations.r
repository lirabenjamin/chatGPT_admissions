library(tidyverse)
library(arrow)
library(psych)

gpt = read_csv("data/gpt_ratings.csv") %>%rename(raw = `0`) %>% mutate(rater = 1423)
gpt2 = read_csv("data/gpt_ratings_2.csv") %>%rename(raw = `0`)%>% mutate(rater = 2345)
gpt3 = read_csv("data/gpt_ratings_3.csv")

gpt  <- bind_rows(gpt, gpt2, gpt3)

development = read_rds("data/development.rds")

gpt = gpt %>%
  mutate(score = parse_number(raw),
          name = case_when(
            str_starts(tolower(raw), "lear") ~ "mastery_gpt",
            str_starts(tolower(raw), "goal") ~ "goal_gpt",
            str_starts(tolower(raw), "intrin") ~ "selfconcordance_gpt",
            str_starts(tolower(raw), "proso") ~ "prosocial_gpt",
            str_starts(tolower(raw), "team") ~ "teamwork_gpt",
            str_starts(tolower(raw), "perse") ~ "perseverance_gpt",
            str_starts(tolower(raw), "leadership") ~ "leadership_gpt",
            T ~ "error"
          )) %>%
  filter(name != "error") %>% 
  select(id, name, score,rater)


# correlation between raters
gpt %>% 
  distinct() %>%
  group_by(id,name) %>%
  mutate(rater = letters[1:n()]) %>%
  pivot_wider(names_from = rater, values_from = score) %>%
  group_by(name) %>%
  summarise(r = cor(a,b, use = "pairwise.complete.obs"))

alphas  <- gpt %>% 
  distinct() %>%
  group_by(id,name) %>%
  mutate(rater = letters[1:n()]) %>%
  pivot_wider(names_from = rater, values_from = score) %>%
  group_by(name) %>%
  select(-id) %>%
  nest() %>% 
  mutate(alpha = map(data, ~ psych::alpha(.)$total$raw_alpha)) %>% 
  unnest(alpha) %>% 
  select(name, alpha)

write_parquet(alphas, "output/alphas.parquet")

gpt  <- 
gpt %>% 
  mutate(score = score / 100) %>%
  unique() %>%
  group_by(id,name) %>% 
  summarise_all(mean, na.rm = TRUE) %>%
  select(-rater) %>% 
  ungroup() %>%
  pivot_wider(names_from = name, values_from = score)

gpt %>%
  left_join(development, by = "id") %>% 
  select(id, response,matches("gpt"), matches("ms"), matches("hr")) %>%
  pivot_longer(3:ncol(.)) %>% 
  separate(name, into = c("name", "source"), sep = "_") %>%
  pivot_wider(names_from = source, values_from = value) %>%
  mutate(dif = gpt - hr) %>%
  arrange(desc(abs(dif))) %>% 
  group_by(name) %>%
  slice_max(abs(dif), n = 5) %>%
  gt::gt()

correlations <- 
  gpt %>% 
  left_join(development, by = "id") %>%
  select(-id,-response) %>%
  fastDummies::dummy_cols() %>%
  select_if(is.numeric) %>%
  select(sort(colnames(.))) %>% 
  corrr::correlate() %>%
  corrr::stretch() %>%
  separate(x, into = c("x", "x_source"), sep = "_") %>% 
  separate(y, into = c("y", "y_source"), sep = "_")

# Correlations with itself
correlations %>% filter(x_source == "gpt" & y_source == "hr" & x == y) %>% summarise(mean = mean(abs(r)))
correlations %>% filter(x_source == "gpt" & y_source == "ms" & x == y) %>% summarise(mean = mean(abs(r)))
same  <- correlations %>% filter(x_source == "gpt" & y_source == "hr" & x == y) 


# Discriminant Correlations with other vars
correlations %>% filter(x_source == "gpt" & y_source == "ms" & x != y) %>% summarise(mean = mean(abs(r)))
correlations %>% filter(x_source == "gpt" & y_source == "hr" & x != y) %>% summarise(mean = mean(abs(r)))
dif = correlations %>% filter(x_source == "gpt" & y_source == "ms" & x != y) %>% 
group_by(x) %>%
summarise(mean = mean(abs(r)))

same %>% 
  left_join(dif) %>% 
  mutate(x = fct_reorder(x, r)) %>%
  ggplot(aes(x = reorder(x, r), y = r)) +
  geom_col(position = "dodge")+
  facet_grid(. ~ x, scales = "free_x")+
  geom_hline(aes(yintercept = mean), linetype = "dashed", color = "red")+
  labs(x = "", y = "Correlation to Human Coder")+
  geom_text(
    aes(label = round(r, 2)), 
    vjust = -0.5, size = 3,
    )+
  theme(
  strip.text = element_blank()
  )
ggsave("plots/correlations.png", width = 5, height = 3)



# Correlations with demographics
demo = c("ell","female","parentmarried","title1hs","race","parentdegree")
demo  <- correlations %>% 
  filter(
    x_source %in% c("gpt","hr") & 
    !(y_source %in% c("ms","hr","gpt")),
    y %in% demo)


    
demo %>%
  mutate(x_source = factor(x_source, levels = c("hr", "gpt"))) %>%
  ggplot(aes(x_source, r)) +
  # geom_point(alpha = .2) +
  geom_line(alpha = .1, aes(group = paste(y, y_source))) +
  # geom_point(data = demo %>% group_by(x, x_source) %>% summarise(r = mean(r)), 
  #   size = 2) +
  geom_line(data = demo %>% group_by(x, x_source) %>% summarise(r = mean(r)), 
    aes(group = 1), size = 2) +
  geom_text(data = demo %>% group_by(x, x_source) %>% summarise(r = mean(r)), 
    aes(label = Ben::numformat(r)), size = 3, vjust = 1.75) +
  facet_grid(. ~ x, scales = "free_x") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "red") +
  labs(x = "", , y = "r", title = "Correlation between ratings and demographic")
ggsave("plots/correlations_demo.png", width = 6, height = 4)

demo %>%
  unite(y, y, y_source, sep = "_") %>%
  mutate(x_source = factor(x_source, levels = c("hr", "gpt"))) %>%
  ggplot(aes(x_source, r)) +
  geom_line(alpha = .1, aes(group = paste(x,y))) +
  geom_line(data = demo %>%  unite(y, y, y_source, sep = "_") %>% group_by(y,x_source) %>% summarise(r = mean(r)), 
    aes(group = 1), size = 2) +
  geom_text(data = demo %>%  unite(y, y, y_source, sep = "_") %>% group_by(y,x_source) %>% summarise(r = mean(r)), 
    aes(label = Ben::numformat(r)), size = 3, vjust = 1.75) +
  facet_wrap(. ~ y, scales = "free_x") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "red") +
  labs(x = "", , y = "r", title = "Correlation between ratings and demographic")

demo %>%
  unite(y, y, y_source, sep = "_") %>%
  mutate(x_source = factor(x_source, levels = c("hr", "gpt"))) %>%
  ggplot(aes(x_source, r)) +
  geom_line(alpha = .1, aes(group = paste(x,y))) +
  geom_line(data = demo %>%  unite(y, y, y_source, sep = "_") %>% group_by(x_source) %>% summarise(r = mean(r)), 
    aes(group = 1), size = 2) +
  geom_text(data = demo %>%  unite(y, y, y_source, sep = "_") %>% group_by(x_source) %>% summarise(r = mean(r)), 
    aes(label = Ben::numformat(r)), size = 3, vjust = 1.75) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "red") +
  labs(x = "", , y = "r", title = "Correlation between ratings and demographic")

# make a histogram of the word count of the essays
development %>% 
  mutate(word_count = str_count(response, "\\w+")) %>% 
  ggplot(aes(word_count)) +
  geom_histogram() +
  labs(x = "Word Count", y = "Frequency", title = "Word Count of Essays")

# Correlations with outcomes
out = c("grad6")
out  <- correlations %>% 
  filter(
    x_source %in% c("gpt","hr") & 
    !(y_source %in% c("ms","hr","gpt")),
    y %in% out)

out %>%
  mutate(x_source = factor(x_source, levels = c("hr", "gpt"))) %>%
  ggplot(aes(x_source, r)) +
  # geom_point(alpha = .2) +
  geom_col() +
    geom_text(data = out %>% group_by(x, x_source) %>% summarise(r = mean(r)), 
    aes(label = Ben::numformat(r)), size = 3, vjust = 1.75) +
  facet_grid(. ~ x, scales = "free_x") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "red") +
  labs(x = "", , y = "r", title = "Correlation between ratings and outcome")
ggsave("plots/correlations_out.png", width = 6, height = 4)


get_cor = function(data,x,y){
  x = data %>% pull(x)
  y = data  %>% pull(y)
  cor = cor.test(x,y, method = "pearson")
  tidy = cor %>% broom::tidy()
  return(tidy)
}

data = gpt %>% 
  left_join(development, by = "id") %>%
  select(-id,-response) %>%
  fastDummies::dummy_cols() %>%
  select_if(is.numeric) %>%
  select(sort(colnames(.)))

data %>% select(ends_with("gpt"),ends_with("hr")) %>% 
colnames() %>% 
enframe() %>% expand_grid(out) %>% 
mutate(cor = map2(value, out, get_cor, data = data)) %>% 
unnest(cor) %>% 
separate(value, into = c("x", "x_source"), sep = "_") %>%
ggplot(aes(x = x, y = estimate, color = x_source)) +
geom_point(position = position_dodge(width = .5)) +
facet_grid(. ~ out) +
geom_text(aes(label = Ben::numformat(estimate)), size = 3, vjust = 1.75,
position = position_dodge(width = .5)) +
geom_hline(aes(yintercept = 0), linetype = "dashed", color = "red") +
labs(x = "", , y = "r", col ="", title = "Correlation between ratings and outcome")+
geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .1,
  position = position_dodge(width = .5))+ 
  theme(
  strip.text = element_blank(),
  legend.position = c(.9,.9)
  )+
  scale_color_manual(values = c("gpt" = "darkred", "hr" = "gray40"))
ggsave("plots/correlations_out.png", width = 6, height = 4)
