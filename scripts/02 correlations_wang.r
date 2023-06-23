library(tidyverse)
library(arrow)
library(psych)

gpt  <- read_csv("data/wang/gpt_ratings.csv")
data = read_csv("data/wang/wang.csv")

data %>% 
  rename(id = ID) %>%
  left_join(gpt  , by = "id") %>%
  select(Meta_cat, Specific_cat, specific_gpt = raw) %>% 
  # remove periods and make lowercase
  mutate(Specific_cat = str_replace_all(Specific_cat, "\\.", ""),
         Specific_cat = str_to_lower(Specific_cat),
         specific_gpt = str_replace_all(specific_gpt, "\\.", ""),
         specific_gpt = str_to_lower(specific_gpt)) %>% 
  mutate(main_gpt = case_when(
    specific_gpt == "creating sustainable products and processes" ~ "Transforming",
    specific_gpt ==  "embracing innovation for sustainability" ~ "Transforming",
    specific_gpt ==  "changing how work is done" ~ "Transforming",
    specific_gpt ==  "choosing responsible alternatives" ~ "Transforming",
    specific_gpt ==  "monitoring environmental impact" ~ "Avoiding harm",
    specific_gpt ==  "preventing pollution" ~ "Avoiding harm",
    specific_gpt ==  "strengthening ecosystems" ~ "Avoiding harm",
    specific_gpt ==  "reducing use" ~ "Conserving",
    specific_gpt ==  "reusing" ~ "Conserving",
    specific_gpt ==  "recycling" ~ "Conserving",
    specific_gpt ==  "repurposing" ~ "Conserving",
    specific_gpt ==  "encouraging and supporting others" ~ "Influencing others",
    specific_gpt ==  "educating and training for sustainability" ~ "Influencing others",
    specific_gpt ==  "instituting programs and policies" ~ "Taking initiative",
    specific_gpt ==  "putting environmental interests first" ~ "Taking initiative",
    specific_gpt ==  "lobbying and activism" ~ "Taking initiative"
    )) %>%
  mutate(eval = Meta_cat == main_gpt) %>%
  #mutate(eval = Specific_cat == specific_gpt) %>%
  count(eval)

gpt  <- gpt %>% 
  separate(raw, letters[1:6], "\\|") %>% 
  mutate_at(vars(b, d, f), parse_number) %>%
  select(id, rater, temp, idea = b, active = d, social = f) %>% 
  mutate_at(vars(idea, active, social), ~ ifelse(is.na(.),0, .)) %>% 
  rename(idea_gpt = idea, active_gpt = active, social_gpt = social)

# correlation between raters
gpt %>% 
  distinct() %>%
  pivot_longer(cols = idea_gpt:social_gpt, names_to = "name", values_to = "score") %>%
  group_by(id,name) %>%
  mutate(rater = letters[1:n()]) %>%
  pivot_wider(names_from = rater, values_from = score) %>%
  group_by(name) %>%
  summarise(r = cor(a,b, use = "pairwise.complete.obs"))

alphas  <- gpt %>% 
  distinct() %>%
  pivot_longer(cols = idea_gpt:social_gpt, names_to = "name", values_to = "score") %>%
  group_by(id,name) %>%
  mutate(rater = letters[1:n()]) %>%
  filter(rater %in% c("a","b")) %>%
  pivot_wider(names_from = rater, values_from = score) %>%
  group_by(name) %>%
  select(-id,-temp) %>%
  nest() %>% 
  mutate(alpha = map(data, ~ psych::alpha(.)$total$raw_alpha)) %>% 
  unnest(alpha) %>% 
  select(name, alpha)

write_parquet(alphas, "output/alphas_rohrer.parquet")

data = data %>%
  rename(idea_hr = idea, active_hr = active, social_hr = social) %>%
  left_join(gpt  %>% select(id, idea_gpt:social_gpt), by = "id") %>%
  # keep final ratings
  select(-variable, -coder, -value) %>%
  distinct()

correlations <- 
  data %>% 
  select(-id,-text) %>%
  select_if(is.numeric) %>%
  select(sort(colnames(.))) %>% 
  corrr::correlate() %>%
  corrr::stretch() %>%
  separate(x, into = c("x", "x_source"), sep = "_") %>% 
  separate(y, into = c("y", "y_source"), sep = "_")

# Correlations with itself
correlations %>% filter(x_source == "gpt" & y_source == "hr" & x == y) %>% summarise(mean = mean(abs(r)))
# correlations %>% filter(x_source == "gpt" & y_source == "ms" & x == y) %>% summarise(mean = mean(abs(r)))
same  <- correlations %>% filter(x_source == "gpt" & y_source == "hr" & x == y) 


# Discriminant Correlations with other vars
correlations %>% filter(x_source == "gpt" & y_source == "hr" & x != y) %>% summarise(mean = mean(abs(r)))
dif = correlations %>% filter(x_source == "gpt" & y_source == "hr" & x != y) %>% 
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
ggsave("plots/correlations_rohrer.png", width = 5, height = 3)



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
data %>% 
  mutate(word_count = str_count(text, "\\w+")) %>% 
  ggplot(aes(word_count)) +
  geom_histogram() +
  labs(x = "Word Count", y = "Frequency", title = "Word Count of Essays")

# Correlations with outcomes
out = c("ls14", "ls15")


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

data %>% 
  select(ends_with("gpt"),ends_with("hr")) %>% 
  colnames() %>% 
  enframe() %>% 
  expand_grid(out) %>% 
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
  legend.position = c(.9,.9)
  )+
  scale_color_manual(values = c("gpt" = "darkred", "hr" = "gray40"))
ggsave("plots/correlations_out_rohrer.png", width = 6, height = 4)
