# source: Paris 2024 Judo, results book
# https://olympics.com/en/paris-2024/reports/judo
# https://olympics.com/OG2024/pdf/OG2024/JUD/OG2024_JUD_B99_JUD-------------------------------.pdf

library(tidyverse)
library(ggtext)

# data in -----------------------------------------------------------------

df <- read_csv("data/agg-data.csv")

df_win<- df %>% 
  filter(type == "winning")

df <- df %>% 
  filter(type == "all") %>% 
  mutate(shido = case_when(substr(code, 1, 1) == "P" ~ "Y", 
                           TRUE ~ "N"))

lookup <- read_csv("data/technique-lookup.csv")

# colour palette ----------------------------------------------------------

paris_blue <- "#022366"
paris_pink <- "#FBA9CB"
paris_lightblue <- "#149DDE"
paris_gold <- "#D5C278"

# theme -------------------------------------------------------------------

lm_theme <- function(){
  
  theme_minimal() %+replace%
    
    theme(text = element_text(family = "sans"),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 13),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12, 
                                      face = "bold"),
          title = element_text(size = 14, 
                               face = "bold"), 
          plot.subtitle = element_text(size = 12, 
                                       face = "italic",
                                       hjust = 0),
          plot.caption = element_text(size = 11, 
                                      face = "italic",
                                      hjust = 0)
    )
  
  
}

# penalty vs non-penalty --------------------------------------------------

df_pen <- df %>% 
  summarise(sum_shido = sum(count, na.rm = TRUE),
            .by = c("sex", "class", "shido")) %>% 
  mutate(perc_shido = sum_shido / sum(sum_shido, na.rm = TRUE),
         ymax = cumsum(perc_shido), 
         ymin = case_when(shido == "Y" ~ 0, 
                          TRUE ~ head(ymax, n = -1)),
         .by = c("sex", "class"),
         class = factor(class, levels = c("u48", "u52", "u57", "u63", "u70", "u78", "o78", 
                                          "u60", "u66", "u73", "u81", "u90", "u100", "o100")))

# calculate what proportion of everything was a shido
df_pen %>% 
  summarise(sum_shido = sum(sum_shido, na.rm = TRUE),
            .by = c("shido")) %>% 
  mutate(perc_shido = sum_shido / sum(sum_shido, na.rm = TRUE))

# plot these by weight class
shido_plot <- df_pen %>% 
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = shido)) + 
  geom_rect() + 
  coord_polar(theta = "y") + 
  xlim(c(2, 4)) + 
  facet_wrap(~class, nrow = 2) + 
  scale_fill_manual(values = c(paris_pink, paris_blue)) + 
  theme_void() +
  geom_text(data = df_pen %>% filter(shido == "Y"), 
            aes(label = paste0(round(100*perc_shido, 1), "%"),
                x = 2, y = 0), 
            color = paris_blue) + 
  theme(legend.position = "none",
        title = element_text(color = paris_blue, face = "bold"))

# pareto chart of techniques ----------------------------------------------

# wrangle the data
df_par <- df %>% 
  summarise(count = sum(count, na.rm = TRUE), 
            .by = c(technique, shido)) %>% 
  arrange(-count) %>% 
  mutate(cum_sum = cumsum(count), 
         cum_perc = cum_sum/sum(count, na.rm = TRUE), 
         id = row_number())

# plot the data
coeff <- max(df_par$count)

par_plot <- df_par %>% 
  left_join(lookup, by = "technique") %>% 
  filter(cum_perc <= 0.9) %>% 
  ggplot(aes(x = reorder(disp_name, -id), fill = shido)) +
  scale_y_continuous(name = "Instances of technique", 
                     labels = scales::percent, 
                     sec.axis = sec_axis(trans = ~.*coeff, name = "")) +
  geom_bar(aes(y = count/coeff),
           stat = "identity") +
  geom_line(aes(y = cum_perc), 
            group = 1,
            linetype = "dashed", 
            col = "black", 
            linewidth = 1) +
  coord_flip() + 
  lm_theme() + 
  theme(axis.title.y = element_blank(), 
        legend.position = "none") +
  scale_fill_manual(values = c(paris_blue, paris_pink)) +
  labs(title = "Are there too many penalties in Judo?", 
       subtitle = "2/3 of 'techniques' at Paris 2024 were penalties", 
       caption = "Source: https://olympics.com/en/paris-2024/reports/judo - top 90% shown")

# look at the same by winning technique
df_par_win <- df_win %>% 
  left_join(lookup) %>% 
  summarise(count = sum(count, na.rm = TRUE), 
            .by = c(technique, tech_type)) %>% 
  arrange(-count) %>% 
  mutate(cum_sum = cumsum(count), 
         cum_perc = cum_sum/sum(count, na.rm = TRUE), 
         id = row_number())

# plot the data
coeff_win <- max(df_par_win$count)

par_plot_win <- df_par_win %>% 
  left_join(lookup) %>% 
  filter(cum_perc <= 0.9) %>% 
  ggplot(aes(x = reorder(disp_name, -id), fill = tech_type)) +
  scale_y_continuous(name = "Instances of technique", 
                     labels = scales::percent,
                     sec.axis = sec_axis(trans = ~.*coeff_win, name = "")) +
  geom_bar(aes(y = count/coeff_win), 
           stat = "identity") +
  geom_line(aes(y = cum_perc), 
            group = 1,
            linetype = "dashed", 
            col = "black", 
            linewidth = 1) +
  coord_flip() + 
  lm_theme() + 
  theme(axis.title.y = element_blank(), 
        legend.position = "none") +
  scale_fill_manual(values = c(paris_gold, paris_blue, paris_lightblue)) +
  labs(title = "So did anyone actually throw anyone?",
       subtitle = "Seoi Nage and Juji Gatame were the top contest winning techniques", 
       caption = "Source: https://olympics.com/en/paris-2024/reports/judo - top 90% shown")

# most popular (non-penalty) technique by class ---------------------------

df_tw <- df_win %>% 
  left_join(lookup, by = c("technique", "code")) %>% 
  filter(tech_type != "O") %>% 
  summarise(sum_tech = sum(count, na.rm = TRUE),
            .by = c("sex", "class", "tech_type")) %>% 
  select(-sex) %>% 
  complete(class, tech_type) %>% 
  arrange(desc(tech_type)) %>% 
  mutate(sum_tech = ifelse(is.na(sum_tech), 0, sum_tech),
         perc_tech = sum_tech / sum(sum_tech, na.rm = TRUE),
         ymax = cumsum(perc_tech), 
         ymin = case_when(tech_type == "T" ~ 0, 
                          TRUE ~ head(ymax, n = -1)),
         .by = c("class"),
         class = factor(class, levels = c("u48", "u52", "u57", "u63", "u70", "u78", "o78", 
                                          "u60", "u66", "u73", "u81", "u90", "u100", "o100")))

# standing vs groundwork plot
tachi_waza_plot <- df_tw %>% 
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = tech_type)) + 
  geom_rect() + 
  coord_polar(theta = "y") + 
  xlim(c(2, 4)) + 
  facet_wrap(~class, nrow = 2) + 
  scale_fill_manual(values = c(paris_gold, paris_lightblue)) + 
  theme_void() +
  geom_text(data = df_tw %>% filter(tech_type == "T"), 
            aes(label = paste0(round(100*perc_tech, 1), "%"),
                x = 2, y = 0), 
            color = paris_blue) + 
  theme(legend.position = "none") +
  labs(caption = "Note: FSG excluded")
