library(tidyquant)
library(tidyverse)

data <- tq_get(c("^GSPC"), from = "2007-01-01",
               to   = "2019-01-01")
vix <-  tq_get(c("^VIX"), from = "2007-01-01",
               to   = "2020-05-12") 

data <- data_backuo %>%  left_join(vix %>% filter(open <= 25) %>%  select(date) %>%  mutate(vix = "vix")) %>% filter(vix == "vix")
kurtasjen <- 1

data_kjop_close_open <- 
  data %>% mutate(close_open = "Ja",
                  kjop = lag(adjusted),
                  salg = open) %>% 
  filter(!is.na(kjop)) %>% 
  mutate(avkastning = (salg - kjop)*kurtasjen/kjop,
         total = purrr::map_dbl(1:nrow(.), ~ Return.cumulative(avkastning[1:.x])))

data_kjop_open_close <- 
  data %>%
  slice(2:nrow(.)) %>% 
  mutate(close_open = "Nei",
         kjop = open,
         salg = adjusted) %>% 
  mutate(avkastning = (salg - kjop)*kurtasjen/kjop,
         total = purrr::map_dbl(1:nrow(.), ~ Return.cumulative(avkastning[1:.x])))


plot_data <- bind_rows(data_kjop_close_open, data_kjop_open_close)

d_ends <- plot_data %>% 
  group_by(close_open) %>% 
  top_n(1, total) %>% 
  pull(total) %>%  round(.,2)

plot_data %>%
  mutate(Handelsmønster = case_when(close_open == "Ja" ~ "Kjøp Close",
                                    TRUE  ~ "Kjøp Open")) %>% 
  ggplot(aes(x = date, y = total, group = close_open, color=Handelsmønster)) +
  geom_line() +
  labs(title = "Strategi", 
       subtitle = "Kjøp close og selg open vs. kjøpe open og selge close (1% kurtasje)",
       caption = "Alle close-kurser er justerte",
       y = "Avkastning", x = "") +
  scale_y_continuous(sec.axis = sec_axis(~ ., breaks = d_ends)) +
  theme_tq()


