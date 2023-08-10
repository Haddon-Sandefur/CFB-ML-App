# Libs
library(tidyverse)
library(cfbplotR)

# Etc.
year = 2022
df <- read.csv(paste("../downstream/gamesModified", year, "Condensed.csv", sep = ""))

# Mutation
df2 <- df %>% 
       group_by(conference.x, logo, logo_2, school) %>% 
       filter(classification == "fbs",
              week > 10,
              conference.x %in% c("ACC", 
                                  "Big Ten", 
                                  "Big 12", 
                                  "Pac-12",
                                  "SEC")) %>% 
       mutate(`Correct Bet` =  if_else(cover == coverPred, 1, 0)) %>% 
       summarise(`Correct Bet Average` =  mean(`Correct Bet`, na.rm = T))


ggplot(data = df2, aes(x = school, y = `Correct Bet Average`)) +
geom_col(aes(fill = school, color = school),size = 1.5) +
scale_fill_cfb(alpha = .8) +
scale_color_cfb(alt_colors = df2$school) +
geom_hline(yintercept=.5, linetype="dashed", color = "red", size = 1.5) +
labs(x = "", y = "Correct Bet Average") +
theme_bw() +
theme(axis.text.x = element_cfb_logo()) +
facet_wrap(vars(conference.x), nrow = 3, scales = "free") 
    
