library(tidyverse)
library(readxl)
library(xts)
library(readr)

X2019_General_Results <- read_excel("data/2019-General-Results.xlsx", 
                                    sheet = "Registered Voters")
names(X2019_General_Results) <- str_replace_all(names(X2019_General_Results), " ","_")

MCDC_Results_Municipal_2019_Turnout <- read_csv("data/MCDC Results Municipal 2019 - Turnout.csv")
names(MCDC_Results_Municipal_2019_Turnout) <- str_replace_all(names(MCDC_Results_Municipal_2019_Turnout), " ", "_")

MCDC_Results_Municipal_2019_Turnout <- MCDC_Results_Municipal_2019_Turnout %>% 
  rename(H8 = "8:00", H10 ="10:00", H12 = "12:00", H14 = "2:00", H16 = "4:00", H18 = "6:00") %>% 
  as.data.frame() %>% 
  filter(Municipality != "Florham Park") %>% 
  mutate(District = str_replace(District, "-", " ")) %>% 
  mutate(District = ifelse(Municipality %in% c("Boonton", "Dover", "Roxbury"),
         paste(Municipality, "Town Ward", substr(District, 1,1), "District", 
               substr(District, nchar(District)-1, nchar(District) )),
               District)) %>% 
  mutate(District = ifelse( !(Municipality %in% c("Boonton", "Dover", "Roxbury")),
                       paste( Municipality,
                              "District",
                              substr(District, nchar(District)-1, nchar(District) )), District)) %>% 
  mutate(District = str_replace(District, "Harding", "Harding Township")) %>% 
  mutate(District = str_replace(District, "Morris Twp", "Morris Township")) %>% 
  mutate(District = str_replace(District, "Morris Plains", "Morris Plains Borough")) %>%  
  mutate(District = str_replace(District, "Parsippany", "Parsippany - Troy Hills Township")) %>% 
  mutate(District = str_replace(District, "Randolph", "Randolph Township")) %>% 
  mutate(District = str_replace(District, "Roxbury Town", "Roxbury Township")) %>% 
  mutate(District = str_replace(District, " Boro ", " Borough ")) %>% 
  mutate(District = str_replace(District, " Twp ", " Township ")) %>% 
  mutate(District = str_squish(District))


MCDC_Results_Municipal_2019_Turnout <- MCDC_Results_Municipal_2019_Turnout %>% left_join(X2019_General_Results) 

MCDC_Results_Municipal_2019_Turnout_long <- MCDC_Results_Municipal_2019_Turnout%>% 
  mutate(H6 = 0, 
         H20 = Ballots_Cast) %>% 
  pivot_longer(cols = starts_with("H"), names_to = "Hour", values_to = "Cumulative_Votes_Cast") %>% 
  mutate(Hour = as.numeric(str_replace(Hour, "H", ""))) %>% 
  arrange(Municipality, District, Hour) %>% 
  select(Municipality, District, Hour, Cumulative_Votes_Cast, Registered_Voters, Ballots_Cast, Voter_Turnout) %>% 
  mutate(Cum_NA_flag = is.na(Cumulative_Votes_Cast))


MCDC_Results_Municipal_2019_Turnout_long_filled <- MCDC_Results_Municipal_2019_Turnout_long %>% 
  group_by(District) %>% mutate(Fill = na.approx(Cumulative_Votes_Cast)) %>% 
  select(Municipality, District, Hour, Cumulative_Votes_Cast, Fill, Cum_NA_flag, Ballots_Cast) %>% 
  mutate(Diff = Fill - lag(Fill)) %>%
  mutate(Prior_Hour_Flag = lag(Cum_NA_flag), 
         Est_Diff_Flag = Cum_NA_flag | Prior_Hour_Flag) %>% 
  ungroup() %>% 
  mutate(Municipality = str_replace(Municipality, "Boonton", "Boonton Town"))
 
# Cumulative Votes 
MCDC_Results_Municipal_2019_Turnout_long_filled %>% filter(Municipality == "Chester Twp") %>% 
  ggplot() + geom_bar(stat= "identity", aes(x=as.factor(Hour), y = Fill, fill = Cum_NA_flag))+ 
  labs(x = "Two Hour Block", y = "Ballots Cast", fill = "Interpolated Value", 
       title = "Cumulated Votes Cast per Two Hours") + 
  facet_wrap(~District)+ 
  theme(legend.position = "bottom")

# Percent Cumulative 
MCDC_Results_Municipal_2019_Turnout_long_filled %>% filter(Municipality == "Chester Twp") %>% 
  mutate(Percent_Cumulative = 100 * Fill/ Ballots_Cast) %>% 
  ggplot() + geom_bar(stat= "identity", aes(x=as.factor(Hour), y = Percent_Cumulative, fill = Cum_NA_flag))+ 
  labs(x = "Two Hour Block", y = "% Ballots Cast", fill = "Interpolated Value", 
       title = "Percent Cumulated Votes Cast per Two Hours") + 
  facet_wrap(~District)+ 
  theme(legend.position = "bottom")

# Votes by 2 Hours
MCDC_Results_Municipal_2019_Turnout_long_filled %>% filter(Municipality == "Chester Twp") %>% 
  ggplot() + geom_bar(stat= "identity", aes(x=as.factor(Hour), y = Diff, fill = Est_Diff_Flag))+ 
  labs(x = "Two Hour Block", y = "Ballots Cast", fill = "Interpolated Value", 
       title = "Votes Cast per Two Hour Block") + 
  facet_wrap(~District) + 
  theme(legend.position = "bottom")

# Percent Votes by 2 Hours
MCDC_Results_Municipal_2019_Turnout_long_filled %>% filter(Municipality == "Chester Twp") %>% 
  mutate(Percent_Cast = 100 * Diff/ Ballots_Cast) %>% 
  ggplot() + geom_bar(stat= "identity", aes(x=as.factor(Hour), y = Percent_Cast, fill = Est_Diff_Flag))+ 
  labs(x = "Two Hour Block", y = "Percent Ballots Cast", fill = "Interpolated Value", 
       title = "Percent Votes Cast per Two Hour Block") + 
  facet_wrap(~District) + 
  theme(legend.position = "bottom")

# Percent Votes by 2 Hours - All Districts 


temp <- MCDC_Results_Municipal_2019_Turnout_long_filled %>% 
  filter(Municipality == "Mendham Boro", Hour != 6) %>% 
  mutate(Percent_Cast = 100 * Diff/ Ballots_Cast, 
         District = str_replace(District, "Chester Township District ", "") ) %>% 
  ggplot() + #geom_point( aes(x=Hour, y = Percent_Cast, color = District))+ 
  geom_bar( stat ="identity", aes(x=Hour, y = Percent_Cast, fill = District), position ="dodge")+ 
  labs(x = "Two Hour Block", y = "Percent Ballots Cast", fill = "Diff", 
       title = paste("Percent Votes Cast per Two Hour Block", "Mendham Borough")) + 
  scale_y_continuous(limits = c(0,30)) + 
  scale_x_continuous(breaks = seq(8, 20, by =2), labels = seq(8, 20, by =2)) + 
  facet_wrap(~District, ncol =1) +
  theme_bw() + 
  theme(legend.position = "none")