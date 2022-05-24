library("tidyverse")
library("vtable")

# Import cleaned dataset
df <- read_csv("data/clean_flag.csv")

# Display summary statistics from vtable package
sumtable(df)

# Fig 1. Number of Colours on a Country's Flag, Separated by Hemisphere
ggplot(data = df, mapping = aes(x = as.character(colours))) +
  geom_bar((aes(y = ..prop.., group = 1)), position = "dodge") +
  ylim(0, 0.5) +
  facet_grid(cols = vars(zone)) +
  labs(title = "Number of Colours on a Country's Flag, Separated by Hemisphere",
       subtitle = "Hemispheres numbered 1-4, where 1=NE, 2=SE, 3=SW, 4=NW",
       x = "Number of Colours",
       y = "Frequency") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Fig 2. Number of Colours on a Country's Flag, Separated by Continent
ggplot(data = df, mapping = aes(x = as.character(colours))) +
  geom_bar((aes(y = ..prop.., group = 1)), position = "dodge") +
  scale_y_continuous(breaks=seq(0, .6, .1)) +
  facet_grid(cols = vars(landmass)) +
  labs(title = "Number of Colours on a Country's Flag, Separated by Continent",
       subtitle = "Continents numbered 1-6, where 1=N.America, 2=S.America, 3=Europe, 4=Africa, 5=Asia, 6=Oceania",
       x = "Number of Colours",
       y = "Frequency") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Fig 3. Boxplot of Number of Colours on a Country's Flag, Separated by Continent
ggplot(data = df) +
  geom_boxplot(mapping = aes(x = as.character(landmass), y = colours), fill = "gray") +
  scale_y_continuous(breaks=seq(0, 8, 1)) +
  labs(title = "Number of Colours on a Country's Flag, Separated by Continent",
       subtitle = "Continents numbered 1-6, where 1=N.America, 2=S.America, 3=Europe, 4=Africa, 5=Asia, 6=Oceania",
       x = "Continent",
       y = "Number of Colours") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Vector to arrange continents East -> West
z <- c(5,4,3,2,1,6)

# New dataframe to organize landmass by z, and calculate mean of each colour by landmass
df_west_colour <- df %>%
  mutate(landmass = factor(landmass, levels = z)) %>%
  group_by(landmass) %>%
  mutate(blue_mean = mean(blue),
         red_mean = mean(red),
         white_mean = mean(white),
         gold_mean = mean(gold),
         green_mean = mean(green),
         black_mean = mean(black),
         orange_mean = mean(orange)) %>%
  summarize(landmass,
            blue, blue_mean,
            red, red_mean,
            white, white_mean,
            gold, gold_mean,
            green, green_mean,
            black, black_mean,
            orange, orange_mean)

# Fig 4. Frequency of Blue on National Flags, Separated by Continent, Arranged East to West
ggplot(data = df_west_colour) +
  geom_jitter(mapping = aes(x = landmass, y = blue),
              width = .04,
              height = .04,
              size = 7,
              alpha = .15) +
  geom_smooth(mapping = aes(x = as.double(landmass), y = blue_mean),
              color = "blue",
              size = 2,
              se = F) +
  labs(title = "Frequency of Blue on National Flags, Separated by Continent, Arranged East to West",
       subtitle = "Continents numbered 1-6, where 5=Asia, 4=Africa, 3=Europe, 2=S.America, 1=N.America, 6=Oceania",
       x = "Continent",
       y = "Presence of Blue") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Fig 5. Frequency of Red on National Flags, Separated by Continent, Arranged East to West
ggplot(data = df_west_colour) +
  geom_jitter(mapping = aes(x = landmass, y = red), width = .04, height = .04, size = 7, alpha = .15) +
  geom_smooth(mapping = aes(x = as.double(landmass), y = red_mean), color="red", size=2, se=F) + 
  labs(title = "Frequency of Red on National Flags, Separated by Continent, Arranged East to West",
       subtitle = "Continents numbered 1-6, where 5=Asia, 4=Africa, 3=Europe, 2=S.America, 1=N.America, 6=Oceania",
       x = "Continent",
       y = "Presence of Red") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Fig 6. Frequency of White on National Flags, Separated by Continent, Arranged East to West
ggplot(data = df_west_colour) +
  geom_jitter(mapping = aes(x = landmass, y = white), width = .04, height = .04, size = 7, alpha = .15) +
  geom_smooth(mapping = aes(x = as.double(landmass), y = white_mean), color="black", size=3, se=F) +
  geom_smooth(mapping = aes(x = as.double(landmass), y = white_mean), color="white", size=2, se=F) +
  labs(title = "Frequency of White on National Flags, Separated by Continent, Arranged East to West",
       subtitle = "Continents numbered 1-6, where 5=Asia, 4=Africa, 3=Europe, 2=S.America, 1=N.America, 6=Oceania",
       x = "Continent",
       y = "Presence of White") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Fig 7. Frequency of Gold on National Flags, Separated by Continent, Arranged East to West
ggplot(data = df_west_colour) +
  geom_jitter(mapping = aes(x = landmass, y = gold), width = .04, height = .04, size = 7, alpha = .15) +
  geom_smooth(mapping = aes(x = as.double(landmass), y = gold_mean), color="gold", size=2, se=F) +
  theme_bw() +
  labs(title = "Frequency of Gold on National Flags, Separated by Continent, Arranged East to West",
       subtitle = "Continents numbered 1-6, where 5=Asia, 4=Africa, 3=Europe, 2=S.America, 1=N.America, 6=Oceania",
       x = "Continent",
       y = "Presence of Gold") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Fig 8. Frequency of Green on National Flags, Separated by Continent, Arranged East to West
ggplot(data = df_west_colour) +
  geom_jitter(mapping = aes(x = landmass, y = green), width = .04, height = .04, size = 7, alpha = .15) +
  geom_smooth(mapping = aes(x = as.double(landmass), y = green_mean), color="green", size=2, se=F) + 
  theme_bw() +
  labs(title = "Frequency of Green on National Flags, Separated by Continent, Arranged East to West",
       subtitle = "Continents numbered 1-6, where 5=Asia, 4=Africa, 3=Europe, 2=S.America, 1=N.America, 6=Oceania",
       x = "Continent",
       y = "Presence of Green") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Fig 9. Frequency of Black on National Flags, Separated by Continent, Arranged East to West
ggplot(data = df_west_colour) +
  geom_jitter(mapping = aes(x = landmass, y = black), width = .04, height = .04, size = 7, alpha = .15) +
  geom_smooth(mapping = aes(x = as.double(landmass), y = black_mean), color="black", size=2, se=F) + 
  theme_bw() +
  labs(title = "Frequency of Black on National Flags, Separated by Continent, Arranged East to West",
       subtitle = "Continents numbered 1-6, where 5=Asia, 4=Africa, 3=Europe, 2=S.America, 1=N.America, 6=Oceania",
       x = "Continent",
       y = "Presence of Black") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Fig 10. Frequency of Orange on National Flags, Separated by Continent, Arranged East to West
ggplot(data = df_west_colour) +
  geom_jitter(mapping = aes(x = landmass, y = orange), width = .04, height = .04, size = 7, alpha = .15) +
  geom_smooth(mapping = aes(x = as.double(landmass), y = orange_mean), color="orange", size=2, se=F) + 
  theme_bw() +
  labs(title = "Frequency of Orange on National Flags, Separated by Continent, Arranged East to West",
       subtitle = "Continents numbered 1-6, where 5=Asia, 4=Africa, 3=Europe, 2=S.America, 1=N.America, 6=Oceania",
       x = "Continent",
       y = "Presence of Orange") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())
