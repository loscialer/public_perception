##Public perception data analysis
library("tidyverse")
library("ggplot2")
library("dplyr")
library("ggpubr")
library("forcats")
library("gplots")
library("latticeExtra")
library("grid")
library("vcd")
library("corrplot")
library("stats")
library("ggrepel")
library("brms")
library("extrafont")
library("hrbrthemes")
library("statmod")
pp_df <- read.csv("../data/public_perception_data/public_perception.csv")
age_distribution <- ggplot(data=subset(pp_df, !is.na(Age))) +
  geom_bar(mapping = aes(Age,stat(prop), group = 1)) +
  theme_classic() +
  coord_flip()

gender_distribution <- ggplot (data = pp_df) +
  geom_bar(mapping = aes(x = Gender, y = stat(prop), group = 1)) +
  scale_y_continuous(limits = c(0,0.75))

pp_df <- within(pp_df, 
                Education <- factor(Education, 
                                    levels=names(sort(table(Education), 
                                                      decreasing = TRUE))))
ed_plot <- ggplot (data = pp_df) +
  geom_bar(mapping = aes(x = Education, y = stat(prop), group = 1)) +
  coord_flip()


pp_df <- within(pp_df, 
                main_activity_trim <- factor(main_activity_trim, 
                                             levels=names(sort(table(main_activity_trim), 
                                                               decreasing=F))))
activities_plot <- ggplot(data =subset(pp_df, !is.na(main_activity_trim)), aes(x=main_activity_trim)) +
  geom_bar(mapping = aes(x = main_activity_trim, y = stat(prop), group = 1)) +
  coord_flip()

pp_df <- within(pp_df, 
                visit_freq <- factor(visit_freq, 
                                     levels=names(sort(table(visit_freq), 
                                                       decreasing=F))))
visit_plot <- ggplot(data = subset(pp_df, !is.na(visit_freq))) +
  geom_bar(mapping = aes(x = visit_freq, y = stat(prop), group = 1)) +
  coord_flip()

recreation <- tribble( 
  ~activity, ~count,
  "Scuba", 41,
  "Recreationa fishing", 50,
  "Spearfishing", 20,
  "Sailing", 46, 
  "Snorkelling / Freediving", 64,
  "Boating", 36,
  "Jetski", 8,
  "Kite surfing", 8,
  "Kayak", 2)

rec_pl <- recreation %>% ggplot(aes(x = reorder(activity, count), y = count)) + geom_bar(stat = "identity") + coord_flip()



demographic <- ggarrange(age_distribution, gender_distribution, 
                         ed_plot, activities_plot, visit_plot, 
                         rec_pl, ncol = 2, nrow = 3, labels = c("A", "B", "C", "D", "E", "F"))

demographic
pp_df %>%
  count(Age)
pp_df %>%
  count(Gender)
pp_df %>%
  count(Education)
pp_df %>%
  count(main_activity)
pp_df %>%
  count(visit_freq)

## Change factors levels in dataframe 

Age2 <- fct_collapse(pp_df$Age,
                     young_adults = c("18-24", "25-34"), 
                     middle_aged = c("35-44", "45-54"),
                     older_adults = c("55-64", "65+"))
Education2 <- fct_collapse(pp_df$Education,
                           secondary_education = c("Secondary Education", "Other", "Technical College "),
                           University = c("Bachelor's degree", "Diploma", "Master's degree", "Ph. D"))
seagrass_perception <- fct_collapse(pp_df$seagrass_image,
                                    healthy = c("good","very_good"),
                                    unhealthy = c("poor","very_poor"))
coral_perception <- fct_collapse(pp_df$coral_image,
                                 healthy = c("good","very_good"),
                                 unhealthy = c("poor","very_poor"))
activity_type <- fct_collapse(pp_df$main_activity_trim,
                              above_water = c("Boating", "Jetski","Kayak", "Kite_surfing", "Rec_fish", "Sailing"),
                              below_water = c("Scuba", "Snorkeling_freediving", "Spearfishing"))

visit_freq2 <- fct_collapse(pp_df$visit_freq, 
                            frequent_visitor = c("Every day","About once a week", "Once a month"), 
                            non_frequent_visitor = c("Few times a year", "Once a year", "Less than once a year"))
# Add new column to dataframe with cbind
pp_df <- cbind(pp_df, Age2, Education2, seagrass_perception, coral_perception, activity_type, visit_freq2)

age2_plot <- ggplot(data =subset(pp_df, !is.na(Age2)), aes(x=Age2)) +
  geom_bar(mapping = aes(x = Age2, y = stat(prop), group = 1)) +
  coord_flip()

edu2_plot <- ggplot(data =subset(pp_df, !is.na(Education2)), aes(x=Education2)) +
  geom_bar(mapping = aes(x = Education2, y = stat(prop), group = 1)) +
  coord_flip()

act2_plot <- ggplot(data =subset(pp_df, !is.na(activity_type)), aes(x=activity_type)) +
  geom_bar(mapping = aes(x = activity_type, y = stat(prop), group = 1)) +
  coord_flip()

visit2_plot <- ggplot(data =subset(pp_df, !is.na(visit_freq2)), aes(x=visit_freq2)) +
  geom_bar(mapping = aes(x = visit_freq2, y = stat(prop), group = 1)) +
  coord_flip()

demographic2 <- ggarrange(age2_plot, edu2_plot, act2_plot, visit2_plot, ncol = 2, nrow = 2, labels = c("a","b","c","d"))

demographic2

## What is the public perception of the status of seagrass and coral reef in the GBR? (Image selection responses)
pp_df$seagrass_image <- factor(pp_df$seagrass_image,levels = c("very_poor", "poor", "good", "very_good"))

pl1 <- pp_df %>%
  filter(!is.na(seagrass_image)) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = seagrass_image, y = stat(prop), group = 1 ))

pl2 <- pp_df %>%
  filter(!is.na(seagrass_perception)) %>%
  ggplot() +
  geom_bar(mapping = aes(x = seagrass_perception, y = stat(prop), group = 1 ))

pp_df$coral_image <- factor(pp_df$coral_image,levels = c("very_poor", "poor", "good", "very_good"))

pl3 <- pp_df %>%
  filter(!is.na(coral_image)) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = coral_image, y = stat(prop), group = 1 ))

pl4 <- pp_df %>%
  filter(!is.na(coral_perception)) %>%
  ggplot() +
  geom_bar(mapping = aes(x = coral_perception, y = stat(prop), group = 1 ))

perception <- ggarrange(pl1, pl2, pl3, pl4, ncol = 2, nrow = 2, labels = c("A","B","C","D"))

perception

### Is there a correlation between perception of status and demographic variables?

## Age
# Seagrass
age_sea <- pp_df %>%
  select(Age2, seagrass_perception) %>%
  filter(!is.na(seagrass_perception) & !is.na(Age2))

age_sea <- table(age_sea$Age2, age_sea$seagrass_perception)

balloonplot(t(age_sea), main = "Perception of seagrass status", xlab ="",
            ylab = "", label = F, show.margins = F)

assoc(head(age_sea, 5), shade = TRUE, las=3)
prop.table(age_sea) # cell percentages
prop.table(age_sea, 1) # row percentages
prop.table(age_sea, 2) # column percentages
chis_age <- chisq.test(age_sea)
chis_age
chis_age$observed
round(chis_age$expected,2)
round(chis_age$residuals, 3)

corrplot(chis_age$residuals, is.cor = FALSE)
# Contibution in percentage (%)
contrib <- 100*chis_age$residuals^2/chis_age$statistic
round(contrib, 3)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)

# Corals
age_cor <- pp_df %>%
  select(Age2, coral_perception) %>%
  filter(!is.na(coral_perception) & !is.na(Age2))

age_cor <- table(age_cor$Age2, age_cor$coral_perception)

balloonplot(t(age_cor), main = "Perception of coral status", xlab ="",
            ylab = "", label = F, show.margins = F)

assoc(head(age_cor, 5), shade = TRUE, las=3)
prop.table(age_cor) 
prop.table(age_cor, 1) 
prop.table(age_cor, 2) 
chis_age <- chisq.test(age_cor)
chis_age
chis_age$observed
round(chis_age$expected,2)
round(chis_age$residuals, 3)

corrplot(chis_age$residuals, is.cor = FALSE)
contrib <- 100*chis_age$residuals^2/chis_age$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)

# Correlation between Age and seagrass image choice (p < 0.05). No corr between age and coral perception (p>0.05)

## Education 
# Seagrass

edu_sea1 <- pp_df %>%
  select(Education2, seagrass_perception) %>%
  filter(!is.na(seagrass_perception) & !is.na(Education2))
edu_sea1 <- table(edu_sea1$Education2, edu_sea1$seagrass_perception)

balloonplot(t(edu_sea1), main = "Perception of seagrass status", xlab ="",
            ylab = "", label = F, show.margins = F)
mosaicplot(edu_sea1, shade = TRUE, las=2,
           main = "Perception of seagrass status")

prop.table(edu_sea1) # cell percentages
prop.table(edu_sea1, 1) # row percentages
prop.table(edu_sea1, 2) # column percentages
chis_edu1 <- chisq.test(edu_sea1)
chis_edu1
chis_edu1$observed
round(chis_edu1$expected,2)
round(chis_edu1$residuals, 3)
corrplot(chis_edu1$residuals, is.cor = FALSE)
# Contibution in percentage (%)
contrib_edu1 <- 100*chis_edu1$residuals^2/chis_edu1$statistic
round(contrib_edu1, 3)
# Visualize the contribution
corrplot(contrib_edu1, is.cor = FALSE)

# Corals
edu_cor <- pp_df %>%
  select(Education2, coral_perception) %>%
  filter(!is.na(coral_perception) & !is.na(Education2))
edu_cor <- table(edu_cor$Education2, edu_cor$coral_perception)

balloonplot(t(edu_cor), main = "Perception of coral status", xlab ="",
            ylab = "", label = F, show.margins = F)
assoc(head(edu_cor, 5), shade = TRUE, las=3)

# No correlation between education and image choice

## Activity type
# Seagrass
act_sea1 <- pp_df %>%
  select(activity_type, seagrass_perception) %>%
  filter(!is.na(seagrass_perception) | !is.na(activity_type))
act_sea1 <- table(act_sea1$activity_type, act_sea1$seagrass_perception)

balloonplot(t(act_sea1), main = "Perception of seagrass status", xlab ="",
            ylab = "", label = F, show.margins = F)
mosaicplot(act_sea1, shade = TRUE, las=2,
           main = "Perception of seagrass status")
assoc(head(act_sea1, 5), shade = TRUE, las=3)
prop.table(act_sea1) # cell percentages
prop.table(act_sea1, 1) # row percentages
prop.table(act_sea1, 2) # column percentages
chis_activity1 <- chisq.test(act_sea1)
chis_activity1
chis_activity1$observed
round(chis_activity1$expected,2)
round(chis_activity1$residuals, 3)
corrplot(chis_activity1$residuals, is.cor = FALSE)
# Contibution in percentage (%)
contrib_activity1 <- 100*chis_activity1$residuals^2/chis_activity1$statistic
round(contrib_activity1, 3)
# Visualize the contribution
corrplot(contrib_activity1, is.cor = FALSE)

# Corals
activity_cor <- pp_df %>%
  select(activity_type, coral_perception) %>%
  filter(!is.na(coral_perception) & !is.na(activity_type))
activity_cor <- table(activity_cor$activity_type, activity_cor$coral_perception)

balloonplot(t(activity_cor), main = "Perception of coral status", xlab ="",
            ylab = "", label = F, show.margins = F)
mosaicplot(activity_cor, shade = TRUE, las=2,
           main = "Perception of coral status")
assoc(head(activity_cor, 5), shade = TRUE, las=3)

# Correlation between activity type and seagrass image (p<0.05) / No correlation between activity type and coral uimage choiche

## Visit Frequency
# Seagrass
visit_sea1 <- pp_df %>%
  select(visit_freq2, seagrass_perception) %>%
  filter(!is.na(seagrass_perception) & !is.na(visit_freq2))
visit_sea1 <- table(visit_sea1$visit_freq2, visit_sea1$seagrass_perception)

balloonplot(t(visit_sea1), main = "Perception of seagrass status", xlab ="",
            ylab = "", label = F, show.margins = F)
mosaicplot(visit_sea1, shade = TRUE, las=2,
           main = "Perception of seagrass status")
assoc(head(visit_sea1, 5), shade = TRUE, las=3)

chis_visit1 <- chisq.test(visit_sea1)
chis_visit1

# Corals

visit_cor1 <- pp_df %>%
  select(visit_freq2, coral_perception) %>%
  filter(!is.na(coral_perception) & !is.na(visit_freq2))
visit_cor1 <- table(visit_cor1$visit_freq2, visit_cor1$coral_perception)
balloonplot(t(visit_cor1), main = "Perception of seagrass status", xlab ="",
            ylab = "", label = F, show.margins = F)
mosaicplot(visit_cor1, shade = TRUE, las=2,
           main = "Perception of seagrass status")
assoc(head(visit_cor1, 5), shade = TRUE, las=3)

# No association with visit frequency

## Multicollinearity
# Seagrass $ Corals
sea_cor <- pp_df %>%
  select(seagrass_perception, coral_perception) %>%
  filter(!is.na(coral_perception) & !is.na(seagrass_perception))
sea_cor <- table(sea_cor$seagrass_perception, sea_cor$coral_perception)

balloonplot(t(sea_cor), main = "Perception of seagrass status against coral status", xlab ="",
            ylab = "", label = F, show.margins = F)

assoc(head(sea_cor, 5), shade = TRUE, las=3)

# Education and Age
pp_df %>% 
  filter(!is.na(Age2)) %>%
  count(Age2, Education2) %>%
  ggplot() +
  geom_tile(mapping = aes(x = Age2, y = Education2, fill = n))

age_edu <- pp_df %>%
  select(Age2, Education2) %>%
  filter(!is.na(Age2) & !is.na(Education2))
age_edu <- table(age_edu$Age2, age_edu$Education2)

balloonplot(t(age_edu), main = "Perception of seagrass status", xlab ="",
            ylab = "", label = F, show.margins = F)
assoc(head(age_edu, 5), shade = TRUE, las=3)

chis_age_edu <- chisq.test(age_edu)
chis_age_edu

# Age and activity type (yes)
pp_df %>% 
  filter(!is.na(Age2) & !is.na(activity_type)) %>% 
  count(Age2, activity_type) %>%
  ggplot() +
  geom_tile(mapping = aes(x = Age2, y = activity_type, fill = n))
age_act <- pp_df %>%
  select(Age2, activity_type) %>%
  filter(!is.na(Age2) & !is.na(activity_type))
age_act <- table(age_act$Age2, age_act$activity_type)

mosaicplot(age_act, shade = TRUE, las=2,
           main = "Perception of seagrass status")
assoc(head(age_act, 5), shade = TRUE, las=3)
prop.table(age_act) # cell percentages
prop.table(age_act, 1) # row percentages
prop.table(age_act, 2) # column percentages
chis_age_act <- chisq.test(age_act)
chis_age_act
chis_age_act$observed
round(chis_age_act$expected,2)
round(chis_age_act$residuals, 3)
corrplot(chis_age_act$residuals, is.cor = FALSE)
contrib_age_act <- 100*chis_age_act$residuals^2/chis_age_act$statistic
round(contrib_age_act, 3)
corrplot(contrib_age_act, is.cor = FALSE)

#Age and visit frequency
pp_df %>% 
  filter(!is.na(Age2), !is.na(visit_freq2)) %>%
  count(Age2, visit_freq2) %>% 
  ggplot() +
  geom_tile(mapping = aes(x = Age2, y = visit_freq2, fill = n))
age_freq <- pp_df %>%
  select(Age2, visit_freq2) %>%
  filter(!is.na(Age2) & !is.na(visit_freq2))
age_freq <- table(age_freq$Age2, age_freq$visit_freq2)

balloonplot(t(age_freq), main = "Perception of seagrass status", xlab ="",
            ylab = "", label = F, show.margins = F)
assoc(head(age_freq, 5), shade = TRUE, las=3)

## How much people value seagrass and corals ecosystem services?
stderr <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

ben_imp <- pp_df %>%
  select(ends_with("_ben")) %>%
  sapply(mean, na.rm = T)
ben_imp_SE <- pp_df %>%
  select(ends_with("_ben")) %>%
  sapply(stderr, na.rm = T)

ben_sea <- pp_df %>%
  select(seafood_sea: edu_sea) %>%
  sapply(mean, na.rm = T)
ben_sea_SE <- pp_df %>%
  select(seafood_sea: edu_sea) %>%
  sapply(stderr, na.rm = T)
ben_cor <- pp_df %>%
  select(seafood_cor:edu_cor) %>%
  sapply(mean, na.rm = T)
ben_cor_SE <- pp_df %>%
  select(seafood_cor:edu_cor) %>%
  sapply(stderr, na.rm = T)
benefits <- data.frame(ben_imp, ben_cor, ben_sea, ben_imp_SE, ben_cor_SE, ben_sea_SE)
ben_num <- 1:11
ben_names <- c("Seafood availability", "Recreational fishing", 
               "Biodiveristy",
               "Water clarity",
               "Beaches",
               "Climate change mitigation",
               "Cultural heritage",
               "Coastal protection",
               "Iconic species",
               "Scientific research",
               "Education")

#Find slope and intercept
imp_sea <- lm(ben_imp~ben_sea, data = benefits)
coeff <- coefficients(imp_sea)
coeff
# Equation of the line : 
eq_sea <- paste0("y = ", round(coeff[2],1), "x + ", round(coeff[1],1))

imp_cor <- lm(ben_imp~ben_cor, data = benefits)
coeff2 <- coefficients(imp_cor)
coeff2
# Equation of the line : 
eq_cor <- paste0("y = ", round(coeff2[2],1), "x + ", round(coeff2[1],1))

seagrass_benefits <- benefits %>% 
  ggplot(mapping = aes(ben_imp, ben_sea)) +
  geom_point() +
  geom_abline(intercept = 0.9101300, slope = 0.7718461, color = "red") + 
  ggtitle(eq_sea) +
  theme_classic() + 
  geom_pointrange(aes(xmin = ben_imp - ben_imp_SE, xmax = ben_imp + ben_imp_SE)) + 
  geom_pointrange(aes(ymin = ben_sea - ben_sea_SE, ymax = ben_sea + ben_sea_SE)) + 
  geom_label_repel(aes(label = ben_names), fill = "black", color = "white", size = 2.5) +
  geom_vline(xintercept = mean(ben_imp), linetype = "dashed") +
  geom_hline(yintercept = mean(ben_sea), linetype = "dashed") + 
  xlab("Importance for society") + 
  ylab("Relation to the seagrass habitat") + 
    annotate(geom = "text", x = 4.3, y = 4.8, label = "Seagrass crucial services", family = "serif",
             fontface = "bold") + 
  scale_y_continuous(breaks = c(3, 3.942073, 4, 5), 
                     labels = c("Weak", "3.94", "Moderate", "Strong"), 
                     limits = c(3, 5)) + 
  scale_x_continuous(breaks = c(2, 3, 4, 5), 
                     labels = c("Minimal", "Moderate", "Important", "Very Important"), 
                     limits = c(2,5))

coral_benefits <- benefits %>% 
  ggplot(mapping = aes(ben_imp, ben_cor)) +
  geom_point() +
  geom_abline(intercept = 1.1685085, slope = 0.6837136, color = "red") + 
  ggtitle(eq_cor) +
  geom_label_repel(aes(label = ben_names), fill = "black", color = "white", size = 2.5) +
  theme_classic() + 
  geom_pointrange(aes(xmin = ben_imp - ben_imp_SE, xmax = ben_imp + ben_imp_SE)) + 
  geom_pointrange(aes(ymin = ben_cor - ben_cor_SE, ymax = ben_cor + ben_cor_SE)) +
  geom_vline(xintercept = mean(ben_imp), linetype = "dashed") +
  geom_hline(yintercept = mean(ben_cor), linetype = "dashed") +
  xlab("Importance for society") + 
  ylab("Relation to the coral reef habitat") + 
  annotate(geom = "text", x = 4.3, y = 4.8, label = "Coral crucial services", family = "serif",
           fontface = "bold") +
  scale_y_continuous(breaks = c(3, 4.072313, 4, 5), 
                     labels = c("Weak", "4.07", "Moderate", "Strong"), 
                     limits = c(3, 5)) + 
  scale_x_continuous(breaks = c(2, 3, 4, 5), 
                     labels = c("Minimal", "Moderate", "Important", "Very Important"), 
                     limits = c(2,5))

benefit_pl <- ggarrange(seagrass_benefits, coral_benefits, ncol = 1, nrow = 2, labels = c("A","B"))
benefit_pl

# Main outcome from this graphs: 
# 1) People value a lot the presence of iconic species and biodiveristy in the GBR. They also know that 
# these services are provided by both seagrass and coral reef habitat. Recreational fishing and seafood availability are the least important benefit. 
# However this can be due to the sample population, (young vegetarian and non-fisherman)
# Cultural heritage and provision of beaches are the least associated with habitats (beach because we are in townsville, and cultural heritage because sample pop)


## Benefits - Opportunity for recreational fishing
activity_fish <- pp_df %>% 
  filter(!is.na(main_activity_trim)) %>%
  group_by(main_activity_trim) %>% 
  summarise(a = mean(recr_fish_ben, na.rm = T))
activity_fish
activity_fish %>% 
  filter(main_activity_trim != "Jetski") %>% 
  ggplot(aes(x = reorder(main_activity_trim, a) , y = a)) + 
  geom_bar(stat = "identity") + 
  xlab("main activity performed") +
  ylab("avearge score") +
  ggtitle("Importance of Recreational fishing") +
  coord_flip()
age_fish <- pp_df %>%
  filter(!is.na(Age2)) %>%
  group_by(Age2) %>%
  summarise(b = mean(recr_fish_ben, na.rm = T))
age_fish
age_fish %>% 
  ggplot(aes(x = reorder(Age2, b) , y = b)) + 
  geom_bar(stat = "identity") + 
  coord_flip()

## Benefits - Availability of seafood
activity_seafood <- pp_df %>% 
  filter(!is.na(main_activity_trim) & !is.na(Age2)) %>% 
  group_by(main_activity_trim) %>% 
  summarise(x = mean(seafood_ben, na.rm = T), count = n())

activity_seafood %>% 
  filter(main_activity_trim != "Jetski") %>% 
  ggplot(aes(x = reorder(main_activity_trim, x) , y = x)) + 
  geom_bar(stat = "identity") + 
  xlab("main activity performed") +
  ylab("avearge score") +
  ggtitle("Importance of Seafood availability") +
  coord_flip()
# The benefit of recreational fishing is more important for fishermans but less for every other activity types

# Benefits - Cultural heritage


pp_df %>% 
  filter(!is.na(Age2) & !is.na(recr_fish_ben)) %>% 
  count(Age2, recr_fish_ben) %>% 
  ggplot() + 
  geom_tile(mapping = aes(Age2, recr_fish_ben, fill = n))
pp_df %>% 
  filter(!is.na(main_activity_trim) & !is.na(recr_fish_ben)) %>% 
  count(main_activity_trim, recr_fish_ben) %>% 
  ggplot() + 
  geom_tile(mapping = aes(main_activity_trim, recr_fish_ben, fill = n))
age_fish <- pp_df %>%
  select(Age2, recr_fish_ben) %>%
  filter(!is.na(Age2) & !is.na(recr_fish_ben))
age_fish <- table(age_fish$Age2, age_fish$recr_fish_ben)

balloonplot(t(age_fish), main = "Perception of seagrass status", xlab ="",
            ylab = "", label = F, show.margins = F)
assoc(head(age_fish, 5), shade = TRUE, las=3)


pp_df %>%
  filter(!is.na(recr_fish_ben) | !is.na(main_activity_trim)) %>%
  ggplot() +
  geom_boxplot(mapping = aes(main_activity_trim, recr_fish_ben))


benefits_df %>%
  filter(!is.na(Age2) & !is.na(seafood_ben) & !is.na(main_activity_trim)) %>%
  ggplot() +
  geom_boxplot(mapping = aes(Age2, seafood_ben)) +
  coord_flip()

benefits_df %>%
  filter(!is.na(Age2) & !is.na(seafood_ben) & !is.na(activity_type)) %>%
  ggplot() +
  geom_boxplot(mapping = aes(activity_type, seafood_ben)) +
  coord_flip()

benefits_df %>% 
  filter(!is.na(Age2) & !is.na(seafood_ben) & !is.na(activity_type)) %>%
  ggplot(aes(x = activity_type, y = stat(count), fill = as.factor(seafood_ben))) +
  geom_bar(position = "dodge")

## Benefits - provision of seafood

benefits_df %>%
  filter(!is.na(Age2) & !is.na(recr_fish_ben) & !is.na(activity_type)) %>%
  ggplot() +
  geom_boxplot(mapping = aes(activity_type, recr_fish_ben)) +
  coord_flip()

benefits_df %>%
  filter(!is.na(Age2) & !is.na(recr_fish_ben) & !is.na(activity_type)) %>%
  ggplot() +
  geom_boxplot(mapping = aes(recr_fish_ben), ) +
  coord_flip()

## Word responses
words <- read.csv("../data/public_perception_data/word_responses.csv")
words <- head(words, -25)
words <- words %>%
  select(Age:visit_freq, starts_with("Category"), threat_cor_category_1:threat_cor_category_3, threat_sea_category_1:threat_sea_category_3, ends_with("_gbr"))
words <- cbind(words, Age2, Education2, seagrass_perception, coral_perception, activity_type, visit_freq2)

words <- within(words, 
                Category_1 <- factor(Category_1, 
                                    levels=names(sort(table(Category_1), 
                                                      decreasing = F))))
words %>% 
  filter(!is.na(Category_1)) %>%
  ggplot(aes(reorder(Category_1))) +
  geom_bar(mapping = aes(Category_1, stat(prop), group = 1)) +
  coord_flip()

## Climate change belif
pp_df %>%
  filter(!is.na(cc_bel), !is.na(Age2)) %>%
  ggplot(mapping = aes(x = cc_bel, color = Age2),) + 
  geom_freqpoly(binwidth = 10)
