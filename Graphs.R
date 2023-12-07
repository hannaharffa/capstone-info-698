### Set working directory

setwd("C:\\Users\\hanna\\PycharmProjects\\INFO698\\Data\\Capstone")

library("readxl")
library("dplyr")

### Import data sets

# Import site distances with reservation info

site_res <- read_xlsx("site_res.xlsx", sheet = 1)

# Import site distances with media

site_media <- read_xlsx("site_media.xlsx", sheet = 1)

# Make tables into data frames

site_res <- as.data.frame(site_res)
site_media <- as.data.frame(site_media)

#Load ggplot 

install.packages("ggplot2")
library(ggplot2)
options(scipen = 9999)



### Histogram

# HI, AS, GU, FM, MP, PR, VI excluded from site

histogram <- ggplot(site_res,
       aes(x = NEAR_DIST*.000189394)) +                     # convert ft to miles
  geom_histogram(color = "black", fill = "lightgrey", bins = 50) +
  labs(x = "Minimum Distance in Miles between Superfund Sites and Native American Lands", y = "Count", title = "Proximity of Native American Lands to Superfund Sites") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0), axis.title.x = element_text(margin = margin(t = 10)), axis.title.y = element_text(margin = margin(r = 10)), panel.background = element_rect(fill = "white")) +
scale_x_continuous(breaks = seq(0, 150, 10), limits = c(0, 150)) +
scale_y_continuous(breaks = seq(0, 1000, 100))

ggsave("histogram.png", width = 10, height = 5)



### Density Diagram

# HI, AS, GU, FM, MP, PR, VI excluded from site

library("RColorBrewer")
install.packages("viridis")
library("viridis")

# First, group by media
site_media1 <- site_media %>%
  mutate(Media_Group = ifelse(Media %in% c("Air", "Landfill Gas", "Soil Gas"), "Air/Gas",
                              ifelse(Media == "Surface Water", "Surface Water",
                                     ifelse(Media %in% c("Free-phase NAPL", "Groundwater", "Leachate"), "Ground Water/Leachate",
                                            ifelse(Media %in% c("Sediment", "Soil", "Sludge"), "Land",
                                                   ifelse(Media %in% c("Liquid Waste", "Solid Waste"), "Waste",
                                                          ifelse(Media %in% c("Buildings/Structures", "Debris"), "Buildings/Structures",
                                                                 ifelse(Media == "Residuals", "Residuals",
                                                                               ifelse(Media == "Fish Tissue", "Fish Tissue",
                                                                                      "Other/Unknown"))))))))) %>%
  select(SITE_NAME, EPA_ID, key, GIS_AREA, NEAR_FID, NEAR_DIST, IND_NAME, R_CODE, Contaminants, Media_Group)

# Remove residuals from table
site_media1 <- site_media1[!(site_media1$Media_Group %in% "Residuals"),]

# Remove other/unknown from table
site_media2 <- site_media1[!(site_media1$Media_Group %in% "Other/Unknown"),]
write.csv(site_media2, "site_media2.csv")

# Find the mean for each media group
site_media_means <- site_media2 %>%
  group_by(Media_Group) %>%
  summarise_at(vars(NEAR_DIST), list(name = mean))

# Merge the means back into the site media table
site_media_final <- left_join(site_media2, site_media_means,
                              by = join_by(Media_Group == Media_Group))
colnames(site_media_final)[colnames(site_media_final) == "name"] = "mean"
site_media_final$Media_Group <- factor(site_media_final$Media_Group, levels = c("Air/Gas", "Waste", "Ground Water/Leachate", "Land", "Surface Water", "Buildings/Structures", "Fish Tissue"))

# Faceted density diagram
density_diagram <- ggplot(site_media_final,
       aes(x = NEAR_DIST*.000189394, fill = Media_Group)) +
  geom_density(alpha = 0.8) +
  labs(x = "Minimum Distance in Miles between Superfund Sites and Native American Lands", y = "Count", title = "Proximity of Native American Lands to Superfund Sites by Contaminated Media") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0), axis.title.x = element_text(margin = margin(t = 10)), axis.title.y = element_text(margin = margin(r = 10)), panel.background = element_rect(fill = "white"), legend.position = "none") +
  scale_x_continuous(breaks = seq(0, 150, 10), limits = c(0, 150)) +
  # scale_y_continuous(breaks = seq(0, 1000, 100)) +
  # scale_color_viridis(discrete = TRUE, option = "A")+
  # scale_fill_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE, option = "A") +
  facet_grid(Media_Group ~ .) +
  geom_vline(aes(xintercept = mean*.000189394), color = "red", linetype = "dashed")

ggsave("density_diagram.png", width = 10, height = 15)

# ANOVA test
anova <- aov(NEAR_DIST~Media_Group, data = site_media_final)
summary(anova)

# Tukey test
tukey <- data.frame(TukeyHSD(anova)$Media_Group)

write.csv(tukey, "tukey.csv")

# Find the meadian for each media group
site_media_medians <- site_media_final %>%
  group_by(Media_Group) %>%
  summarise_at(vars(NEAR_DIST), list(name = median))

write.csv(site_media_medians, "medians.csv")