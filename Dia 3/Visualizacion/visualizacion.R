library(tidyverse)
library(ggplot2)
library(RColorBrewer)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Este es el directorio de trabajo, cambienlo por su carpeta donde van a trabajar

# Scatterplot ------------------------------------------------------------

df <- read.csv("country-level-taxes-vs-income.csv")
df <- df[df$Year == 2018,]
df <- df[,c(1,3,4,5,6)]
colnames(df) <- c("country", "year", "gdp_pc", "tax_rev_gdp", "population")

p <- ggplot(data = df) +
  geom_point(aes(x = tax_rev_gdp, y = log(gdp_pc))) +
  theme_test() + 
  xlab("Ingresos tributarios como % del PIB") +
  ylab("Log(PIB per capita)")

p

#ggsave("../Imagenes/scatterplot.eps", p, width = 6, height = 3.5)

p <- ggplot(data = df) +
  geom_point(aes(x = tax_rev_gdp, y = log(gdp_pc), size = 1.1*population), shape = 1) +
  theme_test() + 
  xlab("Ingresos tributarios como % del PIB") +
  ylab("Log(PIB per capita)") +
  guides(size = FALSE)

p

#ggsave("../Imagenes/scatterplot_weighted.eps", p, width = 6, height = 3.5)

df <- read.csv("country-level-taxes-vs-income.csv")
df <- df[df$Year == 1990 | df$Year == 2000 | df$Year == 2010 | df$Year == 2015,]
df <- df[,c(1,3,4,5,6)]
colnames(df) <- c("country", "year", "gdp_pc", "tax_rev_gdp", "population")

p <- ggplot(data = df) +
  geom_point(aes(x = tax_rev_gdp, y = log(gdp_pc), size = 1.1*population), shape = 1) +
  geom_smooth(method="lm", aes(x = tax_rev_gdp, y = log(gdp_pc))) +
  facet_wrap(~year) + 
  theme_test() + 
  xlab("Ingresos tributarios como % del PIB") +
  ylab("Log(PIB per capita)") +
  guides(size = FALSE)

p

#ggsave("../Imagenes/scatterplot_lm_weighted.eps", p, width = 6, height = 3.5)


# Bar chart ---------------------------------------------------------------

df_sample <- read.csv("casen_muestra.csv")
df_sample$educ <- as.factor(df_sample$educ)

df_sum <- df_sample %>%
  group_by(educ) %>%
  summarize(ypc = mean(ypc))

blues = colorRampPalette(c("light blue", "dark blue"))

p <- ggplot(data = df_sum) +
  geom_bar(aes(x = educ, y = ypc, fill = educ), 
           stat = "identity") + 
  xlab("Nivel educacional") + 
  ylab("Ingreso promedio") +
  scale_fill_manual(values=blues(13)) +
  theme_test() +
  guides(fill = FALSE)

p

#ggsave("../Imagenes/barplot.eps", p, width = 6, height = 3.5)

df_sum_w <- df_sample %>%
  group_by(educ) %>%
  summarize(ypc = weighted.mean(ypc, w = expc))

blues = colorRampPalette(c("light blue", "dark blue"))

p <- ggplot(data = df_sum_w) +
  geom_bar(aes(x = educ, y = ypc, fill = educ), 
           stat = "identity") + 
  xlab("Nivel educacional") + 
  ylab("Ingreso promedio") +
  scale_fill_manual(values=blues(13)) +
  theme_test() +
  guides(fill = FALSE)

p

#ggsave("../Imagenes/barplot_weighted.eps", p, width = 6, height = 3.5)

df_sum$weights <- "No"
df_sum_w$weights <- "Sí"

df_sum <- rbind(df_sum, df_sum_w)

p <- ggplot(data = df_sum) +
  geom_bar(aes(x = educ, y = ypc, fill = weights), 
           stat = "identity", position=position_dodge()) + 
  xlab("Nivel educacional") + 
  ylab("Ingreso promedio") +
  scale_fill_brewer(palette = "Set1", name = "Factor de expansion") +
  theme_test() +
  theme(legend.position = "bottom")

p

#ggsave("../Imagenes/barplot_comparison.eps", p, width = 6, height = 3.5)


# Histogram ---------------------------------------------------------------

df_sample$univ <- NA 
df_sample$univ[as.numeric(df_sample$educ) >= 9 ] <- "Sí"
df_sample$univ[as.numeric(df_sample$educ) < 9 ] <- "No"

p <- ggplot(data = df_sample) +
  geom_histogram(aes(x = log(ypc), fill = univ, weight = expc), 
                 color = "black") +
  xlab("Logaritmo del ingreso") +
  ylab("Frecuencia") +
  scale_fill_brewer(palette = "Set1", name = "Universidad (completa o incompleta)") +
  theme_test() +
  theme(legend.position = "bottom")

p

#ggsave("../Imagenes/histogram_weighted.eps", p, width = 6, height = 3.5)

# Line plot ---------------------------------------------------------------

country_list <- read_csv("WorldBank_AgeDependencyRatio.csv")[3]

df_age <- read_csv("age_dependency.csv", skip = 4)
df_age <- df_age[,-c(2,3,4)]
colnames(df_age)[1] <- "country"
df_age <- df_age %>%
  pivot_longer(cols = !country , names_to = "year", values_to = "ratio")
df_age$japan_dummy <- "Japan"
df_age$japan_dummy[df_age$country != "Japan"] <- "Otros"
df_age <- df_age[df_age$country %in% country_list$`Country Name`,]
df_age$year <- as.numeric(df_age$year)
df_age$japan_dummy <- factor(df_age$japan_dummy, levels = c("Japan", "Otros"))

p <- ggplot(data = df_age[df_age$country == "Japan",]) +
  geom_line(aes(x = year, y = ratio, group = country, color = country)) +
  xlab("Año") +
  ylab("Tasa de dependencia") +
  theme_test() +
  scale_color_discrete(name = "País")
  
p

#ggsave("../Imagenes/line_japan.eps", p, width = 6, height = 3.5)

p <- ggplot(data = df_age) +
  geom_line(aes(x = year, y = ratio, group = country, color = country)) +
  xlab("Año") +
  ylab("Tasa de dependencia") +
  theme_test() +
  scale_color_discrete(name = "País")

p

#ggsave("../Imagenes/line_all.eps", p, width = 6, height = 3.5)


df_age$country <- factor(df_age$country, 
                         levels = c(unique(df_age$country)[-which(unique(df_age$country) == "Japan")],
                                                    "Japan"))

p <- ggplot(data = df_age) +
  geom_line(aes(x = year, y = ratio, group = country, color = japan_dummy)) +
  xlab("Año") +
  ylab("Tasa de dependencia") +
  scale_color_manual(name = "País", values = c("red", "gray")) +
  theme_test() +
  theme(legend.position = "bottom")

p

#ggsave("../Imagenes/line_all_good.eps", p, width = 6, height = 3.5)

