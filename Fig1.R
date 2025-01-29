# R version 4.3.1 (2023-06-16)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Ventura 13.4.1
#
# Package: 
# - dplyr_1.1.2
# - stringr_1.5.0
# - readxl_1.4.2
# - nlme_3.1-162
# - emmeans_1.8.7
# - mgcv_1.8-42
# - piecewiseSEM_2.3.0
# - car_3.1-2
# - ggplot2_3.4.2
# - ggeffects_1.2.3
# - ggpubr_0.6.0   

# Cleaning workspace
rm(list = ls())

#### > 1. Packages ####
libs <- c(
  # Data handling
  'dplyr', 'stringr', 'readxl',
  # Statistical analyses
  'nlme', 'emmeans', "mgcv", 'car',
  # Data visualization
  'ggplot2', 'ggpubr', 'ggeffects'
)

invisible(lapply(libs, library, character.only = T))

#### > 2. Data ####
# Loading data
unzip('climate-data.csv.zip')
df.clim = read.csv('climate-data.csv')

# Hourly scale dataset
d.2.2 = df.clim %>% 
  mutate(date = paste0(year, month, day)) %>%
  mutate(hour.f = factor(hour)) |>
  mutate(month.f = factor(month)) |>
  mutate(year.month = year + (month-1)/12) |> 
  filter(TreeDiv > 0)

d.2.1 = 
  df.clim |> 
  filter(TreeDiv > 0 & year %in% 2015:2020 & !is.na(temp)) |>
  filter(!(plot == 'E31' & year == 2020)) |> #logger fail
  group_by(site, plot, TreeDiv, year, month) %>%
  # calculation of min, median and maximum monthly temperatures
  summarise(
    T.max = median(temp[temp>quantile(temp,probs=c(.95), na.rm = T)], na.rm = T),
    T.med = median(temp, na.rm = T),
    T.min = median(temp[temp<quantile(temp,probs=c(.05), na.rm = T)], na.rm = T)) |>
  mutate(month.f = factor(month)) |>
  mutate(year.month = year + (month-1)/12)

#### > 3. Statistical analyses ####
#### >> 3.1 Daily scale ####
# !!! DO NOT RUN 3.1 !!! #
# !!! Fitting the model takes several days !!! #
# The entire section was commented and replaced by pre-fitted models 
# Load mod-day-pred.RDS and hourly-model.csv for pre-fitted models

`mod-day-pred` = readRDS("mod-day-pred.RDS")
mod.2 = `mod-day-pred`[[1]]
mod.2.res = `mod-day-pred`[[2]]
pred.2 = `mod-day-pred`[[3]]
df.hour = read.csv('hourly-model.csv')

# Model fit (do not run)
# mod.2 = lme(temp ~ log(TreeDiv) * hour.f,
#             random = ~ 1|site/plot/date,
#             data = d.2.2,
#             correlation=corCAR1(),
#             na.action=na.exclude)
summary(mod.2)

# Prediction of the model outputs
# pred.2 = ggpredict(model = mod.2,   
#                    terms = c("TreeDiv", 'hour.f'))

# Estimation of the random structure residuals
# mod.2.res = lme(temp ~ 1,
#                 random = ~ 1|site/plot/date,
#                 data = d.2.2,
#                 correlation=corCAR1(),
#                 na.action=na.exclude)

d.2.2$group = d.2.2$hour.f

t.lab = c('0-1','1-2','2-3','3-4','4-5','5-6','6-7',
          '7-8','8-9','9-10','10-11','11-12',
          '12-13','13-14','14-15','15-16','16-17','17-18','18-19',
          '19-20','20-21','21-22','22-23','23-0')
names(t.lab) = 0:23

anova(mod.2)

# Estimatition of hourly p-value from individual models (Do not run)
# df.hour <- matrix(nrow = 1, ncol = 6) %>% as.data.frame() 
# colnames(df.hour) <- c("Value", "Std.Error", "DF", "t-value", "p-value", "hour")
# 
# for (h in 0:23) {
#   tryCatch( # Catching non fitting models 
#   {mod.hour <- lme(temp ~ log(TreeDiv),
#                     random = ~ 1|site/plot/date,
#                     data = d.2.2 |> filter(hour == h),
#                     correlation=corCAR1(),
#                    na.action=na.exclude)
#   df <- summary(mod.hour)$tTable[2,] %>% matrix(nrow=1) %>% as.data.frame()
#   colnames(df) <- c("Value", "Std.Error", "DF", "t-value", "p-value")
#   df$hour <- h
#   df.hour <- rbind(df.hour,df)
#   },
#   warning=function(w) {},
#   error=function(e) {}
#   )
# }
# df.hour <- df.hour[-1,]
lab.h =
  df.hour |>
  add_row(Value = NA, Std.Error = NA, DF = NA, t.value = NA, p.value = 1.00, hour = 16) |> # adding non-fitted model
  mutate(group = hour) |>
  mutate(lab = if_else(p.value < 0.001, "p < 0.001", paste0('p = ',round(p.value, 3))))

#### >> 3.2 Monthly extremes ####
#### >> 3.2.1 temperature maximum ####
mod.1.tmax = lme(T.max ~ log(TreeDiv, base = 2) * month.f,
                 random = ~ 1|site/plot/year,
                 data = d.2.1,
                 correlation=corCAR1(),
                 na.action=na.exclude)

pred.1.max   = ggpredict(model = mod.1.tmax,   terms = c("TreeDiv", 'month.f'))

mod.1.tmax.res = lme(T.max ~ 1,
                     random = ~ 1|site/plot/year,
                     data = d.2.1)

summary(mod.1.tmax)
anova(mod.1.tmax)

d.2.1$res.max = residuals(mod.1.tmax.res) + mod.1.tmax.res$coefficients$fixed

df.out.max <- matrix(nrow = 1, ncol = 6) %>% as.data.frame() 
colnames(df.out.max) <- c("Value", "Std.Error", "DF", "t-value", "p-value", "month")

# Estimatition of monthly p-value from individual models
for (m in 1:12) {
  mod.month <- lme(T.max ~ log(TreeDiv, base = 2),
                   random = ~ 1|site/plot/year,
                   data = d.2.1 %>% filter(month == m),
                   correlation=corCAR1())
  
  df <- summary(mod.month)$tTable[2,] %>% matrix(nrow=1) %>% as.data.frame()
  colnames(df) <- c("Value", "Std.Error", "DF", "t-value", "p-value")
  df$month <- m
  df.out.max <- rbind(df.out.max,df)
}

df.out.max <- df.out.max[-1,]

#### >> 3.2.2 temperature median ####
mod.1.tmed = lme(T.med ~ log(TreeDiv, base = 2) * month.f,
                 random = ~ 1|site/plot/year,
                 data = d.2.1,
                 correlation=corCAR1())
summary(mod.1.tmed)
anova(mod.1.tmed)

pred.1.med   = ggpredict(model = mod.1.tmed,   terms = c("TreeDiv", 'month.f'))

mod.1.tmed.res = lme(T.med ~ 1,
                     random = ~ 1|site/plot/year,
                     data = d.2.1)

d.2.1$res.med = residuals(mod.1.tmed.res)  + mod.1.tmed.res$coefficients$fixed

df.out.med <- matrix(nrow = 1, ncol = 6) %>% as.data.frame() 
colnames(df.out.med) <- c("Value", "Std.Error", "DF", "t-value", "p-value", "month")
for (m in 1:12) {
  mod.month <- lme(T.med ~ log(TreeDiv, base = 2),
                   random = ~ 1|site/plot/year,
                   data = d.2.1 %>% filter(month == m),
                   correlation=corCAR1())
  
  df <- summary(mod.month)$tTable[2,] %>% matrix(nrow=1) %>% as.data.frame()
  colnames(df) <- c("Value", "Std.Error", "DF", "t-value", "p-value")
  df$month <- m
  
  df.out.med <- rbind(df.out.med,df)
  
}

df.out.med <- df.out.med[-1,]

#### >> 3.2.3 temperature minimum ####
mod.1.tmin = lme(T.min ~ log(TreeDiv, base = 2) * month.f,
                 random = ~ 1|site/plot/year,
                 data = d.2.1,
                 correlation=corCAR1())
summary(mod.1.tmin)
anova(mod.1.tmin)

pred.1.min   = ggpredict(model = mod.1.tmin,   terms = c("TreeDiv", 'month.f'))

mod.1.tmin.res = lme(T.min ~ 1,
                     random = ~ 1|site/plot/year,
                     data = d.2.1)
d.2.1$res.min = residuals(mod.1.tmin.res)  + mod.1.tmin.res$coefficients$fixed

df.out.min <- matrix(nrow = 1, ncol = 6) %>% as.data.frame() 
colnames(df.out.min) <- c("Value", "Std.Error", "DF", "t-value", "p-value", "month")

for (m in 1:12) {
  mod.month <- lme(T.min ~ log(TreeDiv, base = 2),
                   random = ~ 1|site/plot/year,
                   data = d.2.1 %>% filter(month == m),
                   correlation=corCAR1())
  
  df <- summary(mod.month)$tTable[2,] %>% matrix(nrow=1) %>% as.data.frame()
  colnames(df) <- c("Value", "Std.Error", "DF", "t-value", "p-value")
  df$month <- m
  
  df.out.min <- rbind(df.out.min,df)
  
}

df.out.min <- df.out.min[-1,]

#### > 4. Figure 1 ####
#### >> 4.1 Figure 1.A. ####
p.day =
  ggplot(data = pred.2,
         aes(x, predicted)) +
  geom_line(data = pred.2,
            aes(x, predicted),
            col = 'black') +
  geom_ribbon(data = pred.2, aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  scale_x_continuous(trans = 'log2', breaks = c(1,4,24)) +
  labs(title = "Daily scale", 
       subtitle = "Sp. Rich.: p < 0.001, Hour: p < 0.001, Sp. Rich. x Hour: p < 0.001",
       x = 'Tree species richness x hour', 
       y = expression(paste('Hourly temperature [',~degree,'C]',sep=''))) +
  geom_text(aes(x=4, y=23.25, label = lab),
            data = lab.h, 
            size = 3,
            angle = 90) +
  facet_grid(cols = vars(group),
             labeller = labeller(group = t.lab)) +
  lims(y = c(min(pred.2$conf.low, pred.2$conf.high), 
             (max(pred.2$conf.low, pred.2$conf.high)+1.5))) + 
  theme_bw() +
  theme(panel.grid = element_blank(), 
        panel.spacing.x = unit(0.6, "lines"), 
        strip.text.x = element_text(size = 8.5)) 

p.day

#### >> 4.2 Figure 1.B. ####
d.2.1$group = d.2.1$month.f
text_max <- data.frame(
  lab = c(paste0("p ", ifelse(round(df.out.max$`p-value` <.001), "< 0.001", 
                              paste0("= ", round(df.out.max$`p-value`, 3))))
                               ),group = factor(1:12, levels=c(1:12))
  )

text_med <- data.frame(
  lab = c(paste0("p ", ifelse(round(df.out.med$`p-value` <.001), "< 0.001", 
                              paste0("= ", round(df.out.med$`p-value`, 3))))),
  group = factor(1:12, levels=c(1:12))
  )

text_min <- data.frame(
  lab = c(paste0("p ", ifelse(round(df.out.min$`p-value` <.001), "< 0.001", 
                              paste0("= ", round(df.out.min$`p-value`, 3))))),
  group = factor(1:12, levels=c(1:12))
  )

p.month =
  ggplot(data = pred.1.max, 
         aes(x, predicted)) + 
  geom_jitter(data = d.2.1, 
              aes(x = TreeDiv, y = res.max), 
              alpha = .02, color = 'red',
              size = .5) + 
  geom_line(data = pred.1.max, 
            aes(x, predicted), 
            col = 'red') +
  geom_ribbon(data = pred.1.max, 
              aes(ymin = conf.low, ymax = conf.high), 
              fill = 'red', alpha = .2) +
  geom_jitter(data = d.2.1, 
              aes(x = TreeDiv, y = res.med), 
              alpha = .02, color = 'black',
              size = .5) + 
  geom_line(data = pred.1.med, 
            aes(x, predicted), 
            col = 'black') +
  geom_ribbon(data = pred.1.med, 
              aes(ymin = conf.low, ymax = conf.high), 
              fill = 'black', alpha = .2) +
  geom_jitter(data = d.2.1, 
              aes(x = TreeDiv, y = res.min), 
              alpha = .02, color = 'blue',
              size = .5) + 
  geom_line(data = pred.1.min, 
            aes(x, predicted), 
            col = 'blue') +
  geom_ribbon(data = pred.1.min, 
              aes(ymin = conf.low, ymax = conf.high), 
              fill = 'blue', alpha = .2) +
  scale_x_continuous(trans = 'log2', breaks = c(1,2,4,8,24)) +
  labs(x = 'Tree species richness x month', 
       y = expression(paste('Monthly temperature [',~degree,'C]',sep='')), 
       title = "Monthly scale") +
  facet_grid(cols = vars(group)) +
  theme_bw() + 
  theme(axis.ticks.y.right = element_line(color = 'blue'),
        axis.text.y.right  = element_text(color = 'blue'),
        axis.title.y.right =  element_text(color = 'blue'),
        panel.grid = element_blank()) + 
  geom_text(aes(x=4,y=55, label = lab),
            data = text_max, 
            size = 3, 
            color = "red") +
  geom_text(aes(x=4,y=50, label = lab),
            data = text_med, 
            size = 3, 
            color = "black") +
  geom_text(aes(x=4,y=45, label = lab),
            data = text_min, 
            size = 3, 
            color = "blue")

p.month

#### >> 4.3 Figure ####
p = ggarrange(p.day,
              p.month, 
              nrow = 2, 
              labels = paste0(LETTERS[1:2],'.')
              ) 
p

# Saving
ggsave(filename = 'Figure-1.png', 
       height = 17, width = 28, units = 'cm')