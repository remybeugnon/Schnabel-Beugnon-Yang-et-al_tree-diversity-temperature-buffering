# R version 4.2.2 Patched (2022-11-10 r83330)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 22.04.2 LTS
# Package: 
# -  ggfortify_0.4.15
# - gam_1.22
# - foreach_1.5.2
# - corrplot_0.92
# - forcats_1.0.0
# - purrr_0.3.5
# - readr_2.1.3       
# - tidyverse_1.3.2
# - ggeffects_1.1.4
# - ggpubr_0.5.0
# - ggplot2_3.4.0
# - car_3.1-1         
# - piecewiseSEM_2.1.2
# - mgcv_1.8-42
# - emmeans_1.8.2
# - nlme_3.1-162
# - readxl_1.4.1

# Cleaning workspace
rm(list = ls())

#### > 0. Packages ####
libs <- c(
  'tidyverse', 'readxl', 'readr', 'purrr',
  'nlme', "mgcv", 'piecewiseSEM', 'corrplot', 'gam',
  'ggplot2', 'ggpubr', 'ggeffects','ggfortify'
)
invisible(lapply(libs, library, character.only = T))

#### > 1. Loading data ####
# Climate data
load("workspace_data.RData")

# LAI data
df.lai <- 
  read_csv("LAI.csv") |> 
  select(site, site.plot, time, LAI)

df.lai.1 = 
  df.lai |>
  filter(time == '2014.Aug') |> 
  group_by(site, site.plot) |>
  summarise(LAI = mean(LAI, na.rm = T))

# Terrestrial laser scanning data
df.TLS <- read_csv("SSCI_Data_2019.csv") |> 
  mutate(site = 'A') |>
  mutate(site.plot = paste0(site, '.', 'plot')) |> 
  select(site, plot = PLOT_NO, MeanFrac, ENL, SSCI)

# Inventory data
df.inventory = 
  read.csv('inventory_data.csv', 
           sep = ',', dec = '.')
names.inv = colnames(df.inventory)

# Basal area
df.basal.area = df.inventory |> 
  select(site.plot = 1,grep('basal.area', names.inv)) |> 
  pivot_longer(cols = 2:7, 
               names_to = 'year',
               values_to = 'BA') |> 
  mutate(year = str_remove_all(year, '.basal.area..m2.ha.') |> 
           str_remove_all('X') |> 
           as.numeric()) |> 
  mutate(site.plot = str_replace_all(site.plot, '_', '.'))

# Mean height
df.mean.height = df.inventory |> 
  select(site.plot = 1,grep('.mean.height..cm.', names.inv)) |> 
  pivot_longer(cols = 2:7, 
               names_to = 'year',
               values_to = 'mean.height') |> 
  mutate(year = str_remove_all(year, '.mean.height..cm.') |> 
           str_remove_all('X') |> 
           as.numeric()) |> 
  mutate(site.plot = str_replace_all(site.plot, '_', '.'))

# Crown length
df.crown.length = df.inventory |> 
  select(site.plot = 1,grep('.crown.length..cm.', names.inv)) |> 
  pivot_longer(cols = 2:7, 
               names_to = 'year',
               values_to = 'crown.length') |> 
  mutate(year = str_remove_all(year, '.crown.length..cm.') |> 
           str_remove_all('X') |> 
           as.numeric()) |> 
  mutate(site.plot = str_replace_all(site.plot, '_', '.'))

df.inventory.1 = 
  df.basal.area |> 
  select(site.plot, year, BA) |> 
  left_join(df.crown.length, 
            by = c('site.plot', 'year')) |>
  left_join(df.mean.height, 
            by = c('site.plot', 'year'))

# Merging data
df.explanatory = 
  df.inventory.1 |> 
  filter(year == 2019) |>
  left_join(df.TLS |>
              mutate(site.plot = paste0(site, '.',plot))|>
              select(-site), 
            by = c('site.plot')) |>
  left_join(df.lai.1 |> 
              select(-site) |>
              mutate(site.plot = str_replace_all(site.plot, ':', '.') ), 
            by =c('site.plot')) |> 
  na.omit()

d.1 = 
  df.clim |>
  filter(TreeDiv > 0 & year == 2019 & site == 'A' & !is.na(T)) |>
  group_by(site, plot, month) |>
  summarise(Buff = mean(T)/sd(T) , 
            TreeDiv = mean(TreeDiv)) |>
  left_join(df.explanatory |> 
              mutate(site = str_sub(site.plot,1,1)), 
            by = c('site', 'plot'))

d.2 = d.1 |> na.omit()
d.2 |> group_by(month, TreeDiv) |> summarise(n = n())

# Plot information
df.plot = read.csv('plots_Site_A.csv') |> 
  select(plot = PLOT_NO, TreeDiv = TREE_R, compo = TREE_CMP) |> 
  mutate(site.plot = paste0('A.',plot)) |>
  mutate(KoBi = if_else(str_detect(compo, 'K. bipinnata'), 1 , 0)) |>
  mutate(ScSu = if_else(str_detect(compo, 'S. superba'), 1 , 0)) |>
  mutate(QuFa = if_else(str_detect(compo, 'Q. fabri'), 1 , 0)) |> 
  mutate(sp.prim = if_else((KoBi + ScSu + QuFa)>0,1,0))

df.lai.2 =
  left_join(df.lai.1 |>
              filter(site == 'A') |> 
              select(-site) |>
              mutate(site.plot = str_replace_all(site.plot, ':', '.')),
            df.plot,
            by =c('site.plot')) |> 
  mutate(l.TreeDiv = log(TreeDiv,2)) |> 
  filter(!is.na(LAI) & !is.na(l.TreeDiv))

df.SSCI.2 =
  left_join(df.TLS |> 
              filter(site == 'A') |> 
              mutate(site.plot = paste0(site,'.',plot)) |>
              select(-plot),
            df.plot,
            by =c('site.plot')) |> 
  mutate(l.TreeDiv = log(TreeDiv,2)) |> 
  mutate(l.SSCI = log(SSCI,2))

df.crown.length.2 = 
  left_join(df.mean.height,
            df.plot, 
            by = 'site.plot') |> 
  mutate(l.TreeDiv = log(TreeDiv,2)) |> 
  filter(TreeDiv >0)

# Macroclimate
df.macro.T = read.csv("macroclimate.csv") %>%
  filter(year == 2019) %>% 
  select(month, avg.t = tmp_celsius, min.t = tmn_celsius , max.t = tmx_celsius)

d.2 = d.2 |> mutate(l.TreeDiv = log(TreeDiv, 2))
d.2 = d.2 |> mutate(l.SSCI = log(SSCI, 2))

d.3 = d.2 |> 
  left_join(df.macro.T, 
            by = 'month') |>
  mutate(Buff.cor = Buff/avg.t) |> 
  left_join(df.plot |> 
              select(site.plot, compo, KoBi, ScSu, QuFa, sp.prim), 
            by = 'site.plot')|> 
  mutate(l.SSCI = log(SSCI,2))

#### > 2. Correlation between variables ####
cor = cor(d.2 |> 
            ungroup() |>
            select(Buff, 
                   crown.length , 
                   mean.height , ENL, 
                   MeanFrac,
                   SSCI , LAI , TreeDiv) |>
            as.matrix()
)

corrplot(cor, addCoef.col = T)

# PCA analysis
comp = prcomp(
  d.2 |> 
    ungroup() |>
    select(BA, 
           crown.length , 
           mean.height, 
           MeanFrac, ENL, SSCI, 
           LAI) |>
    as.matrix(),
  center = T,
  scale. = T
)

plot(comp)

autoplot(comp, 
         loadings = T, loadings.label = T, 
         scale = T) + 
  theme_bw()

d.3 |> 
  filter(month==1) |> 
  group_by(TreeDiv) |>
  summarize(n = n())

cor = cor(d.3 |> 
            ungroup() |>
            select(Buff.cor, Buff, BA,
                   crown.base, crown.length , 
                   mean.height , ENL, 
                   SSCI , LAI , TreeDiv) |>
            as.matrix()
)
corrplot(cor, addCoef.col = T)

#### > 3. SEM yearly on monthly data ####
# Data scaling 
d.3$Buff.cor = (d.3$Buff.cor - mean(d.3$Buff.cor)) / sd(d.3$Buff.cor)
d.3$mean.height = (d.3$mean.height - mean(d.3$mean.height)) / sd(d.3$mean.height)
d.3$l.SSCI = (d.3$l.SSCI - mean(d.3$l.SSCI)) / sd(d.3$l.SSCI)
d.3$l.TreeDiv = (d.3$TreeDiv - mean(d.3$TreeDiv)) / sd(d.3$TreeDiv)
d.3$LAI = (d.3$LAI - mean(d.3$LAI)) / sd(d.3$LAI)

# SEM model
mod.sem = psem(
  # Temperature buffering model 
  lme(Buff.cor ~ mean.height + l.SSCI + LAI + l.TreeDiv,
      data = d.3, 
      random = ~ 1|plot/month,
      correlation=corCAR1(form = ~ month)),
  # SSCI model 
  lme(l.SSCI ~ l.TreeDiv,
      data = df.SSCI.2, 
      random = ~ 1|plot),
  # LAI model
  lme(LAI ~ l.TreeDiv + KoBi + ScSu + QuFa,
      data = df.lai.2 |> data.frame(), 
      random = ~ 1|plot),
  # Mean height model
  lme(mean.height ~ l.TreeDiv,
      data = df.crown.length.2 |> filter(year == 2019), 
      random = ~ 1|compo/plot),
  # Correlations
  LAI %~~% l.SSCI,
  mean.height %~~% l.SSCI,
  mean.height %~~% LAI,
  # Data
  data = d.3 |> data.frame()
)

summary(mod.sem)

# Addition of missing correlations to fulfill direct separation test
mod.sem = psem(
  lme(Buff.cor ~ mean.height + l.SSCI + LAI + l.TreeDiv,
      data = d.3, 
      random = ~ 1|plot/month,
      correlation=corCAR1(form = ~ month)),
  lme(l.SSCI ~ l.TreeDiv,
      data = df.SSCI.2, 
      random = ~ 1|plot),
  lme(LAI ~ l.TreeDiv + KoBi + ScSu + QuFa,
      data = df.lai.2 |> data.frame(), 
      random = ~ 1|plot),
  lme(mean.height ~ l.TreeDiv ,
      data = df.crown.length.2 |> filter(year == 2019), 
      random = ~ 1|compo/plot),
  LAI %~~% l.SSCI,
  mean.height %~~% l.SSCI,
  mean.height %~~% LAI,
  mean.height %~~% KoBi,
  mean.height %~~% ScSu,
  mean.height %~~% QuFa,
  l.SSCI %~~% QuFa,
  data = d.3 |> data.frame()
)

summary(mod.sem, standardized = T)
plot(mod.sem)

#### > 4. Monthly fitting ####
models.month = 
  1:12 |> 
  map_df(
    .f = function(m){
      dd = d.3 |> filter(month == m)
      mod.dd = psem(
        lme(Buff.cor ~ mean.height + l.SSCI + LAI + l.TreeDiv,
            data = dd, 
            random = ~ 1|plot,
            correlation=corCAR1(form = ~ month)),
        lme(l.SSCI ~ l.TreeDiv,
            data = df.SSCI.2, 
            random = ~ 1|compo/plot),
        lme(LAI ~ l.TreeDiv + KoBi + ScSu + QuFa,
            data = df.lai.2, 
            random = ~ 1|compo/plot),
        lme(mean.height ~ l.TreeDiv,
            data = df.crown.length.2 |> filter(year == 2019), 
            random = ~ 1|compo/plot),
        LAI %~~% l.SSCI,
        mean.height %~~% l.SSCI,
        mean.height %~~% LAI,
        mean.height %~~% KoBi,
        mean.height %~~% ScSu,
        mean.height %~~% QuFa,
        l.SSCI %~~% QuFa,
        data = dd |> data.frame()
      )
      summary(mod.dd)$coefficients |>
        data.frame() |> 
        mutate(month = m) |> 
        mutate(r2.buff = summary(mod.dd)$R2[1,5])
    }
  )

#### . 5. Extraction results ####
plot.models = 
  models.month |> 
  filter(Response == 'Buff.cor') |>
  mutate(signif = if_else(P.Value < 0.05, 1, 0))

# Smoothing monthly estimates 
df.plot.est = plot.models |> 
  filter(Response == 'Buff.cor') |> 
  select(month, est = Std.Estimate, pred = Predictor) |> 
  pivot_wider(id_cols = 'month', names_from = pred, values_from = est)

df.plot.est = 
  df.plot.est |> 
  add_row(
    df.plot.est |> 
      filter(month == 12) |> 
      mutate(month = 0)
  ) |> 
  add_row(
    df.plot.est |> 
      filter(month == 1) |> 
      mutate(month = 13)
  )

mod.cown = gam(mean.height ~ s(month), data = df.plot.est)
mod.SSCI = gam(l.SSCI ~ s(month), data = df.plot.est)
mod.LAI = gam(LAI ~ s(month), data = df.plot.est)
mod.div = gam(l.TreeDiv ~ s(month), data = df.plot.est)
df.plot.est$mean.height = predict(mod.cown)
df.plot.est$l.SSCI = predict(mod.SSCI)
df.plot.est$LAI = predict(mod.LAI)
df.plot.est$l.TreeDiv = predict(mod.div)

df.plot.est = df.plot.est |>
  filter(month %in% 1:12) |> 
  pivot_longer(2:5) |> 
  left_join(
    plot.models |>
      filter(Response == 'Buff.cor') |> 
      select(month, error = signif, name = Predictor),
    by = c('month', 'name')
  )

plot.models$Predictor[plot.models$Predictor == 'mean.height'] = 'Mean height'
plot.models$Predictor[plot.models$Predictor == 'l.TreeDiv'] = 'Tree sp. richness'
plot.models$Predictor[plot.models$Predictor == 'l.SSCI'] = 'SSCI'
plot.models$Predictor = factor(plot.models$Predictor, levels = c('Mean height',
                                                                 'SSCI', 'LAI',
                                                                 'Tree sp. richness'))

df.plot.est$name[df.plot.est$name == 'mean.height'] = 'Mean height'
df.plot.est$name[df.plot.est$name == 'l.TreeDiv'] = 'Tree sp. richness'
df.plot.est$name[df.plot.est$name == 'l.SSCI'] = 'SSCI'
df.plot.est$name = factor(df.plot.est$name, levels = c('Mean height',
                                                       'SSCI', 'LAI',
                                                       'Tree sp. richness'))
#### > 6. Plots ####
plot = ggplot() + 
  geom_bar(data = plot.models |> 
             filter(Predictor == 'SSCI'), 
           aes(x = month, y = r2.buff), stat = 'identity',
           fill = 'gray') + 
  geom_bar(data = plot.models |> 
             filter(Predictor == 'SSCI', month %in% c(1,7)), 
           aes(x = month, y = r2.buff), stat = 'identity', width = .9,
           fill = 'gray40') + 
  geom_line(data = df.plot.est , 
            aes(x = month, y = value, color = name),size = 2, alpha = .8, 
            linetype = 3) + 
  geom_line(data = df.plot.est |> filter(error == 1 & month >= 6) , 
            aes(x = month, y = value, color = name),size = 2) + 
  geom_line(data = df.plot.est |> filter(error == 1 & month <= 6) , 
            aes(x = month, y = value, color = name),size = 2) + 
  theme_bw() +
  scale_colour_brewer(type = 'qual',palette = 3) +
  scale_x_continuous(breaks = 1:12) + 
  scale_y_continuous(breaks = seq(0,1,.1), limits = c(0,1.2),
                     sec.axis = sec_axis( trans=~.*100, 
                                          name= bquote('Buffering'~R^2~'(%)'),
                                          breaks = c(0,25,50,75,100))) +
  scale_alpha_continuous(range = c(.2,1)) + 
  labs(x = 'Month', y = "Std. direct effects on buffering", color = 'Driver: ') + 
  theme(legend.position = 'bottom')

plot

# Saving plots
ggsave(plot= plot, 
       filename = 'monthly-SEM.svg', 
       height = 13, width = 15, unit = 'cm')
# The final figure was produced using Inkshape 1.1.2 (https://inkscape.org)