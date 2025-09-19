# Correlational evidence: Tracks, cooperatives, and assembly houses
#
# Date updated:   2025-02-05
# Auhtor:         Christian Vedel
# Purpose:        Produces descriptive statistics, which shows how cooperative
#                 creameries, assembly houses, and railway tracks are correlated

# ==== Libraries ====
library(tidyverse)
library(fixest)

# ==== Load data ====
assembly_houses = read_csv2("Data/Panel_of_assembly_houses.csv")
coops = read_csv("Data/creameries_parish.csv")
railways = read_csv2("Data/Panel_of_railways_in_parishes.csv")
shape_parishes = read_sf("Data/sogne_shape/sogne.shp")


# ==== Build cross section ====
assembly_houses_1915 = assembly_houses %>% 
    filter(Year == 1915) %>% 
    filter(Assembly_house > 0) %>%
    select(GIS_ID, Assembly_house) %>%
    mutate(GIS_ID = as.character(GIS_ID))

railways_1915 = railways %>% 
    filter(Year == 1915) %>%
    filter(Connected_rail == 1) %>%
    select(GIS_ID, Connected_rail) %>%
    mutate(GIS_ID = as.character(GIS_ID))

coops1915 = coops %>%
    count(GIS_ID) %>%
    rename(Coop = n) %>%
    select(GIS_ID, Coop) %>%
    mutate(GIS_ID = as.character(GIS_ID))

cross_section = shape_parishes %>%
    data.frame() %>%
    select(GIS_ID, AMT, HERRED, SOGN) %>%
    left_join(assembly_houses_1915, by = "GIS_ID") %>%
    left_join(railways_1915, by = "GIS_ID") %>%
    left_join(coops1915, by = "GIS_ID") %>%
    mutate(
        Assembly_house = replace_na(Assembly_house, 0),
        Connected_rail = replace_na(Connected_rail, 0),
        Coop = replace_na(Coop, 0)
    ) %>%
    mutate(
        Coop_count = Coop,
        Coop = ifelse(Coop > 0, 1, 0),
        Assembly_house_count = Assembly_house,
        Assembly_house = ifelse(Assembly_house > 0, 1, 0),
    )

# Hundred level
cross_section_herred = cross_section %>% 
    group_by(HERRED) %>% 
    summarise(
        Assembly_house = sum(Assembly_house_count),
        Connected_rail = sum(Connected_rail),
        Coop = sum(Coop_count)
    )


# ==== Descriptive statistics ====
p1 = cross_section_herred %>%
    ggplot(aes(Assembly_house, Coop)) + geom_point() + geom_smooth(method = "lm") + 
    theme_bw()

p2 = cross_section_herred %>%
    ggplot(aes(Connected_rail, Coop)) + geom_point() + geom_smooth(method = "lm") + 
    theme_bw()

ggsave("Plots/Assembly_houses_vs_coops.png", p1, width = 4, height = 3)
ggsave("Plots/Connected_rail_vs_coops.png", p2, width = 4, height = 3)

# ==== In regression form ====
# Assembly houses
mod1 = feols(
    Coop ~ Connected_rail, data = cross_section,
    cluster = ~GIS_ID
)

mod2 = feols(
    Coop ~ Assembly_house, data = cross_section,
    cluster = ~GIS_ID
)

# Coops
# Parish level
mod3 = feols(
    Coop ~ Connected_rail + Assembly_house, data = cross_section,
    cluster = ~GIS_ID
)

# Hundred level
mod4 = fepois(
    Coop ~ Connected_rail + Assembly_house, data = cross_section_herred
)

etable(
    list(mod1, mod2, mod3, mod4),
    tex = TRUE
)

etable(
    list(mod1, mod2, mod3, mod4)
)


