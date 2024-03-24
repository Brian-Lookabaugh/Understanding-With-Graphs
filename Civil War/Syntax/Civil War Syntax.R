################################################################################
##################### Understanding Civil Wars in X Graphs #####################
################################################################################

pacman::p_load(
  "dplyr", # Data Manipulation
  "stringr", # Working with Texts
  "ggplot2", # Visualization
  "sf", # Creating Maps
  "rnaturalearth", # Maps
  "rnaturalearthdata", # Maps
  "peacesciencer", # Conflict Data
  "countrycode", # Working with Country Codes
  "viridis", # Pretty Color Palettes
  "readxl", # Importing Excel Files
  install = FALSE
)

################################################################################
############################## Create the Data Set #############################
################################################################################

# Import Data Sets
ucdp <- readRDS("Civil War/Data/UcdpPrioConflict_v23_1.rds")
vdem <- readRDS("Civil War/Data/V-Dem-CY-Full+Others-v14.rds")
ged <- readRDS("Civil War/Data/GEDEvent_v23_1.rds")
term <- read_excel("Civil War/Data/ucdp-term-acd-3-2021 (4).xlsx")

# Clean UCDP Data
ucdp <- ucdp %>%
  mutate(gwno.a = as.numeric(gwno_a)) %>%
  # Only Keep Civil Wars
  filter(type_of_conflict == 3 | type_of_conflict == 4) %>%
  # Create a Civil War Dummy Variable
  mutate(civ.war = 1) %>%
  # Collapse This Data to the Country-Year Level
  group_by(gwno.a, year) %>%
  summarise(civ.war = max(civ.war)) %>%
  ungroup()

# Create Base State-Year Data to Merge with UCDP Data
states <- create_stateyears(system = "gw")

# Merge State-Year Data with UCDP Data
conflict <- left_join(states, ucdp, by = c("gwcode" = "gwno.a", "year"))

conflict <- conflict %>%
  # Re-Code NA Civil War Values to 0
  mutate(civ.war = ifelse(is.na(civ.war), 0, civ.war)) %>%
  # Keep Relevant Years
  filter(year >= 1950 & year < 2021) 

# Filter a Subset of V-Dem Data Before Merging
vdem.filtered <- vdem %>%
  select(
    COWcode, # Country ID
    year, # Time ID
    country_name, # Country Name
    e_pt_coup_attempts, # Coup Attempts
    v2x_polyarchy, # Democracy
    e_area, # Land Area
    e_regiongeo, # 19 Region Categories
    e_regionpol_7C, # 7 Region Categories
    e_gdppc, # GDP per Capita
    e_pop, # Population Size
    e_miferrat, # Fertility Rate
    v2x_clphy # Physical Violence
  )

# Merge The UCDP and V-Dem Data Together
merged <- left_join(conflict, vdem.filtered, by = c("gwcode" = "COWcode", "year"))

merged <- merged %>%
  # Re-Code Coup Attempts as Non-Civil War Cases
  mutate(civ.war = ifelse(e_pt_coup_attempts > 0, 0, civ.war)) %>%
  # Create an "Ever Civil War" Variable
  group_by(gwcode) %>%
  arrange(gwcode, year) %>%
  mutate(
    first.civ.war.year = min(year[civ.war == 1]), 
    ever.civ.war = ifelse(year >= first.civ.war.year, 1, 0) 
  ) %>%
  select(-first.civ.war.year) %>%
  ungroup()

# Clean and Merge Battle-Deaths Data
ged <- ged %>%
  # Collapse to State-Year Data
  mutate(gwcode = as.numeric(gwnoa)) %>%
  group_by(gwcode, year) %>%
  summarise(deaths = max(best),
            high.deaths = max(high),
            low.deaths = max(low),
            deaths.gov = max(deaths_a),
            deaths.reb = max(deaths_b),
            deaths.civ = max(deaths_civilians),
            deaths.un = max(deaths_unknown)) %>%
  ungroup()

merged <- left_join(merged, ged, by = c("gwcode", "year"))

# Clean and Merge Conflict Termination Data
term <- term %>%
  mutate(gwcode = as.numeric(gwno_loc)) %>%
  # Keep Internal Conflicts
  filter(type_of_conflict == 3) %>%
  # Re-Code Conflict Outcome Variables
  mutate(peace.agg = ifelse(outcome == 1, 1, 0),
         ceasefire = ifelse(outcome == 2, 1, 0),
         gov.vic = ifelse(outcome == 3, 1, 0),
         reb.vic = ifelse(outcome == 4, 1, 0),
         low.act = ifelse(outcome == 5, 1, 0),
         no.exist = ifelse(outcome == 6, 1, 0)) %>%
  # Re-Code Conflict Type
  mutate(type.territory = ifelse(incompatibility == 1, 1, 0),
         type.government = ifelse(incompatibility == 2, 1, 0),
         type.both = ifelse(incompatibility == 3, 1, 0)) %>%
  # Re-Code Intensity Level
  mutate(high.intensity = ifelse(intensity_level == 2, 1, 0)) %>%
  # Collapse Data to Country-Year Level
  group_by(gwcode, year) %>% 
  summarise(
    peace.agg = max(peace.agg),
    ceasefire = max(ceasefire),
    gov.vic = max(gov.vic),
    reb.vic = max(reb.vic),
    low.act = max(low.act),
    no.exist = max(no.exist),
    type.territory = max(type.territory),
    type.government = max(type.government),
    type.both = max(type.both),
    high.intensity = max(high.intensity),
    outcome = max(outcome)
  ) %>% 
  ungroup()

merged <- left_join(merged, term, by = c("gwcode", "year"))

# Clean Final Data Set
final <- merged %>%
  # Filter Variables
  select(-c("country_name")) %>%
  # Re-Code US Values to Civil War = 0
  mutate(civ.war = ifelse(gwcode == 2, 0, civ.war)) %>%
  # Filter People's Republic of Yemen
  filter(gwcode != 680) %>%
  # Manually Re-Code UCDP Civil War Values for Yemen
  mutate(civ.war = ifelse(gwcode == 678 & year %in% c(1994, 2009:2022), 1, civ.war),
         civ.war = ifelse(gwcode == 678 & is.na(civ.war), 0, civ.war))

# Remove Data Sets
rm(conflict, ged, merged, states, term, ucdp, vdem, vdem.filtered)

# Create a Custom Graphics Theme
theme.forester <- function() {
  theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold", size = rel(0.8), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.title = element_text(face = "bold"))
}

################################################################################
######### Graph 1: Time Series Plot of Number of Conflicts Each Year ###########
################################################################################

final %>%
  # Filter Non-Civil War Cases
  filter(civ.war == 1) %>%
  # Create Counts for Civil Wars
  group_by(year) %>%
  mutate(civ.war.count = sum(civ.war == 1)) %>%
  ungroup() %>%
  # Create the Plot
  ggplot(aes(x = year)) +
  geom_line(aes(y = civ.war.count), size = 1, color = "#1A4314") +
  geom_smooth(aes(y = civ.war.count), method = "loess", se = FALSE, 
              size = 1, color = "#B8D8AAFF", linetype = "dashed") +
  scale_x_continuous(breaks = seq(1950, 2020, by = 5)) +
  labs(
    title = "Number of Countries with Active Civil Conflicts (1950-2020)",
    x = "",
    y = ""
  ) +
  theme.forester()

################################################################################
########## Graph 2: Map of Countries Who Have Had The Most Civil Wars ##########
################################################################################

# Create a Count of Unique Civil Wars for Each Country
final <- final %>%
  group_by(gwcode) %>%
  mutate(onset = ifelse(civ.war == 1 & lag(civ.war) == 0, 1, 0),
         onset = if_else(row_number() == first(which(civ.war == 1)), 1, onset),
         count.onset = cumsum(onset),
         count.onset = ifelse(is.na(count.onset) & !is.na(civ.war), 0, count.onset)) %>%
  ungroup()

# Load Map Data
world.sf <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

# Rename Country IDs to Match
world.sf <- world.sf %>%
  mutate(iso_n3 = as.numeric(iso_n3)) %>%
  mutate(gwcode = countrycode(sourcevar = iso_n3, origin = "iso3n", destination = "gwn"),
         gwcode = ifelse(iso_n3 == 887, 678, gwcode),
         gwcode = ifelse(brk_name == "Somaliland", 520, gwcode))

# Merge Map Data with Final Data
map.data <- final %>%
  full_join(world.sf, by = c("gwcode")) %>%
  # Drop Antarctica/Non-Existent Countries
  filter(statename != "Antarctica") %>%
  # Manually Add in Somaliland Civil Conflicts
  mutate(count.onset = ifelse(brk_name == "Somaliland", 1, count.onset))

map.data %>%
  # Create the Map
  ggplot() +
  geom_sf(
    aes(geometry = geometry, fill = count.onset),
    color = "black",
    size = .2,
    na.rm = T) +
  scale_fill_viridis_c(
    option = "rocket",
    direction = -1,
    breaks = seq(0, 12, by = 2),  
    labels = seq(0, 12, by = 2),  
    na.value = "#fbf2e9"
  ) +
  labs(
    title = "Number of Civil Conflicts From 1950 to 2020",
    fill = "") +
  # Not Using Forester Theme Here Because Maps Are Weird
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 8, vjust = 1),
    legend.text = element_text(size = 6),
    legend.key.height = unit(0.25, 'cm'),
    legend.key.width = unit(1, 'cm'),
    plot.margin = unit(c(-1, -0.7, -1, -0.7), "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) 

################################################################################
############## Graph 3: Bar Chart of Duration of Current Conflicts #############
################################################################################

final %>%
  # Only Keep Countries With Civil Wars By 2020
  filter(!is.na(civ.war)) %>%
  group_by(gwcode) %>%
  mutate(peace.fail = ifelse(row_number() == 1 & civ.war == 1, 1,
                             ifelse(lag(civ.war == 0) & civ.war == 1, 1, 0))) %>%
  ungroup() %>%
  mutate(id = cumsum(peace.fail)) %>%
  filter(civ.war == 1) %>%
  group_by(id) %>%
  mutate(duration = row_number()) %>%
  ungroup() %>%
  mutate(statename = ifelse(statename == "Russia (Soviet Union)", "Russia", statename),
         statename = ifelse(statename == "Burkina Faso (Upper Volta)", "Burkina Faso", statename),
         statename = ifelse(statename == "Congo, Democratic Republic of (Zaire)", "DRC", statename),
         statename = ifelse(statename == "Tanzania/Tanganyika", "Tanzania", statename),
         statename = ifelse(statename == "Turkey (Ottoman Empire)", "Turkey", statename),
         statename = ifelse(statename == "Myanmar (Burma)", "Myanmar", statename),
         statename = ifelse(statename == "Iran (Persia)", "Iran", statename),
         statename = ifelse(statename == "Central African Republic", "CAR", statename),
         statename = ifelse(statename == "Yemen (Arab Republic of Yemen)", "Yemen", statename)) %>%
  filter(year == 2020) %>%
  # Create the Plot
  ggplot(aes(x = duration, y = reorder(factor(statename), duration), fill = duration)) +
  geom_col() +
  scale_x_continuous(breaks = seq(0, 50, by = 5)) +
  labs(
    title = "Duration of Ongoing Civil Conflicts Up to 2020",
    x = "Years of Continuous Conflict",
    y = ""
  ) +
  scale_fill_viridis(option = "mako", end = 0.8, direction = -1) +
  theme.forester() +
  theme(legend.position = "none")

################################################################################
######### Graph 4: Bar Chart of Deaths per capita for Current Conflicts ########
################################################################################

final %>%
  # Only Keep Countries With Civil Wars By 2020
  filter(!is.na(civ.war)) %>%
  group_by(gwcode) %>%
  mutate(peace.fail = ifelse(row_number() == 1 & civ.war == 1, 1,
                             ifelse(lag(civ.war == 0) & civ.war == 1, 1, 0))) %>%
  ungroup() %>%
  mutate(id = cumsum(peace.fail)) %>%
  filter(civ.war == 1) %>%
  group_by(id) %>%
  mutate(dpc = (sum(deaths)) / e_pop, na.rm = TRUE) %>%
  ungroup() %>%
  mutate(statename = ifelse(statename == "Russia (Soviet Union)", "Russia", statename),
         statename = ifelse(statename == "Burkina Faso (Upper Volta)", "Burkina Faso", statename),
         statename = ifelse(statename == "Congo, Democratic Republic of (Zaire)", "DRC", statename),
         statename = ifelse(statename == "Tanzania/Tanganyika", "Tanzania", statename),
         statename = ifelse(statename == "Turkey (Ottoman Empire)", "Turkey", statename),
         statename = ifelse(statename == "Myanmar (Burma)", "Myanmar", statename),
         statename = ifelse(statename == "Iran (Persia)", "Iran", statename),
         statename = ifelse(statename == "Central African Republic", "CAR", statename)) %>%
  filter(year == 2018) %>%
  # Filter India and Yemen Due to Missing Deaths Data
  filter(!gwcode %in% c(678, 750)) %>%
  # Create the Plot
  ggplot(aes(x = dpc, y = reorder(factor(statename), dpc), fill = dpc)) +
  geom_col() +
  labs(
    title = "Total Battle Deaths for Civil Conflicts Ongoing Up to 2019",
    subtitle = "India and Yemen Excluded Due to Missing Deaths Data", 
    x = "Battle Deaths Per 10,000 People (Civilian and Combatant)",
    y = ""
  ) +
  scale_fill_viridis(option = "mako", end = 0.6, direction = -1) +
  theme.forester() +
  theme(legend.position = "none",
        plot.subtitle = element_text(size = 10))

################################################################################
################## Graph 5: Donut Plot of Civil War Outcomes ###################
################################################################################

# Re-Load the Conflict Termination Data
term <- read_excel("Civil War/Data/ucdp-term-acd-3-2021 (4).xlsx")

final %>%
  filter(!is.na(outcome)) %>%
  count(outcome) %>%
  mutate(percentage = n / sum(n) * 100,
         y.max = cumsum(percentage),
         y.min = lag(y.max, default = 0)) %>%
  ggplot(aes(ymax = y.max, ymin = y.min, xmax = 4, xmin = 3.55, fill = as.factor(outcome))) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  scale_fill_viridis_d(option = "magma", end = 0.9, labels = c(
    "Peace Agreement", "Ceasefire", "Government Victory", "Rebel Victory",
    "Low Activity", "State Ceases to Exist"
  )) +
  labs(
    title = "Distribution of Civil War Outcomes from 1950-2019",
    fill = "Type of Outcome"
  ) +
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))

################################################################################
##### Graph 6: Time Series Plot of Conflict Intensity by Conflict Duration #####
################################################################################

# Define a Custom Break Sequence for the Graph X-Axis
custom.breaks <- c(1, seq(5, 70, by = 5))

final %>%
  filter(!is.na(civ.war)) %>%
  group_by(gwcode) %>%
  mutate(peace.fail = ifelse(row_number() == 1 & civ.war == 1, 1,
                             ifelse(lag(civ.war == 0) & civ.war == 1, 1, 0))) %>%
  mutate(id = cumsum(peace.fail)) %>%
  filter(civ.war == 1) %>%
  mutate(war.count = row_number()) %>%
  ungroup() %>%
  group_by(war.count) %>%
  summarise(avg.deaths = mean(deaths, na.rm = T)) %>%
  ungroup() %>%
  ggplot(aes(x = war.count, y = avg.deaths)) +
  geom_line(size = 1, color = "#1A4314") +
  geom_smooth(aes(y = avg.deaths), method = "loess", se = FALSE, 
              size = 1, color = "#B8D8AAFF", linetype = "dashed") +
  scale_x_continuous(breaks = custom.breaks) +
  labs(
    title = "Average Battle-Related Deaths by Year of Active Civil Conflict",
    x = "Years at Civil Conflict",
    y = "Average Battle-Related Deaths Per 10,000 People"
  ) +
  theme.forester()

################################################################################
############### Graph 7: GDP per capita and Civil War Frequency ################
################################################################################

final %>%
  group_by(count.onset) %>%
  mutate(mean.gdp = mean(e_gdppc, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = count.onset, y = mean.gdp)) +
  geom_point(size = 1.75) +
  geom_smooth(method = "loess", se = FALSE, size = 1, color = "#22a884") +
  labs(
    title = "Economic Development and Civil Conflict Frequency (1950-2019)",
    x = "Number of Civil Conflicts",
    y = "Average GDP per capita",
    fill = "Number of Countries"
  ) +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) +
  scale_y_continuous(breaks = seq(0, 14, by = 2)) +
  theme.forester() 

################################################################################
################# Graph 8: Democracy and Civil War Frequency ###################
################################################################################

final %>%
  group_by(count.onset) %>%
  mutate(mean.dem = mean(v2x_polyarchy, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = count.onset, y = mean.dem)) +
  geom_point(size = 1.75) +
  geom_smooth(method = "loess", se = FALSE, size = 1, color = "#2a788e") +
  labs(
    title = "Democracy and Civil Conflict Frequency (1950-2020)",
    subtitle = "Index Ranges from 0 (Total Autocracy) to 1 (Total Democracy)",
    x = "Number of Civil Conflicts",
    y = "Average Electoral Democracy Index Score"
  ) +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) +
  scale_y_continuous(breaks = seq(0.2, 0.5, by = 0.05)) +
  scale_fill_viridis(option = "mako", begin = 0.15) +
  theme.forester()

################################################################################
################# Graph 9: Repression and Civil War Frequency ##################
################################################################################

final %>%
  group_by(count.onset) %>%
  mutate(mean.phy = mean(v2x_clphy, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = count.onset, y = mean.phy)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, size = 1, color = "#414487") +
  labs(
    title = "Violent State Repression and Civil Conflict Frequency (1950-2020)",
    subtitle = "The Physical Violence Index Measures the Degree to Which Individuals\nare Free From Political Killings and Torture By the Government",
    x = "Number of Civil Conflicts",
    y = "Average Freedom from Physical Violence Index Score"
  ) +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) +
  scale_y_continuous(breaks = seq(0.1, 1, by = 0.2)) +
  theme.forester()

################################################################################
################### Graph 10: Combined Charts of Demography ####################
################################################################################

final %>%
  group_by(count.onset) %>%
  mutate(mean.fer = mean(e_miferrat, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = count.onset, y = mean.fer)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, size = 1, color = "#440154") +
  labs(
    title = "Fertility Rates and Civil Conflict Frequency (1950-2019)",
    subtitle = "Fertility Rates Refer to the Average Number of Children 1 Woman\nIs Expected to Have In Her Lifetime",
    x = "Number of Civil Conflicts",
    y = "Average Fertility Rate"
  ) +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) +
  scale_y_continuous(breaks = seq(3.5, 7, by = 0.5)) +
  theme.forester()
