################################################################################
##################### Understanding Civil Wars in X Graphs #####################
################################################################################

pacman::p_load(
  "dplyr", # Data Manipulation
  "ggplot2", # Visualization
  "peacesciencer", # Conflict Data
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
  filter(type_of_conflict == 3) %>%
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
  # Filter Civil War Years Outside of Time Frame
  filter(year > 1949 & year < 2021) %>%
  # Re-Code NA Civil War Values to 0
  mutate(civ.war = ifelse(is.na(civ.war), 0, civ.war)) %>%
  # Add Rugged Terrain Data from the {peacesciencer} Package
  add_rugged_terrain()

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
    e_miferrat # Fertility Rate
  )

# Merge The UCDP and V-Dem Data Together
merged <- left_join(conflict, vdem.filtered, by = c("gwcode" = "COWcode", "year"))

merged <- merged %>%
  # Re-Code Coup Attempts as Non-Civil War Cases
  mutate(civ.war = ifelse(e_pt_coup_attempts > 0, 0, civ.war)) %>%
  # Re-Code Observations Where Civil War = 0 For One Year to Civil War
  group_by(gwcode) %>%
  mutate(civ.war = ifelse(lag(civ.war) == 1 & lead(civ.war == 1), 1, civ.war)) %>%
  # Create an "Ever Civil War" Variable
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
    high.intensity = max(high.intensity)
  ) %>% 
  ungroup()

merged <- left_join(merged, term, by = c("gwcode", "year"))

# Clean Final Data Set
final <- merged %>%
  # Filter Variables
  select(-c("newlmtnest", "country_name"))

# Remove Data Sets
rm(conflict, ged, merged, states, term, ucdp, vdem, vdem.filtered)

################################################################################
# Graph 1: Time Series Plot of Number of Conflicts Each Year by Conflict Type ##
################################################################################

################################################################################
## Graph 2: Map of Countries Who Experienced/Currently Experiencing Civil War ##
################################################################################

################################################################################
############## Graph 3: Bar Chart of Duration of Current Conflicts #############
################################################################################

################################################################################
############ Graph 4: Map of Deaths per capita for Current Conflicts ###########
################################################################################

################################################################################
########### Graph 5: Bar Chart of Conflict Intensity by Conflict Type ##########
################################################################################

################################################################################
################## Graph 6: Donut Plot of Civil War Outcomes ###################
################################################################################

################################################################################
##### Graph 7: Time Series Plot of Conflict Intensity by Conflict Duration #####
################################################################################

################################################################################
################# Graph 8: Combined Charts of GDP per capita ###################
################################################################################

# Onset, Intensity, and Duration

################################################################################
#################### Graph 9: Combined Charts of Democracy #####################
################################################################################

# Onset, Intensity, and Duration

################################################################################
#################### Graph 10: Combined Charts of Geography #####################
################################################################################

# Onset, Intensity, and Duration

################################################################################
################### Graph 11: Combined Charts of Demography ####################
################################################################################

# Onset, Intensity, and Duration
