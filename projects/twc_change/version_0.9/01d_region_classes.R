# Libraries ====================================================================

source('source/twc_change.R')

# Constants & Variables ========================================================

REGION_CLASS <- data.table(
  region = c(
    "ARP","CAF","CAR","CAU","CNA","EAS","EAU","ECA","EEU","ENA",
    "ESAF","ESB","GIC","MDG","MED","NAU","NCA","NEAF","NEN","NES",
    "NEU","NSA","NWN","NWS","NZ","RAR","RFE","SAH","SAM","SAS",
    "SAU","SCA","SEA","SEAF","SES","SSA","SWS","TIB","WAF","WCA",
    "WCE","WNA","WSAF","WSB"
  ),
  
  region_full = c(
    "Arabian Peninsula",
    "Central Africa",
    "Caribbean",
    "Central Australia",
    "Central N. America",
    "E. Asia",
    "E. Australia",
    "E. Central Asia",
    "E. Europe",
    "E. N. America",
    "E. Southern Africa",
    "E. Siberia",
    "Greenland/Iceland",
    "Madagascar",
    "Mediterranean",
    "N. Australia",
    "Central America",
    "NE. Africa",
    "NE. N. America",
    "NE. S. America",
    "N. Europe",
    "N. S. America",
    "NW. N. America",
    "NW. S. America",
    "New Zealand",
    "Russian Arctic",
    "Russian Far East",
    "Sahara",
    "S. American Monsoon",
    "S. Asia",
    "S. Australia",
    "S. Central America",
    "SE. Asia",
    "SE. Africa",
    "SE. S. America",
    "S. S. America",
    "SW. S. America",
    "Tibetan Plateau",
    "W. Africa",
    "W. Central Asia",
    "W. & Central Europe",
    "W. N. America",
    "W. Southern Africa",
    "W. Siberia"
  ),
  
  hemisphere = c(
    "north","north","north","south","north","north","south","north","north","north",
    "south","north","north","south","north","south","north","north","north","north",
    "north","north","north","south","south","north","north","north","south","north",
    "south","north","north","south","south","south","south","north","north","north",
    "north","north","south","north"
  ),
  
  continent = c(
    "Asia","Africa","Central America","Australasia","N. America","Asia","Australasia","Asia","Europe","N. America",
    "Africa","Asia","Europe","Africa","Europe","Australasia","Central America","Africa","N. America","S. America",
    "Europe","S. America","N. America","S. America","Australasia","Asia","Asia","Africa","S. America","Asia",
    "Australasia","Central America","Asia","Africa","S. America","S. America","S. America","Asia","Africa","Asia",
    "Europe","N. America","Africa","Asia"
  ),
  
  circulation = c(
    "dry_subsidence",  # ARP
    "monsoon",         # CAF
    "itcz_margin",     # CAR
    "dry_subsidence",  # CAU
    "continental",     # CNA
    "monsoon",         # EAS
    "itcz_margin",     # EAU
    "continental",     # ECA
    "continental",     # EEU
    "stormtrack",      # ENA
    "itcz_margin",     # ESAF
    "continental",     # ESB
    "stormtrack",      # GIC
    "itcz_margin",     # MDG
    "stormtrack",      # MED
    "itcz_margin",     # NAU
    "itcz_margin",     # NCA
    "dry_subsidence",  # NEAF
    "stormtrack",      # NEN
    "itcz_margin",     # NES
    "stormtrack",      # NEU
    "itcz_margin",     # NSA
    "stormtrack",      # NWN
    "itcz_margin",     # NWS
    "stormtrack",      # NZ
    "continental",     # RAR
    "stormtrack",      # RFE
    "dry_subsidence",  # SAH
    "monsoon",         # SAM
    "monsoon",         # SAS
    "stormtrack",      # SAU
    "itcz_margin",     # SCA
    "monsoon",         # SEA
    "itcz_margin",     # SEAF
    "dry_subsidence",  # SES
    "stormtrack",      # SSA
    "dry_subsidence",  # SWS
    "continental",     # TIB
    "monsoon",         # WAF
    "continental",     # WCA
    "stormtrack",      # WCE
    "stormtrack",      # WNA
    "dry_subsidence",  # WSAF
    "continental"      # WSB
  ),
  
  lat_zone = c(
    "subtropical",   # ARP
    "tropical",      # CAF
    "tropical",      # CAR
    "subtropical",   # CAU
    "midlatitude",   # CNA
    "midlatitude",   # EAS
    "subtropical",   # EAU
    "midlatitude",   # ECA
    "midlatitude",   # EEU
    "midlatitude",   # ENA
    "tropical",      # ESAF
    "highlatitude",  # ESB
    "highlatitude",  # GIC
    "tropical",      # MDG
    "subtropical",   # MED
    "tropical",      # NAU
    "tropical",      # NCA
    "subtropical",   # NEAF
    "highlatitude",  # NEN
    "tropical",      # NES
    "highlatitude",  # NEU
    "tropical",      # NSA
    "highlatitude",  # NWN
    "tropical",      # NWS
    "midlatitude",   # NZ
    "highlatitude",  # RAR
    "highlatitude",  # RFE
    "subtropical",   # SAH
    "tropical",      # SAM
    "subtropical",   # SAS
    "midlatitude",   # SAU
    "tropical",      # SCA
    "tropical",      # SEA
    "tropical",      # SEAF
    "subtropical",   # SES
    "midlatitude",   # SSA
    "subtropical",   # SWS
    "midlatitude",   # TIB
    "tropical",      # WAF
    "midlatitude",   # WCA
    "midlatitude",   # WCE
    "midlatitude",   # WNA
    "subtropical",   # WSAF
    "highlatitude"   # WSB
  ),
  
  climate_main = c(
    "arid",        # ARP
    "monsoonal",   # CAF
    "tropical",    # CAR
    "arid",        # CAU
    "temperate",   # CNA
    "monsoonal",   # EAS
    "temperate",   # EAU
    "arid",        # ECA
    "cold",        # EEU
    "temperate",   # ENA
    "tropical",    # ESAF
    "cold",        # ESB
    "cold",        # GIC
    "tropical",    # MDG
    "temperate",   # MED
    "tropical",    # NAU
    "tropical",    # NCA
    "arid",        # NEAF
    "cold",        # NEN
    "tropical",    # NES
    "cold",        # NEU
    "tropical",    # NSA
    "cold",        # NWN
    "tropical",    # NWS
    "temperate",   # NZ
    "cold",        # RAR
    "cold",        # RFE
    "arid",        # SAH
    "monsoonal",   # SAM
    "monsoonal",   # SAS
    "temperate",   # SAU
    "tropical",    # SCA
    "monsoonal",   # SEA
    "tropical",    # SEAF
    "temperate",   # SES
    "temperate",   # SSA
    "arid",        # SWS
    "cold",        # TIB
    "monsoonal",   # WAF
    "arid",        # WCA
    "temperate",   # WCE
    "temperate",   # WNA
    "arid",        # WSAF
    "cold"         # WSB
  ),
  
  hydrobelt = c(
    "arid",          # ARP
    "humid",         # CAF
    "humid",         # CAR
    "arid",          # CAU
    "humid",         # CNA
    "humid",         # EAS
    "humid",         # EAU
    "arid",          # ECA
    "humid",         # EEU
    "humid",         # ENA
    "subhumid",      # ESAF
    "cold",          # ESB
    "cold",          # GIC
    "humid",         # MDG
    "dry_subhumid",  # MED
    "subhumid",      # NAU
    "humid",         # NCA
    "arid",          # NEAF
    "cold",          # NEN
    "humid",         # NES
    "humid",         # NEU
    "humid",         # NSA
    "cold",          # NWN
    "humid",         # NWS
    "humid",         # NZ
    "cold",          # RAR
    "cold",          # RFE
    "arid",          # SAH
    "humid",         # SAM
    "monsoonal",     # SAS
    "dry_subhumid",  # SAU
    "humid",         # SCA
    "monsoonal",     # SEA
    "subhumid",      # SEAF
    "subhumid",      # SES
    "humid",         # SSA
    "arid",          # SWS
    "cold",          # TIB
    "subhumid",      # WAF
    "arid",          # WCA
    "humid",         # WCE
    "humid",         # WNA
    "arid",          # WSAF
    "cold"           # WSB
  )
)

REGION_CLASS[, hemisphere := factor(hemisphere, levels = c("north", "south"))]

REGION_CLASS[, circulation := factor(
  circulation,
  levels = c(
    "polar",
    "stormtrack",
    "continental",
    "monsoon",
    "itcz_margin",
    "dry_subsidence"
  )
)]

REGION_CLASS[, lat_zone := factor(
  lat_zone,
  levels = c(
    "highlatitude",
    "midlatitude",
    "subtropical",
    "tropical"
  )
)]

REGION_CLASS[, climate_main := factor(
  climate_main,
  levels = c(
    "cold",
    "temperate",
    "tropical",
    "monsoonal",
    "arid"
  )
)]

REGION_CLASS[, hydrobelt := factor(
  hydrobelt,
  levels = c(
    "cold",
    "humid",
    "subhumid",
    "dry_subhumid",
    "monsoonal",
    "arid"
  )
)]

# Outputs=======================================================================

saveRDS(REGION_CLASS, file.path(PATH_OUTPUT_DATA, 'region_classes.Rds'))
