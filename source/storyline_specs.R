# ============================================================================
# Storyline specifications
# Conditions use raw column names: prec_abs_change, evap_abs_change,
# avail_abs_change, flux_abs_change
# ============================================================================

STORYLINE_SPECS <- list(
  
  storyline_1 = list(
    label        = "storyline_1_arctic_boreal_amplification",
    regions      = c("RAR", "NWN", "NEN", "NEU", "WSB", "ESB", "RFE"),
    primary      = quote(prec_abs_change > 0 & avail_abs_change > 0),
    sub_variants = list(
      full_thermo = quote(prec_abs_change > 0 & evap_abs_change > 0  & avail_abs_change > 0),
      precip_led  = quote(prec_abs_change > 0 & evap_abs_change <= 0 & avail_abs_change > 0)
    ),
    context_var  = "lat_zone",
    context_val  = "highlatitude"
  ),
  
  storyline_2 = list(
    label        = "storyline_2_poleward_stormtrack_wetting",
    regions      = c("NEU", "WCE", "EEU", "EAU", "NZ"),
    primary      = quote(prec_abs_change > 0 & avail_abs_change > 0),
    sub_variants = list(
      evap_coupled    = quote(prec_abs_change > 0 & evap_abs_change > 0  & avail_abs_change > 0),
      precip_dominant = quote(prec_abs_change > 0 & evap_abs_change <= 0 & avail_abs_change > 0)
    ),
    context_var  = "circulation",
    context_val  = "stormtrack"
  ),
  
  storyline_3 = list(
    label        = "storyline_3_monsoon_amplification",
    regions      = c("WAF", "SAS", "EAS", "TIB", "NAU"),
    primary      = quote(prec_abs_change > 0 & avail_abs_change > 0),
    sub_variants = list(
      full_monsoon = quote(prec_abs_change > 0 & evap_abs_change > 0  & avail_abs_change > 0),
      precip_only  = quote(prec_abs_change > 0 & evap_abs_change <= 0 & avail_abs_change > 0)
    ),
    context_var  = "circulation",
    context_val  = "monsoon"
  ),
  
  storyline_4 = list(
    label        = "storyline_4_humid_tropical_intensification",
    regions      = c("NSA", "CAF", "SEA"),
    primary      = quote(prec_abs_change > 0 & evap_abs_change > 0 & flux_abs_change > 0),
    sub_variants = list(
      avail_positive = quote(prec_abs_change > 0 & evap_abs_change > 0 & flux_abs_change > 0 & avail_abs_change > 0),
      avail_negative = quote(prec_abs_change > 0 & evap_abs_change > 0 & flux_abs_change > 0 & avail_abs_change < 0)
    ),
    context_var  = "lat_zone",
    context_val  = "tropical"
  ),
  
  storyline_5 = list(
    label        = "storyline_5_subtropical_circulation_drying",
    regions      = c("MED", "WNA", "NCA", "SCA", "SWS", "SES", "SSA", "SAU", "WSAF"),
    primary      = quote(prec_abs_change < 0 & avail_abs_change < 0),
    sub_variants = list(
      hotter_drier = quote(prec_abs_change < 0 & avail_abs_change < 0 & evap_abs_change > 0),
      moisture_lim = quote(prec_abs_change < 0 & avail_abs_change < 0 & evap_abs_change < 0)
    ),
    context_var  = "circulation",
    context_val  = "dry_subsidence"
  ),
  
  storyline_6 = list(
    label        = "storyline_6_land_atm_coupling_amplification",
    regions      = c("MED", "WCE", "WNA", "CNA", "SWS"),
    primary      = quote(prec_abs_change < 0 & avail_abs_change < 0),
    sub_variants = list(
      soil_moisture_lim = quote(prec_abs_change < 0 & avail_abs_change < 0 & evap_abs_change < 0),
      demand_sustained  = quote(prec_abs_change < 0 & avail_abs_change < 0 & evap_abs_change >= 0)
    ),
    context_var  = "climate_main",
    context_val  = "temperate"
  ),
  
  storyline_7 = list(
    label        = "storyline_7_deforestation_induced_deceleration",
    regions      = c("NES", "SAM", "NSA", "SEA"),
    primary      = quote(prec_abs_change < 0 & evap_abs_change < 0 & flux_abs_change < 0),
    sub_variants = list(
      full_deceleration = quote(prec_abs_change < 0 & evap_abs_change < 0 & flux_abs_change < 0 & avail_abs_change < 0),
      recycling_only    = quote(prec_abs_change < 0 & evap_abs_change < 0 & flux_abs_change < 0 & avail_abs_change >= 0)
    ),
    context_var  = "lat_zone",
    context_val  = "tropical"
  ),
  
  storyline_8 = list(
    label        = "storyline_8_dryland_soilmoisture_collapse",
    regions      = c("SAH", "ARP", "WNA"),
    primary      = quote(prec_abs_change < 0 & evap_abs_change < 0 & avail_abs_change < 0 & flux_abs_change < 0),
    sub_variants = list(
      precip_led = quote(prec_abs_change < 0 & evap_abs_change < 0 & avail_abs_change < 0 & abs(prec_abs_change) > abs(evap_abs_change)),
      evap_led   = quote(prec_abs_change < 0 & evap_abs_change < 0 & avail_abs_change < 0 & abs(evap_abs_change) >= abs(prec_abs_change))
    ),
    context_var  = "climate_main",
    context_val  = "arid"
  )
)