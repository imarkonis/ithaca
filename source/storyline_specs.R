# ============================================================================
# Storyline specifications
# Conditions use raw column names: prec_abs_change, evap_abs_change,
# avail_abs_change, flux_abs_change
# ============================================================================

STORYLINE_SPECS <- list(
  
  storyline_1 = list(
    label        = "1_arctic_boreal_amplification",
    regions      = c("RAR", "NWN", "NEN", "NEU", "WSB", "ESB", "RFE"),
    criteria      = quote(prec_abs_change > 0 & evap_abs_change > 0 & avail_abs_change > 0)
  ),
  
  storyline_2 = list(
    label        = "2_poleward_stormtrack_wetting",
    regions      = c("NEU", "WCE", "EEU", "EAU", "NZ"),
    criteria      = quote(prec_abs_change > 0 & evap_abs_change > 0 & avail_abs_change > 0)
  ),
  
  storyline_3 = list(
    label        = "3_monsoon_amplification",
    regions      = c("WAF", "SAS", "EAS", "TIB", "NAU"),
    criteria      = quote(prec_abs_change > 0 & evap_abs_change > 0 & avail_abs_change > 0)
  ),
  
  storyline_4 = list(
    label        = "4_humid_tropical_intensification",
    regions      = c("NSA", "CAF", "SEA"),
    criteria      = quote(prec_abs_change > 0 & evap_abs_change > 0)
  ),
  
  storyline_5 = list(
    label        = "5_subtropical_circulation_drying",
    regions      = c("MED", "WNA", "NCA", "SCA", "SWS", "SES", "SSA", "SAU", "WSAF"),
    criteria      = quote(prec_abs_change < 0 & avail_abs_change < 0)
  ),
  
  storyline_6 = list(
    label        = "6_land_atm_coupling_amplification",
    regions      = c("MED", "WCE", "WNA", "CNA", "SWS"),
    criteria      = quote(prec_abs_change < 0 & avail_abs_change < 0)
  ),
  
  storyline_7 = list(
    label        = "7_deforestation_induced_deceleration",
    regions      = c("NES", "SAM", "NSA", "SEA"),
    criteria      = quote(prec_abs_change < 0 & evap_abs_change < 0 & avail_abs_change < 0)
  ),
  
  storyline_8 = list(
    label        = "8_dryland_soilmoisture_collapse",
    regions      = c("SAH", "ARP", "WNA"),
    criteria      = quote(prec_abs_change < 0 & evap_abs_change < 0 & avail_abs_change < 0)
  )
)
