# Volume fraction of agreement index over environments ----

source('source/partition_evap.R')
source('source/graphics.R')

## Data ----
load(paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_masks.Rdata"))

## Colors ----
cols_agreement <- c("Low" = colset_RdBu_5[1], "Below average" = colset_RdBu_5[2], 
                    "Average" = colset_RdBu_5[3], 
                    "Above average" = colset_RdBu_5[4], "High" = colset_RdBu_5[5])



### Land Use

fig_land_cover_partition_fraction <- ggplot(land_cover_agreement[land_cover_short_class != "Other"]) +
  geom_bar(aes(x = land_cover_short_class, y = land_cover_fraction, fill = agreement_fac), stat = "identity") +
  xlab('Land cover type')  +
  ylab('Fraction [-]')  +
  labs(fill = 'Distribution\nagreement')  +
  scale_fill_manual(values = cols_agreement) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

### Biomes

fig_biome_partition_fraction <- ggplot(biome_agreement) +
  geom_bar(aes(x = biome_short_class, y = biome_fraction, fill = agreement_fac), stat = "identity") +
  xlab('Biome')  +
  ylab('Fraction [-]')  +
  labs(fill = 'Distribution\nagreement')  +
  scale_fill_manual(values = cols_agreement) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

### Elevation

fig_elevation_partition_fraction <- ggplot(elevation_agreement) +
  geom_bar(aes(x = elev_class, y = elev_fraction, fill = agreement_fac), stat = "identity") +
  xlab('Elevation [m]')  +
  ylab('Fraction [-]')  +
  labs(fill = 'Distribution\nagreement')  +
  scale_fill_manual(values = cols_agreement) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

### Evaporation classes

fig_evap_partition_fraction <- ggplot(evap_quant_agreement) +
  geom_bar(aes(x = evap_quant, y = evap_quant_fraction, fill = agreement_fac), stat = "identity") +
  xlab('Evaporation intensity class')  +
  ylab('Fraction [-]')  +
  labs(fill = 'Distribution\nagreement')  +
  scale_fill_manual(values = cols_agreement) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

### IPCC
IPCC_Africa <- c("CAF", "ESAF", "MDG", "NEAF", "SAH", "SEAF", "WAF", "WSAF")
IPCC_Asia <-   c("ARP", "EAS", "ECA", "ESB",  "RFE", "RAR",  "SAS", "SEA",  "TIB", "WCA", "WSB")
IPCC_Australasia <- c("CAU", "EAU", "NAU", "NZ", "PAC", "SAU")
IPCC_Europe <- c("EEU", "GIC","MED", "NEU", "WCE")
IPCC_Namerica <- c("CAR", "CNA", "ENA", "NCA","NEN", "NWN", "SCA", "WNA")
IPCC_Samerica <- c("NES","NSA","NWS","SAM","SES", "SSA","SWS")

fig_ipcc_fraction_Africa <- ggplot(IPCC_agreement[IPCC_ref_region %in% IPCC_Africa]) +
  geom_bar(aes(x = IPCC_ref_region, y = IPCC_ref_region_fraction, fill = agreement_fac), stat = "identity") +
  xlab('IPCC reference regions')  +
  ylab('Fraction [-]')  +
  labs(fill = 'Distribution\nagreement')  +
  scale_fill_manual(values = cols_agreement) +
  theme_light() +
  ggtitle("Africa")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

fig_ipcc_fraction_Asia <- ggplot(IPCC_agreement[IPCC_ref_region %in% IPCC_Asia]) +
  geom_bar(aes(x = IPCC_ref_region, y = IPCC_ref_region_fraction, fill = agreement_fac), stat = "identity") +
  xlab('IPCC reference regions')  +
  ylab('Fraction [-]')  +
  labs(fill = 'Distribution\nagreement')  +
  scale_fill_manual(values = cols_agreement) +
  theme_light() +
  ggtitle("Asia")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

fig_ipcc_fraction_Australasia <- ggplot(IPCC_agreement[IPCC_ref_region %in% IPCC_Australasia]) +
  geom_bar(aes(x = IPCC_ref_region, y = IPCC_ref_region_fraction, fill = agreement_fac), stat = "identity") +
  xlab('IPCC reference regions')  +
  ylab('Fraction [-]')  +
  labs(fill = 'Distribution\nagreement')  +
  scale_fill_manual(values = cols_agreement) +
  theme_light() +
  ggtitle("Australasia")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))


fig_ipcc_fraction_Europe <- ggplot(IPCC_agreement[IPCC_ref_region %in% IPCC_Europe]) +
  geom_bar(aes(x = IPCC_ref_region, y = IPCC_ref_region_fraction, fill = agreement_fac), stat = "identity") +
  xlab('IPCC reference regions')  +
  ylab('Fraction [-]')  +
  labs(fill = 'Distribution\nagreement')  +
  scale_fill_manual(values = cols_agreement) +
  ggtitle("Europe")+
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

fig_ipcc_fraction_Namerica <- ggplot(IPCC_agreement[IPCC_ref_region %in% IPCC_Namerica]) +
  geom_bar(aes(x = IPCC_ref_region, y = IPCC_ref_region_fraction, fill = agreement_fac), stat = "identity") +
  xlab('IPCC reference regions')  +
  ylab('Fraction [-]')  +
  labs(fill = 'Distribution\nagreement')  +
  scale_fill_manual(values = cols_agreement) +
  ggtitle("North America")+
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

fig_ipcc_fraction_Samerica <- ggplot(IPCC_agreement[IPCC_ref_region %in% IPCC_Samerica]) +
  geom_bar(aes(x = IPCC_ref_region, y = IPCC_ref_region_fraction, fill = agreement_fac), stat = "identity") +
  xlab('IPCC reference regions')  +
  ylab('Fraction [-]')  +
  labs(fill = 'Distribution\nagreement')  +
  scale_fill_manual(values = cols_agreement) +
  ggtitle("South America")+
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))



## Composite Figures ---- 
gg_fig_land <- ggarrange(fig_land_cover_partition_fraction,fig_biome_partition_fraction,
                         labels = c('a', 'b'), align = 'hv',
                         common.legend = T, legend = 'right', 
                         nrow = 1, ncol = 2, widths = c(0.75, 1.1))

gg_ipcc <-  ggarrange(fig_ipcc_fraction_Africa, fig_ipcc_fraction_Asia, fig_ipcc_fraction_Australasia,
                      fig_ipcc_fraction_Europe, fig_ipcc_fraction_Namerica, fig_ipcc_fraction_Samerica,
                      labels = c('c', 'd', 'e', 'f', 'g', 'h'),
                      ncol = 3, nrow = 2, common.legend = T, align = 'hv', legend = 'none')

gg_fig_main <- ggarrange(gg_fig_land, gg_ipcc,
                         common.legend = T, legend = 'right', 
                         nrow = 2, heights = c(0.7,1),
                         labels = c('', ''))

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, 
              "main/fig3_environments_distribution_agreement.png"), width = 10, height = 10)


gg_fig_SI <- ggarrange(fig_elevation_partition_fraction, fig_evap_partition_fraction,
                       labels = c('a', 'b'), align = 'hv',
                       common.legend = T, legend = 'right', 
                       nrow = 1, ncol = 2)

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig3_SI_environments_distribution_agreement.png"), 
       width = 8, height = 3.5)
