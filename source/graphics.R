library(colorspace)

theme_opts <- list(theme(axis.ticks.length =unit(-0.1, "cm"),  
                         axis.text.x = element_text(margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")), 
                         axis.text.y = element_text(margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))))

main_cols <-  c("#97B8C2", "#BF9A77", "#D35C37") 

colset_mid <- c( "#4D648D", "#337BAE", "#97B8C2",  "#739F3D", "#ACBD78",  
                 "#F4CC70", "#EBB582",  "#BF9A77",
                 "#E38B75", "#CE5A57",  "#D24136", "#785A46" )
colset_mid_qual <- colset_mid[c(11, 2, 4, 6,  1, 8, 10, 5, 7, 3, 9, 12)]

colset_agreement <- colset_mid_qual[c(1, 4, 3, 2, 5)]
colset_land_cover_short <- colset_mid_qual[c(12, 8, 3, 11, 13, 6, 9, 5, 10)] #[Bar, Crop, For, Grass, Other, Sava, Shrub, Snow, Water]
colset_biome_short <- colset_mid_qual[c(3, 13, 10, 11, 8, 4, 12, 9, 7, 5)] #[B.Forest, Desert, Flooded, M.Grassland, Mediterranean, T.Forests, T.Grassland, T/S Forests, T/S Grassland, Tundra]
colset_elev <- colset_mid[c(3, 12, 10, 11, 8, 4)]
colset_elev_mono <- rev(sequential_hcl(n = 8, "YlOrBr"))[3:8]
colset_prec_quant <- rev(sequential_hcl(n = 14, "Blues"))[3:12]

colset_KG_1_names <- colset_mid[c(2, 3, 7, 5, 4)] #Polar, Continental, Dry, Temperate, Tropical
colset_RdBu_5 <- colset_mid[c(10, 9, 6, 3, 1)]

axis_scientific <- function(x){
  ifelse(x == 0, "0", parse(text = gsub("[+]", "", gsub("e", "%*%10^", scientific_format()(x)))))
}

palette_mid <- colorRampPalette(colset_mid)
palette_mid_qual <- colorRampPalette(colset_mid_qual)
