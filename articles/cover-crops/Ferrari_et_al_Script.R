### READ THIS ###

# To work, this script must be placed in the same folder with:
# Covercrops_Dataset.xlsx


# 0 - PREPARATION ----

.start_time <- Sys.time() #Set the clock!


packages <- c(
  # Operate with data
  "openxlsx", "readxl", "writexl", "dplyr", "tidyr", "tibble", "stats", "devtools",
  
  # Models
  "FSA", "vegan", "Maaslin2",
  
  # Plots
  "ggplot2", "paletteer", "VennDiagram",
  
  # Wake up!
  "beepr", "here" 
)


# Install and load missing packages
install.packages(setdiff(packages, rownames(installed.packages())))
invisible(lapply(packages, library, character.only = T))


# Create folders to save results and figures
directory <- c("figures", "results")
for(name in directory) {
  if (!dir.exists(name)){
    dir.create(name)
  }else{
    print("dir already exists")
  }
}


# Clean the environment
rm(list = ls())


# Is the directory correct?
here()


### YOU SHOULD NOW BE ABLE TO RUN THE SCRIPT ###
beep()
cat("Required packages installed, now the script should run.\n\n\n")


## FOR MAASLIN AND SPIECESI INSTALLATION ##

#if(!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("Maaslin2")

# 1. Usa la libreria utente per evitare problemi di permessi
#.libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()))

# 2. Installa BiocManager se manca
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")

# 3. Aggiorna Bioconductor alla versione compatibile
#BiocManager::install(version = "3.22", ask = FALSE)

# 4. Prova a installare Maaslin2 da Bioconductor
#if (!requireNamespace("Maaslin2", quietly = TRUE)) {
#  BiocManager::install("Maaslin2", ask = FALSE)
#}

# 5. Se ancora non va, installa dalla versione GitHub più recente
#if (!requireNamespace("Maaslin2", quietly = TRUE)) {
#  if (!requireNamespace("devtools", quietly = TRUE))
#    install.packages("devtools")
#  devtools::install_github("biobakery/maaslin2")
#}

#devtools::install_github("cran/huge")

# 6. Installa SpiecEasi da GitHub
#if (!requireNamespace("SpiecEasi", quietly = TRUE)) {
#  if (!requireNamespace("devtools", quietly = TRUE))
#    install.packages("devtools")
#  devtools::install_github("zdk123/SpiecEasi")
#}




#---------------------------------------------------------------------------#





#LOAD DATASETS

#Load metadata
metadata <- read.xlsx("Covercrops_Dataset.xlsx", sheet = "Metadata")
metadata_otu <- metadata[-9,]

metadata$Treatment <- as.factor(metadata$Treatment)
metadata$Site <- as.factor(metadata$Site)
metadata$Sample <- as.factor(metadata$Sample)

metadata_otu$Treatment <- as.factor(metadata_otu$Treatment)
metadata_otu$Site <- as.factor(metadata_otu$Site)
metadata_otu$Sample <- as.factor(metadata_otu$Sample)

treatment <- unique(metadata$Treatment)
sites <- metadata$Site
sites_otu <- metadata_otu$Site

metadata2 <- metadata
rownames(metadata2) <- metadata2$Site
metadata2$Site <- NULL

# Spontaneous (fallow) is our reference level
metadata$Treatment <- relevel(metadata$Treatment, ref = "Spontaneous")
metadata2$Treatment <- relevel(metadata2$Treatment, ref = "Spontaneous")
metadata_otu$Treatment <- relevel(metadata_otu$Treatment, ref = "Spontaneous")


#Load OTU table
otu_table <- read.xlsx("Covercrops_Dataset.xlsx", sheet = "OTU-Norm")
otu_table$Phylum <- as.factor(otu_table$Phylum)
otu_table$Class <- as.factor(otu_table$Class)
otu_table$Order <- as.factor(otu_table$Order)
otu_table$Family <- as.factor(otu_table$Family)
otu_table$Genus <- as.factor(otu_table$Genus)
otu_table$Species <- as.factor(otu_table$Species)
otu_table$Soil_dweller <- as.factor(otu_table$Soil_dweller)

str(otu_table)
head(otu_table)

# Create OTU matrix
otu_mat <- otu_table[,2:15]
rownames(otu_mat) <- otu_table$OTU
otu_mat <- as.data.frame(t(otu_mat))

str(otu_mat)
head(otu_mat)


#Load OTU table raw
otu_table_raw <- read.xlsx("Covercrops_Dataset.xlsx", sheet = "OTU-Raw")

# Create OTU matrix
otu_mat_raw <- otu_table_raw[,2:15]
rownames(otu_mat_raw) <- otu_table_raw$OTU
otu_mat_raw <- as.data.frame(t(otu_mat_raw))

str(otu_mat_raw)
head(otu_mat_raw)



# Taxonomy
taxonomy <- otu_table %>%
  dplyr::select(OTU, Phylum, Class, Order, Family, Genus, Species, Soil_dweller)

str(taxonomy)
head(taxonomy)


# Taxonomy
taxonomy_raw <- otu_table_raw %>%
  dplyr::select(OTU, Phylum, Class, Order, Family, Genus, Species)


# Soil properties
soil_table <- read.xlsx("Covercrops_Dataset.xlsx", sheet = "Soil")
soil_table$Site <- as.factor(soil_table$Site)
soil_table$Treatment <- as.factor(soil_table$Treatment)
soil_table$Treatment <- relevel(soil_table$Treatment, ref = "Spontaneous")

str(soil_table)
head(soil_table)


# Create soil matrix
soil_mat <- soil_table[,4:12]
rownames(soil_mat) <- soil_table$Site

str(soil_mat)
head(soil_mat)



# Create the palette
my_palette <- c(
  "Spontaneous" = "green4",
  "Grasses" = "gold2",
  "Legumes" = "skyblue3"
)



#Finish
beep()
cat("Tables prepared.\n\n\n")





#---------------------------------------------------------------------------#



# 1 - ANIMAL DIVERSITY ----

## 1.1 - OTU Richness ----

# OTUs
otu_richness <- otu_table %>%
  dplyr::select(all_of(sites_otu)) %>%
  summarise(across(everything(), ~ sum(. > 0))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Site",
    values_to = "OTU_richness"
  )

# SOIL dwellers
otu_soil_richness <- otu_table %>%
  filter(Soil_dweller == "Yes") %>%
  dplyr::select(all_of(sites_otu)) %>%
  summarise(across(everything(), ~ sum(. > 0))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Site",
    values_to = "Soil_OTUs"
  )



# Final dataset
richness <- otu_richness %>%
  left_join(otu_soil_richness, by = "Site") %>%
  left_join(metadata, by = c("Site" = "Site"))

rm(otu_richness, otu_soil_richness)


# Save this data
write.csv(richness, "results/Alpha-diversity.csv", row.names = F)


### 1.1.1 - OTU ----

sink("results/Kruskal-OTU.txt")

cat("\n\n\n ----- FULL MODEL ----- \n\n\n")
kruskal.test(OTU_richness ~ Treatment, data = richness) # P = 0.021

cat("\n\n\n ----- DUNN TEST ----- \n\n\n")
dunnTest(OTU_richness ~ Treatment, data = richness, method = "bh")

sink()



### 1.1.2 - OTU-Soil----
sink("results/Kruskal-OTU-Soil.txt")

cat("\n\n\n ----- FULL MODEL ----- \n\n\n")
kruskal.test(Soil_OTUs ~ Treatment, data = richness) # NS

cat("\n\n\n ----- DUNN TEST ----- \n\n\n")
dunnTest(Soil_OTUs ~ Treatment, data = richness, method = "bh")

sink()



### 1.1.3 - Plot  community composition ----

# Prepare the dataset
otus_long <- otu_table %>%
  dplyr::select(all_of(sites_otu), Class, Order) %>%
  pivot_longer(
    cols = all_of(sites_otu),
    names_to = "Site",
    values_to = "Reads"
  ) %>%
  left_join(metadata, by = "Site")


otus_class <- otus_long %>%
  group_by(Site, Treatment, Class) %>%
  summarise(RelAbundance = sum(Reads), .groups = "drop")

otus_order <- otus_long %>%
  group_by(Site, Treatment, Order) %>%
  summarise(RelAbundance = sum(Reads), .groups = "drop")


# Prepare the palette
levs <- levels(otus_class$Class)
cols <- paletteer_d("ggsci::default_igv", length(levs))
cols <- as.character(cols)
names(cols) <- levs


#Plot
ggplot(otus_class, aes(x = Site, y = RelAbundance, fill = Class)) +
  geom_bar(stat = "identity", colour = "black", linewidth = 0.1) +
  facet_wrap(~ Treatment, scales = "free_x") +
  theme_bw() +
  theme(text = element_text(size = 18),
        axis.text = element_text(size = 13, color = "black"),
        legend.position = "right") +
  labs(y = "Relative abundance (reads)" ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = cols)

ggsave("figures/Class_composition.pdf", height = 25, width = 30, units = "cm")



# Prepare the palette
levs <- levels(otus_order$Order)
cols <- paletteer_d("ggsci::default_igv", length(levs))
names(cols) <- levs
cols["None"] <- "grey90"


#Plot
ggplot(otus_order, aes(x = Site, y = RelAbundance, fill = Order)) +
  geom_bar(stat = "identity", colour = "black", linewidth = 0.1) +
  facet_wrap(~ Treatment, scales = "free_x") +
  theme_bw() +
  theme(text = element_text(size = 18),
        axis.text = element_text(size = 13, color = "black"),
        legend.position = "right") +
  labs(y = "Relative abundance (reads)" ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = cols)

ggsave("figures/Order_composition.pdf", height = 25, width = 35, units = "cm")


# Save the abundances
write.csv(otus_long, "results/Rel-Abundances.csv", row.names = F)



# Plot Venn Diagram
sites_by_group <- split(metadata_otu$Site, metadata_otu$Treatment)

otus_by_group <- lapply(sites_by_group, function(sites) {
  subset <- otu_mat[sites, , drop = F]
  present <- colSums(subset) > 0
  names(present)[present]
})

pdf("figures/Venn.pdf", height = 5, width = 5)
venn.diagram(
  x = otus_by_group,
  filename = NULL,
  fill = c("green4", "gold2", "skyblue3"),
  alpha = 0.7,
  cex = 2,
  cat.cex = 0,
  cat.pos = 0)
dev.off()


### 1.1.4 - Plot comparisons ----

# The bar for comparisons
y_max <- max(richness$OTU_richness, na.rm = T)
y_bar <- y_max + 5


ggplot(richness, aes(x = Treatment, y = OTU_richness, fill = Treatment)) +
  geom_boxplot(alpha = 0.9, outlier.shape = NA) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 15, color = "black"),
        legend.position = "none") +
  labs(x = "",
       y = "OTU richness") +
  scale_fill_manual(values = my_palette) +
  
  # Statistical comparison
  geom_segment(aes(x = 2, xend = 3, y = y_bar, yend = y_bar),
               inherit.aes = F,
               linewidth = 0.7) +
  geom_segment(aes(x = 2, xend = 2, y = y_bar, yend = y_bar - 2),
               inherit.aes = F,
               linewidth = 0.7) +
  geom_segment(aes(x = 3, xend = 3, y = y_bar, yend = y_bar - 2),
               inherit.aes = F,
               linewidth = 0.7) +
  annotate("text", x = 2.5, y = y_bar + 3,
           label = "p = 0.016",
           size = 6)

ggsave("figures/Boxplot-OTU.pdf", height = 20, width = 13, units = "cm")



#Plot

# Filter Collembola amd Clitellata
otus_subset <- otus_class %>%
  filter(Class %in% c("Collembola", "Clitellata"))

ggplot(otus_subset, aes(x = Treatment, y = RelAbundance, fill = Treatment)) +
  geom_boxplot(alpha = 0.9, outlier.shape = NA) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 11, color = "black"),
        legend.position = "none") +
  labs(x = "",
       y = "Read count") +
  scale_fill_manual(values = my_palette) +
  facet_wrap(~ Class, scales = "free_y") 

ggsave("figures/Clitellata-Collembola.pdf", height = 20, width = 20, units = "cm")
  

#Finish
beep()
rm(otus_long, otus_class, otus_order, cols, levs, y_bar, y_max)
gc()
dev.off()
cat("Richness analysed.\n\n\n")





## 1.2 - NMDS + PERMANOVA ----

# Calculate distance
otu_bc <- vegdist(otu_mat, method = "bray")


# NMDS
set.seed(123456)

nmds_bc_otu <- metaMDS(otu_bc, k = 2, try = 999, trymax = 999999)

pdf("figures/NMDS-Bray-OTU-Stressplot.pdf")
stressplot(nmds_bc_otu)
dev.off()

nmds_bc_otu$stress # 0.097 Acceptable

# Extract the scores to plot
nmds_scores <- as.data.frame(scores(nmds_bc_otu, display = "sites"))
nmds_scores$Site <- rownames(nmds_scores)
nmds_scores <- nmds_scores %>%
  left_join(metadata, by = c("Site" = "Site"))


# Plot
ggplot(nmds_scores, aes(NMDS1, NMDS2, color = Treatment, fill = Treatment)) +
  geom_point(size = 5, shape = 21, alpha = 0.6) +
  stat_ellipse(level = 0.95, linewidth = 1) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 15, color = "black"),
        legend.position = "none") +
  scale_fill_manual(values = my_palette) +
  scale_colour_manual(values = my_palette)

ggsave("figures/NMDS-Bray-OTU.pdf", height = 15, width = 15, units = "cm")


# PERMANOVA
permanova_bc <- adonis2(otu_bc ~ Treatment, data = metadata_otu,
                        method = "bray", permutations = 999999)

# Check the assumptions
disp <- betadisper(vegdist(otu_bc, method = "bray"), metadata_otu$Treatment)
anova(disp) #Ok
permutest(disp) #OK
tapply(disp$distances, metadata_otu$Treatment, mean)
anova(disp)
TukeyHSD(disp)

# Results
sink("results/PERMANOVA-Bray-OTU.txt")
permanova_bc # P = 0.07
sink()

# Remove unused objects
rm(otu_bc, nmds_bc_otu, permanova_bc, disp, nmds_scores)



#Finish
beep()
rm(otu_jac, otu_pa, nmds_jac_otu, permanova_jac, disp, nmds_scores)
gc()
dev.off()
cat("NMDS run.\n\n\n")





## 1.3 - MAASLIN ----
taxonomy2 <- taxonomy
taxonomy2 <- taxonomy2[match(colnames(otu_mat), taxonomy2$OTU), ]
all(taxonomy2$OTU == colnames(otu_mat))

# Collapse to Class and Order
otu_class <- t(rowsum(t(otu_mat), group = taxonomy2$Class))
otu_order <- t(rowsum(t(otu_mat), group = taxonomy2$Order))


### 1.3.1 - Class ----
maas_otu <- Maaslin2(
  input_data = otu_class, 
  input_metadata = metadata2, 
  output = "OTU-Class", 
  fixed_effects = "Treatment",
  normalization = "NONE",
  transform = "NONE",
  correction = "BH",
  min_prevalence = 0.8, #80% of the samples
  min_abundance = 0.01) # 1%

# Save the results
res <- maas_otu$results
res$evidence <- "ns"
res$evidence[res$qval < 0.25] <- "explorative"
res$evidence[res$qval < 0.05] <- "robust"
write.csv(res, "results/Maaslin-OTU-Class.csv")


### 1.3.2 - Order----
maas_otu <- Maaslin2(
  input_data = otu_order, 
  input_metadata = metadata2, 
  output = "OTU-Order", 
  fixed_effects = "Treatment",
  normalization = "NONE",
  transform = "LOG",
  correction = "BH",
  min_prevalence = 0.8, #80% of the samples
  min_abundance = 0.01 ) # 1%

# Save the results
res <- maas_otu$results
res$evidence <- "ns"
res$evidence[res$qval < 0.25] <- "explorative"
res$evidence[res$qval < 0.05] <- "robust"
write.csv(res, "results/Maaslin-OTU-Order.csv")



#Finish
beep()
rm(otu_class, otu_order, res, taxonomy2, maas_otu)
gc()
cat("MaAsLin run.\n\n\n")





#---------------------------------------------------------------------------#





# 2 - SOIL CHEMISTRY ----

# Prepare the datasets
soil_table2 <- soil_table[,c(2, 4:12)]


## 2.1 - Comparisons soils ----

sink("results/Kruskal-TOC.txt")

cat("\n\n\n ----- FULL MODEL ----- \n\n\n")
kruskal.test(TOC ~ Treatment, data = soil_table2) # NS

cat("\n\n\n ----- DUNN TEST ----- \n\n\n")
dunnTest(TOC ~ Treatment, data = soil_table2, method = "bh")

sink()



sink("results/Kruskal-TON.txt")

cat("\n\n\n ----- FULL MODEL ----- \n\n\n")
kruskal.test(TON ~ Treatment, data = soil_table2) # NS

cat("\n\n\n ----- DUNN TEST ----- \n\n\n")
dunnTest(TON ~ Treatment, data = soil_table2, method = "bh")

sink()



sink("results/Kruskal-CN.txt")

cat("\n\n\n ----- FULL MODEL ----- \n\n\n")
kruskal.test(C_N ~ Treatment, data = soil_table2) # NS

cat("\n\n\n ----- DUNN TEST ----- \n\n\n")
dunnTest(C_N ~ Treatment, data = soil_table2, method = "bh")

sink()



sink("results/Kruskal-DOC.txt")

cat("\n\n\n ----- FULL MODEL ----- \n\n\n")
kruskal.test(DOC ~ Treatment, data = soil_table2) # NS

cat("\n\n\n ----- DUNN TEST ----- \n\n\n")
dunnTest(DOC ~ Treatment, data = soil_table2, method = "bh")

sink()



sink("results/Kruskal-Nitrate.txt")

cat("\n\n\n ----- FULL MODEL ----- \n\n\n")
kruskal.test(Nitrate ~ Treatment, data = soil_table2) # NS

cat("\n\n\n ----- DUNN TEST ----- \n\n\n")
dunnTest(Nitrate ~ Treatment, data = soil_table2, method = "bh")

sink()



sink("results/Kruskal-Phospate.txt")

cat("\n\n\n ----- FULL MODEL ----- \n\n\n")
kruskal.test(Av_Phosphate ~ Treatment, data = soil_table2) # NS

cat("\n\n\n ----- DUNN TEST ----- \n\n\n")
dunnTest(Av_Phosphate ~ Treatment, data = soil_table2, method = "bh")

sink()



sink("results/Kruskal-pH.txt")

cat("\n\n\n ----- FULL MODEL ----- \n\n\n")
kruskal.test(pH ~ Treatment, data = soil_table2) # NS

cat("\n\n\n ----- DUNN TEST ----- \n\n\n")
dunnTest(pH ~ Treatment, data = soil_table2, method = "bh")

sink()



sink("results/Kruskal-Humic.txt")

cat("\n\n\n ----- FULL MODEL ----- \n\n\n")
kruskal.test(Humic_Ac ~ Treatment, data = soil_table2) # NS

cat("\n\n\n ----- DUNN TEST ----- \n\n\n")
dunnTest(Humic_Ac ~ Treatment, data = soil_table2, method = "bh")

sink()



sink("results/Kruskal-Fulvic.txt")

cat("\n\n\n ----- FULL MODEL ----- \n\n\n")
kruskal.test(Fulvic_Ac ~ Treatment, data = soil_table2) # NS

cat("\n\n\n ----- DUNN TEST ----- \n\n\n")
dunnTest(Fulvic_Ac ~ Treatment, data = soil_table2, method = "bh")

sink()



## 2.2 - Plot comparisons ----

#TOC
ggplot(soil_table, aes(x = Treatment, y = TOC, fill = Treatment)) +
  geom_boxplot(alpha = 0.9, outlier.shape = NA) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 15, color = "black"),
        legend.position = "none") +
  labs(x = "",
       y = expression("Total Organic Carbon (g kg"^{-1}*")")) +
  scale_fill_manual(values = my_palette)

ggsave("figures/Boxplot-TOC.pdf", height = 20, width = 13, units = "cm")


#TON
ggplot(soil_table, aes(x = Treatment, y = TON, fill = Treatment)) +
  geom_boxplot(alpha = 0.9, outlier.shape = NA) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 15, color = "black"),
        legend.position = "none") +
  labs(x = "",
       y = expression("Total Organic Nitrogen (g kg"^{-1}*")")) +
  scale_fill_manual(values = my_palette)

ggsave("figures/Boxplot-TON.pdf", height = 20, width = 13, units = "cm")


#C-N
ggplot(soil_table, aes(x = Treatment, y = C_N, fill = Treatment)) +
  geom_boxplot(alpha = 0.9, outlier.shape = NA) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 15, color = "black"),
        legend.position = "none") +
  labs(x = "",
       y = "C:N Ratio") +
  scale_fill_manual(values = my_palette)

ggsave("figures/Boxplot-C-N.pdf", height = 20, width = 13, units = "cm")


#DOC
ggplot(soil_table, aes(x = Treatment, y = DOC, fill = Treatment)) +
  geom_boxplot(alpha = 0.9, outlier.shape = NA) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 15, color = "black"),
        legend.position = "none") +
  labs(x = "",
       y = expression("Dissolved Organic Carbon (g kg"^{-1}*")")) +
  scale_fill_manual(values = my_palette)

ggsave("figures/Boxplot-DOC.pdf", height = 20, width = 13, units = "cm")


#Nitrate
ggplot(soil_table, aes(x = Treatment, y = Nitrate, fill = Treatment)) +
  geom_boxplot(alpha = 0.9, outlier.shape = NA) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 15, color = "black"),
        legend.position = "none") +
  labs(x = "",
       y = expression("Nitrate (g kg"^{-1}*")")) +
  scale_fill_manual(values = my_palette)

ggsave("figures/Boxplot-Nitrate.pdf", height = 20, width = 13, units = "cm")


#Av_Phosphate
ggplot(soil_table, aes(x = Treatment, y = Av_Phosphate, fill = Treatment)) +
  geom_boxplot(alpha = 0.9, outlier.shape = NA) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 15, color = "black"),
        legend.position = "none") +
  labs(x = "",
       y = expression("Available Phosphate (g kg"^{-1}*")")) +
  scale_fill_manual(values = my_palette)

ggsave("figures/Boxplot-Av_Phosphate.pdf", height = 20, width = 13, units = "cm")


#pH
ggplot(soil_table, aes(x = Treatment, y = pH, fill = Treatment)) +
  geom_boxplot(alpha = 0.9, outlier.shape = NA) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 15, color = "black"),
        legend.position = "none") +
  labs(x = "",
       y = "Soil pH") +
  scale_fill_manual(values = my_palette)

ggsave("figures/Boxplot-pH.pdf", height = 20, width = 13, units = "cm")


#Humic_Ac
ggplot(soil_table, aes(x = Treatment, y = Humic_Ac, fill = Treatment)) +
  geom_boxplot(alpha = 0.9, outlier.shape = NA) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 15, color = "black"),
        legend.position = "none") +
  labs(x = "",
       y = expression("Humic Acids (g kg"^{-1}*")")) +
  scale_fill_manual(values = my_palette)

ggsave("figures/Boxplot-Humic_Ac.pdf", height = 20, width = 13, units = "cm")


#Fulvic_Ac
ggplot(soil_table, aes(x = Treatment, y = Fulvic_Ac, fill = Treatment)) +
  geom_boxplot(alpha = 0.9, outlier.shape = NA) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 15, color = "black"),
        legend.position = "none") +
  labs(x = "",
       y = expression("Fulvic Acids (g kg"^{-1}*")")) +
  scale_fill_manual(values = my_palette)

ggsave("figures/Boxplot-Fulvic_Ac.pdf", height = 20, width = 13, units = "cm")

dev.off()


## 2.3 - Bray NMDS ----

# Calculate distance
soil_bc <- vegdist(soil_mat, method = "bray")


# NMDS
set.seed(123)

nmds_bc_soil <- metaMDS(soil_bc, k = 2, try = 999, trymax = 999999)

pdf("figures/NMDS-Bray-Soil-Stressplot.pdf")
stressplot(nmds_bc_soil)
dev.off()

nmds_bc_soil$stress # 0.018 Acceptable

# Extract the scores to plot
nmds_scores <- as.data.frame(scores(nmds_bc_soil, display = "sites"))
nmds_scores$Site <- rownames(nmds_scores)
nmds_scores <- nmds_scores %>%
  left_join(metadata, by = c("Site" = "Site"))


# Plot
ggplot(nmds_scores, aes(NMDS1, NMDS2, color = Treatment, fill = Treatment)) +
  geom_point(size = 5, shape = 21, alpha = 0.6) +
  stat_ellipse(level = 0.95, linewidth = 1) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 15, color = "black"),
        legend.position = "none") +
  scale_fill_manual(values = my_palette) +
  scale_colour_manual(values = my_palette)

ggsave("figures/NMDS-Bray-Soil.pdf", height = 15, width = 15, units = "cm")


# PERMANOVA
permanova_bc <- adonis2(soil_bc ~ Treatment, data = metadata,
                        method = "bray", permutations = 999999)

# Check the assumptions
disp <- betadisper(vegdist(soil_bc, method = "bray"), metadata$Treatment)
anova(disp) #Ok
permutest(disp) #OK
tapply(disp$distances, metadata$Treatment, mean)
anova(disp)
TukeyHSD(disp)

# Results
sink("results/PERMANOVA-Bray-Soil.txt")
permanova_bc # NS
sink()

#Finish
beep()
rm(soil_bc, soil_table2, nmds_bc_soil, permanova_bc, disp, nmds_scores, soil_long)
gc()
dev.off()
cat("Chemical analysis run.\n\n\n")





#---------------------------------------------------------------------------#





# 3 - CAP SCALE ----

# Prepare the data
soil2 <- soil_table[,c(2, 4:12)]
soil2 <- soil2[-9,]


# Hellinger transformation of the community data
otu_hell <- decostand(otu_mat, method = "hellinger")


# Full Model
full_model <- capscale(otu_hell ~ ., distance = "bray", data = soil2)


# Null Model
null_model <- capscale(otu_hell ~ 1, distance = "bray", data = soil2)


# Forward selection
set.seed(123456)

final_model <- ordistep(null_model, scope = formula(full_model),
                        direction = "forward", perm.max = 999999, trace = TRUE)


cap_model <- final_model
summary(cap_model)

# Full ANOVA 
cap_anova <- anova.cca(cap_model, permutations = 999999)
print(cap_anova)

# Terms ANOVA
cap_anova_terms <- anova.cca(cap_model, by = "terms", permutations = 999999)
print(cap_anova_terms)

sink("results/CapScale.txt")
print(cap_anova)
print(cap_anova_terms)
sink()

#FINISH
gc()
cat("Capscale run.\n\n\n")
rm(soil2, otu_hell, full_model, null_model, cap_anova_terms, cap_anova,
   cap_model, final_model)





#---------------------------------------------------------------------------#






#FINISH
gc()
rm(list = ls())

beep(3)
.end_time <- Sys.time()
elapsed <- difftime(.end_time, .start_time, units = "mins")
cat("Script run in", round(elapsed, 2), "minutes\n")
