setwd("~/Proyectos/Atacama2020_MG/")

library(data.table) ##make tables
library(reshape2)
library(vegan) ## diversity
library(ape) ## diversity
library(ggplot2) ## garphics
library(patchwork) ## arrange graphics
library(viridis) ## colrs
library(pheatmap) ## heatmaps
library(ggpubr) ## arrange graphics
library(splitstackshape) ## tables

options(stringsAsFactors = F) ##imported tables as numbers and chars, not factors
dist_to_SG_order = c("Salar Grande", "ALMA", "Yungay", "Chañaral") ##order for boxplots


contigs = read.csv("contig_abundance_2.csv") ##Counts from quant_bin module, taxonomy from classify module Metawrap
contig_taxonomy = read.csv("contig_taxonomy.csv", sep = "\t")

contigs = merge(contigs, contig_taxonomy, by = "Contig") 
a = subset(contigs, Domain == "Archaea")
b = subset(contigs, Domain == "Bacteria")
v = subset(contigs, Domain == "Viruses")
e = subset(contigs, Domain == "Eukaryota")
contigs = rbind(a,b) ##just keep archaea and bacteria

bins = read.csv("bin_abundance2.csv", header = T) ##Counts from quant_bin module, taxonomy from classify module Metawrap
bins[,c(2,3,6)] = NULL
bin_domain = read.csv("bin_domain2.csv", header = T)
bins = merge(bins, bin_domain, by = "Genomic_bins")
a = subset(bins, Domain == "Archaea")
b = subset(bins, Domain == "Bacteria")
bins = rbind(a,b)

##-----Transpose tables for biodiversity (counts are already normalized to sample size in Metawrap)-----
###transpose contigs table
contigs_0.1 = contigs
contig_rnames = contigs_0.1$Contig
contig_cnames = colnames(contigs_0.1)
contigs_0.1 = transpose(contigs_0.1)

## transform numbers to numeric 
chars = contigs_0.1[19:27,]
nums = contigs_0.1[2:15,]
counts = matrix(NA, nrow = length(nums[,1]), ncol = length(nums[1,]))
for(i in 1:length(nums))
{
  temp = as.numeric(nums[,i])
  counts[,i] = temp
}

contigs_tax = as.data.frame(counts)
colnames(contigs_tax) = contig_rnames
rownames(contigs_tax) = contig_cnames[2:15] ##sample names

##Transpose bin table
bin_cnames =  colnames(bins)
bin_rnames = bins$Genomic_bins
bins_0.1 = transpose(bins)

nums = bins_0.1[2:15,]
counts = matrix(NA, nrow = length(nums[,1]), ncol = length(nums[1,]))
for(i in 1:length(nums))
{
  temp = as.numeric(nums[,i])
  counts[,i] = temp
}

bins_tax = as.data.frame(counts) 
colnames(bins_tax) = bin_rnames
rownames(bins_tax) = bin_cnames[2:15]

##-----Biodiversity analysis--------------------------------------
##Archaea:Bacteria contig
arch_contig = apply((contigs_tax[,1:14598]), MARGIN = 1, FUN = sum)
bac_contig = apply((contigs_tax[,14599:length(contigs_tax[1,])]), MARGIN = 1, FUN = sum)
domain_contig = as.data.frame(cbind(arch_contig, bac_contig))
colnames(domain_contig) = c("Archaea", "Bacteria")

domain_contig$Ratio = domain_contig$Archaea/domain_contig$Bacteria

domain_contig$Location = c(rep("ALMA", 3), rep("Chañaral",3),
                           rep("Salar Grande", 5), rep("Yungay",3))

domain_contig_means = compare_means(Ratio ~ Location, data = domain_contig, method = 't.test')
domain_contig_comparisons = list(c("Salar Grande", "ALMA"))

ratio_contig = ggplot(domain_contig, aes(x = Location, y = Ratio, fill = Location)) + 
  geom_boxplot() + scale_fill_viridis_d()
ratio_contig = ratio_contig + labs(y = "Archaea:bacteria ratio", x = NULL) 
ratio_contig = ratio_contig + theme_minimal() +
  theme(text = element_text(size = 25), 
        axis.text.x = element_text(angle = 45, hjust =1),
        legend.position = "none") +
  scale_x_discrete(limits = dist_to_SG_order)
print(ratio_contig)

##Archaea:Bacteria bins 
arch_bins = apply((bins_tax[,1:69]), MARGIN = 1, FUN = sum)
bac_bins = apply((bins_tax[,70:length(bins_tax[1,])]), MARGIN = 1, FUN = sum)
domain_bin = as.data.frame(cbind(arch_bins, bac_bins))
colnames(domain_bin) = c("Archaea", "Bacteria")

domain_bin$Ratio = domain_bin$Archaea/domain_bin$Bacteria

domain_bin$Location = c("Yungay", "ALMA", "Salar Grande", "Chañaral", "ALMA", 
                        "Yungay", "Chañaral", "Yungay", "Chañaral", 
                        rep("Salar Grande",4),"ALMA") 

domain_bin_means = compare_means(Ratio ~ Location, data = domain_bin, method = 't.test')
domain_bin_comparisons = list(c("ALMA", "Salar Grande"))

ratio_bin = ggplot(domain_bin, aes(x = Location, y = Ratio, fill = Location)) + 
  geom_boxplot() + scale_fill_viridis_d()
ratio_bin = ratio_bin + labs(y = "Archaea:bacteria ratio", x = NULL) 
ratio_bin = ratio_bin + theme_minimal() +
  theme(text = element_text(size = 25), 
        axis.text.x = element_text(angle = 45, hjust =1),
        legend.position = "top") +
  scale_x_discrete(limits = dist_to_SG_order)
print(ratio_bin)

### Samples clustering - PCoA contigs
contigs_beta = vegdist(decostand(contigs_tax,"total"), method = "bray")
contigs_pcoa = pcoa(contigs_beta)

group = domain_contig$Location
anosim_contigs = anosim(decostand(contigs_tax, "total"), grouping = group, distance = "bray", permutations = 999)
summary(anosim_contigs)

pcoa_table_contigs = as.data.frame(cbind(contigs_pcoa$vectors[,1],
                                         contigs_pcoa$vectors[,2]))
pcoa_table_contigs$Location = domain_contig$Location
colnames(pcoa_table_contigs) = c("PCoA1", "PCoA2", "Location")

contigs_pcoa_plot = ggplot(pcoa_table_contigs, aes(x = PCoA1, y = PCoA2, colour = Location)) +
  geom_point(size = 6)
contigs_pcoa_plot = contigs_pcoa_plot + geom_text(aes(label = row.names(pcoa_table_contigs),
                                                      x = PCoA1, y = PCoA2), size = 6, vjust = 2)
contigs_pcoa_plot = contigs_pcoa_plot + theme_minimal() + labs(x = "PCoA1 33.3%",
                                                               y = "PCoA2 46.4%") +
  theme(text = element_text(size = 25),
        legend.position = "none") +
  scale_color_viridis_d()
print(contigs_pcoa_plot)

## Samples clustering - PCoA bins
bins_beta = vegdist(decostand(bins_tax, "total"), method = "bray")
bins_pcoa = pcoa(bins_beta)

group_bins = domain_bin$Location
anosim_bins = anosim(decostand(bins_tax, "total"), grouping = group_bins, distance = "bray", permutations = 999)
summary(anosim_contigs)

pcoa_table_bins = as.data.frame(cbind(bins_pcoa$vectors[,1],
                                      bins_pcoa$vectors[,2]))
pcoa_table_bins$Location = domain_bin$Location
colnames(pcoa_table_bins) = c("PCoA1", "PCoA2", "Location")

bins_pcoa_plot = ggplot(pcoa_table_bins, aes(x = PCoA1, y = PCoA2, colour = Location)) +
  geom_point(size = 6)
bins_pcoa_plot = bins_pcoa_plot + geom_text(aes(label = row.names(pcoa_table_bins),
                                                x = PCoA1, y = PCoA2), size = 6, vjust = 2)
bins_pcoa_plot = bins_pcoa_plot + theme_minimal() + labs(x = "PCoA1 35.7%",
                                                         y = "PCoA2 50.2%") +
  theme(text = element_text(size = 25),
        legend.position = "none") +
  scale_color_viridis_d()
print(bins_pcoa_plot)

## HEATMAP -- bins
bin_phyla = read.csv("bin_phylum2.csv", header = T)
bin_phyla = bin_phyla[-49,]
rownames(bin_phyla) = bin_phyla$Genomic_bins
bin_phyla$Genomic_bins = NULL

bin_tax_hm = as.data.frame(t(bins_tax))
sample_ID = colnames(bin_tax_hm)
bin_location = as.data.frame(domain_bin$Location, sample_ID)

pheatmap(decostand(bin_tax_hm, "total", MARGIN = 2)*100, 
         annotation_row = bin_phyla, show_rownames = F) ## with individual samples

bins_tax$Location = domain_bin$Location
mean_by_loc_bins = aggregate(.~ Location, bins_tax, function(x) c(mean = mean(x)))
rownames(mean_by_loc_bins) = mean_by_loc_bins$Location
mean_by_loc_bins = as.data.frame(t(mean_by_loc_bins[,-1])) 

mean_by_loc_bins = decostand(mean_by_loc_bins, "total", MARGIN = 2)*100
mean_by_loc_bins = mean_by_loc_bins[,dist_to_SG_order]

pheatmap(mean_by_loc_bins, annotation_row = bin_phyla,
         show_rownames = T, fontsize_row = 4, cluster_cols = F) ## By location

to_subset = c("MAG.i.2", "MAG.i.4", "MAG.i.27", "MAG.i.34", "MAG.i.36", "MAG.i.39")
bin_phyla_min = bin_phyla[(rownames(bin_phyla)%in%to_subset),]
bin_phyla_min = as.data.frame(cbind(to_subset,bin_phyla_min))
rownames(bin_phyla_min) = bin_phyla_min$to_subset
bin_phyla_min$to_subset = NULL
colnames(bin_phyla_min) = "Phylum"
mean_by_loc_bins_min = mean_by_loc_bins[(rownames(mean_by_loc_bins)%in%to_subset),]
ann_cols_min = list(Phylum = c(Euryarchaeota = "#b80045", 
                               Bacteroidetes = "#ff99f2", 
                               Cyanobacteria = "#5bb000"))
pheatmap(mean_by_loc_bins_min, cluster_cols = F, annotation_row = bin_phyla_min, 
         annotation_colors = ann_cols_min,
         fontsize = 15)

### Phylum boxplots by location
## Contigs
phylum_contig = as.data.frame(t(contigs_tax))
phylum = contigs$Phylum
phylum_contig = cbind(phylum_contig, phylum)
phylum_contig = aggregate(.~phylum, data = phylum_contig, FUN = sum)
rownames(phylum_contig) = phylum_contig$phylum
phylum_contig = cbind(phylum_contig$phylum, decostand(phylum_contig[,2:15], "total", MARGIN = 2)*100)
colnames(phylum_contig) = c("Phylum", colnames(phylum_contig)[2:15])

phylum_contig = subset(phylum_contig, Phylum == "Actinobacteria" |
                         Phylum == "Bacteroidetes" |  
                         Phylum == "Euryarchaeota" | 
                         Phylum == "Proteobacteria" |
                         Phylum == "Cyanobacteria" |
                         Phylum == "Unidentified bacteria") ##Get a table only for these phyla

contigs_gg_bp = melt(phylum_contig)

variable = c("SG1", "SG2", "SG3", "SG4", "SG5", "AL1", "AL3", 'AL4', 'CH1','CH2',
             'CH3', 'YG1', 'YG2', 'YG3')
Location = c(rep("Salar Grande", 5), rep("ALMA", 3), rep("Chañaral", 3),
             rep("Yungay", 3))
id_locations = data.frame(variable, Location) ## id locations to add to contigs table

contigs_gg_bp = merge(contigs_gg_bp, id_locations, by = "variable")
colnames(contigs_gg_bp) = c("ID", "Phylum", "Relative_abundance", "Location")

eury_contigs = subset(contigs_gg_bp, Phylum == "Euryarchaeota")
cyano_contigs = subset(contigs_gg_bp, Phylum == "Cyanobacteria")
bacte_contigs = subset(contigs_gg_bp, Phylum == "Bacteroidetes")
acti_contigs = subset(contigs_gg_bp, Phylum == "Actinobacteria")
unin_contigs = subset(contigs_gg_bp, Phylum == "Unidentified Bacteria")
prote_contigs = subset(contigs_gg_bp, Phylum == "Proteobacteria")

eury_c_means = compare_means(Relative_abundance ~ Location, data = eury_contigs, method = "t.test")
e_c_gg = ggplot(eury_contigs, aes(x = Location, y = Relative_abundance, fill = Location)) + 
  geom_boxplot() + labs(x = "", y = "Relative abundance", title = "Euryarchaeota") +
  scale_fill_viridis_d()
e_c_gg = e_c_gg + theme_minimal() +
  theme(text = element_text(size = 15),
        axis.text.x = element_blank(),
        legend.position = "none") +
  scale_x_discrete(limits = dist_to_SG_order)
print(e_c_gg)

cyano_c_means = compare_means(Relative_abundance ~ Location, data = cyano_contigs, method = "t.test")
c_c_gg = ggplot(cyano_contigs, aes(x = Location, y = Relative_abundance, fill = Location)) + 
  geom_boxplot() + labs(x = "", y = "", title = "Cyanobacteria") +
  stat_compare_means(comparisons = list(c("ALMA", "Salar Grande")), method = "t.test", label = "p.signif") +
  scale_fill_viridis_d()
c_c_gg = c_c_gg + theme_minimal() +
  theme(text = element_text(size = 15),
        axis.text.x = element_blank(),
        legend.position = "none") +
  ylim(0,30) +
  scale_x_discrete(limits = dist_to_SG_order)
print(c_c_gg)

bacte_c_means = compare_means(Relative_abundance ~ Location, data = bacte_contigs, method = "t.test")
b_c_gg = ggplot(bacte_contigs, aes(x = Location, y = Relative_abundance, fill = Location)) + 
  geom_boxplot() + labs(x = "", y = "", title = "Bacteroidetes") +
  scale_fill_viridis_d()
b_c_gg = b_c_gg + theme_minimal() +
  theme(text = element_text(size = 15),
        axis.text.x = element_blank(),
        legend.position = "none") +
  ylim(0,30) +
  scale_x_discrete(limits = dist_to_SG_order)
print(b_c_gg)

acti_c_means = compare_means(Relative_abundance ~ Location, data = acti_contigs, method = "t.test")
a_c_gg = ggplot(acti_contigs, aes(x = Location, y = Relative_abundance, fill = Location)) + 
  geom_boxplot() + labs(x = "", y = "Relative abundance", title = "Actinobacteria") +
  scale_fill_viridis_d()
a_c_gg = a_c_gg + theme_minimal() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  ylim(0,7) +
  scale_x_discrete(limits = dist_to_SG_order)
print(a_c_gg)

unin_c_means = compare_means(Relative_abundance ~ Location, data = unin_contigs, method = "t.test")
u_c_gg = ggplot(unin_contigs, aes(x = Location, y = Relative_abundance, fill = Location)) + 
  geom_boxplot() + labs(x = "", y = "", title = "Unidentified bacteria") +
  scale_fill_viridis_d()
u_c_gg = u_c_gg + theme_minimal() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  ylim(0,7) +
  scale_x_discrete(limits = dist_to_SG_order)
print(u_c_gg)

prote_c_means = compare_means(Relative_abundance ~ Location, data = prote_contigs, method = "t.test")
p_c_gg = ggplot(prote_contigs, aes(x = Location, y = Relative_abundance, fill = Location)) + 
  geom_boxplot() + labs(x = "", y = "", title = "Proteobacteria") +
  scale_fill_viridis_d()
p_c_gg = p_c_gg + theme_minimal() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  ylim(0,7) +
  scale_x_discrete(limits = dist_to_SG_order)
print(p_c_gg)

(e_c_gg + c_c_gg + b_c_gg)/(a_c_gg + u_c_gg + p_c_gg)

## MAGs
bin_phyla$variable = rownames(bin_phyla)
bins_tax$Location = NULL
bins_tax_rel = decostand(bins_tax, "total")*100

bins_gg_bp = melt(t(bins_tax_rel))
colnames(bins_gg_bp) = c("variable", "ID", "Relative_abundance")

bins_gg_bp = merge(bins_gg_bp, bin_phyla, by = "variable")
colnames(id_locations) = c("ID", "Location")
bins_gg_bp = merge(bins_gg_bp, id_locations, by = "ID")
bins_gg_bp$variable = NULL
bins_gg_bp = aggregate(.~ ID + Phylum + Location, data = bins_gg_bp, sum)

eury_bins = subset(bins_gg_bp, Phylum == "Euryarchaeota")
cyano_bins = subset(bins_gg_bp, Phylum == "Cyanobacteria")
bacte_bins = subset(bins_gg_bp, Phylum == "Bacteroidetes")
acti_bins = subset(bins_gg_bp, Phylum == "Actinobacteria")
unin_bins = subset(bins_gg_bp, Phylum == "Unidentified_Bacteria")
prote_bins = subset(bins_gg_bp, Phylum == "Proteobacteria")

eury_means = compare_means(Relative_abundance ~ Location, data = eury_bins, method = "t.test")
e_b_gg = ggplot(eury_bins, aes(x = Location, y = Relative_abundance, fill = Location)) + 
  geom_boxplot() + labs(x = "", y = "Relative abundance", title = "Euryarchaeota") +
   scale_fill_viridis_d()
e_b_gg = e_b_gg + theme_minimal() +
  theme(text = element_text(size = 15),
        axis.text.x = element_blank(),
        legend.position = "none") +
  scale_x_discrete(limits = dist_to_SG_order)
print(e_b_gg)

cyano_means = compare_means(Relative_abundance ~ Location, data = cyano_bins, method = "t.test")
c_b_gg = ggplot(cyano_bins, aes(x = Location, y = Relative_abundance, fill = Location)) + 
  geom_boxplot() + labs(x = "", y = "", title = "Cyanobacteria") +
  stat_compare_means(comparisons = list(c("ALMA", "Salar Grande")), method = "t.test", label = "p.signif") +
  scale_fill_viridis_d()
c_b_gg = c_b_gg + theme_minimal() +
  theme(text = element_text(size = 15),
        axis.text.x = element_blank(),
        legend.position = "none") +
  ylim(0,35) +
  scale_x_discrete(limits = dist_to_SG_order)
print(c_b_gg)

bacte_means = compare_means(Relative_abundance ~ Location, data = bacte_bins, method = "t.test")
b_b_gg = ggplot(bacte_bins, aes(x = Location, y = Relative_abundance, fill = Location)) + 
  geom_boxplot() + labs(x = "", y = "", title = "Bacteroidetes") +
  scale_fill_viridis_d()
b_b_gg = b_b_gg + theme_minimal() +
  theme(text = element_text(size = 15),
        axis.text.x = element_blank(),
        legend.position = "none") +
  ylim(0,35) +
  scale_x_discrete(limits = dist_to_SG_order)
print(b_b_gg)

acti_means = compare_means(Relative_abundance ~ Location, data = acti_bins, method = "t.test")
a_b_gg = ggplot(acti_bins, aes(x = Location, y = Relative_abundance, fill = Location)) + 
  geom_boxplot() + labs(x = "", y = "Relative abundance", title = "Actinobacteria") +
  scale_fill_viridis_d()
a_b_gg = a_b_gg + theme_minimal() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  ylim(0,15) +
  scale_x_discrete(limits = dist_to_SG_order)
print(a_b_gg)

unin_means = compare_means(Relative_abundance ~ Location, data = unin_bins, method = "t.test")
u_b_gg = ggplot(unin_bins, aes(x = Location, y = Relative_abundance, fill = Location)) + 
  geom_boxplot() + labs(x = "", y = "", title = "Unidentified bacteria") +
  scale_fill_viridis_d()
u_b_gg = u_b_gg + theme_minimal() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  ylim(0,15) +
  scale_x_discrete(limits = dist_to_SG_order)
print(u_b_gg)

prote_means = compare_means(Relative_abundance ~ Location, data = prote_bins, method = "t.test")
p_b_gg = ggplot(prote_bins, aes(x = Location, y = Relative_abundance, fill = Location)) + 
  geom_boxplot() + labs(x = "", y = "", title = "Proteobacteria") +
  scale_fill_viridis_d()
p_b_gg = p_b_gg + theme_minimal() + 
  theme(text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  ylim(0,15) +
  scale_x_discrete(limits = dist_to_SG_order)
print(p_b_gg)

(e_b_gg + c_b_gg + b_b_gg)/(a_b_gg + u_b_gg + p_b_gg)

####--------------------- ALGAE AND VIRUS ----------------------------------------------------------
## Dolichomastix rel abundamce
## Dolichomastix genome recovered from https://github.com/ursky/metatranscriptome_paper/tree/master/MAGS
## Quantification from metawrap Quant_bins module
Dol = read.csv("Dolicho_abundance.csv", header = T, row.names = 1, sep = '\t')
bins_dol = cbind(bins_tax, Dol)

bins_dol_rel_abun = decostand(bins_dol, method = "total")*100

Dol_rel_abun = as.data.frame(bins_dol_rel_abun$Dolichomastix)
Dol_rel_abun = cbind(Dol_rel_abun, domain_bin$Location)
rownames(Dol_rel_abun) = rownames(bins_dol_rel_abun)
colnames(Dol_rel_abun) = c("Rel_abun", "Location")

dolicho_means = compare_means(Rel_abun ~ Location, data = Dol_rel_abun, method = "t.test")

dolico_gg = ggplot(Dol_rel_abun, aes(x = Location, y = Rel_abun, fill = Location)) + 
  geom_boxplot() + scale_fill_viridis_d() +
  labs(y = "*Dolichomastix* relative abundance", x = "")
dolico_gg = dolico_gg + theme_minimal() +
  theme(text = element_text(size = 25),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = ggtext::element_markdown(),
        legend.position = "none") +
  scale_x_discrete(limits = dist_to_SG_order) 
print(dolico_gg)

### Viral contigs
### List of contigs with Virsorter2
### Classification with Cenote-taker2
viral_contigs = read.csv("Virus_contig.csv", header = T)
viral_contigs_count = read.csv("vir_contig_count.csv")

virus_abun = merge(viral_contigs, viral_contigs_count, by = "Contig")

virus_abun = aggregate(.~ Virus_tax, data = virus_abun[,-1], FUN = sum)
rel_abun_halovirus = decostand(virus_abun[,c(2:15)], MARGIN = 2, method = "total")*100

gg_virus = melt(rel_abun_halovirus)
gg_virus = cbind(gg_virus, rep(virus_abun$Virus_tax, 14))
gg_virus = cbind(gg_virus, c(rep("ALMA", 21), rep("Chañaral", 21),
                             rep("Salar Grande", 35), rep("Yungay", 21)))
colnames(gg_virus) = c("Sample", "Rel_abun", "Virus_tax", "Location")
gg_halovirus = gg_virus[gg_virus$Virus_tax == "Halovirus",]

halovirus_means = compare_means(Rel_abun ~ Location, data = gg_halovirus, method = "t.test")

halovirus_boxplot = ggplot(gg_halovirus, aes(x = Location, y = Rel_abun, fill = Location)) +
  geom_boxplot() + scale_fill_viridis_d() +
  labs(y = "Halovirus relative abundance", x = "")
halovirus_boxplot = halovirus_boxplot + theme_minimal() +
  theme(text = element_text(size = 25),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_x_discrete(limits = dist_to_SG_order)
print(halovirus_boxplot)


####---------------------- BIN FUNTIONAL ANALYSIS --------------------------------------------------
### Reanding tables from JGI and KO encyclopedia, mergin pathways to IMG table
function_profile = read.csv("function_profile.csv", header = T, sep = '\t')
function_profile[,c(14:16)] = NULL ## delete Salar Grande TopHill
number_reads = c("AL1" = 18010716, "AL3" = 18765103, "AL4" = 21126380, 
                 "CH1" = 23782897, "CH2" = 17557563, "CH3" = 7727612,
                 "SG1" = 22288669, "SG2" = 20968987, "SG3" = 11711914, "SG4" = 20148335, "SG5" = 30407631,
                 "YG1" = 24526940, "YG2" = 20694014, "YG3" = 18033928)/1000000 ##for normalization IMG results are in gene counts, analysis should be done in gene counts per million reads
function_profile_norm = mapply(function(x,y) x/y, function_profile[,3:16], t(number_reads)) #normalization
function_profile = cbind(function_profile[,1:2], as.data.frame(function_profile_norm))

kegg_brite =  read.csv("kegg_brite.csv", header = T)

### Functional profile heatmap  --- 40 most abundant pathways
## by Pathway|Function and site
function_brite = merge(kegg_brite, function_profile, by = "KO_ID")
function_brite$Path_Fun = paste(function_brite$Pathway, function_brite$Function, sep = '| ')

function_brite = function_brite[,c(7:21)]
function_brite = cbind(function_brite$Path_Fun, function_brite[,1:14])
colnames(function_brite) = c("Path_Func", colnames(function_brite)[2:15])
function_brite = aggregate(.~ Path_Func, data = function_brite, FUN = sum)
rownames(function_brite) = function_brite$Path_Func

function_brite_pf = decostand(as.data.frame(t(function_brite[,2:15])),
                              "total", MARGIN = 1)*100

function_brite_pf$Location = c(rep("ALMA",3), rep("Chañaral",3), rep("Salar Grande",5),
                               rep("Yungay",3))

function_brite_pf = aggregate(.~ Location, function_brite_pf, function(x) c(mean = mean(x)))
rownames(function_brite_pf) = function_brite_pf$Location
function_brite_pf$Location = NULL
function_brite_pf = as.data.frame(t(function_brite_pf))
function_brite_pf = function_brite_pf[order(rowSums(function_brite_pf),decreasing = T), dist_to_SG_order]

pheatmap(function_brite_pf[1:40,], cluster_cols = F)

#### Isoelectric point and trk potential by site
Location = c(rep("ALMA",3), rep("Chañaral",3), rep("Salar Grande",5), rep("Yungay",3))
iep_by_loc  = read.csv("Isoelectric_points_sites.csv", header = T) #Isoelectric points
iep_by_loc = iep_by_loc[-c(12:14),]
iep_by_loc = cbind(iep_by_loc, Location)

iep_means = compare_means(mean.IeP ~ Location, data = iep_by_loc, method = "t.test")
iep_comparisons = list(c("ALMA", "Salar Grande"))

iep_plot = ggplot(iep_by_loc, aes(x = Location, y = mean.IeP, fill = Location)) + 
  geom_boxplot() + scale_fill_viridis_d()
iep_plot = iep_plot + stat_compare_means(comparisons = iep_comparisons, method = "t.test", label = "p.signif")
iep_plot = iep_plot + labs(x = NULL, y = "Mean isoelectric point")
iep_plot = iep_plot + theme_minimal() + 
  theme(text = element_text(size = 25),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "position") +
  scale_x_discrete(limits = dist_to_SG_order)
print(iep_plot)

## trk potential
function_brite = merge(kegg_brite, function_profile, by = "KO_ID")
function_brite_ra = decostand(function_brite[,7:20], "total", MARGIN = 2)*100
rownames(function_brite_ra) = function_brite$KO_ID
trk_by_loc = function_brite_ra[rownames(function_brite_ra)%in%c("K03498","K03499"),]
trk_by_loc = colSums(trk_by_loc)
trk_by_loc = data.frame(names(trk_by_loc), trk_by_loc)
trk_by_loc = cbind(trk_by_loc, Location)
colnames(trk_by_loc) = c("ID", "trk_potential", "Location")

#trk_by_loc = subset(function_brite, KO_ID == "K03498" | KO_ID == "K03499")
#trk_by_loc = trk_by_loc[,-c(1:6)]
#names_trk = names(trk_by_loc)
#trk_by_loc = as.numeric(colSums(trk_by_loc))
#trk_by_loc = data.frame(names_trk, trk_by_loc)
#colnames(trk_by_loc) = c("ID", "trk_potential")
#trk_by_loc = cbind(trk_by_loc, Location)

trk_means = compare_means(trk_potential ~ Location, data = trk_by_loc, method = "t.test")
trk_comparisons = list(c("ALMA", "Salar Grande"))

trk_plot = ggplot(trk_by_loc, aes(x = Location, y = trk_potential, fill = Location)) + 
  geom_boxplot() + scale_fill_viridis_d()
trk_plot = trk_plot + labs(x = NULL, y = "*trK* relative abundance")
trk_plot = trk_plot + theme_minimal() + 
  theme(text = element_text(size = 25),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = ggtext::element_markdown(),
        legend.position = "none")  +
  scale_x_discrete(limits = dist_to_SG_order)
print(trk_plot)

### Functional anotations from MAGs
## Create prescence/absecence table from MAG KOs
AL1_bin22 = read.csv("AL1_bin.22_Koala.txt", sep = '\t', row.names = NULL, header = F)
AL1_bin22 = as.vector(unique(AL1_bin22$V2))
AL3_bin33 = read.csv("AL3_bin.33_Koala.txt", sep = '\t', row.names = NULL, header = F)
AL3_bin33 = as.vector(unique(AL3_bin33$V2))
SG5_bin30 = read.csv("SG5_bin.30_Koala.txt", sep = '\t', row.names = NULL, header = F)
SG5_bin30 = as.vector(unique(SG5_bin30$V2))
YG1_bin4 = read.csv("YG1_bin.4_Koala.txt", sep = '\t', row.names = NULL, header = F)
YG1_bin4 = as.vector(unique(YG1_bin4$V2))
YG1_bin19 = read.csv("YG1_bin.19_Koala.txt", sep = '\t', row.names = NULL, header = F)
YG1_bin19 = as.vector(unique(YG1_bin19$V2))
YG1_bin29 = read.csv("YG1_bin.29_Koala.txt", sep = '\t', row.names = NULL, header = F)
YG1_bin29 = as.vector(unique(YG1_bin29$V2))

mags = c("AL1_bin22", "AL3_bin33", "SG5_bin30", "YG1_bin19", "YG1_bin29", "YG1_bin4")
mags_annotation = as.data.frame(t(splitstackshape:::charMat(listOfValues = mget(mags), fill = 0L))) ##presence absence from bin list
colnames(mags_annotation) = mags
mags_annotation$KO_ID = rownames(mags_annotation)
mags_annotation = mags_annotation[-1,]

## Saline osmoadaptations - list of salinity adapttation genes NIxon (2019 https://journals.asm.org/doi/10.1128/mSphere.00613-19)
osmo_genes = read.csv("osmo_genes.csv", header = T)

osmo_mags = merge(osmo_genes, mags_annotation, by = "KO_ID")
rownames(osmo_mags) = osmo_mags$Gene
osmo_mags = osmo_mags[order(osmo_mags$Adaptation),]
colnames(osmo_mags) = c(colnames(osmo_mags)[1:5], "MAG.i.2", "MAG.i.4", "MAG.i.27", "MAG.i.34", "MAG.i.36", "MAG.i.39")

annotation_row_adapt = as.data.frame(osmo_mags[,c(3,5)])
annotation_col_mags = data.frame(Phylum = c("Bacteroidetes", "Bacteroidetes", "Euryarchaeota", "Euryarchaeota", "Cyanobacteria", "Euryarchaeota"),
                            #Location = c("CH-SG", "YG", "AL", "YG", "YG", "AL"),
                            row.names = c("MAG.i.2", "MAG.i.39", "MAG.i.4", "MAG.i.36", "MAG.i.27", "MAG.i.34"))

#Mag_cols = list(Phylum = c(Euryarchaeota = "#b80045", Bacteroidetes = "#ff99f2", Cyanobacteria = "#5bb000"))

pheatmap(osmo_mags[,6:11], cluster_rows = F, color = c("gray", "red3"), legend = F,
         annotation_row = annotation_row_adapt[2],
         gaps_row = c(8,16,20), fontsize = 15)