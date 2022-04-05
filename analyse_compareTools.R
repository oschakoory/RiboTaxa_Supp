library(ggplot2)
library(ggpubr)
library(ggpattern)
library(viridis)
setwd("~/Documents/mock_community_GascPeyret2018/MG/Raw_abundance_treated_R")

#fig 7 de la publi
file <- read.table(file = 'db_build_time.tsv', sep = '\t', header = TRUE)
file1<- read.table(file = 'running_time.csv', sep = ',', header = TRUE)
colour <- c("lightblue2")

A<-ggplot(file1, aes(fill=Tools,y=Data, x=Tools)) + 
  geom_bar(stat="identity",width =0.8, alpha=0.7)+ 
  facet_grid (. ~ Purpose, scales = "free",space  = "free") +
  theme(axis.text.x = element_text(angle = 90))+
  geom_text(aes(label = Time,size=14), vjust = -0.3) + 
  theme_bw()+
  labs(x = " ", y = "Running time (minutes)") + 
  ylim(0,150) +
  scale_fill_viridis(discrete = TRUE, option = "D")+
  theme(legend.text = element_text(colour =c("black")),strip.background = element_rect(
    color="white", fill="white"), 
    strip.text.x = element_text(size = 16, color = "black", face = "bold"),
    strip.text.y = element_blank(), 
    axis.text.x = element_text(size=14,angle = 45, hjust=0.95,vjust=0.9),
    axis.title.x = element_text(face = "bold", size=16),
    axis.title.y = element_text(face = "bold", size=16),
    legend.position = "none",panel.spacing = unit(2,"lines"))
  
A

colour<-("lightpink2")
B<-ggplot(file, aes(fill=Purpose,y=Data, x=Tools)) + 
  geom_bar(stat="identity",width =0.8,colour="lightpink2", alpha=0.7)+ 
  facet_grid (. ~ Purpose, scales = "free",space  = "free") +
  theme(axis.text.x = element_text(angle = 90))+
  geom_text(aes(label = Time,size=14), vjust = -0.3) + 
  theme_bw()+
  ylim(0,150) +
  labs(x = " ", y = "Running time (minutes)") + 
  theme(legend.text = element_text(colour =c("black")),
        strip.background = element_rect(color="white", fill="white"), 
        strip.text.x = element_text(size = 16, color = "black", face = "bold"),
        strip.text.y = element_blank(), 
        axis.text.x = element_text(size=14,angle = 45, hjust=0.95,vjust=0.9),
        axis.title.x = element_text(face = "bold", size=16),
        axis.title.y = element_text(face = "bold", size=16),
        legend.position = "none",
        panel.spacing = unit(2,"lines"))+
  scale_fill_manual(values=colour)

ggarrange(B,A, labels = c("A","B"),  heights = c(0.2,0.8),widths = c(0.4,0.6), font.label = list(size = 16))

#fig 2 du publi
file1 <- read.table(file = 'seqleng.tsv', sep = '\t', header = FALSE)

mycolorsB <-c("#69BBD0", "#CF6876" ,"#CF9F68" )
#mycolorsB <- c("#00AFBB", "#E7B800", "#FC4E07")
A <-ggplot(file1, aes(x=V1, y=V2, fill=V4)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin(alpha=0.7, colour="white")+
  stat_summary(
    fun.data = "mean_sdl",  fun.args = list(mult = 1), 
    geom = "pointrange", color = "black",position = position_dodge(0.9)
  )+
  #geom_boxplot(colour="black",width=0.1,position = position_dodge(0.9)) +
  theme_bw()+
  scale_fill_manual(name = "Data Used :", labels = c("Raw MG reads", "SortMeRNA Filtered SSU reads", "BBmap filtered SSU reads"),values=mycolorsB)+
  labs(x = "Tools used to reconstruct SSU sequences", y = "Length of reconstructed sequences (bp)")+ 
  facet_grid (V5 ~ ., scales = "free",space  = "free")+
  #theme(plot.margin=unit(c(0.1,1,1,1),"cm"))+
  scale_y_continuous(limits = c(300, 1500), breaks = seq(300, 1500, by = 150))+
  theme(legend.position="top",strip.text.y = element_blank(),
        axis.text.x = element_text(face = "bold",size=14),
        axis.text.y = element_text(face = "bold",size=14),
        axis.title.y = element_text(face = "bold", size=14),
        axis.title.x = element_text(face = "bold", size=14))

Tool <- c('MATAM','Emirge.py','Emirge.py','Emirge_amplicon.py', 'MetaRib', 'MATAM','Emirge.py','Emirge_amplicon.py', 'MetaRib','Emirge_amplicon.py', 'MetaRib', 'MATAM')
Number <- c(13,12,15, 54, 36, 14, 14 ,50, 27, 42, 21, 12)
Data <- c('C','B','A','A','A','A','C','C','C','B','B','B')
purpose <- c('Number of reconstructed SSU sequences','Number of reconstructed SSU sequences','Number of reconstructed SSU sequences','Number of reconstructed SSU sequences','Number of reconstructed SSU sequences', 'Number of reconstructed SSU sequences', 'Number of reconstructed SSU sequences', 'Number of reconstructed SSU sequences', 'Number of reconstructed SSU sequences','Number of reconstructed SSU sequences','Number of reconstructed SSU sequences','Number of reconstructed SSU sequences')
reconstruct_SSU <- data.frame(Tool, Number,Data,purpose)

#mycolorsB <-c("lightcoral","darkcyan", "goldenrod3" )
B <-ggplot(reconstruct_SSU, aes(fill=Data,y=Number, x=Tool)) + 
  geom_bar(position="dodge", stat="identity",colour="white", alpha=0.7)+
  geom_text(aes(label = Number),size=6, hjust = 0.4, vjust = -0.1, position = position_dodge(width = 1)) +  theme_bw()+
  labs(x = "Tools used to reconstruct SSU sequences", y = "Number of Reconstructed SSU  sequences") +
  facet_grid (purpose ~ ., scales = "free",space  = "free") +
  scale_fill_manual(name = "Data Used :", labels = c("Unfiltered metagenomic reads","BBmap filtered reads", "SortMeRNA Filtered reads"),values=mycolorsB)+ 
  #theme(plot.margin=unit(c(1,1,-0.5,1),"cm"))+ 
  theme(legend.position="top",strip.text.y = element_blank(),
        legend.text = element_text(colour =c("black"), size = 14),
        axis.text.x = element_text(face = "bold",size=14),
        axis.text.y = element_text(face = "bold",size=14),
        axis.title.y = element_text(face = "bold", size=14),
        axis.title.x = element_text(face = "bold", size=14))
ggarrange(B, A, ncol = 2, nrow = 1,labels = c("A", "B"),common.legend = TRUE)


Tool <- c('SortMeRNA','BBmap')
Number <- c(10675, 6339)
purpose <- c('SSU reads extraction','SSU reads extraction')
extract_SSU <- data.frame(Tool, Number,purpose)

library(RColorBrewer)
nb.cols <- 20
mycolorsA <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols)


library(randomcoloR)
n <- 40
mycolorsA <- distinctColorPalette(n)
A <- ggplot(extract_SSU, aes(fill=Tool,y=Number, x=Tool)) + 
  geom_bar(stat="identity",width = 0.4,colour="black")+ 
  facet_grid (. ~ purpose, scales = "free",space  = "free") + 
  scale_x_discrete(guide = guide_axis(angle = 90))+
  geom_text(aes(label = Number), hjust = 1.2) + 
  labs(fill="Tools used to extract SSU reads :", x = "Tools used to extract SSU reads from metagenomis data", y = "Number of SSU reads extracted") + 
  theme(legend.position = "none") + theme_bw()+
  scale_fill_manual(values=mycolorsA)+ 
  theme(legend.position="top") + coord_flip()
A

Tool <- c('MATAM','Emirge.py','Emirge.py','Emirge_amplicon.py', 'MetaRib', 'MATAM','Emirge.py','Emirge_amplicon.py', 'MetaRib','Emirge_amplicon.py', 'MetaRib', 'MATAM')
Number <- c(13,12,15, 54, 36, 14, 14 ,50, 27, 42, 21, 12)
Data <- c('C','B','A','A','A','A','C','C','C','B','B','B')
purpose <- c('Number of reconstructed SSU sequences','Number of reconstructed SSU sequences','Number of reconstructed SSU sequences','Number of reconstructed SSU sequences','Number of reconstructed SSU sequences', 'Number of reconstructed SSU sequences', 'Number of reconstructed SSU sequences', 'Number of reconstructed SSU sequences', 'Number of reconstructed SSU sequences','Number of reconstructed SSU sequences','Number of reconstructed SSU sequences','Number of reconstructed SSU sequences')
reconstruct_SSU <- data.frame(Tool, Number,Data,purpose)

n <- 20
mycolorsB <- distinctColorPalette(n)
ggplot(reconstruct_SSU, aes(fill=Data,y=Number, x=Tool)) + 
  geom_bar(position="dodge", stat="identity",colour="black")+ 
  geom_text(aes(label = Number), hjust = 0.4, vjust = -0.1, position = position_dodge(width = 1)) +  theme_bw()+
  labs(x = "Tools used to reconstruct SSU sequences", y = "Number of Reconstructed SSU  sequences") +
  facet_grid (purpose ~ ., scales = "free",space  = "free") +
  scale_fill_manual(name = "Data Used :", labels = c("Unfiltered metagenomic reads","BBmap filtered reads", "SortMeRNA Filtered reads"),values=mycolorsB)+ 
  #theme(plot.margin=unit(c(1,1,-0.5,1),"cm"))+ 
  theme(legend.position="top",strip.text.y = element_blank())

B

Tool <- c('95%','100%')
Number <- c(22, 50)
purpose <- c('Parameter for sequence reconstruction','Parameter for sequence reconstruction')
parameter_extract_SSU <- data.frame(Tool, Number,purpose)

n <- 50
mycolorsD <- distinctColorPalette(n)
C <- ggplot(parameter_extract_SSU, aes(fill=Tool,y=Number, x=Tool)) + 
  geom_bar(stat="identity",width = 0.4,colour="black")+ 
  facet_grid (. ~ purpose, scales = "free",space  = "free") + 
  theme(axis.text.x = element_text(angle = 90))+
  geom_text(aes(label = Number), vjust =-0.2) + 
  labs(fill = "Sequence dentity :" ,x = "Sequences identity during sequence reconstruction", y = "Number of SSU sequences reconstructed") + 
  theme(legend.position = "top") + theme_bw()+ 
  scale_fill_manual(values=mycolorsD)+ 
  theme(legend.position="top")

C

Tool <- c('Sklearn classifier', 'RDP classifier','Centrifuge','Kraken2', 'Sklearn classifier', 'RDP classifier')
Number <- c(14,6,14,15,3,10)
Data <- c('Species-level','Species-level','Genus-level','Genus-level','Genus-level','Genus-level')
purpose <- c('Taxonomic identification','Taxonomic identification','Taxonomic identification', 'Taxonomic identification', 'Taxonomic identification', 'Taxonomic identification')
identify_SSU <- data.frame(Tool, Number,Data,purpose)

n <- 40
mycolorsD <- distinctColorPalette(n)
D <- ggplot(identify_SSU, aes(fill=Data,y=Number, x=Tool)) + 
  geom_bar( stat="identity", width = 0.6)+ 
  geom_text(aes(label = Number),position = position_stack(vjust = 0.5))   +
  labs( fill = "Taxonomy :",x = "Tools used to classify sequences", y = "Number of taxonomy identified") +
  facet_grid (. ~ purpose, scales = "free",space  = "free")+ 
  scale_fill_manual(values=mycolorsD)+ 
  theme_bw()+ 
  theme(legend.position="top")

D
#multiplot(A,C,B, D, cols=2, labels = c("A","B", "C", "D"))


#comparing RiboTaxa with existing pipelines
Tool <- c('RiboTaxa', 'METAXA2','phyloFlash(BS)','phyloFlash(BE)', 'phyloFlash(SE)','RiboTaxa', 'METAXA2','phyloFlash(BE)', 'phyloFlash(SE)', 'Centrifuge', 'Kraken2','RiboTaxa', 'METAXA2','phyloFlash(BS)','phyloFlash(BE)', 'phyloFlash(SE)','Centrifuge', 'Kraken2')
Number <- c(14,10,12,9,9,3,10,1,1,9,16,11,8,16,18,18,19,12)
Data <- c('1','1','1','1','1','2','2','2','2','2','2','3','3','3','3','3','3','3')
#purpose <- c('Comparing RiboTaxa with existing tools','Comparing RiboTaxa with existing tools','Comparing RiboTaxa with existing tools', 'Comparing RiboTaxa with existing tools', 'Comparing RiboTaxa with existing tools', 'Comparing RiboTaxa with existing tools', 'Comparing RiboTaxa with existing tools','Comparing RiboTaxa with existing tools','Comparing RiboTaxa with existing tools','Comparing RiboTaxa with existing tools','Comparing RiboTaxa with existing tools','Comparing RiboTaxa with existing tools','Comparing RiboTaxa with existing tools','Comparing RiboTaxa with existing tools')
compare_tool <- data.frame(Tool, Number,Data)

ggplot(data=compare_tool, aes(x="", y=Number, fill=Data)) +
  geom_bar(stat="identity") +
  coord_polar(theta="y") +
  facet_wrap(~Tool, ncol=4) + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank()) +
  geom_text(aes(label = Number), 
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(name = "Taxonomy :", values = c("#d8b365",  "#C3D7A4", "#00AFBB"),labels = c("Species-level", "Genus-level", "Not identified") )+ 
  theme(legend.position="top")


E <-ggplot(compare_tool, aes(x="", y=Number, fill=Data)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="top") +
  geom_text(aes(y = Number, label = Data), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")+
  facet_wrap(~Tool, ncol=4) +
  geom_text(aes(label = Number), 
            position = position_stack(vjust = 0.5))+
  scale_fill_discrete(name = "Taxonomy :", labels = c("Species-level", "Genus-level", "Not identified") )

  #comparing RiboTaxa with existing pipelines
  Tool <- c('Centrifuge', 'Kraken2','METAXA2', 'phyloFlash (BE)', 'phyloFlash (BS)','phyloFlash (SE)', 'RiboTaxa','Centrifuge', 'Kraken2','METAXA2', 'phyloFlash (BE)', 'phyloFlash (BS)','phyloFlash (SE)', 'RiboTaxa')
  Percentage <- c(28.125,43.243,66.667,100,100,100,100,32.143,57.429,71.4286,42.8571,35.7143,35.7143,60.7143)
  Measure <- c('Efficiency', 'Efficiency', 'Efficiency','Efficiency','Efficiency','Efficiency','Efficiency','Accuracy','Accuracy','Accuracy','Accuracy','Accuracy','Accuracy','Accuracy')
  purpose <- c('Without sequence reconstruction','Without sequence reconstruction','Without sequence reconstruction','With sequence reconstruction','With sequence reconstruction','With sequence reconstruction','With sequence reconstruction','Without sequence reconstruction','Without sequence reconstruction','Without sequence reconstruction','With sequence reconstruction','With sequence reconstruction','With sequence reconstruction','With sequence reconstruction')
  accuracy_Efficiency <- data.frame(Tool, Percentage,Measure, purpose)
  
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

ggplot(accuracy_Efficiency, aes(x=Tool, y=Percentage)) +
  geom_point(aes(color=Percentage,shape=Measure),alpha=0.7, size=5)+ 
  scale_shape_manual(values = c(16,18)) +
  facet_grid (. ~ purpose, scales = "free",space  = "free")+
  labs(x = "Tool used for taxonomy", y = "Percentage of Tool Accurracy and efficiency to infer taxa ") +
  theme_bw()+
  scale_colour_gradientn(colours = myPalette(5), limits=c(0, 100))


abundance <- read.table(file = 'modified_rawResults_forR.csv', sep = ',', header = TRUE)

#abundance <- arrange(abundance,Species)

#either
library(RColorBrewer)
nb.cols <- 47
mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols)

#or
library(randomcoloR)
n <- 51
mycolors <- distinctColorPalette(n)
#mycolors <- c("royalblue3","tomato3","darkgreen","lightpink3","lightskyblue3","deeppink3","chocolate3","darkorchid3","indianred3","khaki3","goldenrod3","firebrick3","aquamarine3","lightsalmon3","springgreen3","peachpuff3","cyan3","coral3","slateblue3","maroon3","ivory3","cadetblue3","chartreuse3","turquoise3","mediumvioletred","darkolivegreen3","pink","lightblue1","moccasin","azure3","bisque1","darkolivegreen1","lightsalmon","cadetblue1","wheat","palegreen","lavender","coral","skyblue","darkseagreen1","burlywood1","rosybrown1","khaki1","paleturquoise1","slategray1","peachpuff","grey70","aquamarine1","bisque2","mistyrose1","lightgreen","seashell2","salmon","tan","thistle","steelblue1","snow3","lightcyan2","lightgoldenrodyellow","olivedrab1","tan1","rosybrown2","honeydew2","darkseagreen2","lemonchiffon3","lightblue2","azure3","pink","grey89","burlywood","darkseagreen2","violet","darkslategray1")
mycolors <- c("turquoise4","purple3","darkgoldenrod2","darkseagreen3","maroon3","darkolivegreen3","slateblue1","darkolivegreen","lightblue3","darkorange3","skyblue3","chartreuse3","aquamarine3","lightsalmon3","springgreen3","peachpuff3","cyan3","coral3","slateblue3","maroon3","ivory3","cadetblue3","chartreuse3","turquoise3","mediumvioletred","darkolivegreen3","steelblue1","pink","lightblue1","moccasin","azure3","bisque1","darkolivegreen1","lightsalmon","cadetblue1","wheat","palegreen","lavender","coral","skyblue","darkseagreen1","burlywood1","rosybrown1","khaki1","paleturquoise1","slategray1","peachpuff","grey70","aquamarine1","bisque2","mistyrose1","lightgreen","seashell2","salmon","tan","thistle","steelblue","snow3","lightcyan2","lightgoldenrodyellow","olivedrab1","tan1","rosybrown2","honeydew2","darkseagreen2","lemonchiffon3","lightblue2","azure3","pink","grey89","burlywood","darkseagreen2","violet","darkslategray1")

data.labs <- c("Reference", "Genus-level classification")
names(data.labs) <- c("Abundance Reference", "Genus-level")

library(RColorBrewer)
myColors <- c("red","blue")
names(myColors) <- levels(abundance$Microbe)
#data.col <- c("black", "red")
#names(data.col) <- c("Present", "Absent")


cols <- c("Present" = "Black", "Absent"="red")
Genus <- 
ggplot(abundance,aes(fill=as.factor(Species_number),y=Abundance, x=Data., alpha=as.factor(Microbe))) + 
  geom_bar(alpha=0.6,width = 0.8, stat="identity",position = position_stack(reverse = TRUE), size=0.5)+  
  facet_grid(wrap ~ Taxonomy, labeller = labeller(Taxonomy=data.labs), scales = "free", space = "free") + 
  scale_fill_manual(values = mycolors, name = "" , labels = c("Clostridium","Halomicrobium","Saccharophagus","Tsukamurella","Novosphingobium","Desulfovibrio","Ruegeria","Pseudomonas","Corynebacterium","Geobacter","Pedobacter","Roseobacter","Cellulomonas","Saccharopolyspora","Lactobacillus","Halogeometricum","Planctomyces","Methanoculleus","Methanospirillum","Levilactobacillus","Flavobacterium","Streptomyces","Methanocorpusculum","Listeria","Sphingobium","Methanobrevibacter","Methanococcus","Acinetobacter","Ammopiptanthus","Anaerosinus","Bacillus","Blautia","Cutibacterium","Endomicrobium","Escherichia-Shigella","Fusobacterium","Klebsiella","Lacticaseibacillus","Mycobacterium","Myroides","Neisseria","Nitrospina","Pseudohongiella","Romboutsia","Sphingomonas","Sphingopyxis","Streptococcus","Vibrio","Alteromonadaceae","Massilia","Methylobacterium","Rhodobacteraceae","Halobacillus","Planococcus","Abiotrophia","Facklamia","Dolosigranulum","Enterococcus","Pediococcus","Pedomicrobium","Ensifer","Bauldia","Rhodobacterales","Rhodobacteraceae","Actibacterium","Celeribacter","Lutimaribacter","Oceanicola","Paracoccus","Sediminimonas","Sulfitobacter","Rhodospirillales","Acinetobacter","Stenotrophomonas") )+
  labs(x = "", y = "Relative abundance (%)") +  theme_bw()+
  scale_alpha_manual(values = c(Present = 0.8, Absent = 0.8),guide = 'none')+
  theme(legend.text = element_text(colour =c("black"), size = 20),strip.background = element_rect(
    color="white", fill="white"), strip.text.x = element_text(
      size = 20, color = "black", face = "bold"
    ),strip.text.y = element_blank(), axis.text.x = element_text( angle = 90, hjust=0.95,vjust=0.2,face = "bold",size=20),axis.title.y = element_text(face = "bold", size=20))

abundance_S <- read.table(file = 'species_abundance.tsv', sep = '\t', header = TRUE)
mycolors2 <- c(c("turquoise4","purple3","darkgoldenrod2","darkseagreen3","maroon3","darkolivegreen3","slateblue1","darkolivegreen","lightblue3","darkorange3","skyblue3","chartreuse3","aquamarine3","lightsalmon3","springgreen2","peachpuff3","cyan3","coral3","slateblue3","maroon2","rosybrown4","cadetblue3","darkgoldenrod2","turquoise3","springgreen4","darkolivegreen3","plum3","steelblue1"))
data.labs <- c("Reference", "Species-level classification")
names(data.labs) <- c("Abundance Reference", "Species-level")



Species <- 
ggplot(abundance_S,aes(fill=as.factor(Species_number), y=Abundance, x=Data.)) + 
  geom_bar(alpha=0.6,width = 0.8, stat="identity",position = position_stack(reverse = TRUE), size=0.5) + 
  facet_grid(wrap ~ Taxonomy,labeller = labeller(Taxonomy=data.labs), scales = "free", space = "free") + 
  scale_fill_manual(values = mycolors2, name = "" , labels = c("Clostridium acetobutylicum","Halomicrobium mukohataei","Saccharophagus degradans","Tsukamurella paurometabola","Novosphingobium pentaromativorans","Desulfovibrio vulgaris","Ruegeria pomeroyi","Pseudomonas putida","Corynebacterium glutamicum","Trichlorobacter (Geobacter) lovleyi","Pedobacter heparinus","Roseobacter denitrificans","Cellulomonas flavigena","Saccharopolyspora erythraea","Lactobacillus delbrueckii","Halogeometricum borinquense","Planctopirus (Planctomyces) limnophilus","Methanoculleus marisnigri","Methanospirillum hungatei","Levilactobacillus (Lactobacillus) brevis","Flavobacterium psychrophilum","Streptomyces avermitilis","Methanocorpusculum labreanum","Listeria welshimeri","Sphingobium indicum","Clostridium leptum","Methanobrevibacter smithii","Methanococcus aeolicus") )+
  labs(x = "", y = "Relative abundance (%)") +  theme_bw()+
  theme(legend.text = element_text(colour = 'black', size = 20, face = "italic"),strip.background = element_rect(
    color="white", fill="white"), strip.text.x = element_text(
      size = 20, color = "black", face = "bold"
    ),strip.text.y = element_blank(), axis.text.x = element_text( angle = 90, hjust=0.95,vjust=0.2,face = "bold",size=20),axis.title.y = element_text(face = "bold", size=20))

ggarrange(Genus, Species,ncol = 1, nrow = 2,labels = c("A", "B"),font.label = list(size = 20), align="v")
options(repr.plot.width = 14, repr.plot.height = 8)


ref_abundance <- read.table(file = 'ref_abundance.tsv', sep = '\t', header = FALSE)

library(randomcoloR)
n <- 28
mycolors <- distinctColorPalette(n)
ggplot(ref_abundance,aes(fill=as.factor(V2), y=V3, x=V4)) + 
  geom_bar( stat="identity",position = position_stack(reverse = TRUE), width = 0.2) + 
  #geom_text(aes(label=V6,x=1.12), position = position_stack(reverse = TRUE,vjust = 0.5),hjust = "outward",size=3.3) +
  scale_fill_manual(values = mycolors)+ theme_bw()


file1 <- read.table(file = 'seqleng.tsv', sep = '\t', header = FALSE)

n <- 3
mycolorsB <- distinctColorPalette(n)
mycolorsB <-c("lightcoral","darkcyan", "goldenrod3" )
#mycolorsB <- c("#00AFBB", "#E7B800", "#FC4E07")
ggplot(file1, aes(x=V1, y=V2, fill=V4)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin(alpha=0.5)+
  stat_summary(
    fun.data = "mean_sdl",  fun.args = list(mult = 1), 
    geom = "pointrange", color = "black",position = position_dodge(0.9)
  )+
  #geom_boxplot(colour="black",width=0.1,position = position_dodge(0.9)) +
  theme_bw()+
  scale_fill_manual(name = "Data Used :", labels = c("Raw MG reads", "SortMeRNA Filtered SSU reads", "BBmap filtered SSU reads"),values=mycolorsB)+
  labs(x = "Tools used to reconstruct SSU sequences", y = "Length of reconstructed sequences (bp)")+ 
  facet_grid (V5 ~ ., scales = "free",space  = "free")+
  #theme(plot.margin=unit(c(0.1,1,1,1),"cm"))+
  scale_y_continuous(limits = c(300, 1500), breaks = seq(300, 1500, by = 150))+
  theme(legend.position="top",strip.text.y = element_blank())
 


ggplot(file1, aes(x=V2, y=V2)) +
  geom_point(aes(color=V2,shape=V3),alpha=0.7, size=5)+ 
  scale_shape_manual(values = c(16,18,15)) +
  facet_grid (V1 ~ ., scales = "free",space  = "free")+
  labs(x = "Tool used for taxonomy", y = "Percentage of Tool Accurracy and efficiency to infer taxa ") +
  theme_bw()+
  scale_colour_gradientn(colours = myPalette(5), limits=c(300, 1500))



ggarrange(A,B,C, D,
          labels = c("(A)", "(C)", "(B)"),
          ncol = 2, nrow = 2)




ggarrange(A, B,ncol = 2, nrow = 1,labels = c("A", "B"), common.legend = TRUE)
G
ggarrange(A, C,ncol = 1, nrow = 2, labels = c("(A)", "(B)"),align="v") -> H
H

ggarrange(H,G,ncol = 2, nrow = 1)


#recall precision, f-measure

PRF_df <- read.csv(file="PR_EA_metarib_ref_16s.tsv", sep = '\t', header = FALSE)


# New facet label names for supp variable
data.labs <- c("Reconstructed 16S sequences", "Reference 16S sequences")
names(data.labs) <- c("A", "B")


mycolors <- c("red3", "royalblue3","goldenrod3", "aquamarine4")
A<-
  ggplot(PRF_df, aes(x = V3, y = V4, color = V1, group= V1)) + 
  geom_point(size=2) + geom_line(size=1) + 
  geom_errorbar(ymin= PRF_df$V4-0.05, ymax=PRF_df$V4+0.05,width = 0.02)+
  facet_grid(V6 ~ V8, labeller = labeller(V8=data.labs),)+
  theme_bw()+ ylim(0.2, 1.0)+
  xlab("Taxonomic level")+
  ylab("  Recall               Precision")+
  theme(strip.background = element_rect(color="white", fill="white"), 
        strip.text.x = element_text(size = 14, color = "White",face = "bold"),
        strip.text.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "None",
        plot.margin=unit(c(-0.05,0.1,0.1,0.1),"cm"),
        axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2, face = "bold", size=14),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(face = "bold", size=14),
        axis.title.x = element_text(face = "bold", size=14))+ 
  scale_colour_manual(values = mycolors)+
  scale_x_discrete(labels = c("Class","Order","Family","Genus","Species")) 

A
PRF_df_1 <- read.csv(file="Fmeasure_EA_metarib_ref_16s.tsv", sep = '\t', header = FALSE)
#write.csv(PRF_df_1,file="PRF_boxplot_data")
C<-
  ggplot(PRF_df_1, aes(fill=V1, y=V4, x=V1)) + 
  geom_boxplot()+
  scale_fill_manual(values = mycolors)+
  facet_grid(. ~ V8)+
   labs(x = "", y = "F-measure")+
  theme_bw()+ ylim(0.2,1.0)+
  theme(strip.text.x = element_blank(),
        legend.position = "none", 
        axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2, face = "bold",size=14),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(face = "bold", size=14),
        axis.title.x = element_text(face = "bold", size=14))

C
seq_df <- read.csv(file="tax_num_EA_metarib_ref_16s.tsv", sep = '\t', header = FALSE)

B<-ggplot(seq_df, aes(x = V3, y = V4, color = V1, group= V1)) + 
  geom_point(size=2) + geom_line(size=1) + 
  facet_grid(V6 ~ V8, labeller = labeller(V8=data.labs),)+
  theme_bw()+ ylim(0,22)+
  ylab("Number of Taxa")+
  theme(strip.background = element_rect(
    color="white", fill="white"), 
    strip.text.x = element_text(size = 14, color = "black", face = "bold"),
    axis.title.y = element_text(face = "bold", size=14),
    strip.text.y = element_blank(),
    axis.text.y = element_text(size=12),
    legend.title = element_blank(),
    legend.position = "top",
    legend.text = element_text(size=14),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    plot.margin=unit(c(0.1,0.1,-0.05,0.18),"cm"))+ 
  scale_colour_manual(values = mycolors)

B
ggarrange(B,A,ncol = 1,nrow = 2, heights=c(0.38,0.62))-> G
G
ggarrange(G, C,ncol = 1, labels = c("A", "B"),nrow = 2, heights=c(0.66,0.35)) 

fmeasure <- read.csv(file="New_Fmeasure.tsv", sep = '\t', header = FALSE)
#write.csv(PRF_df_1,file="PRF_boxplot_data")
ggplot(fmeasure, aes(fill=V1, y=V4, x=V1)) + 
  geom_boxplot()+
  scale_fill_manual(values = mycolors)+
  facet_grid(. ~ V7)+
  labs(x = "", y = "F-measure")+
  theme_bw()+
  theme(strip.text.x = element_blank(),legend.position = "none", axis.text.x = element_text(angle = 90, vjust = .5, face = "bold"))



S16_analysis <- read.csv(file="16S_mock_analysis.csv", sep="\t", header = FALSE)


ggplot(S16_analysis, aes(V4, V2, fill= V3)) + 
  geom_tile()+scale_fill_gradient(name = "Number of\nidentified 16S genes\nfrom the MOCK",low="darkseagreen2",high="salmon2")+
  geom_text(aes(label = V3))+theme_minimal()+
  scale_x_discrete(labels = c("Unfiltered metagenomic reads", "BBmap filtered reads", "SortMeRNA filtered reads")) + 
  theme(axis.title.x = element_text(face="bold",vjust = 0.1),axis.title.y = element_text(face="bold",vjust = 1.5),axis.text.x = element_text(vjust = 2, face = "bold"),axis.text.y = element_text(angle = 90,vjust=0.1, hjust = 0.5, face = "bold"))+
  labs(x="Input data",y="Tools used to reconstruct SSU sequences")


#sequence length Precision Recall

mycolors <- c("red3", "cyan4","darkorange3","hotpink3","royalblue3","goldenrod3","olivedrab4")

lenseq_df <- read.csv(file="lengthseq_PR.tsv", sep = '\t', header = FALSE)

data.labs <- c("Without sequence reconstruction", "With sequence reconstruction")
names(data.labs) <- c("A", "B")

A <-
  ggplot(lenseq_df, aes(x = V2, y = V3, color = V1, group= V1)) + 
  geom_line(size=1,alpha=0.9) + 
  facet_grid(V4 ~ V6,scales = "free_x",labeller = labeller(V6=data.labs),)+
  theme_bw()+
  xlab("Length of sequences (bp)")+
  ylab("   Recall                                      Precision")+
  theme(strip.background = element_rect(color="white", fill="white"),
    strip.text.x = element_text(size = 14, color = "black", face = "bold"),
    strip.text.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "top",
    legend.text = element_text(size=14),
    axis.title.x = element_text(face = "bold", size=14),
    axis.title.y = element_text(face = "bold", size=14),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14)) +
  scale_colour_manual(values = mycolors)
  
#fmeasure
library(tidyverse)
len_fmeasure <- read.csv(file="seqleng_fmeasure.tsv", sep = '\t', header = FALSE)
mycolors <- c("#1478a2", "#ff8878", "#ffc071")
data.labs <- c("Without sequence reconstruction", "With sequence reconstruction")
names(data.labs) <- c("A", "B")

  ggplot(len_fmeasure, aes(fill=V4, y=V2, x=fct_rev(V1))) + 
    geom_bar(stat = "identity", alpha=0.7,width=0.6,position = position_dodge(preserve = 'single'), colour="white")+
  #stat_summary(fun.data = "mean_sdl",  fun.args = list(mult = 1), geom = "pointrange", color = "black",position = position_dodge(0.9))+
  #geom_boxplot(colour="black",width=0.1,position = position_dodge(0.9)) +
  coord_flip() +
    scale_fill_manual(values = mycolors,labels = c("F-measure", "Recall", "Precision"))+
    facet_grid(V6 ~ .,labeller = labeller(V6=data.labs),scales = "free_y")+
    labs(x = "", y = "")+
    theme_bw()+
  theme(strip.background = element_rect(color="white", fill="white"),
        strip.text.y = element_text(size = 14, color = "black", face = "bold"),
        strip.text.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size=14),
        axis.title.x = element_text(face = "bold", size=14),
        axis.title.y = element_text(face = "bold", size=14),
        axis.text.x = element_text(size=14,angle = 270, hjust = 1, vjust = 0.4),
        axis.text.y = element_text(size=14))+ 
    guides(fill = guide_legend(reverse = TRUE))+
    scale_y_continuous(position = 'right')
#B
#ggarrange(A,B,labels = c("A", "B"),ncol = 1,nrow = 2, heights=c(0.6,0.4))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#variants per tool
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

variants_analysis <- read.csv(file="variants_per_tool.csv", sep=",", header = TRUE)
variants_count <- read.csv(file="variant_count.csv", sep=",", header = TRUE)

data.labs <- c("Reference", "Unfiltered reads", "BBmap filtered reads", "SortMeRNA filtered reads")
names(data.labs) <- c("A", "B", "C", "D")
mycolors <- c("gray88","lightblue3","aquamarine3", "gold","lightsalmon","tomato3","white", "white","white", "white", "white", "white", "white", "white", "white", "white", "white")
ggplot(variants_analysis, aes(x=Tools,y=as.factor(Strain_label))) + 
  geom_tile(aes(fill= as.factor(Number)),colour = "white",size=0.3,width=0.9, height=0.95)+
  geom_text(size=6,aes(label=as.factor(variants_count$Number)))+
  facet_grid(. ~ Data_label,scales = "free",space  = "free",labeller = labeller(Data_label=data.labs))+
  theme_minimal() +
  theme(strip.text.x = element_text(color = "black", face = "bold",size=12),
        axis.text.y = element_text( face = "bold.italic",size=14),
        axis.text.x = element_text(angle = 90, face = "bold",hjust=0.95,vjust=0.2,size=14),
        legend.text =  element_text(size = 14),
        legend.title = element_text(size = 14))+
  labs(x="",y="")+
  scale_fill_manual(values = mycolors,name = "Number of\n16S variants", labels = c("Not detected", "1", "2","3","4","5",'','','','','','','','','','',''))+
  scale_y_discrete(labels = rev(variants_analysis$Strain))

A + scale_x_discrete(sec.axis = dup_axis())
#----------------
# Human gut simulation
#-----------------

HG_PRF <- read.csv(file="HG_PRF.csv", sep = ',', header = FALSE)

data.labs <- c("Precision", "Recall", "F-measure")
names(data.labs) <- c("A", "B", "C")
#mycolors <- c("red3", "royalblue3","goldenrod3", "aquamarine4")
ggplot(HG_PRF, aes(x = V2, y = V3, color = V5,group=1)) + 
  geom_point(size=2, alpha=0.6) + geom_line(size=1,alpha=0.6) +
  facet_grid(. ~ V5,scales = "free",space  = "free",labeller = labeller(V5=data.labs))+
  scale_x_discrete(labels = c("Class","Order","Family","Genus","Species"))+
  theme_bw()+
  ylab("")+
  xlab("Taxonomic level")+
  ylim(0, 1)+
  theme(legend.position="none",strip.background = element_rect(
    color="white", fill="white"), strip.text.x = element_text(
      size = 12, color = "black", face = "bold"
    ),strip.text.y = element_blank(), axis.text.x = element_text(face = "bold"))

HG_details <- read.csv(file="HG_details.csv", sep = '\t', header = TRUE)

ggplot(HG_details, aes(x = Confidence, y = Length,color=Taxonomy)) + 
  geom_point(size=2, alpha=0.6)+ theme_bw()+
  xlab("Confidence")+
  ylab("Length of SSU sequences (bp)")

ggplot(HG_details, aes(x = Assigned_reads, y = Relative_abundance,color=Taxonomy)) + 
  geom_point(size=2)+ theme_bw()

ggplot(HG_details, aes(y = Relative_abundance, x = Length,color=Taxonomy)) + 
  geom_point(size=2)+ theme_bw()

  scale_x_discrete(labels = c("Class","Order","Family","Genus","Species"))+
  theme_bw()+
  xlab("Taxonomic level")






  geom_errorbar(ymin= PRF_df$V4-0.05, ymax=PRF_df$V4+0.05,width = 0.02)+
  facet_grid(V6 ~ V8, labeller = labeller(V8=data.labs),)+
  theme_bw()+
  xlab("Taxonomic level")+
  ylab("  Recall                     Precision")+
  theme(strip.background = element_rect(
    color="white", fill="white"), strip.text.x = element_text(
      size = 12, color = "White"
    ),strip.text.y = element_blank(),legend.title = element_blank(),legend.position = "None",plot.margin=unit(c(-0.05,0.1,0.1,0.1),"cm"))+ 
  scale_colour_manual(values = mycolors)+
  scale_x_discrete(labels = c("Class","Order","Family","Genus","Species")) + theme(axis.text.x = element_text(angle = 90, vjust = .5, face = "bold"))

  n <- 50
  mycolorsB <- distinctColorPalette(n)
  #mycolorsB <- c("cadetblue1", 'lightcoral','palegreen3')
  A<-ggplot(reconstruct_SSU, aes(fill=Data,y=Number, x=Tool)) + 
    geom_bar(position="dodge", stat="identity",colour="black",alpha=0.6)+ 
    geom_text(aes(label = Number), hjust = 0.4, vjust = -0.1, position = position_dodge(width = 1)) +  theme_bw()+
    labs(x = " ", y = "Number of Reconstructed SSU  sequences") +
    facet_grid (purpose ~ ., scales = "free",space  = "free") +
    scale_fill_manual(name = " ",labels = c("Unfiltered metagenomic reads","BBmap filtered reads", "SortMeRNA Filtered reads"),values=mycolorsB)+ 
    #theme(plot.margin=unit(c(1,1,-0.5,1),"cm"))+ 
    theme(legend.position="top", legend.text =  element_text(size = 14),strip.text.y = element_blank(),axis.title.x = element_text(face="bold", size = 14),axis.title.y = element_text(face="bold",size = 14),
          axis.text.x = element_text(angle = 90, face = "bold",hjust=0.95,vjust=0.2,size=14))
  
  
  B<-ggplot(file1, aes(x=V1, y=V2, fill=V4)) + # fill=name allow to automatically dedicate a color for each group
    geom_violin(alpha=0.6)+
    stat_summary(
      fun.data = "mean_sdl",  fun.args = list(mult = 1), 
      geom = "pointrange", color = "black",position = position_dodge(0.9)
    )+
    #geom_boxplot(colour="black",width=0.1,position = position_dodge(0.9)) +
    theme_bw()+
    scale_fill_manual(name = " ", labels = c("Raw MG reads", "SortMeRNA Filtered SSU reads", "BBmap filtered SSU reads"),values=mycolorsB)+
    labs(x = " ", y = "Length of reconstructed sequences (bp)")+ 
    facet_grid (V5 ~ ., scales = "free",space  = "free")+
    #theme(plot.margin=unit(c(0.1,1,1,1),"cm"))+
    scale_y_continuous(limits = c(300, 1500), breaks = seq(300, 1500, by = 100))+
    theme(legend.position="top", legend.text =  element_text(size = 14),strip.text.y = element_blank(),axis.title.x = element_text(face="bold", size = 14),axis.title.y = element_text(face="bold",size = 14),
          axis.text.x = element_text(angle = 90, face = "bold",hjust=0.95,vjust=0.2,size=14))
  
  
  ggarrange(A,B,
            labels = c("A", "B"),
            ncol = 2, nrow = 1,common.legend = TRUE)


  library(randomcoloR)
  n <- 75
  mycolors <- distinctColorPalette(n)
  
  genus_file<-read.csv(file="Genus_HM.csv",sep=',', header= TRUE)
  
  data.labs <- c("Genus-level classification")
  
  Genus <- 
    ggplot(genus_file,aes(fill=as.factor(Label), y=Abundance, x=Accession)) + 
    geom_bar(alpha=0.6,width = 0.9, stat="identity",position = position_stack(reverse = TRUE), size=0.5) +
    facet_grid( ~ wrap, labeller = labeller(Taxonomy=data.labs), scales = "free", space = "free") + 
    scale_fill_manual(values = mycolors, name = "", labels = c("Klebsiella",
                                                               "Parabacteroides",
                                                               "Ruminococcus",
                                                               "Clostridium",
                                                               "Haemophilus",
                                                               "Enterococcus",
                                                               "Escherichia",
                                                               "Corynebacterium",
                                                               "Bacteroides",
                                                               "Streptococcus",
                                                               "Neisseria",
                                                               "Bacillus",
                                                               "Lacrimispora",
                                                               "Porphyromonas",
                                                               "Eubacterium",
                                                               "Leuconostoc",
                                                               "Paenibacillus",
                                                               "Lachnoclostridium",
                                                               "Desulfovibrio",
                                                               "Citrobacter",
                                                               "Alistipes",
                                                               "Roseburia",
                                                               "Atopobium",
                                                               "Thermoanaerobacterium",
                                                               "Coprococcus",
                                                               "Enterobacter",
                                                               "Acholeplasma",
                                                               "Propionibacterium",
                                                               "Intestinibacter",
                                                               "Megasphaera",
                                                               "Barnesiella",
                                                               "Anaerococcus",
                                                               "Peptoniphilus",
                                                               "Desulfohalovibrio",
                                                               "Oceanobacillus",
                                                               "Erysipelatoclostridium",
                                                               "Veillonella",
                                                               "Bifidobacterium",
                                                               "Ruminococcustorques",
                                                               "Butyrivibrio",
                                                               "Ligilactobacillus",
                                                               "Turicibacter",
                                                               "Oscillibacter",
                                                               "Enterocloster",
                                                               "Sporobacter"))+
    labs(x = "", y = "Relative abundance (%)") +  theme_bw()+
    theme(legend.text = element_text(colour =c("black"), size = 14),
          strip.background = element_rect(color="white", fill="white"), 
          strip.text.x = element_text(size = 14, color = "black", face = "bold"),
          strip.text.y = element_blank(), 
          axis.text.x = element_text(size=13),
          axis.title.y = element_text(face = "bold", size=14))+
    scale_x_discrete(labels=c("Reference" = "Human Gut\nReference", "RiboTaxa" = "RiboTaxa\nclassification"))
  
  
  species_file<-read.csv(file="Species_HM.csv",sep=',', header= TRUE)
  
  
  species <-
    ggplot(species_file,aes(fill=as.factor(label), y=Abundance, x=Accession)) + 
    geom_bar(alpha=0.6,width = 0.9, stat="identity",position = position_stack(reverse = TRUE), size=0.5) +
    facet_grid( ~ wrap, labeller = labeller(Taxonomy=data.labs), scales = "free", space = "free") + 
    scale_fill_manual(values = mycolors, name = "",labels = c("Ruminococcus sp.", 
                                                              "Klebsiella pneumoniae", 
                                                              "Paenibacillus macerans",
                                                              "Klebsiella aerogenes", 
                                                              "Klebsiella variicola",
                                                              "Enterococcus pallens",
                                                              "Parabacteroides merdae", 
                                                              "Haemophilus aegyptius", 
                                                              "Citrobacter sp.", 
                                                              "Parabacteroides distasonis",
                                                              "Haemophilus parainfluenzae", 
                                                              "Haemophilus pittmaniae", 
                                                              "Klebsiella sp. KTE92",
                                                              "Roseburia intestinalis",
                                                              "Thermoanaerobacterium xylanolyticum", 
                                                              "Neisseria mucosa", 
                                                              "Neisseria meningitidis",
                                                              "Streptococcus oralis", 
                                                              "Eubacterium limosum",
                                                              "Leuconostoc lactis",
                                                              "Porphyromonas cangingivalis",
                                                              "Corynebacterium falsenii", 
                                                              "Leuconostoc mesenteroides",
                                                              "Ruminococcus bicirculans",
                                                              "Lachnoclostridium (Clostridium) symbiosum",
                                                              "Bacillus sp.", 
                                                              "Ruminococcus albus",
                                                              "Corynebacterium camporealensis",
                                                              "Enterococcus villorum", 
                                                              "Lachnoclostridium (Clostridium) scindens",
                                                              "Desulfovibrio sp.", 
                                                              "Porphyromonas gingivalis",
                                                              "Corynebacterium halotolerans",
                                                              "Parabacteroides sp. HGS0025",
                                                              "Atopobium sp.",
                                                              "Barnesiella intestinihominis",
                                                              "Eubacterium sp.",
                                                              "Corynebacterium accolens", 
                                                              "Enterobacter asburiae",
                                                              "Propionibacterium acidifaciens", 
                                                              "Acholeplasma palmae",
                                                              "Desulfohalovibrio reitneri", 
                                                              "Oceanobacillus massiliensis",
                                                              "Atopobium minutum",
                                                              "Erysipelatoclostridium ramosum",
                                                              "Alistipes communis",
                                                              "Bacillus sp. JCM 19059",     
                                                              "Parabacteroides sp.",
                                                              "Megasphaera sp.",
                                                              "Klebsiella sp. AS10", 
                                                              "Neisseria macacae", 
                                                              "Ruminococcus flavefaciens", 
                                                              "Veillonella sp.",
                                                              "Bifidobacterium asteroides",
                                                              "Bacteroides salyersiae",
                                                              "Ligilactobacillus agilis", 
                                                              "Megasphaera micronuciformis",
                                                              "Desulfovibrio brasiliensis",
                                                              "Bacteroides acidifaciens",
                                                              "Bacteroides intestinalis",
                                                              "Acholeplasma modicum",
                                                              "Bacteroides xylanisolvens",
                                                              "Enterocloster citroniae", 
                                                              "Sporobacter termitidis", 
                                                              "Peptoniphilus vaginalis",
                                                              "Parabacteroides sp. D13", 
                                                              "Propionibacterium cyclohexanicum",
                                                              "Parabacteroides faecis",
                                                              "Desulfovibrio sp. TomC",
                                                              "Porphyromonas sp.", 
                                                              "Alistipes shahii",
                                                              "Anaerococcus tetradius"))+
    labs(x = "", y = "Relative abundance (%)") +  theme_bw()+
    theme(legend.text = element_text(colour =c("black"), size = 14),
          strip.background = element_rect(color="white", fill="white"), 
          strip.text.x = element_text(size = 14, color = "black", face = "bold"),
          strip.text.y = element_blank(), 
          axis.text.x = element_text(size=13),
          axis.title.y = element_text(face = "bold", size=14))+
    scale_x_discrete(labels=c("Reference" = "Human Gut\nReference", "RiboTaxa" = "RiboTaxa\nclassification"))
  
  
  ggarrange(Genus, species,ncol = 1, nrow = 2,labels = c("A", "B"), align="v",heights=c(0.51,0.49)) 
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Beaudry dot plot graph
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  genus_file<-read.csv(file="Genus_HM.csv",sep=',', header= TRUE)
  data.labs <- c("Genus-level classification")
  lab=genus_file$Genus
  genus <-ggplot(genus_file, aes(x=Abundance, y=as.factor(Label), colour=Accession))+
    geom_point(aes(shape=Accession, size=Accession))+
    facet_grid( ~ wrap, labeller = labeller(Taxonomy=data.labs), scales = "free", space = "free") + 
    scale_shape_manual(values=c(108, 16))+
    scale_color_manual(values=c('black','#E69F00'))+
    scale_size_manual(values=c(3,2)) +
    theme_bw()+
    theme(legend.position="top",legend.title=element_blank(),strip.background = element_rect(color="white", fill="white"), 
          strip.text.x = element_text(size = 12, color = "black", face = "bold"),
          strip.text.y = element_blank(),axis.text.y = element_text(face="italic"))+
    #theme(legend.position = "none")+
    scale_y_discrete(label = lab)+
    ylab('')+xlab("Abundance (%)")
  
  species_file<-read.csv(file="Species_HM.csv",sep=',', header= TRUE)
  data.labs <- c("Species-level classification")
  lab=species_file$Species
  species <-ggplot(species_file, aes(x=Abundance, y=as.factor(label), colour=Accession))+
    geom_point(aes(shape=Accession, size=Accession))+
    facet_grid( ~ wrap, labeller = labeller(Taxonomy=data.labs), scales = "free", space = "free") + 
    scale_shape_manual(values=c(108, 16))+
    scale_color_manual(values=c('black','#E69F00'))+
    scale_size_manual(values=c(3,2)) +
    theme_bw()+
    theme(legend.position="top",legend.title=element_blank(),strip.background = element_rect(color="white", fill="white"), 
          strip.text.x = element_text(size = 12, color = "black", face = "bold"),
          strip.text.y = element_blank(),axis.text.y = element_text(face="italic"))+
    #theme(legend.position = "none")+
    scale_y_discrete(label = lab)+
    ylab('')+xlab("Abundance (%)")
  
  
  
  ggarrange(genus,species,
            labels = c("A", "B"),
            ncol = 2, nrow = 1,common.legend = TRUE, 
            widths = c(0.8,1) )

  