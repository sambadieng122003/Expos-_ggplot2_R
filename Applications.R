#'DIENG Samba
#'COULIBALY Khadidiatou
#'Applications du package ggplot2()

# Importing libraries-----------------------------------------------------------
library("tidyverse")
library("haven")
library("knitr")
library("kableExtra")
library("questionr")

# Little warm up with code chunks...--------------------------------------------------------------


# Importing the database
Base <- 
  read_dta("C:/Users/DELL/Documents/ISEP2_2024/ISEP2coursR2024/Exposé R/Application/Base_de_données.dta")


# Observe the following code chunks.
# 
# 1.  Try to guess their outputs, not looking at the images (7)
# 
# 2.  Match each code chunk with its image. How many of them did you get right ?
  
#  **Code chunk N1**

#___________________________________________________________________
#'Avez-vous rencontré un problème de santé 
#'durant les 12 derniers mois qui a entrainé une hospitalisation ?

Base$s03q19_rec <- factor(Base$s03q19, labels= names(attr(Base$s03q19, "labels")), 
                          levels=unname(attr(Base$s03q19, "labels")))

ggplot(Base)+
  geom_bar(aes(x=s03q19, fill=s03q19_rec))+
  theme_void()+
  labs(title = "Problème de santé durant les 12 derniers mois qui a entrainé une hospitalisation ?",
       x="Réponse donnée")

#___________________________________________________________________



#**Code chunk N2**
#_________________________________________________________________________________

#'Bénéficiez-vous d'une couverture maladie ?

Base$s03q32_rec <- factor(Base$s03q32, labels= names(attr(Base$s03q32, "labels")), 
                          levels=unname(attr(Base$s03q32, "labels")))

Base$mil_resid <- factor(Base$s00q04, labels= names(attr(Base$s00q04, "labels")), 
                         levels=unname(attr(Base$s00q04, "labels")))

ggplot(Base)+
  geom_bar(aes(x=mil_resid, fill=s03q32_rec), position="dodge" )+
  coord_flip()
#_______________________________________________________________________________



#**Code chunk N3**
#________________________________________________________________________________
#'Bénéficiez-vous d'une couverture maladie ?

Base$s03q32_rec <- factor(Base$s03q32, labels= names(attr(Base$s03q32, "labels")), 
                          levels=unname(attr(Base$s03q32, "labels")))

Base$mil_resid <- factor(Base$s00q04, labels= names(attr(Base$s00q04, "labels")), 
                         levels=unname(attr(Base$s00q04, "labels")))

ggplot(Base)+
  geom_bar(aes(x=mil_resid, fill=s03q32_rec), position="fill" )+
  coord_flip()
#__________________________________________________________________________________



# **Code chunk N4**
#________________________________________________________________________

#'Avez-vous rencontré un problème de santé 
#'durant les 12 derniers mois qui a entrainé une hospitalisation ?

Base$s03q19_rec <- factor(Base$s03q19, labels= names(attr(Base$s03q19, "labels")), 
                          levels=unname(attr(Base$s03q19, "labels")))

ggplot(Base)+
  geom_bar(aes(x=s03q19,y="", fill=s03q19_rec), stat="identity", width = 1)+
  theme_void()+
  coord_polar("x", start=0)
#____________________________________________________________________________



# **Code chunk N5**
#____________________________________________________________________________
#'Milieu de résidence 

Base$mil_resid <- factor(Base$s00q04, labels= names(attr(Base$s00q04, "labels")), 
                         levels=unname(attr(Base$s00q04, "labels")))

ggplot(Base)+
  geom_bar(aes(x=mil_resid,y="", fill=mil_resid), width=1, stat="identity")+
  theme_void()+
  coord_polar("x", start=0)
#___________________________________________________________________



# **Code chunk N6**
#______________________________________________________________________________

#'s03q40: Moyens de prévention du paludisme autres que moutiquaire,
#'suivant la tendance à utiliser une moustiquaire. 

#'s03q38: Dormez-vous habituellement sous une moustiquaire ?

Base$s03q40_rec <- factor(Base$s03q40, labels= names(attr(Base$s03q40, "labels")), 
                          levels=unname(attr(Base$s03q40, "labels")))

Base$s03q38_rec <- factor(Base$s03q38, labels= names(attr(Base$s03q38, "labels")), 
                          levels=unname(attr(Base$s03q38, "labels")))

Base %>% filter(!is.na(s03q38_rec)) %>%
  ggplot()+
  geom_bar(aes(x=s03q38_rec,y=after_stat(count*100/sum(count)), fill=s03q40_rec))+
  facet_grid(cols= vars(s03q40_rec))
#____________________________________________________________________________



# **Code chunk N7**
#___________________________________________________________________

#'s03q40: Moyens de prévention du paludisme autres que moutiquaire,
#'suivant la tendance à utiliser une moustiquaire. 

#'s03q38: Dormez-vous habituellement sous une moustiquaire ?

Base$s03q40_rec <- factor(Base$s03q40, labels= names(attr(Base$s03q40, "labels")), 
                          levels=unname(attr(Base$s03q40, "labels")))

Base$s03q38_rec <- factor(Base$s03q38, labels= names(attr(Base$s03q38, "labels")), 
                          levels=unname(attr(Base$s03q38, "labels")))

Base %>% filter(!is.na(s03q38_rec)) %>%
  ggplot()+
  geom_bar(aes(x=s03q38_rec,y=after_stat(count*100/sum(count)), fill=s03q40_rec))+
  facet_wrap(vars(s03q40_rec), nrow=2)+
  theme(legend.position = "top")
#_____________________________________________________________________________


# INTO THE DATABASE--------------------------------------------------------------------------
#Il serait intéressant de faire la répartition de l'échantillon suivant les différentes régions.

# Let's make a bar plot

Base$Reg_rec <- factor(Base$s00q01, labels= names(attr(Base$s00q01, "labels")), 
                       levels=unname(attr(Base$s00q01, "labels")))

ggplot(Base) +
  geom_bar(aes(y=Reg_rec, x=after_stat(count*100/sum(count))), color="white", fill="royalblue")+
  theme_minimal()+
  labs(title = "Répartition de l'échantillon suivant les différentes régions",
       y="Régions", x="Part dans l'échantillon", caption="Source : ANSD, EHCVM 2018/2019, calculs propres ")


## Let's dig deep into the database...

# Le tableau précédent nous sera très utile. Il suggère quelques analyses descriptives
# que nous auront le plaisir de réaliser ensemble.
# Ready ?
# Let's!


#Répartition des individus suivant la rencontre ou non d’un problème de santé pendant les trente (30) derniers jours.**----------------------------------------------------------------------

Base$s03q01_rec <- factor(Base$s03q01, labels= names(attr(Base$s03q01, "labels")), 
                                levels=unname(attr(Base$s03q01, "labels")))

 ggplot(Base) +
  geom_bar(aes(x = "", y = s03q01, fill = s03q01_rec), width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void()+
   labs(title = "Répartition des individus suivant la rencontre ou non d’un problème de santé pendant les trente (30) derniers jours", caption="Source : ANSD, EHCVM 2018/2019, calculs propres ")



#Répartition des principaux problèmes de santé rencontrés pendant les trente (30) derniers jours suivant le milieu de résidence.**--------------------------------------------------


Base$mil_resid <- factor(Base$s00q04, labels= names(attr(Base$s00q04, "labels")), 
                                levels=unname(attr(Base$s00q04, "labels")))

Base$pblems <- factor(Base$s03q02, labels= names(attr(Base$s03q02, "labels")), 
                                levels=unname(attr(Base$s03q02, "labels")))

Base %>% filter(s03q01==1 & !is.na(pblems)) %>%
ggplot() +
  geom_bar(aes(y=pblems, x=after_stat(count*100/sum(count)),fill=mil_resid),  position="dodge",color="white")+
  theme_minimal()+
  labs(title = "Problèmes de santé rencontrés suivant le milieu de résidence",
       y="Part dans l'échantillon", x="Problèmes", caption="Source : ANSD, EHCVM 2018/2019, calculs propres ")



#Répartition des principaux problèmes de santé des trente (30) derniers jours par région**---------------------------------------------------


# On représente pour les différentes régions les différents cas de maladies.

Base %>% 
  filter(s03q01 == 1 & !is.na(pblems)) %>%
  ggplot() +
  geom_density(aes(y =pblems,
                    fill = Reg_rec),position = "dodge") +
  theme_minimal() +
  labs(title = "Principaux problèmes de santé rencontrés par région",
       y = "Part dans l'échantillon", 
       x = "Problèmes", 
       caption = "Source : ANSD, EHCVM 2018/2019, calculs propres ")+
  coord_flip()


Base %>% 
  filter(s03q01 == 1 & !is.na(pblems)) %>%
  ggplot() +
  geom_density(aes(x =s03q02 ,
                    fill = Reg_rec), alpha=0.2) +
  theme(legend.position="top")+
  labs(title = "Principaux problèmes de santé rencontrés par région",
       y = "Part dans l'échantillon", 
       x = "Problèmes", 
       caption = "Source : ANSD, EHCVM 2018/2019, calculs propres ")
   
 


# Ce graphique n'est pas trop lisible...--------------------------------------------------------------
# Et si l'on prenait un autre angle de vue. Pour chaque région, représentons les différents cas de maladies.

# On représente pour les différentes régions les différents cas de maladies.

Base %>% 
  filter(s03q01 == 1 & !is.na(pblems)) %>%
  ggplot() +
  geom_bar(aes(x =pblems, fill=pblems),position = "dodge") +
  theme(legend.position = "top") +
  facet_wrap(vars(Reg_rec)) +
  labs(title = "Principaux problèmes de santé rencontrés par région",
       caption = "Source : ANSD, EHCVM 2018/2019, calculs propres ")
 


#Répartition des différentes raisons de non consultation suite à un problème de santé rencontré dans les (30) dernies jours en fonction du milieu de résidence.**-----------------------------------------

Base$raisons <- factor(Base$s03q06, labels= names(attr(Base$s03q06, "labels")), 
                                levels=unname(attr(Base$s03q06, "labels")))

Base %>% filter(s03q01==1 & s03q05==2) %>% # Si la personne a rencontré un problème de santé mais ne s'est pas fait consulter.
  ggplot() +
  geom_bar(aes(y=raisons, x=after_stat(count*100/sum(count)),fill=mil_resid),  position="fill",color="white")+
  theme_minimal()+
  labs(title = "Raisons de non consultation suivant le milieu de résidence",
      x="Raisons", caption="Source : ANSD, EHCVM 2018/2019, calculs propres ")

# On voit donc , pour raison évoquée, la répartition en fonction du milieu de résidence.

# Et si l'on "superposait" des graphiques, l'un en-dessous de l'autre.------------------------------------------------------------------

Base %>% filter(s03q01==1 & s03q05==2) %>% # Si la personne a rencontré un problème de santé mais ne s'est pas fait consulter.
  ggplot() +
  geom_bar(aes(y=mil_resid, x=after_stat(count*100/sum(count)), fill=raisons),  position="dodge",color="white")+
  theme_minimal()+
  facet_grid(rows = vars(raisons))+
  labs(title = "Raisons de non consultation suivant le milieu de résidence",
      x="Raisons", caption="Source : ANSD, EHCVM 2018/2019, calculs propres ")
  



#Voyons les choses sous un autre angle. Pour chaque milieu de résidence, voyons la part de chaqu raison évoquée.--------------------------------------------------

#'On oubliera pas de filter pour avoir les individus qui 
#'ont rencontré un problème de santé
#'mais ne se sont pas fait consulter.


Base %>% filter(s03q01==1 & s03q05==2) %>%
  ggplot() +
  geom_bar(aes(y=mil_resid, x=after_stat(count*100/sum(count)), fill=raisons ),  position="fill",color="white")+
  theme_minimal()+
  labs(title = "Raisons de non consultation par milieu de résidence",
       y="Part dans l'échantillon", x="Raisons", caption="Source : ANSD, EHCVM 2018/2019, calculs propres ")

# Le graphique ce-dessus permet de voir des fréquences relatives.

# Retournons le graphique pour une mailleure visualisation.---------------------------------------------------------------------

Base %>% filter(s03q01==1 & s03q05==2) %>%
  ggplot() +
  geom_bar(aes(y=mil_resid, x=after_stat(count*100/sum(count)), fill=raisons ),  position="dodge",color="white")+
  theme_minimal()+
  labs(title = "Part des raisons de non consultation par milieu de résidence",
       y="Milieu de résidence", x="Raisons", caption="Source : ANSD, EHCVM 2018/2019, calculs propres ")+
  coord_flip()



#Répartition des principaux lieux de consultation en fonction du milieu de résidence.**----------------------------------------------------------

Base$lieu_cons <- factor(Base$s03q08, labels= names(attr(Base$s03q08, "labels")), 
                                levels=unname(attr(Base$s03q08, "labels")))


Base %>% filter(s03q01==1 & s03q05==1) %>%
  ggplot() +
  geom_bar(aes( x= lieu_cons, y=after_stat(count*100/sum(count)), fill= mil_resid), color="white")+
  theme_minimal()+
  labs(title = "Lieux de consultation suivant le milieu de résidence. ",
       x="Lieux de consultation", caption="Source : ANSD, EHCVM 2018/2019, calculs propres ")+
  coord_flip()


## **BONUS: Let's make some maps**-------------------------------------------------------------------
  
 # Représentons le Sénégal, le Camaroun et Madagascar...


#install.packages("maps")
library(maps)

# Getting map data for our  Senegal

SN <- map_data("world", region="Senegal")

# Plot the map
ggplot() + 
  geom_polygon(data = SN, aes(x = long, y = lat, group = group, fill=region),
               color = "black", fill = "green") 



# Représentons l'Afrique-----------------------------------------------------------------------

library(rnaturalearth)
library(rnaturalearthdata)

# Télécharger les données des frontières des pays du monde
world <- ne_countries(scale = "medium", returnclass = "sf")

# Plot de la carte de l'Afrique avec une couleur pour chaque pays
ggplot(data = world[world$continent == "Africa", ]) +
  geom_sf(aes(fill = name), color = "black") +
  scale_color_brewer(palette =2 ) +
  labs(title = "Carte de l'Afrique avec une couleur pour chaque pays") +
  theme_minimal()





