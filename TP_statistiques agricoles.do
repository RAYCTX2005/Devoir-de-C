

********************************************************************
************2.1  Preparation et nettoyage des donnees***************
********************************************************************


*1 spéccification du dossier de travail
cd "C:\Users\Lenovo\Desktop\docs\AS2\statistique agricoles\projet de stat agri"

**2 importation de la base '5_production_final'
use "5_production_final.dta", clear

**3 Détermination des  céréales cultivées
tab  culture
/*
       culture |      Freq.     Percent        Cum.
---------------+-----------------------------------
           Mil |      2,719       26.04       26.04
        Sorgho |        742        7.11       33.14
          Mais |      1,551       14.85       48.00
           Riz |        851        8.15       56.15
         Fonio |         38        0.36       56.51 */


* 4.1 vérifions s'il y a des superficies nulles*
summarize superficie
/*
    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
  superficie |     10,442    1.876753    1.965832     .00142         27 */

**aucune superficie nulle

** 4.2 vérifier s'il y a  les productions aberrantes

gen rendement=.
replace rendement=production_kg/superficie
codebook rendement
graph box rendement

/*a travers le graph box on remarque qu'il y a certaines cultures qui s'écartent de la moyenne mais elle ne se seront pas directement désignées comme valeurs abérrantes.

En effet, Compte tenu de la diversité des cultures, nous allons imputer les valeurs qui s'ecartent Significativement de la moyenne mais le seuil sera fixé pour chaque culture. 

Ainsi on considérera que les valeurs s'éloignent significativement de la moyenne lorsqu'elles sont en dehors de l'intervalle |Q1-1,5(Q3-Q1); Q1+1,5(Q3-Q1)| 
avec:
	  Q1: premier quartile
	  Q3: troisième quartile*/

**calcul des quartiles 
bysort culture : egen Q1= pctile(rendement), p(25)
bysort culture : egen Q3= pctile(rendement), p(75)
egen min_rend=min(rendement)
gen seuil1 = max(Q1 - 1.5 * (Q3 - Q1), min_rend)
gen seuil2= Q1+1.5*(Q3-Q1)
tab seuil2

**5 imputation des variables
*generation d'un nouveau rendement
gen rend_new=.
replace rend_new = rendement if (rendement < seuil2 & rendement > seuil1)  // Exclure les valeurs aberrantes
drop seuil1 seuil2 Q1 Q3 min_rend

** imputations des nouveaux rendements par les rendements moyens par commune.
bysort commune culture : egen sup_com = sum(sup) if rend_new != .
bysort commune culture: egen prod_com = sum(prod) if rend_new != .
gen rend_com=prod_com/sup_com
bysort commune culture: egen rend_com_new=mean(rend_com)
replace rend_new=rend_com_new if rend_new==0 | rend_new==.
drop rend_com rend_com_new sup_com prod_com

** Calcul des nouveaux rendements moyens par département
bysort departements culture : egen sup_dep = sum(superficie) if rend_new != 0
bysort departements culture: egen prod_dep = sum(production_kg) if rend_new != 0
gen rend_dep=prod_dep/sup_dep
bysort departements culture: egen rend_dep_new=mean(rend_dep)
replace rend_new=rend_dep_new if rend_new==0 | rend_new==.
drop rend_dep rend_dep_new prod_dep sup_dep

** Calcul des nouveaux rendements moyens par région
bysort regions culture : egen sup_reg = sum(superficie) if rend_new != .
bysort regions culture: egen prod_reg = sum(production_kg) if rend_new != .
gen rend_reg=prod_reg/sup_reg
drop sup_reg prod_reg
bysort regions culture: egen rend_reg_new=mean(rend_reg)
replace rend_new=rend_reg_new if rend_new==0  | rend_new==.
drop rend_reg rend_reg_new

** Calcul des nouveaux rendements par pays
bysort culture : egen sup_pays = sum(superficie) if rend_new != .
bysort culture: egen prod_pays = sum(production_kg) if rend_new != .
gen rend_pays=prod_pays/sup_pays
bysort culture: egen rend_pays_new=mean(rend_pays)
replace rend_new=rend_pays_new if rend_new==0  | rend_new==.
drop sup_pays prod_pays rend_pays rend_pays_new


**Nous allons maintenant calculer les nouvelles productions
replace production_kg=superficie*rend_new
replace rendement=rend_new 
drop rend_new

*verification des resultats de l'imputaion
codebook rendement

/*--------------------------------------------------------------------
rendements
----------------------------------------------------------------------
                  type:  numeric (float)

                 range:  [25,12000]                   units:  1.000e-06
         unique values:  1,669                    missing .:  0/10,442

                  mean:   473.043
              std. dev:   669.999

           percentiles:        10%       25%       50%       75%       90%
                               100       200       350   562.219       800
------------------------------------------------------------------*/

save "base_imputation.dta", replace

**6 aggrégation des superfices et production par ménage et culture
*** 6.1.1 aggregation des superficies par ménage
egen sup_men= sum(superficie), by(id_men)
***6.1.2 aggregation des productions par menage
egen prod_men=sum(production_kg),by(id_men)

***6.2.1 aggregation des superficies par culture 
egen sup_cult=sum(superficie), by (culture)

***6.2.2 aggregation des productions par culture
egen prod_cult= sum(production_kg), by (culture)

*****
egen menage_culture = group(id_men culture)
save "base_imputation.dta", replace
collapse (sum) superficie production_kg, by(menage_culture)
list
use "base_imputation.dta", clear



********************************************************************************
*2.2  Analyse de la productivité
********************************************************************************


**7 INDICE DE DIVERSITE DE CULTURE PAR MENAGE(INDICE DE SHANNON)

levelsof culture, local(liste_culture)
local nombre_modalites : word count `liste_culture'

display "Le nombre de culture est :" `nombre_modalites'
egen sup_tot_men= sum(superficie), by(id_men)
egen sup_cult_men= sum(superficie),by(culture id_men)
gen Pi = sup_cult_men/sup_tot_men // calcul des ponderations

* Calcul de l'indice de Shannon-Wiener
egen ind_shannon = sum(- Pi * ln(Pi)), by(id_men)
codebook ind_shannon
* Calcul de l'indice de Shannon-Wiener
egen ind_shannon = sum(- Pi * ln(Pi)), by(id_men)
codebook ind_shannon

/*
                  type:  numeric (float)

                 range:  [0,1.8734517]                units:  1.000e-09
         unique values:  828                      missing .:  0/10,442

                  mean:    .85635
              std. dev:   .382485

           percentiles:        10%       25%       50%       75%       90%
                           .264052   .673012     .9557    1.0889 1.32089
*/  
graph hbox ind_shannon

***les indices seront regroupés en classe pour une meilleure interpretation
gen ind_shannon_classe = floor(ind_shannon * 5 / 1.9) + 1

** Étiquetage des classes avec des intervalles
label define classe_labels 1 "0.0 - 0.39" 2 "0.4 - 0.79" 3 "0.8 - 1.19" 4 "1.2 - 1.59" 5 "1.6 - 1.9"

** Application les étiquettes aux classes
label values ind_shannon_classe classe_labels
tab ind_shannon_classe

/*ind_shannon |
    _classe |      Freq.     Percent        Cum.
------------+-----------------------------------
 0.0 - 0.39 |      1,106       10.59       10.59
 0.4 - 0.79 |      3,204       30.68       41.28
 0.8 - 1.19 |      4,353       41.69       82.96
 1.2 - 1.59 |      1,527       14.62       97.59
  1.6 - 1.9 |        252        2.41      100.00
------------+-----------------------------------
      Total |     10,442      100.00*/

hist ind_classe	  
ren ind_shannon_classe ind_classe
tab regions ind_classe
export excel tableau_indice using "C:\Users\Lenovo\Desktop\docs\AS2\statistique agricoles\projet de stat agri"

** Affichage un histogramme de la variable ind_classe par régions
histogram ind_classe, by(regions) ///
title("Histogramme de ind_shannon_classe par régions") ///
xtitle("Classes de ind_shannon") ///
ytitle("Fréquence") ///
legend(label(1 "Classe 1") label(2 "Classe 2") label(3 "Classe 3") label(4 "Classe 4") label(5 "Classe 5"))

keep if culture_label=="Arachide"|culture_label=="Mil"|culture_label=="Sorgho"|culture_label=="Mais"|culture_label=="Riz"|culture_label=="Fonio"|culture_label=="Niebe"
tab culture_label //pour travailler uniquement sur les superficies emblavées(cereales+arachide+niebe)
********************************************************************************
*******8.Calculer pour chaque menage les rendements des differentes cultures****

egen ren_cult_men= production_kg/superficie, by(id_men) // cree une variable qui alcule pour chaque menage les rendements des differentes cultures.
	
**********9.Part de l'arachide dans les superficies emblavées*******************
egen sup_tot= sum(superficie) //calcul de la superficie totale de l'ensemble des menages.
egen sup_men= sum(superficie), by(id_men) // calcul de la superficie des menages.

gen part_arachide=.
replace part_arachide=(sup_cult/sup_tot)*100 if culture_label=="Arachide" 
// calcul la partde l'arachide dans les différents menages.
codebook part_arachide departement
*le résultat est en pourcentage.
********************************************************************************

clear
*test ttest
gen part_arachide_men=. // cree une variable qui mesure la part de l'arachide par menage.
replace part_arachide_men=superficie/sup_men if culture_label=="Arachide"
gen groupe= 1 if sup_tot_men>5 // on regroupe les menages qui ont une superficie totale supérieure a 5ha
replace groupe =0 if sup_tot_men<=5 // on regroupe les menages qui ont une superficie totale inferieure a 5ha.
ttest part_arachide_men,by(groupe) // on effectue le test.
*Donc,cette part est significativement differente pour les deux types de menages
********************************************************************************
egen totale_dep= sum(superficie),by(departement) // calcul de la superficie totale par département
egen totale_cult= sum(superficie), by(culture departement) // calcul la superficie totale pour chaque combinaison de culture et département. 
gen part_culture=.
replace part_culture=(totale_cult/totale_dep)*100 // cree une variable qui calcule part de chaque culture dans les différents departements, les resultats sont en pourcentage.

graph pie part_culture ,over(culture_label) title("Diagramme circulaire") ///
            legend(size(small) position(3)) 
********************************************************************************

/* 10. Calculons pour chaque departement les rendements des differentes cultures.  Exporter le tableau vers un fichier Excel qu’on nommera ”tableau2.xlsx”*/

***********11.calcul le rendement de chaque culture par departement****************

egen rend_cult_dep=sum(rendement),by(departement culture) // calcul le rendement de chaque culture par departement, en sommant les rendements des menages pour chaque combinaison de culture et departement
table rend_cult_dep culture 
codebook rend_cult_dep culture
ssc install tabout
tabout rend_cult_dep culture using "C:\Users\JH\Documents\TP_AS\tableau2.xlsx",dpcomma
********************************************************************************
**************************Analyse des intrants**********************************
********************************************************************************
*12. comparaison de la diversité de culture dans les différents régions.

**indice de shannon par region
egen sup_tot_reg= sum(superficie), by(region) //calcul de la superficie totale par region
egen sup_cult_reg= sum(superficie),by(culture region) // calcul de la superficie totale utilisée pour chaque culture par region.
gen Pi_reg = sup_cult_reg/sup_tot_reg // ponderations par region.
* Calcul de l'indice de Shannon-Wiener
egen ind_shannon_region = sum(-Pi_reg * ln(Pi_reg)), by(region)
codebook ind_shannon_region
tab ind_shannon_region region if ind_shannon_region>=366.447 // pour observer les regions les plus diversifiées
tab ind_shannon_region region if ind_shannon_region<=179.221 //// pour observer les regions les moins diversifiées.
tabstat ind_shannon_region, by(region) 
** il ressort que Kaffrine est la region la plus diversifiée et la plus équilibrée, suivi de Kolda, selon l'indice de Shannon-Wiener
** Dakar est la region la plus homogene, elle est fortement dominée par une culture
** les zones les plus spécialisées sont: Dakar,Ziguinchor,Saint-Louis,Matam et Kédougou.

/*bysort id_men: egen somme=sum((superficie/sup_tot_men)^2)
gen idc= 1-somme
codebook idc
bysort region: egen somme_reg=sum((sup_cult_reg/sup_tot_reg)^2)
gen idc_reg= 1-somme_reg
codebook idc_reg
tab idc_reg region
tab idc region if idc>=0.63
*/

************calcul du rendement de l'arachide par region************************

keep if culture_label=="Arachide" // on travaille avec les menages concernées par la culture de l'arachide.
save "arachide.dta",replace
egen totale_reg= sum(superficie),by(region) // calcul la superficie totale par region.
egen prod_arachide= sum(production_kg),by(region) // calcul la production totale d'arachide par region.

gen ren_arachide_reg=.
replace ren_arachide_reg=(prod_arachide/totale_reg)  // calcul le rendement de l'arachide par region.
tabstat ren_arachide_reg,by(region)
codebook ren_arachide_reg region
tab ren_arachide_reg region if ren_arachide>=638.055
*la region de Ziguinchor enregistre le plus fort rendement en arachide
tab ren_arachide_reg region if ren_arachide >=567.676
** plus de 75% du rendement d'arachide sur l'ensemble des regions sont obtenus par les regions: Ziguinchor,Sedhiou et Kedougou

tab ren_arachide_reg region if ren_arachide <= 290.622
**Dakar enregistre le plus faible rendement en arachide, suivi de Thies puis de Louga
scatter ren_arachide_reg ind_shannon_region, by(region)
*tracer le nuage de points du rendement en fonction de l'indice de Shannon, par région

********************************************************************************
**13. comparation de l'utilisation de l'engrais entre l'arachide et mil. 
********************************************************************************
*On cree le code de fusion
	gen code=string(id_men)+"_"+string(culture)
	save "base_imputation.dta",replace
	*On sauvegarde la modification
	save "base_imputation",replace

	*On restaure a nouveau le dofile
	clear

	*On importe la base 7_engrais_final_long.dta
	use "7_engrais_final_long.dta",replace
	*On cree le meme code pour la base 6_semences_final_long.dta 
	gen code=string(id_men)+"_"+string(culture)

	*On fusionne 
	merge m:1 code using "base_imputation.dta"

	*On garde les bonnes fusion
	keep if _merge==3

keep if culture_label=="Arachide"| culture_label=="Mil" // on garde les enregistrements concernées par les cultures arachide et mil.
save "arachide_mil.dta",replace // enregistre dans une nouvelle base de données.
egen qte_eng_arachide=sum(qte_kg) if culture_label=="Arachide" // cree une variable qui stock la quantité de l'engrais utilisée pour la culture d'arachide
egen qte_eng_mil=sum(qte_kg) if culture_label=="Mil" // cree une variable qui stock la quantité de l'engrais utilisée pour la culture de mil
gen int_engrais=qte_eng_arachide/sup_cult if culture_label=="Arachide" // cree une variable qui mesure l'intensité d'utilisation de l'engrais.
replace int_engrais=qte_eng_mil/sup_cult if culture_label=="Mil" 
gen groupe= 0 if culture_label=="Arachide" // cree une variable groupe, qui regroupe les menages en deux groupes, 0 pour l'arachide et 1 pour le mil.
replace groupe= 1 if culture_label=="Mil"
table groupe int_engrais
br  
**correlation entre le rendement a l'hectar et l'utilisation de l'engrais
gen rend_ha=prod_cult/sup_cult if culture_label=="Arachide"  // calcul le rendement a l'hectar de l'arachide.
replace rend_ha=prod_cult/sup_cult if culture_label=="Mil"
// calcul le rendement a l'hectar
corr int_engrais rend_ha
br int_engrais rend_ha
// il existe une tres forte correlation positive entre l'intensite de l'engrais et le rendement a l'hectar

*Analyser la provenance des semences pour chaque speculation. Commenter egalement les resultats par zone.
*On sauvegarde la premiere fusion
	save "merge_engrais"
	
	use "merge_engrais.dta",replace
	
	*On supprime la variable merge
	drop _merge
	
	clear
	
	use "6_semences_final_long.dta",replace	
	
	gen code=string(id_men)+"_"+string(culture)
	save "6_semences_final_long.dta",replace	
	merge m:m code using "6_semences_final_long.dta" 
	
	keep if _merge==3
use "6_semences_final_long.dta",replace
********************************************************************************
************14.Analyse des types de semences par source de provenance***********
********************************************************************************
tab types_semences fournisseur1 if culture_label=="Arachide"
* les "semences certifiees" proviennent le plus des fournisseurs d'intrants et du marché local, suivis des coopératives semencieres, la proportion de menages agricoles qui utilisent les semences certifiées pour leur culture d'arachide est de 12%.
* les "semences non certifiées" sont le plus fournies par le marché local et les producteurs et a proportion de menages agricoles qui utilisent les semences non certifiées pour leur culture d'arachide est de 57,54%.
*la plupart des "semences subventionnées" sont fournies par le gouvernement, cependant,
*la proportion de menages agricoles qui utilisent les semences subventionnées pour leur culture d'arachide est de 29,3%.
* on observe une tres faible proportion de l'utilisationdes des "semences dons et transit" (0,9%), qui sont fournies grace aux projets et programme d'amelioration des rendements arachidiers. Les proportions restantes proviennent des menages eux-memes.

//On cree la variable zone//
	gen zone="zone ouest" if inlist(regions,1,7) //ouest//
	replace zone="zone sud" if inlist(regions,2,13,14,10)//sud//
	replace zone="zone nord" if regions==4 //nord//
	replace zone="zone centre" if inlist(regions,8,9,12,6,3) //centre//	
	replace zone="zone est" if inlist(regions,5,11) //est//
tab zone 
tab types_semences fournisseur1 if zone=="zone ouest" 

/* les "semences constituées a partir du stock" et les "semences non certifiées"(provenant le plus du marché local) sont 
les plus utilisées pour la culture d'arachide dans la zone sud (elles cumulent 83.69% des semences utilisées pour la culture d'arachide dans cette zone) */

tab types_semences fournisseur1 if zone=="zone nord" 
* la majorité des menages de la zone nord utilisent des "semences certifiées" et des "semences non certifiées" le plus fournies par les fournsseurs d'intrants et le marché local.

tab types_semences  fournisseur1 if zone=="zone centre" 
/*la zone centre concentre le plus ménages agricoles qui cultivent de l'arachide, la plupart
d'entre eux utilisent des "semences non certifiées", fournies le plus par le marché local 
et une proportition non négligeable des "semences subventionnées" par le gouvernement.
*/
tab types_semences fournisseur1 if zone=="zone est" 

/* la plupart des semences utilisées dans la zone Est sont des "semences non certifiées",fournies le plus par le marché local, suivi des "semences subventionnées"
fournies par le gouvernement.
*/

tab types_semences fournisseur1 if zone=="zone sud"
br zone

drop sup_commune
********************************************************************************
******15.Analyse des rendements d’arachide par source de provenance des semences
********************************************************************************
use "merge_engrais.dta",replace
keep if culture_label=="Arachide"
// l'on conviendra de considérer la source de provenance comme étant la zone telle que définie plus haut.

*on calcule les rendements par zone
egen sup_zone=sum(superficie),by(zone) // calcul de la superficie totale par zone
egen prod_cult_zone=sum(production_kg),by(zone) // calcul de la production d'arachide par zone.
gen ren_arachide_zone=prod_cult_zone/sup_zone if culture_label=="Arachide" // calcul du rendement d'arachide par zone.
 
tab ren_arachide_zone zone
* il ressort que la "zone ..." enregistre le plus fort rendement d'arachide a l'hectar


*L’utilisation de semences certifiees implique-t-elle l’atteinte de meilleurs rendements? 
tabstat ren_arachide_zone 

save "base_imputation.dta",replace
**************************************************************************************
***16.regression lineaire a partir de la diversite culturale, l’utilisation de l’engrais et l’utilisation des semences certifiees.
**************************************************************************************
use "base_imputation",replace
use "6_semences_final_long..dta",replace
	*On cree le meme code pour la base 6_semences_final_long.dta 
	gen code=string(id_men)+"_"+string(culture)

	*On fusionne 
	merge m:1 code using "base_imputation.dta"

	*On garde les bonnes fusion
	keep if _merge==3
	save "base_16.dta" // sauvegarder la fusion dans une nouvelle base.
	//On cree la variable zone//
	gen zone="zone ouest" if inlist(regions,1,7) //ouest//
	replace zone="zone sud" if inlist(regions,2,13,14,10) //sud//
	replace zone="zone nord" if regions==4 //nord//
	replace zone="zone centre" if inlist(regions,8,9,12,6,3) //centre//	
	replace zone="zone Est" if inlist(regions,5,11) //est//
egen sup_tot_zone= sum(superficie), by(zone) //calcul de la superficie totale par zone
egen sup_cult_zone= sum(superficie),by(culture zone) // calcul de la superficie totale utilisée pour chaque culture par zone.
gen Pi_zone= sup_cult_zone/sup_tot_zone // ponderations par zone.
* Calcul de l'indice de Shannon-Wiener par zone
egen ind_shannon_zone = sum(-Pi_zone * ln(Pi_zone)), by(zone) //indice de shannon par zone pour chaque culture.

// calcul de l'intensité de l'engrais par zone et par culture
egen qte_eng_zone=sum(qte_kg),by (culture zone)  // calcul la quantité d'engrais utilisée pour chaque combinaison de culture et zone.

//quantité d'engrais utilisées par culture et par zone.

gen int_engrais_zone=. // cree une variable pour mesurer et stocker en memoire l'intensité
* d'utilisation de l'engrais par zone.
replace int_engrais_zone=qte_eng_zone/sup_tot_zone
keep if types_semences==2 // on ne retient que les menages qui utilisent des semences certifiées.
reg ren_arachide_zone ind_shannon_zone int_engrais_zone // on effectue la regression linéaire.
/*
  Source |       SS           df       MS      Number of obs   =       221
-------------+----------------------------------   F(2, 218)       =    319.92
       Model |  175205.768         2   87602.884   Prob > F        =    0.0000
    Residual |  59694.9392       218  273.829996   R-squared       =    0.7459
-------------+----------------------------------   Adj R-squared   =    0.7435
       Total |  234900.707       220  1067.73049   Root MSE        =    16.548

----------------------------------------------------------------------------------
ren_arachide_z~e |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-----------------+----------------------------------------------------------------
ind_shannon_zone |  -.0557582   .0026628   -20.94   0.000    -.0610064     -.05051
int_engrais_zone |    76.7402   3.683909    20.83   0.000     69.47956    84.00084
           _cons |  -1201.472   86.42139   -13.90   0.000      -1371.8   -1031.143
----------------------------------------------------------------------------------



/Le modèle explique 74,35 % de la variance du rendement de l’arachide, ce qui est un bon ajustement. Les deux variables explicatives sont significatives et ont des effets opposés sur le rendement: la diversité des cultures le réduit, tandis que l’utilisation des engrais l’augmente.
Le coefficient de ind_shannon_zone est négatif, ce qui indique une relation inverse entre cette variable et le rendement de l’arachide. Ce coefficient signifie que pour chaque unité d’augmentation de l’indice de diversité de Shannon, le rendement de l’arachide diminue en moyenne de 0,0557582 kg/ha, toutes choses égales par ailleurs.
Le coefficient de int_engrais_zone est positif, ce qui indique une relation directe entre cette variable et le rendement de l’arachide. Ce coefficient signifie que pour chaque unité d’augmentation de l’intensité d’utilisation des engrais, le rendement de l’arachide augmente en moyenne de 76,7402 kg/ha, toutes choses égales par ailleurs.
Le terme constant (_cons) représente la valeur moyenne du rendement de l’arachide lorsque les deux variables explicatives sont égales à zéro. Il est négatif, ce qui signifie qu’il n’existe pas de rendement positif de l’arachide sans diversité ni engrais.
*/
save "base_16.dta",replace // sauvegarder les nouvelles variables.
scatter  // représentation du nuage des points entre l'indice de diversité de culture par zone et l'intensité de l'engrais par zone.
************************************************************************************
*17. Reprendre la r´egression en ajoutant d’autres variables explicatives que vous jugez pertinentes.
****************************************************************************************
//nouvelle fusion
use "base_16",replace
merge m:1 id_men using "1_menage_final.dta"
keep if _merge==3 // on retient les bonnes fusions

save "base_17.dta",replace // on sauvegarde la nouvelle base.
gen code=string(id_men)+"_"+string(culture)
save "base_17.dta",replace
use "6_semences_final_long..dta",replace
gen code=string(id_men)+"_"+string(culture)
save "6_semences_final_long..dta",replace
merge m:1 code using "base_17.dta"
keep if _merge==3
save "base_17.dta",replace
merge m:1 code using "merge_engrais.dta"
reg ren_arachide_zone ind_shannon_zone int_engrais_zone taille_menage niveau_instruction nbre_heure_travail // l'on reprend la regression précédente en y ajoutant les variables 'taille du ménage', 'le niveau d'instruction du chef de menage',le nombre d'heure de travail par semaine, 

/*

      Source |       SS           df       MS      Number of obs   =       221
-------------+----------------------------------   F(5, 215)       =    148.63
       Model |  182191.793         5  36438.3585   Prob > F        =    0.0000
    Residual |  52708.9145       215  245.157742   R-squared       =    0.7756
-------------+----------------------------------   Adj R-squared   =    0.7704
       Total |  234900.707       220  1067.73049   Root MSE        =    15.658

------------------------------------------------------------------------------------
 ren_arachide_zone |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------------+----------------------------------------------------------------
  ind_shannon_zone |  -.0578829   .0025584   -22.62   0.000    -.0629256   -.0528401
  int_engrais_zone |    76.5191   3.490428    21.92   0.000     69.63926    83.39894
     taille_menage |  -.0187944   .1644885    -0.11   0.909    -.3430109    .3054221
niveau_instruction |  -.3597701   .8586503    -0.42   0.676    -2.052221     1.33268
nbre_heure_travail |   .1440193   .0269809     5.34   0.000     .0908385    .1972002
             _cons |  -1197.001   81.85952   -14.62   0.000    -1358.351   -1035.651
------------------------------------------------------------------------------------
cette regression laisse faire comprendre que la taille du menage, le niveau d'instruction du chef de menage ou encore le nombre d'heure de travail par semaine n'influence pas significativement le rendement d'arachide.




