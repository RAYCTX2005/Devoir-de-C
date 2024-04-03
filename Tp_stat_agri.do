*********************************************
********* Title : TP2 - ISEP2 - Statistiques Grandes cultures
********* Fichiers : "1_menage_final","7_engrais_final_long","6_semences_final_long","5_production_final"
********* dernière modification : Avril 2024
********* Auteurs : RAHERINASOLO Ange Emilson Rayan et FOGWONG Djoufack Sarah Laure (rayanemil20@gmail.com/ syrassoul@gmail.com)
*********************************************
*___________________________________________________________________________________________________________________________________

**************************************************
**********2.1 Préparation et nettoyage des données
**************************************************

***     1. Créer un nouveau dofile, spécifier le dossier de travail
		cd "C:/Users/bmd tech/OneDrive/Documents/TP_FINAL"

***     2. importer la base 5_production_final
		use "5_production_final.dta", clear
		set dp comma

***    3. Quelles sont les céréales cultivées?
		tab culture
		*** Les cultures céréalières sont : Mil, Sorgho, Maïs, Riz, Fonio.
	
***   4. Y a-t-il des superficies nulles? des productions aberrantes?
		codebook superficie
		***La plus petite superficie est 0,00142ha donc il n'y a aucune superficie nulle
		gen rend = production_kg/superficie
		graph box rend, title("Boxplot de la variable") ylabel()
		***On constate qu'il y a 420 productions aberrantes
		
***   5. Imputer ces variables si nécessaire les valeurs aberrantes
		bys culture: egen Q1= pctile(rend), p(25)
		bys culture: egen Q3= pctile(rend), p(75)
		bys culture: egen seuilmax=min(max(rend),Q3+1.5*(Q3-Q1))
		bys culture: egen seuilmin=max(min(rend),Q1-1.5*(Q3-Q1))
		replace rend = cond(rend > seuilmax| rend< seuilmin | rend==0, ., rend)
		replace production_kg = cond(rend == ., ., production_kg)
		
		bys communes culture : egen prod_tot = sum(production_kg)
		bys communes culture : egen sup_tot = sum(superficie)
		gen rend_moy = prod_tot/sup_tot
		drop prod_tot sup_tot
		replace rend = cond(rend == ., rend_moy, rend)
		replace production_kg = cond(production_kg == ., superficie*rend, production_kg)	
		
		bys departements culture : egen prod_tot = sum(production_kg)
		bys departements culture : egen sup_tot = sum(superficie)
		gen rend_dpt = prod_tot/sup_tot
		drop prod_tot sup_tot
		replace rend = cond(rend == 0, rend_dpt, rend)
		replace production_kg = cond(production_kg == 0, superficie*rend, production_kg)	

		bys regions culture : egen prod_tot = sum(production_kg)
		bys regions culture : egen sup_tot = sum(superficie)
		gen rend_rg = prod_tot/sup_tot
		drop prod_tot sup_tot
		replace rend = cond(rend == 0, rend_rg, rend)
		replace production_kg = cond(production_kg == 0, superficie*rend, production_kg)	

		bys culture : egen prod_tot = sum(production_kg)
		bys culture : egen sup_tot = sum(superficie)
		gen rend_pop = prod_tot/sup_tot
		drop prod_tot sup_tot
		replace rend = cond(rend == 0, rend_pop, rend)
		replace production_kg = cond(production_kg == 0, superficie*rend, production_kg)

***   6. Agréger les superficies et production par ménage et par culture
		***Par ménage
		bys id_men : egen sup_men = sum(superficie)
		bys id_men : egen prod_men = sum(production_kg)
		
		***Par culture
		bys culture : egen sup_cult = sum(superficie)
		bys culture : egen prod_cult = sum(production_kg)


*******************************************
**********2.2 Analyse de la productivité
*******************************************
	***7. Calculer pour chaque ménage l’indice de diversité des cultures
		gen poids = superficie/sup_men
		gen valeur= -poids*(ln(poids)/ln(2))
		bys id_men : egen indice=sum(valeur)

	***8. Calculer pour chaque ménage les rendement des diffrentes cultures
		gen rend_moy_cult=production_kg/superficie // dans cahque ménage il n'ya pas plusieurs occurence d'une meme culture
		sort id_men
		
	**** 9. Calculer la part de l’arachide dans les superficies emblavees. Cette part est-elle differente selon que le menage emblave plus de 5ha? utiliser ttest pour effectuer le test.

		egen sup_tot=sum(superficie)
		bys culture: egen sup_culture=sum(superficie)
		bys culture: gen part=sup_culture/sup_tot
		table culture part if culture==7  //38,86%
		
		bys id_men: egen sup_men_cults=sum(superficie)
		gen part_cult_men=superficie/sup_men_cults
		gen groupe=1 if sup_men_cults<=5
		replace groupe=0 if sup_men_cults>5
		ttest part_cult_men if culture==7, by(groupe) 
		//la part de la superficie emblavées allouée à l'arachide est plus grande en moyenne si le menage emblave plus de 5ha  au total
	
	**** 10. Calculer pour chaque departement la part des differentes speculations dans les superficies emblavée et tracer l diagramme circulaire correspondant

		bys departements: egen sup_dep = sum(superficie)
		bys departements culture: egen sup_dep_cult=sum(superficie)
		bys departements culture: gen part_sup_spec=sup_dep_cult/sup_dep
		sort departements culture
		
		* segmentation des départements en 6 groupe
		gen departement=1 if inrange(departements,11,29)
		replace departement=2 if inrange(departements,31,59)
		replace departement=3 if inrange(departements,61,79)
		replace departement=4 if inrange(departements,81,109)
		replace departement=5 if inrange(departements,111,129)
		replace departement=6 if inrange(departements,131,149)
		
		* Representation et exportation des diagrammes circulaire
		graph pie part_sup_spec if departement==1, over(culture) by(departements)
		graph export "groupe1.png", replace
		graph pie part_sup_spec if departement==2, over(culture) by(departements)
		graph export "groupe2.png", replace
		graph pie part_sup_spec if departement==3, over(culture) by(departements)
		graph export "groupe3.png", replace
		graph pie part_sup_spec if departement==4, over(culture) by(departements)
		graph export "groupe4.png", replace
		graph pie part_sup_spec if departement==5, over(culture) by(departements)
		graph export "groupe5.png", replace
		graph pie part_sup_spec if departement==6, over(culture) by(departements)
		graph export "groupe6.png", replace

		
		*** 11.Calculer pour chaque departement les rendements des differentes cultures. Exporter le resultats sur excel
		bys departements culture: egen pro_dep=sum(production_kg)		
		gen ren_moy_dep=pro_dep/sup_dep_cult
		
		preserve
		keep if culture<=7 ///  les cultures céréalières le niébé et l'arachide
		collapse ren_moy_dep, by(departements culture)
		export excel "taleau2.xlsx", sheet("tableau2.") replace firstrow()
		
		** representation du diagramme en barre pour chaque culture
		
		/*graph bar ren_moy_dep if departement==1, over(culture) by(departements)
		graph export "ren_groupe1.png", replace
		graph bar ren_moy_dep if departement==2, over(culture) by(departements)
		graph export "ren_groupe2.png", replace
		graph bar ren_moy_dep if departement==3, over(culture) by(departements)
		graph export "ren_groupe3.png", replace
		graph bar ren_moy_dep if departement==4, over(culture) by(departements)
		graph export "ren_groupe4.png", replace
		graph bar ren_moy_dep if departement==5, over(culture) by(departements)
		graph export "ren_groupe5.png", replace
		graph bar ren_moy_dep if departement==6, over(culture) by(departements)
		graph export "ren_groupe6.png", replace*/
		restore
		
	***** 12. Comparer la diversite des cultures dans les diferentes regions. Quelles sont les zones les plus specialisées et la relation avec le rendement en arachide
		bys regions: egen sup_reg=sum(superficie)
		bys regions culture: egen sup_cult_reg=sum(superficie)
		gen poids_reg_culte=sup_cult_reg/sup_reg
		gen valeur_reg=-poids_reg_culte*(ln(poids_reg_culte)/ln(2))
		bys regions : egen indice_reg=sum(valeur_reg)
		graph bar indice_reg, over(regions)
		graph export "diversite_region.png", replace
		* les region specialisées sont dakar saint-louis matam kedougou ziguinchor
		
		bys regions culture: egen pro_cult_reg=sum(production_kg)
		gen ren_reg=pro_cult_reg/sup_cult_reg
		preserve
		keep if culture==7
		twoway(scatter ren_reg indice_reg, sort mlabel(regions))
		graph export "relation_rend.png", replace
		restore

		
		
	* 13. Comparer l’utilisation de l’engrais entre l’arachide et le mil. Pourquoi est-ce que cet indicateur est plus ´elev´e pour l’arachide? Tester 		statistiquement, pour l’arachide, la correlation entre l’intensité d’utilisation de l’engrais et le rendement à l’hectare. Conclure

// Fusion de la base 5_production_final et 7_engrais_final_long en utilisant l'id_men et la culture comme code 
bys culture: egen sup_tota=sum(superficie)
gen code = string(id_men)+"_" + string(culture)
save "5_production_final1.dta", replace
clear 

use "7_engrais_final_long"
gen code = string(id_men) + "_" + string(culture)
save "7_engrais_final_long1", replace
clear 
use "5_production_final1", clear
merge 1:m code using "7_engrais_final_long1"

/* merge == 1 7944 qui est le nombre de culture n'ayant pas utilisé d'engrais
merge == 2  0 
merge == 3 good */

preserve
keep if culture == 7 | culture == 1
drop _merge
bys culture : egen qtem = sum(qte_kg)
gen indeng = qtem/sup_tota
tab culture indeng
restore
////* L'utilisation de l'engrais est plus élévée pour l'arachide(23,9 kg/ha) que pour le mil(22,4kg/ha)). Cela pourrait s'expliquer par le fait que l'engrais est plus subventionné pour l'arachide que pour le mil.*/
tab mode_acquisition if culture ==7 //54,43% subventionné)
tab mode_acquisition if culture == 1 // 49,16% subventionné

// Tester statistiquement, pour l’arachide, la corrélation entre l’intensité d’utilisation de l’engrais et le rendement à l’hectare. Conclure 
bys code : egen  qte_kg_arach = sum(qte_kg) if culture == 7
bys code : gen  int_uti=qte_kg_arach/superficie
reg int_uti rend_moy_cult if culture == 7 
drop _merge
save "5_production_final1.dta", replace



***14. Analyser la provenance des semences pour chaque spéculation. Commenter également les résultats par zone.
	use "6_semences_final_long", clear
	gen provenance = "Coopératives semencières" if strmatch(fournisseur1, "Coopératives semencières")| strmatch(fournisseur2, "Coopératives semencières")
	replace provenance = "Fournisseurs d'intrants" if strmatch(fournisseur1,"Fournisseurs d'intrants")
	replace provenance = "Gourvenement" if strmatch(fournisseur1, "Gourvenement")
	replace provenance = "Marché local" if strmatch(fournisseur1, "Marché local")| strmatch(fournisseur2, "Marché local")
	replace provenance = "Multiplicateurs semenciers" if strmatch(fournisseur1, "Multiplicateurs semenciers")| strmatch(fournisseur2, "Marché local")|strmatch(fournisseur3, "Multiplicateurs semenciers")
	replace provenance = "ONG" if strmatch(fournisseur1, "ONG")
	replace provenance = "Producteurs" if strmatch(fournisseur1, "Producteurs")| strmatch(fournisseur2, "Producteurs")
	replace provenance = "Projets et programme" if strmatch(fournisseur1, "Projets et programme")
	replace provenance = "Autre" if strmatch(provenance, "")
	
	***On constate que la majorité (64,82%) des semences viennent d'origines autres ou divers (par un proche, une aide...). La principale source secondaire est le Marché local avec 18,91% des semences qui en sont issues.
	
	tab culture provenance, column
	tab culture provenance, row
	***L'arachide et le mil représentent à eux deux 57,10% des semences et respectivement 24,46% et 32,06% des semences de provenance autre pour 44,97% et 15,81% des semences venant du marché local. En outre 49,60% des semences en arachide sont d'origines autres pour 26,60% en provenance du marché local. Pour ce qui est des semences de mil, elles sont principalement (82,71%)de provenance autre.
	gen zone = "Centre" if regions == 3| regions == 8| regions == 12| regions == 9| regions == 6
	replace zone = "Nord" if regions == 4
	replace zone = "Ouest" if regions == 1| regions == 7
	replace zone = "Sud" if regions == 2| regions == 10| regions == 14| regions == 13
	replace zone = "Est" if regions == 5| regions == 11
	move zone regions
	tab zone provenance, column
	/*DIOURBEL, LOUGA, KAFFRINE, FATICK, KAOLACK
	48,87% des semences, soit prés de la moitié, sont concentrées au centre (Diourbel, Louga, Kaffrine, Fatick, Kaolack)
	KEDOUGOU, ZIGUINCHOR, SEDHIOU, KOLDA
	28,08% des semences proviennent du Sud*/
foreach var of varlist qte_kg-fournisseurs_other {
    gen `var'_S = `var'
}	
drop qte_kg-fournisseurs_other
order zone-types_semences qte_kg_S-fournisseurs_other_S provenance	



 ***15. Analyser les rendements d’arachide par source de provenance des semences. L’utilisation de semences certifi´ees implique-t-elle l’atteinte de meilleurs rendements? Commenter ´egalement les r´esultats par zone
       
	    sort id_men
        merge m:m id_men using "5_production_final.dta"
	  ***rendement d'arachide selon la provenance
	    bys provenance culture : egen sup_prov = sum(superficie)
	    bys provenance culture : egen prod_prov = sum(production_kg)
		bys provenance culture : gen rend_prov= prod_prov/sup_pro
		br culture provenance rend_prov 
	****rendement d'arachide par region 
	     bys zone culture : egen sup_zone = sum(superficie)
		 bys zone culture : egen prod_zone = sum(production_kg)
		 bys zone culture : gen rend_zone = prod_zone/sup_zone
	****rendement d'arachide par type de semences
	     bys types_semences culture : egen sup_sem = sum(superficie)
		 bys types_semences culture : egen prod_sem = sum(production_kg)
		 bys types_semences culture : gen rend_sem = prod_sem/sup_sem
	****visualisation des differnts rendements 
		 br region types_semences  culture provenance rend_prov rend_reg rend_sem
	**** 
	     preserve 
		 keep if culture == 7 
		 tab provenance rend_prov  
	     collapse (mean) rend_prov , by(provenance)
		 export excel  using "provenancetable.xlsx", replace firstrow() 
		 restore
	*****
		 preserve
		 keep if culture == 7
		 tab  zone rend_zone 
		 collapse (mean) rend_zone, by (zone)
		  export excel  using "regions.xlsx", replace firstrow() 
		 restore
    *****
	     preserve
		 keep if culture == 7
		 tab types_semences rend_sem
	     collapse (mean) rend_sem , by(types_semences)
		 export excel  using "semence.xlsx", replace firstrow() 
		 restore


    ****
	    *le rendement d'arachide est plus élévé chez les semences qui proviennent des ONG 176,30kg/ha , du gouvernement(305,26kg/ha)  et plus faible pour ceux des        marchés locale 169,11kg/ha et autres sources 
	****
	    *l'utilisation des semences certifiées ne donne pas un meilleurs rendements (353,73kg/ha) contrairement aux semences constitués (447,67kg/ha), subventionné(402,49kg/ha) , 
	***
	   *Par zone on note que les zones  du sud (585,02kg/ha) et l'Est (446,34kg/ha) qui ont un meilleurs rendelents contrairement aux Nord (197,74kg/ha) et à l'Ouest (262,54kg/ha)
	    ***Repartition des rendement d'acharide
	   preserve 
	   keep if culture == 7
	   graph bar rend_prov , over (provenance)
	   graph export "provenance.png"
       graph bar rend_zone , over (zone)
	   graph export "zone.png"
	   graph bar rend_sem , over (types_semences)
	   graph export "semence.png"
	   restore



 
***16. Analyser les déterminants du rendement d’arachide par une régression linéaire où les variables explicatives sont la diversit´e culturale, l’utilisation de l’engrais et l’utilisation des semences certifiées.
drop _merge
merge m:m id_men using "1_menage_final"
move zone regions
keep if culture == 7
drop _merge
rename rend rend_arachide
gen int_uti_sem = qte_kg_S/superficie
gen use_certified = cond(types_semences == 2, int_uti_sem, .)
regress rend_arachide  indice int_uti use_certified

***17. Reprendre la r´egression en ajoutant d’autres variables explicatives que vous jugez pertinentes. Que trouvez-vous?
regress rend_arachide indice int_uti_sem use_certified type_engrais***2.2 Analyse de la productivité

			bys regions : egen indice_reg=sum(valeur_reg)
		graph bar indice_reg, over(regions)
		graph export "diversite_region.png", replace
		* les region specialisées sont dakar saint-louis matam kedougou ziguinchor
		
		bys regions culture: egen pro_cult_reg=sum(production_kg)
		gen ren_reg=pro_cult_reg/sup_cult_reg
		preserve
		keep if culture==7
		twoway(scatter ren_reg indice_reg, sort mlabel(regions))
		graph export "relation_rend.png", replace
		restore
	   graph bar rend_sem , over (types_semences)
	   graph export "semence.png"
	   restore
