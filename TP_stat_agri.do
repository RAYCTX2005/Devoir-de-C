***************** Projet statistiques du travail AS2 OUEDRAOGO Faical / FA LO Ibrahima / BA Baba ******************
****************1ere partie Preparation et nettoyage des données*********************

// 1-2) Specification du repertoire de travail et importation de la base de donnée
	cd "C:\Users\hp\Desktop\AS\AS 2\Stats agricoles\test_2"
	use "5_production_final.dta"
	br     

	
// 3) Cereales cultivées : Le mais / le mil / le sorgho / le fonio / le riz
	tab culture

	
// 4) Superficies nulles ou production aberrantes
	codebook superficie // Aucune superficie nulle

	//Calculons le rendement pour voir les productions aberrantes car la production depend de la superficie
	gen rendement = production_kg / superficie

	//Le boxplot des rendements montre qu'il y a des productions aberrantes
		tab rendement
		graph box rendement


// 5) Imputation des valeurs aberrantes
	// Les rendements superieurs au seuil Q3+1,5IQR seront imputés par les moyennes de la spéculation en question dans la commune concernée. Les rendements nuls ne sont pas considérés comme aberrants
	sort commune culture 
	egen com_cult = concat(commune culture_label) // Pour le calcul des seuils par commune par speculation


	// Les Q3 par commune par culture
	bys com_cult : egen Q3 = pctile(rendement), p(75)

	// Les Q1 par commune par culture
	bys com_cult : egen Q1 = pctile(rendement), p(25)

	// seuil a partir duquel on a les valeurs aberrantes
	gen seuil = Q3+1.5*(Q3-Q1)
	tab seuil

	// On va realiser une imputation par commune et par culture_label
	egen somme_prod = sum(production) if rendement <= seuil, by (com_cult)
	egen somme_sup = sum(superficie) if rendement <= seuil, by (com_cult)
	gen moyenne = somme_prod / somme_sup if rendement <= seuil
	bysort com_cult (moyenne) : replace moyenne = moyenne[_n-1] if missing(moyenne) // Pour remplir les valeurs manquantes
	replace rendement = moyenne if rendement > seuil // Imputation des rendements

	// Imputation des productions aberrantes
	gen production_imput = rendement * superficie
	codebook production_imput

	
// 6) Agregation des sup et des prod par menage
	sort id_men
	egen sup_men = sum(superficie), by (id_men)
	egen pro_men = sum(production_imput), by (id_men)





********************2e partie : Analyse de la productivité*******************

// 7) Calcul de l'indice de diversité des cultures en utilisant la formule de l'indice de Simpson avec les superficies
	gen sup_i = superficie / sup_men // Pour generer les proportions des superficies par menage
	gen c_sup = sup_i*sup_i
	egen s_ = sum(c_sup), by (id_men) 
	gen indice = 1 - s_
	tab indice

	
// 8) Calcul des rendements par culture pour chaque menage
	keep if culture_label == "Arachide" | culture_label == "Niébé" | culture_label == "Mil" | culture_label == "Sorgho" | culture_label == "Mais" | culture_label == "Riz" | culture_label == "Fonio"
	gen rendement_menage = production_imput / superficie


// 9) Part de l'arachide dans les superficies emblavées 39,75%
	egen sup_ara = sum(superficie) if culture_label == "Arachide"
	egen sup_tot = sum(superficie)
	gen ara_part = sup_ara / sup_tot //39,75%
	// Faison ttest
	// Part de l'arachide par menage pour le ttest
	gen ara_part_men = superficie / sup_men if culture_label == "Arachide" 
	gen group = "moins de 5" if sup_men < 5
	replace group = "plus de 5" if sup_men >= 5
	ttest ara_part_men , by (group) 
	** D'apres le ttest il y a une difference significative de la part de l'arachide dans la production selon que le menage utilise moins de 5 ha ou plus de 5 ha


// 10) Calculer pour chaque departement, la part des differentes speculations dans les superficies emblavees et produire le diagramme circulaire correspondant
	egen som_dep = sum(superficie), by (departements)
	egen som_dep_cult = sum(superficie), by (departements culture_label)
	sort departements
	gen part_dep = som_dep_cult / som_dep // Part des speculations
	// Les graphiques par departement
	gen departement=1 if inrange(departements,11,29)
			replace departement=2 if inrange(departements,31,59)
			replace departement=3 if inrange(departements,61,79)
			replace departement=4 if inrange(departements,81,109)
			replace departement=5 if inrange(departements,111,129)
			replace departement=6 if inrange(departements,131,149)
			graph pie part_dep if departement==1, over(culture_label) by(departements)
			graph export "groupe1.png", replace
			graph pie part_dep if departement==2, over(culture_label) by(departements)
			graph export "groupe2.png", replace
			graph pie part_dep if departement==3, over(culture_label) by(departements)
			graph export "groupe3.png", replace
			graph pie part_dep if departement==4, over(culture_label) by(departements)
			graph export "groupe4.png", replace
			graph pie part_dep if departement==5, over(culture_label) by(departements)
			graph export "groupe5.png", replace
			graph pie part_dep if departement==6, over(culture_label) by(departements)
			graph export "groupe6.png",replace

			
// 11) Rendement par departement par culture
	egen som_dep_cult_pro = sum(production_imput), by (departements culture_label)
	gen rend_cult_dep = som_dep_cult_pro / som_dep_cult
	sort departements culture
	// Recuperation des rendements des cultures par departement
	preserve
	collapse rend_cult_dep, by(departements culture)
	export excel using "C:\Users\hp\Desktop\AS\AS 2\Stats agricoles\test_2\Tableau.xlsx", firstrow()
	restore




*************3e partie : Analyse des intrants **************

// 12) Calcul de l'indice de diversité des cultures en utilisant la formule de l'indice de Simpson avec les productions et les superficies
	egen sup_reg_cult = sum(superficie), by (regions culture)
	egen sup_reg = sum(superficie), by (regions)
	gen sup_ig = sup_reg_cult / sup_reg // Pour generer les proportions des superficies par culture par region
	gen ln_sup_ig = (ln(sup_ig)/ln(2)) * sup_ig
	egen indice_g = sum(ln_sup_ig), by (regions)
	replace indice_g = - indice_g 
	tab indice_g
	// Diagramme en barre pour comparer les indices
	graph bar indice_g, over(regions)
	*graph export "12)indice.png",replace
	// Les regions les plus specialisées sont Dakar Saint-Louis Ziguinchor Matam Kedougou
	egen prod_reg_ara = sum(production_imput) if culture_label == "Arachide", by (regions)
	gen arachide_rendement = prod_reg_ara / sup_reg if culture_label == "Arachide"
	twoway (scatter arachide_rendement indice_g, sort mlabel(regions)) // Pour voir la relation entre la diversification et le rendement de l'arachide
	graph export "rend_ara_indice 12.png",replace


// Il faut merger les bases engrais, production et semence
// Nous allons utiliser comme clé la variable code 
	gen code = string(id_men)+"_"+string(culture) // Ne pas oublier d'enregitrer la base avant de clear
	gen group_eng = "eng_ara" if culture_label == "Arachide"
	replace group_eng = "eng_mil" if culture_label == "Mil"
	egen sup_ara_mil = sum(superficie), by (group_eng)
	save "5_production_final.dta", replace
	clear
	*Importons la base de l'engrais et creons le meme code
	use "7_engrais_final_long.dta"
	gen code = string(id_men)+"_"+string(culture)
	** Changeons le nom des quantité pour l'engrais
	rename fournisseur1 fournisseur1_engrais
	rename fournisseur2 fournisseur2_engrais
	rename fournisseur3 fournisseur3_engrais
	rename qte_kg qte_kg_engrais
	rename prix_kg prix_kg_engrais // Renommer les variables QTE ET PRIX
	gen group_eng = "eng_ara" if culture_label == "Arachide"
	replace group_eng = "eng_mil" if culture_label == "Mil"
	egen eng_ara_mil = sum(qte_kg_engrais), by (group_eng)
	
	// 1ere fusion 
	merge m:m code using "5_production_final.dta"
	// Calcul de l'intensité d'utilisation de l'engrais
	gen int_eng = eng_ara_mil / sup_ara_mil
	sort culture
	// Garder la bonne fusion 
	keep if _merge == 3
	// Sauvegarder la 1ere fusion
	save "production_engrais"
	// Faire la 2e fusion
	use "production_engrais.dta"
	// Supprimer la variable merge et enregitrer la base mergée
	drop _merge
	save "production_engrais.dta", replace
	clear

	// Importons la base semence et mergeons la avec la nouvelle base créée
	use "6_semences_final_long.dta"		
	gen code=string(id_men)+"_"+string(culture)
	merge m:m code using "production_engrais.dta" 
	keep if _merge==3
	save "production_engrais_semence" // La base production_engrais_semence contient les 3 bases mergées
	use "production_engrais_semence.dta"
	drop _merge
	save "production_engrais_semence.dta", replace
	use "production_engrais_semence.dta"
	
// 13) : Comparer lengrais entre l'arachide et le mil
	// Comparaison des moyennes
	tab group_eng int_eng  // On constate que la moyenne de la quantité d'engrais pour l'arachide(23.95) est legerement supérieure a celle du mil (22.46)

	// Quantité totale d'engrais par speculation
	bys culture_label : egen som_eng_cult = sum(qte_kg_engrais)

	// Cet indicateur est plus elevé pour l'arachhide peut etre car il y a plus de semences d'arachide que de mil

	gen int_eng_ara = qte_kg_engrais/superficie // L'intensité de l'engrais
	pwcorr int_eng_ara rendement if culture_label == "Arachide" , sig // La p-value etant inferieure a 0,05 alors on peut rejeter l'hypothese nulle (H0:r = 0) et donc il y'a une correlation positive entre l'intensité de l'engrais et le rendement a l'hectare pour l'arachide meme si cette correlation est relativement faible (27,6%)


// 14) Analyser la provenance des semences pour chaque spéculation. Commenter également lesrésultats par zone.
	// Regroupons tous les fournisseurs en une seule variable
	gen fournisseur = fournisseur1 
	replace fournisseur = fournisseur2 if missing(fournisseur)
	replace fournisseur = fournisseur3 if missing(fournisseur)
	replace fournisseur = fournisseurs_other if missing(fournisseur)
	replace fournisseur = "Autre" if missing(fournisseur)

	tabout fournisseur culture using "C:\Users\hp\Desktop\AS\AS 2\Stats agricoles\test_2\semence_culture.xls",append // Voir excel semence_culture

	// Par zone (regions)
	tabout fournisseur regions using "C:\Users\hp\Desktop\AS\AS 2\Stats agricoles\test_2\semence_culture.xls",append // Voir excel semence_culture


// 15) Analyser les rendements d’arachide par source de provenance des semences ... 
	egen prod_four_ara = sum(production_imput) if culture_label == "Arachide", by (fournisseur)
	egen sup_four_ara = sum(superficie) if culture_label == "Arachide", by (fournisseur)
	gen rend_ara_four = prod_four_ara / sup_four_ara

	tabout fournisseur rend_ara_four using "C:\Users\hp\Desktop\AS\AS 2\Stats agricoles\test_2\semence_culture.xls",append // Voir excel semence_culture

	// certification semences
	egen prod_cert_ara = sum(production_imput) if culture_label == "Arachide", by (types_semences)
	egen sup_cert_ara = sum (superficie) if culture_label == "Arachide", by (types_semences)
	gen rend_ara_cert = prod_cert_ara / sup_cert_ara

	tabout types_semences rend_ara_cert using "C:\Users\hp\Desktop\AS\AS 2\Stats agricoles\test_2\semence_culture.xls",append // Voir excel semence_culture

	// Par zone 
	tabout types_semences regions using "C:\Users\hp\Desktop\AS\AS 2\Stats agricoles\test_2\semence_culture.xls",append // Voir excel semence_culture // L'utilisation de semences certifiées n'implique pas un meilleur rendement


// Question 16
	gen certifiées = "certifiée" if types_semences == 2
	gen ind_sem = qte_kg / superficie if certifiées == "certifiée"
	gen rend_arachide = rendement if culture_label == "Arachide"
	regress rend_arachide indice int_eng_ara ind_sem // Le modele n'est pas globalement significatif
	save "production_engrais_semence.dta", replace 
	
	
// 17) On pourrait faire une regression avec le type d'engrais 
// Ajoutons la base menage pour voir s'il y a d'autres variables explicatives
	clear
	use "1_menage_final.dta"
	merge m:m id_men using "production_engrais_semence"
	keep if _merge == 3
	drop _merge
	save "production_engrais_semence_menage"
	regress rend_arachide indice int_eng_ara ind_sem age // Modele globalement significatif avec une grosse part expliquée par l'age
	regress rend_arachide int_eng_ara age // Avec les 2 valeurs le modele est d'autant plus significatif avec une diminution du rendement avec l'age et une augmentation du rendement avec une augmentation de l'intensité


********* Fin du dofile***********



















































