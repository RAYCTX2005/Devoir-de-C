* 1. importer la base 5_production_final
use "5_production_final.dta", clear

* 2. Vérifier l’existence de doublons
duplicates report

* 3. Quelles sont les céréales cultivées?
tab culture

* 4. Y a-t-il des superficies nulles? des productions aberrantes?
codebook superficie
gen rend = production_kg/superficie
graph box rend, title("Boxplot de la variable") ylabel()

* 5. Imputer ces variables si nécessaire et documenter la méthode d’imputation
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

* 6. Agréger les superficies et production par ménage et par culture
* Par ménage
bys id_men : egen sup_men = sum(superficie)
bys id_men : egen prod_men = sum(production_kg)

* Par culture
bys culture : egen sup_cult = sum(superficie)
bys culture : egen prod_cult = sum(production_kg)

* 7. Enregistrer la base ainsi apurée en lui donnant un nom distinct
save "5_production_final_apuree.dta", replace

* 8. Calculer pour chaque ménage l’indice de diversité des cultures
gen poids = superficie / sup_men
gen valeur = -poids * (ln(poids) / ln(2))
bys id_men : egen indice = sum(valeur)

* 9. Calculer pour chaque ménage les rendements des différentes cultures
gen rend_moy_cult = production_kg / superficie // dans chaque ménage il n'y a pas plusieurs occurences d'une même culture
sort id_men

* 10. Calculer la part de l’arachide dans les superficies emblavées. Cette part est-elle différente selon que le ménage emblave plus de 5ha? Utiliser la commande ttest pour tester la significativité de cette différence.
egen sup_tot = sum(superficie)
bys culture: egen sup_culture = sum(superficie)
bys culture: gen part = sup_culture / sup_tot
table culture part if culture == 7 // 38,86%

bys id_men: egen sup_men_cults = sum(superficie)
gen part_cult_men = superficie / sup_men_cults
gen groupe = 1 if sup_men_cults <= 5
replace groupe = 0 if sup_men_cults > 5
ttest part_cult_men if culture == 7, by(groupe)
// La part de la superficie emblavées allouée à l'arachide est plus grande en moyenne si le ménage emblave plus de 5ha au total

* 11. Calculer pour chaque département, la part des différentes spéculations dans les superficies emblavées et produire le diagramme circulaire correspondant.
bys departements: egen sup_dep = sum(superficie)
bys departements culture: egen sup_dep_cult = sum(superficie)
bys departements culture: gen part_sup_spec = sup_dep_cult / sup_dep
sort departements culture

* Representation et exportation des diagrammes circulaire
graph pie part_sup_spec, over(culture) by(departements)

* 12. Calculer pour chaque département les rendements des différentes cultures. Exporter le tableau dans un fichier Excel
bys departements culture: egen pro_dep = sum(production_kg)     
gen ren_moy_dep = pro_dep / sup_dep_cult

preserve
keep if inlist(culture, 1, 7, 8) // Sélection des cultures : céréales, arachide, niébé
collapse ren_moy_dep, by(departements culture)
export excel "tableau_rendements.xlsx", sheet("Rendements par département") replace firstrow()
restore
* 13. Visualiser la diversité des cultures dans les différentes régions et la relation avec le rendement de l’arachide
bys regions: egen sup_reg = sum(superficie)
bys regions culture: egen sup_cult_reg = sum(superficie)
gen poids_reg_culte = sup_cult_reg / sup_reg
gen valeur_reg = -poids_reg_culte * (ln(poids_reg_culte) / ln(2))
bys regions : egen indice_reg = sum(valeur_reg)
graph bar indice_reg, over(regions)
graph export "diversite_region.png", replace

* 14. Comparaison de l’utilisation de l’engrais entre l’arachide et le mil et test de corrélation pour l’arachide
bys culture: egen sup_tota = sum(superficie)
gen code = string(id_men) + "_" + string(culture)
merge 1:m code using "7_engrais_final_long1"
keep if culture == 7 | culture == 2 // Arachide et mil seulement
drop _merge
bys culture : egen qtem = sum(qte_kg)
gen indeng = qtem / sup_tota
tab culture indeng
tab mode_acquisition if culture == 7 // Pourcentage subventionné pour l'arachide
tab mode_acquisition if culture == 2 // Pourcentage subventionné pour le mil
bys code: egen qte_kg_arach = sum(qte_kg) if culture == 7
gen int_uti = qte_kg_arach / superficie
regress int_uti rend_moy_cult if culture == 7

* 15. Visualiser la provenance des semences pour chaque spéculation et commenter les résultats par zones
use "6_semences_final_long", clear
gen provenance = "Coopératives semencières" if strmatch(fournisseur1, "Coopératives semencières") | strmatch(fournisseur2, "Coopératives semencières")
replace provenance = "Fournisseurs d'intrants" if strmatch(fournisseur1, "Fournisseurs d'intrants")
replace provenance = "Gouvernement" if strmatch(fournisseur1, "Gouvernement")
replace provenance = "Marché local" if strmatch(fournisseur1, "Marché local") | strmatch(fournisseur2, "Marché local")
replace provenance = "Multiplicateurs semenciers" if strmatch(fournisseur1, "Multiplicateurs semenciers") | strmatch(fournisseur2, "Multiplicateurs semenciers") | strmatch(fournisseur3, "Multiplicateurs semenciers")
replace provenance = "ONG" if strmatch(fournisseur1, "ONG")
replace provenance = "Producteurs" if strmatch(fournisseur1, "Producteurs") | strmatch(fournisseur2, "Producteurs")
replace provenance = "Projets et programmes" if strmatch(fournisseur1, "Projets et programmes")
replace provenance = "Autre" if strmatch(provenance, "")
// Analyse de la provenance des semences
tab provenance

* 16. Visualiser les rendements d’arachide par source de provenance des semences et analyser l'effet des semences certifiées par zone
merge m:m id_men using "5_production_final.dta"
bys provenance culture: egen sup_prov = sum(superficie)
bys provenance culture: egen prod_prov = sum(production_kg)
gen rend_prov = prod_prov / sup_pro
br culture provenance rend_prov
bys zone provenance: egen sup_zone = sum(superficie)
bys zone provenance: egen prod_zone = sum(production_kg)

gen rend_zone = prod_zone / sup_zone
br zone provenance rend_zone

* 17. Analyser les déterminants du rendement d’arachide par une régression linéaire
drop _merge
merge m:m id_men using "1_menage_final"
rename rend rend_arachide
gen int_uti_sem = qte_kg_S / superficie
gen use_certified = cond(types_semences == 2, int_uti_sem, .)
regress rend_arachide indice int_uti use_certified

* 18. Reprendre la régression en ajoutant d’autres variables explicatives pertinentes
regress rend_arachide indice int_uti_sem use_certified type_engrais
bys regions : egen indice_reg=sum(valeur_reg)
		graph bar indice_reg, over(regions)
		graph export "diversite_region.png", replace
		
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
