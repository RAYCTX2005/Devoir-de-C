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
regress rend_arachide indice int_uti_sem use_certified type_engrais // Ajouter les autres variables pertinentes selon vos données
