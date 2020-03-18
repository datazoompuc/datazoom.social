******************************************************
*             datazoom_pnadcontinua.ado              *
******************************************************
* version 1.0

program define datazoom_pnadcontinua

syntax, years(numlist) original(str) saving(str) 

/* Pastas para guardar arquivos da sessão */
cd `"`saving'"'

/* Criação de pastas para salvar os arquivos */
capture mkdir pnadcontinua
if _rc == 693 {
   tempname numpasta
   local numpasta = 0
   while _rc == 693 {
      capture mkdir "pnadcontinua_`++numpasta'"
   }
   cd "pnadcontinua_`numpasta'"
}
else {
   cd "pnadcontinua"
}
loc caminhoprin = c(pwd)


/* Dicionário */
findfile pnadcontinua.dct
loc dic = r(fn)

/* Extração dos arquivos */

* separar o ado em duas partes
foreach year in `years'{
	foreach trim in 01 02 03 04 {
			if (`trim' == 04 & `year' == 2019) {
			di as input "Extraindo arquivo PNADC_`trim'`year'  ..."
				cap infile using "`dic'", using("`original'/PNADC_`trim'`year'.txt") clear
				if _rc == 0 {
					qui capture egen hous_id = concat(UPA V1008 V1014), format(%14.0g)
					qui destring hous_id, replace
					qui capture egen ind_id = concat(UPA V1008 V1014 V2003), format(%16.0g)
					qui destring ind_id, replace
					save PNADC_`trim'`year', replace
					}
				else continue, break
			}	
			else if (`trim' == 03 & `year' == 2019) {
			di as input "Extraindo arquivo PNADC_`trim'`year'  ..."
				cap infile using "`dic'", using("`original'/PNADC_`trim'`year'.txt") clear
				if _rc == 0 {
					qui capture egen hous_id = concat(UPA V1008 V1014), format(%14.0g)
					qui destring hous_id, replace
					qui capture egen ind_id = concat(UPA V1008 V1014 V2003), format(%16.0g)
					qui destring ind_id, replace
					save PNADC_`trim'`year', replace
					}
				else continue, break
			}
			else if (`trim' == 02 & `year' == 2019) {
			di as input "Extraindo arquivo PNADC_`trim'`year'  ..."
				cap infile using "`dic'", using("`original'/PNADC_`trim'`year'.txt") clear
				if _rc == 0 {
					qui capture egen hous_id = concat(UPA V1008 V1014), format(%14.0g)
					qui destring hous_id, replace
					qui capture egen ind_id = concat(UPA V1008 V1014 V2003), format(%16.0g)
					qui destring ind_id, replace
					save PNADC_`trim'`year', replace
					}
				else continue, break
			}						
			else  {
			di as input "Extraindo arquivo PNADC_`trim'`year'  ..."
				cap infile using "`dic'", using("`original'/PNADC_`trim'`year'_20190729.txt") clear
				if _rc == 0 {
					qui capture egen hous_id = concat(UPA V1008 V1014), format(%14.0g)
					qui destring hous_id, replace
					qui capture egen ind_id = concat(UPA V1008 V1014 V2003), format(%16.0g)
					qui destring ind_id, replace
					save PNADC_`trim'`year', replace
					}
				else continue, break
			}
			}

	}

*if _rc != 0 continue, break

di _newline "Esta versão do pacote datazoom_pnadcontinua é compatível com a última versão dos microdados da PNAD Contínua divulgados em 18/02/2020"
di _newline "As bases de dados foram salvas em `c(pwd)'"
end
