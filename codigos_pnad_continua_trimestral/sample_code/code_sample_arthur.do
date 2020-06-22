 
 * 2012 - 2018

clear
 
forvalues yr = 2012(1)2018 {
	
import delimited "C:\Users\Francisco\Dropbox\DataZoom\BasesIBGE\datazoom_rar\PNAD_CONTINUA\pnadcontinua_trimestral_20190729\PNADC_01`yr'_20190729.txt", encoding(Big5) clear
sample 2
export delimited using "C:\Users\Francisco\Desktop\pasta\pnad_painel\arthur_basico\PNADC_01`yr'_20190729.txt", replace	

import delimited "C:\Users\Francisco\Dropbox\DataZoom\BasesIBGE\datazoom_rar\PNAD_CONTINUA\pnadcontinua_trimestral_20190729\PNADC_02`yr'_20190729.txt", encoding(Big5) clear
sample 2
export delimited using "C:\Users\Francisco\Desktop\pasta\pnad_painel\arthur_basico\PNADC_02`yr'_20190729.txt", replace	

import delimited "C:\Users\Francisco\Dropbox\DataZoom\BasesIBGE\datazoom_rar\PNAD_CONTINUA\pnadcontinua_trimestral_20190729\PNADC_03`yr'_20190729.txt", encoding(Big5) clear
sample 2
export delimited using "C:\Users\Francisco\Desktop\pasta\pnad_painel\arthur_basico\PNADC_03`yr'_20190729.txt", replace	


import delimited "C:\Users\Francisco\Dropbox\DataZoom\BasesIBGE\datazoom_rar\PNAD_CONTINUA\pnadcontinua_trimestral_20190729\PNADC_04`yr'_20190729.txt", encoding(Big5) clear
sample 2
export delimited using "C:\Users\Francisco\Desktop\pasta\pnad_painel\arthur_basico\PNADC_04`yr'_20190729.txt", replace	
}

* 2019

import delimited "C:\Users\Francisco\Dropbox\DataZoom\BasesIBGE\datazoom_rar\PNAD_CONTINUA\pnadcontinua_trimestral_20190729\PNADC_012019_20190729.txt", encoding(Big5) clear
sample 2
export delimited using "C:\Users\Francisco\Desktop\pasta\pnad_painel\arthur_basico\PNADC_012019_20190729.txt", replace	

import delimited "C:\Users\Francisco\Dropbox\DataZoom\BasesIBGE\datazoom_rar\PNAD_CONTINUA\pnadcontinua_trimestral_20190729\PNADC_022019.txt", encoding(Big5) clear
sample 2
export delimited using "C:\Users\Francisco\Desktop\pasta\pnad_painel\arthur_basico\PNADC_022019.txt", replace	


import delimited "C:\Users\Francisco\Dropbox\DataZoom\BasesIBGE\datazoom_rar\PNAD_CONTINUA\pnadcontinua_trimestral_20190729\PNADC_032019.txt", encoding(Big5) clear
sample 2
export delimited using "C:\Users\Francisco\Desktop\pasta\pnad_painel\arthur_basico\PNADC_032019.txt", replace	


import delimited "C:\Users\Francisco\Dropbox\DataZoom\BasesIBGE\datazoom_rar\PNAD_CONTINUA\pnadcontinua_trimestral_20190729\PNADC_042019.txt", encoding(Big5) clear
sample 2
export delimited using "C:\Users\Francisco\Desktop\pasta\pnad_painel\arthur_basico\PNADC_042019.txt", replace	



* 
datazoom_pnadcontinua, years( 2012 2013 2014 2015 2016 2017 2018 2019 ) original(C:\Users\Francisco\Dropbox\sample_arthur) saving(C:\Users\Francisco\Dropbox\sample_arthur\basico) idbas

clear

datazoom_pnadcontinua, years( 2012 2013 2014 2015 2016 2017 2018 2019 ) original(C:\Users\Francisco\Dropbox\sample_arthur) saving(C:\Users\Francisco\Dropbox\sample_arthur\avancado) idrs