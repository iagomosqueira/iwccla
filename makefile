all: Trials_KFJ_base.csv

Trials_KFJ_base.csv: Trials_KFJ.xlsx ; cscript scripts\saveAllTabsAsCSV.vbs Trials_KFJ.xlsx
	rm Trials_KFJ_info.csv
	rm Trials_KFJ_input.csv
	rm Trials_KFJ_AEP.csv