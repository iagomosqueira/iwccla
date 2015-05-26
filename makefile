all: Trials_KFJ_base.csv

Trials_KFJ_base.csv: Trials_KFJ.xlsx ; cscript scripts\saveAllTabsAsCSV.vbs Trials_KFJ.xlsx
	del Trials_KFJ_info.csv
	del Trials_KFJ_input.csv
	del Trials_KFJ_AEP.csv
