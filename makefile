all: MANST14_scenarios.docx

MANST14_scenarios.docx: MANST14_scenarios.md ; pandoc --reference-docx=references\template.docx --bibliography=references\iwccla.bib MANST14_scenarios.md -o MANST14_scenarios.docx