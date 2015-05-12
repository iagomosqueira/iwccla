all: iwccla_msy-testing.docx SC66aRMPX.docx

iwccla_msy-testing.docx: iwccla_msy-testing.md ; pandoc --reference-docx=..\references\template.docx --bibliography=..\references\iwccla.bib iwccla_msy-testing.md -o iwccla_msy-testing.docx
SC66aRMPX.docx: SC66aRMPX.md ; pandoc --reference-docx=..\references\template.docx --bibliography=..\references\iwccla.bib SC66aRMPX.md -o SC66aRMPX.docx
