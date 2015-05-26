set old=%cd%
cd c:\iwccla
for /f %%f in ('dir *0 /b /ad') do (
  xcopy %%f\*.RRR c:\users\kelli\dropbox\iwccla\%%f\*.RRR /Y
)
cd %old%
