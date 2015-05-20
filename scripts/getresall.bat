set old=%cd%
cd c:\iwccla\orig100
del MSYL.RRR
del RESOUT.RRR
for /f %%h in ('dir /b') do copy c:\iwccla\lib\res.exe %%h\res.exe /Y
for /f %%f in ('dir /b') do (
  echo %%f
  cd %%f
  res.exe
  cd ..
)
cd c:\iwccla\orig300
del MSYL.RRR
del RESOUT.RRR
for /f %%h in ('dir /b') do copy c:\iwccla\lib\res.exe %%h\res.exe /Y
for /f %%f in ('dir /b') do (
  echo %%f
  cd %%f
  res.exe
  cd ..
)
cd c:\iwccla\pslope4100
del MSYL.RRR
del RESOUT.RRR
for /f %%h in ('dir /b') do copy c:\iwccla\lib\res.exe %%h\res.exe /Y
for /f %%f in ('dir /b') do (
  echo %%f
  cd %%f
  res.exe
  cd ..
)
cd c:\iwccla\pslope4300
del MSYL.RRR
del RESOUT.RRR
for /f %%h in ('dir /b') do copy c:\iwccla\lib\res.exe %%h\res.exe /Y
for /f %%f in ('dir /b') do (
  echo %%f
  cd %%f
  res.exe
  cd ..
)
cd c:\iwccla
scripts\res2dropbox.bat
cd %old%
