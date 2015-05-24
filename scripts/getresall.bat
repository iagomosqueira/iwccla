set old=%cd%
for /f %%h in ('dir *0 /b /ad') do (
  cd %%h
  del MSYL.RRR
  del RESOUT.RRR
  for /f %%f in ('dir /b /ad') do (
    cd %%f
    copy c:\iwccla\lib\res.exe res.exe /Y
    res.exe
    cd ..
  )
)
cd c:\iwccla
scripts\res2dropbox.bat
cd %old%
