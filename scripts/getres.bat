del MSYL.RRR
del RESOUT.RRR

for /f %%f in ('dir /b') do (
  cd %%f
  copy c:\iwccla\lib\res.exe res.exe /Y
  res.exe
  cd ..
)
