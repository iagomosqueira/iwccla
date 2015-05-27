del *.RRR

for /f %%f in ('dir /b /ad') do (
  cd %%f
  del res.exe
  copy c:\iwccla\lib\res.exe res.exe /Y
  res.exe
  cd ..
)
