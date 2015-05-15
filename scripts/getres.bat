del MSYL.RRR
del RESOUT.RRR

for /f %%f in ('dir /b') do (
  echo %%f
  cd %%f
  res.exe
  cd ..
)
