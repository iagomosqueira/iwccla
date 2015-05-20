set old=chdir
cd c:\iwccla\lib
gfortran Man-v14.for a.exe
gfortran Man-v14z.for az.exe
gfortran MANRESV9.for res.exe
cd %old%
