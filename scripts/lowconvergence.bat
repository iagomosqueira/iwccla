del RES0
del RESOUT
xcopy ..\..\lib\MANTST14_lowconvergence.FOR MANTST14.FOR /Y
gfortran Man-v14.for
gfortran Man-v14z.for -o az.exe
az
a
res
