C     Last change:  CA   23 Aug 2013    2:07 pm
C     MAN-V13 : Single Stock Control program
C     Program links MANTST12 modules with catches set by the CLC

      INCLUDE 'MANTST14.FOR'
      INCLUDE 'MANTSTALT.FOR'
      INCLUDE 'MANEXTRD.FOR'
      INCLUDE 'NRMP-INF.FOR'
      INCLUDE 'NRMP.FOR'


      SUBROUTINE EXTRA (NTRIAL, OPTRAN, ERATE, ENDSUR, IOUT,PROG,ITUNE,
     +                  OPTDEPL) 

C     Subroutine EXTRA is used to open output files.
C     and set global parameters.
C
      INTEGER NTRIAL, OPTRAN, ENDSUR,IOUT,ITUNE,OPTDEPL
      DOUBLE PRECISION ERATE
      CHARACTER PROG*8

      OPEN (IOUT,FILE='RESTEST')
      OPEN (1,FILE='MANLOG')
      PROG = 'MANV13'
      ITUNE = 0

C     EXTRA may also be used to avoid wasting time by setting the NTRIAL=1 
C     for trials when the catch quota is set to zero
C     IF (OPTDEPL.EQ.0.AND.OPTRAN.NE.1 .AND. ERATE.EQ.0.0) NTRIAL = 1
C     ENDSUR = 0

      RETURN
      END
