C     Last change:  AEP   18 Jan 2007
C     Is the same as V6 except the zero catch trajectory is read in
C     instead of being calculated in this program.
C     Coastal trials sections deleted
C     Generalized the Continuning catch statistic

      PROGRAM MANRES

C     MANRES Version 8  (29 March 1995)

C   Changes by RBH, April 2007:
C
C     Three lines instead of four are skipped just before reading 'NTRIAL'
C     and 'NYEAR' from 'RESTEST' in order to read the correct input from
C     'RESTEST'.
C
C     The trajectories in 'RESTEST' and 'RES0' are read in free format instead
C     of fixed format.
C
C     Because birth trajectories do not appear in 'RESTEST', birth
C     trajectories are not read. Instead birth is set equal to 0.
C
C     It is checked that year numbers read from 'RESTEST' and 'RES0' appear
C     in the following order: 0, 1, ..., NYEAR.
C
C     The tolerance in the subroutine that finds SY has been increased from
C     0.00001 to 0.0001 because it sometimes does not converge when the
C     tolerance is too low.
C
C     AAV has been corrected. Now the nominator and denominator in the
C     definition are summed over the same number of years.
C
C     The status of the output files 'MSYL.RRR' and 'RESOUT.RRR' has been
C     changed from 'APPEND' to 'unknown'.
C
C     The output units corresponding to 'Andre.out' and 'Andre2.out' are
C     changed.
C
C     The output to standard output has been modified.
C     The fields have been reordered.
C
C     The median, the 5% and 95% quantiles of the population size scaled
C     as specified in 'ISCALE.DAT' are computed.
C     'SS-TRAJ.OUT' includes the computed quantiles.
C     Previously, only the quantiles of the population size scaled by K
C     were computed.
C
C------------------------------------------------------------------------------
C
C  The program reads in the results from the management trials and
C  calculates the following required summary statistics.
C  (Reference: Tokyo report (RIWC 42, page 318) & Kirkwood fax 7/10/91)
C    1  Total catch over management period (CT): median, 5%, 95% & mean values
C    2  Final population size (PFIN): median, 5% & 95% values
C    3  Minimum population sizes reached in each trial (PMIN): 5,10 & 25% values
C    4  Average annual variation (AAV)
C       = mean annual change in catch / mean catch over all simulations
C  * 5  Continuing catch distribution (CC): median, 5% & 95% values
C  * 6  Realised protection level distribution (RPL): median & 5%iles
C       = lowest stock size for which a catch was set
C    7  Relative recovery distribution (RR) : median & 5%iles
C       = stock level in the year when the zero catch trajectory reaches 54%
C
C  * Not for trials with varying K or MSYR or M (epidemics)
C
C    Store the following for graphical presentation:
C    i     Mean population trajectory over the NTRIAL simulations
C    ii    Set of maximum & minimum populations reached in each year
C    iii   Mean catch trajectory over the NTRIAL simulations
C    iv    First 2 stochastic catch trajectories
C
C  Note: catch & population sizes are scaled by the carrying capacity K1
C      unless ISCALE=1.  ISCALE =1 (read in) on varying K or MSYR, epidemics or
C      random parameter trials when population sizes are scaled by the
C      stock size resulting if zero catch is taken during the management period.
C
C PARAMETERS -------------------------------------------------------------------

C **  Single stock version

C     P       Population size at start of current year (before catch is taken)
C     C       Catch in current year   (set by management procedure)
C     PSCALE(IYR) Used to scale size in IYR = K1 in base cases.
C             For varying K or MSYR, epidemics or random parameter trials
C             PSCALE = PZERO
C     PZERO(IYR) The population in IYR if zero catch is taken after year 0.
C             It is read in from file RES0
C     NTRIAL  Number of trials.  Read in.  1-400
C     REALNT  = REAL(NTRIAL)
C     NYEAR   Number of years of management in simulation.  Read in. 1-100
C     K1      Initial carrying capacity (mature size).  Read in.
C
C     PFIN(N)    Final population size in trial N                           [2]
C     PTMIN(N)   Minimum size reached during trial N                        [3]
C     PMEAN(IYR) Mean population size in year IYR                           [i]
C     PIMIN(IYR) Minimum size reached in year IYR in any trial             [ii]
C     PIMAX(IYR) Maximum size reached in year IYR in any trial             [ii]
C     RPL(N)     Realised protection level in trial N                       [6]
C     RR(N)      Relative recovery statistic = population in year when zero
C                catch trajectory = 0.54K  (Use interpolation)              [7]
C     RR1(N)     As for RR(N), except relevant to the 1+ population
C     IYR54      is last year in which the zero catch trajectory < 0.54K    [7]
C                (Interpolate to give the year on a continuous scale)
C     IYR541     as for IYR54, but relevant to the 1+ population
C     CT(N)      Total catch in trial N                                     [1]
C     CTT(N)     Total catch in trial N                                     [1]
C     CTSUM      Total catch over all trials                                [1]
C     CTAV       Average total catch                                        [1]
C     CSTORE     Stores catch from previous year                            [4]
C     AAV        Average annual variability                                 [4]
C     CC(N)      Continuing catch statistic in trial N                      [5]
C     SY(IP)     Sustainable yield/K at population level IP<MSYL            [5]
C     CMEAN(IYR) Mean catch over all trials in year IYR                   [iii]
C     CSTOC(IYR,N) Stores 1st 2 stochastic catch trajectories              [iv]

C     IYR     Current year
C     N       Current trial number  (1,NTRIAL)
C     IP      Population level index                                        [5]
C     IPNT    Output file
C     REF     Reference number of run

C     OPTRAN  = 1 if random values of MSYR, NPCAT & DEPL used for each trial.
C     OPTMOD  Population model.             0: Standard Pella Tomlinson
C            1: P-T with maximum recruitment limitation
C            2: Tent Model  (constant MSYR only)
C            3: Age structured, maturity = recruitment [base case]
C            4: Age structured, differing ages of maturity & recruitment
C               with MSY / 1+ population at MSYL = base case ratio
C            5: Age structured with generalized density-dependence component


C DEFINITIONS ------------------------------------------------------------------
C     Changed by RBH: The array PP is included.
      REAL MSYL,M,PSCALE(0:2000),PZERO(0:2000),ERATE,DEPL,MSYR1,REALNT,
     +     P,K1,A1,Z,RPL(400),PFIN(400),PTMIN(400),PK,RR(400),K99,
     +     MSYR99,KTOT, KTOT1,C, CTAV, CSTORE, AAV, CTSUM, CT(400),
     +     CTT, CTTSUM,
     +     CC(400), SY(0:2000), SUMAV, CMEAN(0:2000), CSTOC(0:2000,2),
     +     PMEAN(0:2000), PIMIN(0:2000), PIMAX(0:2000), PL54,
     +     MAT1,MSIG,REC1,RSIG,MORT1,MORT2,K1P,P1ZERO(0:2000),P1,
     +     PSCALE1(0:2000),P1FIN(400),PT1MIN(400),RR1(400),MFIN(400),
     +     M1FIN(400),PMATF(400,0:2000),PBirth(400,0:2000),
     +     PP(400,0:2000),Avec(0:2),Zvec(0:2)
      REAL PSCALE2(0:2000),PQ,PQMIN(400)
      REAL MSYLT,MSYLE,MSYLM,AMSYRT,AMSYLT,AMSYRE,AMSYLE,AMSYRM,AMSYLM

      INTEGER ISTEP,NYEAR,OPTRAN,OPTMOD, MAXAGE,NTRIAL,ISCALE,NREAD,IYR,
     +        N,I5,I10,I25,I50,I51,I96,IP,IYR54,IYR541,
     +        I,IN,IN2,IN3,IPNT,IOUT2,OPTDEPL,OPTF,OPTMSYL,OPTDD,
     +        IOUT978, IOUT979, IOUT998, IOUT999
      INTEGER IREAD
      REAL DUMMY
      CHARACTER REF*10,ASCALE*3,DESC*60, LINE*120, CRR*16,CRPL*16,CCC*20
      CHARACTER CRR1*16
      LOGICAL ICC,IRR,IRPL
      DATA IPNT/6/, IN/10/, IN2/11/ IN3/12/, IOUT2/13/,
     +     IOUT978/978/, IOUT979/979/, IOUT998/998/, IOUT999/999/
      OPEN (IN, FILE='RESTEST')
      OPEN (IN2, FILE='RES0')
      OPEN (IN3, FILE='ISCALE.DAT')
      OPEN (IOUT2, FILE='SS-TRAJ.OUT')

C INITIALISATION --------------------------------------------------------------

C     This version of MANRES is for single stocks only

C     Read in data. First copy lines defining run parameters to output
C                   Skip the same lines in the zero catch file
      DO 5 I=1,200
        READ (IN,'(A)') LINE
        IF (LINE(1:10).EQ.'Trial:   1') GO TO 6
        WRITE (IPNT,'(1X,A)') LINE
        READ (IN2,'()')
    5 CONTINUE
    6 REWIND IN
      BACKSPACE IN2

C     Now read in required parameters
      READ (IN,'(/6X,A10 /A/)') REF,DESC
      READ (IN,'( / T41,I6)') OPTRAN
      READ (IN,'(T41,I6)') OPTDEPL
      READ (IN,'(T41,F6.2 //  T41,F6.3)') K99,MSYR99
      READ (IN,'( / T41,I6 )') OPTMOD

C     Changed by RBH: Skip three lines instead of four
      READ (IN,'( ///// (T41,I6))') NTRIAL,NYEAR
      READ (IN,'(///)')
      IF (OPTMOD.LT.3) THEN
        READ (IN,'(//T41,I6)') ISTEP
        READ (IN,'(T41,F12.5 //)')  M
      ELSE
        READ (IN,'(T41,I6)') ISTEP
        READ (IN,'((T41,F9.2,9X,F9.2))') MAT1,MSIG,REC1,RSIG
        READ (IN,'((T41,F9.2,F6.2))') MORT1,MORT2
        READ (IN,'(T41,I6)') MAXAGE
        READ (IN,'(T41,I6)') OPTF
        READ (IN,'(T41,I6)') OPTMSYL
        READ (IN,'(T41,I6)') OPTDD
        READ (IN,'(/)')
        ISTEP = 0
      ENDIF

      READ (IN,'(T51,F12.5)') MSYL,MSYR1
      READ (IN,'(T51,F12.5)') Zvec(0),Avec(0),Zvec(1),Avec(1),Zvec(2),Avec(2)
      READ (IN,'(T51,F12.5)') K1,K1P,DEPL
      READ (IN,'(/// T41,F12.5 //)') ERATE
      A1 = Avec(0)
      Z = Zvec(0)
C     Read the scaling option ISCALE which defines how the statistics
C          will be scaled
      READ (IN3,'(I1)') ISCALE
      CLOSE (IN3)

C     Calculate required percentiles to be printed out
      I5  = NTRIAL / 20
      I10 = NTRIAL / 10
      I25 = NTRIAL / 4
      I50 = (NTRIAL+1) / 2
      I51 = (NTRIAL+2) / 2
      I96 = NTRIAL - I5 + 1
      REALNT = REAL(NTRIAL)

C     Initialise arrays
      IYR54 = -1
      IYR541 = -1
      CTAV = 0.0
      DO 40 N=1,NTRIAL
        CC(N) = 0.0
        RPL(N) = 1.E8
        RR(N)  = 1.E8
        PTMIN(N) = 1.E8
        PQMIN(N) = 1.E8
        PT1MIN(N) = 1.E8
        RR1(N) = 1.E8
   40 CONTINUE
      SUMAV  = 0.0
      CTSUM = 0.0
      CTTSUM = 0.0
      AAV = 0.0
      DO 55 IYR=0,NYEAR
        PIMIN(IYR) = 1.E8
        PIMAX(IYR) = 0.0
        PMEAN(IYR) = 0.0
        CMEAN(IYR) = 0.0
   55 CONTINUE

C     Set ICC in trials for which to calculate the continuing catch statistic
C     It is not computed if K, MSYR or M (epidemics) vary or random parameters
C     Call SETUPA to set up the age-structured population parameters
      ICC = K99.EQ.0.0 .AND. MSYR99.EQ.0. .AND. ERATE.EQ.0.0 .AND.
     +      ISTEP.EQ.0 .AND. OPTRAN.NE.1  .AND. OPTDEPL.NE.1
C      ICC = .FALSE.
      IF (ICC.AND.OPTMOD.GE.3)
     +          CALL SETUPA (MAXAGE,MAT1,MSIG,REC1,RSIG,MORT1,MORT2)      [5]

C     Set IRR & IRPL for trials in which to calculate RR and RPL
      IRPL = K99.EQ.0.0 .AND. ERATE.EQ.0.0                                  [6]
      IRR  = DEPL.LT.0.54 .AND.K99.EQ.0. .AND.OPTRAN.NE.1.AND.ERATE.EQ.0.   [7]
     +                   .AND.OPTDEPL.EQ.0

C READ DATA AND INCREMENT STORED VARIABLES ------------------------------------
      DO 200 N = 1,NTRIAL

C       Read trial heading line & check that trial number is correct
        READ (IN,'(/ 6xI4)') NREAD
        READ (IN,'(16X,20F10.5)') MSYLT,MSYLE,MSYLM,AMSYRT,AMSYLT,
     +         AMSYRE,AMSYLE,AMSYRM,AMSYLM
        DO 58 IP = 0, NINT(AMSYLM*1000.0)
          SY(IP) = -1.0
   58   CONTINUE

        IF (NREAD.NE.N) THEN
          WRITE (IPNT,'('' **** ERROR IN READING TRIAL NUMBER'')')
          PRINT *,N,NREAD
          STOP
        ENDIF

C       Read in parameters if they change with each trial
        IF ((OPTRAN.EQ.1.OR.OPTDEPL.EQ.1) .AND. N.GE.1) THEN
           READ (IN,'(T21,8X,5F14.7/)') DEPL,MSYR1,K1,A1,Z,K1P
        ELSE
          READ (IN,'(/)')
        ENDIF

C       Read zero catch trajectory into PZERO
C       Changed by RBH: The zero catch trajectory is read in free format.
        IF (N.EQ.1 .OR. OPTDEPL.EQ.1. OR. OPTRAN.EQ.1 .OR.
     +                                             ERATE.GT.0.0) THEN
           READ (IN2,'(////)')
           DO 59 IYR=0,NYEAR
              IF (IYR.NE.NYEAR) THEN
                 READ (IN2,*) IREAD,PZERO(IYR),P1ZERO(IYR),DUMMY
              ELSE
                 READ (IN2,*) IREAD,PZERO(IYR),P1ZERO(IYR)
              ENDIF
C             Changed by RBH: Check that year number read from 'RES0' is correct.
              IF (IYR.NE.IREAD) THEN
                 STOP ' * ERROR IN READING YEAR NUMBER FROM RES0'
              ENDIF

   59      CONTINUE
C          WRITE(*,*) PZERO(0),K1,PZERO(0)/K1*1000.0,DEPL*1000.0
          IF ( NINT(PZERO(0)/K1*1000.0-DEPL*1000.0).GT.0 )
     +              STOP 'ERROR IN PZERO'

C         Set IYR54 to the last year in which the zero catch trajectory < 0.54K
          IF (DEPL.LT.0.54) THEN
            DO 60 IYR=1,NYEAR
              IF (PZERO(IYR) .GE. 0.9*AMSYLM*K1) GO TO 62
   60       CONTINUE
   62       IYR54 = IYR-1                                                   [7]
            DO 70 IYR=1,NYEAR
              IF (P1ZERO(IYR) .GE. 0.9*AMSYLT*K1P) GO TO 72
   70       CONTINUE
   72       IYR541 = IYR-1
          ENDIF

C         Set PSCALE (= K unless ISCALE=1 when the zero catch trajectory
C             is used instead)
          IF (ISCALE.NE.1) THEN
            DO 64 IYR=0,NYEAR
              PSCALE(IYR) = K1
              PSCALE1(IYR) = K1P
   64       CONTINUE
          ELSE
            DO 65 IYR=0,NYEAR
              PSCALE(IYR) = PZERO(IYR)
              PSCALE1(IYR) = P1ZERO(IYR)
   65       CONTINUE
          ENDIF
          DO 66 IYR=0,NYEAR
            PSCALE2(IYR) = PZERO(IYR)
   66     CONTINUE

          KTOT = K1
          KTOT1 = K1P

        ENDIF

        CTT = 0.0
        DO 100 IYR = 0,NYEAR

C         Read population size & catch in IYR; store & sum variables.
C         Check zero catch file matches in year 0

C         Changed by RBH: Birth is not read from 'RESTEST' but set equal to 0.
C         Birth trajectories do not appear in 'RESTEST'.
          Birth = 0.0

C         Changed by RBH: The trajectores are read in free format.
          IF (IYR.NE.NYEAR) THEN
             READ(IN,*) IREAD, P, P1, C
          ELSE
             READ(IN,*) IREAD, P, P1
          ENDIF

C         Changed by RBH: Check that year number read from 'RESTEST' is correct.
          IF (IYR.NE.IREAD) THEN
             STOP ' * ERROR IN READING YEAR NUMBER FROM RESTEST'
          ENDIF
          IF (IYR.EQ.0.AND.ABS(P-PZERO(0)).GT.0.01)
     +      STOP ' * ERROR: file mismatch'

C         Population size variables: scale by PSCALE
          Birth = Birth / P
          PK = P / K1
          PQ = P / PSCALE2(IYR)
          P = P / PSCALE(IYR)
          IF (P.LT.PTMIN(N)) PTMIN(N) = P                                   [3]
          IF (PQ.LT.PQMIN(N)) PQMIN(N) = PQ                                 [3a]
          PMEAN(IYR) = PMEAN(IYR) + P                                       [i]
          IF (P.LT.PIMIN(IYR)) PIMIN(IYR) = P                              [ii]
          IF (P.GT.PIMAX(IYR)) PIMAX(IYR) = P                              [ii]
          PK1 = P1 / K1P
          P1 = P1 / PSCALE1(IYR)
          IF (P1.LT.PT1MIN(N)) PT1MIN(N) = P1                               [3]

C         Changed by RBH: PP is scaled by PSCALE. PMATF is scaled by K1 as before.
          PP(N,IYR) = P
          PMATF(N,IYR) = PK
          PBIRTH(N,IYR) = Birth

C         Set Relative Recovery = population in year when zero catch trajectory
C         reaches 0.54K.  Interpolate to give value on a continuous scale
C         NB If PZERO(I=0,NYEAR) < 0.54K then IYR54=NYEAR & RR will not be reset
          IF (IYR.EQ.IYR54)   PL54 = PK                                     [7]
          IF (IYR.EQ.IYR54+1 .AND. IRR) RR(N) = PL54 + (PK-PL54) *          [7]
     +              (0.54*K1-PZERO(IYR54)) / (PZERO(IYR)-PZERO(IYR54))
          IF (IYR.EQ.IYR541)   PL541 = PK1                                  [7]
          IF (IYR.EQ.IYR541+1 .AND. IRR) RR1(N) = PL541 + (PK1-PL541) *     [7]
     +        (0.54*K1P-P1ZERO(IYR541)) / (P1ZERO(IYR)-P1ZERO(IYR541))

          IF (IYR.EQ.NYEAR) GO TO 100
C
          IF (PK.LT.RPL(N) .AND. C.GT.0.0) RPL(N) = PK                      [6]

C         Catch variables: scale by initial K
          C = C / KTOT

C         Changed by RBH. Now the nominator and denominator in the definition of AAV
C         are summed over the same number of years.
          IF (IYR .LT. (NYEAR-1)) THEN
           CTT = CTT + C                                                  [1]
          ELSE
           CT(N) = CTT + C
          ENDIF

          IF (IYR.GT.0) SUMAV = SUMAV + ABS(C - CSTORE)                     [4]
          CSTORE = C

          IF (ICC .AND. IYR.GE.NYEAR-10) THEN                               [5]
C           Increment the continuing catch statistic
            IF (PK.GT.AMSYLM) PK = AMSYLM                                   .
            IP = NINT(PK*1000.0)
C           If necessary set SY(IP) = sustainable yield at popn level IP / K.
            IF (SY(IP).EQ.-1.0) CALL SETSY (SY,MSYR1,A1,Z,M,MAXAGE,
     +                                         OPTMOD,IP,OPTDD)
            CC(N) = CC(N) + MIN (C,SY(IP))                                  [5]
          ENDIF

C         Increment / store graphical catch variables
          CMEAN(IYR) = CMEAN(IYR) + C                                     [iii]
          IF (N.LE.2) CSTOC(IYR,N) = C                                     [iv]

  100   CONTINUE

C       End of trial N.  Store final stock size & total catch
        PFIN(N)  = P                                                        [2]
        P1FIN(N) = P1
        MFIN(N) = P / AMSYLM
        M1FIN(N) = P1 / AMSYLT
        CTSUM = CTSUM + CT(N)                                               [1]
        CTTSUM = CTTSUM + CTT                                               [1]

  200 CONTINUE


C CALCULATE SUMMARY STATISTICS ------------------------------------------------

C     All data now read.  Calculate the summary statistics

C     Sort arrays
C     Changed by RBH: Sort PP
  202 CALL SHSORT (PFIN,NTRIAL,1,1)                                         [2]
      CALL SHSORT (PTMIN,NTRIAL,1,1)                                        [3]
      CALL SHSORT (PQMIN,NTRIAL,1,1)                                        [3]
      CALL SHSORT (P1FIN,NTRIAL,1,1)                                        [2]
      CALL SHSORT (PT1MIN,NTRIAL,1,1)                                       [3]
      CALL SHSORT (MFIN,NTRIAL,1,1)
      CALL SHSORT (M1FIN,NTRIAL,1,1)
      CALL SHSORT (PP,NTRIAL,(Nyear+1),2001)
      CALL SHSORT (PMATF,NTRIAL,(Nyear+1),2001)
      CALL SHSORT (PBIRTH,NTRIAL,(NYear+1),2001)
      IF (ICC)  CALL SHSORT (CC,NTRIAL,1,1)                                 [5]
      IF (IRPL) CALL SHSORT (RPL,NTRIAL,1,1)                                [6]
      IF (IRR)  CALL SHSORT (RR,NTRIAL,1,1)                                 [7]
      IF (IRR)  CALL SHSORT (RR1,NTRIAL,1,1)                                [7]
      CALL SHSORT (CT,NTRIAL,1,1)                                          [1]

C     Calculate mean catch and average annual variation
      IF (CTSUM.GT.0.0) AAV = SUMAV / CTTSUM                                [4]
      CTAV = CTAV + CTSUM/REALNT

C     Calculate mean catch & population trajectories
      DO 500 IYR = 0,NYEAR-1
        PMEAN(IYR) = PMEAN(IYR) / REALNT                                    [i]
        CMEAN(IYR) = CMEAN(IYR) / REALNT                                  [iii]
  500 CONTINUE
      PMEAN(NYEAR) = PMEAN(NYEAR) / REALNT                                  [i]

C PRINT OUT RESULTS -----------------------------------------------------------

C     Print * in results line, col 1 if stock sizes scaled by 0 catch trajectory
      ASCALE='   '
      IF (ISCALE.EQ.1) ASCALE=' * '

      WRITE (IPNT,'(/3A)') ' Scaling: in lines beginning with a *  the '
     +      ,'P(low) & P(final) statistics are scaled by the stock size'
     +      ,' obtained after a zero catch during the management period'
      WRITE(IPNT,999)'RPL','no catch was set during that simulation'
      WRITE(IPNT,999)'RR','the zero catch trajectory didnt reach 0.54K'
 999  FORMAT (' ***** in the',A4,' column indicates that ',A)

      WRITE (IPNT,'(/A7,A16,T30,A,T55,A,T75,A,T94,A,T113,A8,2A16)')
     +      'Ref.','Initial',
     +      'Total catch values','Final stock size','Low population',
     +    '100*ContinuingCatch','AAV','RPL    ','Rel.Recovery'
C    Changed by RBH: Output to standard output is modified.
      WRITE (IPNT,'(13X,2A6,2A7,2A6,3(A6,A8,A6),8X,4A8)')
     +     'Size', 'MSYR',
     +     '5% ','Median','95%','Mean',
     +     '5% ','Median','95%','Min','5% ','Median',
     +     '5% ','Median','95%',('5%','Median',I=1,2)

      CCC  = ' '
      CRPL = ' '
      CRR  = ' '
      IF (ICC) WRITE (CCC,'(3F6.3,2X)') CC(I5)*10.,(CC(I50)+CC(I51))*5.,
     +     CC(I96)*10.
      IF (IRPL) WRITE(CRPL,'(2F6.3)')RPL(I5),(RPL(I50)+RPL(I51))*.5
      IF (IRR) WRITE (CRR,'(2F6.3)') RR(I5),(RR(I50)+RR(I51))*.5
      IF (IRR) WRITE (CRR1,'(2(2X,F6.3))')
     +     RR1(I5),(RR1(I50)+RR1(I51))*.5

C     Changed by RBH: status changed from 'APPEND' to 'unknown'.
      OPEN(IOUT978,FILE='..\RESOUT.RRR',ACCESS='APPEND')

      WRITE (IOUT978,'(A3,A10,F7.3,7(3F7.3,2X),F8.3,2A16,F8.3,2X,A)')
     +     ASCALE,REF,(CT(I50)+CT(I51))*.5,
     +     CT(I5),CT(I96),CTAV,
     +     (PFIN(I50)+PFIN(I51))*.5,PFIN(I5),PFIN(I96),
     +     (P1FIN(I50)+P1FIN(I51))*.5,P1FIN(I5),P1FIN(I96),
     +     PTMIN(I5),PTMIN(I10),PTMIN(I25),
     +     PT1MIN(I5),PT1MIN(I10),PT1MIN(I25),
     +     (MFIN(I50)+MFIN(I51))*.5,MFIN(I5),MFIN(I96),
     +     (M1FIN(I50)+M1FIN(I51))*.5,M1FIN(I5),M1FIN(I96),
     +     AAV,CRR,CRR1,DEPL,DESC
      CLOSE(IOUT978)
      OPEN(IOUT979,FILE='..\MSYL.RRR',ACCESS='APPEND')
      WRITE (IOUT979,'(A10,9F8.5,2X,A)') REF,MSYLT,MSYLE,MSYLM,
     +     AMSYRT,AMSYLT,AMSYRE,AMSYLE,AMSYRM,AMSYLM,DESC
      CLOSE(IOUT979)

C     Changed by RBH: status changed from 'APPEND' to 'unknown'.
      OPEN(IOUT978,FILE='..\RESOUT2.RRR',ACCESS='APPEND')
C
C     Changed by RBH: Output to standard output is modified.
      WRITE (IOUT978,'(A3,A10,F6.3,F6.3,2X,4F6.3,2X,2(3F6.3,2X),
     +     A20,F8.3,2X,2A16,2X,A)')
     +     ASCALE,REF,DEPL,MSYR1,
     +     CT(I5),(CT(I50)+CT(I51))*.5,CT(I96),CTAV,
     +     PFIN(I5),(PFIN(I50)+PFIN(I51))*.5,PFIN(I96),
     +     PTMIN(I5),PTMIN(I10),PTMIN(I25),
     +     CCC,AAV,CRPL,CRR,DESC
      CLOSE(IOUT978)
     
C
C     Changed by RBH: Output to standard output is modified.
      WRITE (IPNT,'(A3,A10,F6.3,F6.1,2X,4F6.3,2X,2(3F6.3,2X),
     +     A20,F8.3,2X,2A16,2X,A)')
     +     ASCALE,REF,DEPL,MSYR1,
     +     CT(I5),(CT(I50)+CT(I51))*.5,CT(I96),CTAV,
     +     PFIN(I5),(PFIN(I50)+PFIN(I51))*.5,PFIN(I96),
     +     PTMIN(1),PTMIN(I5),(PTMIN(I50)+PTMIN(I51))*.5,
     +     CCC,AAV,CRPL,CRR,DESC

C     Changed by RBH: Output to 'SS-TRAJ.OUT' includes the median, the 5% and 95% quantiles
C     of the scaled population size.
      WRITE (IOUT2,'(2A)') 'Note: ignore zero catch trajectory ',
     +        '(P: C0 column) in random parameter or epidemic trials'
      WRITE (IOUT2,'(//1X,3A/)') REF, ASCALE
      WRITE (IOUT2,'(6X,10A9)') 'P5% ','MedP','P95%',
     +     'MeanP','MinP','MaxP','MeanC','C1','C2',' P: C0'
      DO 550 IYR=0,NYEAR-1
        WRITE (IOUT2,'(I6,9F9.4,F12.4)') IYR,
     +        PP(I5,IYR), (PP(I50,IYR)+PP(I51,IYR))*.5, PP(I96,IYR),
     +        PMEAN(IYR), PIMIN(IYR), PIMAX(IYR),
     +        CMEAN(IYR),CSTOC(IYR,1),CSTOC(IYR,2),PZERO(IYR)
 550  CONTINUE
      WRITE (IOUT2,'(I6,6F9.4,27X,F12.4)') NYEAR, PP(I5,NYEAR),
     +     (PP(I50,NYEAR)+PP(I51,NYEAR))*.5, PP(I96,NYEAR),
     +     PMEAN(NYEAR),PIMIN(NYEAR),PIMAX(NYEAR),PZERO(IYR)

      CLOSE (IN2)
      CLOSE (IOUT2)
C
C     Extra outputs for IST testing
C     =============================

C     Changed by RBH: The output unit is changed to 'IOUT999'..
      OPEN(IOUT999,FILE='Andre.Out')
      WRITE(IOUT999,10602)
      DO 11000 N = 1,Ntrial
       WRITE(IOUT999,10601) N/FLOAT(Ntrial),PFIN(N),PTMIN(N),PQMIN(N)
11000 CONTINUE
      CLOSE(IOUT999)

C     Changed by RBH: The output unit is changed to 'IOUT998'.
      OPEN(IOUT998,FILE='Andre2.out')
      WRITE(IOUT998,10701)
      DO 12000 IYR = 0,NYear
       WRITE(IOUT998,10702) IYR,
     +   PMATF(I5,IYR),(PMATF(I50,IYR)+PMATF(I51,IYR))/2.0,
     +   PMATF(I96,IYR),
     +   PBIRTH(I5,IYR),(PBIRTH(I50,IYR)+PBIRTH(I51,IYR))/2.0,
     +   PBIRTH(I96,IYR)
12000 CONTINUE
      CLOSE(IOUT998)
      STOP
10602 FORMAT(1x," PROB  PFIN PFMINA PMINS")
10601 FORMAT(1x,4(F5.3,1x))
10701 FORMAT(1x," Year Female Pop Size       Births"/
     +       1x,"        5%   Median  95%   5% Median 95%")
10702 FORMAT(1x,I4,1x,2(3(F5.3,2x)))
      END


C ------------------------------------------------------------------------------
C ------------------------------------------------------------------------------

      SUBROUTINE SETSY (SY,MSYR1,A1,Z,M,MAXAGE,OPTMOD,IP,OPTDD)

C     This subroutine sets SY(P) = sustainable yield/K for population sizes
C     at .1% intervals from P=0 to K.

      IMPLICIT NONE

      COMMON /AGEPAR/ UNR, RECF, FMATUR, SUR, FEC
      REAL    SUR(0:100),UNR(0:100),RECF(0:100),FMATUR(0:100),FEC

      REAL SY(0:2000),MSYR1,A1,Z,M,P,CONST,S,UF,DIF,UFMIN,UFMAX,RMAT,
     +     RREC,RTOT,THEREC,R(0:100),TERM2
      INTEGER MAXAGE, OPTMOD, IP, ICOUNT, L, OPTDD

C     Calculate number of recruits for an equilibrium population level P=N/K

      P = REAL(IP)/1000.0

      IF (OPTMOD.LE.1) THEN
        S = EXP(-M)
        CONST = A1 * (1.0-S) / S
        SY(IP) = CONST * P * (1.0 - P**Z)

        RETURN

      ELSE IF (OPTMOD.EQ.2) THEN
C       Tent model:  (this assumes MSYR is constant)
        SY(IP) = MSYR1 * P

        RETURN

      ENDIF

C     Age structured model (OPTMOD >= 3)

C     Find fishing survivorship UF which balances with population level P
C     (P = Nmature / Kmature)
      R(0)   = RECF(0)
      ICOUNT = 0
      UFMAX = 1.0
      UFMIN = 0.8

   10 UF = (UFMIN + UFMAX) * 0.5
      ICOUNT = ICOUNT + 1
C
C     Per-recruit calculations
      RREC = R(0)
      RMAT = 0.0
      RTOT = 0.0
      DO 20 L = 1,MAXAGE-1
        R(L)   = SUR(L-1)*(R(L-1)*UF+UNR(L-1)*RECF(L))
        RMAT = RMAT + (R(L)+UNR(L)) * FMATUR(L)
        RREC = RREC + R(L)
        RTOT = RTOT + R(L)+UNR(L)
   20 CONTINUE
      R(MAXAGE) = SUR(MAXAGE-1) * (R(MAXAGE-1)*UF + UNR(MAXAGE-1))
      R(MAXAGE) = R(MAXAGE)/(1. - SUR(MAXAGE)*UF)
C     All animals in maximum age class are mature and recruited
      RMAT = RMAT + R(MAXAGE)
      RTOT = RTOT + R(MAXAGE)
      RREC = RREC + R(MAXAGE)
C
C     Special case (OPTMODEL=3)
      IF (OPTMOD.EQ.3) RMAT = RREC
C
C     Recruitment
      TERM2 = 1.0-(1.0/(FEC*RMAT)-1.0)/A1
      IF (TERM2.LT.0) THEN
       TheRec = -1
      ELSE
       IF (OPTMOD.EQ.3.OR.OPTDD.EQ.2) THEN
        TheRec = TERM2**(1/Z)/RMAT
       ELSEIF (OPTDD.EQ.1) THEN
        TheRec = TERM2**(1/Z)/RTOT
       ELSEIF (OPTDD.EQ.0) THEN
        TheRec = TERM2**(1/Z)/RREC
       ELSE
        WRITE(*,*) "Invalid value for OPTDD"
        STOP
       ENDIF
      ENDIF
      DIF = TheRec*RMAT - P

C     Changed by RBH: The tolerance has been increased from 0.00001 to 0.0001.
      IF (ABS(DIF).LT.0.0001) GO TO 90

      IF (DIF.LT.0.0) THEN
        UFMIN = UF
      ELSE
        UFMAX = UF
      ENDIF
      IF (ICOUNT.GT.500) STOP ' **** ERROR: SY NOT FOUND'
      GO TO 10

   90 CONTINUE
      IF (OPTMOD.EQ.3) THEN
        SY(IP) = (1.0-UF) * P
      ELSE
        SY(IP) = (1.0-UF) * RREC * TheRec
      ENDIF

      RETURN
      END


C ------------------------------------------------------------------------------
C ------------------------------------------------------------------------------

      SUBROUTINE SETUPA (MAXAGE,MAT1,MSIG,REC1,RSIG,MORT1,MORT2)

C     Subroutine sets up parameters needed by SETSY to calculate the
C     sustainable yield in age-structured cases

      COMMON /AGEPAR/ UNR, RECF, FMATUR, SUR, FEC
      REAL    SUR(0:100),UNR(0:100),RECF(0:100),FMATUR(0:100),FEC

      REAL MAT1,MSIG,REC1,RSIG,MORT1,MORT2,SURV,PA,RMAT
      INTEGER MAXAGE,L
      EXTERNAL SURV

C     Set up maturity ogive:  FMATUR = proportion mature of age A
      CALL SETO(FMATUR,MSIG,MAT1,MAXAGE)

C     Set up natural mortality-at-age array
      DO 4 L = 0,MAXAGE
        SUR(L) = SURV(MORT1,MORT2,L)
    4 CONTINUE

C     Calculate the relative mature population size starting with
C     unity in the first age class (L=0)
C     Adjust for last age class being pooled (and always fully mature)
      PA   = 1.0
      RMAT = 0.0
      DO 5 L = 0,MAXAGE-1
        RMAT = RMAT + PA*FMATUR(L)
        PA   = PA*SUR(L)
    5 CONTINUE
      PA   = PA/(1.0 - SUR(MAXAGE))
      RMAT = RMAT + PA

C     Set up the recruitment ogive in transition form:
C     set RECF =fraction of unrecruited animals of age A which recruit
C     at age A+1, except RECF(0) = fraction recruited of age 0
      CALL SETO (RECF,RSIG,REC1,MAXAGE)
      CALL TRFORM(RECF,MAXAGE)

C     Set up unrecruited component relative to # of age 0
      UNR(0) = 1.0 - RECF(0)
      DO 9 L = 1,MAXAGE
        UNR(L) = UNR(L-1)*SUR(L-1)*(1. - RECF(L))
   9  CONTINUE

C     Store Fec (P=K)
      FEC = 1.0/RMAT

      RETURN
      END


C     -------------------------------------------------------------------
C     -------------------------------------------------------------------

      SUBROUTINE SETO (V,SIG,MEAN,MAXAGE)

C     SETO computes the ogive corresponding to the given parameters

      REAL V(0:100),SIG,MEAN
      INTEGER MAXAGE,L

C     Loop over all ages
      DO 10 L = 1, MAXAGE-2
        IF ((REAL(L)-MEAN)/SIG.GT.10.0) THEN
          V(L) = 1.0
        ELSE IF ((REAL(L)-MEAN)/SIG.LT.-10.0) THEN
          V(L) = 0.0
        ELSE
          V(L) = 1.0/(1.0 + EXP(-(REAL(L)-MEAN)/SIG))
        ENDIF
   10 CONTINUE
      V(0) = 0.0
      V(MAXAGE-1) = 1.0
      V(MAXAGE) = 1.0

      RETURN
      END

C     -------------------------------------------------------------------

      REAL FUNCTION SURV(MEAN0,MEAN1,A)

C     This function computes survival as function of age

      REAL MEAN0,MEAN1,BETA,ALPHA
      INTEGER A

      BETA = (MEAN1-MEAN0)/16.0
      ALPHA = MEAN0 - 4.0*BETA
      IF (A.LE.4) THEN
         SURV = EXP(-MEAN0)
      ELSE
         SURV = EXP(-(ALPHA+BETA*A))
      ENDIF

      END

C     -------------------------------------------------------------------

      SUBROUTINE TRFORM(V,MAXAGE)

C     Adjust an ogive to transition form, that is so that V(L) =
C     the proportion of animals in a given class at age A-1 which make the
C     transition to a different class age A

      REAL V(0:100),RM,D
      INTEGER MAXAGE,L

      RM   = V(0)
      DO 90 L = 1,MAXAGE
        IF (RM .LT. 1.0) THEN
          D = RM
          RM = V(L)
          V(L) = (RM - D)/(1.0 - D)
        ELSE
          RM = V(L)
          V(L) = 1.0
        ENDIF
   90 CONTINUE

      RETURN
      END


C ------------------------------------------------------------------------------
C ------------------------------------------------------------------------------

      SUBROUTINE SHSORT(ARR,NTRIAL,NSORT,IDIM)

C     Subroutine sorts rows of array ARR into ascending order, by the Shell-Mezgar
C     algorithm (diminishing increment sort).  (Reference: Press, Flannery,
C     Teukolsky & Vetterling: Numerical Recipes, CUP, Cambridge 1986 (Page 229)
C     ARR is replaced on output by its sorted rearrangement.
C     NSORT is number of rows to be sorted
C
      INTEGER NTRIAL,NSORT,JE,M,K,NN,LOGNB2,I,J,L,IDIM
      REAL ARR(400,IDIM),ALN2I,TINY,T
      PARAMETER (ALN2I=1./0.69314718, TINY=1.E-5)

      LOGNB2 = INT(LOG(REAL(NTRIAL))*ALN2I+TINY)
      DO 20 JE=1,NSORT
        M = NTRIAL
        DO 12 NN = 1,LOGNB2
          M = M/2
          K = NTRIAL-M
          DO 11 J=1,K
            I = J
    3       CONTINUE
            L = I+M
            IF (ARR(L,JE).LT.ARR(I,JE)) THEN
              T = ARR(I,JE)
              ARR(I,JE) = ARR(L,JE)
              ARR(L,JE) = T
              I = I-M
              IF (I.GE.1) GO TO 3
            ENDIF
   11     CONTINUE
   12   CONTINUE
   20 CONTINUE
      RETURN
      END

