C --------------------------------------------------------------------------
C --------------------------------------------------------------------------
C
      SUBROUTINE MSYPAR2 (OK,MORT1,MORT2,MORTIP,MSYR2)
C
C    Subroutine finds the resilience A1 & density dependent exponent Z
C    which give the required MSY at the specified MSYL.  It equates the
C    slope in the balancing per capita birthrates at two levels of
C    fishing mortality just above and below MSY, with the derivative of
C    per capita birth rate at MSYL. A root finding routine using Brent's
C    method is used to solve for the density dependent exponent.

      IMPLICIT NONE
      COMMON /HITFIT/ FMSY,SMSYL,RFTOT0,RFEXP0,
     +       FMSYL,FECMSY,MSYLT1,MSYLT0,MSYLT2,AMSYR1,AMSYL1,
     +       AMSYR0,AMSYL0,AMSYR2,AMSYL2,OPTDD,OPTF,OPTMSYL,OPTDK,OPTDT
      DOUBLE PRECISION FMSY,SMSYL,RFTOT0,RFEXP0,FMSYL,FECMSY
      DOUBLE PRECISION AMSYR0,AMSYL0,AMSYR1,AMSYL1,AMSYR2,AMSYL2
      DOUBLE PRECISION MSYLT0,MSYLT1,MSYLT2
      INTEGER OPTDD,OPTF,OPTMSYL,OPTDK,OPTDT

      COMMON /STKVRS/ PTRUE(-65:2000),MSYL,MSYR1,Z,KSURV,
     +         PSURV(-65:2000),
     +         PSURV1,ERATE,CATERR,A1,K1,K1P,PROBE(0:2000),OPTMOD,OPTC
      DOUBLE PRECISION PTRUE,MSYL,MSYR1,Z,KSURV,PSURV,PSURV1,ERATE,
     +        CATERR,A1,K1,K1P
      INTEGER PROBE,OPTMOD,OPTC

      COMMON /AGEPAR/ MAT1,MSIG,REC1,RSIG,FEC,SUR(0:60),UNREC(0:60),
     +                REC(0:60),RECF(0:60),FMATUR(0:60),MAXAGE,MINMAT
      DOUBLE PRECISION MAT1,MSIG,REC1,RSIG,FEC,SUR,UNREC,REC,RECF,FMATUR
      INTEGER MAXAGE,MINMAT

      COMMON /MORTPASS / MORT1A,MORT2A,MORTIPA
      DOUBLE PRECISION MORT1A,MORT2A
      INTEGER MORTIPA

      COMMON / DENSPASS / AA0,AA1,ZZ0,ZZ1,FMSYPASS,MSYLPASS,
     +     CATCHOUT,MSYLOUT,DERIV,MSYLPRED,XBEST,FUNKBEST,DTYPE,PHASE
      DOUBLE PRECISION AA0,AA1,ZZ0,ZZ1,DERIV,MSYLPRED
      DOUBLE PRECISION FMSYPASS,MSYLPASS,CATCHOUT,MSYLOUT(0:2)
      DOUBLE PRECISION XBEST(100),FUNKBEST
      INTEGER DTYPE,PHASE

      COMMON /INITS/ INITA,INITZ
      DOUBLE PRECISION INITA,INITZ

      EXTERNAL ZCALC,SURV,SMSYR,FUNK
      DOUBLE PRECISION MORT1,MORT2,SURV,SMSYR,MSYR2
      DOUBLE PRECISION X(100),SS,FF,FUNK,Y(100),SS2
      DOUBLE PRECISION AA1STORE,ZZ1STORE,SSBEST
      INTEGER OK,L,MORTIP,STOREOPTF,IFF
      DOUBLE PRECISION MSYLSAVE,MSYRSAVE
      SAVE MSYLSAVE,MSYRSAVE

C     LOCAL VARIABLES
      INTEGER MYLOOP,MYLOOP2
      DOUBLE PRECISION try(2),keep(2),oldss

C     Need to do this?
      IF (MSYLSAVE.EQ.MSYL.AND.MSYRSAVE.EQ.MSYR2) RETURN
      MSYLSAVE = MSYL
      MSYRSAVE = MSYR2
C
C     Note SUR array set in RESET or in conditioning (in GENPAR & HITPEG)
C     Set up maturity ogive: FMATUR =proportion of age A which are mature {A5}
C     Knife edge at age MAT50 in this version
      CALL SETO (FMATUR,MSIG,MAT1,MAXAGE)
      DO 7 L = 1,MINMAT
       FMATUR(L) = 0.D0
7     CONTINUE
C
C     Set the mortality parameters
      MORT1A = MORT1
      MORT2A = MORT2
      MORTIPA = MORTIP
C
C     First find A and Z (DTYPE = 1)

      DTYPE = 1
      FMSYPASS = MSYR2
      MSYLPASS = MSYL

      keep(1) = log(1.1)
      X(1) = log(1.1)
      keep(2) = log(2.39)
      X(2) = log(2.39)

      PHASE = 1
      oldss = FUNK(X)
      OPEN (31,FILE='XY')
      try(1) = INITA
      DO 1001 MYLOOP=1,25,1
        try(1) = try(1) + 0.02
        try(1) = 0.02*EXP(0.4d0*MYLOOP)
        X(1) = log(try(1))
        try(2) = INITZ
        DO 1002 MYLOOP2=1,50,1
         try(2) = try(2) + 0.01
         try(2) = 0.1 + 0.1*MYLOOP2
         X(2) = log(try(2))
         SS = FUNK(X)
         IF (SS.LT.oldss) THEN
          keep(1) = X(1)
          keep(2) = X(2)
          oldss = SS
        ENDIF
1002  CONTINUE
1001  CONTINUE
      X(1) = keep(1)
      X(2) = keep(2)
      PHASE = 1
      CALL FIT22(X,SS,2)
      WRITE(*,112) "Phase1:",AA1,ZZ1,SS
      PHASE = 2
      CALL FIT22(X,SS,2)
      X = XBEST
      CALL FIT22(X,SS,2)
      WRITE(*,112) "Phase2:",AA1,ZZ1,SS
      PHASE = 3
      SSBEST = FUNK(X)
9898  CALL FIT22(X,SS,2)
      WRITE(*,112) "Phase3:",AA1,ZZ1,SS
  112 FORMAT(1X, A, "A", 1X, F18.8, "  Z", F15.8, "  SS", 1X, F15.8)
      IF (ABS(SS-SSBEST).GT.0.0000001) THEN
       WRITE(*,*) SS,SSBEST
       SSBEST = SS
       GOTO 9898
      ENDIF

      CALL MODELPROJ(MSYR2)
C      WRITE(*,*) "FIN",AA1,ZZ1,SS
      IF (ABS(SS).GT.1.0E-5) WRITE(*,*) "WARNING"

      DO 1000 IFF = 1,100
C       FF = FLOAT(IFF)/30.0*MSYR2
C       CALL MODELPROJ(FF)
C       WRITE(*,609) FF,CATCHOUT,MSYLOUT
1000  CONTINUE
C
C     Find MSYL and MSYR for alternative definitions of selectivity
      DTYPE = 2
      STOREOPTF = OPTF
      OPTF = 0
      FUNKBEST = 1.0E20
      X(1) = log(MSYR2)
      CALL FIT22 (X,SS,1)
      X = XBEST
      CALL FIT22 (X,SS,1)
      X = XBEST
      SS = FUNK(X)
      AMSYR0 = EXP(X(1))
      AMSYL0 = MSYLOUT(0)
      OPTF = 1
      FUNKBEST = 1.0E20
      X(1) = log(MSYR2)
      CALL FIT22 (X,SS,1)
      X = XBEST
      CALL FIT22 (X,SS,1)
      X = XBEST
      SS = FUNK(X)
      AMSYR1 = EXP(X(1))
      AMSYL1 = MSYLOUT(1)
      OPTF = 2
      FUNKBEST = 1.0E20
      X(1) = log(MSYR2)
      SS = FUNK(X)
      CALL FIT22 (X,SS,1)
      X = XBEST
      CALL FIT22 (X,SS,1)
      X = XBEST
      SS = FUNK(X)
      AMSYR2 = EXP(X(1))
      AMSYL2 = MSYLOUT(2)
      OPTF = STOREOPTF

      AA1STORE = AA1
      ZZ1STORE = ZZ1
      DO 2000 MYLOOP = 1,2
       DO 2000 MYLOOP2 = 3,4
        FUNKBEST = 1.0E20
        AA1 = 0.005*EXP(0.1d0*MYLOOP)
        ZZ1 = -0.2 +0.07*MYLOOP2
        X(1) = log(0.01)
        SSBEST = 1.0E20
        XBEST = 0
2099    CALL FIT22(X,SS,1)
        X = XBEST
        SS = FUNK(X)
        IF (ABS(SS-SSBEST).GT.1.0E-5) THEN
         SSBEST = SS
         GOTO 2099
        ENDIF
        AMSYR2 = EXP(XBEST(1))
        AMSYL2 = MSYLOUT(OPTF)
        WRITE (31,610) OPTF,AA1,ZZ1,AMSYR2,AMSYL2
2000  CONTINUE
      OPTF = STOREOPTF
      AA1 = AA1STORE
      ZZ1 = ZZ1STORE
      CLOSE (31)
C
      RETURN
609   FORMAT(1x,F9.5,1x,F9.5,1x,3(F9.5,1x))
610   FORMAT(1x,I2,1x,2(F10.5,1x),2(F15.10,1x))
600   FORMAT(1x,3(F6.3,1x))
701   FORMAT(2x,F10.8)
      END
C
C ===================================================================================================================================
C
      SUBROUTINE MODELPROJ(FF)
C
C This subroutine projects forward under a given F
C
      IMPLICIT NONE
C
      COMMON /HITFIT/ FMSY,SMSYL,RFTOT0,RFEXP0,
     +       FMSYL,FECMSY,MSYLT1,MSYLT0,MSYLT2,AMSYR1,AMSYL1,
     +       AMSYR0,AMSYL0,AMSYR2,AMSYL2,OPTDD,OPTF,OPTMSYL,OPTDK,OPTDT
      DOUBLE PRECISION FMSY,SMSYL,RFTOT0,RFEXP0,FMSYL,FECMSY
      DOUBLE PRECISION AMSYR0,AMSYL0,AMSYR1,AMSYL1,AMSYR2,AMSYL2
      DOUBLE PRECISION MSYLT0,MSYLT1,MSYLT2
      INTEGER OPTDD,OPTF,OPTMSYL,OPTDK,OPTDT

      COMMON /AGEPAR/ MAT1,MSIG,REC1,RSIG,FEC,SUR(0:60),UNREC(0:60),
     +                REC(0:60),RECF(0:60),FMATUR(0:60),MAXAGE,MINMAT
      DOUBLE PRECISION MAT1,MSIG,REC1,RSIG,FEC,SUR,UNREC,REC,RECF,FMATUR
      INTEGER MAXAGE,MINMAT

      COMMON /MORTPASS / MORT1A,MORT2A,MORTIPA
      DOUBLE PRECISION MORT1A,MORT2A
      INTEGER MORTIPA

      COMMON / DENSPASS / AA0,AA1,ZZ0,ZZ1,FMSYPASS,MSYLPASS,
     +     CATCHOUT,MSYLOUT,DERIV,MSYLPRED,XBEST,FUNKBEST,DTYPE,PHASE
      DOUBLE PRECISION AA0,AA1,ZZ0,ZZ1,DERIV,MSYLPRED
      DOUBLE PRECISION FMSYPASS,MSYLPASS,CATCHOUT,MSYLOUT(0:2)
      DOUBLE PRECISION XBEST(100),FUNKBEST
      INTEGER DTYPE,PHASE
C
C     GLOBAL VARIABLES
      DOUBLE PRECISION FF
C
C     LOCAL VARIABLES
      DOUBLE PRECISION PA,RMAT,RREC,R1PLUS,SURVF
      DOUBLE PRECISION DENST
      DOUBLE PRECISION UNRC(0:2000,0:200), RC(0:2000,0:200)
      DOUBLE PRECISION CATCH,DEPL
      DOUBLE PRECISION NEXP(0:2000),NMAT(0:2000),NTOT(0:2000)
      INTEGER L,IYR
      REAL*8 SURV
      EXTERNAL SURV
C
C     Set up recruitment ogive RECF: knife edge at age 1  {Eqn A4.1}
      CALL SETO (RECF,RSIG,REC1,MAXAGE)
C
C     Set up the survival vector (unfished(
      DO 8 L = 0,MAXAGE
        SUR(L) = SURV(MORT1A,MORT2A,L,MORTIPA,1.0d0,1.0d0,1.0d0)
    8 CONTINUE
C
C *** Add the relative mature & recruited pristine population sizes starting
C     with unity in the zero age class (L=0)  (PA=relative no.in Lth age class)
      PA = 1
      RMAT = 0
      RREC = 0
      R1PLUS = 0
      DO 9 L = 0,MAXAGE-1
        UNRC(0,L) = PA*(1.0-RECF(L))
        RC(0,L) = PA*RECF(L)
        RMAT = RMAT + PA*FMATUR(L)
        RREC = RREC + PA*RECF(L)
        IF (L.GE.1) R1PLUS = R1PLUS + PA
        PA   = PA*SUR(L)
    9 CONTINUE
      PA   = PA/(1.D0 - SUR(MAXAGE))
      RMAT = RMAT + PA
      RREC = RREC + PA
      R1PLUS = R1PLUS + PA
      UNRC(0,MAXAGE) = PA*(1.0-RECF(MAXAGE))
      RC(0,MAXAGE) = PA*RECF(MAXAGE)
C
C     Calculate mature, total1+ and recruited totals
      NMAT(0) = 0.D0
      NTOT(0) = 0.D0
      NEXP(0) = 0.D0
      DO 1000 L = 1,MAXAGE
        NTOT(0) = NTOT(0) + RC(0,L) + UNRC(0,L)
        NEXP(0) = NEXP(0) + RC(0,L)
        NMAT(0) = NMAT(0) + (RC(0,L) + UNRC(0,L))*FMATUR(L)
 1000  CONTINUE
      DEPL = 1.0

C     Set the birth rate FEC so as to give balance at equilibrium
C     FEC = # of age 0 / # mature in pristine population
      FEC = 1.d0 / RMAT
C
C     Set up the recruitment ogive in transition form:
C     set RECF =fraction of unrecruited animals of age A which recruit
C     at age A+1, except RECF(0) = fraction recruited of age 0
      CALL SETO  (RECF,RSIG,REC1,MAXAGE)
      CALL TRFORM(RECF,MAXAGE)
C
C     Survival from fishing
      SURVF = 1.0 - FF
C
C  Set up equilibrium population age structure under F
      DO 10000 IYR = 1,1000
C
C      Set survival
       DO 10100 L = 0,MAXAGE
        SUR(L) = SURV(MORT1A,MORT2A,L,MORTIPA,AA1,ZZ1,DEPL)
10100  CONTINUE

       CATCH = 0
       IF (OPTF.EQ.0) THEN
        DO 11100 L = 1,MAXAGE-1
          UNRC(IYR,L) = SUR(L-1)*UNRC(IYR-1,L-1)*(1.D0 - RECF(L))
          RC(IYR,L)   = SUR(L-1)*(RC(IYR-1,L-1)*SURVF +
     +                            UNRC(IYR-1,L-1)*RECF(L))
          CATCH = CATCH + RC(IYR-1,L-1)*FF
11100   CONTINUE
       ENDIF
       IF (OPTF.EQ.1) THEN
        UNRC(IYR,1) = 0
        RC(IYR,1) =  SUR(0)*UNRC(IYR-1,0)
        DO 11200 L = 2,MAXAGE-1
         UNRC(IYR,L) = 0
         RC(IYR,L)   = SUR(L-1)*RC(IYR-1,L-1)*SURVF
         CATCH = CATCH + RC(IYR-1,L-1)*FF
11200   CONTINUE
       ENDIF
       IF (OPTF.EQ.2) THEN
        DO 11300 L = 1,MAXAGE-1
         SURVF = 1.D0 - FF*FMATUR(L-1)
         UNRC(IYR,L) = SUR(L-1)*SURVF*
     +                 UNRC(IYR-1,L-1)*(1.D0 - RECF(L))
         RC(IYR,L)   = SUR(L-1)*SURVF*
     +                 (RC(IYR-1,L-1) + UNRC(IYR-1,L-1)*RECF(L))
         CATCH = CATCH + (UNRC(IYR-1,L-1)+RC(IYR-1,L-1))*
     +              FF*FMATUR(L-1)
11300   CONTINUE
       ENDIF
       SURVF = 1.D0 - FF
       UNRC(IYR,MAXAGE) = 0
       RC(IYR,MAXAGE) = RC(IYR-1,MAXAGE-1)*SUR(MAXAGE-1)*SURVF+
     +                  RC(IYR-1,MAXAGE)*SUR(MAXAGE)*SURVF
       CATCH = CATCH + (RC(IYR-1,MAXAGE) + RC(IYR-1,MAXAGE-1))*FF
C
C      Calculate mature, total1+ and recruited totals
       NMAT(IYR) = 0.D0
       NTOT(IYR) = 0.D0
       NEXP(IYR) = 0.D0
       DO 12000 L = 1,MAXAGE
         NTOT(IYR) = NTOT(IYR) + RC(IYR,L) + UNRC(IYR,L)
         NEXP(IYR) = NEXP(IYR) + RC(IYR,L)
         NMAT(IYR) = NMAT(IYR) + (RC(IYR,L) + UNRC(IYR,L))*FMATUR(L)
12000  CONTINUE
C
C      Find density-dependent component
       IF (OPTDD.EQ.0) THEN
        DEPL = NEXP(IYR)/NEXP(0)
       ENDIF
       IF (OPTDD.EQ.1) THEN
        DEPL = NTOT(IYR)/NTOT(0)
       ENDIF
       IF (OPTDD.EQ.2) THEN
        DEPL = NMAT(IYR)/NMAT(0)
       ENDIF
       IF (OPTDT.EQ.0) THEN
        IF (DEPL.LT.0) THEN
         DENST = 0
        ELSE
         DENST = AA0*(1.0d0-DEPL**ZZ0)
        ENDIF
       ELSE
        DENST = 0.0d0
       ENDIF
C
C      ADD Recruitment
       UNRC(IYR,0) = FEC*NMAT(IYR)*(1.0d0+DENST)
       RC(IYR,0)   = 0.0
C
10000 CONTINUE
      CATCHOUT = CATCH
C
      MSYLOUT(0) = NEXP(999)/NEXP(0)
      MSYLOUT(1) = NTOT(999)/NTOT(0)
      MSYLOUT(2) = NMAT(999)/NMAT(0)
C
      RETURN
600   FORMAT(1x,I3,F8.4,1x,F8.4,1x,F8.4)
      END
C
C ========================================================================================
C
      DOUBLE PRECISION FUNCTION FUNK(X)
C
      IMPLICIT NONE

      COMMON /HITFIT/ FMSY,SMSYL,RFTOT0,RFEXP0,
     +       FMSYL,FECMSY,MSYLT1,MSYLT0,MSYLT2,AMSYR1,AMSYL1,
     +       AMSYR0,AMSYL0,AMSYR2,AMSYL2,OPTDD,OPTF,OPTMSYL,OPTDK,OPTDT
      DOUBLE PRECISION FMSY,SMSYL,RFTOT0,RFEXP0,FMSYL,FECMSY
      DOUBLE PRECISION AMSYR0,AMSYL0,AMSYR1,AMSYL1,AMSYR2,AMSYL2
      DOUBLE PRECISION MSYLT0,MSYLT1,MSYLT2
      INTEGER OPTDD,OPTF,OPTMSYL,OPTDK,OPTDT

      COMMON / DENSPASS / AA0,AA1,ZZ0,ZZ1,FMSYPASS,MSYLPASS,
     +     CATCHOUT,MSYLOUT,DERIV,MSYLPRED,XBEST,FUNKBEST,DTYPE,PHASE
      DOUBLE PRECISION AA0,AA1,ZZ0,ZZ1,DERIV,MSYLPRED
      DOUBLE PRECISION FMSYPASS,MSYLPASS,CATCHOUT,MSYLOUT(0:2)
      DOUBLE PRECISION XBEST(100),FUNKBEST
      INTEGER DTYPE,PHASE
C
C     GLOBAL VARIABLES
      DOUBLE PRECISION X(100)
C
C     LOCAL VARIABLES
      DOUBLE PRECISION DF,CATCH1,CATCH2,FF
C
C     Set tolerances
      DF = 0.0001D0
C
C     Objective is to match MSYL and derivative equals 0 give FF
      IF (DTYPE.EQ.1) THEN
       AA0 = 0
       ZZ0 = 1
       AA1 = 0
       ZZ1 = 1
       IF (OPTDT.EQ.0) THEN
C        AA0 = EXP(X(1))
C        ZZ0 = EXP(X(2))
        AA0 = X(1)
        ZZ0 = X(2)
       ENDIF
       IF (OPTDT.EQ.1) THEN
        AA1 = EXP(X(1))
        ZZ1 = EXP(X(2))
C        AA1 = X(1)
C        ZZ1 = X(2)
       ENDIF
C
C      Derivative
       CALL MODELPROJ(FMSYPASS+DF)
       CATCH1 = CATCHOUT
       CALL MODELPROJ(FMSYPASS-DF)
       CATCH2 = CATCHOUT
       DERIV = (CATCH1-CATCH2)/(2*DF)
C
C      MSYL
       CALL MODELPROJ(FMSYPASS)
       MSYLPRED = MSYLOUT(OPTMSYL)
C
C      Objective function
       IF (PHASE.EQ.1) THEN
        FUNK = ABS(DERIV)*0.01+(MSYLPRED-MSYLPASS)**2.0
       ENDIF
       IF (PHASE.EQ.2) THEN
        FUNK = ABS(DERIV)*100+(MSYLPRED-MSYLPASS)**2.0
       ENDIF
       IF (PHASE.EQ.3) THEN
        FUNK = ABS(DERIV)+(MSYLPRED-MSYLPASS)**2.0
        WRITE(*,111) DERIV, FUNK, " MSYL(pred,T) ", MSYLPRED, MSYLPASS
  111 FORMAT("Phase3: Deriv ",F13.8," FUNK ",F10.7,A,F8.5,1X,F8.5)
       ENDIF
C
      ENDIF
C
C     Objective function is to maximize catch
      IF (DTYPE.EQ.2) THEN
       FF = EXP(X(1))
       CALL MODELPROJ(FF)
       FUNK = -CATCHOUT*1000
      ENDIF
C
      IF (FUNK.LT.FUNKBEST) THEN
       FUNKBEST = FUNK
       XBEST = X
      ENDIF
C
      RETURN
600   FORMAT(1x,5(F8.4,1x,F8.4,1x))
      END
C
C ===========================================================================
C
      SUBROUTINE FIT22(X,SS,NDIM)

C     Set up the parameters for a fit

C     IMPLICIT NONE
      REAL*8 P(101,100),Y(101),X(100),SS,FUNK,TOL,GRD
      INTEGER*4 NDIM,I,J,ITER
      EXTERNAL FUNK

C     SET UP TOLERANCES AND GRIDDING
      TOL = 0.00000001
      GRD = 1.2

      DO 1 I=1,NDIM+1
         DO 2 J=1,NDIM
            P(I,J)=X(J)
            IF((I-1).EQ.J) P(I,J) = GRD*P(I,J)
2        CONTINUE
1     CONTINUE

      DO 3 I=1,NDIM+1
         DO 4 J=1,NDIM
            X(J)=P(I,J)
4        CONTINUE
         Y(I)=FUNK(X)
3     CONTINUE

      CALL AMOEBA(P,Y,101,100,NDIM,TOL,ITER)

      DO 5 J = 1,NDIM
         X(J) = P(1,J)
5     CONTINUE
      SS = Y(1)

      RETURN
      END

      SUBROUTINE AMOEBA(P,Y,MP,NP,NDIM,FTOL,ITER)
C
C     MULTIDIMENSIONAL MINIMISATION OF THE FUNCTION FUNK(X) WHERE X IS
C     AN NDIM-DIMENSIONAL VECTOR, BY THE DOWNHILL SIMPLEX METHOD OF
C     NELDER AND MEAD. INPUT IS A MATRIX P WHOSE NDIM+1 ROWS ARE THE
C     NDIM-DIMENSIONAL VECTORS WHICH ARE THE VERTICES OF THE STARTING
C     SIMPLEX. [LOGICAL DIMENSIONS OF P ARE P(NDIM+1,NDIM); PHYSICAL
C     DIMENSIONS ARE INPUT AS P(MP,NP).] ALSO INPUT IS THE VECTOR Y
C     OF LENGTH NDIM+1, WHOSE COMPONENTS MUST BE PRE-INITIALISED TO
C     THE VALUES OF FUNK EVALUATED AT THE NDIM+1 VERTICES (ROWS) OF P;
C     AND FTOL IS THE FRACTIONAL CONVERGENCE TOLERANCE TO BE ACHIEVED
C     IN THE FUNCTION VALUE (N.B.!). ON OUTPUT, P AND Y WILL HAVE BEEN
C     RESET TO NDIM+1 NEW POINTS ALL WITHIN FTOL OF A MINIMUM FUNCTION
C     VALUE, AND ITER GIVES THE NUMBER OF ITERATIONS TAKEN.
C
C     FROM: NUMERICAL RECIPES - THE ART OF SCIENTIFIC COMPUTING
C           BY W. H. PRESS ET AL, CAMBRIDGE UNIVERSITY PRESS
C           ISBN 0-251-30811-9
C
C     ********************************************************************
C

C     SPECIFY THE MAXIMUM NUMBER OF DIMENSIONS, THREE PARAMETERS WHICH
C     DEFINE THE EXPANSIONS AND CONTRACTIONS, AND THE MAXIMUM NUMBER OF
C     ITERATIONS ALLOWED

C     IMPLICIT NONE
      REAL*8 ALPHA,BETA,GAMMA,EPS
      INTEGER*4 ITMAX
      PARAMETER (ALPHA=1.0,BETA=0.5,GAMMA=2.0,ITMAX=500,EPS=1.0E-10)

C     Global Data
      INTEGER*4 MP,NP,NDIM,ITER
      REAL*8 P(MP,NP),Y(MP),PR(100),PRR(100),PBAR(100),FTOL,FUNK
      EXTERNAL FUNK

C     Local Data
      REAL*8 YPR,YPRR
      INTEGER*4 I,ILO,IHI,INHI,J,MPTS

C     NOTE THAT MP IS THE PHYSICAL DIMENSION CORRESPONDING TO THE LOGICAL
C     DIMENSION MPTS, NP TO NDIM.

      MPTS = NDIM+1
      ITER=0

C     FIRST WE MUST DETERMINE WHICH POINT IS THE HIGHEST (WORST), NEXT
C     HIGHEST, AND LOWEST (BEST).

1     ILO=1
      IF(Y(1).GT.Y(2)) THEN
         IHI=1
         INHI=2
      ELSE
         IHI=2
         INHI=1
      ENDIF
      DO 11 I=1,MPTS
         IF(Y(I).LT.Y(ILO)) ILO=I
         IF(Y(I).GT.Y(IHI)) THEN
            INHI=IHI
            IHI=I
         ELSE IF(Y(I).GT.Y(INHI)) THEN
            IF(I.NE.IHI) INHI=I
         ENDIF
11    CONTINUE

C     COMPUTE THE FRACTIONAL RANGE FROM THE HIGHEST TO THE LOWEST AND
C     RETURN IF SATISFACTORY

      IF(2.0*ABS(Y(IHI)-Y(ILO)).LT.FTOL*(ABS(Y(IHI))+ABS(Y(ILO))+EPS))
     +   RETURN
      IF(ITER.EQ.ITMAX) THEN
c        WRITE(6,200)
200      FORMAT(1H ,'AMOEBA EXCEEDING MAXIMUM ITERATIONS')
         RETURN
      ENDIF
      ITER=ITER+1

      DO 12 J=1,NDIM
         PBAR(J)=0.
12    CONTINUE

C     BEGIN A NEW ITERATION. COMPUTE THE VECTOR AVERAGE OF ALL POINTS
C     EXCEPT THE HIGHEST, I.E. THE CENTRE OF THE "FACE" OF THE SIMPLEX
C     ACROSS FROM THE HIGH POINT. WE WILL SUBSEQUENTLY EXPLORE ALONG
C     THE RAY FROM THE HIGH POINT THROUGH THE CENTRE.

      DO 14 I=1,MPTS
         IF(I.NE.IHI) THEN
            DO 13 J=1,NDIM
               PBAR(J)=PBAR(J)+P(I,J)
13          CONTINUE
         ENDIF
14    CONTINUE

C     EXTRAPOLATE BY A FACTOR ALPHA THROUGH THE FACE, I.E. REFLECT THE
C     SIMPLEX FROM THE HIGH POINT

      DO 15 J=1,NDIM
         PBAR(J)=PBAR(J)/NDIM
         PR(J)=(1.+ALPHA)*PBAR(J)-ALPHA*P(IHI,J)
15    CONTINUE

C     EVALUATE THE FUNCTION AT THE REFLECTED POINT

      YPR=FUNK(PR)

C     GIVES A BETTER RESULT THAN THE BEST POINT, SO TRY AN ADDITIONAL
C     EXTRAPOLATION BY A FACTOR GAMMA

      IF(YPR.LE.Y(ILO)) THEN
         DO 16 J=1,NDIM
            PRR(J)=GAMMA*PR(J)+(1.-GAMMA)*PBAR(J)
16       CONTINUE

C        CHECK THE FUNCTION THERE

         YPRR=FUNK(PRR)

C        THE ADDITIONAL EXTRAPOLATION SUCCEEDED, AND REPLACES THE
C        HIGHEST POINT

         IF(YPRR.LT.Y(ILO)) THEN
            DO 17 J=1,NDIM
               P(IHI,J)=PRR(J)
17          CONTINUE
            Y(IHI)=YPRR
         ELSE

C        THE ADDITIONAL EXTRAPOLATION FAILED, BUT WE CAN STILL USE THE
C        REFLECTED POINT

         DO 18 J=1,NDIM
            P(IHI,J)=PR(J)
18       CONTINUE
         Y(IHI)=YPR
      ENDIF

C     THE REFLECTED POINT IS WORSE THAN THE SECOND HIGHEST

      ELSE IF(YPR.GE.Y(INHI)) THEN

C        IF IT'S BETTER THAN THE HIGHEST, THEN REPLACE THE HIGHEST

         IF(YPR.LT.Y(IHI)) THEN
            DO 19 J=1,NDIM
               P(IHI,J)=PR(J)
19          CONTINUE
            Y(IHI)=YPR
         ENDIF

C        BUT LOOK FOR AN INTERMEDIATE LOWER POINT; IN OTHER WORDS
C        PERFORM A CONTRACTION OF THE SIMPLEX ALONG ONE DIMENSION
C        AND THEN EVALUATE THE FUNCTION

         DO 21 J=1,NDIM
            PRR(J)=BETA*P(IHI,J)+(1.-BETA)*PBAR(J)
21       CONTINUE
         YPRR=FUNK(PRR)

C        CONTRACTION GIVES AN IMPROVEMENT, SO ACCEPT IT

         IF(YPRR.LT.Y(IHI)) THEN
            DO 22 J=1,NDIM
               P(IHI,J)=PRR(J)
22          CONTINUE
            Y(IHI)=YPRR
         ELSE

C           CAN'T SEEM TO GET RID OF THAT HIGH POINT. BETTER CONTRACT
C           AROUND THE LOWEST (BEST) POINT

            DO 24 I=1,MPTS
               IF(I.NE.ILO) THEN
                  DO 23 J=1,NDIM
                     PR(J)=0.5*(P(I,J)+P(ILO,J))
                     P(I,J)=PR(J)
23                CONTINUE
                  Y(I)=FUNK(PR)
               ENDIF
24          CONTINUE
         ENDIF

      ELSE

C        WE ARRIVE HERE IF THE ORIGINAL REFLECTION GIVES A MIDDLING
C        POINT. REPLACE THE OLD HIGH POINT AND CONTINUE

         DO 25 J=1,NDIM
            P(IHI,J)=PR(J)
25       CONTINUE
         Y(IHI)=YPR

      ENDIF

      GO TO 1

      END

C -----------------------------------------------------------------------------


