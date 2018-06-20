C                                                            <HTML><PRE>
C     PROGRAM  A N R A Y, VERSION 4.73  (PRAHA, JUNE 2013)
C!! modificações marcadas com !! em fev/2017 por Liliana Alcazar
C*******************************************************************
C
C     PROGRAM ANRAY IS DESIGNED FOR RAY, TRAVEL TIME AND
C     AMPLITUDE COMPUTATIONS IN 3D GENERAL ANISOTROPIC AND ISOTROPIC
C     LATERALLY VARYING LAYERED MEDIA. THE PROGRAM MAKES POSSIBLE
C     COMPUTATION OF RAYS SPECIFIED BY INITIAL ANGLES AT THE SOURCE,
C     I.E., INITIAL-VALUE RAY TRACING, OR RAYS STARTING FROM THE
C     SOURCE AND TERMINATING ON A VERTICAL OR SURFACE PROFILE, I.E.
C     BOUNDARY-VALUE RAY TRACING. RAY AMPLITUDES CAN BE COMPUTED
C     ALONG RAYS.
C
C*******************************************************************
C
C
      CHARACTER*80 MTEXT,FILEIN,FILEOU,FILE1,FILE2,FILE3,FILE4,FILE5
      CHARACTER*80 FILE6,FILE7
      DIMENSION Y(18)
      COMMON /AUXI/  IANI(20),INTR,INT1,IPREC,KRE,IREFR,LAY,NDER,IPRINT,
     1 MPRINT,NTR,ISQRT,NAUX,ISOUR,MAUX,MREG,MDIM,IPOL,MSCON,LOUT,
     2 IAMP,MTRNS,ICOEF,IAD,IRHO,ISHEAR,IAC,IRT,mori
      COMMON /AUXX/  MMX(20),MMY(20),MMXY(20)
      COMMON /APROX/ A11,A12,A13,A14,A15,A16,A22,A23,A24,A25,A26,A33,
     1               A34,A35,A36,A44,A45,A46,A55,A56,A66,
     1               DXA11,DXA12,DXA13,DXA14,DXA15,DXA16,DXA22,DXA23,
     1               DXA24,DXA25,DXA26,DXA33,DXA34,DXA35,DXA36,DXA44,
     1               DXA45,DXA46,DXA55,DXA56,DXA66,
     1               DYA11,DYA12,DYA13,DYA14,DYA15,DYA16,DYA22,DYA23,
     1               DYA24,DYA25,DYA26,DYA33,DYA34,DYA35,DYA36,DYA44,
     1               DYA45,DYA46,DYA55,DYA56,DYA66,
     1               DZA11,DZA12,DZA13,DZA14,DZA15,DZA16,DZA22,DZA23,
     1               DZA24,DZA25,DZA26,DZA33,DZA34,DZA35,DZA36,DZA44,
     1               DZA45,DZA46,DZA55,DZA56,DZA66,
     1               A2546,A1266,A1355,A1456,A3645,A2344
      INTEGER CODE
      COMMON /COD/  CODE(50,2),KREF,KC,ITYPE
      COMMON /DIST/ DST(200),NDST,REPS,PROF(2),NDSTP,PREPS,LNDST,
     1XPRF,YPRF,ILOC
      COMMON /DENS/ RHO(20)
      COMPLEX PS
      COMMON /RAY/   AY(28,2000),DS(20,50),KINT(50),HHH(3,3),TMAX,
     1               PS(3,7,50),IS(8,50),N,IREF,IND,IND1
      COMMON /INTRF/ Z(1000),SX(350),SY(350),NX(20),NY(20),BRD(6),NINT,
     1   XINTA
      COMMON /ZERO/ RNULL
      COMMON/VSP/XVSP,YVSP,XNRM,YNRM,ICOD,IVSP
      COMMON/VRML/LUBRD,LUGRD,LUIND,LURAY
C
C**************************************************
C
      LIN=5
      LOU=6
      LU1=1
      LU2=2
      LU3=3
      LUBRD=7
      LUGRD=8
      LUIND=9
      LURAY=10
      FILEIN='anray.dat'
      FILEOU='anray.out'
      FILE1='lu1.anray'		!!
      FILE2='lu2.anray'		!!
      FILE3=' '
      FILE4=' '
      FILE5=' '
      FILE6=' '
      FILE7=' '
C!!      WRITE(*,'(2A)') ' (ANRAY) SPECIFY NAMES OF INPUT AND OUTPUT',
C!!     1' FILES LIN, LOU, LU1, LU2, LU3, LUBRD, LUGRD, LUIND, LURAY: '
C!!      READ(*,*) FILEIN,FILEOU,FILE1,FILE2,FILE3,FILE4,FILE5,FILE6,FILE7
	WRITE(*,*) 'SPECIFY NAME OF INPUT MODEL FILE (in a anray format)' !!
	READ(*,*) FILEIN	!!

      IF(FILE1.EQ.' ') LU1=0
      IF(FILE2.EQ.' ') LU2=0
      IF(FILE3.EQ.' ') LU3=0
      IF(FILE4.EQ.' ') LUBRD=0
      IF(FILE5.EQ.' ') LUGRD=0
      IF(FILE6.EQ.' ') LUIND=0
      IF(FILE7.EQ.' ') LURAY=0
      LOUT=LOU
      OPEN(LIN,FILE=FILEIN,FORM='FORMATTED',STATUS='OLD')
      OPEN(LOU,FILE=FILEOU,FORM='FORMATTED')
      IF(LU1.NE.0)OPEN(LU1,FILE=FILE1,FORM='FORMATTED')
      IF(LU2.NE.0)OPEN(LU2,FILE=FILE2,FORM='FORMATTED')
      IF(LU3.NE.0)OPEN(LU3,FILE=FILE3,FORM='FORMATTED')
      IF(LUBRD.NE.0)OPEN(LUBRD,FILE=FILE4,FORM='FORMATTED')
      IF(LUGRD.NE.0)OPEN(LUGRD,FILE=FILE5,FORM='FORMATTED')
      IF(LUIND.NE.0)OPEN(LUIND,FILE=FILE6,FORM='FORMATTED')
      IF(LURAY.NE.0)OPEN(LURAY,FILE=FILE7,FORM='FORMATTED')
C
C**************************************************
C
      WRITE(LOU,777)
 777  FORMAT(///,'***********************'
     1,//,'  PROGRAM   A N R A Y      ',//,
     2'***********************',//)
      NCODE=1
      MTEXT='ANRAY'
      INUL=4
      READ(LIN,*)MTEXT
      WRITE(LOU,115)MTEXT
      READ(LIN,*)INULL,ISURF
      IF(INULL.EQ.0)INULL=INUL
      RNULL=10.**(-INULL)
      WRITE(LOU,106)INULL,ISURF
C
C
C     SPECIFICATION OF THE MODEL
C
      CALL MODEL(MTEXT,LIN)
C
C     GENERATE FILE FOR PLOTTING VARIOUS CHARACTERISTIC SURFACES
C
      IF(LU3.NE.0)CALL SURFPL(LIN,LU3)
C
C     GENERATE FILE FOR VRML PLOTTING BOUNDARIES OF THE MODEL
C
      IF(LUBRD.NE.0)CALL BOX(BRD)
C
C     GENERATE FILE FOR PLOTTING RAYS
C
      IF(LURAY.NE.0)WRITE(LURAY,113)
      IF(LURAY.NE.0)WRITE(LURAY,105)
C
C     SPECIFICATION OF SYNTHETIC SEISMOGRAMS
C
    2 ICONT=1
      MEP=0
      MOUT=0
      MDIM=0
      METHOD=0
      MREG=0
      ITMAX=10
      IPOL=0
      IPREC=0
      IRAYPL=0
      IPRINT=0
      IAMP=0
      MTRNS=0
      ICOEF=0
      IRT=0
      ILOC=0
      MCOD=0
      MORI=0
      READ(LIN,*)ICONT,MEP,MOUT,MDIM,METHOD,MREG,ITMAX,
     1IPOL,IPREC,IRAYPL,IPRINT,IAMP,MTRNS,ICOEF,IRT,ILOC,MCOD,MORI
      WRITE(LOU,102)ICONT,MEP,MOUT,MDIM,METHOD,MREG,ITMAX,
     1IPOL,IPREC,IRAYPL,IPRINT,IAMP,MTRNS,ICOEF,IRT,ILOC,MCOD,MORI
      IF(ICONT.EQ.0)GO TO 99
C
C
c      IF(MEP.NE.0.AND.MDIM.EQ.0)MDIM=1
      IVSP=0
      IF(ILOC.EQ.0)ITPR=3
      IF(ILOC.EQ.1)THEN
        IVSP=1
        ITPR=43
        MREG=1
      END IF
      IF(ILOC.GT.1)THEN
        ITPR=ILOC+100
      END IF
C
      IF(MEP.EQ.0)THEN
        NDST=0
      END IF
C
      IF(MEP.EQ.1)THEN
        NDST=1
        READ(LIN,*)XREC,YREC
        WRITE(LOU,104)XREC,YREC
        GO TO 4
      END IF
      IF(MEP.LT.0)THEN
        NDST=-MEP
        PROF(1)=0.
        XPRF=0.
        YPRF=0.
        READ(LIN,*)PROF(1),(DST(I),I=1,NDST),XPRF,YPRF
        WRITE(LOU,104)PROF(1),(DST(I),I=1,NDST),XPRF,YPRF
        IF(NDST.EQ.1)RSTEP=1.
        IF(NDST.EQ.1)DST(2)=DST(1)+1.
        IF(NDST.EQ.1)GO TO 4
        RSTEP=(DST(NDST)-DST(1))/FLOAT(NDST-1)
      END IF
C
      IF(MEP.GT.0)THEN
        NDST=MEP
        READ(LIN,*)PROF(1),RMIN,RSTEP,XPRF,YPRF
        WRITE(LOU,104)PROF(1),RMIN,RSTEP,XPRF,YPRF
        DO 13 I=1,MEP
   13   DST(I)=RMIN+(I-1)*RSTEP
        IF(NDST.EQ.1)DST(2)=RMIN+RSTEP
      END IF
      PROF(2)=PROF(1)+1.
      NDSTP=1
C
      IF(IVSP.EQ.1.AND.NDST.NE.0)THEN
        READ(LIN,*)XVSP,YVSP
        WRITE(LOU,104)XVSP,YVSP
      END IF
C
    4 TSOUR=0.
      DT=1.
      AC=0.0001
      REPS=0.05
      PREPS=0.05
      READ(LIN,*)XSOUR,YSOUR,ZSOUR,TSOUR,DT,AC,REPS,PREPS
      WRITE(LOU,104)XSOUR,YSOUR,ZSOUR,TSOUR,DT,AC,REPS,PREPS
C
      IF(ABS(XPRF).LT..000001.AND.ABS(YPRF).LT..000001)THEN
        XPRF=XSOUR
        YPRF=YSOUR
      END IF
      IF(MEP.EQ.1)THEN
        XE=XREC-XPRF
        YE=YREC-YPRF
        RPRF=SQRT(XE*XE+YE*YE)
        XATAN=ATAN2(YE,XE)
        PROF(1)=XATAN
        RMIN=RPRF
        DST(1)=RMIN
        WRITE(LOU,104)RPRF,XATAN
        RSTEP=100.
        DST(2)=DST(1)+100.
        PROF(2)=PROF(1)+1.
        NDSTP=1
      END IF
C
      IF(IVSP.EQ.1.AND.NDST.NE.0)THEN
        XNRM=XVSP-XSOUR
        YNRM=YVSP-YSOUR
        AUX=SQRT(XNRM*XNRM+YNRM*YNRM)
        XNRM=XNRM/AUX
        YNRM=YNRM/AUX
        PROF(1)=ATAN2(YNRM,XNRM)
        PROF(2)=PROF(1)+1.
        XPRF=XSOUR
        YPRF=YSOUR
      END IF
      IF(MCOD.EQ.0)THEN
        READ(LIN,*)AMIN,ASTEP,AMAX
        WRITE(LOU,104)AMIN,ASTEP,AMAX
        READ(LIN,*)BMIN,BSTEP,BMAX
        IF(ABS(BSTEP).LT..000001)THEN
          BMIN=PROF(1)-.3
          BMAX=PROF(1)+.4
          BSTEP=.6
        END IF
        WRITE(LOU,104)BMIN,BSTEP,BMAX
      END IF
      IF((MREG.EQ.0.OR.MREG.EQ.2).AND.MDIM.NE.0) WRITE(LOU,'(/,A,/)')
     1 ' COEFFICIENTS OF CONVERSION ARE APPLIED'
      IF((MREG.NE.0.AND.MREG.NE.2).AND.MDIM.NE.0) WRITE(LOU,'(/,A,/)')
     1 ' COEFFICIENTS OF CONVERSION ARE *** NOT *** APPLIED'
      TMAX=10000.
      IND=-1
      NDER=1
      CALL RAYA(XSOUR,YSOUR,ZSOUR,TSOUR,AMIN1,BMIN,PX,PY,PZ,XX,YY,ZZ,T,
     1DT,AC)
      Y(1)=XSOUR
      Y(2)=YSOUR
      Y(3)=ZSOUR
      IF(IND.EQ.50)WRITE(LOU,111)IND
      IF(IND.EQ.50)GO TO 99
      LAY=IND
      ISOUR=IND
      ITYPE=3
      CALL PARDIS(Y,0)
      VP=SQRT(A11)
      IF(IRHO.EQ.0)RO=1.7+.2*VP
      IF(IRHO.EQ.1)RO=RHO(IND)
C
C     GENERATE FILE LU2 FOR SYNTHETIC SEISMOGRAM COMPUTATIONS
C
      IF(LU2.NE.0.AND.NDST.NE.0)THEN
        WRITE(LU2,115)MTEXT
        KSH=2
        WRITE(LU2,100)NDST,KSH,ILOC
        WRITE(LU2,104)XSOUR,YSOUR,ZSOUR,TSOUR,RSTEP,RO
        IF(MEP.NE.1)WRITE(LU2,104)(DST(I),I=1,NDST)
        IF(MEP.EQ.1)WRITE(LU2,104)XREC,YREC,RPRF,XATAN
      END IF
C
C    LOOP FOR ELEMENTARY WAVES
C
   20 READ(LIN,*)KC,KREF,((CODE(I,K),K=1,2),I=1,KREF)
      WRITE(LOU,100)KC,KREF,((CODE(I,K),K=1,2),I=1,KREF)
      IF(KREF.EQ.0)GOTO 2
      IF(MOUT.NE.0)WRITE(LOU,107)
      WRITE(LOU,103)NCODE,KC,KREF,((CODE(I,K),K=1,2),I=1,KREF)
C
      IF(MCOD.NE.0)THEN
        READ(LIN,*)AMIN,ASTEP,AMAX
        WRITE(LOU,104)AMIN,ASTEP,AMAX
        READ(LIN,*)BMIN,BSTEP,BMAX
        IF(ABS(BSTEP).LT..000001)THEN
          BMIN=PROF(1)-.3
          BMAX=PROF(1)+.4
          BSTEP=.6
        END IF
        WRITE(LOU,104)BMIN,BSTEP,BMAX
      END IF
C
C     GENERATE FILE LU1 FOR PLOTTING OF RAY DIAGRAMS,
C     TIME-DISTANCE AND AMPLITUDE-DISTANCE CURVES
C
      IF(LU1.EQ.0.OR.NDST.EQ.0)GO TO 21
      WRITE(LU1,100)ICONT,NDST,ILOC
      WRITE(LU1,104)RO
      NPN=2
      APN=0.
      WRITE(LU1,100)NPN,NPN,NPN
      WRITE(LU1,101)APN,APN,APN,APN,APN
      WRITE(LU1,101)APN,APN,APN,APN,APN
      WRITE(LU1,104)Xprf,Yprf,0.0,PROF(1)
      WRITE(LU1,104)(DST(I),I=1,NDST)
   21 CONTINUE
C
C
C     SEARCH FOR THE NUMBER OF THE ELEMENT OF THE RAY, STARTING FROM
C     WHICH THE WAVE DOES UNDERTAKE NEITHER REFLECTION NOR CONVERSION
C
      ICOD=0
      IF(IVSP.EQ.0)GO TO 35
      DO 34 I=1,KREF
      ICOD=KREF-I+1
      IF(ICOD.EQ.1) GO TO 34
      IC1=CODE(ICOD,1)
      IC2=CODE(ICOD-1,1)
      IF((IC1-IC2).EQ.0)GO TO 35
      IC1=CODE(ICOD,2)
      IC2=CODE(ICOD-1,2)
      IF((IC1-IC2).NE.0)GO TO 35
   34 CONTINUE
   35 CONTINUE
      IF(MOUT.NE.0)WRITE(LOU,108)
C
C
      CALL RECEIV(XSOUR,YSOUR,ZSOUR,TSOUR,DT,AC,ITMAX,AMIN,ASTEP,
     1AMAX,BMIN,BSTEP,BMAX,MOUT,LU1,LU2,METHOD,ITPR,NCODE)
      IF(IND.EQ.14) WRITE(LOU,111) IND
      NCODE=NCODE+1
      GOTO 20
C
C     END OF LOOP FOR ELEMENTARY WAVES
C
C
  100 FORMAT(26I3)
  101 FORMAT(5E15.5)
  102 FORMAT(1H0,////,2X,26I3)
  103 FORMAT(4X,I4,9X,100I3)
  104 FORMAT(8F10.5)
  105 FORMAT('/')
  106 FORMAT(17I5)
  107 FORMAT(//2X,'INT.CODE',5X,'E X T E R N A L   C O D E')
  108 FORMAT(//)
  111 FORMAT(/2X,'IND=',I5,/)
  113 FORMAT(6H'RAYS')
  115 FORMAT(A)
C
   99 CONTINUE
      IF(LURAY.NE.0)WRITE(LURAY,105)
      IF(LU1.NE.0.AND.NDST.NE.0)WRITE(LU1,100)ICONT,ICONT
      IF(LU1.NE.0)REWIND LU1
      IF(LU2.NE.0)REWIND LU2
C
      STOP
      END
C
C     *********************************************************
C
      SUBROUTINE AMPL (AMPX,AMPY,AMPZ,UU)
C
C     ROUTINE FOR COMPUTING COMPLEX VECTORIAL RAY AMPLITUDES
C
C     OUTPUT PARAMETERS
C     AMPX(2),AMPY(2),AMPZ(2) - X,Y AND Z COMPONENTS OF COMPLEX
C     VECTORIAL RAY AMPLITUDES IN THE MODEL COORDINATES. FOR P WAVE
C     IN ANY MEDIUM AND FOR S WAVES IN AN ANISOTROPIC MEDIUM, I=1.
C     FOR S WAVE GENERATED IN AN ISOTROPIC MEDIUM, I=1,2. I=1 AND 2
C     CORRESPOND TO S WAVES SPECIFIED AT THE SOURCE BY VECTORS E1
C     E2. VECTORS E1 AND E2 TOGETHER WITH UNIT VECTOR TANGENT TO
C     THE RAY FORM A BASIS OF RAY CENTRED COORDINATE SYSTEM.
C     UU - PRODUCT OF RATIOS OF DENSITIES AND COSINES OF INCIDENCE
C     AND OF REFLECTION/TRANSMISSION AT POINTS WHERE THE RAY CROSSES
c     INTERFACES.
C
C     CALLED FROM: RECEIV
C     ROUTINES CALLED: POLAR,TRANSL,COEF
C
      DIMENSION Y(18),UN(3),POLD(3),PNEW(3)
      COMPLEX  AMPX(2),AMPY(2),AMPZ(2),CR(3),UC(3),STU(6),C1,C2,C3
      COMMON /AUXI/  IANI(20),INTR,INT1,IPREC,KRE,IREFR,LAY,NDER,IPRINT,
     1 MPRINT,NTR,ISQRT,NAUX,ISOUR,MAUX,MREG,MDIM,IPOL,MSCON,LOU,
     2 IAMP,MTRNS,ICOEF,IAD,IRHO,ISHEAR,IAC,IRT,mori
      COMMON /DIST/ DST(200),NDST,REPS,PROF(2),NDSTP,PREPS,LNDST,
     1XPRF,YPRF,ILOC
      INTEGER CODE
      COMMON /COD/  CODE(50,2),KREF,KC,ITYPE
      COMMON /DENS/ RHO(20)
      COMPLEX PS
      COMMON /RAY/   AY(28,2000),DS(20,50),KINT(50),HHH(3,3),tmax,
     1               PS(3,7,50),IS(8,50),N,IREF,IND,IND1
      COMMON /RAY2/  DRY(3,2000)
C
      KSS=1
      ISHEAR=0
      ITYPE=CODE(1,2)
      IF(IANI(ISOUR).EQ.0.AND.ITYPE.NE.3)THEN
        ISHEAR=1
        ITYPE=1
      END IF
      ITP=ITYPE
      DO 1 I=1,2
      AMPX(I)=CMPLX(0.,0.)
      AMPY(I)=CMPLX(0.,0.)
      AMPZ(I)=CMPLX(0.,0.)
    1 CONTINUE
C
 3000 NN=N
      IDD=0
      N2=0
      N1=1
      IRE=IREF
      AV=1.
C
C  SPECIFICATION OF DISPLACEMENT VECTOR AT SOURCE
C  IN RAY CENTERED COORDINATES
C
      DO 5 I=1,3
      CR(I)=(0.,0.)
    5 CONTINUE
      CR(ITP)=(1.,0.)
      IREF1=IREF-1
      IF(IRE.GT.1)INAUM=CODE(IRE-1,1)-CODE(IRE,1)
      IF(MREG.GE.1.AND.IRE.GT.1.AND.ILOC.GT.1.AND.INAUM.GE.0)THEN
        IREF1=IREF1+1
        CODE(IREF1+1,2)=3
      END IF
      IF(IREF1.EQ.0) GOTO 100
C
C  LOOP OVER INTERFACES
C
      DO 10 I=1,IREF1
      IREF=I
      IF(KC.NE.0) ITYPE=CODE(IREF,2)
      N=KINT(IREF)
      IF(N.EQ.0) THEN
        IDD=1
        GO TO 10
      ELSE
        N1=N2+1
        N2=N
        IF(IDD.NE.0) N2=-N2
        IDD=0
C
C     COMPUTATION OF POLARIZATION VECTORS
C     CONSIDERED POLARIZATION VECTOR(S) ARE STORED IN CORRESPONDING
C     COLUMNS OF THE MATRIX HHH. OTHER COLUMNS ARE ZERO.
C
        CALL POLAR(N1,N2,NN,IREF)
      END IF
      DO 20 K=1,6
      Y(K)=AY(K+1,N)
  20  CONTINUE
      IF(IAMP.GT.0)WRITE(LOU,'(a,2i5,6f10.5)')' AMPL:I,N,Y',I,N,
     1(Y(L),L=1,6)
      DO 30 K=1,3
      POLD(K)=Y(K+3)
      PS(K,7,IREF)=Y(K+3)
  30  CONTINUE
      DO 40 K=1,3
      UN(K)=DS(K,IREF)
  40  CONTINUE
      LAY=IS(1,IREF)
      ITRANS=IS(2,IREF)
      ITR1=ITRANS
      IF(UN(3).GT.0.0) GOTO 50
C
C  RAY STRIKING THE INTERFACE FROM ABOVE
C
      IF(ITRANS.EQ.0) THEN
        LAY=LAY+1
        ITRANS=1
        GOTO 70
      END IF
      IF(ITRANS.GT.0) THEN
        LAY=LAY-1
        ITRANS=0
        GOTO 70
      END IF
C
C  RAY STRIKING THE INTERFACE FROM BELOW
C
  50  IF(ITRANS.EQ.0) THEN
        LAY=LAY-1
        ITRANS=1
        GOTO 70
      END IF
      IF(ITRANS.GT.0) THEN
        LAY=LAY+1
        ITRANS=0
        GOTO 70
      END IF
C
C  SLOWNESS VECTORS ON THE SIDE OF THE INTERFACE WHERE GENERATED
C  WAVE PROPAGATES WERE DETERMINED DURING THE CALL OF TRANSL IN THE
C  ROUTINE OUT. HERE REMAINING SLOWNESS VECTORS ON THE OTHER SIDE
C  OF THE INTERFACE ARE DETERMINED
C
C  REDEFINITION OF IREF FOR CALL OF ROUTINE TRANSL
C
  70  IF(LAY.EQ.0) THEN
        DO 71 K=4,6
        DO 71 L=1,3
        PS(L,K,IREF)=CMPLX(0.,0.)
  71    CONTINUE
        GO TO 75
      END IF
      IREF=IREF+1
      CALL TRANSL(Y,POLD,PNEW,UN,ITRANS,0)
      IF(IND.EQ.10)RETURN
      IREF=IREF-1
  75  IF(IAMP.NE.0)THEN
        WRITE(LOU,'(A)')' REFLECTED/TRANSMITTED SLOWNESS VECTORS'
        WRITE(LOU,'(6F12.6)')((PS(L,K,IREF),L=1,3),K=1,6)
      END IF
      AV1=(DS(11,IREF)*DS(10,IREF))/(DS(8,IREF)*DS(7,IREF))
      AV=AV*AV1
      IF(IAMP.GT.0) THEN
        WRITE(LOU,'(A)') 'ROI,ROG,UNVGI,UNVGG,AV1,AV'
        WRITE(LOU,'(6F10.5)') DS(8,IREF),
     1  DS(11,IREF),DS(7,IREF),DS(10,IREF),AV1,AV
        WRITE(LOU,'(A,/,6F12.5,/,3(3F12.5/))') ' CR,HHH',
     2  CR,((HHH(J,K),J=1,3),K=1,3)
      END IF
C
C  COMPUTATION OF AMPLITUDE COEFFICIENTS OF REFLECTED/TRANSMITTED WAVES
C
C
C  COMPUTATION OF CARTESIAN COMPONENTS OF INCIDENT DISPLACEMENT VECTOR
C
      DO 87 K=1,3
      STU(K)=CMPLX(0.,0.)
      DO 87 J=1,3
      STU(K)=HHH(J,K)*CR(J)+STU(K)
  87  CONTINUE
      IF(IAMP.GT.0)WRITE(LOU,'(A,6F10.5)') ' STU',(STU(K),K=1,3)
      IF(KC.NE.0)ITYPE=CODE(IREF+1,2)
      IF(MREG.GE.1.AND.IRE.GT.1.AND.I.EQ.IREF1.AND.ILOC.GT.1.AND.
     1(CODE(IRE,1).LE.CODE(IRE-1,1)))ITR1=1
      CALL COEF(STU,CR,ITR1)
      IF(IND.EQ.11)RETURN
      BCR=SQRT(REAL(CR(1)*CONJG(CR(1))+CR(2)*CONJG(CR(2))
     1             +CR(3)*CONJG(CR(3))))
      IF(BCR.LT.1.E-10) THEN
        DO 88 K=1,3
        UC(K)=(0.,0.)
  88    CONTINUE
        GOTO 130
      END IF
  10  CONTINUE
C
C  END OF LOOP OVER INTERFACES
C
C  TERMINATION POINT
C
  100 CONTINUE
      IF(IRE.GT.1)INAUM=CODE(IRE-1,1)-CODE(IRE,1)
      IF((MREG.GE.1.AND.IRE.GT.1).AND.ILOC.GT.1.AND.INAUM.GE.0)THEN
        DO 200 K=1,3
        Y(K+3)=REAL(PS(K,6,IREF1))
 200    CONTINUE
        V=1./SQRT(Y(4)*Y(4)+Y(5)*Y(5)+Y(6)*Y(6))
        DO 201 K=1,3
        HHH(1,K)=0.
        HHH(2,K)=0.
        HHH(3,K)=V*Y(K+3)
 201    CONTINUE
      ELSE
        N1=N2+1
        N2=NN
        IF(KC.NE.0)ITYPE=CODE(IRE,2)
        IF(KC.NE.0)IS(7,IRE)=CODE(IRE,1)
        CALL POLAR(N1,N2,NN,IRE)
      END IF
C
C COMPUTATION OF CARTESIAN COMPONENTS OF INCIDENT DISPLACEMENT VECTOR
C
      DO 107 K=1,3
      STU(K)=CMPLX(0.,0.)
      DO 107 J=1,3
      STU(K)=HHH(J,K)*CR(J)+STU(K)
 107  CONTINUE
      IF(IAMP.GT.0)WRITE(LOU,'(A,6F10.5)') ' STU',(STU(K),K=1,3)
C
      IF(IRE.GT.1)INAUM=CODE(IRE-1,1)-CODE(IRE,1)
      IF(MREG.EQ.1.OR.MREG.EQ.3.OR.
     1(MREG.EQ.2.AND.IRE.GT.1.AND.INAUM.GE.0))THEN
        UC(1)=STU(1)
        UC(2)=STU(2)
        UC(3)=STU(3)
        IF(MREG.GT.1) THEN
C
C     CALCULATION OF PRESSURE AT THE TERMINATION POINT
C
          C1=UC(1)
          C2=UC(2)
          C3=UC(3)
          ARE=REAL(C1)
          IF(ARE.LT.0.)ARE=-ARE
          AIM=AIMAG(C1)
          APHI=ATAN2(AIM,ARE)
          ARE=SQRT(REAL(C1*CONJG(C1)+C2*CONJG(C2)+C3*CONJG(C3)))
          UC(1)=ARE*CMPLX(COS(APHI),SIN(APHI))
          UC(2)=(0.,0.)
          UC(3)=(0.,0.)
          IF(IAMP.GT.0)WRITE(LOU,'(A,4F10.5)') ' UC(1),ARE,APHI',
     1    UC(1),ARE,APHI
        END IF
        GOTO 110
      END IF
      DO 105 K=1,6
      Y(K)=AY(K+1,NN)
      IF(K.LE.3)GO TO 105
      PS(K-3,7,IRE)=Y(K)
      POLD(K-3)=Y(K)
      UN(K-3)=DS(K-3,IRE)
  105 CONTINUE
      N=NN
      IF(MREG.EQ.0.OR.MREG.EQ.2) THEN
        IREF=IREF+1
        IF(INTR.EQ.LAY)LAY=LAY-1
        IF(INTR.NE.LAY)LAY=LAY+1
        CALL TRANSL(Y,POLD,PNEW,UN,1,0)
      END IF
      IREF=IRE
      IF(IAMP.GT.0)THEN
        WRITE(LOU,'(A)')
     1  ' REFLECTED SLOWNESS VECTORS AT TERMINATION POINT'
        WRITE(LOU,'(6F12.6)')((PS(L,K,IRE),L=1,3),K=1,3)
      END IF
C
C     COMPUTATION OF CONVERSION COEFFICIENTS
C
      KTR=999
      CALL COEF(STU,UC,KTR)
      IF(IND.EQ.11)RETURN
  110 CONTINUE
      DO 115 K=1,3
      Y(K)=AY(K+4,NN)
  115 CONTINUE
      VPEND=1./SQRT(Y(1)*Y(1)+Y(2)*Y(2)+Y(3)*Y(3))
      IF(IRE.GT.1)INAUM=CODE(IRE-1,1)-CODE(IRE,1)
      IF((MREG.GE.1.AND.IRE.GT.1).AND.ILOC.GT.1.AND.
     1INAUM.GE.0)VPEND=V
      DO 120 K=1,3
      Y(K)=AY(K+4,1)
  120 CONTINUE
      VP0=1./SQRT(Y(1)*Y(1)+Y(2)*Y(2)+Y(3)*Y(3))
      RHO0=0.2*SQRT(AY(8,1))+1.7
      IF(IRHO.NE.0) RHO0=RHO(ISOUR)
      RHEND=0.2*SQRT(AY(8,NN))+1.7
      IF(IRHO.NE.0) RHEND=RHO(LAY)
      AV=AV*VP0*RHO0
      AV=AV/(VPEND*RHEND)
      UU=SQRT(ABS(AV))
      IF(IAMP.GT.0)
     1WRITE(LOU,'(A,4F12.6)')'VP0,RH0,VPEND,RHEND',VP0,RHO0,VPEND,RHEND
 130  CONTINUE
      N=NN
      IREF=IRE
      AMPX(KSS)=UC(1)
      AMPY(KSS)=UC(2)
      AMPZ(KSS)=UC(3)
      IF(MREG.GT.1)AMPX(KSS)=AMPX(KSS)*VPEND*RHEND
      IF(ISHEAR.NE.0.AND.KSS.NE.2) THEN
        KSS=2
        ITP=2
        GOTO 3000
      END IF
      RETURN
      END
C
C     *********************************************************
C
      SUBROUTINE APPROX(X,Y,YD,KDIM)
C
C     THE ROUTINE PERFORMS THIRD-ORDER INTERPOLATION BETWEEN POINTS
C     YOLD AND YNEW PARAMETERIZED BY AN INDEPENDENT VARIABLE X.
C     DOLD, DNEW ARE THE FIRST DERIVATIVES OF Y WITH RESPECT
C     TO X AT THE POINTS YOLD AND YNEW.
C
      DIMENSION Y(18),YD(18)
      COMMON/APPR/ XOLD,XNEW,YOLD(18),DOLD(18),YNEW(18),DNEW(18)
C
      A=(X-XNEW)/(XNEW-XOLD)
      AUX=A+1.
      A1=(2.*A+3.)*A*A
      A2=1.-A1
      B1=AUX*A*(X-XNEW)
      B2=AUX*A*(X-XOLD)
      AD1=6.*A*AUX/(XNEW-XOLD)
      AD2=-AD1
      BD1=A*(3.*A+2.)
      BD2=AUX*(3.*A+1.)
      DO 1 I=1,KDIM
      Y(I)=A1*YOLD(I)+A2*YNEW(I)+B1*DOLD(I)+B2*DNEW(I)
      YD(I)=AD1*YOLD(I)+AD2*YNEW(I)+BD1*DOLD(I)+BD2*DNEW(I)
    1 CONTINUE
      RETURN
      END
C
C     *********************************************************
C
      SUBROUTINE BIAP(MX1,MX,MY1,MY,MXY1)
C
      DIMENSION X(200),FX(200),V(1000)
      COMMON/ZCOEF/ A02(1000),A20(1000),A22(1000)
      COMMON /INTRF/ Z(1000),SX(350),SY(350),NX(20),NY(20),BRD(6),NINT,
     1   XINTA
      EQUIVALENCE(Z(1),V(1))
C
C     ROUTINE DETERMINING THE COEFFICIENTS
C     OF BICUBIC SPLINE INTERPOLATION
C
      DO 1 J=1,MX
      L=MX1+J-1
    1 X(J)=SX(L)
      DO 3 I=1,MY
      DO 2 J=1,MX
      K=MXY1+(J-1)*MY+I-1
    2 FX(J)=V(K)
      CALL SPLIN(X,FX,1,MX)
      DO 3 J=1,MX
      K=MXY1+(J-1)*MY+I-1
    3 A20(K)=FX(J)
C
      DO 4 I=1,MY
      L=MY1+I-1
    4 X(I)=SY(L)
      DO 6 J=1,MX
      DO 5 I=1,MY
      K=MXY1+(J-1)*MY+I-1
    5 FX(I)=V(K)
      CALL SPLIN(X,FX,1,MY)
      DO 6 I=1,MY
      K=MXY1+(J-1)*MY+I-1
    6 A02(K)=FX(I)
C
      DO 7 J=1,MX
      L=MX1+J-1
    7 X(J)=SX(L)
      DO 9 I=1,MY
      DO 8 J=1,MX
      K=MXY1+(J-1)*MY+I-1
    8 FX(J)=A02(K)
      CALL SPLIN(X,FX,1,MX)
      DO 9 J=1,MX
      K=MXY1+(J-1)*MY+I-1
    9 A22(K)=FX(J)
C
      RETURN
      END
C
C
C     *********************************************************
C
      SUBROUTINE CHRM(Y)
C
C  ROUTINE FOR THE COMPUTATION OF THE ELEMENTS OF THE CHRISTOFFEL
C  MATRIX FOR AN ARBITRARY ANISOTROPIC MEDIUM
C
      DIMENSION Y(18)
      COMMON /APROX/ A11,A12,A13,A14,A15,A16,A22,A23,A24,A25,A26,A33,
     1               A34,A35,A36,A44,A45,A46,A55,A56,A66,
     1               DXA11,DXA12,DXA13,DXA14,DXA15,DXA16,DXA22,DXA23,
     1               DXA24,DXA25,DXA26,DXA33,DXA34,DXA35,DXA36,DXA44,
     1               DXA45,DXA46,DXA55,DXA56,DXA66,
     1               DYA11,DYA12,DYA13,DYA14,DYA15,DYA16,DYA22,DYA23,
     1               DYA24,DYA25,DYA26,DYA33,DYA34,DYA35,DYA36,DYA44,
     1               DYA45,DYA46,DYA55,DYA56,DYA66,
     1               DZA11,DZA12,DZA13,DZA14,DZA15,DZA16,DZA22,DZA23,
     1               DZA24,DZA25,DZA26,DZA33,DZA34,DZA35,DZA36,DZA44,
     1               DZA45,DZA46,DZA55,DZA56,DZA66,
     1               A2546,A1266,A1355,A1456,A3645,A2344
      COMPLEX PS
      COMMON /RAY/   AY(28,2000),DS(20,50),KINT(50),HHH(3,3),tmax,
     1               PS(3,7,50),IS(8,50),N,IREF,IND,IND1
      COMMON /AUXI/  IANI(20),INTR,INT1,IPREC,KRE,IREFR,LAY,NDER,IPRINT,
     1 MPRINT,NTR,ISQRT,NAUX,ISOUR,MAUX,MREG,MDIM,IPOL,MSCON,LOUT,
     2 IAMP,MTRNS,ICOEF,IAD,IRHO,ISHEAR,IAC,IRT,mori
      INTEGER CODE
      COMMON /COD/  CODE(50,2),KREF,KC,ITYPE
      COMMON /DJK/  D11,D12,D13,D22,D23,D33,DTR
      COMMON /GAM/  C11,C12,C13,C22,C23,C33
C
      P1=Y(4)
      P2=Y(5)
      P3=Y(6)
      P2P3=P2*P3
      P1P2=P1*P2
      P1P3=P1*P3
      P1P1=P1*P1
      P2P2=P2*P2
      P3P3=P3*P3
      C11=P1P1*A11+P2P2*A66+P3P3*A55
     1+2.*(P2P3*A56+P1P3*A15+P1P2*A16)
      C22=P1P1*A66+P2P2*A22+P3P3*A44
     1+2.*(P2P3*A24+P1P3*A46+P1P2*A26)
      C33=P1P1*A55+P2P2*A44+P3P3*A33
     1+2.*(P2P3*A34+P1P3*A35+P1P2*A45)
      C23=P1P1*A56+P2P2*A24+P3P3*A34
     1   +P2P3*A2344+P1P3*A3645+P1P2*A2546
      C13=P1P1*A15+P2P2*A46+P3P3*A35
     1   +P2P3*A3645+P1P3*A1355+P1P2*A1456
      C12=P1P1*A16+P2P2*A26+P3P3*A45
     1   +P2P3*A2546+P1P3*A1456+P1P2*A1266
      C11N=C11-1.
      C22N=C22-1.
      C33N=C33-1.
      C23SQ=C23*C23
      C13SQ=C13*C13
      C12SQ=C12*C12
      D11=C22N*C33N-C23SQ
      D22=C11N*C33N-C13SQ
      D33=C11N*C22N-C12SQ
      D12=C13*C23-C12*C33N
      D13=C12*C23-C13*C22N
      D23=C12*C13-C23*C11N
      DTR=D11+D22+D33
      IF(ABS(DTR).LT.0.0000001)THEN
        WRITE(LOUT,'(A)')'CHRM: SHEAR WAVE SINGULARITY'
        IND=10
      END IF
      RETURN
      END
C
C     *********************************************************
C
      SUBROUTINE CHRM1(C,PN,UN)
C
C  ROUTINE FOR THE COMPUTATION OF THE ELEMENTS OF THE CHRISTOFFEL
C  MATRIX FOR AN ARBITRARY ANISOTROPIC MEDIUM
C
      DIMENSION C(3,3),PN(3),UN(3)
      COMMON /APROX/ A11,A12,A13,A14,A15,A16,A22,A23,A24,A25,A26,A33,
     1               A34,A35,A36,A44,A45,A46,A55,A56,A66,
     1               DXA11,DXA12,DXA13,DXA14,DXA15,DXA16,DXA22,DXA23,
     1               DXA24,DXA25,DXA26,DXA33,DXA34,DXA35,DXA36,DXA44,
     1               DXA45,DXA46,DXA55,DXA56,DXA66,
     1               DYA11,DYA12,DYA13,DYA14,DYA15,DYA16,DYA22,DYA23,
     1               DYA24,DYA25,DYA26,DYA33,DYA34,DYA35,DYA36,DYA44,
     1               DYA45,DYA46,DYA55,DYA56,DYA66,
     1               DZA11,DZA12,DZA13,DZA14,DZA15,DZA16,DZA22,DZA23,
     1               DZA24,DZA25,DZA26,DZA33,DZA34,DZA35,DZA36,DZA44,
     1               DZA45,DZA46,DZA55,DZA56,DZA66,
     1               A2546,A1266,A1355,A1456,A3645,A2344
C
      P1=PN(1)
      P2=PN(2)
      P3=PN(3)
      U1=UN(1)
      U2=UN(2)
      U3=UN(3)
      P2U3=P2*U3
      P3U2=P3*U2
      P1U2=P1*U2
      P2U1=P2*U1
      P1U3=P1*U3
      P3U1=P3*U1
      P1U1=P1*U1
      P2U2=P2*U2
      P3U3=P3*U3
      C(1,1)=P1U1*A11+P2U2*A66+P3U3*A55
     1+(P2U3+P3U2)*A56+(P1U3+P3U1)*A15+(P1U2+P2U1)*A16
      C(2,2)=P1U1*A66+P2U2*A22+P3U3*A44
     1+(P2U3+P3U2)*A24+(P1U3+P3U1)*A46+(P1U2+P2U1)*A26
      C(3,3)=P1U1*A55+P2U2*A44+P3U3*A33
     1+(P2U3+P3U2)*A34+(P1U3+P3U1)*A35+(P1U2+P2U1)*A45
      C(2,3)=P1U1*A56+P2U2*A24+P3U3*A34
     1+0.5*((P2U3+P3U2)*A2344+(P1U3+P3U1)*A3645+(P1U2+P2U1)*A2546)
      C(1,3)=P1U1*A15+P2U2*A46+P3U3*A35
     1+0.5*((P2U3+P3U2)*A3645+(P1U3+P3U1)*A1355+(P1U2+P2U1)*A1456)
      C(1,2)=P1U1*A16+P2U2*A26+P3U3*A45
     1+0.5*((P2U3+P3U2)*A2546+(P1U3+P3U1)*A1456+(P1U2+P2U1)*A1266)
      C(2,1)=C(1,2)
      C(3,2)=C(2,3)
      C(3,1)=C(1,3)
      RETURN
      END
C
C     *********************************************************
C
      SUBROUTINE CHRM2(Y,G,i)
C
C     EVALUATES ELEMENTS OF THE CHRISTOFFEL MATRIX
C
      DIMENSION a(21),Y(18),G(3,3)
      COMMON/GAM/G11,G12,G13,G22,G23,G33
      COMMON /APROX1/ e(21,10)
      COMMON /AUXI/  IANI(20),INTR,INT1,IPREC,KRE,IREFR,LAY,NDER,IPRINT,
     1 MPRINT,NTR,ISQRT,NAUX,ISOUR,MAUX,MREG,MDIM,IPOL,MSCON,LOUT,
     2 IAMP,MTRNS,ICOEF,IAD,IRHO,ISHEAR,IAC,IRT,mori
C
      DO 1 J=1,21
      A(J)=E(J,I)
    1 CONTINUE
      P1=Y(4)
      P2=Y(5)
      P3=Y(6)
      P11=P1*P1
      P12=P1*P2
      P13=P1*P3
      P22=P2*P2
      P23=P2*P3
      P33=P3*P3
      G11=A(1)*P11+A(21)*P22+A(19)*P33+
     1    2.*(A(6)*P12+A(5)*P13+A(20)*P23)
      G22=A(21)*P11+A(7)*P22+A(16)*P33+
     1    2.*(A(11)*P12+A(18)*P13+A(9)*P23)
      G33=A(19)*P11+A(16)*P22+A(12)*P33+
     1    2.*(A(17)*P12+A(14)*P13+A(13)*P23)
      G12=A(6)*P11+A(11)*P22+A(17)*P33+
     1    (A(21)+A(2))*P12+(A(20)+A(4))*P13+(A(10)+A(18))*P23
      G13=A(5)*P11+A(18)*P22+A(14)*P33+
     1    (A(20)+A(4))*P12+(A(19)+A(3))*P13+(A(17)+A(15))*P23
      G23=A(20)*P11+A(9)*P22+A(13)*P33+
     1    (A(10)+A(18))*P12+(A(17)+A(15))*P13+(A(16)+A(8))*P23
      G(1,1)=G11
      G(1,2)=G12
      G(1,3)=G13
      G(2,1)=G12
      G(2,2)=G22
      G(2,3)=G23
      G(3,1)=G13
      G(3,2)=G23
      G(3,3)=G33
      RETURN
      END
C
C     *********************************************************
C
      SUBROUTINE PCHRM(Y,G,L,I)
C
C     EVALUATES FIRST DERIVATIVES OF ELEMENTS OF CHRISTOFFEL MATRIX
C     WITH RESPECT TO THE L-TH COMPONENT OF THE SLOWNESS VECTOR
C
      DIMENSION A(21),Y(18),G(3,3)
      COMMON /APROX1/ E(21,10)
C
      DO 1 J=1,21
      A(J)=E(J,I)
    1 CONTINUE
      P1=Y(4)
      P2=Y(5)
      P3=Y(6)
      IF(L.EQ.1)THEN
        G(1,1)=2.*(A(1)*P1+A(6)*P2+A(5)*P3)
        G(2,2)=2.*(A(21)*P1+A(11)*P2+A(18)*P3)
        G(3,3)=2.*(A(19)*P1+A(17)*P2+A(14)*P3)
        AUX=2.*A(6)*P1+(A(21)+A(2))*P2+(A(20)+A(4))*P3
        G(1,2)=AUX
        G(2,1)=AUX
        AUX=2.*A(5)*P1+(A(20)+A(4))*P2+(A(19)+A(3))*P3
        G(1,3)=AUX
        G(3,1)=AUX
        AUX=2.*A(20)*P1+(A(10)+A(18))*P2+(A(17)+A(15))*P3
        G(2,3)=AUX
        G(3,2)=AUX
      END IF
      IF(L.EQ.2)THEN
        G(1,1)=2.*(A(6)*P1+A(21)*P2+A(20)*P3)
        G(2,2)=2.*(A(11)*P1+A(7)*P2+A(9)*P3)
        G(3,3)=2.*(A(17)*P1+A(16)*P2+A(13)*P3)
        AUX=2.*A(11)*P2+(A(21)+A(2))*P1+(A(10)+A(18))*P3
        G(1,2)=AUX
        G(2,1)=AUX
        AUX=2.*A(18)*P2+(A(20)+A(4))*P1+(A(17)+A(15))*P3
        G(1,3)=AUX
        G(3,1)=AUX
        AUX=2.*A(9)*P2+(A(10)+A(18))*P1+(A(16)+A(8))*P3
        G(2,3)=AUX
        G(3,2)=AUX
      END IF
      IF(L.EQ.3)THEN
        G(1,1)=2.*(A(5)*P1+A(20)*P2+A(19)*P3)
        G(2,2)=2.*(A(18)*P1+A(9)*P2+A(16)*P3)
        G(3,3)=2.*(A(14)*P1+A(13)*P2+A(12)*P3)
        AUX=2.*A(17)*P3+(A(20)+A(4))*P1+(A(10)+A(18))*P2
        G(1,2)=AUX
        G(2,1)=AUX
        AUX=2.*A(14)*P3+(A(19)+A(3))*P1+(A(17)+A(15))*P2
        G(1,3)=AUX
        G(3,1)=AUX
        AUX=2.*A(13)*P3+(A(17)+A(15))*P1+(A(16)+A(8))*P2
        G(2,3)=AUX
        G(3,2)=AUX
      END IF
      RETURN
      END
C
C     *********************************************************
C
      SUBROUTINE PPCHRM(G,L,M,i)
C
C     EVALUATES SECOND DERIVATIVES OF ELEMENTS OF CHRISTOFFEL MATRIX
C     WITH RESPECT TO THE L-TH AND M-TH COMPONENTS OF THE SLOWNESS
C     VECTOR
C
      DIMENSION a(21),G(3,3)
      COMMON /APROX1/ e(21,10)
C
      do 1 j=1,21
      a(j)=e(j,i)
    1 continue
      IF(L.EQ.1.AND.M.EQ.1)THEN
        G(1,1)=2.*A(1)
        G(2,2)=2.*A(21)
        G(3,3)=2.*A(19)
        AUX=2.*A(6)
        G(1,2)=AUX
        G(2,1)=AUX
        AUX=2.*A(5)
        G(1,3)=AUX
        G(3,1)=AUX
        AUX=2.*A(20)
        G(2,3)=AUX
        G(3,2)=AUX
      END IF
      IF(L.EQ.2.AND.M.EQ.2)THEN
        G(1,1)=2.*A(21)
        G(2,2)=2.*A(7)
        G(3,3)=2.*A(16)
        AUX=2.*A(11)
        G(1,2)=AUX
        G(2,1)=AUX
        AUX=2.*A(18)
        G(1,3)=AUX
        G(3,1)=AUX
        AUX=2.*A(9)
        G(2,3)=AUX
        G(3,2)=AUX
      END IF
      IF(L.EQ.3.AND.M.EQ.3)THEN
        G(1,1)=2.*A(19)
        G(2,2)=2.*A(16)
        G(3,3)=2.*A(12)
        AUX=2.*A(17)
        G(1,2)=AUX
        G(2,1)=AUX
        AUX=2.*A(14)
        G(1,3)=AUX
        G(3,1)=AUX
        AUX=2.*A(13)
        G(2,3)=AUX
        G(3,2)=AUX
      END IF
      IF((L.EQ.1.AND.M.EQ.2).OR.(L.EQ.2.AND.M.EQ.1))THEN
        G(1,1)=2.*A(6)
        G(2,2)=2.*A(11)
        G(3,3)=2.*A(17)
        AUX=A(21)+A(2)
        G(1,2)=AUX
        G(2,1)=AUX
        AUX=A(20)+A(4)
        G(1,3)=AUX
        G(3,1)=AUX
        AUX=A(10)+A(18)
        G(2,3)=AUX
        G(3,2)=AUX
      END IF
      IF((L.EQ.1.AND.M.EQ.3).OR.(L.EQ.3.AND.M.EQ.1))THEN
        G(1,1)=2.*A(5)
        G(2,2)=2.*A(18)
        G(3,3)=2.*A(14)
        AUX=A(20)+A(4)
        G(1,2)=AUX
        G(2,1)=AUX
        AUX=A(19)+A(3)
        G(1,3)=AUX
        G(3,1)=AUX
        AUX=A(17)+A(15)
        G(2,3)=AUX
        G(3,2)=AUX
      END IF
      IF((L.EQ.2.AND.M.EQ.3).OR.(L.EQ.3.AND.M.EQ.2))THEN
        G(1,1)=2.*A(20)
        G(2,2)=2.*A(9)
        G(3,3)=2.*A(13)
        AUX=A(10)+A(18)
        G(1,2)=AUX
        G(2,1)=AUX
        AUX=A(17)+A(15)
        G(1,3)=AUX
        G(3,1)=AUX
        AUX=A(16)+A(8)
        G(2,3)=AUX
        G(3,2)=AUX
      END IF
      RETURN
      END
C
C     *********************************************************
C
      SUBROUTINE FACETS(N1,N2,NSRF)
      INTEGER LU,N1,N2,NSRF
C
C Subroutine FACETS writes the index file listing the vertices of each
C tetragon covering the structural interface.  The vertices are assumed
C to be stored in a separate file, with inner loop over N1 points along
C the first horizontal axis, middle loop over N2 points along the second
C horizontal axis and outer loop over the surfaces.  The vertices are
C indexed by positive integers according to their order in the vertex
C file.
C
C Input:
C     LU...   Logical unit number connected to the output file to be
C             written by this subroutine.
C     N1...   Number of points along the first horizontal axis.
C     N2...   Number of points along the second horizontal axis.
C     NSRF... Number of interfaces.
C The input parameters are not altered.
C
C No output.
C
C Output index file with the tetragons:
C For each tetragon, a line containing I1,I2,I3,I4,/
C     I1,I2,I3,I4... Indices of the vertices of the tetragon.
C             The vertices are indexed by positive integers according to
C             their order in the respective vertex file.
C     /...    List of vertices is terminated by a slash.
C
C Date: 1999, October 4
C Coded by Ludek Klimes
C
C-----------------------------------------------------------------------
C
C     Auxiliary storage locations:
      CHARACTER*9 FORMAT
      INTEGER I1,I2,ISRF
      COMMON/VRML/LUBRD,LUGRD,LU,LURAY
C
      IF(LU.EQ.0)RETURN
C     Setting output format:
      FORMAT='(4(I0,A))'
      I1=INT(ALOG10(FLOAT(N1*N2*NSRF)+0.5))+1
      FORMAT(5:5)=CHAR(ICHAR('0')+I1)
C
C     Writing the file:
      DO 33 ISRF=0,N1*N2*(NSRF-1),N1*N2
        DO 32 I2=ISRF,ISRF+N1*(N2-2),N1
          DO 31 I1=I2+1,I2+N1-1
            WRITE(LU,FORMAT) I1,' ',I1+1,' ',I1+1+N1,' ',I1+N1,' /'
   31     CONTINUE
   32   CONTINUE
   33 CONTINUE
C
      RETURN
      END
C
C     *********************************************************
C
      SUBROUTINE BOX(BRD)
C
      DIMENSION BRD(6)
      COMMON/VRML/LUBRD,LUGRD,LUIND,LURAY
C
        WRITE(LUBRD,109)
        WRITE(LUBRD,105)
        I=1
        WRITE(LUBRD,112)I
        WRITE(LUBRD,110)BRD(1),BRD(3),BRD(5)
        WRITE(LUBRD,110)BRD(1),BRD(4),BRD(5)
        WRITE(LUBRD,110)BRD(1),BRD(4),BRD(6)
        WRITE(LUBRD,110)BRD(1),BRD(3),BRD(6)
        WRITE(LUBRD,110)BRD(1),BRD(3),BRD(5)
        WRITE(LUBRD,105)
        I=2
        WRITE(LUBRD,112)I
        WRITE(LUBRD,110)BRD(1),BRD(3),BRD(5)
        WRITE(LUBRD,110)BRD(1),BRD(3),BRD(6)
        WRITE(LUBRD,110)BRD(2),BRD(3),BRD(6)
        WRITE(LUBRD,110)BRD(2),BRD(3),BRD(5)
        WRITE(LUBRD,110)BRD(1),BRD(3),BRD(5)
        WRITE(LUBRD,105)
        I=3
        WRITE(LUBRD,112)I
        WRITE(LUBRD,110)BRD(2),BRD(3),BRD(5)
        WRITE(LUBRD,110)BRD(2),BRD(4),BRD(5)
        WRITE(LUBRD,110)BRD(2),BRD(4),BRD(6)
        WRITE(LUBRD,110)BRD(2),BRD(3),BRD(6)
        WRITE(LUBRD,110)BRD(2),BRD(3),BRD(5)
        WRITE(LUBRD,105)
        I=4
        WRITE(LUBRD,112)I
        WRITE(LUBRD,110)BRD(1),BRD(4),BRD(5)
        WRITE(LUBRD,110)BRD(1),BRD(4),BRD(6)
        WRITE(LUBRD,110)BRD(2),BRD(4),BRD(6)
        WRITE(LUBRD,110)BRD(2),BRD(4),BRD(5)
        WRITE(LUBRD,110)BRD(1),BRD(4),BRD(5)
        WRITE(LUBRD,105)
        I=1
        WRITE(LUBRD,112)I
        WRITE(LUBRD,110)BRD(1),BRD(3),BRD(5)
        WRITE(LUBRD,110)BRD(1),BRD(4),BRD(5)
        WRITE(LUBRD,110)BRD(2),BRD(4),BRD(5)
        WRITE(LUBRD,110)BRD(2),BRD(3),BRD(5)
        WRITE(LUBRD,110)BRD(1),BRD(3),BRD(5)
        WRITE(LUBRD,105)
        I=1
        WRITE(LUBRD,112)I
        WRITE(LUBRD,110)BRD(1),BRD(3),BRD(6)
        WRITE(LUBRD,110)BRD(1),BRD(4),BRD(6)
        WRITE(LUBRD,110)BRD(2),BRD(4),BRD(6)
        WRITE(LUBRD,110)BRD(2),BRD(3),BRD(6)
        WRITE(LUBRD,110)BRD(1),BRD(3),BRD(6)
        WRITE(LUBRD,105)
        WRITE(LUBRD,105)
C
  105 FORMAT('/')
  109 FORMAT(25H'BOUNDARIES OF THE MODEL')
  110 FORMAT(3(F10.5,1X),'/')
  112 FORMAT(6H'BOUND,I1,1H',1X,'/')
C
      RETURN
      END
C
C=======================================================================
C
      INCLUDE 'a2.for'
C     <A HREF="a2.for" TYPE="text/html">a2.for</A>
      INCLUDE 'a3.for'
C     <A HREF="a3.for" TYPE="text/html">a3.for</A>
      INCLUDE 'a42.for' !!
C     <A HREF="a4.for" TYPE="text/html">a4.for</A>
      INCLUDE 'a5.for'
C     <A HREF="a5.for" TYPE="text/html">a5.for</A>
C
C <A NAME="MOD"></A><FONT COLOR="RED">Interpolation method:</FONT>
C Include just one of the following files 'mod*.for':
C (a) Isosurface interpolation:
C      INCLUDE 'modis.for'
C     <A HREF="modis.for" TYPE="text/html">modis.for</A>
C (b) (Bi-)(tri-)cubic B-spline interpolation:
      INCLUDE 'modbs.for'
C     <A HREF="modbs.for" TYPE="text/html">modbs.for</A>
C
C=======================================================================
C                                                                 </PRE>
