C                                                            <HTML><PRE>
      SUBROUTINE PROFIL(XSOUR,YSOUR,ZSOUR,TSOUR,PSI0,PAZM,RANG,
     1XXX,YYY,ZZZ,TTT,DT,AC,ASTART,ASTEP,AFIN,ITMAX,MOUT,NCODE,
     2METHOD,ITPR,indr1)
C
C     3-D INITIAL VALUE RAY TRACING AND RAY TRACING FROM THE SOURCE
C     TO A PRESCRIBED PROFILE PASSING THROUGH THE EPICENTER
C
      COMMON /AUXI/  IANI(20),INTR,INT1,IPREC,KRE,IREFR,LAY,NDER,IPRINT,
     1 MPRINT,NTR,ISQRT,NAUX,ISOUR,MAUX,MREG,MDIM,IPOL,MSCON,LOU,
     2 IAMP,MTRNS,ICOEF,IAD,IRHO,ISHEAR,IAC,IRT,mori
      INTEGER CODE
      COMMON /COD/  CODE(50,2),KREF,KC,ITYPE
      COMMON /DIST/ XDST(200),NDSTX,XREPS,DST(2),NDST,REPS,LNDST,
     1XPRF,YPRF,ILOC
      COMPLEX PS
      COMMON /RAY/   AY(28,2000),DS(20,50),KINT(50),HHH(3,3),tmax,
     1               PS(3,7,50),IS(8,50),N,IREF,IND,IND1
      COMMON /ZERO/ RNULL
      COMMON/VSP/XVSP,YVSP,XNRM,YNRM,ICOD,IVSP
      COMMON/DYN/XDYN(3,3),ydyn(3,3)
      COMMON/KM/KMAH,SPROLD,TSTOLD
C
      iwave=0
      itp=code(1,2)
      RANG=0.
      XXX=0.
      ZZZ=0.
      TTT=0.
      REPS1=.05*REPS
      DD=dst(1)
      xcos=cos(dd)
      xsin=sin(dd)
      dd=0.
      X=0.
      ITER=0
      II=0
      LNDST=0
C
      AA=ASTART-ASTEP
      INDEX=0
      INUM=0
      ICLS=0
      ISUC=0
      INDS=ISOUR
C
C     LOOP IN AZIMUTH, FROM VALUE ASTART TO AFIN, WITH THE STEP
C     ASTEP
C
    1 AA=AA+ASTEP
      PNEW=AA
      IF(ASTEP.GT.0..AND.AA.GT.AFIN)GO TO 99
      IF(ASTEP.LT.0..AND.AA.LT.AFIN)GO TO 99
      IND=INDS
      NDER=1
      IF(MDIM.GE.1)NDER=2
      SPROLD=0.
      CALL RAYA(XSOUR,YSOUR,ZSOUR,TSOUR,PSI0,AA,PX,PY,PZ,XX,YY,ZZ,T,
     1DT,AC)
      NDER=1
      IF(IND.EQ.14)RETURN
      x=(yprf-yy)*xcos-(xprf-xx)*xsin
      IF(NDSTX.EQ.0)GO TO 65
      IF(IND.EQ.ITPR)XAX=X
      IF(IND.EQ.ITPR)PNEW=AA
      IF(MOUT.EQ.3)WRITE(LOU,100)IND,IND1,X,XX,YY,ZZ,T,AA,PSI0
      IF(INUM.NE.0)GO TO 2
      AOLD=AA
      IOLD=IND
      IOLD1=IND1
      XOLD=X
      TOLD=T
      INUM=1
      GO TO 1
C
C     PARAMETERS FOR THE PRECEDING RAY: AA=AOLD, X=XOLD, IND=IOLD
C     PARAMETERS FOR THE NEW RAY: AA=ANEW, X=XNEW, IND=INEW
C
    2 INEW=IND
      INEW1=IND1
      ANEW=AA
      XNEW=X
      TNEW=T
      IF(INEW.EQ.ITPR.AND.IOLD.EQ.ITPR)GO TO 21
      IF(INEW.EQ.ITPR)GO TO 50
      IF(IOLD.EQ.ITPR)GO TO 55
      IF(INEW.EQ.9.AND.IOLD.NE.9.AND.IOLD.NE.8)GO TO 30
      IF(INEW.NE.9.AND.INEW.NE.8.AND.IOLD.EQ.9)GO TO 35
      GOTO 3
   21 IF(IOLD1.NE.INEW1)then
        if(inew1.eq.indr1)go to 50
        if(iold1.eq.indr1)go to 55
      else
        GO TO 40
      end if
C
C     NO ITERATIONS, TAKE A NEW RAY IN THE LOOP
C
    3 CONTINUE
      if(isuc.eq.0)ind=0
      IF(IOLD.NE.INEW)IND=0
      IOLD=INEW
      IOLD1=INEW1
      XOLD=XNEW
      AOLD=ANEW
      TOLD=TNEW
      GO TO 1
C
C     REGULAR CASE: IOLD=3, INEW=3
C
   40 XXNEW=XNEW
      XXOLD=XOLD
      AANEW=ANEW
      AAOLD=AOLD
      TTNEW=TNEW
      TTOLD=TOLD
      IBNEW=0
      IBOLD=0
   41 IF(XXNEW.GT.XXOLD.AND.DST(2).GT.DST(1))GO TO 46
      IF(XXNEW.LT.XXOLD.AND.DST(2).LT.DST(1))GO TO 46
C
C     REGULAR CASE: XXNEW.LE.XXOLD, ITREND=-1 (REVERSE BRANCH)
C
      DX=XXOLD
      IF(IBOLD.EQ.1) DX=DX+REPS
      IF(DD.GE.DX) GO TO 3
      DX=XXNEW
      IF(IBNEW.EQ.1) DX=DX-REPS
      IF(DD.LT.DX) GOTO 3
      II=1
      GO TO 43
C
C     REGULAR CASE: XXNEW.GT.XXOLD, ITREND=1 (REGULAR BRANCH)
C
   46 continue
      DX=XXOLD
      IF(IBOLD.EQ.1) DX=DX-REPS
      IF(DD.LE.DX) GO TO 3
      DX=XXNEW
      IF(IBNEW.EQ.1) DX=DX+REPS
      IF(DD.GT.DX) GOTO 3
      II=1
   43 P1=AAOLD
      P2=AANEW
      X1=XXOLD
      X2=XXNEW
      T1=TTOLD
      T2=TTNEW
C
C     I T E R A T I O N S
C
      ITER=0
      ISIGN=1
      IPR1=0
      IPR2=0
      ISUC=0
      GO TO 61
   68 XAX=X
      PAX=PNEW
   61 ITER=ITER+1
      IF(ITER.GT.ITMAX)GO TO 80
      ISIGN=-ISIGN
      AAUX=0.5*(P1+P2)
      IF(METHOD.LE.1.AND.IND.EQ.ITPR.and.iter.gt.1)GO TO 62
      GO TO 69
   62 if(mori.eq.0)AUX=(XDYN(1,1)*xsin-XDYN(2,1)*xcos)*cos(psi0)
      if(mori.ne.0)AUX=(XDYN(1,2)*xsin-XDYN(2,2)*xcos)*cos(pnew)
      IF(ABS(AUX).LT..00001)GO TO 69
      AAUX=PNEW+(DD-X)/AUX
   69 PNEW=AAUX
   71 IND=INDS
      SPROLD=0.
      XOLD=0.
      IF(MDIM.GE.1)NDER=2
      CALL RAYA(XSOUR,YSOUR,ZSOUR,TSOUR,PSI0,PNEW,PX,PY,PZ,XX,YY,ZZ,T,
     1DT,AC)
      if(iwave.eq.1)code(1,2)=itp
      NDER=1
      XE=XX-Xprf
      YE=YY-Yprf
      RPRF=SIGN(1.,XE*XCOS+YE*XSIN)*SQRT(XE*XE+YE*YE)
      x=(yprf-yy)*xcos-(xprf-xx)*xsin
      IF(MOUT.EQ.3)WRITE(LOU,101)
     1IND,IND1,ITER,KMAH,DD,X,XX,YY,T,PNEW,PSI0
C
C     TESTING WHETHER THE RAY OF A QS WAVE DOES NOT TERMINATE
C     OUTSIDE THE RANGE IN WHICH PREVIOUS RAYS TERMINATED; IF YES,
C     A RAY OF THE OTHER QS WAVE WITH THE SAME INITIAL PARAMETERS
C     IS CALCULATED
C
      if((x-x1)*(x-x2).gt.0..and.itype.eq.1)then
        iter=iter+1
        if(iter.gt.itmax)go to 80
        code(1,2)=2
        iwave=1
        go to 71
      end if
      if((x-x1)*(x-x2).gt.0..and.itype.eq.2)then
        iter=iter+1
        if(iter.gt.itmax)go to 80
        code(1,2)=1
        iwave=1
        go to 71
      end if
      IF(ICLS.NE.0)GO TO 70
      IF(IND.NE.ITPR)P2=PNEW
      IF(IND.NE.ITPR)GO TO 61
      IF(ABS(X-XAX).LT..000001)GO TO 67
      IF(ABS(X-DD).LT.REPS)GO TO 65
      IF(X1.LT.X2.AND.DD.GT.X)GO TO 63
      IF(X1.GT.X2.AND.DD.LT.X)GO TO 63
      IF(ABS(X-X1).LT..000001)GO TO 67
      P2=PNEW
      X2=X
      T2=T
      IPR2=1
      GO TO 68
   63 IF(ABS(X-X2).LT..000001)GO TO 67
      P1=PNEW
      X1=X
      T1=T
      IPR1=1
      GO TO 68
   67 IF(ABS(PNEW-PAX).GT..000001)ITER=ITMAX
      AX1=X1-DD
      AX2=X2-DD
      IF((IPR1*IPR2).EQ.0)ITER=ITMAX
      X=X1
      PNEW=P1
      IF(ABS(AX1).GT.ABS(AX2))PNEW=P2
      IF(ABS(AX1).GT.ABS(AX2))X=X2
      IF(ITER.EQ.ITMAX)GO TO 61
      ICLS=1
      GO TO 69
   70 ICLS=0
      GO TO 65
C
C     SUCCESSFUL ITERATIONS
C
   65 INDEX=INDEX+1
      isuc=1
      RANG=rPRF
      XXX=XX
      YYY=YY
      ZZZ=ZZ
      TTT=T
      PAZM=PNEW
      XAX=X
      IF(MOUT.EQ.3)WRITE(LOU,113)DD,X,XX,YY,ZZ,T,PNEW,PSI0,
     1IND,IND1,ITER,II,INDEX
      GO TO 98
C
   80 continue
      P1=PNEW
      X1=X
      T1=T
      IF(ITER.GT.ITMAX)P1=AAOLD
      IF(ITER.GT.ITMAX)X1=XXOLD
      IF(ITER.GT.ITMAX)T1=TTOLD
      P2=AANEW
      X2=XXNEW
      T2=TTNEW
      GO TO 3
C
C     E N D   O F    I T E R A T I O N S
C
C     BOUNDARY RAYS: CASE IOLD.NE.ITPR, INEW=ITPR
C     OR CASE  IOLD=ITPR, INEW=ITPR  BUT  IOLD1.NE.INEW1
C     (IOLD1.NE.INDR1, INEW1=INDR1)
C
   50 XXNEW=XNEW
      TTNEW=TNEW
      AANEW=ANEW
      IBNEW=0
      P1=AOLD
      P2=ANEW
   54 IRES=0
      ITER=0
   51 PNEW=0.5*(P1+P2)
      ITER=ITER+1
      IND=INDS
      NDER=1
      CALL RAYA(XSOUR,YSOUR,ZSOUR,TSOUR,PSI0,PNEW,PX,PY,PZ,XX,YY,ZZ,T,
     1DT,AC)
      x=(yprf-yy)*xcos-(xprf-xx)*xsin
      IF(MOUT.EQ.3)WRITE(LOU,103)IND,IND1,ITER,X,XX,YY,T,PNEW,PSI0
      IF(IND.EQ.ITPR.AND.IND1.EQ.Indr1)GO TO 52
      P1=PNEW
      if((x-dd)*(xnew-dd).gt.0.)iter=itmax
      GO TO 53
   52 XXOLD=X
      AAOLD=PNEW
      TTOLD=T
      IBOLD=1
      if((x-dd)*(xnew-dd).lt.0.)iter=itmax
      IF(ABS(X-XAX).LT.REPS1)ITER=ITMAX
      IRES=1
      XAX=X
      T1=T
      P2=PNEW
   53 IF(ITER.LT.ITMAX)GO TO 51
      IF(MOUT.EQ.3)WRITE(LOU,107)X,ZZ,XX,YY,T,PNEW,IND,IND1,IRES
      IF(IRES.EQ.1) GOTO 41
      GO TO 3
C
C     BOUNDARY RAYS: CASE IOLD=ITPR, INEW.NE.ITPR
C     OR CASE  IOLD=ITPR, INEW=ITPR  BUT  IOLD1.NE.INEW1
C     (IOLD1=INDR1, INEW1.NE.INDR1)
C
   55 XXOLD=XOLD
      AAOLD=AOLD
      TTOLD=TOLD
      IBOLD=0
      P1=AOLD
      P2=ANEW
   59 IRES=0
      ITER=0
   56 PNEW=0.5*(P1+P2)
      ITER=ITER+1
      IND=INDS
      NDER=1
      CALL RAYA(XSOUR,YSOUR,ZSOUR,TSOUR,PSI0,PNEW,PX,PY,PZ,XX,YY,ZZ,T,
     1DT,AC)
      x=(yprf-yy)*xcos-(xprf-xx)*xsin
      IF(MOUT.EQ.3)WRITE(LOU,103)IND,IND1,ITER,X,XX,YY,T,PNEW,PSI0
      IF(IND.EQ.ITPR.AND.IND1.EQ.Indr1)GO TO 57
      P2=PNEW
      if((x-dd)*(xold-dd).gt.0.)iter=itmax
      GO TO 58
   57 XXNEW=X
      AANEW=PNEW
      TTNEW=T
      IBNEW=1
      if((x-dd)*(xold-dd).lt.0.)iter=itmax
      IF(ABS(X-XAX).LT.REPS1.AND.IRES.EQ.1) ITER=ITMAX
      IRES=1
      XAX=X
      T2=T
      P1=PNEW
   58 IF(ITER.LT.ITMAX)GO TO 56
      IF(MOUT.EQ.3)WRITE(LOU,107)X,ZZ,XX,YY,T,PNEW,IND,IND1,IRES
      IF(IRES.EQ.1)GOTO 41
      GO TO 3
C
C     CRITICAL RAYS. CASE IOLD.NE.9, IOLD.NE.3, INEW=9
C
   30 ITER=0
      XCR=XNEW
      P1=AOLD
      P2=ANEW
      IRES=0
   31 PNEW=0.5*(P1+P2)
      ITER=ITER+1
      IND=INDS
      NDER=1
      CALL RAYA(XSOUR,YSOUR,ZSOUR,TSOUR,PSI0,PNEW,PX,PY,PZ,XX,YY,ZZ,T,
     1DT,AC)
      x=(yprf-yy)*xcos-(xprf-xx)*xsin
      IF(MOUT.EQ.3)WRITE(LOU,104)IND,IND1,ITER,X,XX,YY,T,PNEW,PSI0
      IF(IND.EQ.9)GO TO 32
      IF(IND.EQ.ITPR)GO TO 33
      P1=PNEW
      GO TO 34
   32 CONTINUE
C 32  IF(IND1.NE.INEW1)P1=PNEW
C     IF(IND1.NE.INEW1) GOTO 34
      P2=PNEW
      IF(ABS(X-XCR).LT.0.01.AND.KC.NE.0.AND.IRES.EQ.1) GOTO 89
      XCR=X
      GOTO 34
   89 ITER=ITMAX-1
      GO TO 31
   33 IF(ABS(X-XAX).LT.REPS1.AND.IRES.EQ.1)ITER=ITMAX
      IRES=1
      XAX=X
      T2=T
      P1=PNEW
      PAP=PNEW
   34 IF(ITER.LT.ITMAX)GO TO 31
      IF(MOUT.EQ.3)WRITE(LOU,111)X,ZZ,XX,YY,T,PNEW,IND,IND1,IRES
      IF(IRES.EQ.0) GOTO 3
      P2=PAP
      XXNEW=XAX
      AANEW=P2
      TTNEW=T2
      IBNEW=1
      P1=AOLD
      GO TO 54
C
C     CRITICAL RAYS. CASE IOLD=9, INEW.NE.9, INEW.NE.3.
C
   35 ITER=0
      P1=AOLD
      P2=ANEW
      IRES=0
   36 PNEW=0.5*(P1+P2)
      ITER=ITER+1
      IND=INDS
      NDER=1
      CALL RAYA(XSOUR,YSOUR,ZSOUR,TSOUR,PSI0,PNEW,PX,PY,PZ,XX,YY,ZZ,T,
     1DT,AC)
      x=(yprf-yy)*xcos-(xprf-xx)*xsin
      IF(MOUT.EQ.3)WRITE(LOU,104)IND,IND1,ITER,X,XX,YY,T,PNEW,PSI0
      IF(IND.EQ.9)GO TO 37
      IF(IND.EQ.ITPR)GO TO 38
      P2=PNEW
      GO TO 39
   37 IF(IND1.NE.IOLD1)P2=PNEW
      IF(IND1.NE.IOLD1) GO TO 39
      P1=PNEW
      IF(ABS(X-XCR).LT.0.01.AND.KC.NE.0.AND.IRES.EQ.1) GO TO 94
      XCR=X
      GO TO 39
   94 ITER=ITMAX-1
      GOTO 36
   38 IF(ABS(X-XAX).LT.REPS1.AND.IRES.EQ.1) ITER=ITMAX
      IRES=1
      XAX=X
      P2=PNEW
      PAP=PNEW
      T1=T
   39 IF(ITER.LT.ITMAX)GO TO 36
      IF(MOUT.EQ.3)WRITE(LOU,111)X,ZZ,XX,YY,T,PNEW,IND,IND1,IRES
      IF(IRES.EQ.0) GOTO 3
      P1=PAP
      XXOLD=XAX
      AAOLD=P1
      TTOLD=T1
      IBOLD=1
      P2=ANEW
      GO TO 59
C
C
  100 FORMAT('*',2I3,5F10.5,2F15.10)
  101 FORMAT(1X,'*','ITERATIONS',5X,4I3,5F10.5,2F15.10)
  103 FORMAT(2X,'*','BOUNDARY RAY',5X,3I3,4F10.5,2F15.10)
  104 FORMAT(2X,'*','CRITICAL RAY',5X,3I3,4F10.5,2F15.10)
  107 FORMAT(10X,'*',5F10.5,F15.10,3I3,5X,'BOUNDARY RAY')
  111 FORMAT(10X,'*',5F10.5,F15.10,3I3,5X,'CRITICAL RAY')
  113 FORMAT('*',7F10.5,F15.10,5I3)
C
C
   98 CONTINUE
      LNDST=1
   99 CONTINUE
      RETURN
      END
C
C     *********************************************************
C
      SUBROUTINE RAYA (X0,Y0,Z0,T0,FI0,PSI0,PX,PY,PZ,XX,YY,ZZ,T,DT,AC)
C
C  INITIAL-VALUE RAY TRACING BY THE RUNGE-KUTTA METHOD
C
      DIMENSION Y(18),DEP(6),PRM(5),DERY(18),AUX(8,18),DIN(18),VSQ(3)
      dimension pn(3)
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
      COMMON /AUXI/  IANI(20),INTR,INT1,IPREC,KRE,IREFR,LAY,NDER,IPRINT,
     1 MPRINT,NTR,ISQRT,NAUX,ISOUR,MAUX,MREG,MDIM,IPOL,mscon,lou,
     2 IAMP,MTRNS,ICOEF,IAD,IRHO,ISHEAR,IAC,IRT,mori
      INTEGER CODE
      COMMON /COD/  CODE(50,2),KREF,KC,ITYPE
      COMMON /INTRF/ Z(1000),SX(350),SY(350),NX(20),NY(20),BRD(6),NINT,
     1   XINTA
      COMPLEX PS
      COMMON /RAY/   AY(28,2000),DS(20,50),KINT(50),HHH(3,3),tmax,
     1               PS(3,7,50),IS(8,50),N,IREF,IND,IND1
      COMMON /ZERO/ RNULL
      common/dyn/xdyn(3,3),ydyn(3,3)
      common/appr/ xold,xnew,yold(18),dold(18),ynew(18),dnew(18)
      COMMON/KM/KMAH,SPROLD,TSTOLD
      EXTERNAL FCT,OUT
C
      IAC=0
      kmah=0
      Y(1)=X0
      Y(2)=Y0
      Y(3)=Z0
      IREFR=0
      KRE=KREF
      IF(KC.EQ.0) KRE=0
      N=0
      IREF=1
      IF(IND.GT.0) GOTO 6
C
C  FOR IND=-1 DETERMINATION OF THE NUMBER OF THE LAYER IN WHICH THE
C  INITIAL POINT OF THE RAY IS SITIUATED
C
      IF(Y(1).LT.BRD(1).OR.Y(1).GT.BRD(2).OR.Y(2).LT.BRD(3).OR.Y(2).GT.
     1   BRD(4)) GOTO 4
      INTR=1
   1  CALL DISC (Y,DEP)
      ZINT=DEP(1)
      IF(ABS(Y(3)).GT.0.00001) GOTO 2
      ISOUR=1
C
C  REDEFINITION OF Z-COORDINATES FOR A SOURCE ON THE EARTH SURFACE
C
      Z0=ZINT
      GOTO 5
   2  IF(Y(3).LT.ZINT.AND.INTR.EQ.1) GOTO 4
      IF(Y(3).LT.ZINT) GOTO 5
      IF(ABS(Y(3)-ZINT).LT.RNULL.AND.INTR.EQ.NINT) GOTO 5
      IF(INTR.EQ.NINT) GOTO 4
      ISOUR=INTR
      INTR=INTR+1
      GOTO 1
   4  WRITE(lou,'(A,/,6F10.5,/,4F10.5)')' Y,BRD',Y,BRD
      IND=50
      RETURN
C
C  DETERMINATION OF INITIAL CONDITIONS FOR THE RUNGE-KUTTA PROCEDURE
C
   5  IF(IND.GE.0) GOTO 6
      IND=ISOUR
      RETURN
   6  LAY=ISOUR
      is(3,1)=lay
      INT1=ISOUR
      IF(ISOUR.NE.CODE(1,1)) IND=14
      IF(ISOUR.NE.CODE(1,1)) RETURN
      ITYPE=CODE(1,2)
      CALL PARDIS(Y,0)
C
C  DETERMINATION OF INITIAL VALUES FOR RAY TRACING
C  AND DYNAMIC RAY TRACING
C
      kdim=6
      if(nder.gt.1)kdim=18
      csp=cos(psi0)
      snp=sin(psi0)
      csf=cos(fi0)
      snf=sin(fi0)
      if(mori.eq.0)then
        Y(4)=CSP*CSF
        Y(5)=SNP*CSF
        Y(6)=SNF
      else
        y(4)=csf*csp
        y(5)=snp
        y(6)=snf*csp
      end if
      do 8 k=1,3
        pn(k)=y(k+3)
    8 continue
      if(nder.gt.1)then
        do 3 k=7,12
          y(k)=0.
    3   continue
        if(mori.eq.0)then
          y(13)=-snp
          y(14)=csp
          y(15)=0.
          y(16)=-csp*snf
          y(17)=-snp*snf
          y(18)=csf
        else
          y(13)=-snf
          y(14)=0.
          y(15)=csf
          y(16)=-csf*snp
          y(17)=csp
          y(18)=-snf*snp
        end if
      end if
      IF(IANI(ISOUR).ne.0)then
C
C  SOURCE LOCATED IN AN ANISOTROPIC LAYER
C
        CALL INIT(pn,VSQ)
        IF(IPRINT.GT.2)WRITE(lou,'(a,3F14.6)')' V1,V2,V3=', VSQ
        VP=AMAX1(VSQ(1),VSQ(2),VSQ(3))
        VS1=AMIN1(VSQ(1),VSQ(2),VSQ(3))
        VS2=VSQ(1)+VSQ(2)+VSQ(3)-VP-VS1
        IF(ITYPE.EQ.3)V=SQRT(VP)
        IF(ITYPE.EQ.1)V=SQRT(VS1)
        IF(ITYPE.EQ.2)V=SQRT(VS2)
        do 7 i=4,6
          y(i)=y(i)/v
    7   continue
        if(nder.gt.1)then
          nder=1
          call fct(0.,y,dery)
          nder=2
          vg=sqrt(dery(1)*dery(1)+dery(2)*dery(2)+dery(3)*dery(3))
          if(mori.eq.0)then
            auxp=-dery(1)*snp+dery(2)*csp
            auxf=-dery(1)*csp*snf-dery(2)*snp*snf+dery(3)*csf
            y(13)=y(13)-auxp*csp*csf/v
            y(14)=y(14)-auxp*snp*csf/v
            y(15)=y(15)-auxp*snf/v
            y(16)=y(16)-auxf*csp*csf/v
            y(17)=y(17)-auxf*snp*csf/v
            y(18)=y(18)-auxf*snf/v
          else
            auxp=-dery(1)*snf+dery(3)*csf
            auxf=-dery(1)*csf*snp+dery(2)*csp-dery(3)*snf*snp
            y(13)=y(13)-auxp*csf*csp/v
            y(14)=y(14)-auxp*snp/v
            y(15)=y(15)-auxp*snf*csp/v
            y(16)=y(16)-auxf*csf*csp/v
            y(17)=y(17)-auxf*snp/v
            y(18)=y(18)-auxf*snf*csp/v
          end if
          do 11 i=13,18
            y(i)=y(i)/v
   11     continue
C
C     DETERMINATION OF THE SOURCE INDEX IN ANISOTROPIC MEDIUM
C
          call fct(0.,y,dery)
          aaa=y(4)*dery(1)+y(5)*dery(2)+y(6)*dery(3)
          if(abs(aaa-1.).gt.1.0e-02)then
            ind=10
            return
          end if
          el=-(dery(7)*y(13)+dery(8)*y(14)+dery(9)*y(15))/vg
          em=-(dery(10)*y(13)+dery(11)*y(14)+dery(12)*y(15))/vg
          en=-(dery(10)*y(16)+dery(11)*y(17)+dery(12)*y(18))/vg
          ee=y(13)*y(13)+y(14)*y(14)+y(15)*y(15)
          ff=y(13)*y(16)+y(14)*y(17)+y(15)*y(18)
          gg=y(16)*y(16)+y(17)*y(17)+y(18)*y(18)
          egf=(ee*gg-ff*ff)/v/v
          be=el*gg+en*ee-2.*em*ff
          ce=el*en-em*em
          if(egf.gt.0.)then
            if(ce.lt.0.)kmah=-1
            if(ce.gt.0.)then
              if(be.lt.0.)kmah=0
              if(be.gt.0.)kmah=-2
            end if
          end if
          if(egf.lt.0.)then
            if(ce.gt.0.)kmah=-1
            if(ce.lt.0.)then
              if(be.gt.0.)kmah=0
              if(be.lt.0.)kmah=-2
            end if
          end if
        end if
      end if
C
C  SOURCE LOCATED IN AN ISOTROPIC LAYER
C
      IF(IANI(ISOUR).eq.0)then
        IF(ITYPE.EQ.3)V=SQRT(A11)
        IF(ITYPE.NE.3)V=SQRT(A44)
        do 9 i=4,kdim
            y(i)=y(i)/v
    9   continue
      end if
C
      IND=0
      IND1=0
      PRM(1)=T0
      PRM(2)=TMAX
      PRM(3)=DT
      IF(ITYPE.NE.3) PRM(3)=DT*1.7
      PRM(4)=AC
      T=PRM(1)
   20 CONTINUE
      DO 10 I=1,3
        auxx=y(4)*y(4)+y(5)*y(5)+y(6)*y(6)
        auxx=sqrt(auxx)
        DIN(I)=auxx
        din(i+3)=prm(3)/auxx
   10 CONTINUE
      do 25 i=7,kdim
        din(i)=0.
   25 continue
      DO 30 I=1,kdim
        DERY(I)=DIN(I)
   30 CONTINUE
C
C  COMPUTATION OF THE RAY
C
      CALL RKGS(PRM,Y,DERY,kdim,IHLF,FCT,OUT,AUX)
      IF(IHLF.EQ.11) IND=5
      IF(IHLF.EQ.12) IND=6
      IF(IHLF.EQ.13) IND=7
      IF(IND.GE.5.AND.IND.LE.7) RETURN
      IF(ABS(PRM(5)).GT.0.0001) GOTO 35
      IF(IND.eq.12) GOTO 70
      GOTO 60
   35 CONTINUE
      XX=Y(1)
      YY=Y(2)
      ZZ=Y(3)
      T=AY(1,N)
      IF(ABS(PRM(5)-2.).GT.0.0001) GOTO 80
C
C  INTEGRATION FROM THE POINT OF REFLECTION/TRANSMISSION TO THE CLOSEST
C  POINT OF THE RAY CORRESPONDING TO REGULAR TIME STEP
C
      PRM(1)=AY(1,N)
      I=INT((PRM(1)-T0)/DT)
      PRM(2)=FLOAT(I+1)*DT+T0
      PRM(3)=DT
      GOTO 20
   60 PRM(1)=PRM(2)
      PRM(2)=TMAX
      PRM(3)=DT
      IF(ITYPE.NE.3) PRM(3)=1.7*DT
      N=N-1
      GOTO 20
   70 CONTINUE
      XX=Y(1)
      YY=Y(2)
      ZZ=Y(3)
   80 continue
c      if(kmah.ne.0)ind1=ind1+50
      IF(IREFR.EQ.1) IND1=-IND1
      PX=Y(4)
      PY=Y(5)
      PZ=Y(6)
      if(nder.gt.1)then
        do 90 i=1,3
          xdyn(i,1)=y(i+6)
          xdyn(i,2)=y(i+9)
          xdyn(i,3)=dery(i)
          ydyn(i,1)=y(i+12)
          ydyn(i,2)=y(i+15)
          ydyn(i,3)=dery(i+3)
   90   continue
      end if
      RETURN
      END
C
C     *********************************************************
C
      SUBROUTINE RECEIV(XSOUR,YSOUR,ZSOUR,TSOUR,DT,AC,ITMAX,ASTART,
     1ASTEP,AFIN,BMIN,BSTEP,BMAX,MOUT,LU1,LU2,METHOD,ITPR,NCD)
C
C     TWO-POINT RAY TRACING
C
      COMPLEX AMPX1,AMPX2,AMPY1,AMPY2,AMPZ1,AMPZ2,APX,APY,APZ
      DIMENSION JC(50,2),YDD(2),DEP(6)
      DIMENSION TIME(500),XCOOR(500),ZCOOR(500),INDI(500),AMPX1(500),
     1AMPY1(500),AMPZ1(500),AMPX2(500),AMPY2(500),AMPZ2(500),
     2p(500,3),pol(500,3),pol1(500,3),APX(2),APY(2),APZ(2)
      COMMON /AUXI/  IANI(20),INTR,INT1,IPREC,KRE,IREFR,LAY,NDER,IPRINT,
     1 MPRINT,NTR,ISQRT,NAUX,ISOUR,MAUX,MREG,MDIM,IPOL,MSCON,LOU,
     2 IAMP,MTRNS,ICOEF,IAD,IRHO,ISHEAR,IAC,IRT,mori
      INTEGER CODE
      COMMON /COD/  CODE(50,2),KREF,KC,ITYPE
      COMMON /DIST/ DST(200),NDST,REPS,PROF(2),NDSTP,PREPS,LNDST,
     1XPRF,YPRF,ILOC
      COMPLEX PS,CKMAH
      COMMON /RAY/   AY(28,2000),DS(20,50),KINT(50),HHH(3,3),tmax,
     1               PS(3,7,50),IS(8,50),N,IREF,IND,IND1
      COMMON /RAY2/  DRY(3,2000)
      COMMON/VSP/XVSP,YVSP,XNRM,YNRM,ICOD,IVSP
      COMMON/DYN/XDYN(3,3),ydyn(3,3)
      COMMON/KM/KMAH,SPROLD,TSTOLD
      COMMON/VRML/LUBRD,LUGRD,LUIND,LURAY
C
      ITER=0
      indr1=0
      II=0
      DD=0.
C
      AA=ASTART-ASTEP
      BMIN1=BMIN
      REPS1=.05*REPS
      INDEX=0
      INUM=0
      IK1=0
      ICR=0
      INDS=ISOUR
C
C     LOOP IN DECLINATION, FROM VALUE ASTART TO AFIN, WITH THE STEP
C     ASTEP
C
    1 AA=AA+ASTEP
      PNEW=AA
      IF(ASTEP.GT.0..AND.AA.GT.AFIN)GO TO 99
      IF(ASTEP.LT.0..AND.AA.LT.AFIN)GO TO 99
    4 IND=INDS
      CALL PROFIL(XSOUR,YSOUR,ZSOUR,TSOUR,AA,PAZM,r,XX,YY,Zz,T,
     1DT,AC,BMIN,BSTEP,BMAX,ITMAX,MOUT,NCD,METHOD,ITPR,indr1)
      IF(IND.EQ.14)RETURN
      IF(ITPR.EQ.43)r=Zz
      IF(NDST.EQ.0.AND.LNDST.EQ.0)THEN
        BMIN=BMIN1
        GO TO 1
      END IF
      IF(NDST.EQ.0)GO TO 65
      IF(IND.EQ.ITPR)rAX=r
      IF(IND.EQ.ITPR)PNEW=AA
      IF(MOUT.GE.2)WRITE(LOU,100)IND,IND1,KMAH,r,T,AA
      IF(INUM.NE.0)GO TO 2
      dOLD=AA
      aold=pazm
      IOLD=IND
      IOLD1=IND1
      rOLD=r
      xold=xx
      yold=yy
      zold=zz
      TOLD=T
      INUM=1
      GO TO 1
C
C     PARAMETERS FOR THE PRECEDING RAY:
C       DOLD (AA), AOLD (PAZM), ROLD (R), IOLD (IND)
C     PARAMETERS FOR THE NEW RAY:
C       DNEW (AA), ANEW (PAZM), RNEW (R), INEW (IND)
C
    2 INEW=IND
      INEW1=IND1
      dNEW=AA
      anew=pazm
      rNEW=r
      xnew=xx
      ynew=yy
      znew=zz
      TNEW=T
      IF(INEW.EQ.ITPR.AND.IOLD.EQ.ITPR)GO TO 21
      IF(INEW.EQ.ITPR)GO TO 50
      IF(IOLD.EQ.ITPR)GO TO 55
      IF(INEW.EQ.9.AND.IOLD.NE.9.AND.IOLD.NE.8)GO TO 30
      IF(INEW.NE.9.AND.INEW.NE.8.AND.IOLD.EQ.9)GO TO 35
c      indr1=ind1
      GO TO 3
   21 IF(IOLD1.NE.INEW1)IK1=2
      IF(IK1.EQ.2)GO TO 55
c      indr1=ind1
      GO TO 40
C
C     NO ITERATIONS, TAKE A NEW RAY IN THE LOOP
C
    3 CONTINUE
      IOLD=INEW
      IOLD1=INEW1
      rOLD=rNEW
      xold=xnew
      yold=ynew
      zold=znew
      dOLD=dNEW
      aold=anew
      TOLD=TNEW
      GO TO 1
C
C     REGULAR CASE: IOLD=3, INEW=3
C
   40 rrNEW=rNEW
      xxnew=xnew
      yynew=ynew
      zznew=znew
      rrOLD=rOLD
      xxold=xold
      yyold=yold
      zzold=zold
      ddNEW=dNEW
      aanew=anew
      ddOLD=dOLD
      aaold=aold
      TTNEW=TNEW
      TTOLD=TOLD
      IBNEW=0
      IBOLD=0
   41 IF(rrNEW.GT.rrOLD.AND.DST(2).GT.DST(1))GO TO 46
      IF(rrNEW.LT.rrOLD.AND.DST(2).LT.DST(1))GO TO 46
C
C     REGULAR CASE: RRNEW.LE.RROLD, ITREND=-1 (REVERSE BRANCH)
C
      ITREND=-1
      DO 42 I=1,NDST
      NDD=NDST-I+1
      DD=DST(NDD)
      dr=rrOLD
      IF(IBOLD.EQ.1)dr=dr+REPS
      IF(DD.GE.dr)GO TO 42
      dr=rrNEW
      IF(IBNEW.EQ.1)dr=dr-REPS
      IF(DD.LT.dr.AND.IK1.NE.0)GO TO 6
      IF(DD.LT.dr)GO TO 3
      II=NDD
      GO TO 43
   42 CONTINUE
      IF(IK1.NE.0)GO TO 6
      GO TO 3
C
C     REGULAR CASE: RRNEW.GT.RROLD, ITREND=1 (REGULAR BRANCH)
C
   46 ITREND=1
      DO 44 I=1,NDST
      DD=DST(I)
      dr=rrOLD
      IF(IBOLD.EQ.1)dr=dr-REPS
      IF(DD.LE.dr)GO TO 44
      dr=rrNEW
      IF(IBNEW.EQ.1)dr=dr+REPS
      IF(DD.GT.dr.AND.IK1.NE.0)GO TO 6
      IF(DD.GT.dr)GO TO 3
      II=I
      GO TO 43
   44 CONTINUE
      IF(IK1.NE.0)GO TO 6
      GO TO 3
   43 d1=ddOLD
      a1=aaold
      d2=ddNEW
      a2=aanew
      x1=xxold
      y1=yyold
      z1=zzold
      x2=xxnew
      y2=yynew
      z2=zznew
      T1=TTOLD
      T2=TTNEW
C
C     I T E R A T I O N S
C
   60 continue
C
C     TRANSFORMATION OF COORDINATES OF A RECEIVER FROM CYLINDRICAL
C     TO CARTESIAN COORDINATES
C
      IF(ITPR.NE.43)THEN
        XD=Xprf+DD*COS(PROF(1))
        YD=Yprf+DD*SIN(PROF(1))
        YDD(1)=XD
        YDD(2)=YD
        INTR=1
        IF(ITPR.GT.100)INTR=ITPR-100
        CALL DISC(YDD,DEP)
        ZD=DEP(1)
      END IF
      IF(ITPR.EQ.43)THEN
        XD=XVSP
        YD=YVSP
        ZD=DD
      END IF
      DELX=XD-X1
      DELY=YD-Y1
      DELZ=ZD-Z1
      dr1=sqrt(delx*delx+dely*dely+delz*delz)
      DELX=XD-X2
      DELY=YD-Y2
      DELZ=ZD-Z2
      dr2=sqrt(delx*delx+dely*dely+delz*delz)
c
      ITER=0
      GO TO 61
   68 rAX=r
   61 ITER=ITER+1
      IF(ITER.GT.ITMAX)GO TO 80
C
C     PREPARATION FOR ITERATIVE SOLUTION OF TWO-POINT RAY TRACING
C
      if(method.eq.2.or.itpr.ne.ind)go to 69
C
C     PARAXIAL RAY APPROXIMATION
C
      AUX1=XDYN(2,1)*XDYN(3,2)-XDYN(3,1)*XDYN(2,2)
      AUX2=XDYN(3,1)*XDYN(1,2)-XDYN(1,1)*XDYN(3,2)
      AUX3=XDYN(1,1)*XDYN(2,2)-XDYN(2,1)*XDYN(1,2)
      DET=AUX1*XDYN(1,3)+AUX2*XDYN(2,3)+AUX3*XDYN(3,3)
      IF(ABS(DET).LT..0000001)GO TO 69
      AUX1=DELY*XDYN(3,2)-DELZ*XDYN(2,2)
      AUX2=DELZ*XDYN(1,2)-DELX*XDYN(3,2)
      AUX3=DELX*XDYN(2,2)-DELY*XDYN(1,2)
      IF(mori.eq.0)CSF=COS(PNEW)
      if(mori.ne.0)csf=cos(pazm)
      IF(ABS(csf).LT..0000001)GO TO 69
      aux11=(AUX1*XDYN(1,3)+AUX2*XDYN(2,3)+AUX3*XDYN(3,3))/DET/csf
      AUX1=DELZ*XDYN(2,1)-DELY*XDYN(3,1)
      AUX2=DELX*XDYN(3,1)-DELZ*XDYN(1,1)
      AUX3=DELY*XDYN(1,1)-DELX*XDYN(2,1)
      aux22=(AUX1*XDYN(1,3)+AUX2*XDYN(2,3)+AUX3*XDYN(3,3))/DET
      if(mori.eq.0)pazm=pazm+aux11
      if(mori.ne.0)pazm=pazm+aux22
      if(mori.eq.0)pnew=pnew+aux22
      if(mori.ne.0)pnew=pnew+aux11
      go to 72
C
C     HALVING OF INTERVAL
C
   69 pnew=0.5*(d1+d2)
      pazm=0.5*(a1+a2)
C
C     INITIAL ANGLES FOR A NEW RAY WERE DETERMINED
C
   72 ind=inds
      rOLD=0.
      SPROLD=0.
      IF(MDIM.GE.1)NDER=2
      CALL RAYA(XSOUR,YSOUR,ZSOUR,TSOUR,PNEW,PAZM,PX,PY,PZ,XX,YY,Zz,
     1T,DT,AC)
      NDER=1
      XE=XX-Xprf
      YE=YY-Yprf
      r=SQRT(XE*XE+YE*YE)
      delx=xd-xx
      dely=yd-yy
      delz=zd-zz
      drs=sqrt(delx*delx+dely*dely+delz*delz)
      IF(ITPR.EQ.43)r=Zz
      IF(MOUT.GE.2)WRITE(LOU,101)IND,IND1,ITER,KMAH,DD,r,T,PNEW,PAZM
      if(mout.eq.4)write(lou,120)xd,yd,zd,xx,yy,zz
  120 format(1x,'(x,y,z) receiver',3F15.8/1x,'(xx,yy,zz) ray',3F15.8)
      IF(IND.NE.ITPR)then
        d2=PNEW
        a2=pazm
        x2=xx
        y2=yy
        z2=zz
        dr2=drs
        GO TO 61
      end if
      IF(drs.LT.REPS)GO TO 65
      IF(dr2.LT.dr1)GO TO 63
      d2=PNEW
      a2=pazm
      x2=xx
      y2=yy
      z2=zz
      dr2=drs
      T2=T
      GO TO 68
   63 continue
      d1=PNEW
      a1=pazm
      x1=xx
      y1=yy
      z1=zz
      dr1=drs
      T1=T
      GO TO 68
C
C     SUCCESSFUL ITERATIONS
C
   65 INDEX=INDEX+1
      IF(NDST.EQ.0.AND.IND.NE.3)GO TO 900
      IF(MDIM.NE.0)CALL AMPL(APX,APY,APZ,UU)
  900 CONTINUE
      IF(LURAY.NE.0)GO TO 800
      IF(LU1.EQ.0.OR.NDST.EQ.0)GO TO 800
      TIME(INDEX)=T
      XCOOR(INDEX)=r
      ZCOOR(INDEX)=Zz
      INDI(INDEX)=II
  800 CONTINUE
      rAX=r
      IF(MOUT.GE.1)WRITE(LOU,113)DD,r,XX,YY,Zz,T,PNEW,PAZM,
     1IND,IND1,ITER,II,INDEX
      IF(LU1.NE.0.AND.NDST.NE.0)WRITE(LU1,105)N,II
      IF(LU1.NE.0.AND.NDST.NE.0)
     1WRITE(LU1,108)(AY(1,I),AY(2,I),AY(3,I),AY(4,I),AY(5,I),AY(6,I),
     2AY(7,I),AY(8,I),AY(12,I),AY(16,I),AY(17,I),AY(18,I),AY(19,I),
     3AY(20,I),AY(21,I),AY(22,I),I=1,N)
      IF(MDIM.EQ.0)GO TO 80
      IF(IND.NE.ITPR)GO TO 80
      SPR=1.
      CKMAH=(1.,0.)
      IF(MDIM.EQ.2)THEN
        IF(KMAH.NE.0)THEN
          PH=-1.57079632*KMAH
          CSKMAH=COS(PH)
          SNKMAH=SIN(PH)
          CKMAH=CMPLX(CSKMAH,SNKMAH)
        END IF
        SPR1=XDYN(2,1)*XDYN(3,2)-XDYN(3,1)*XDYN(2,2)
        SPR2=XDYN(3,1)*XDYN(1,2)-XDYN(1,1)*XDYN(3,2)
        SPR3=XDYN(1,1)*XDYN(2,2)-XDYN(2,1)*XDYN(1,2)
        SPR=XDYN(1,3)*SPR1+XDYN(2,3)*SPR2+XDYN(3,3)*SPR3
        VV=ay(5,n)*ay(5,n)+ay(6,n)*ay(6,n)+ay(7,n)*ay(7,n)
        SPR=SPR*SQRT(VV)
        SPR=SQRT(ABS(SPR))
        IF(MOUT.GE.2)WRITE(LOU,110)XDYN
        IF(MOUT.GE.2)WRITE(LOU,112)yDYN
      END IF
      DO 802 I=1,2
      APX(I)=APX(I)*UU*CKMAH/SPR
      APY(I)=APY(I)*UU*CKMAH/SPR
      APZ(I)=APZ(I)*UU*CKMAH/SPR
  802 CONTINUE
      IF(MOUT.GE.1)
     1WRITE(LOU,'(2X,F8.5,6(2X,E11.5)/10X,6(2X,E11.5),F10.5,I5)')
     2UU,(APX(I),APY(I),APZ(I),I=1,2),SPR,KMAH
      TAUX=T
      TAST=0.
      NCC=code(1,2)
      ncod=ncd
      IF(iani(isour).eq.0.and.ncc.ne.3)NCOD=-NCD
      call polar(1,1,1,1)
      IF(LU2.NE.0.AND.NDST.NE.0)then
        WRITE(LU2,116)ncod,II,T,APX(1),APY(1),APZ(1),TAST
	write(20,*) ncod,II,T,APX(1),APY(1),APZ(1),TAST      !!Li
	write(30,130) ncod,II,T,APX(1),APY(1),APZ(1),TAST    !!Li

        if(ncc.eq.1.and.ncod.lt.0)WRITE(LU2,115)APX(2),APY(2),APZ(2)
        WRITE(LU2,114)ay(5,1),ay(6,1),ay(7,1)
        if(ncc.eq.1)WRITE(LU2,114)(hhh(1,i),i=1,3)
        if(ncc.eq.1.and.ncod.lt.0)WRITE(LU2,114)(hhh(2,i),i=1,3)
        if(ncc.eq.2)WRITE(LU2,114)(hhh(2,i),i=1,3)
        if(ncc.eq.3)WRITE(LU2,114)(hhh(3,i),i=1,3)
c        write(LU2,114)spr
      end if
      IF(LURAY.NE.0)GO TO 801
      IF(LU1.EQ.0.OR.NDST.EQ.0)GO TO 801
      AMPX1(INDEX)=APX(1)
      AMPY1(INDEX)=APY(1)
      AMPZ1(INDEX)=APZ(1)
      if(ncc.eq.1.and.ncod.lt.0)then
        AMPX2(INDEX)=APX(2)
        AMPY2(INDEX)=APY(2)
        AMPZ2(INDEX)=APZ(2)
      end if
      p(index,1)=ay(5,1)
      p(index,2)=ay(6,1)
      p(index,3)=ay(7,1)
      if(ncc.eq.1)then
        pol(index,1)=hhh(1,1)
        pol(index,2)=hhh(1,2)
        pol(index,3)=hhh(1,3)
      end if
      if(ncc.eq.1.and.ncod.lt.0)then
        pol1(index,1)=hhh(2,1)
        pol1(index,2)=hhh(2,2)
        pol1(index,3)=hhh(2,3)
      end if
      if(ncc.eq.2)then
        pol(index,1)=hhh(2,1)
        pol(index,2)=hhh(2,2)
        pol(index,3)=hhh(2,3)
      end if
      if(ncc.eq.3)then
        pol(index,1)=hhh(3,1)
        pol(index,2)=hhh(3,2)
        pol(index,3)=hhh(3,3)
      end if
  801 CONTINUE
C
C     GENERATE FILE FOR PLOTTING RAYS
C
      IF(LURAY.NE.0)THEN
        WRITE(LURAY,119)INDEX
        DO 803 J=1,N
        WRITE(LURAY,122)AY(2,J),AY(3,J),AY(4,J)
  803   CONTINUE
        WRITE(LURAY,121)
      END IF
C
C
   80 IF(NDST.EQ.0.AND.LNDST.EQ.1)THEN
        BMIN=BMIN+BSTEP
        GO TO 4
      END IF
      d1=PNEW
      a1=pazm
      dr1=drs
      x1=xx
      y1=yy
      z1=zz
      T1=TAUX
      d2=ddNEW
      a2=aanew
      x2=xxnew
      y2=yynew
      z2=zznew
      T2=TTNEW
      IF(ITREND.EQ.(-1))GO TO 85
      II=II+1
      IF(II.GT.NDST.AND.IK1.NE.0)GO TO 6
      IF(method.eq.1)then
        aa=pnew
        inum=0
      end if
      IF(II.GT.NDST)GO TO 3
      DD=DST(II)
      if(method.eq.1)go to 60
      dr=rrNEW
      IF(IBNEW.EQ.1)dr=dr+REPS
      IF(DD.GT.dr.AND.IK1.NE.0)GO TO 6
      IF(DD.GT.dr)GO TO 3
      GO TO 60
   85 II=II-1
      IF(II.LT.1.AND.IK1.NE.0)GO TO 6
      IF(method.eq.1)then
        aa=pnew
        inum=0
      end if
      IF(II.LT.1)GO TO 3
      DD=DST(II)
      if(method.eq.1)go to 60
      dr=rrNEW
      IF(IBNEW.EQ.1)dr=dr-REPS
      IF(DD.LT.dr.AND.IK1.NE.0)GO TO 6
      IF(DD.LT.dr)GO TO 3
      GO TO 60
C
C
    6 CONTINUE
      IF(IK1.EQ.1)GO TO 7
      IK1=1
      d1=dNEW
      a1=anew
      d2=ddNEW
      a2=aanew
      IOLD1=INEW1
      indr1=iold1
      GO TO 59
    7 IK1=0
      rrOLD=rrNEW
      xxold=xxnew
      yyold=yynew
      zzold=zznew
      ddOLD=ddNEW
      aaold=aanew
      TTOLD=TTNEW
      IBOLD=IBNEW
      rrNEW=rNEW
      xxnew=xnew
      yynew=ynew
      zznew=znew
      ddNEW=dNEW
      aanew=anew
      TTNEW=TNEW
      IBNEW=0
      GO TO 41
C
C     E N D   O F    I T E R A T I O N S
C
C
C      BOUNDARY RAYS. CASE IOLD.NE.3, INEW=3
C
   50 rrNEW=rNEW
      xxnew=xnew
      yynew=ynew
      zznew=znew
      TTNEW=TNEW
      ddNEW=dNEW
      aanew=anew
      IBNEW=0
      d1=dOLD
      d2=dNEW
      a1=dOLD
      a2=dNEW
   54 IRES=0
      ITER=0
   51 PNEW=0.5*(d1+d2)
      ITER=ITER+1
      IND=INDS
      CALL PROFIL(XSOUR,YSOUR,ZSOUR,TSOUR,PNEW,PAZM,r,XX,YY,Zz,T,
     1DT,AC,BMIN,BSTEP,BMAX,ITMAX,MOUT,NCD,METHOD,ITPR,indr1)
      IF(ITPR.EQ.43)r=Zz
      IF(MOUT.GE.2)WRITE(LOU,103)IND,IND1,ITER,r,T,PNEW
      IF(IND.EQ.ITPR.AND.LNDST.EQ.1)GO TO 52
      d1=PNEW
      a1=pazm
      GO TO 53
   52 rrOLD=r
      xxold=xx
      yyold=yy
      zzold=zz
      ddOLD=PNEW
      aaold=pazm
      TTOLD=T
      IBOLD=1
      IF(ABS(r-rAX).LT.REPS1.AND.IRES.EQ.1)ITER=ITMAX
      IRES=1
      rAX=r
      ZAX=Zz
      IAX=IND
      IAX1=IND1
      T1=T
      d2=PNEW
      a2=pazm
   53 IF(ITER.LT.ITMAX)GO TO 51
      IF(MOUT.GE.1.AND.IRES.EQ.1)
     1WRITE(LOU,107)rAX,ZAX,T1,d2,IAX,IAX1,IRES
      IF(MOUT.GE.1.AND.IRES.EQ.0)
     1WRITE(LOU,107)r,Zz,T,PNEW,IND,IND1,IRES
      IF(IRES.EQ.1)GO TO 41
      GO TO 3
C
C     BOUNDARY RAYS. CASE IOLD=3, INEW.NE.3
C     OR CASE  IOLD=3, INEW=3  BUT  IOLD1.NE.INEW1
C
   55 rrOLD=rOLD
      xxold=xold
      yyold=yold
      zzold=zold
      ddOLD=dOLD
      aaold=aold
      TTOLD=TOLD
      IBOLD=0
      d1=dOLD
      d2=dNEW
      a1=aOLD
      a2=aNEW
   59 IRES=0
      ITER=0
   56 PNEW=0.5*(d1+d2)
      ITER=ITER+1
      IND=INDS
      CALL PROFIL(XSOUR,YSOUR,ZSOUR,TSOUR,PNEW,PAZM,r,XX,YY,Zz,T,
     1DT,AC,BMIN,BSTEP,BMAX,ITMAX,MOUT,NCD,METHOD,ITPR,indr1)
      IF(ITPR.EQ.43)r=Zz
      IF(MOUT.GE.2)WRITE(LOU,103)IND,IND1,ITER,r,T,PNEW
      IF(IND.EQ.ITPR.AND.IBOLD.EQ.1.AND.LNDST.EQ.1)GO TO 57
      IF(IND.EQ.ITPR.AND.IND1.EQ.IOLD1.AND.LNDST.EQ.1)GO TO 57
      d2=PNEW
      a2=pazm
      GO TO 58
   57 rrNEW=r
      xxnew=xx
      yynew=yy
      zznew=zz
      ddNEW=PNEW
      aanew=pazm
      TTNEW=T
      IBNEW=1
      IF(ABS(r-rAX).LT.REPS1.AND.IRES.EQ.1)ITER=ITMAX
      IRES=1
      rAX=r
      ZAX=Zz
      IAX=IND
      IAX1=IND1
      T2=T
      d1=PNEW
      a1=pazm
   58 IF(ITER.LT.ITMAX)GO TO 56
      IF(MOUT.GE.1.AND.IRES.EQ.1)
     1WRITE(LOU,107)rAX,ZAX,T2,d1,IAX,IAX1,IRES
      IF(MOUT.GE.1.AND.IRES.EQ.0)
     1WRITE(LOU,107)r,Zz,T,PNEW,IND,IND1,IRES
      IF(IRES.EQ.1.AND.IK1.EQ.1)GO TO 7
      IF(IRES.EQ.1)GO TO 41
      GO TO 3
C
C     CRITICAL RAYS. CASE IOLD.NE.9, IOLD.NE.3, INEW=9
C
   30 ITER=0
      d1=dOLD
      d2=dNEW
      IRES=0
   31 PNEW=0.5*(d1+d2)
      ITER=ITER+1
      IND=INDS
      CALL PROFIL(XSOUR,YSOUR,ZSOUR,TSOUR,PNEW,PAZM,r,XX,YY,Zz,T,
     1DT,AC,BMIN,BSTEP,BMAX,ITMAX,MOUT,NCD,METHOD,ITPR,indr1)
      IF(ITPR.EQ.43)r=Zz
      IF(MOUT.GE.2)WRITE(LOU,104)IND,IND1,ITER,r,T,PNEW
      IF(IND.EQ.9)GO TO 32
      IF(IND.EQ.ITPR)GO TO 33
      d1=PNEW
      GO TO 34
   32 IF(IND1.NE.INEW1)d1=PNEW
      IF(IND1.NE.INEW1)GO TO 34
      d2=PNEW
      IF(ITER.EQ.ITMAX.AND.KC.NE.0.AND.IRES.EQ.1)GO TO 89
      GO TO 34
   89 DO 86 K=1,KREF
      DO 86 L=1,2
   86 JC(K,L)=CODE(K,L)
      IRF3=IREF+3
      DO 87 K=IRF3,KREF
      DO 87 L=1,2
   87 CODE(K-2,L)=JC(K,L)
      KREF1=KREF
      KREF=KREF-2
      ICR=1
      ITER=ITMAX-1
      GO TO 31
   33 IF(ABS(r-rAX).LT.REPS1.AND.IRES.EQ.1)ITER=ITMAX
      IRES=1
      rAX=r
      rrnew=r
      xxnew=xx
      yynew=yy
      zznew=zz
      ZAX=Zz
      IAX=IND
      IAX1=IND1
      T2=T
      d1=PNEW
      PAP=PNEW
   34 IF(ITER.LT.ITMAX)GO TO 31
      IF(MOUT.GE.1.AND.IRES.EQ.1)
     1WRITE(LOU,111)rAX,ZAX,T2,PAP,IAX,IAX1,IRES
      IF(MOUT.GE.1.AND.IRES.EQ.0)
     1WRITE(LOU,111)r,Zz,T,PNEW,IND,IND1,IRES
      IF(IRES.EQ.0)GO TO 3
      d2=PAP
      rrNEW=rAX
      ddNEW=d2
      aanew=anew
      TTNEW=T2
      IBNEW=1
      d1=dOLD
      IF(ICR.EQ.0)GO TO 54
      ICR=0
      KREF=KREF1
      DO 88 K=1,KREF
      DO 88 L=1,2
   88 CODE(K,L)=JC(K,L)
      GO TO 54
C
C     CRITICAL RAYS. CASE IOLD=9, INEW.NE.9, INEW.NE.3.
C
   35 ITER=0
      d1=dOLD
      d2=dNEW
      IRES=0
   36 PNEW=0.5*(d1+d2)
      ITER=ITER+1
      IND=INDS
      CALL PROFIL(XSOUR,YSOUR,ZSOUR,TSOUR,PNEW,PAZM,r,XX,YY,Zz,T,
     1DT,AC,BMIN,BSTEP,BMAX,ITMAX,MOUT,NCD,METHOD,ITPR,indr1)
      IF(ITPR.EQ.43)r=Zz
      IF(MOUT.GE.2)WRITE(LOU,104)IND,IND1,ITER,r,T,PNEW
      IF(IND.EQ.9)GO TO 37
      IF(IND.EQ.ITPR)GO TO 38
      d2=PNEW
      GO TO 39
   37 IF(IND1.NE.INEW1)d2=PNEW
      IF(IND1.NE.INEW1)GO TO 39
      d1=PNEW
      IF(ITER.EQ.ITMAX.AND.KC.NE.0.AND.IRES.EQ.1)GO TO 94
      GO TO 39
   94 DO 91 K=1,KREF
      DO 91 L=1,2
   91 JC(K,L)=CODE(K,L)
      IRF3=IREF+3
      DO 92 K=IRF3,KREF
      DO 92 L=1,2
   92 CODE(K-2,L)=JC(K,L)
      KREF1=KREF
      KREF=KREF-2
      ICR=1
      ITER=ITMAX-1
      GO TO 36
   38 IF(ABS(r-rAX).LT.REPS1.AND.IRES.EQ.1)ITER=ITMAX
      IRES=1
      rAX=r
      rrold=r
      xxold=xx
      yyold=yy
      zzold=zz
      ZAX=Zz
      IAX=IND
      IAX1=IND1
      d2=PNEW
      PAP=PNEW
      T1=T
   39 IF(ITER.LT.ITMAX)GO TO 36
      IF(MOUT.GE.1.AND.IRES.EQ.1)
     1WRITE(LOU,111)rAX,ZAX,T1,PAP,IAX,IAX1,IRES
      IF(MOUT.GE.1.AND.IRES.EQ.0)WRITE(LOU,111)r,Zz,T,PNEW,IND,IND1,IRES
      IF(IRES.EQ.0)GO TO 3
      d1=PAP
      rrOLD=rAX
      ddOLD=d1
      aaold=aold
      TTOLD=T1
      IBOLD=1
      d2=dNEW
      IF(ICR.EQ.0)GO TO 59
      ICR=0
      KREF=KREF1
      DO 93 K=1,KREF
      DO 93 L=1,2
   93 CODE(K,L)=JC(K,L)
      GO TO 59
C
C
  100 FORMAT(3I3,2F10.5,F15.10)
  101 FORMAT(1X,'ITERATIONS',5X,4I3,F10.5,4F15.10)
  102 FORMAT(5X,6I3,3F10.5,F15.10)
  103 FORMAT(2X,'BOUNDARY RAY',5X,3I3,2F10.5,F15.10)
  104 FORMAT(2X,'CRITICAL RAY',5X,3I3,2F10.5,F15.10)
  105 FORMAT(2I5)
  107 FORMAT(10X,3F10.5,F15.10,3I3,5X,'BOUNDARY RAY')
  108 FORMAT(16E15.5)
  109 FORMAT(I5,9E15.5)
  110 FORMAT(1X,'XDYN',3F10.5)
  111 FORMAT(10X,3F10.5,F15.10,3I3,5X,'CRITICAL RAY')
  112 FORMAT(1X,'YDYN',3F10.5)
  113 FORMAT(7F10.5,F15.10,5I3)
  114 FORMAT(3F10.5)
  115 FORMAT(6E12.6)
  116 FORMAT(2I3,F12.7,6E12.6,F10.6)
  130 FORMAT(2I4,F13.7,6E14.6,F14.6) !!
  117 FORMAT(6E15.5)
  119 FORMAT(2H'R,I3,1H',1X,'/')
  121 FORMAT('/')
  122 FORMAT(3(F10.5,1X),'/')
C
C
   99 N=0
      NAUX=0
      IF(LURAY.NE.0)RETURN
      IF(LU1.NE.0.AND.NDST.NE.0)WRITE(LU1,100)N,NAUX
      IF(NCC.EQ.1.AND.NCOD.LT.0)INDEX=-INDEX
      IF(LU1.NE.0.AND.NDST.NE.0)WRITE(LU1,100)INDEX
      IF(INDEX.EQ.0)RETURN
      IF(INDEX.LT.0)INDEX=-INDEX
      IF(LU1.NE.0.AND.NDST.NE.0)THEN
        DO 200 I=1,INDEX
        WRITE(LU1,109)INDI(I),XCOOR(I),ZCOOR(I),TIME(I),
     1  AMPX1(I),AMPY1(I),AMPZ1(I)
        IF(NCC.EQ.1.AND.NCOD.LT.0)
     1  WRITE(LU1,117)AMPX2(I),AMPY2(I),AMPZ2(I)
        WRITE(LU1,108)(P(I,K),K=1,3)
        WRITE(LU1,108)(POL(I,K),K=1,3)
        IF(NCC.EQ.1.AND.NCOD.LT.0)
     1  WRITE(LU1,108)(POL1(I,K),K=1,3)
  200   CONTINUE
      END IF
      RETURN
      END
C
C     *********************************************************
C
      SUBROUTINE RKGS(PRMT,Y,DERY,NDIM,IHLF,FCT,OUTP,AUX)
      DIMENSION Y(18),DERY(18),AUX(8,18),A(4),B(4),C(4),PRMT(5)
      EXTERNAL FCT,OUTP
      DO 1 I=1,NDIM
    1 AUX(8,I)=.0666667*DERY(I)
      X=PRMT(1)
      XEND=PRMT(2)
      H=PRMT(3)
      PRMT(5)=0.
      CALL FCT(X,Y,DERY)
      IF(H*(XEND-X))38,37,2
   2  A(1)=.5
      A(2)=.2928932
      A(3)=1.707107
      A(4)=.1666667
      B(1)=2.
      B(2)=1.
      B(3)=1.
      B(4)=2.
      C(1)=.5
      C(2)=.2928932
      C(3)=1.707107
      C(4)=.5
      DO 3 I=1,NDIM
      AUX(1,I)=Y(I)
      AUX(2,I)=DERY(I)
      AUX(3,I)=0.
    3 AUX(6,I)=0.0
      IREC=0
      H=H+H
      IHLF=-1
      ISTEP=0
      IEND=0
    4 IF((X+H-XEND)*H)7,6,5
    5 H=XEND-X
    6 IEND=1
    7 CALL OUTP(X,Y,DERY,IREC,NDIM,PRMT)
      IF(PRMT(5))40,8,40
    8 ITEST=0
    9 ISTEP=ISTEP+1
      J=1
   10 AJ=A(J)
      BJ=B(J)
      CJ=C(J)
      DO 11 I=1,NDIM
      R1=H*DERY(I)
      R2=AJ*(R1-BJ*AUX(6,I))
      Y(I)=Y(I)+R2
      R2=R2+R2+R2
   11 AUX(6,I)=AUX(6,I)+R2-CJ*R1
      IF(J-4)12,15,15
   12 J=J+1
      IF(J-3)13,14,13
   13 X=X+.5*H
   14 CALL FCT(X,Y,DERY)
      GO TO 10
   15 IF(ITEST)16,16,20
   16 DO 17 I=1,NDIM
   17 AUX(4,I)=Y(I)
      ITEST=1
      ISTEP=ISTEP+ISTEP-2
   18 IHLF=IHLF+1
      X=X-H
      H=.5*H
      DO 19 I=1,NDIM
      Y(I)=AUX(1,I)
      DERY(I)=AUX(2,I)
   19 AUX(6,I)=AUX(3,I)
      GO TO 9
   20 IMOD=ISTEP/2
      IF(ISTEP-IMOD-IMOD)21,23,21
   21 CALL FCT(X,Y,DERY)
      DO 22 I=1,NDIM
      AUX(5,I)=Y(I)
   22 AUX(7,I)=DERY(I)
      GO TO 9
   23 DELT=0.
      DO 24 I=1,NDIM
   24 DELT=DELT+AUX(8,I)*ABS(AUX(4,I)-Y(I))
      IF(DELT-PRMT(4))28,28,25
   25 IF(IHLF-10)26,36,36
   26 DO 27 I=1,NDIM
   27 AUX(4,I)=AUX(5,I)
      ISTEP=ISTEP+ISTEP-4
      X=X-H
      IEND=0
      GO TO 18
   28 CALL FCT(X,Y,DERY)
      DO 29 I=1,NDIM
      AUX(1,I)=Y(I)
      AUX(2,I)=DERY(I)
      AUX(3,I)=AUX(6,I)
      Y(I)=AUX(5,I)
   29 DERY(I)=AUX(7,I)
      CALL OUTP(X-H,Y,DERY,IHLF,NDIM,PRMT)
      IF(PRMT(5))40,30,40
   30 DO 31 I=1,NDIM
      Y(I)=AUX(1,I)
   31 DERY(I)=AUX(2,I)
      IREC=IHLF
      IF(IEND)32,32,39
   32 IHLF=IHLF-1
      ISTEP=ISTEP/2
      H=H+H
      IF(IHLF)4,33,33
   33 IMOD=ISTEP/2
      IF(ISTEP-IMOD-IMOD)4,34,4
   34 IF(DELT-.02*PRMT(4))35,35,4
   35 IHLF=IHLF-1
      ISTEP=ISTEP/2
      H=H+H
      GO TO 4
   36 IHLF=11
      CALL FCT(X,Y,DERY)
      GO TO 39
   37 IHLF=12
      GO TO 39
   38 IHLF=13
   39 CALL OUTP(X,Y,DERY,IHLF,NDIM,PRMT)
   40 RETURN
      END
C
C     *********************************************************
C
      SUBROUTINE ROOT(XA,FXYZA,XB,FXYZB,XINT,PRMT,ITP)
C
C     ROUTINE FOR THE ITERATIVE COMPUTATION OF THE POINTS OF
C     INTERSECTION OF RAYS WITH INTERFACES
C
      DIMENSION YINT(3),YDINT(3),PRMT(5),DEP(6)
      COMMON /VSP/XVSP,YVSP,XNRM,YNRM,ICOD,IVSP
      COMMON /AUXI/  IANI(20),INTR,INT1,IPREC,KRE,IREFR,LAY,NDER,IPRINT,
     1 MPRINT,NTR,ISQRT,NAUX,ISOUR,MAUX,MREG,MDIM,IPOL,MSCON,LOU,
     2 IAMP,MTRNS,ICOEF,IAD,IRHO,ISHEAR,IAC,IRT,mori
C
      IAC=0
      ISIGN=-1
      XXINT=0.
    1 IAC=IAC+1
      ISIGN=-ISIGN
      AUX=FXYZA-FXYZB
      IF(ABS(AUX).LT..000001)XINT=.5*(XA+XB)
      IF(ABS(AUX).GE..000001.AND.ISIGN.GT.0)XINT=(FXYZA*XB-FXYZB*XA)/AUX
      IF(ABS(AUX).GE..000001.AND.ISIGN.LT.0)THEN
        IF(ITP.GT.0)AUX=DEP(2)*YDINT(1)+DEP(3)*YDINT(2)-YDINT(3)
        IF(ITP.LT.0)AUX=XNRM*YDINT(1)+YNRM*YDINT(2)
        IF(ABS(AUX).LT..000001)XINT=.5*(XA+XB)
        IF(ABS(AUX).GE..000001)XINT=XXINT-AUX3/AUX
      END IF
      IF(XINT.LT.XA.OR.XINT.GT.XB)XINT=.5*(XA+XB)
      IF(IRT.EQ.2)
     1WRITE(LOU,'(A,/,4E15.9,I5,/)') ' XA,XB,XINTOLD,XINT,IAC',
     2XA,XB,XXINT,XINT,IAC
      IF(IAC.GT.1.AND.ABS(XINT-XXINT).LT.PRMT(4))RETURN
      IF(IAC.GE.10)RETURN
      CALL APPROX(XINT,YINT,YDINT,3)
      XXINT=XINT
      IF(ITP.GT.0)THEN
        CALL DISC(YINT,DEP)
        AUX3=DEP(1)-YINT(3)
      ELSE
        AUX3=(YINT(1)-XVSP)*XNRM+(YINT(2)-YVSP)*YNRM
      END IF
      IF((FXYZA*AUX3).GT.0.)THEN
        XA=XINT
        FXYZA=AUX3
        GO TO 1
      ELSE
        XB=XINT
        FXYZB=AUX3
        GO TO 1
      END IF
      END
C                                                                 </PRE>
