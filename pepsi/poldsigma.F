*72*********************************************************************

       FUNCTION POLDSIGMA(XP)

       IMPLICIT NONE
C...Differential cross section for first order QCD processes.


*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      REAL PSAVE,XMIN,XMAX,YMIN,YMAX,Q2MIN,Q2MAX,W2MIN,W2MAX
      INTEGER KSAVE,ILEP,INU,IG,IZ
      SAVE /LINTRL/

      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/

      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /LUDAT1/


      REAL POLDSIGMA,XP,DQCDI,POLDQCDI,ULMASS
      INTEGER I,IH,IL,IU,IP
      REAL PQH,AMU,XI,ZPMIN,ZPMAX,XPQ,WQ,WQB,TQ,TQB,T1,S13,SGN,XDPQ,SIG
*...Added array XDPQ to store delta parton distributions.      
      DIMENSION XPQ(-6:6),XDPQ(-6:6),PQH(17,2)
*___
      POLDSIGMA=0.
      DO 10 I=1,17
      PQH(I,1)=0.
      PQH(I,2)=0.
   10 PQ(I)=0.

      MSTJ(93)=1
      AMU=ULMASS(2)
*PEPSI>>
        IL=1
        IU=3
*PEPSI<<
      XI=X/XP
C...Scheme for ME cutoff: W2, Q2, mixed
      IF(LST(20).LE.1) THEN
        ZPMIN=(1.-X)*XP/(XP-X)*PARL(27)
      ELSEIF(LST(20).EQ.2) THEN
        ZPMIN=X*XP/(XP-X)*PARL(27)
      ELSEIF(LST(20).GE.3.AND.LST(20).LE.5) THEN
        ZPMIN=PARL(27)
      ELSEIF(LST(20).GE.6) THEN
        ZPMIN=PARL(8)
      ENDIF
      IF(ZPMIN.LE.0..OR.ZPMIN.GE.0.5) RETURN
      ZPMAX=1.D0-DBLE(ZPMIN)
      CALL LNSTRF(XI,Q2,XPQ)
      CALL DNSTRF(XI,Q2,XDPQ)
      IF(LST(24).EQ.3) GOTO 3000

C...Gluon bremsstrahlung process, i.e. qg-event.
 2000 DO 2500 IP=IL,IU
      SIG=DQCDI(1,IP,XP,ZPMIN,ZPMAX)
*PEPSI>>
      IF (IP.EQ.3) THEN
      SIG=POLDQCDI(1,IP,XP,ZPMIN,ZPMAX)
      ENDIF
*PEPSI<<
      SGN=SIGN(1.,5.-2.*IP)
      DO 2300 IH=1,2
      IF(IH.EQ.1) THEN
        IF(PARL(6).GT.0.99) GOTO 2300
        IF(LST(32).EQ.0.AND.LST(30).NE.-1) GOTO 2300
      ELSEIF(IH.EQ.2) THEN
        IF(PARL(6).LT.-0.99) GOTO 2300
        IF(LST(32).EQ.0.AND.LST(30).NE.1) GOTO 2300
      ENDIF
      IF(LST(32).NE.0) LST(30)=SIGN(1.,IH-1.5)
      IF(LST(23).NE.2) THEN
        DO 2100 I=1,LST(12)
        WQ=XPQ(I)*SIG*(EWQC(1,IH,I)+SGN*EWQC(2,IH,I))
        WQB=XPQ(-I)*SIG*SGN*(EWQC(1,IH,I)+SGN*EWQC(2,IH,I))
*...EM polarized case: if IP=3 use proper formula for
*...the interference part of cross section.
*PEPSI>>
        IF(IP.EQ.3)THEN
          WQ=LST(40)*XDPQ(I)*SIG*(EWQC(1,IH,I)+EWQC(2,IH,I))
          WQB=LST(40)*XDPQ(-I)*SIG*(EWQC(1,IH,I)+EWQC(2,IH,I))
        ENDIF
*PEPSI<<
C...Include y-dependence.
        WQ=WQ*PARI(23+IP)
        WQB=WQB*PARI(23+IP)
        PQH(I,IH)=PQH(I,IH)+WQ
        PQH(I+LST(12),IH)=PQH(I+LST(12),IH)+WQB
        PQH(17,IH)=PQH(17,IH)+WQ+WQB
 2100   CONTINUE
      ELSEIF(LST(23).EQ.2) THEN
C...Zero CC cross-section for one helicity state.
        IF(KSAVE(1).LT.0.AND.IH.EQ.1
     &  .OR.KSAVE(1).GT.0.AND.IH.EQ.2) GOTO 2300
        IF(IP.EQ.3) THEN
          TQ=-LST(30)
          TQB=-TQ
        ELSE
          TQ=1.
          TQB=1.
        ENDIF
        DO 2200 I=1,LST(12)
        T1=-K(3,2)*QC(I)
        IF(T1.GT.0) THEN
          WQ=XPQ(I)*SIG*TQ
          WQB=0.
        ELSE
          WQB=XPQ(-I)*SIG*TQB
          WQ=0.
        ENDIF
C...Include y-dependence.
        WQ=WQ*PARI(23+IP)
        WQB=WQB*PARI(23+IP)
        PQH(I,IH)=PQH(I,IH)+WQ
        PQH(I+LST(12),IH)=PQH(I+LST(12),IH)+WQB
        PQH(17,IH)=PQH(17,IH)+WQ+WQB
 2200   CONTINUE
        ENDIF
 2300 CONTINUE
 2500 CONTINUE
      DO 2600 I=1,17
 2600 PQ(I)=(1.-PARL(6))/2.*PQH(I,1)+(1.+PARL(6))/2.*PQH(I,2)
      IH=1
      IF(LST(30).EQ.1) IH=2
      IF(LST(32).EQ.0) THEN
C...Simulation: cross section for chosen helicity state only.
        POLDSIGMA=PQH(17,IH)
      ELSEIF(LST(19).EQ.-1) THEN
C...Integration event-by-event: normalize and include alpha_s*1/(1-xp)
        POLDSIGMA=PQH(17,IH)/PARI(20)*PARL(25)/(1.-XP)
C...Max of dsigma/dxp for L- and R-handed lepton.  
        IF(PQH(17,1).GT.PARI(15)) PARI(15)=PQH(17,1)
        IF(PQH(17,2).GT.PARI(16)) PARI(16)=PQH(17,2)
      ELSE
C...Integration: normalize and include alpha_s*1/(1-xp)
        POLDSIGMA=PQH(17,IH)/PARI(20)*PARL(25)/(1.-XP)
        IF(LST(17).EQ.0.AND.LST(40).EQ.0) THEN
C...Fixed beam energy, max of dsigma/dxp for L- and R-handed lepton.
          IF(PQH(17,1).GT.PARI(15)) PARI(15)=PQH(17,1)
          IF(PQH(17,2).GT.PARI(16)) PARI(16)=PQH(17,2)
      ELSE
C...Variable beam energy or polarizzed case, max of dsigma/dxp 
*...for S, T, I contributions.
        IF(PQ(17)/PARI(23+LST(32)).GT.PARI(14+LST(32)))
     &    PARI(14+LST(32))=PQ(17)/PARI(23+LST(32))
        ENDIF
      ENDIF
      RETURN
C...Boson-gluon fusion, i.e. qq-event.
 3000 S13=Q2*(1.-XP)/XP
      IF(S13.LT.4.*AMU**2) RETURN
      DO 3500 IP=IL,IU
*PEPSI>>
*HI      SIG=XPQ(0)*DQCDI(2,IP,XP,ZPMIN,ZPMAX)
      IF(IP.EQ.3.) THEN
         SIG=FLOAT(LST(40))*XDPQ(0)*POLDQCDI(2,IP,XP,ZPMIN,ZPMAX)
      ELSE
         SIG=XPQ(0)*DQCDI(2,IP,XP,ZPMIN,ZPMAX)
      ENDIF
*PEPSI<<
      DO 3300 IH=1,2
      IF(IH.EQ.1) THEN
      IF(PARL(6).GT.0.99) GOTO 3300
      IF(LST(32).EQ.0.AND.LST(30).NE.-1) GOTO 3300
      ELSEIF(IH.EQ.2) THEN
      IF(PARL(6).LT.-0.99) GOTO 3300
      IF(LST(32).EQ.0.AND.LST(30).NE.1) GOTO 3300
      ENDIF
      IF(LST(32).NE.0) LST(30)=SIGN(1.,IH-1.5)
      IF(LST(23).NE.2) THEN
      DO 3100 I=1,LST(13)
        MSTJ(93)=1
      IF(S13.LT.4.*ULMASS(I)**2) GOTO 3100
      WQ=SIG/2.*(EWQC(1,IH,I)+EWQC(2,IH,I))
      WQB=WQ
C...Include y-dependence.
      WQ=WQ*PARI(23+IP)
      WQB=WQB*PARI(23+IP)
      PQH(I,IH)=PQH(I,IH)+WQ
      PQH(I+LST(13),IH)=PQH(I+LST(13),IH)+WQB
      PQH(17,IH)=PQH(17,IH)+WQ+WQB
 3100   CONTINUE
      ELSEIF(LST(23).EQ.2) THEN
C...Zero CC cross-section for one helicity state.
      IF(KSAVE(1).LT.0.AND.IH.EQ.1
     &  .OR.KSAVE(1).GT.0.AND.IH.EQ.2) GOTO 3300
      DO 3200 I=1,LST(13)
        MSTJ(93)=1
      IF(S13.LT.(AMU+ULMASS(I))**2) GOTO 3200
      IF(K(3,2)*QC(I).LT.0) THEN
        WQ=SIG
        WQB=0.
      ELSE
        WQB=SIG
        WQ=0.
      ENDIF
C...Include y-dependence.
      WQ=WQ*PARI(23+IP)
      WQB=WQB*PARI(23+IP)
      PQH(I,IH)=PQH(I,IH)+WQ
      PQH(I+LST(13),IH)=PQH(I+LST(13),IH)+WQB
      PQH(17,IH)=PQH(17,IH)+WQ+WQB
 3200   CONTINUE
      ENDIF
 3300 CONTINUE
 3500 CONTINUE
      DO 3600 I=1,17
 3600 PQ(I)=(1.-PARL(6))/2.*PQH(I,1)+(1.+PARL(6))/2.*PQH(I,2)
      IH=1
      IF(LST(30).EQ.1) IH=2
      IF(LST(32).EQ.0) THEN
C...Simulation: cross section for chosen helicity state only.
        POLDSIGMA=PQH(17,IH)
      ELSEIF(LST(19).EQ.-1) THEN
C...Integration event-by-event: normalize and include alpha_s*1/(1-xp)
        POLDSIGMA=PQH(17,IH)/PARI(20)*PARL(25)/(1.-XP)
C...Max of dsigma/dxp for L- and R-handed lepton.  
        IF(PQH(17,1).GT.PARI(18)) PARI(18)=PQH(17,1)
        IF(PQH(17,2).GT.PARI(19)) PARI(19)=PQH(17,2)
      ELSE
C...Integration for grid: normalize and include alpha_s*1/(1-xp)
      POLDSIGMA=PQ(17)/PARI(20)*PARL(25)/(1.-XP)
      IF(LST(17).EQ.0.AND.LST(40).EQ.0) THEN
*---
C...Fixed beam energy, max of dsigma/dxp for L- and R-handed lepton.
        IF(PQH(17,1).GT.PARI(18)) PARI(18)=PQH(17,1)
        IF(PQH(17,2).GT.PARI(19)) PARI(19)=PQH(17,2)
      ELSE
C...Variable beam energy, max of dsigma/dxp for S, T, I contributions.
        IF(PQ(17)/PARI(23+LST(32)).GT.PARI(17+LST(32)))
     &    PARI(17+LST(32))=PQ(17)/PARI(23+LST(32))
      ENDIF

      ENDIF
      RETURN
      END
