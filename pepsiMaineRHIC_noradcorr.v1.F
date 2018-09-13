        program pepsiMaineRHIC

        implicit none
        include "leptou.inc"
        include "lujets.inc"
        include "ludat1.inc"
        include "mcRadCor.inc"
        include "phiout.inc"
        include "mc_set.inc"

        integer nevgen, i, j, lastgenevent, genevent
        integer IEV, NEV, NPRT
        integer idum1, idum2, initseed
        integer ltype, tracknr, T1, T2
        integer*4 today(3), now(3)
        real rval, alpha
        real mcfixedweight, mcextraweight
        double precision dilut, F1, F2, R, dA1, dA2
        double precision gendilut, genF1, genF2, genR, genA1, genA2
        DOUBLE PRECISION radgamEnucl
        DOUBLE PRECISION pbeamE, pbeta, pgamma, ebeamE, epznucl
        DOUBLE PRECISION depol, gamma2, d, eta, eps, chi
        DOUBLE PRECISION gendepol, gend, geneta, geneps, genchi
        DOUBLE PRECISION DBRACK, dxsec

        CHARACTER PARAM*20

c---------------------------------------------------------------------
c     ASCII output file
c ---------------------------------------------------------------------
        integer asciiLun
        parameter (asciiLun=29)
        CHARACTER*256 outputfilename
        CHARACTER*256 outname

        alpha = 1./137.
C Initialize event generation counter
        qedrad = 0
        nevgen = 0
        genevent = 0
        lastgenevent = 0.
        dilut= 1.
        R = 0.
        depol =0. 
        dA1 = 0.
        dA2 = 0.
        F1 = 0.
        F2 = 0.
        gamma2= 1.
        d = 0.
        eta = 0.
        eps = 0.
        chi = 0.
        gendilut= 1.
        genR = 0.
        gendepol =0.
        genA1 = 0.
        genA2 = 0.
        genF1 = 0.
        genF2 = 0.
        gend = 0.
        geneta = 0.
        geneps = 0.
        genchi = 0.
        radgamEnucl = 0.
        mcfixedweight=1.
        mcextraweight=1.
        weight=1.
C Init all the parameters and read them in 
        ltype = 11
        pbeam = 100.
        ebeam = 4.

C...Read parameters for LINIT call
C...Read output file name
       READ(*,*) outname
C...Read beam target particle type
       READ(*,*) ltype
C...Read beam and target particle energy
       READ(*,*) pbeam, ebeam
C...Read number of events to generate, and to print.
       READ(*,*) NEV,NPRT
C...Read min/max x of radgen lookup table
       READ(*,*) mcSet_XMin, mcSet_XMax
C...Read min/max y of generation range      
       READ(*,*) mcSet_YMin, mcSet_YMax
C...Read min/max Q2 of generation range      
       READ(*,*) mcSet_Q2Min, mcSet_Q2Max
C...Read information for cross section used in radgen
       READ(*,*) genSet_FStruct, genSet_R
C...Read switch for rad-corrections
       READ(*,*) qedrad
C...Read min/max W2 of generation range      
       READ(*,*) mcSet_W2Min,mcSet_W2Max
C...Read min/max Nu of generation range      
       READ(*,*) mcSet_NuMin,mcSet_NuMax
C...Read Beam and Target polarisation direction
       READ(*,*) mcset_PBValue, mcset_PTValue
C...Read Beam and Target polarisation 
       READ(*,*) mcset_PBeam, mcset_PTarget
C...Read all the pepsi/lepto parameters
  100  READ(*,*, END=200) PARAM, j, rval
       if (PARAM.eq.'LST') then
          LST(j)=rval
          write(*,*) 'LST(',j,')=',rval
       endif
       if (PARAM.eq.'PARL') then
          PARL(j)=rval
          write(*,*) 'PARL(',j,')=',rval
       endif
       if (PARAM.eq.'PARJ') then
          PARJ(j)=rval
          write(*,*) 'PARJ(',j,')=',rval
       endif
       if (PARAM.eq.'MSTJ') then
          MSTJ(j)=rval
          write(*,*) 'MSTJ(',j,')=',rval
       endif
       if (PARAM.eq.'MSTU') then 
          MSTU(j)=rval
          write(*,*) 'MSTU(',j,')=',rval
       endif
       if (PARAM.eq.'PARU') then
          PARU(j)=rval
          write(*,*) 'PARU(',j,')=',rval
       endif
       GOTO 100
  200  write(*,*) '*********************************************'
       write(*,*) 'NOW all parameters are read by PEPSI'
       write(*,*) '*********************************************'

C ... PEPSI polarization
        if (mcset_PBValue*mcset_PTValue.lt.0.) then
          lst(40) = 1.
        elseif (mcset_PBValue*mcset_PTValue.gt.0.) then
          lst(40) = -1.
        else
          lst(40) = 0.
        endif

C ... polarization for helium 3 !
C     use lst(39)=1 for ideal helium 3 (no S prime and D state)
C     use lst(39)=3 or (02 should work too ?) for a real helium 3.
C       lst(39) = 0
        if (lst(40).ne.0.and.parl(1).eq.3.and.parl(2).eq.2) then
          lst(39) = 3
          lst(36) = 1
        endif

C ... xmin,xmax
        cut(1)=mcSet_XMin
        cut(2)=mcSet_XMax
C ... ymin,ymax
        cut(3)=mcSet_YMin
        cut(4)=mcSet_YMax
C ... Q2min, Q2max
        cut(5)=mcSet_Q2Min
        cut(6)=mcSet_Q2Max
C ... W2min, W2max
        cut(7)=mcSet_W2Min
C        cut(8)=mcSet_W2Max
C ... numin, numax
C        cut(9)=mcSet_NuMin
C        cut(10)=mcSet_NuMax

        mcSet_TarA=PARL(1)
        mcSet_TarZ=PARL(2)

C     Getting the date and time of the event generation
      call idate(today)   ! today(1)=day, (2)=month, (3)=year
      call itime(now)     ! now(1)=hour, (2)=minute, (3)=second
C      
C     Take date as the SEED for the random number generation
      initseed = today(1) + 10*today(2) + today(3) + now(1) + 5*now(3)
      write(6,*) 'SEED = ', initseed 
      call rndmq (idum1,idum2,initseed,' ')

      if (abs(ltype).eq.11) then
         masse=0.000510998910
      else
        write(*,*) "Only positron and electron beam supported"
      endif
      
      if ((PARL(1).eq.1).and.(PARL(2).eq.1)) then
         massp=0.938272013
      elseif ((PARL(1).eq.1).and.(PARL(2).eq.0)) then 
         massp=0.93956536
      elseif ((PARL(2).eq.2).and.(PARL(2).eq.1)) then
         massp=1.875612793
      else
        write(*,*) "Currently only p,n,d as target supported"
      endif

      pbeamE=sqrt(pbeam**2+massp**2)
      pbeta=pbeam/pbeamE
      pgamma=pbeamE/massp
      ebeamE=sqrt(ebeam**2+masse**2)
      ebeamEnucl=pgamma*ebeamE-pgamma*pbeta*(-ebeam)
      epznucl=-pgamma*pbeta*(ebeamE)+pgamma*(-ebeam)
      write(*,*) ebeamEnucl, ebeamE, epznucl, -ebeam
      mcSet_EneBeam=sngl(ebeamEnucl)

C Initialize PEPSI
       CALL TIMEX(T1)
       call linit(0,ltype,(-ebeam),pbeam,LST(23))
       CALL TIMEX(T2)

c ---------------------------------------------------------------------
c     Open ascii output file
c ---------------------------------------------------------------------
       outputfilename=outname
       open(asciiLun, file=outputfilename)
       write(*,*) 'the outputfile will be named: ', outname

C The total integrated xsec, in pbarns
       mcextraweight = parl(23)

! ============== Generate until happy (past cuts) =================

C   This is what we write in the ascii-file

        write(29,*)' PEPSI EVENT FILE '
        write(29,*)'============================================'
        write(29,30)
30      format('I, ievent, genevent, process, subprocess, nucleon,
     &  struckparton, partontrck, trueY, trueQ2, trueX, trueW2,
     &  trueNu, FixedWeight, weight, dxsec, Extraweight, 
     &  dilut, F1, F2, A1, A2, R, Depol, d, eta, eps, chi, 
     &  gendilut, genF1, genF2, genA1, genA2, genR, genDepol, gend,
     &  geneta, geneps, genchi, Sigcor, radgamEnucl, nrTracks')

        write(29,*)'============================================'
        write(29,*)' I  K(I,1)  K(I,2)  K(I,3)  K(I,4)  K(I,5)
     &  P(I,1)  P(I,2)  P(I,3)  P(I,4)  P(I,5)  V(I,1)  V(I,2)  V(I,3)'
        write(29,*)'============================================'

       DO 300 IEV=1,NEV

C Count generated events
10      nevgen = nevgen+1

        if (qedrad.eq.0) then
          If (IEV.eq.1) then
              write(*,*) 'Generating full event with LEPTO/PEPSI.'
          endif
          DO I=1,MSTU(4)
           DO J=1,5
              K(I,J)=0.
              P(I,J)=0.
              V(I,J)=0.
           enddo
          enddo
          lst(7) = 1
          call lepto
          if (lst(21).ne.0) then
              write(*,*) 'PEPSI failed, trying again, 
     &                    no rad-corrections'
              goto 10
          endif
          dA2=0.
          dA1=0.
          F1=0.
          F2=0.
          dilut=1.
          R=0.
          depol=0.
          gamma2=0.
          d=0.
          eta=0.
          eps=0.
          chi=0.
          genevent=nevgen-lastgenevent

        endif

         DO I=1,MSTU(4)
            if ((K(I,1).eq.0).and.(K(I,2).eq.0).and.(K(I,3).eq.0).and.
     &          (K(I,4).eq.0).and.(K(I,5).eq.0)) then
                tracknr=I-1
            goto 400
            endif
         ENDDO
400    continue

C Get some info for the event
       call mkasym (dble(q2), dble(X), mcSet_TarA, mcSet_TarZ, dA1, dA2)
       call mkf2(dble(q2), dble(X), mcSet_TarA, mcSet_TarZ, F2, F1)
       call mkR(dble(q2), dble(X), R)
       call fdilut(dble(Q2), dble(X), mcSet_TarA, dilut)

C... Calculate some kinematic variables
       gamma2=q2/u**2
       depol=(y*((1.+gamma2*y/2.)*(2.-y)-2*y**2*masse**2/Q2))/
     &       (y**2*(1.-2.*masse**2/Q2)*(1.+gamma2)+2.*(1.+R)*
     &       (1.-y-gamma2*(y**2)/4.))
       d=depol*((sqrt(1.-y-gamma2*(y**2)/4.)*(1+gamma2*y/2.)) /
     &   (1.-y/2.)*(1+gamma2*y/2.)-y**2*masse**2/q2)
       eta=(sqrt(gamma2)*(1.-y-gamma2*y**2/4.-y**2*masse**2/q2)) /
     &     ((1.+gamma2**2*y/2.)*(1-y/2.)-y**2*masse**2/q2)
       chi=(sqrt(gamma2)*(1.-y/2.-y**2.*masse**2/q2))/(1.+gamma2*y/2.)
       eps=1./(1.+1./2.*(1.-2.*masse**2/Q2)*((y**2+gamma2*y**2)/
     &        (1.-y-gamma2*(y**2)/4.)))

        DBRACK = (4.*PARU(1)*alpha**2)/q2**2
c cross section in hbar^2c^2/GeV^2/dQ^2/dx
        dxsec = (1.-y-(massp*x*y/2./ebeamE))*F2/x+y**2*F1
c cross sectin in mbarn/dQ^2/dx
        dxsec = DBRACK * dxsec * 0.389379292

         write(29,32) 0, IEV, genevent, LST(23), LST(24), LST(22),
     &   LST(25), LST(26), Y, Q2, X, W2, U, mcfixedweight, weight, 
     &   dxsec, mcextraweight, real(dilut), real(F1), 
     &   real(F2), real(dA1), real(dA2), real(R), real(Depol), real(d), 
     &   real(eta), real(eps), real(chi), real(gendilut), real(genF1),
     &   real(genF2), real(genA1), real(genA2), real(genR), 
     &   real(genDepol), real(gend), real(geneta), real(geneps), 
     &   real(genchi),  mcRadCor_Sigcor, radgamEnucl, tracknr
 32      format((I4,1x,$),(I10,1x,$),6(I6,1x,$),
     &          33(E18.10,1x,$),I12,/)
         write(29,*)'============================================'

         DO I=1,tracknr
         write(29,34) I,K(I,1),K(I,2),K(I,3),K(I,4),K(I,5),
     &        P(I,1),P(I,2),P(I,3),P(I,4),P(I,5),
     &        V(I,1),V(I,2),V(I,3)
         ENDDO
 34      format(2(I6,1x,$),I10,1x,$,3(I6,1x,$),8(f15.6,1x,$),/)
         write(29,*)'=============== Event finished ==============='

         lastgenevent=nevgen

  300    CONTINUE

C...Print the Pythia cross section which is needed to get an absolut 
C   normalisation the number is in pbarns
       write(*,*)'==================================================='
       write(*,*)'Pepsi total cross section in pb from numerical', 
     &            ' integration', parl(23)
       write(*,*)'Pepsi total cross section in pb from MC simulation',
     &            parl(24)
       write(*,*)'Total Number of generated events', NEV
       write(*,*)'Total Number of trials', nevgen
       write(*,*)'==================================================='
       close(29)
       END
