
      subroutine radgen_init(bUseLUT,bGenLUT)

      implicit none

       include "mc_set.inc"
       include "density.inc"
       include "cmpcom.inc"
       include "tailcom.inc"
       include "radgen.inc"
       include "radgenkeys.inc"

      logical bUseLUT,bGenLUT
      CHARACTER*256 tname
      integer aaa

! ... force block data modules to be read
      external radata

*     kill_elas_res =0, all rad corrections
*     kill_elas_res =1, ???
*     kill_elas_res =2, no rad corrections
      kill_elas_res=0
      aaa=0

*     initialise the rad. correction part !
*     =  2 : fint cernlib extrapolation routine
*     =  1 : 2dim spline
*     =  0 : make lookuptable
*     = -1 : do not lookup table , calculate all events
      if( bUseLUT ) then
        if( bGenLUT ) then
          ixytest = 0
        else
          ixytest = 2
        endif
      else
        ixytest = -1
      endif

*----------------------------
*     ire is target 1,2,3
*     now also 14 (nitrogen) and 84 (krypton)
*     add neutron target
      if(     mcSet_TarA .eq. 1 .and. mcSet_TarZ .eq. 0 ) then
        ire = 0
      elseif( mcSet_TarA .eq. 1 .and. mcSet_TarZ .eq. 1 ) then
        ire = 1
      elseif( mcSet_TarA .eq. 2 .and. mcSet_TarZ .eq. 1 ) then
        ire = 2
      elseif( mcSet_TarA .eq. 3 .and. mcSet_TarZ .eq. 2 ) then
        ire = 3
      elseif( mcSet_TarA .eq. 4 .and. mcSet_TarZ .eq. 2 ) then
        ire = 4
      elseif( mcSet_TarA .eq. 14 .and. mcSet_TarZ .eq. 7 ) then
        ire = 14
      elseif( mcSet_TarA .eq. 20 .and. mcSet_TarZ .eq. 10 ) then
        ire = 20
      elseif( mcSet_TarA .eq. 84 .and. mcSet_TarZ .eq. 36 ) then
        ire = 84
      else
        write(*,*)( 'RADGEN_INIT: invalid target selection' )
      endif

! We handle the following target and beam polarizations 

      if (mcSet_PBeam(1:1).ne.'L') then
        write(*,*)('RADGEN_INIT: '//
     +   'Only longitudinal polarized Beam (PBeam) makes sense')
      endif
      if ((mcSet_PTarget(1:1).ne.'L').and.
     +    (mcSet_PTarget(1:1).ne.'T').and.
     +    (mcSet_PTarget(1:2).ne.'DT')) then
        write(*,*)('RADGEN_INIT: '//
     +   'Check your value for the Target polarization(PTarget)')
      endif

*----------------------------
*     plrun : beam polarisation    [-1.,+1.]
*     pnrun : nucleon polarisation [-1.,0,+1.]

C if this code would be used with pepsi than you can calculate rad-corrections
C for all this target and beam polarisation settings.

      IF (((mcset_PBValue*mcset_PTValue).lt.0).and.
     &         (mcSet_PTarget(1:1).eq.'L'))  THEN
*  larger cross section state (anti parallel )
         plrun = -1.
         pnrun = 1.
         tname='radgen/xytab0ant.ee.ppp.dat'
      ELSEIF (((mcset_PBValue*mcset_PTValue).gt.0).and.
     &         (mcSet_PTarget(1:1).eq.'L')) THEN
*  smaller cross section state ( parallel )
         plrun = -1.
         pnrun = -1.
         tname='radgen/xytab0par.ee.ppp.dat'
      ELSEIF (((mcset_PBValue*mcset_PTValue).eq.0).and.
     &         (mcSet_PTarget(1:2).eq.'DT')) THEN
*  cross section state for tensor polarised deuterium
         plrun = 0.
         pnrun = mcSet_PTValue * 2.
         if (mcSet_PTValue.lt.0.) then
             tname='radgen/xytab0tenant.dat'
         elseif (mcSet_PTValue.gt.0.) then
             tname='radgen/xytab0tenpar.dat'
         endif
      ELSEIF (((mcSet_PBValue*mcSet_PTValue).lt.0.).and.
     +         (mcSet_PTarget(1:1).eq.'T')) THEN
*  larger cross section state (anti parallel ) transverse proton
         plrun = -1.
         pnrun = 1.
         tname='radgen/xyten0ant.ee.ppp.dat'
      ELSEIF (((mcSet_PBValue*mcSet_PTValue).gt.0.).and.
     +        (mcSet_PTarget(1:1).eq.'T')) THEN
*  smaller cross section state ( parallel ) transverse proton
         plrun = -1.
         pnrun = -1.
         tname='radgen/xyten0par.ee.ppp.dat'
      ELSE
         plrun = 0.
         pnrun = 0.
         if(ire.lt.10) then
            tname='radgen/xytab0unp.ee.ppp.dat'
         else
            tname='radgen/xytab00unp.ee.ppp.dat'
         endif
      ENDIF

      if (ire.lt.10) then
          write(tname(13:13),'(i1)')ire
          if ((ebeam.gt.0).and.(ebeam.lt.10)) then
            write(tname(18:18),'(i1)')aaa
            write(tname(19:19),'(i1)')int(ebeam)
          endif
          if (ebeam.ge.10) then
            write(tname(18:19),'(i2)')int(ebeam)
          endif
          if ((pbeam.ge.10).and.(pbeam.lt.100)) then
            write(tname(21:21),'(i1)')aaa
            write(tname(22:23),'(i2)')int(pbeam)
          endif
          if (pbeam.ge.100) then
            write(tname(21:23),'(i3)')int(pbeam)
          endif
      else
          write(tname(13:14),'(i2)')ire
          if ((ebeam.gt.0).and.(ebeam.lt.10)) then
            write(tname(19:19),'(i1)')aaa
            write(tname(20:20),'(i1)')int(ebeam)
          endif
          if (ebeam.ge.10) then
            write(tname(19:20),'(i2)')int(ebeam)
          endif
          if ((pbeam.ge.10).and.(pbeam.lt.100)) then
            write(tname(22:22),'(i1)')aaa
            write(tname(23:24),'(i2)')int(pbeam)
          endif
          if (pbeam.ge.100) then
            write(tname(22:24),'(i3)')int(pbeam)
          endif
      endif


*----------------------------
* grid of important regions in theta (7*ntk)
      ntk = 35
*----------------------------
* photonic energy grid
      nrr = 100
*----------------------------
* min. energy in the calo (resolution parameter)
* as the Hermes calorimeter can only see single photons from a minimum
* energy of 0.5 GeV should the parameter not be changed from 0.1 
* to 0.5 GeV (E.C.A)
      demin=0.10

*----------------------------
      ap=2.*amp
      amp2=amp**2
      ap2=2.*amp**2
      if(kill_elas_res.eq.1)amc2=4d0

      if(ire.eq.0) then
        amt=.939565d0
        rtara=1d0
        rtarz=0d0
        fermom=0d0
      elseif(ire.eq.1)then
        amt=.938272d0
        rtara=1d0
        rtarz=1d0
        fermom=0d0
      elseif(ire.eq.2)then
        amt=1.87561d0
        rtara=2d0
        rtarz=1d0
        fermom=.07d0
      elseif(ire.eq.3)then
        amt=2.80923d0
        rtara=3d0
        rtarz=2d0
        fermom=.164d0
      elseif(ire.eq.4)then
        amt=3.72742d0
        rtara=4d0
        rtarz=2d0
        fermom=.164d0
      elseif(ire.eq.14)then
        amt=13.0438d0
        rtara=14d0
        rtarz=7d0
        fermom=.221d0
        call fordop
      elseif(ire.eq.20)then
        amt=18.6228d0
        rtara=20d0
        rtarz=10d0
        fermom=.225d0
        call fordop
      elseif(ire.eq.84)then
        amt=78.1631d0
        rtara=84d0
        rtarz=36d0
        fermom=.260d0
        call fordop
      endif

*-----------------------------
      if (mcSet_XMin.ge. 0.002) then
        ntx=ntdis
        write(*,*)
     &  ('Radiative corrections for DIS kinematics (xmin=0.002)')
      elseif (mcSet_XMin.ge. 1.e-09) then
        ntx=ntpho
        write(*,*)
     &  ('Radiative corrections for photoproduction (xmin=1.e-09)')
        write(*,*)
     &  ('Elastic and quesielastric contributions set to ZERO !')
        if(ixytest.gt.0 .and. mcSet_XMin.lt.1.e-07) then
          write(*,*)
     &  ('Xmin in lookup table = 1.e-07 --> extrapolation to 1.e-09')
        endif
        if(ixytest.eq.0 .and. mcSet_XMin.lt.1.e-07) then
          write(*,*)
     &  ('Xmin in lookup table = 1.e-07 --> change minimum x')
          stop
        endif
      else
        write(*,*)
     &  ('Minimum x value below minimum value of 10**-9 ! ')
        stop
      endif

* initialize lookup table
      if(ixytest.ge.0) then

* number of x bins depending on x range desired
* finer grid at lower x values
* number of y bins = number of x bins (equidistant in y)
* (needed to avoid double loop - speed optimisation)
      write(*,*) ('*********************************************')
      write(*,*) 
     &  ('Make sure that you did create the correct lookup table')
      write(6,*) 'number of x bins in RC table = ',ntx
      write(6,*) 'size of x bins in RC table depending on x'
      nty=ntx
      write(6,*) 'number of y bins in RC table = ',nty
      write(6,*) 'size of y bins in RC table = ',
     &  (radgen_ymax-radgen_ymin)/(nty-1)
      write(*,*) ('*********************************************')

        if(ixytest.eq.0) then
          write(*,*)('RADGEN_INIT: Creating lookup table '//tname)
        elseif (ixytest.eq.2) then
           write(*,*)
     +       ('RADGEN_INIT: Loading lookup table '//tname)
        endif
        call xytabl(tname,mcSet_EneBeam,plrun,pnrun,ixytest,ire)
      endif
      END
