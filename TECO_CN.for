      program TECOS_CN

      implicit none
      character(len=4), parameter:: facesite = 'DUKE'
      integer, parameter :: No_site=1            ! the sites to run
      integer, parameter :: iiterms=9            ! MDK data for Oak Ridge input
      integer, parameter :: ilines=12*366*24     ! the maxmum records of Oak Ridge FACE, 1999~2007

      real, parameter:: times_storage_use=720.
      integer  lines,idays
      real inputstep,step_NEE
      integer,dimension(ilines):: year_data,year_NEE
      real,dimension(ilines) :: doy_data,hour_data,doy_NEE,hour_NEE
      real NEE_obs(ilines)
      real input_data(iiterms,ilines),input_data0(iiterms,ilines)
      real input_amb(iiterms,ilines),input_elev(iiterms,ilines) ! For duke Forest

!     site specific parameters
      real lat,longi,rdepth,LAIMAX,LAIMIN
      real wsmax,wsmin,co2ca,co2array(9,2)
      real tau_L,tau_W,tau_R
      real tau_F,tau_C,tau_Micr,tau_Slow,tau_Pass
      real TauC(8)

!     state variables
      real Q_soil,PlantC
      real QC(8) !  leaf,wood,root,fine lit.,coarse lit.,Micr,Slow,Pass
      real OutC(8)
      real Rh_pools(5)
      character(len=6) vegtype
      character(len=6) site

!     soil water
      real WILTPT,FILDCP
      real Rsoilabs
      real fwsoil,topfws,wscontent,omega,omega_s
      real WaterR,WaterS(10),SatFracL(10)

!     Plant growth and allocation
      real NSC,NSCmin,NSCmax,add               ! none structural carbon pool
      real Growth,Groot,Gshoot,GRmax           ! growth rate of plant,root,shoot,and max of root
      real St,Sw,Ss,Sn,Srs,Sps,fnsc,Weight     ! scaling factors for growth
      real Twsoil(7),Tavg,Tcur

!      canopy model
      real evap,transp,ET,G,Qh,Qle 
      real wind,windu0,eairp,esat,wethr,rnet
      real LWdown,Pa_air,Qair  !  Near surface specific humidity
      real gpp,gpp_ra,NPP,NEE,NEP,gpp_d,NPP_d
      real evap_d,transp_d
      real,dimension(3):: tauL,rhoL,rhoS,reffbm,reffdf,extkbm,extkdm
      real,dimension(2):: Radabv,Acan,Ecan,Hcan,Tcan,Gbwcan,Gswcan
      real Qcan(3,2),Qcan0(3)
!      parameters for photosynthesis model
      real stom_n,a1,Ds0,Vcmx0,Vcmax0,extkU,xfang,alpha
      real pi,emleaf,emsoil
      real Rconst,sigma,cpair,Patm,Trefk,H2OLv0,airMa,H2OMw,chi,Dheat
      real wleaf,gsw0,eJmx0,theta,conKc0,conKo0,Ekc,Eko,o2ci
      real Eavm,Edvm,Eajm,Edjm,Entrpy,gam0,gam1,gam2

!     nitrogen model
      real CNmin,CNmax,NSNmax,NSNmin
      real NSN
!
!      QNleaf, QNwood, QNroot, QNfine, QNcoarse, QNmicr, QNslow,  QNpass
!      1       2       3       4       5         6       7        8
!      CN_leaf,CN_wood,CN_root,CN_fine,CN_coarse,CN_micr,CN_slowC,CN_pass
      real QN(8),CN0(8),CN(8),OutN(8),QNplant,QNminer
      real N_leaf,N_wood,N_root,N_npp,N_deficit
      real N_LF,N_WF,N_RF
      real N_uptake,N_leach,N_vol,N_fixation,N_deposit,N_fert
      real N_up_d,N_fix_d,N_dep_d,N_leach_d,N_vol_d
      real N_up_yr,N_fix_yr,N_dep_yr,N_leach_yr,N_vol_yr
      real N_miner,alphaN
      real SNvcmax,SNgrowth,SNRauto,SNrs

!      additional arrays to allow output of info for each layer
      real,dimension(5):: RnStL,QcanL,RcanL,AcanL,EcanL,HcanL
      real,dimension(5):: GbwcL,GswcL,hG,hIL
      real,dimension(5):: Gaussx,Gaussw,Gaussw_cum 
!      for phenology
      real LAI,bmroot,bmstem,bmleaf,bmplant,totlivbiom,ht
      real SLA,L_fall,L_add,litter,seeds
      real GDDonset,GDD5,accumulation,storage,stor_use,store
      real RaL,RaS,RaR  !allocation to respiration
      real alpha_L,alpha_W,alpha_R ! allocation ratio to Leaf, stem, and Root
      real Q10,Rl0,Rs0,Rr0         ! parameters for auto respiration
      real Rgrowth,Rnitrogen,Rmain,Rauto !respirations
      real RmLeaf,RmStem,RmRoot          ! maintanence respiration
      real RgLeaf,RgStem,RgRoot          ! growth respiration
      real RaLeaf,RaStem,RaRoot
      real Rsoil,Rhetero,Rtotal
      real Ra_Nfix,Rh_Nfix
      real gpp_yr,NPP_yr,NEE_yr,RaL_yr,RaR_yr,RaS_yr,Rh_yr
      real R_Ntr_yr
      real NPPL_yr,NPPR_yr,NPPS_yr,NPP_L,NPP_R,NPP_W
      real Rootmax,Stemmax,SapS,SapR,StemSap,RootSap
      REAL ws,wdepth
!      climate variables for every day
      real Ta,Tair,Ts,Tsoil
      real doy,hour,Dair,Rh,radsol
      real PAR
!      output daily means of driving variables
      real CO2air_d_avg,LWdown_d_avg,SWdown_d_avg,Psurf_d_avg
      real Qair_d_avg,Rain_d_avg,Tair_d_avg,Wind_d_avg

!      output from canopy model
      real TEVAP,AEVAP,evap_yr,transp_yr
      real,dimension(10):: thksl,wupl,evapl,wcl,FRLEN
      real runoff,runoff_d,runoff_yr,rain,rain_d,rain_yr
      real wsc(10),ws1,ws2,dws,net_dws
      real Esoil,Hcrop,ecstot,Anet,DEPH2O,Acanop
      real Hcanop,Hcanop_d
      real Raplant,Glmax,Gsmax,Rh_d

!     output variables for ORNL model comparison (NCEAS project)
      real CO2h,PARh,ATh,STh,VPDh,SWh
      real NEPh,GPPh,CEXh,CVOCh,RECOh,RAUTOh,RLEAFh,RWOODh,RROOTh 
      real RHETh,RSOILh,ETh,Th,Eh,INTh,ROh,DRAINh,LEh,SHh
      real LWH,Rgrowth_d,abvLitter,blLitter
!     daily output
	real PAR_d,AT_d,ST_d,VPD_d
      real SW_d,NEP_d,CEX_d,CVOC_d,RECO_d
      real Ra_d,RLEAV_d,RWOOD_d,RROOT_d,RHET_d,RSOIL_d,ET_d,T_d
      real E_d,INT_d,RO_d,DRAIN_d,LE_d,SH_d,CL_d,CW_d,CFR_d,TNC_d
      real CSOIL_d,GL_d,GW_d,GR_d,LFALL_d,LMA_d,NCAN_d,NWOOD_d
      real NFR_d,NSOIL_d,NUP_d,NMIN_d,NVOL_d,NLEACH_d
	real N_LG_d,N_WG_d,N_RG_d
      real N_LF_d,N_WF_d,N_RF_d
      real WFALL_D,RFALL_D

!      NEE observation
      real NEE_annual,Cumol2gram
      real NEE_annual_array(30)
      integer year_array(30),year_obs
!     for loops
      integer jrain,W_flag(7)
      integer onset,duration,offset,dormancy  !flag of phenological stage
      integer year,yr,days,i,j,k,m,n,yrs_eq,hoy
      integer lines_NEE,yr_NEE
      integer istat1,istat2,istat3,istat4
      integer dtimes,yr_data
      integer eq_run
      integer num_scen,isite
      integer idoy,ihour,ileaf,num
!    scenario
      integer ScCO2 ! 1: ambient, 2: elevated
      integer ScTair
      character(len=50) filepath_in,filepath_out
      character(len=50) climfile,modelsite,out_d,out_h
      character(len=50) parafile,logfile       ! parameter file
      character(len=450) hourlyHeader1,hourlyHeader2,dailyHeader1
      character(len=450)  dailyHeader2       ! head of output files
      character(len=80) commts

!     define CO2 concentrations of Oak Ridge FACE
      data co2array /392.,390.,394.,398.,393.,396.,402.,394.,401.,
     &              538.,546.,548.,552.,549.,546.,543.,544.,559./


!     thickness of soil layers (cm)
      data thksl /10.,10.,10.,10.,10.,20.,20.,20.,20.,20./
!     ratio of roots in every layer, Oak Ridge FACE
      data FRLEN /0.1,0.25,0.25,0.2,0.1,0.05,0.025,0.015,0.005,0.005/

!     Default C/N ratios of Oak Ridge FACE
!     Default C/N ratios of Duke FACE
      if(facesite == 'ORNL')then
         CN0 = (/50.,350.,60.,40.,300.,10.,20.,12./)
      else !if(facesite == 'DUKE')then
         CN0 = (/50.,350.,60.,40.,300.,10.,20.,12./)
      endif
!     Standard air pressure
      Pa_air=101325.0         !Pa input_data(9,m)/100.*1000.0 !
!     Nitrogen input
!      N_deposit=0.000144634702 !(gN/h/m2, 1.2+0.067 gN/yr/m2,Oak ridge)
!     0.7 gN/yr/m2, 13.4 kg N ha-1 yr-1, 2000, Dentener et al. 2006, GBC, Duke FACE
      N_deposit = 0.0 ! 2.34/8760. !(gN/h/m2, )
!      N_fert=0. ! (20.0 gN m-2 yr-1, in spring, from 2004, Oak Ridge)
      N_fert=0. !5.6 ! (11.2 gN m-2 yr-1, in spring, Duke Forest FACE)
      co2array=co2array*1.0E-6

!     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      define input files
      filepath_in='input/'
      filepath_out='output/'
      parafile=trim(filepath_in)//'para_OR_MDK.txt'

!     open and read input file for getting climate data
      open(10,file=parafile,status='old',ACTION='read',IOSTAT=istat1)
      read(10,'(a160)')commts

      open(63,file=trim(filepath_out)//'annual_output.csv')
      open(64,file=trim(filepath_out)//'daily_output.csv')

      do isite=1,No_site  ! run model site by site
          ScCO2=1.0
          ScTair=0.0       ! tmperature scenario
          read(10,*,IOSTAT=istat1)site,vegtype,climfile,out_h,
     &        out_d,lat,longi,wsmax,wsmin,LAIMAX,LAIMIN,rdepth,
     &        Rootmax,Stemmax,SapR,SapS,SLA,GLmax,GRmax,Gsmax,
     &        stom_n,a1,Ds0,Vcmax0,extkU,xfang,alpha,
     &        tau_L,tau_W,tau_R,tau_F,tau_C,tau_Micr,        ! the unit is year
     &        tau_Slow,tau_Pass,gddonset,
     &        Q10,Rl0,Rs0,Rr0
          Vcmx0 = Vcmax0 !
          rdepth=150.    ! cm
!         the unit of residence time is transformed from yearly to hourly
          tauC=(/tau_L,tau_W,tau_R,tau_F,tau_C,
     &           tau_Micr,tau_Slow,tau_Pass/)*8760.

!         max. growth rates of plant
          GLmax=GLmax/24.
          GRmax=GRmax/24.
          Gsmax=GSmax/24.

!         end of setting parameters
          site=trim(site)
          vegtype=trim(vegtype)
          climfile=trim(filepath_in)//trim(climfile)
          modelsite=trim(modelsite)     !//'_TECO_01'//'.csv'
          write(*,*)site,vegtype,climfile,out_d

!         open forcing data
          open(11,file=climfile,status='old',ACTION='read',
     &         IOSTAT=istat2)
!         skip 2 lines of input met data file
          read(11,'(a160)') commts
!          read(11,'(a160)') commts ! MDK data only has one line comments

          m=0  ! to record the lines in a file
          yr_data=0 ! to record years of a dataset
          do    ! read forcing files
              m=m+1
              read(11,*,IOSTAT=istat3)year_data(m),
     &             doy_data(m),hour_data(m),
     &             (input_data(n,m),n=1,iiterms)
              if(istat3<0)exit
          enddo ! end of reading the forcing file
          lines=m-1
          yr_data=(year_data(lines)-year_data(1))+1
          inputstep=hour_data(2)-hour_data(1)
          write(*,*)"forcing",lines,yr_data,inputstep
          if (inputstep==1.0)then
!            write(*,*)"the data freqency is hourly"
          elseif(inputstep==0.5)then
!            write(*,*)"the data freqency is half hourly"
          else
            write(*,*)"Please check time step first!"
            goto 999
          endif
          close(11)    ! close forcing file
          if(isite==1)input_amb=input_data
          if(isite==2)input_elev=input_data
          input_data0=input_data
!         end of reading forcing data

!     ===============================================================
!!         open output files (for NCEAS simulations)
!          open(61,file=trim(filepath_out)//out_h)
!          open(62,file=trim(filepath_out)//out_d)


! satge 1 format (for NCEAS simulations)
      hourlyHeader1="YEAR,HOY,CO2h,PPTh,PARh,LWh,ATh,STh,VPDh,SWh,NEPh,
     &GPPh,NPPh,CEXh,CVOCh,RECOh,RAUTOh,RLEAFh,RWOODh,RROOTh,RGROh,
     &RHETh,RSOILh,ETh,Th,Eh,INTh,ROh,DRAINh,LEh,SHh"

      hourlyHeader2="-,-,ppm,kgH2O m-2 h-1,umol/m2/s,W m-2,C,C,kPa,%,
     &gC m-2 h-1,gC m-2 h-1,gC m-2 h-1,gC m-2 h-1,gC m-2 h-1,
     &gC m-2 h-1,gCm-2h-1,gC m-2 h-1, gC m-2 h-1, gC m-2 h-1,  
     &gC m-2 h-1, gC m-2 h-1,gC m-2 h-1,mm/m2/h, mm/m2/h, mm/m2/h, 
     &mm/m2/h, mm/m2/h, mm/m2/h,w/m2,w/m2"

      dailyHeader1="YEAR,DOY,CO2,PPT,PAR,AT,ST,VPD,SW,NDEP,NEP,GPP,NPP,
     &CEX,CVOC,RECO,RAUTO,RLEAF,RWOOD,RROOT,RGROW,RHET,RSOIL,ET,T,E,
     &INT,RO,DRAIN,LE,SH,CL,CW,CCR,CFR,TNC,CLIT,CRLIT,CDW,CSOIL,GL,GW,
     &GCR,GR,CLLFALL,CRLIN,CWIN,LAI,LMA,NCAN,NWOOD,NCR,NFR,NSTOR,NLIT,
     &NRLIT,NDW,NSOIL,NPOOLM,NPOOLO,NFIX,NLITIN,NWLIN,NRLIN,NUP,NGMIN,
     &NMIN,NVOL,NLEACH,NGL,NGW,NGCR,NGR"

          dailyHeader2="-"

          hourlyHeader1=trim(hourlyHeader1)
          hourlyHeader2=trim(hourlyHeader2)
          dailyHeader1=trim(dailyHeader1)
          dailyHeader2=trim(dailyHeader2)

!          write(61,'(a160)')hourlyHeader1
!          write(61,'(a260)')hourlyHeader2
!          write(62,'(a400)')dailyHeader1
!          write(62,'(a2)')dailyHeader2

        write(63,'(60(a8,","))')'yr','year','gpp_yr','Rnitr',
     & 'NPP_yr','Rh_yr','NEE_yr',
     & 'Precp','Evap','Transp','Runoff',
     & 'N_up', 'N_fix' , 'N_depo', 'N_leach', 'N_vol', 'N_def',
     & 'Q_leaf','Q_wood','Q_root',
     & 'Q_fine','Q_coarse','Q_Micr','Q_Slow','Q_Pass',
     & 'N_leaf','N_wood','N_root','N_fine',
     & 'N_coarse','N_micr','N_slowC','N_pass','QNminer','NSN'

          write(64,'(a250)')"yr,doy,gpp_d,NPP_d,Rh_d,NEE_d,
     & Precp,Evap,Transp,Runoff,SoilW,
     & N_up,N_fix_d,N_dep_d,N_leach_d,N_vol_d,N_def,
     & LAI,Q_L,Q_W,Q_R,Q_fine,Q_coarse,Q_M,Q_S,Q_P,
     & N_leaf,N_wood,N_root,N_fine,
     & N_coarse,N_micr,N_slowC,N_pass,QNminer,NSC,NSN"


!         ===================================================
!         Initialize parameters and initial state:

!         initiazing the canopy model
          call consts(pi,tauL,rhoL,rhoS,emleaf,emsoil,
     &    Rconst,sigma,cpair,Patm,Trefk,H2OLv0,airMa,H2OMw,chi,Dheat,
     &    wleaf,gsw0,Vcmx0,eJmx0,theta,conKc0,conKo0,Ekc,Eko,o2ci,
     &    Eavm,Edvm,Eajm,Edjm,Entrpy,gam0,gam1,gam2)

          WILTPT=wsmin/100.0
          FILDCP=wsmax/100.0

!         define soil for export variables for satisfying usage of canopy submodel first time
          wscontent=WILTPT
          fwsoil=1.0
          topfws=1.0
          omega=1.0
          do i=1,10
              wcl(i)=FILDCP
          enddo
!         gddonset=320.0
          Storage=32.09           !g C/m2
          stor_use=Storage/times_storage_use
          onset=0
          duration=0
          offset=0
          dormancy=1 
!         initial values of the C pools
          nsc=85.35
!         Initial values of C and N pools
!              leaf,wood, root, fine, coarse, micr, slow,  passive
          QC=(/300.,4300.,300., 119., 300.,   322., 3834., 312./)
          QN=QC/CN0
          QNplant  =QN(1) + QN(2) + QN(3)

          LAI=LAIMIN
          bmleaf=QC(1)/0.45
          bmstem=QC(2)/0.45
          bmroot=QC(3)/0.45
          bmplant=bmstem+bmroot+bmleaf

!         initial values of Nitrogen pools and C/N ratio
          alphaN=0.0    ! the transfer of N before littering

          NSN=6.0       ! gram N
          QNminer= 1.2
          N_deficit=0
          CN=CN0

!         ====================================================
!         Simulating daily
          yrs_eq= 20*yr_data  !140*3
          eq_run = 1
          num=0
          m=1
          n=1

          idays=365
          input_data=input_amb
          do yr=1,yrs_eq+yr_data  ! how many years
!            using ambient data to run equilibiurm, elevated only for the last cycle
             ! if(isite==2.and.yr>yrs_eq)input_data=input_elev  ! turned on by Weng 2012-03-13

             GDD5=0.0
             onset=0
             gpp_yr=0.0
             R_Ntr_yr=0.
             NPP_yr=0.0
             Rh_yr =0.0
             NEE_yr=0.0
!            water fluxes
             rain_yr=0.0
             transp_yr=0.0
             evap_yr=0.0
             runoff_yr=0.0
!            Nitrogen fluxes
             N_up_yr=0
             N_fix_yr=0.
             N_dep_yr=0.
             N_leach_yr=0.
             N_vol_yr=0.

             hoy=0
!           for Duke Forest, leap year
             if(     year_data(m).eq.1996.OR.year_data(m).eq.2000
     &           .OR.year_data(m).eq.2004.OR.year_data(m).eq.2008)then
                 idays=366
             else
                 idays=365
             endif
!     end of leap year

             do days=1,idays !the days of a year
!             Nitrogen fertilization since 2004 in Oak Ridge
                if(facesite == 'ORNL')then
                    if(yr>yrs_eq+5.and.days==135)then
                      QNminer=QNminer+N_fert     !(20 gN/yr/m2,N fertiliztion in Spring)
                    endif
                else !if(facesite == 'DUKE')then
!                   Nitrogen fertilization since 1999 in Duke
                    if(yr>yrs_eq+1.and.(days==75.OR.days==105))then
                       QNminer=QNminer+N_fert     !(5.6 gN/yr/m2,N fertiliztion in March and Apr)
                     endif
                endif 

              StemSap=AMIN1(Stemmax,SapS*bmStem)
              RootSap=AMIN1(Rootmax,SapR*bmRoot)
              NSCmin=5. 
              NSCmax=0.05*(StemSap+RootSap+QC(3))
              if(Ta.gt.0.0)GDD5=GDD5+Ta-5.0
!             THE FIRST PART:  coupled canopy and soil model
              gpp_d   =0.0   ! daily
              gpp_ra  =0.0   ! daily
              NPP_d   =0.0   ! daily
              NEP_d=0.0
!             rain_d,transp_d,evap_d
              transp_d=0.0   ! daily
              Hcanop_d=0.0   ! daily
              evap_d  =0.0   ! daily
              ta=0.0         ! daily 
              Ts=0.0         ! daily
              rain_d=0.0     ! daily
              runoff_d=0.0    ! daily
              LE_d=0.0
              RaL=0.0
              RaS=0.0
              RaR=0.0
              Rauto=0.0
              Rh_d=0.0
              N_up_d=0.
              N_fix_d=0.
              N_dep_d=0.
              N_leach_d=0.
              N_vol_d=0.
              PAR_d=0.
              VPD_d=0.0
              RECO_d=0.0
              RLEAV_d=0.0 
              RWOOD_d=0.0
              RROOT_d=0.0
              GL_d   =0.0
              GW_d   =0.0
              GR_d   =0.0
              LFALL_d=0.0
              NUP_d=0.0
              NVOL_d=0.
              NLEACH_d=0.0
              NMIN_d=0.0
              N_LG_d=0.0
              N_WG_d=0.0
              N_RG_d=0.0
              N_LF_d=0.0
              N_WF_d=0.0
              N_RF_d=0.0
              WFALL_d=0.0
              RFALL_d=0.0
              dtimes=24 !how many times a day,24 means every hour
              do i=1,dtimes
!                 input data
                  if( eq_run >0 .AND. yr > yrs_eq)then
                     m      = 1
                     n      = 1
                     hoy    = 0
                     if(     year_data(m).eq.1996 .OR.
     &                       year_data(m).eq.2000 .OR.
     &                       year_data(m).eq.2004.OR.
     &                       year_data(m).eq.2008)    then
                         idays=366
                     else
                         idays=365
                     endif
                     eq_run = 0
!                    update initial values of C and N pools
                     if(facesite == 'ORNL')then
!                       QC=(/300.,4300.,300.,119.,300.,322.,6500.,312./)
                        QC(2) = 5300.
                        QC(4) = 119.
                        QC(5) = 300.
                        QC(6) = 500.
                        QC(7) = 6500.
                        QN=QC/CN0
                        QNplant  =QN(1) + QN(2) + QN(3)
                     else !if(facesite == 'DUKE')then
!                       QC=(/300.,4200.,300.,119.,300.,322.,3834.,312./)
                        QC(2) = 4200.
                        QC(4) = 119.
                        QC(5) = 300.
                        QC(7) = 3834.
                        QN=QC/CN0
                        QNplant  =QN(1) + QN(2) + QN(3)
                     endif
                  endif
!                 check if data have approached the last line
                  if(m > lines)then 
                     m=1
                     n=1
                     hoy=0
                  endif
                  year =year_data(m)
                  doy  =doy_data(m)
                  hour =hour_data(m)
!!     with leap year in Oak Ridge data, MDK data
                  PAR    = input_data(1,m) ! only used in daily mean calculation
                  radsol = input_data(1,m) ! "radsol" is PAR in codes
                  Tair   = input_data(3,m) ! Atmean2m
                  Tsoil  = input_data(4,m) ! Stmean
                  RH     = input_data(5,m) ! 12-->RH22m %; 4-->RH2m %
                  rain   = input_data(6,m) ! rain
                  wind   = input_data(7,m) ! wind speed
                  Pa_air = input_data(8,m) ! 101325.0 
                  co2ca  = input_data(9,m)*1.0E-6 ! CO2 concentration,ppm-->1.0E-6
!!     end of Oak Ridge


!                 Ajust some unreasonable values
                  RH=AMAX1(0.01,AMIN1(99.99,RH))
                  Dair =esat(Tair)*(1.-RH*0.01) !VPD, Pa				 
                  eairP=esat(Tair)*RH*0.01
                  wind  = ABS(wind)
                  windU0=AMAX1(1.0,wind) 
                  radsol=AMAX1(radsol,0.01)
                  wethr=1

                  hoy=hoy+1
                  if(inputstep.eq.1.0)then
                      m=m+1
                  else
                      rain=(input_data(7,m)+input_data(7,m+1))*1800.  ! for ORNL
                      m=m+2
                  endif

                  if(radsol.gt.10.0) then
                      G=-25.0
                  else
                      G=20.5
                  endif
                  Esoil=0.05*radsol
                  if(radsol.LE.10.0) Esoil=0.5*G
                  Hcrop=0.1  ! never used
                  Ecstot=0.1 ! never used
                  Anet=0.1   ! never used
                  DepH2O=0.2
!                 for daily mean conditions
                  PAR_d=PAR_d+PAR/24.
                  VPD_d=VPD_d+Dair/24./1000.                ! KPa
                  ta= ta + tair/24.0             ! sum of a day, for calculating daily mean temperature
                  Ts=Ts+Tsoil/24.0
                  rain_d=rain_d+rain
!                 calculating scaling factor of NSC
                  if(NSC.le.NSCmin)fnsc=0.0
                  if(NSC.ge.NSCmax)fnsc=1.0
                  if((NSC.lt.NSCmax).and.(NSC.gt.NSCmin))then 
                     fnsc=(NSC-NSCmin)/(NSCmax-NSCmin)
                  endif
!                 update vcmx0 and eJmx0 according to C/N of leaves
                  Vcmx0 = Vcmax0*SNvcmax
!                  eJmx0 = 2.7*Vcmx0  ! original
                  eJmx0 = 1.67*Vcmx0 ! Weng 02/21/2011 Medlyn et al. 2002
                  call canopy(gpp,evap,transp,Acanop,Hcanop,Rsoilabs,   ! outputs
     &              fwsoil,topfws,wscontent,                    ! from soil model
     &              LAI,Sps,
     &              doy,hour,radsol,tair,dair,eairP,            ! from climate data file,including 
     &              windU0,rain,wethr,
     &              Rnet,G,Esoil,Hcrop,Ecstot,Anet,
     &              Tsoil,DepH2O,
     &              wsmax,wsmin,                                !constants specific to soil and plant
     &              lat,co2ca,a1,Ds0,Vcmx0,extkU,xfang,alpha,
     &              stom_n,pi,tauL,rhoL,rhoS,emleaf,emsoil,
     &              Rconst,sigma,cpair,Patm,Trefk,H2OLv0,airMa,
     &              H2OMw,chi,Dheat,wleaf,gsw0,eJmx0,theta,
     &              conKc0,conKo0,Ekc,Eko,o2ci,Eavm,Edvm,Eajm,
     &              Edjm,Entrpy,gam0,gam1,gam2)

                  call soilwater(wsmax,wsmin,rdepth,FRLEN,         !constants specific to soil/plant
     &                    rain,tair,transp,wcl,tsoil,RH,thksl,LAI, !inputs
     &                    evap,runoff,wscontent,fwsoil,topfws,     !outputs
     &                    omega,omega_S,WaterR,WaterS,SatFracL)    !outputs
                  omega=omega_S
                  ET=evap+transp
                  rain_yr=rain_yr+rain
                  transp_yr=transp_yr+transp
                  evap_yr=evap_yr+evap
                  runoff_yr=runoff_yr+runoff

                  call respiration(LAIMIN,GPP,Tair,Tsoil,DepH2O,
     &                       Q10,Rl0,Rs0,Rr0,SNRauto,
     &                       LAI,SLA,bmstem,bmroot,bmleaf,
     &                       StemSap,RootSap,NSC,fnsc,
     &                       RmLeaf,RmStem,RmRoot,Rmain)

!                 THE Third Part: update LAI
                  call plantgrowth(Tair,omega,GLmax,GRmax,GSmax,
     &                    LAI,LAIMAX,LAIMIN,SLA,TauC(1),             !Tau_L,
     &                    bmleaf,bmroot,bmstem,bmplant,
     &                    Rootmax,Stemmax,SapS,SapR,
     &                    StemSap,RootSap,Storage,GDD5,
     &                    stor_use,onset,accumulation,gddonset,
     &                    Sps,NSC,fnsc,NSCmin,NSCmax,
     &                    NSN,CN,CN0,SNgrowth,N_deficit,
     &                    store,add,L_fall,ht,
     &                    NPP,alpha_L,alpha_W,alpha_R,
     &                    RgLeaf,RgStem,RgRoot,Rgrowth)

!                 THE Fourth PART: simulating C influx allocation in pools
                  call TCS_CN(Tair,Tsoil,omega,runoff,
     &               NPP,alpha_L,alpha_W,alpha_R,L_fall,
     &               tauC,QC,OutC,Rh_pools,Rnitrogen,NSC,
     &               CNmin,CNmax,NSNmax,NSNmin,alphaN,            ! nitrogen
     &               NSN,N_uptake,N_miner,QN,QNminer,
     &               CN,CN0,fnsc,rdepth,N_deficit,
     &               N_leaf,N_wood,N_root,N_LF,N_WF,N_RF,
     &               N_deposit,N_fixation,N_leach,N_vol,
     &               SNvcmax,SNgrowth,SNRauto,SNrs)

!                 update NSC
                  Rauto  =Rmain+Rgrowth+Rnitrogen
                  NSC    =NSC+GPP-Rauto-(NPP-add)-store

                  if(NSC<0)then
                      bmstem=bmstem+NSC/0.45
                      NPP=NPP+NSC
                      NSN=NSN-NSC/CN(2)
                      NSC=0.
                  endif
                  GL_d   =GL_d+NPP*alpha_L
                  GW_d   =GW_d+NPP*alpha_W
                  GR_d   =GR_d+NPP*alpha_R
                  LFALL_d=LFALL_d+L_fall
!                 update
                  RaLeaf = RgLeaf + RmLeaf
				RaStem = RgStem + RmStem
				RaRoot = RgRoot + RmRoot + Rnitrogen
                  WFALL_d=WFALL_d+OutC(2) !_wood
                  RFALL_d=RFALL_d+OutC(3) !_root
                  N_LG_d=N_LG_d+N_leaf
                  N_WG_d=N_WG_d+N_wood
                  N_RG_d=N_RG_d+N_root
                  N_LF_d=N_LF_d+N_LF
                  N_WF_d=N_WF_d+N_WF
                  N_RF_d=N_RF_d+N_RF

                  N_up_d=N_up_d+N_uptake
                  N_fix_d=N_fix_d+N_fixation
                  N_dep_d=N_dep_d+N_deposit
                  N_leach_d=N_leach_d+N_leach
                  N_vol_d=N_vol_d+N_vol

                  N_up_yr=N_up_yr+N_uptake
                  N_fix_yr=N_fix_yr+N_fixation
                  N_dep_yr=N_dep_yr+N_deposit
                  N_leach_yr=N_leach_yr+N_leach
                  N_vol_yr=N_vol_yr+N_vol

                  R_Ntr_yr=R_Ntr_yr + Rnitrogen

!                 Rhetero=Rh_f + Rh_c + Rh_Micr + Rh_Slow + Rh_Pass
                  Rhetero= Rh_pools(1)+Rh_pools(2)+Rh_pools(3)
     &                    +Rh_pools(4)+Rh_pools(5)
                  Rsoil  =Rhetero+RmRoot+RgRoot+Rnitrogen
                  NEE=Rauto+Rhetero - GPP
                  Q_soil=QC(6) + QC(7) + QC(8)

                  bmleaf=QC(1)/0.45
                  bmstem=QC(2)/0.45
                  bmroot=QC(3)/0.45
                  bmplant=bmleaf+bmroot+bmstem
                  LAI=bmleaf*SLA
                  NMIN_d = NMIN_d+N_miner
!                 output hourly
                  CEXh=-9999
                  CVOCh=-9999
                  Recoh=Rauto+Rhetero
                  ETh =ET !*1000.
                  Th  =transp !*1000.
                  Eh  =evap !*1000.
                  INTh=-9999
                  VPDh=Dair/1000.
                  ROh =runoff !*1000.
                  DRAINh=-9999
                  LEh =ETh*((2.501-0.00236*Tair)*1000.0)/3600.
                  SHh =-9999
                  LWh =-9999
                  NEP=-NEE

!                  if((yr.gt.yrs_eq).and.(yr.le.(yrs_eq+yr_data)))then
!                      write(61,161)year,hoy,co2ca * 1.0E6,rain,PAR,
!     &                  LWh,Tair,Tsoil,VPDh,wscontent,NEP,GPP,NPP,CEXh,
!     &                  CVOCh,Recoh,Rauto,RaLeaf,RaStem,RaRoot,Rgrowth,
!     &                  Rhetero,Rsoil,ETh,Th,Eh,INTh,ROh,DRAINh,
!     &                  LEh,SHh              
!                  endif

!                 sums of a day
                  gpp_d=gpp_d + gpp*(24./dtimes)
                  gpp_ra=gpp_ra+Rauto
                  NPP_d   =NPP_d+NPP
                  NEP_d=NEP_d+NEP
                  RECO_d=RECO_d+Recoh
                  Rh_d=  Rh_d + Rhetero
                  Ra_d=Reco_d-Rh_d
                  RLEAV_d=RLEAV_d+RmLeaf+RgLeaf
                  RWOOD_d=RWOOD_d+RmStem+RgStem
                  RROOT_d=RROOT_d+RmRoot+RgRoot+Rnitrogen
                  Rsoil_d=Rh_d+RROOT_d
                  NUP_d=NUP_d+N_uptake
                  NVOL_d=NVOL_d+N_vol
                  NLEACH_d=NLEACH_d+N_leach
                  transp_d=transp_d + transp*(24./dtimes)
                  evap_d=evap_d + evap*(24./dtimes)
                  ET_d=transp_d + evap_d
                  LE_d=LE_d+LEh/24.
                  Hcanop_d=Hcanop_d+Hcanop/(24./dtimes)
                  runoff_d=runoff_d+runoff
!                 sum of the whole year
                  gpp_yr=gpp_yr+gpp
                  NPP_yr=NPP_yr+NPP
                  Rh_yr =Rh_yr +Rhetero
                  NEE_yr=NEE_yr+NEE
!                 numbering                         
                  n=n+1        
              enddo              ! end of a day
!             +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!             output results of canopy and soil models, daily
!             daily output
              CEX_d=-9999
              CVOC_d=-9999
              INT_d=-9999
              DRAIN_d=-9999
              SH_d=-9999
              CSOIL_d=QC(6)+QC(7)+QC(8)
              LMA_d=QC(1)/0.45
              NSOIL_d=QN(6)+QN(7)+QN(8)+QNminer

              Rgrowth_d=-9999
              abvLitter=QC(4)  !-9999
              blLitter=QC(5) !-9999

!!            yearly output
              if((yr.gt.yrs_eq).and.(yr.le.(yrs_eq+yr_data)))then

!!                 NCEAS output
!                  write(62,162)year,doy,CO2ca*1.E6,rain_d,PAR_d,Ta,Ts,
!!                        VPD,  SW,       NDEP,         NEP,  GPP,
!     &                   VPD_d,wscontent,N_deposit*24.,NEP_d,gpp_d,
!!                        NPP,  CEX,  CVOC,  RECO,  RAUTO,RLEAF,
!     &                   NPP_d,CEX_d,CVOC_d,RECO_d,Ra_d,RLEAV_d,
!!                        RWOOD,  RROOT,  RGROW,    RHET,RSOIL,
!     &                   RWOOD_d,RROOT_d,Rgrowth_d,Rh_d,RSOIL_d,
!!                        ET,  T,       E,      INT, RO,      DRAIN,
!     &                   ET_d,transp_d,evap_d,INT_d,runoff_d,DRAIN_d,
!!                        LE,  SH,  CL,    CW,        CCR,        CFR,
!     &                   LE_d,SH_d,QC(1),QC(2)*0.8,QC(2)*0.2,QC(3),
!!                        TNC,CLIT,     CRLIT,   CDW,   CSOIL,  GL,
!     &                   NSC,abvLitter,blLitter,-9999.,CSOIL_d,GL_d,
!!                        GW,      GCR,     GR,  CLLFALL,CRLIN,  CWIN,
!     &                   GW_d*0.8,GW_d*0.2,GR_d,LFALL_d,RFALL_d,WFALL_d,
!!                        LAI, LMA, NCAN,  NWOOD,     NCR,
!     &                   LAI,LMA_d,QN(1),QN(2)*0.8,QN(2)*0.2,
!!                        NFR,   NSTOR,
!     &                   QN(3),NSN,
!!                        NLIT,                  NRLIT,
!     &                   (QN(4)+QN(5))*0.7,(QN(4)+QN(5))*0.3,
!     &                   -9999.,NSOIL_d,QNminer,-9999.,-9999.,N_LF_d,
!     &                   N_WF_d,N_RF_d,NUP_d,-9999.,NMIN_d,
!     &                   NVOL_d,NLEACH_d,
!     &                   N_LG_d,N_WG_d*0.8,N_WG_d*0.2,N_RG_d !

!!                 Ensheng's format, daily output
                  write(64,162)year,doy,gpp_d,NPP_d,Rh_d,NEP_d,
     &                      rain_d,evap_d,transp_d,runoff_d,wscontent, 
     &                      N_up_d,N_fix_d,N_dep_d,N_leach_d,N_vol_d,
     &                      N_deficit,LAI,(QC(i),i=1,8),(QN(i),i=1,8),
     &                      QNminer,NSC,NSN
              endif
161           format(I6,",",I6,",",120(f12.4,","))
162           format(I6,",",f8.0,",",125(f12.4,","))
            enddo                         ! end of a year
            storage=accumulation
            stor_use=Storage/times_storage_use
            write(*,*)year,gpp_yr,NPP_yr,NEE_yr
            write(63,161)yr,year,gpp_yr,R_Ntr_yr,NPP_yr,Rh_yr,NEE_yr,
     &                   rain_yr,transp_yr,evap_yr,runoff_yr,           
     &                   N_up_yr,N_fix_yr,N_dep_yr,N_leach_yr,N_vol_yr,
     &                   N_deficit,(QC(i),i=1,8),(QN(i),i=1,8),
     &                   QNminer,NSN
            accumulation=0.0
            onset=0
         enddo            !end of simulations multiple years
 
         close(21)
         close(51)
         close(62)
999      continue

      enddo   ! end of one site

9999  continue
      close(63)
      close(64)
      close(10) ! close the parameter file

      end

!     ****************************************************************************
      subroutine consts(pi,tauL,rhoL,rhoS,emleaf,emsoil,
     &   Rconst,sigma,cpair,Patm,Trefk,H2OLv0,airMa,H2OMw,chi,Dheat,
     &   wleaf,gsw0,Vcmx0,eJmx0,theta,conKc0,conKo0,Ekc,Eko,o2ci,
     &   Eavm,Edvm,Eajm,Edjm,Entrpy,gam0,gam1,gam2)
     
      real tauL(3), rhoL(3), rhoS(3)
      pi = 3.1415926
!     physical constants
      tauL(1)=0.1                  ! leaf transmittance for vis
      rhoL(1)=0.1                  ! leaf reflectance for vis
      rhoS(1)=0.1                  ! soil reflectance for vis
      tauL(2)=0.425                ! for NIR
      rhoL(2)=0.425                ! for NIR
      rhoS(2)=0.3                  ! for NIR - later function of soil water content
      tauL(3)=0.00                 ! for thermal
      rhoL(3)=0.00                 ! for thermal
      rhoS(3)=0.00                 ! for thermal
      emleaf=0.96
      emsoil=0.94
      Rconst=8.314                 ! universal gas constant (J/mol)
      sigma=5.67e-8                ! Steffan Boltzman constant (W/m2/K4)
      cpair=1010.                  ! heat capapcity of air (J/kg/K)
      Patm=101325. !1.e5           ! atmospheric pressure  (Pa)
      Trefk=293.2                  !reference temp K for Kc, Ko, Rd
      H2OLv0=2.501e6               !latent heat H2O (J/kg)
      AirMa=29.e-3                 !mol mass air (kg/mol)
      H2OMw=18.e-3                 !mol mass H2O (kg/mol)
      chi=0.93                     !gbH/gbw
      Dheat=21.5e-6                !molecular diffusivity for heat
!     plant parameters
      gsw0 = 1.0e-2                !g0 for H2O in BWB model
      eJmx0 = Vcmx0*2.7            !@20C Leuning 1996 from Wullschleger (1993)
      theta = 0.9
      wleaf = 0.01                   !leaf width (m)

!     thermodynamic parameters for Kc and Ko (Leuning 1990)
      conKc0 = 302.e-6                !mol mol^-1
      conKo0 = 256.e-3                !mol mol^-1
      Ekc = 59430.                    !J mol^-1
      Eko = 36000.                    !J mol^-1
!     Erd = 53000.                    !J mol^-1
      o2ci= 210.e-3                   !mol mol^-1

!     thermodynamic parameters for Vcmax & Jmax (Eq 9, Harley et al, 1992; #1392)
      Eavm = 116300.               !J/mol  (activation energy)
      Edvm = 202900.               !J/mol  (deactivation energy)
      Eajm = 79500.                !J/mol  (activation energy) 
      Edjm = 201000.               !J/mol  (deactivation energy)
      Entrpy = 650.                !J/mol/K (entropy term, for Jmax & Vcmax)

!     parameters for temperature dependence of gamma* (revised from von Caemmerer et al 1993)
      gam0 = 28.0e-6               !mol mol^-1 @ 20C = 36.9 @ 25C
      gam1 = .0509
      gam2 = .0010
      return
      end
!****************************************************************************

!      a sub-model for calculating C flux and H2O flux of a canopy
!      adapted from a two-leaf canopy model developed by Wang Yingping
        subroutine canopy(gpp,evap,transp,Acanop,Hcanop,Rsoilabs,   ! outputs
     &                    fwsoil,topfws,wscontent,           ! from soil model
     &                    LAI,Sps,
     &               doy,hour,radsol,tair,Dair,eairP,! from climate data file,including 
     &               windU0,rain,wethr,
     &               Rnet,G,Esoil,Hcrop,Ecstot,Anet,
     &               Tsoil,DepH2O,
     &               wsmax,wsmin,  !constants specific to soil and plant
     &               lat,co2ca,a1,Ds0,Vcmx0,extkU,xfang,alpha,
     &               stom_n,pi,tauL,rhoL,rhoS,emleaf,emsoil,
     &               Rconst,sigma,cpair,Patm,Trefk,H2OLv0,airMa,
     &               H2OMw,chi,Dheat,wleaf,gsw0,eJmx0,theta,
     &               conKc0,conKo0,Ekc,Eko,o2ci,Eavm,Edvm,Eajm,
     &               Edjm,Entrpy,gam0,gam1,gam2)

      real lat
      real gpp,evap,transp,LAI,Rsoilabs
      real tauL(3),rhoL(3),rhoS(3),reffbm(3),reffdf(3)
      real extkbm(3),extkdm(3)
      real Radabv(2),Qcan(3,2),Qcan0(3)
      real Acan(2),Ecan(2),Hcan(2),Tcan(2), Gbwcan(2), Gswcan(2)
!     extra variables used to run the model for the wagga data
      real topfws        ! from siol subroutine      
      integer idoy,ihour,ileaf
      integer jrain,i,j,k

!     additional arrays to allow output of info for each layer
      real RnStL(5),QcanL(5),RcanL(5),AcanL(5),EcanL(5),HcanL(5)
      real GbwcL(5),GswcL(5),hG(5),hIL(5)
      real Gaussx(5),Gaussw(5),Gaussw_cum(5)
      
      character*80 commts
!     Normalised Gaussian points and weights (Goudriaan & van Laar, 1993, P98)
!     5-point
      data Gaussx/0.0469101,0.2307534,0.5,0.7692465,0.9530899/
      data Gaussw/0.1184635,0.2393144,0.2844444,0.2393144,0.1184635/
      data Gaussw_cum/0.11846,0.35777,0.64222,0.88153,1.0/

!     calculate beam fraction in incoming solar radiation
      call  yrday(doy,hour,lat,radsol,fbeam)
      idoy=int(doy)
      hours=idoy*1.0+hour/24.0
      coszen=sinbet(doy,lat,pi,hour)             !cos zenith angle of sun

!     set windspeed to the minimum speed to avoid zero Gb
      if(windU0.lt.0.01) windU0=0.01
!     calculate soil albedo for NIR as a function of soil water (Garratt pp292)
      if(topfws.gt.0.5) then
            rhoS(2)=0.18
      else
            rhoS(2)=0.52-0.68*topfws
      endif
!        assign plant biomass and leaf area index at time t
!        assume leaf biomass = root biomass
      FLAIT =LAI 
      eairP=esat(Tair)-Dair                !air water vapour pressure
      radabv(1)=0.5*radsol                 !(1) - solar radn, Weng: PAR -> radiation
      radabv(2)=0.5*radsol                 !(2) - NIR
!     call multilayer model of Leuning - uses Gaussian integration but radiation scheme
!     is that of Goudriaan
      call xlayers(Sps,Tair,Dair,radabv,G,Esoil,fbeam,eairP,
     &           windU0,co2ca,fwsoil,wcl,LAI,coszen,idoy,hours,
     &           tauL,rhoL,rhoS,xfang,extkd,extkU,wleaf,
     &           Rconst,sigma,emleaf,emsoil,theta,a1,Ds0,
     &           cpair,Patm,Trefk,H2OLv0,AirMa,H2OMw,Dheat,
     &           gsw0,alpha,stom_n,wsmax,wsmin,
     &           Vcmx0,eJmx0,conKc0,conKo0,Ekc,Eko,o2ci,
     &           Eavm,Edvm,Eajm,Edjm,Entrpy,gam0,gam1,gam2,
     &           extKb,Rsoilabs,Hsoil,Acan1,Acan2,Ecan1,Ecan2,
     &           RnStL,QcanL,RcanL,AcanL,EcanL,HcanL,GbwcL,GswcL)


         Acanop=Acan1+Acan2
         Ecanop=Ecan1+Ecan2
         gpp=Acanop*3600.0*12.0                           ! every hour, g C m-2 s-1
         transp=AMAX1(Ecanop*3600.0/(1.0e6*(2.501-0.00236*Tair)),0.) ! mm H2O /hour
         evap=AMAX1(Esoil*3600.0/(1.0e6*(2.501-0.00236*Tair)),0.)
!        H2OLv0=2.501e6               !latent heat H2O (J/kg)
      return
      end
!****************************************************************************
!     maintenance respiration
      subroutine respiration(LAIMIN,GPP,Tair,Tsoil,DepH2O,
     &                       Q10,Rl0,Rs0,Rr0,SNRauto,
     &                       LAI,SLA,bmstem,bmroot,bmleaf,
     &                       StemSap,RootSap,NSC,fnsc,
     &                       RmLeaf,RmStem,RmRoot,Rmain)
!     calculate plant and soil respiration by the following equation:
!     RD=BM*Rd*Q10**((T-25)/10) (Sun et al. 2005. Acta Ecologica Sinica)
      implicit none
      real LAIMIN,LAI,GPP,SLA
      real Tair,Tsoil,DepH2O
      real bmstem,bmroot,bmleaf,StemSap,RootSap
      real NSC,fnsc
      real Q10
      real RmLeaf,RmStem,RmRoot,Rmain
      real Rl0,Rs0,Rr0,SNRauto
      real conv                  ! converter from "umol C /m2/s" to "gC/m2/hour"

      conv=3600.*12./1000000.    ! umol C /m2/s--> gC/m2/hour
      if(LAI.gt.LAIMIN) then
      RmLeaf=Rl0*SNRauto*bmleaf*0.45*SLA*0.01
     &       *Q10**((Tair-25.)/10.)*fnsc*conv
      RmStem=Rs0*SNRauto*StemSap*0.001 *Q10**((Tair-25.)/10.)*fnsc*conv
      RmRoot=Rr0*SNRauto*RootSap*0.001 *Q10**((Tair-25.)/10.)*fnsc*conv
      else
      RmLeaf=0.3*GPP
      RmStem=0.3*GPP
      RmRoot=0.4*GPP
      endif
      Rmain=Rmleaf+Rmstem+Rmroot
      if(Rmain > 0.0015*NSC)then
          Rmleaf=Rmleaf/Rmain*0.0015*NSC
          Rmstem=Rmstem/Rmain*0.0015*NSC
          Rmroot=Rmstem/Rmain*0.0015*NSC
          Rmain=Rmleaf+Rmstem+Rmroot
      endif

      return
      end

!=======================================================================

!     subroutine for soil moisture
      subroutine soilwater(wsmax,wsmin,rdepth,FRLEN,!constants specific to soil/plant
     &                rain,tair,transp,wcl,tsoil,Rh,thksl,LAI,       !inputs
     &                evap,runoff,wscontent,fwsoil,topfws,  !outputs
     &                omega,omega_S,WaterR,WaterS,SatFracL)  !outputs
!     All of inputs, the unit of water is 'mm', soil moisture or soil water content is a ratio
       implicit none
!       soil traits
       real wsmax,wsmin,wsmaxL(10),wsminL(10) !from input percent x%
       real FLDCAP,WILTPT,FLDCAPL(10),WILTPTL(10) ! ie. 0.xx
!       plant traits
       real LAI,rdepth
       integer nfr
!       climate conditions
       real precp,rain ! mm/hour
       real tair,TSOIL,ts          ! updated every hour
!       output from canopy model
       real evap,transp,evaptr,TEVAP,AEVAP
!       output variables
       real wscontent,fwsoil,topfws,omega,topomega,omega_S
       real fw(10),ome(10),W_signal
       real WaterR,WaterS(10),SatFracL(10)
!       omega: (wscontent-wiltpt)/(fldcap-wiltpt)
       real RAWCL(10) ! omega of every layers
       real thksl(10),depth(10),wsc(10),WUPL(10),EVAPL(10),SRDT(10)
       real plantup(10)
       real Tsrdt
       real frlen(10) !fraction of root length in every layer
       real wcl(10) !volum ratio
       real fwcln(10) !  fraction of water in layers, like field capacity
       real wtdeficit(10),DWCL(10),Tr_ratio(10)
       real Twater,Twater1,Twater2,Tthk,dWaterS,netflux
       real wtneed,wtadd,twtadd,infilt,runoff,roff_layer,tr_allo

       real RH,Rsoil,Rd,density,sp_heat,psychro,la,P
       real esat
       real exchangeL,supply,demand,omegaL(10)
       integer i,j,k


       WILTPT =wsmin/100.0
       FLDCAP =wsmax/100.0
       WILTPTL=wsmin/100.0
       FLDCAPL=wsmax/100.0

       twater=0.0
       twater1=0.0
       twater2=0.0

       precp=rain

       do i=1,10
           wtdeficit(i)=0.0
           dwcl(i)=0.0
           evapl(i)=0.0
           WUPL(i)=0.0
           SRDT(i)=0.0
           DEPTH(i)=0.0
       enddo

!     Determine which layers are reached by the root system. 
!     Layer volume (cm3)
       DEPTH(1)=10.0
       DO i=2,10
           DEPTH(i)=DEPTH(i-1)+THKSL(i)
       enddo
       do i=1,10
              IF(rdepth.GT.DEPTH(i)) nfr=i+1
       enddo
       IF (nfr.GT.10) nfr=10
       do i=1,10
           if(FLDCAPL(i).gt.wcl(i))wtdeficit(i)=FLDCAPL(i)-wcl(i)
       enddo

! *** water infiltration through layers
      infilt=precp  !mm/hour
!     Loop over all soil layers.
       TWTADD=0
       roff_layer=0.0
      do i=1,10 

         IF(infilt.GT.0.0)THEN
!           Add water to this layer, pass extra water to the next.
            WTADD=AMIN1(INFILT,wtdeficit(i)*thksl(i)*10.0) ! from cm to mm
!           change water content of this layer
            WCL(i)=(WCL(i)*(thksl(i)*10.0)+WTADD)/(thksl(i)*10.0)
            FWCLN(I)=WCL(I)       !  /VOLUM(I)! update fwcln of this layer
            TWTADD=TWTADD+WTADD       !calculating total added water to soil layers (mm)
            INFILT=INFILT-WTADD !update infilt
         ENDIF
!         produce runoff during infiltration
!         if(infilt.GT.0.0)THEN
!             roff_layer=roff_layer + INFILT*0.05*(i-1)
!             INFILT=INFILT -  INFILT*0.05*(i-1)
!         endif
      enddo

!       if(precp.gt.0.0.and.wcl(1).gt.wcl(2))then
!              supply=(wcl(1)-wcl(2))/3.0
!              wcl(1)=wcl(1)-2.0*supply
!              wcl(2)=wcl(2)+supply
!       endif

!      runoff
       runoff=INFILT + roff_layer   !precp-TWTADD + roff_layer   !weng 10072006

!     water redistribution among soil layers
!       do i=1,10
!              wsc(i)=Amax1(0.00,(wcl(i)-wiltpt)*THKSL(i)*10.0)
!              omegaL(i)=Amax1(0.001,(wcl(i)-WILTPT)/(FLDCAPL(i)-WILTPT))
!       enddo
!       supply=0.0
!       demand=0.0
!       do i=1,9
!              if(omegaL(i).gt.0.3)then
!                     supply=wsc(i)/360.0*omegaL(i)
!                     demand=(FLDCAPL(i)-wcl(i+1))*THKSL(i+1)*10.0/360.0
!     &                              *(1.0-omegaL(i+1))
!                     exchangeL=AMIN1(supply,demand)
!                     wsc(i)=wsc(i)- exchangeL
!                     wsc(i+1)=wsc(i+1)+ exchangeL
!                     wcl(i)=wsc(i)/(THKSL(i)*10.0)+wiltpt
!                     wcl(i+1)=wsc(i+1)/(THKSL(i+1)*10.0)+wiltpt
!              endif
!       enddo
!    end of water redistribution among soil layers


!  ***Actual Soil evaporation; SRDT(I) for contribution of each layer. 
!     Units here are g H2O m-2 layer-1 h-1.
       Twater=0
      do i=1,10
          wsc(i)=(wcl(i)-wiltpt)*THKSL(I)*10.0
          Twater=Twater+wsc(i)  ! total water in soils,mm
      enddo

      Tsrdt=0.0
      DO i=1,10
!        Fraction of SEVAP supplied by each soil layer
         SRDT(I)=EXP(-6.73*(DEPTH(I)-THKSL(I)/2.0)/100.0) !/1.987
!        SRDT(I)=AMAX1(0.0,SRDT(I)*(wcl(i)-wiltpt)) !*THKSL(I))
         Tsrdt=Tsrdt+SRDT(i)/(i*i)  ! to normalize SRDT(i)
      enddo

      SRDT=SRDT/Tsrdt
      do i=1,10
         EVAPL(I)=Amax1(AMIN1(evap*SRDT(i),wsc(i)),0.0)  !mm
         DWCL(I)=EVAPL(I)/(THKSL(I)*10.0) !ratio
      enddo

!       update water content of every layer
       do i=1,10
              wcl(i)=wcl(i)-DWCL(i)
       enddo
!       the actual evapration
       evap=0.0       
       do i=1,10
         evap=evap+EVAPL(I)
       enddo
!     WATER UPTAKE by plant roots,Weng, 2.13.2006, a passive proccess
       Twater=0
       do i=1,nfr
              wsc(i)=(wcl(i)-wiltpt)*THKSL(I)*10.0
              Twater=Twater+AMAX1(wsc(i),0.0) ! total water in roots reached soil,mm
       enddo
!       if(transp.gt.Twater/2.0)transp=Twater/2.0                     
       tr_allo=0.0
       do i=1,nfr
              tr_ratio(i)=FRLEN(i)*wsc(i) !*(wcl(i)-wiltpt)) !*THKSL(I))
              tr_allo=tr_allo+tr_ratio(i)
       enddo

       do i=1,nfr
              plantup(i)=AMIN1(transp* tr_ratio(i)/tr_allo, wsc(i)) !mm              
              wupl(i)=plantup(i)/(thksl(i)*10.0)
              wcl(i)=wcl(i)-wupl(i)
       enddo

!       transp=0.0
!       do i=1,nfr
!              transp=transp+plantup(i)
!       enddo

!    output (fwsoil,topfws,omega) which would be used by canopy model
      Twater=0
      Tthk=0
      do i=1,nfr
             Twater=Twater+wcl(i)*THKSL(I)*10.0 ! total water in soils,mm
             Tthk=Tthk+thksl(i)*10.0 !total thicknes of soil layers, mm
      enddo
      wscontent=Twater/Tthk
      if(wscontent.lt.WILTPT) wscontent=WILTPT+0.00001
      omega_S=(wscontent-WILTPT)/(FLDCAP-WILTPT)
      fwsoil=amin1(1.0,3.333*omega) !+0.01
      topfws=amin1(1.0,(wcl(1)-WILTPT)/((FLDCAP-WILTPT)))
      if(fwsoil.lt.0.0) fwsoil=0.000001
      if(topfws.lt.0.0) topfws=0.000001
      if(omega.lt.0.0) omega=0.0000001
      Twater=Twater-WILTPT*Tthk
      WaterR=Twater+WILTPT*Tthk
    
!     a new approach for calculating fwsoil
      do i=1,10 !nfr
         ome(i)=(wcl(i)-WILTPT)/(FLDCAP-WILTPT)
         WaterS(i)=wcl(i)*THKSL(I)*10.0
         ome(i)=AMIN1(1.0,AMAX1(0.0,ome(i)))
         SatFracL(i)=ome(i)
         fw(i)=amin1(1.0,3.333*ome(i))
      enddo
      fwsoil=0.0
      omega=0.0
      do i=1,nfr
         fwsoil=fwsoil+fw(i)*frlen(i)
         omega=omega+ome(i)*frlen(i)
      enddo
      return
      end
!=================================================================

!     plant growth model
      subroutine plantgrowth(Tair,omega,GLmax,GRmax,GSmax,
     &                       LAI,LAIMAX,LAIMIN,SLA,Tau_L,
     &                       bmleaf,bmroot,bmstem,bmplant,
     &                       Rootmax,Stemmax,SapS,SapR,
     &                       StemSap,RootSap,Storage,GDD5,
     &                       stor_use,onset,accumulation,gddonset,
     &                       Sps,NSC,fnsc,NSCmin,NSCmax,
     &                       NSN,CN,CN0,SNgrowth,N_deficit,
     &                       store,add,L_fall,ht,
     &                       NPP,alpha_L,alpha_W,alpha_R,
     &                       RgLeaf,RgStem,RgRoot,Rgrowth)
      implicit none
      real NSC,NSCmin,NSCmax,fnsc,N_deficit
      real CN(8),CN0(8),NSN,nsCN
      real SnscnL,SnscnS,SnscnR
      real store,Storage,GDD5,stor_use,accumulation,gddonset
      integer onset,duration,offset,dormancy
      real GLmax,GRmax,GSmax,TauLeaf
      real GrowthP,GrowthL,GrowthR,GrowthS
      real Tair,omega,LAI,LAIMAX,LAIMIN,SLA
!     biomass
      real bmleaf,bmroot,bmstem,bmplant,NPP
      real ht,hmax,hl0,CNP0
      REAL LAIMAX0,la0,GPmax,acP,c1,c2
      real Rootmax,Stemmax,SapS,SapR
      real bmL,bmR,bmP,bmS,StemSap,RootSap
      real Rgrowth,Rgroot,Rgleaf,Rgstem
!     scalars
      real St,Sw,Ss,Sn,SL_rs,SR_rs,Slai,Sps,SNgrowth,phiN
      real RS,RS0,RSw
      real gamma_W,gamma_Wmax,gamma_T,gamma_Tmax,gamma_N
      real beta_T,Tcold,Twarm,Topt
      real bW,bT,W
      real L_fall,L_add,add,NL_fall,NL_add,Tau_L
      real alpha_L,alpha_W,alpha_R,alpha_St
      real Twsoil(7),Tavg
      integer i

      Twarm=35.0
      Tcold=5.0
      Topt=30.
      phiN=0.33

      bmL=bmleaf*0.45   ! Carbon
      bmR=bmRoot*0.45
      bmS=bmStem*0.45

      if(bmL.lt.NSC/0.333)bmL=NSC/0.333
      if(bmR.lt.NSC/0.333)bmR=NSC/0.333
      if(bmS.lt.NSC/0.334)bmS=NSC/0.334
      StemSap=SapS*bmS  ! Weng 12/05/2008
      RootSap=bmR
      if(StemSap.lt.0.001)StemSap=0.001
      if(RootSap.lt.0.001)RootSap=0.001

      bmP=bmL+bmR+bmS
      acP=bmL+StemSap+bmS
      CNp0=bmP/(bmL/CN0(1)+bmR/CN0(3)+bmS/CN0(2))

      hmax=24.19   ! m
      hl0=0.00019  ! m2/kg C
      LAIMAX0=5.
      la0=0.2
      ht=hmax*(1.-exp(-hl0*bmP))
      LAIMAX=AMAX1(LAIMAX0*(1.-exp(-la0*ht)),LAIMIN+0.1)

!     Phenology
      if((GDD5.gt.gddonset).and.onset.eq.0.and.storage.gt.stor_use) then
            onset=1
      endif
      if((onset.eq.1).and.(storage.gt.stor_use))then
          if(LAI.lt.LAIMAX)add=stor_use
            storage=storage-add
      else
            add=0.0
            onset=0
      endif
      if(accumulation.lt.(NSCmax+0.005*RootSap))then
            store=AMAX1(0.,0.005*NSC)
      else
            store=0.0
      endif
      accumulation=accumulation+store

!     Scalars for plant growth
!      Sps=Amin1(1.0,3.33*AMAX1(0.0,1.0 - fnsc))
      Sps=Sps*(1.-exp(-phiN*NSN))
      Ss=AMIN1(1.0,2.*fnsc)
      RS0=1.0
      RS=bmR/bmL
      SL_rs=RS/(RS+RS0*(2.-W))
      SR_rs=(RS0*(2.-W))/(RS+RS0*(2.-W))
      Slai=amin1(1.0,2.333*(LAIMAX-LAI)/(LAIMAX-LAIMIN))
      St=AMAX1(0.0, 1.0-exp(-(Tair-Tcold-5.)/5.0))  !0.5 !
      Sw=AMAX1(0.333, 0.333+omega)
      W = AMIN1(1.0,3.333*omega)

!     Plant growth and allocation
      GPmax=(GLmax*bmL+GSmax*StemSap+GRmax*bmR) !/acP
      GrowthP=AMIN1(GPmax*fnsc*St*(1.-exp(-NSN)),   ! 
     &              0.004*NSC,
     &              0.004*NSN*CNp0)
!      c1=(bmR+200.)/bmL*CN(1)/CN0(1) !+N_deficit/NSN
      c1=bmL/bmR*CN(1)/CN0(1) !+N_deficit/NSN
      c2=0.5*250e3*SLA*0.00021*ht*2.

      GrowthL=MAX(0.0,MIN(GrowthP*0.4,(LAIMAX-LAI)/(SLA/0.45)))    ! 1./(1.+c1+c2)
      GrowthR=MIN(GrowthP*0.3,MAX(0.0,1.5/Sw*bmL-bmR))  ! *c1/(1.+c1+c2)
      GrowthS=MAX(0.0,GrowthP - (GrowthL+GrowthR) )         ! *c2/(1.+c1+c2)

      NPP = GrowthL + GrowthR + GrowthS + store

      if(NPP.eq.0.0)then
            alpha_L=0.333
            alpha_W=0.333
            alpha_R=0.333
      else
            alpha_L=(GrowthL+add)/NPP
            alpha_W=GrowthS/NPP
            alpha_R=GrowthR/NPP
      endif
!     Carbon cost for growth
!     Rgrowth,Rgroot,Rgleaf,Rgstem, 0.5 is from IBIS and Amthor, 1984
      Rgleaf=0.5*GrowthL
      Rgstem=0.5*GrowthS
      Rgroot=0.5*GrowthR
      Rgrowth=Rgleaf+Rgstem+Rgroot

!     Leaf litter 

      gamma_Wmax=0.12/24. ! maxmum leaf fall rate per hour
      gamma_Tmax=0.12/24.

      bW=4.0
      bT=2.0

      if(Tair.gt.(Tcold+10.)) then
            beta_T=1.
      else 
            if(Tair.gt.Tcold)beta_T=(Tair-Tcold)/10.
            if(Tair.LE.Tcold)beta_T=0.
      endif

      if (tau_L < 8760.)then
                gamma_W=(1. - W)     **bW  * gamma_Wmax
                gamma_T=(1. - beta_T)**bT * gamma_Tmax
      else
                gamma_W=0.
                gamma_T=0.
      endif
      gamma_N=1.0/(Tau_L*Sw)
      if(LAI < LAIMIN) then
            gamma_W=0.
            gamma_T=0.
            gamma_N=0.
      endif
      L_fall=bmleaf*0.45*AMIN1((gamma_W+gamma_T+gamma_N),0.99)

      return
      end

!===========================================================================
!     carbon transfer according to Xu et al. 2007 
      subroutine TCS_CN(Tair,Tsoil,omega,runoff,
     &               NPP,alpha_L,alpha_W,alpha_R,L_fall,
     &               tauC,QC,OutC,Rh_pools,Rnitrogen,NSC,
     &               CNmin,CNmax,NSNmax,NSNmin,alphaN,            ! nitrogen
     &               NSN,N_uptake,N_miner,QN,QNminer,
     &               CN,CN0,fnsc,rdepth,N_deficit,
     &               N_leaf,N_wood,N_root,N_LF,N_WF,N_RF,
     &               N_deposit,N_fixation,N_leach,N_vol,
     &               SNvcmax,SNgrowth,SNRauto,SNrs) 
         
      implicit none
      real NPP,NPP_L,NPP_W,NPP_R
      real L_fall,L_add,LAI,SLA,rdepth
      real Tair,Tsoil,omega,runoff
!     allocation ratios
      real alpha_L,alpha_W,alpha_R
!     pools
      real Q_plant,QC(8),TauC(8),OutC(8)
      real etaL,etaW,etaR                ! the percentage of fine litter of the litters from plant parts
      real f_F2M,f_C2M,f_C2S,f_M2S,f_M2P,f_S2P,f_S2M,f_P2M
      real Rh_pools(5),Q10h(5) ! Q10 of the litter and soil C pools
!     the fraction of C-flux which enters the atmosphere from the kth pool
      real f_CO2_fine,f_CO2_coarse,f_CO2_Micr,f_CO2_Slow,f_CO2_Pass
!     for nitrogen sub-model
      real CN0(8),CN(8),OutN(8),QN(8),QNminer,QNplant
      real CNmin,CNmax,NSNmax,NSNmin,NSN
      real CN_plant,CN_foliage
      real N_demand,N_deficit,N_immob,N_imm(5),N_fixation,Nfix0
      real N_transfer,N_miner,N_uptake,N_deposit,N_loss,N_leach,N_vol
      real alphaN,Qroot0,Cfix0
      real Scalar_N_flow,Scalar_N_T
      real N_leaf,N_wood,N_root,N_npp
      real N_LF,N_WF,N_RF
      real NSC,fnsc,ksye
      real SNvcmax,SNgrowth,SNRauto,SNrs
      real kappaVcmax
      real SNfine,SNcoarse,SNmicr,SNslow,SNpass
      real Rnitrogen,costCuptake,costCfix,costCreuse
      real Creuse0,Nup0,N_deN0,LDON0
!     the variables relative to soil moisture calcualtion
      real S_omega    !  average values of the moisture scaling functions
      real S_t(5)     !  average values of temperature scaling functions
      real S_w_min    !  minimum decomposition rate at zero available water
!     For test
      real totalC1,totalN1,C_in,C_out,N_in,N_out,totalC2,totalN2
      real ScNloss

      integer i,j,k,n,m
      integer day,week,month,year

!     temperature sensitivity of Rh_pools
!      Q10h=(/2.0,2.0,2.0,2.0,2.0/)  ! for Oak Ridge
      Q10h=(/2.2,2.2,2.2,2.2,2.2/)  ! for Oak Ridge
!
      Qroot0=500.
      Nfix0=1./60.   ! maximum N fix ratio, N/C
      Nup0 =0.02     ! nitrogen uptake rate
      Cfix0=12.      ! C cost per N for fixation
      ksye=0.05      ! C cost per N for uptake
      Creuse0=2.     ! C cost per N for resorption
      ScNloss=1.
      N_deN0=1.E-3*ScNloss   ! 1.E-3, 5.E-3, 10.E-3, 20.E-3
      LDON0=1.E-3*ScNloss

      Rnitrogen=0.
!     for N scalars
      CNmin=40.0
      CNmax=200.0
!     Max and min NSN pool
      NSNmax = QN(1) + 0.2*QN(2) + QN(3)  ! 15.0
      NSNmin = 0.01
!     partitioning coefficients
      etaL=0.6          ! 60% of foliage litter is fine, didn't use
      etaW=0.001        ! 15% of woody litter is fine
      etaR=0.85         ! 85% of root litter is fine  , didn't use    
      f_F2M=0.55        ! *exp((CN0(4)-CN_fine)*0.1)
      f_C2M=0.275       ! *exp((CN0(5)-CN_coarse)*0.1)
      f_C2S=0.275       ! *exp((CN0(5)-CN_coarse)*0.1)
      f_M2S=0.496
      f_M2P=0.004
      f_S2P=0.03
      f_S2M=0.42
      f_P2M=0.45

!     calculating soil scaling factors, S_omega and S_tmperature
      S_w_min=0.08 !minimum decomposition rate at zero soil moisture
      S_omega=S_w_min + (1.-S_w_min) * Amin1(1.0,2.0*omega)

      do i=1,5
!        S_t(i)=Q10h(i)**((Tsoil-5.)/10.)  ! Oak
         S_t(i)=Q10h(i)**((Tsoil-0.)/10.)  ! Duke
      enddo

!     Calculating NPP allocation and changes of each C pool
      NPP_L = alpha_L * NPP            ! NPP allocation
      NPP_W = alpha_W * NPP
      NPP_R = alpha_R * NPP
!     N scalars on decomposition
      SNfine  =exp(-(CN0(4)-CN(4))/CN0(4)) 
      SNcoarse=exp(-(CN0(5)-CN(5))/CN0(5)) 
      SNmicr  =exp(-(CN0(6)-CN(6))/CN0(6)) 
      SNslow  =1. !exp(-(CN0(7)-CNC(7))/CN0(7)) 
      SNpass  =exp(-(CN0(8)-CN(8))/CN0(8)) 
      
!     the carbon leaving the pools
      OutC(1)=L_fall  ! Turnover of leaves
      OutC(2)=QC(2)/tauC(2)*S_omega ! Turnover of woody biomass 
      OutC(3)=QC(3)/tauC(3)*S_omega ! Turnover of fine roots

      OutC(4)=QC(4)/tauC(4)*S_omega* S_T(1)*CN(4)/CN0(4)!*SNfine
      OutC(5)=QC(5)/tauC(5)*S_omega* S_T(2)*CN(5)/CN0(5)!*SNcoarse
      OutC(6)=QC(6)/tauC(6)*S_omega* S_T(3)!*SNmicr
      OutC(7)=QC(7)/tauC(7)*S_omega* S_T(4)!*SNslow
      OutC(8)=QC(8)/tauC(8)*S_omega* S_T(5)!*SNpass

!     heterotrophic respiration from each pool
      Rh_pools(1)=OutC(4)* (1. - f_F2M)
      Rh_pools(2)=OutC(5)* (1. - f_C2M - f_C2S)
      Rh_pools(3)=OutC(6)* (1. - f_M2S - f_M2P)
      Rh_pools(4)=OutC(7)* (1. - f_S2P - f_S2M)
      Rh_pools(5)=OutC(8)* (1. - f_P2M)

!========================================================================
!     Nitrogen part
!     nitrogen leaving the pools and resorption
      do i=1,8
         OutN(i) = OutC(i)/CN(i)
      enddo

!     nitrogen mineralization
      N_miner=OutN(4)* (1. - f_F2M)
     &       +OutN(5)* (1. - f_C2M - f_C2S)
     &       +OutN(6)* (1. - f_M2S - f_M2P)
     &       +OutN(7)* (1. - f_S2P - f_S2M)
     &       +OutN(8)* (1. - f_P2M)

!     Nitrogen immobilization
      N_imm=0.
      N_immob=0.
      if(QNminer>0)then
          do i=4,8
            if(CN(i)>CN0(i))then
                N_imm(i-3)=Amin1(QC(i)/CN0(i)-QC(i)/CN(i)
     &             ,0.1*QNminer)
                N_immob=N_immob+N_imm(i-3)
            endif
          enddo
      endif

!     Let plant itself choose the strategy between using C to uptake
!     or fix N2 by comparing C invest.
!     N demand
      N_demand=NPP_L/CN0(1)+NPP_W/CN0(2)+NPP_R/CN0(3) !+N_deficit
!     Nitrogen input:
      N_transfer=0.
	N_uptake=0.
	N_fixation=0.
      costCuptake=0.
      costCfix=0.
      costCreuse=0.
!     1. Nitrogen resorption
      N_transfer=(OutN(1) + OutN(2) +OutN(3))*alphaN
      costCreuse= Creuse0*N_transfer
      N_demand=N_demand-N_transfer

      If(N_demand>0.0)then

!     2.  N uptake
          if(ksye/QNminer<Cfix0)then
              N_uptake=AMIN1(N_demand+N_deficit,
     &                       QNminer*QC(3)/(QC(3)+Qroot0),
     &                       Nup0*NSC/(ksye/QNminer))
              costCuptake=N_uptake*(ksye/QNminer)
              N_demand=N_demand-N_uptake
          elseif(NSN<24.*30.*N_demand)then
!     3.  Nitrogen fixation
              N_fixation=Amin1(N_demand,fnsc*Nfix0*NSC)
              costCfix=Cfix0*N_fixation
              N_demand=N_demand-N_fixation
          endif
      endif
      N_deficit=N_deficit+N_demand

!     update NSN
      NSN=NSN+N_transfer+N_uptake+N_fixation
!     Total C cost for nitrogen
      Rnitrogen=costCuptake+costCfix+costCreuse

!      Oak Ridge N fixation rate: 
!      asymbiotic: 2 mg N/m2/yr ;  symbiotic: 65 mg/m2/yr, Oak Ridge
!      N_fixation=0.067/8760. ! Oak Ridge
!      N_fixation=0.23/8760.  ! Duke

!     Nitrogen using, non-structural nitrogen pool, NSN
      N_leaf =AMIN1(NPP*alpha_L/CN(1)+QC(1)/CN0(1)-QC(1)/CN(1),0.2*NSN)
      N_wood =AMIN1(NPP*alpha_W/CN(2)                         ,0.1*NSN)
      N_root =AMIN1(NPP*alpha_R/CN(3)+QC(3)/CN0(3)-QC(3)/CN(3),0.2*NSN)
      NSN=NSN-(N_leaf+N_wood+N_root)

      N_LF=OutN(1)*(1.-alphaN)
      N_WF=OutN(2)*(1.-alphaN)
      N_RF=OutN(3)*(1.-alphaN)

!     update QNminer
      QNminer=QNminer+N_miner+N_deposit
     &              -(N_uptake+N_immob)

!     Loss of mineralized N and dissolved organic N
      Scalar_N_flow=0.5*runoff/rdepth
!      Scalar_N_T=0.005*(Tsoil+273.)/(Tsoil+273+333.)
      Scalar_N_T=N_deN0*exp((Tsoil-25.)/10.)
      N_leach=Scalar_N_flow*QNminer+Scalar_N_flow*QN(6)*LDON0
      N_vol  =Scalar_N_T*QNminer
      N_loss =N_leach + N_vol

!     update QNminer
      QNminer=QNminer-N_loss


!     update plant carbon pools, ! daily change of each pool size
      QC(1)=QC(1) - OutC(1) + NPP_L    
      QC(2)=QC(2) - OutC(2) + NPP_W
      QC(3)=QC(3) - OutC(3) + NPP_R
      QC(4)=QC(4) - OutC(4) + OutC(1)+etaW*OutC(2)+OutC(3)
      QC(5)=QC(5) - OutC(5) + (1.-etaW)*OutC(2)
      QC(6)=QC(6) - OutC(6) + f_F2M*OutC(4)+f_C2M*OutC(5)            
     &                      + f_S2M*OutC(7)+f_P2M * OutC(8)
      QC(7)=QC(7) - OutC(7)+f_C2S*OutC(5)+f_M2S*OutC(6)
      QC(8)=QC(8) - OutC(8)+f_M2P*OutC(6)+f_S2P*OutC(7)

      Q_plant =QC(1) + QC(2) + QC(3)
!     update nitrogen pools
      QN(1)=QN(1) - OutN(1) + N_leaf
      QN(2)=QN(2) - OutN(2) + N_wood
      QN(3)=QN(3) - OutN(3) + N_root
      QN(4)=QN(4) - OutN(4)+ N_imm(1)
     &            + (OutN(1) + etaW*OutN(2) + OutN(3))*(1.-alphaN)
      QN(5)=QN(5) - OutN(5) + N_imm(2)
     &            + (1.-etaW)*OutN(2)*(1.-alphaN)

      QN(6)=QN(6) - OutN(6) + N_imm(3) - Scalar_N_flow*QN(6)*LDON0
     &            + f_F2M*OutN(4)+f_C2M*OutN(5)
     &            + f_S2M*OutN(7)+f_P2M*OutN(8)
      QN(7)= QN(7) - OutN(7) + N_imm(4)
     &                         + f_C2S*OutN(5)
     &                         + f_M2S*OutN(6)
      QN(8)= QN(8) - OutN(8) + N_imm(5)
     &         + f_M2P*OutN(6) + f_S2P*OutN(7)
      QNplant = QN(1) + QN(2)+ QN(3)

!     update C/N ratio
      CN=QC/QN
      CN_foliage=(QC(1)+QC(3))/(QN(1)+QN(3))

!     calculate N related scalars for Oak Ridge
!      SNvcmax =exp(-(CN(1)-CN0(1))) ! /CN0(1) ! Oak Ridge
!      SNgrowth=exp(-(CN(1)-CN0(1))/CN0(1)) !  AMAX1((CNmax-CN_foliage)/(CNmax-CNmin),0.0)+0.25
!      SNRauto =AMAX1((CNmax-CN_foliage)/(CNmax-CNmin),0.0)+0.5
!      SNrs=1.

!     calculate N related scalars for Duke FACE
      kappaVcmax=CN0(1)/1.
      SNvcmax =exp(-kappaVcmax*(CN(1)-CN0(1))/CN0(1)) ! /CN0(1) ! Duke
      SNgrowth=exp(-(CN(1)-CN0(1))/CN0(1)) !  AMAX1((CNmax-CN_foliage)/(CNmax-CNmin),0.0)+0.25
      SNRauto =exp(-(CN(1)-CN0(1))/CN0(1)) !  AMAX1((CNmax-CN_foliage)/(CNmax-CNmin),0.0)+0.5
      SNrs=1.

      return
      end

!     ========================================================================================
!     subroutines used by canopy submodel
      subroutine xlayers(Sps,Tair,Dair,radabv,G,Esoil,fbeam,eairP,
     &           windU0,co2ca,fwsoil,wcl,FLAIT,coszen,idoy,hours,
     &           tauL,rhoL,rhoS,xfang,extkd,extkU,wleaf,
     &           Rconst,sigma,emleaf,emsoil,theta,a1,Ds0,
     &           cpair,Patm,Trefk,H2OLv0,AirMa,H2OMw,Dheat,
     &           gsw0,alpha,stom_n,wsmax,wsmin,
     &           Vcmx0,eJmx0,conKc0,conKo0,Ekc,Eko,o2ci,
     &           Eavm,Edvm,Eajm,Edjm,Entrpy,gam0,gam1,gam2,
     &           extKb,Rsoilabs,Hsoil,Acan1,Acan2,Ecan1,Ecan2,
     &           RnStL,QcanL,RcanL,AcanL,EcanL,HcanL,GbwcL,GswcL)


!    the multi-layered canopy model developed by 
!    Ray Leuning with the new radiative transfer scheme   
!    implemented by Y.P. Wang (from Sellers 1986)
!    12/Sept/96 (YPW) correction for mean surface temperature of sunlit
!    and shaded leaves
!    Tleaf,i=sum{Tleaf,i(n)*fslt*Gaussw(n)}/sum{fslt*Gaussw(n)} 
!    
      real Gaussx(5),Gaussw(5)
      real layer1(5),layer2(5)
      real tauL(3),rhoL(3),rhoS(3),Qabs(3,2),Radabv(2),Rnstar(2)
      real Aleaf(2),Eleaf(2),Hleaf(2),Tleaf(2),co2ci(2)
      real gbleaf(2),gsleaf(2),QSabs(3,2),Qasoil(2)
      integer ng,nw
      real rhoc(3,2),reff(3,2),kpr(3,2),scatt(2)       !Goudriaan

      real rsoil,rlai,raero
      real wsmax,wsmin,WILTPT,FILDCP,wcl(10)

!    additional arrays to allow output of info for each Layer
      real RnStL(5),QcanL(5),RcanL(5),AcanL(5),EcanL(5),HcanL(5)
      real GbwcL(5),GswcL(5)
      
! Normalised Gaussian points and weights (Goudriaan & van Laar, 1993, P98)
!* 5-point
      data Gaussx/0.0469101,0.2307534,0.5,0.7692465,0.9530899/
      data Gaussw/0.1184635,0.2393144,0.2844444,0.2393144,0.1184635/

!     soil water conditions
      WILTPT=wsmax/100.
      FILDCP=wsmin/100.
!     reset the vairables
      Rnst1=0.0        !net rad, sunlit
      Rnst2=0.0        !net rad, shaded
      Qcan1=0.0        !vis rad
      Qcan2=0.0
      Rcan1=0.0        !NIR rad
      Rcan2=0.0
      Acan1=0.0        !CO2
      Acan2=0.0
      Ecan1=0.0        !Evap
      Ecan2=0.0
      Hcan1=0.0        !Sens heat
      Hcan2=0.0
      Gbwc1=0.0        !Boundary layer conductance
      Gbwc2=0.0
      Gswc1=0.0        !Canopy conductance
      Gswc2=0.0
      Tleaf1=0.0       !Leaf Temp
      Tleaf2=0.0  
  
!     aerodynamic resistance                                                
      raero=50./windU0                           

!    Ross-Goudriaan function for G(u) (see Sellers 1985, Eq 13)
      xphi1 = 0.5 - 0.633*xfang -0.33*xfang*xfang
      xphi2 = 0.877 * (1.0 - 2.0*xphi1)
      funG=xphi1 + xphi2*coszen                             !G-function: Projection of unit leaf area in direction of beam
      
      if(coszen.gt.0) then                                  !check if day or night
        extKb=funG/coszen                                   !beam extinction coeff - black leaves
      else
        extKb=100.
      end if

!     Goudriaan theory as used in Leuning et al 1995 (Eq Nos from Goudriaan & van Laar, 1994)
!     Effective extinction coefficient for diffuse radiation Goudriaan & van Laar Eq 6.6)
      pi180=3.1416/180.
      cozen15=cos(pi180*15)
      cozen45=cos(pi180*45)
      cozen75=cos(pi180*75)
      xK15=xphi1/cozen15+xphi2
      xK45=xphi1/cozen45+xphi2
      xK75=xphi1/cozen75+xphi2
      transd=0.308*exp(-xK15*FLAIT)+0.514*exp(-xK45*FLAIT)+
     &       0.178*exp(-xK75*FLAIT)
      extkd=(-1./FLAIT)*alog(transd)
      extkn=extkd                        !N distribution coeff 

!canopy reflection coefficients (Array indices: first;  1=VIS,  2=NIR
!                                               second; 1=beam, 2=diffuse
      do nw=1,2                                                      !nw:1=VIS, 2=NIR
       scatt(nw)=tauL(nw)+rhoL(nw)                      !scattering coeff
       if((1.-scatt(nw))<0.0)scatt(nw)=0.9999           ! Weng 10/31/2008
       kpr(nw,1)=extKb*sqrt(1.-scatt(nw))               !modified k beam scattered (6.20)
       kpr(nw,2)=extkd*sqrt(1.-scatt(nw))             !modified k diffuse (6.20)
       rhoch=(1.-sqrt(1.-scatt(nw)))/(1.+sqrt(1.-scatt(nw)))            !canopy reflection black horizontal leaves (6.19)
       rhoc15=2.*xK15*rhoch/(xK15+extkd)                                !canopy reflection (6.21) diffuse
       rhoc45=2.*xK45*rhoch/(xK45+extkd)
       rhoc75=2.*xK75*rhoch/(xK75+extkd)
       rhoc(nw,2)=0.308*rhoc15+0.514*rhoc45+0.178*rhoc75
       rhoc(nw,1)=2.*extKb/(extKb+extkd)*rhoch                          !canopy reflection (6.21) beam 
       reff(nw,1)=rhoc(nw,1)+(rhoS(nw)-rhoc(nw,1))                      !effective canopy-soil reflection coeff - beam (6.27)
     &            *exp(-2.*kpr(nw,1)*FLAIT) 
       reff(nw,2)=rhoc(nw,2)+(rhoS(nw)-rhoc(nw,2))                      !effective canopy-soil reflection coeff - diffuse (6.27)
     &            *exp(-2.*kpr(nw,2)*FLAIT)  
      enddo


!     isothermal net radiation & radiation conductance at canopy top - needed to calc emair
      call Radiso(flait,flait,Qabs,extkd,Tair,eairP,cpair,Patm,
     &            fbeam,airMa,Rconst,sigma,emleaf,emsoil,
     &            emair,Rnstar,grdn)
      TairK=Tair+273.2

!     below      
      do ng=1,5
         flai=gaussx(ng)*FLAIT
!        radiation absorption for visible and near infra-red
         call goudriaan(FLAI,coszen,radabv,fbeam,reff,kpr,
     &                  scatt,xfang,Qabs) 
!        isothermal net radiation & radiation conductance at canopy top
         call Radiso(flai,flait,Qabs,extkd,Tair,eairP,cpair,Patm,
     &               fbeam,airMa,Rconst,sigma,emleaf,emsoil,
     &               emair,Rnstar,grdn)
         windUx=windU0*exp(-extkU*flai)             !windspeed at depth xi
         scalex=exp(-extkn*flai)                    !scale Vcmx0 & Jmax0
         Vcmxx=Vcmx0*scalex
         eJmxx=eJmx0*scalex
         if(radabv(1).ge.10.0) then                          !check solar Radiation > 10 W/m2
!           leaf stomata-photosynthesis-transpiration model - daytime
            call agsean_day(Sps,Qabs,Rnstar,grdn,windUx,Tair,Dair,
     &               co2ca,wleaf,raero,theta,a1,Ds0,fwsoil,idoy,hours,
     &               Rconst,cpair,Patm,Trefk,H2OLv0,AirMa,H2OMw,Dheat,
     &               gsw0,alpha,stom_n,
     &               Vcmxx,eJmxx,conKc0,conKo0,Ekc,Eko,o2ci,
     &               Eavm,Edvm,Eajm,Edjm,Entrpy,gam0,gam1,gam2,
     &               Aleaf,Eleaf,Hleaf,Tleaf,gbleaf,gsleaf,co2ci)
         else
            call agsean_ngt(Sps,Qabs,Rnstar,grdn,windUx,Tair,Dair,
     &               co2ca,wleaf,raero,theta,a1,Ds0,fwsoil,idoy,hours,
     &               Rconst,cpair,Patm,Trefk,H2OLv0,AirMa,H2OMw,Dheat,
     &               gsw0,alpha,stom_n,
     &               Vcmxx,eJmxx,conKc0,conKo0,Ekc,Eko,o2ci,
     &               Eavm,Edvm,Eajm,Edjm,Entrpy,gam0,gam1,gam2,
     &               Aleaf,Eleaf,Hleaf,Tleaf,gbleaf,gsleaf,co2ci)
         endif  
         fslt=exp(-extKb*flai)                        !fraction of sunlit leaves
         fshd=1.0-fslt                                !fraction of shaded leaves
         Rnst1=Rnst1+fslt*Rnstar(1)*Gaussw(ng)*FLAIT  !Isothermal net rad`
         Rnst2=Rnst2+fshd*Rnstar(2)*Gaussw(ng)*FLAIT
         RnstL(ng)=Rnst1+Rnst2
!
         Qcan1=Qcan1+fslt*Qabs(1,1)*Gaussw(ng)*FLAIT  !visible
         Qcan2=Qcan2+fshd*Qabs(1,2)*Gaussw(ng)*FLAIT
         QcanL(ng)=Qcan1+Qcan2
!
         Rcan1=Rcan1+fslt*Qabs(2,1)*Gaussw(ng)*FLAIT  !NIR
         Rcan2=Rcan2+fshd*Qabs(2,2)*Gaussw(ng)*FLAIT
         RcanL(ng)=Rcan1+Rcan2
!
         if(Aleaf(1).lt.0.0)Aleaf(1)=0.0      !Weng 2/16/2006
         if(Aleaf(2).lt.0.0)Aleaf(2)=0.0      !Weng 2/16/2006

         Acan1=Acan1+fslt*Aleaf(1)*Gaussw(ng)*FLAIT*stom_n    !amphi/hypostomatous
         Acan2=Acan2+fshd*Aleaf(2)*Gaussw(ng)*FLAIT*stom_n
         AcanL(ng)=Acan1+Acan2

         layer1(ng)=Aleaf(1)
         layer2(ng)=Aleaf(2)

         Ecan1=Ecan1+fslt*Eleaf(1)*Gaussw(ng)*FLAIT
         Ecan2=Ecan2+fshd*Eleaf(2)*Gaussw(ng)*FLAIT
         EcanL(ng)=Ecan1+Ecan2
!
         Hcan1=Hcan1+fslt*Hleaf(1)*Gaussw(ng)*FLAIT
         Hcan2=Hcan2+fshd*Hleaf(2)*Gaussw(ng)*FLAIT
         HcanL(ng)=Hcan1+Hcan2
!
         Gbwc1=Gbwc1+fslt*gbleaf(1)*Gaussw(ng)*FLAIT*stom_n
         Gbwc2=Gbwc2+fshd*gbleaf(2)*Gaussw(ng)*FLAIT*stom_n
!
         Gswc1=Gswc1+fslt*gsleaf(1)*Gaussw(ng)*FLAIT*stom_n
         Gswc2=Gswc2+fshd*gsleaf(2)*Gaussw(ng)*FLAIT*stom_n
!
         Tleaf1=Tleaf1+fslt*Tleaf(1)*Gaussw(ng)*FLAIT
         Tleaf2=Tleaf2+fshd*Tleaf(2)*Gaussw(ng)*FLAIT
      enddo  ! 5 layers

      FLAIT1=(1.0-exp(-extKb*FLAIT))/extkb
      Tleaf1=Tleaf1/FLAIT1
      Tleaf2=Tleaf2/(FLAIT-FLAIT1)

!     Soil surface energy and water fluxes
!    Radiation absorbed by soil
      Rsoilab1=fbeam*(1.-reff(1,1))*exp(-kpr(1,1)*FLAIT)
     &         +(1.-fbeam)*(1.-reff(1,2))*exp(-kpr(1,2)*FLAIT)          !visible
      Rsoilab2=fbeam*(1.-reff(2,1))*exp(-kpr(2,1)*FLAIT)
     &         +(1.-fbeam)*(1.-reff(2,2))*exp(-kpr(2,2)*FLAIT)          !NIR
      Rsoilab1=Rsoilab1*Radabv(1)
      Rsoilab2=Rsoilab2*Radabv(2)
!  
      Tlk1=Tleaf1+273.2
      Tlk2=Tleaf2+273.2
!      temp1=-extkd*FLAIT
      QLair=emair*sigma*(TairK**4)*exp(-extkd*FLAIT)
      QLleaf=emleaf*sigma*(Tlk1**4)*exp(-extkb*FLAIT)
     &      +emleaf*sigma*(Tlk2**4)*(1.0-exp(-extkb*FLAIT))
      QLleaf=QLleaf*(1.0-exp(-extkd*FLAIT)) 
      QLsoil=emsoil*sigma*(TairK**4)
      Rsoilab3=(QLair+QLleaf)*(1.0-rhoS(3))-QLsoil

!    Net radiation absorbed by soil
!    the old version of net long-wave radiation absorbed by soils 
!    (with isothermal assumption)
!     Rsoil3=(sigma*TairK**4)*(emair-emleaf)*exp(-extkd*FLAIT)         !Longwave
!     Rsoilab3=(1-rhoS(3))*Rsoil3

!    Total radiation absorbed by soil    
      Rsoilabs=Rsoilab1+Rsoilab2+Rsoilab3 

!    thermodynamic parameters for air
      TairK=Tair+273.2
      rhocp=cpair*Patm*AirMa/(Rconst*TairK)
      H2OLv=H2oLv0-2.365e3*Tair
      slope=(esat(Tair+0.1)-esat(Tair))/0.1
      psyc=Patm*cpair*AirMa/(H2OLv*H2OMw)
      Cmolar=Patm/(Rconst*TairK)
      fw1=AMIN1(AMAX1((FILDCP-wcl(1))/(FILDCP-WILTPT),0.05),1.0)
      Rsoil=30.*exp(0.2/fw1)
      rLAI=exp(FLAIT)
!     latent heat flux into air from soil
!           Eleaf(ileaf)=1.0*
!     &     (slope*Y*Rnstar(ileaf)+rhocp*Dair/(rbH_L+raero))/    !2* Weng 0215
!     &     (slope*Y+psyc*(rswv+rbw+raero)/(rbH_L+raero))
      Esoil=(slope*(Rsoilabs-G)+rhocp*Dair/(raero+rLAI))/
     &      (slope+psyc*(rsoil/(raero+rLAI)+1.))
!     sensible heat flux into air from soil
      Hsoil=Rsoilabs-Esoil-G

      return
      end 

!     ****************************************************************************
      subroutine goudriaan(FLAI,coszen,radabv,fbeam,reff,kpr,
     &                  scatt,xfang,Qabs)
     
!    for spheric leaf angle distribution only
!    compute within canopy radiation (PAR and near infra-red bands)
!    using two-stream approximation (Goudriaan & vanLaar 1994)
!    tauL: leaf transmittance
!    rhoL: leaf reflectance
!    rhoS: soil reflectance
!    sfang XiL function of Ross (1975) - allows for departure from spherical LAD
!         (-1 vertical, +1 horizontal leaves, 0 spherical)
!    FLAI: canopy leaf area index
!    funG: Ross' G function
!    scatB: upscatter parameter for direct beam
!    scatD: upscatter parameter for diffuse
!    albedo: single scattering albedo
!    output:
!    Qabs(nwave,type), nwave=1 for visible; =2 for NIR,
!                       type=1 for sunlit;   =2 for shaded (W/m2)

      real radabv(2)
      real Qabs(3,2),reff(3,2),kpr(3,2),scatt(2)
      xu=coszen                                         !cos zenith angle
      
!     Ross-Goudriaan function for G(u) (see Sellers 1985, Eq 13)
      xphi1 = 0.5 - 0.633*xfang -0.33*xfang*xfang
      xphi2 = 0.877 * (1.0 - 2.0*xphi1)
      funG=xphi1 + xphi2*xu                             !G-function: Projection of unit leaf area in direction of beam
      
      if(coszen.gt.0) then                                  !check if day or night
        extKb=funG/coszen                                   !beam extinction coeff - black leaves
      else
        extKb=100.
      end if
                       
! Goudriaan theory as used in Leuning et al 1995 (Eq Nos from Goudriaan & van Laar, 1994)
      do nw=1,2
       Qd0=(1.-fbeam)*radabv(nw)                                          !diffuse incident radiation
       Qb0=fbeam*radabv(nw)                                               !beam incident radiation
       Qabs(nw,2)=Qd0*(kpr(nw,2)*(1.-reff(nw,2))*exp(-kpr(nw,2)*FLAI))+   !absorbed radiation - shaded leaves, diffuse
     &            Qb0*(kpr(nw,1)*(1.-reff(nw,1))*exp(-kpr(nw,1)*FLAI)-    !beam scattered
     &            extKb*(1.-scatt(nw))*exp(-extKb*FLAI))
       Qabs(nw,1)=Qabs(nw,2)+extKb*Qb0*(1.-scatt(nw))                     !absorbed radiation - sunlit leaves 
      end do
      return
      end

!****************************************************************************
      subroutine Radiso(flai,flait,Qabs,extkd,Tair,eairP,cpair,Patm,
     &                  fbeam,airMa,Rconst,sigma,emleaf,emsoil,
     &                  emair,Rnstar,grdn)
!     output
!     Rnstar(type): type=1 for sunlit; =2 for shaded leaves (W/m2)
!     23 Dec 1994
!     calculates isothermal net radiation for sunlit and shaded leaves under clear skies
!     implicit real (a-z)
      real Rnstar(2)
      real Qabs(3,2)
      TairK=Tair+273.2

! thermodynamic properties of air
      rhocp=cpair*Patm*airMa/(Rconst*TairK)   !volumetric heat capacity (J/m3/K)

! apparent atmospheric emissivity for clear skies (Brutsaert, 1975)
      emsky=0.642*(eairP/Tairk)**(1./7)       !note eair in Pa
     
! apparent emissivity from clouds (Kimball et al 1982)
      ep8z=0.24+2.98e-12*eairP*eairP*exp(3000/TairK)
      tau8=amin1(1.0,1.0-ep8z*(1.4-0.4*ep8z))            !ensure tau8<1
      emcloud=0.36*tau8*(1.-fbeam)*(1-10./TairK)**4      !10 from Tcloud = Tair-10

! apparent emissivity from sky plus clouds      
!      emair=emsky+emcloud
! 20/06/96
      emair=emsky

      if(emair.gt.1.0) emair=1.0
      
! net isothermal outgoing longwave radiation per unit leaf area at canopy
! top & thin layer at flai (Note Rn* = Sn + Bn is used rather than Rn* = Sn - Bn in Leuning et al 1985)
      Bn0=sigma*(TairK**4.)
      Bnxi=Bn0*extkd*(exp(-extkd*flai)*(emair-emleaf)
     &    + exp(-extkd*(flait-flai))*(emsoil-emleaf))
!     isothermal net radiation per unit leaf area for thin layer of sunlit and
!     shaded leaves
      Rnstar(1)=Qabs(1,1)+Qabs(2,1)+Bnxi
      Rnstar(2)=Qabs(1,2)+Qabs(2,2)+Bnxi
!     radiation conductance (m/s) @ flai
      grdn=4.*sigma*(TairK**3.)*extkd*emleaf*
     &    *(exp(-extkd*flai)+exp(-extkd*(flait-flai)))
     &    /rhocp
      return
      end
!     ****************************************************************************
      subroutine agsean_day(Sps,Qabs,Rnstar,grdn,windUx,Tair,Dair,
     &               co2ca,wleaf,raero,theta,a1,Ds0,fwsoil,idoy,hours,
     &               Rconst,cpair,Patm,Trefk,H2OLv0,AirMa,H2OMw,Dheat,
     &               gsw0,alpha,stom_n,
     &               Vcmxx,eJmxx,conKc0,conKo0,Ekc,Eko,o2ci,
     &               Eavm,Edvm,Eajm,Edjm,Entrpy,gam0,gam1,gam2,
     &               Aleaf,Eleaf,Hleaf,Tleaf,gbleaf,gsleaf,co2ci)

!    implicit real (a-z)
      integer kr1,ileaf
      real Aleaf(2),Eleaf(2),Hleaf(2),Tleaf(2),co2ci(2)
      real gbleaf(2), gsleaf(2)
      real Qabs(3,2),Rnstar(2)
!    thermodynamic parameters for air
      TairK=Tair+273.2
      rhocp=cpair*Patm*AirMa/(Rconst*TairK)
      H2OLv=H2oLv0-2.365e3*Tair
      slope=(esat(Tair+0.1)-esat(Tair))/0.1
      psyc=Patm*cpair*AirMa/(H2OLv*H2OMw)
      Cmolar=Patm/(Rconst*TairK)
      weighJ=1.0
!    boundary layer conductance for heat - single sided, forced convection
!    (Monteith 1973, P106 & notes dated 23/12/94)
      if(windUx/wleaf>=0.0)then
          gbHu=0.003*sqrt(windUx/wleaf)    !m/s
      else
          gbHu=0.003 !*sqrt(-windUx/wleaf)
      endif         ! Weng 10/31/2008
!     raero=0.0                        !aerodynamic resistance s/m
      do ileaf=1,2              ! loop over sunlit and shaded leaves
!        first estimate of leaf temperature - assume air temp
         Tleaf(ileaf)=Tair
         Tlk=Tleaf(ileaf)+273.2    !Tleaf to deg K
!        first estimate of deficit at leaf surface - assume Da
         Dleaf=Dair                !Pa
!        first estimate for co2cs
         co2cs=co2ca               !mol/mol
         Qapar = (4.6e-6)*Qabs(1,ileaf)
!    ********************************************************************
         kr1=0                     !iteration counter for LE
!        return point for evaporation iteration
         do               !iteration for leaf temperature
!          single-sided boundary layer conductance - free convection (see notes 23/12/94)
           Gras=1.595e8*ABS(Tleaf(ileaf)-Tair)*(wleaf**3.)     !Grashof
           gbHf=0.5*Dheat*(Gras**0.25)/wleaf
           gbH=gbHu+gbHf                         !m/s
           rbH=1./gbH                            !b/l resistance to heat transfer
           rbw=0.93*rbH                          !b/l resistance to water vapour
!          Y factor for leaf: stom_n = 1.0 for hypostomatous leaf;  stom_n = 2.0 for amphistomatous leaf
           rbH_L=rbH*stom_n/2.                   !final b/l resistance for heat  
           rrdn=1./grdn
           Y=1./(1.+ (rbH_L+raero)/rrdn)
!          boundary layer conductance for CO2 - single side only (mol/m2/s)
           gbc=Cmolar*gbH/1.32            !mol/m2/s
           gsc0=gsw0/1.57                 !convert conductance for H2O to that for CO2
           varQc=0.0
           weighR=1.0
           call photosyn(Sps,CO2Ca,CO2Cs,Dleaf,Tlk,Qapar,Gbc, !Qaparx<-Qapar,Gbcx<-Gsc0
     &         theta,a1,Ds0,fwsoil,varQc,weighR,
     &         gsc0,alpha,Vcmxx,eJmxx,weighJ,
     &         conKc0,conKo0,Ekc,Eko,o2ci,Rconst,Trefk,
     &         Eavm,Edvm,Eajm,Edjm,Entrpy,gam0,gam1,gam2,
     &         Aleafx,Gscx)  !outputs
!          choose smaller of Ac, Aq
           Aleaf(ileaf) = Aleafx      !0.7 Weng 3/22/2006          !mol CO2/m2/s
!          calculate new values for gsc, cs (Lohammer model)
           co2cs = co2ca-Aleaf(ileaf)/gbc
           co2Ci(ileaf) = co2cs-Aleaf(ileaf)/gscx
!          scale variables
!           gsw=gscx*1.56      !gsw in mol/m2/s, oreginal:gsw=gsc0*1.56,Weng20060215
           gsw=gscx*1.56       !gsw in mol/m2/s, oreginal:gsw=gscx*1.56,Weng20090226
           gswv=gsw/Cmolar                           !gsw in m/s
           rswv=1./gswv
!          calculate evap'n using combination equation with current estimate of gsw
           Eleaf(ileaf)=1.0*
     &     (slope*Y*Rnstar(ileaf)+rhocp*Dair/(rbH_L+raero))/    !2* Weng 0215
     &     (slope*Y+psyc*(rswv+rbw+raero)/(rbH_L+raero))

!          calculate sensible heat flux
           Hleaf(ileaf)=Y*(Rnstar(ileaf)-Eleaf(ileaf))
!          calculate new leaf temperature (K)
           Tlk1=273.2+Tair+Hleaf(ileaf)*(rbH/2.+raero)/rhocp
!          calculate Dleaf use LE=(rhocp/psyc)*gsw*Ds
           Dleaf=psyc*Eleaf(ileaf)/(rhocp*gswv)
           gbleaf(ileaf)=gbc*1.32*1.075
           gsleaf(ileaf)=gsw
!          compare current and previous leaf temperatures
           if(abs(Tlk1-Tlk).le.0.1) exit ! original is 0.05 C Weng 10/31/2008
!          update leaf temperature  ! leaf temperature calculation has many problems! Weng 10/31/2008
           Tlk=Tlk1
           Tleaf(ileaf)=Tlk1-273.2
           kr1=kr1+1
           if(kr1 > 500)then
               Tlk=TairK
               exit
           endif
           if(Tlk < 200.)then
                Tlk=TairK
                exit 
           endif                     ! Weng 10/31/2008
!        goto 100                          !solution not found yet
         enddo
! 10  continue
      enddo
      return
      end
!     ****************************************************************************
      subroutine agsean_ngt(Sps,Qabs,Rnstar,grdn,windUx,Tair,Dair,co2ca,
     &               wleaf,raero,theta,a1,Ds0,fwsoil,idoy,hours,
     &               Rconst,cpair,Patm,Trefk,H2OLv0,AirMa,H2OMw,Dheat,
     &               gsw0,alpha,stom_n,
     &               Vcmxx,eJmxx,conKc0,conKo0,Ekc,Eko,o2ci,
     &               Eavm,Edvm,Eajm,Edjm,Entrpy,gam0,gam1,gam2,
     &               Aleaf,Eleaf,Hleaf,Tleaf,gbleaf,gsleaf,co2ci)
!    implicit real (a-z)
      integer kr1,ileaf
      real Aleaf(2),Eleaf(2),Hleaf(2),Tleaf(2),co2ci(2)
      real gbleaf(2), gsleaf(2)
      real Qabs(3,2),Rnstar(2)
!    thermodynamic parameters for air
      TairK=Tair+273.2
      rhocp=cpair*Patm*AirMa/(Rconst*TairK)
      H2OLv=H2oLv0-2.365e3*Tair
      slope=(esat(Tair+0.1)-esat(Tair))/0.1
      psyc=Patm*cpair*AirMa/(H2OLv*H2OMw)
      Cmolar=Patm/(Rconst*TairK)
      weighJ=1.0

!     boundary layer conductance for heat - single sided, forced convection
!     (Monteith 1973, P106 & notes dated 23/12/94)
      gbHu=0.003*sqrt(windUx/wleaf)    !m/s
!     raero=0.0                        !aerodynamic resistance s/m

      do ileaf=1,2                  ! loop over sunlit and shaded leaves
!        first estimate of leaf temperature - assume air temp
         Tleaf(ileaf)=Tair
         Tlk=Tleaf(ileaf)+273.2    !Tleaf to deg K
!        first estimate of deficit at leaf surface - assume Da
         Dleaf=Dair                !Pa
!        first estimate for co2cs
         co2cs=co2ca               !mol/mol
         Qapar = (4.6e-6)*Qabs(1,ileaf)
!        ********************************************************************
         kr1=0                     !iteration counter for LE
         do
!100        continue !    return point for evaporation iteration
!           single-sided boundary layer conductance - free convection (see notes 23/12/94)
            Gras=1.595e8*abs(Tleaf(ileaf)-Tair)*(wleaf**3)     !Grashof
            gbHf=0.5*Dheat*(Gras**0.25)/wleaf
            gbH=gbHu+gbHf                         !m/s
            rbH=1./gbH                            !b/l resistance to heat transfer
            rbw=0.93*rbH                          !b/l resistance to water vapour
!           Y factor for leaf: stom_n = 1.0 for hypostomatous leaf;  stom_n = 2.0 for amphistomatous leaf
            rbH_L=rbH*stom_n/2.                   !final b/l resistance for heat  
            rrdn=1./grdn
            Y=1./(1.+ (rbH_L+raero)/rrdn)
!           boundary layer conductance for CO2 - single side only (mol/m2/s)
            gbc=Cmolar*gbH/1.32            !mol/m2/s
            gsc0=gsw0/1.57                        !convert conductance for H2O to that for CO2
            varQc=0.0                  
            weighR=1.0
!           respiration      
            Aleafx=-0.0089*Vcmxx*exp(0.069*(Tlk-293.2))
            gsc=gsc0
!           choose smaller of Ac, Aq
            Aleaf(ileaf) = Aleafx                     !mol CO2/m2/s
!           calculate new values for gsc, cs (Lohammer model)
            co2cs = co2ca-Aleaf(ileaf)/gbc
            co2Ci(ileaf) = co2cs-Aleaf(ileaf)/gsc
!           scale variables
            gsw=gsc*1.56                              !gsw in mol/m2/s
            gswv=gsw/Cmolar                           !gsw in m/s
            rswv=1./gswv
!           calculate evap'n using combination equation with current estimate of gsw
            Eleaf(ileaf)=
     &      (slope*Y*Rnstar(ileaf)+rhocp*Dair/(rbH_L+raero))/
     &      (slope*Y+psyc*(rswv+rbw+raero)/(rbH_L+raero))
!           calculate sensible heat flux
            Hleaf(ileaf)=Y*(Rnstar(ileaf)-Eleaf(ileaf))
!           calculate new leaf temperature (K)
            Tlk1=273.2+Tair+Hleaf(ileaf)*(rbH/2.+raero)/rhocp
!           calculate Dleaf use LE=(rhocp/psyc)*gsw*Ds
            Dleaf=psyc*Eleaf(ileaf)/(rhocp*gswv)
            gbleaf(ileaf)=gbc*1.32*1.075
            gsleaf(ileaf)=gsw

!          compare current and previous leaf temperatures
            if(abs(Tlk1-Tlk).le.0.1)exit
            if(kr1.gt.500)exit
!           update leaf temperature
            Tlk=Tlk1 
            Tleaf(ileaf)=Tlk1-273.2
            kr1=kr1+1
         enddo                          !solution not found yet
10    continue
      enddo
      return
      end
!     ****************************************************************************
      subroutine ciandA(Gma,Bta,g0,X,Rd,co2Cs,gammas,ciquad,Aquad)
!     calculate coefficients for quadratic equation for ci
      b2 = g0+X*(Gma-Rd)
      b1 = (1.-co2cs*X)*(Gma-Rd)+g0*(Bta-co2cs)-X*(Gma*gammas+Bta*Rd)
      b0 = -(1.-co2cs*X)*(Gma*gammas+Bta*Rd)-g0*Bta*co2cs

      bx=b1*b1-4.*b2*b0
      if(bx.gt.0.0) then 
!       calculate larger root of quadratic
        ciquad = (-b1+sqrt(bx))/(2.*b2)
      endif

      IF(ciquad.lt.0.or.bx.lt.0.) THEN
        Aquad = 0.0
        ciquad = 0.7 * co2Cs
      ELSE
        Aquad = Gma*(ciquad-gammas)/(ciquad+Bta)
      ENDIF
      return
      end

!****************************************************************************
      subroutine goud1(FLAIT,coszen,radabv,fbeam,
     &                  Tair,eairP,emair,emsoil,emleaf,sigma,
     &                  tauL,rhoL,rhoS,xfang,extkb,extkd,
     &                  reffbm,reffdf,extkbm,extkdm,Qcan)
!    use the radiation scheme developed by
!    Goudriaan (1977, Goudriaan and van Larr 1995)
!=================================================================
!    Variable      unit      defintion
!    FLAIT         m2/m2     canopy leaf area index       
!    coszen                  cosine of the zenith angle of the sun
!    radabv(nW)    W/m2      incoming radiation above the canopy
!    fbeam                   beam fraction
!    fdiff                   diffuse fraction
!    funG(=0.5)              Ross's G function
!    extkb                   extinction coefficient for beam PAR
!    extkd                   extinction coefficient for diffuse PAR
!    albedo                  single scattering albedo
!    scatB                   upscattering parameter for beam
!    scatD                   upscattering parameter for diffuse
! ==================================================================
!    all intermediate variables in the calculation correspond
!    to the variables in the Appendix of of Seller (1985) with
!    a prefix of "x".
      integer nW
      real radabv(3)
      real rhocbm(3),rhocdf(3)
      real reffbm(3),reffdf(3),extkbm(3),extkdm(3)
      real tauL(3),rhoL(3),rhoS(3),scatL(3)
      real Qcan(3,2),Qcan0(3)
!
!     for PAR: using Goudriann approximation to account for scattering
      fdiff=1.0-fbeam
      xu=coszen
      xphi1 = 0.5 -0.633*xfang - 0.33*xfang*xfang
      xphi2 = 0.877 * (1.0 - 2.0*xphi1)
      funG = xphi1 + xphi2*xu
      extkb=funG/xu
                       
!     Effective extinction coefficient for diffuse radiation Goudriaan & van Laar Eq 6.6)
      pi180=3.1416/180.
      cozen15=cos(pi180*15)
      cozen45=cos(pi180*45)
      cozen75=cos(pi180*75)
      xK15=xphi1/cozen15+xphi2
      xK45=xphi1/cozen45+xphi2
      xK75=xphi1/cozen75+xphi2
      transd=0.308*exp(-xK15*FLAIT)+0.514*exp(-xK45*FLAIT)+
     &       0.178*exp(-xK75*FLAIT)
      extkd=(-1./FLAIT)*alog(transd)

!     canopy reflection coefficients (Array indices: 1=VIS,  2=NIR
      do nw=1,2                                                         !nw:1=VIS, 2=NIR
         scatL(nw)=tauL(nw)+rhoL(nw)                                    !scattering coeff
         if((1.-scatL(nw))<0.0) scatL(nw)=0.9999                        !Weng 10/31/2008
         extkbm(nw)=extkb*sqrt(1.-scatL(nw))                            !modified k beam scattered (6.20)
         extkdm(nw)=extkd*sqrt(1.-scatL(nw))                            !modified k diffuse (6.20)
         rhoch=(1.-sqrt(1.-scatL(nw)))/(1.+sqrt(1.-scatL(nw)))          !canopy reflection black horizontal leaves (6.19)
         rhoc15=2.*xK15*rhoch/(xK15+extkd)                              !canopy reflection (6.21) diffuse
         rhoc45=2.*xK45*rhoch/(xK45+extkd)
         rhoc75=2.*xK75*rhoch/(xK75+extkd)   
       
         rhocbm(nw)=2.*extkb/(extkb+extkd)*rhoch                        !canopy reflection (6.21) beam 
         rhocdf(nw)=0.308*rhoc15+0.514*rhoc45+0.178*rhoc75
         reffbm(nw)=rhocbm(nw)+(rhoS(nw)-rhocbm(nw))                    !effective canopy-soil reflection coeff - beam (6.27)
     &             *exp(-2.*extkbm(nw)*FLAIT)                              
         reffdf(nw)=rhocdf(nw)+(rhoS(nw)-rhocdf(nw))                    !effective canopy-soil reflection coeff - diffuse (6.27)
     &             *exp(-2.*extkdm(nw)*FLAIT)  

!        by the shaded leaves
         abshdn=fdiff*(1.0-reffdf(nw))*extkdm(nw)                       !absorbed NIR by shaded
     &      *(funE(extkdm(nw),FLAIT)-funE((extkb+extkdm(nw)),FLAIT))
     &      +fbeam*(1.0-reffbm(nw))*extkbm(nw)
!    &      *(funE(extkbm(nw),FLAIT)-funE((extkb+extkdm(nw)),FLAIT))    ! error found by De Pury
     &      *(funE(extkbm(nw),FLAIT)-funE((extkb+extkbm(nw)),FLAIT))
     &      -fbeam*(1.0-scatL(nw))*extkb
     &      *(funE(extkb,FLAIT)-funE(2.0*extkb,FLAIT))
!        by the sunlit leaves
         absltn=fdiff*(1.0-reffdf(nw))*extkdm(nw)                             !absorbed NIR by sunlit
     &      *funE((extkb+extkdm(nw)),FLAIT)                         
     &      +fbeam*(1.0-reffbm(nw))*extkbm(nw)
!    &      *funE((extkb+extkdm(nw)),FLAIT)                         ! error found by De Pury
     &      *funE((extkb+extkbm(nw)),FLAIT)
     &      +fbeam*(1.0-scatL(nw))*extkb
     &      *(funE(extkb,FLAIT)-funE(2.0*extkb,FLAIT))

!        scale to real flux 
!        sunlit    
          Qcan(nw,1)=absltn*radabv(nw)
!        shaded
          Qcan(nw,2)=abshdn*radabv(nw)
      enddo
!     
!    calculate the absorbed (iso)thermal radiation
      TairK=Tair+273.2
      
!     apparent atmospheric emissivity for clear skies (Brutsaert, 1975)
      emsky=0.642*(eairP/Tairk)**(1./7)      !note eair in Pa

!     apparent emissivity from clouds (Kimball et al 1982)
      ep8z=0.24+2.98e-12*eairP*eairP*exp(3000.0/TairK)
      tau8=amin1(1.0,1-ep8z*(1.4-0.4*ep8z))                !ensure tau8<1
      emcloud=0.36*tau8*(1.-fbeam)*(1-10./TairK)**4        !10 from Tcloud = Tair-10 

!     apparent emissivity from sky plus clouds      
!     emair=emsky+emcloud
! 20/06/96
      emair=emsky
      if(emair.gt.1.0) emair=1.0                             

      Bn0=sigma*(TairK**4)
      QLW1=-extkd*emleaf*(1.0-emair)*funE((extkd+extkb),FLAIT)
     &     -extkd*(1.0-emsoil)*(emleaf-emair)*exp(-2.0*extkd*FLAIT)
     &     *funE((extkb-extkd),FLAIT)
      QLW2=-extkd*emleaf*(1.0-emair)*funE(extkd,FLAIT)
     &     -extkd*(1.0-emsoil)*(emleaf-emair)
     &     *(exp(-extkd*FLAIT)-exp(-2.0*extkd*FLAIT))/extkd
     &     -QLW1
      Qcan(3,1)=QLW1*Bn0
      Qcan(3,2)=QLW2*Bn0
      return
      end

!****************************************************************************
      subroutine photosyn(Sps,CO2Ca,CO2Csx,Dleafx,Tlkx,Qaparx,Gbcx,
     &         theta,a1,Ds0,fwsoil,varQc,weighR,
     &         g0,alpha,
     &         Vcmx1,eJmx1,weighJ,conKc0,conKo0,Ekc,Eko,o2ci,
     &         Rconst,Trefk,Eavm,Edvm,Eajm,Edjm,Entrpy,gam0,gam1,gam2,
     &         Aleafx,Gscx)

!     calculate Vcmax, Jmax at leaf temp (Eq 9, Harley et al 1992)
!     turned on by Weng, 2012-03-13
!     VcmxT = Vjmax(Tlkx,Trefk,Vcmx1,Eavm,Edvm,Rconst,Entrpy)
!     eJmxT = Vjmax(Tlkx,Trefk,eJmx1,Eajm,Edjm,Rconst,Entrpy)
      CO2Csx=AMAX1(CO2Csx,0.6*CO2Ca)
!    check if it is dark - if so calculate respiration and g0 to assign conductance 
      if(Qaparx.le.0.) then                            !night, umol quanta/m2/s
        Aleafx=-0.0089*Vcmx1*exp(0.069*(Tlkx-293.2))   ! original: 0.0089 Weng 3/22/2006
        Gscx=g0
      endif
!     calculate  Vcmax, Jmax at leaf temp using Reed et al (1976) function J appl Ecol 13:925
      TminV=12.  ! original -5.
      TmaxV=45.
      ToptV=30.
      
      TminJ=TminV
      TmaxJ=TmaxV
      ToptJ=ToptV 
      
      Tlf=Tlkx-273.2
      VcmxT=VJtemp(Tlf,TminV,TmaxV,ToptV,Vcmx1)
      eJmxT=VJtemp(Tlf,TminJ,TmaxJ,ToptJ,eJmx1)      
!     calculate J, the asymptote for RuBP regeneration rate at given Q
      eJ = weighJ*fJQres(eJmxT,alpha,Qaparx,theta)
!     calculate Kc, Ko, Rd gamma*  & gamma at leaf temp
      conKcT = EnzK(Tlkx,Trefk,conKc0,Rconst,Ekc)
      conKoT = EnzK(Tlkx,Trefk,conKo0,Rconst,Eko)
!     following de Pury 1994, eq 7, make light respiration a fixed proportion of
!     Vcmax
      Rd = 0.0089*VcmxT*weighR                              !de Pury 1994, Eq7
      Tdiff=Tlkx-Trefk
      gammas = gam0*(1.+gam1*Tdiff+gam2*Tdiff*Tdiff)       !gamma*
!     gamma = (gammas+conKcT*(1.+O2ci/conKoT)*Rd/VcmxT)/(1.-Rd/VcmxT)
      gamma = 0.0
!     ***********************************************************************
!     Analytical solution for ci. This is the ci which satisfies supply and demand
!     functions simultaneously
!     calculate X using Lohammer model, and scale for soil moisture
      a1= 1./(1.-0.7)
      X = a1*fwsoil/((co2csx - gamma)*(1.0 + Dleafx/Ds0))
!     calculate solution for ci when Rubisco activity limits A
      Gma = VcmxT  
      Bta = conKcT*(1.0+ o2ci/conKoT)
      call ciandA(Gma,Bta,g0,X,Rd,co2Csx,gammas,co2ci2,Acx)
!     calculate +ve root for ci when RuBP regeneration limits A
      Gma = eJ/4.
      Bta = 2.*gammas
!    calculate coefficients for quadratic equation for ci
      call ciandA(Gma,Bta,g0,X,Rd,co2Csx,gammas,co2ci4,Aqx)
!     choose smaller of Ac, Aq
      sps=AMAX1(0.001,sps)                  !Weng, 3/30/2006
      Aleafx = (amin1(Acx,Aqx) - Rd) !*sps     ! Weng 4/4/2006
!      if(Aleafx.lt.0.0) Aleafx=0.0    ! by Weng 3/21/2006
!    calculate new values for gsc, cs (Lohammer model)
      CO2csx = co2ca-Aleafx/Gbcx
      Gscx=g0 + X*Aleafx  ! revised by Weng
      return
      end
!***********************************************************************
      function funeJ(alpha,eJmxT,Qaparx)
      funeJ=alpha*Qaparx*eJmxT/(alpha*Qaparx+2.1*eJmxT)
      return
      end
!****************************************************************************
      real function esat(T)
!     returns saturation vapour pressure in Pa
      esat=610.78*exp(17.27*T/(T+237.3))
      return
      end

!****************************************************************************
      real function evapor(Td,Tw,Patm)
!* returns vapour pressure in Pa from wet & dry bulb temperatures
      gamma = (64.6 + 0.0625*Td)/1.e5
      evapor = esat(Tw)- gamma*(Td-Tw)*Patm
      return
      end

!****************************************************************************
      real function Vjmax(Tk,Trefk,Vjmax0,Eactiv,Edeact,Rconst,Entrop)
      anum = Vjmax0*EXP((Eactiv/(Rconst*Trefk))*(1.-Trefk/Tk))
      aden = 1. + EXP((Entrop*Tk-Edeact)/(Rconst*Tk))
      Vjmax = anum/aden
      return
      end
!****************************************************************************
      real function funE(extkbd,FLAIT)
      funE=(1.0-exp(-extkbd*FLAIT))/extkbd
      return
      end

!     ****************************************************************************
!     Reed et al (1976, J appl Ecol 13:925) equation for temperature response
!     used for Vcmax and Jmax
      real function VJtemp(Tlf,TminVJ,TmaxVJ,ToptVJ,VJmax0)
      if(Tlf.lt.TminVJ) Tlf=TminVJ   !constrain leaf temperatures between min and max
      if(Tlf.gt.TmaxVJ) Tlf=TmaxVJ
      pwr=(TmaxVJ-ToptVJ)/(ToptVj-TminVj)
      VJtemp=VJmax0*((Tlf-TminVJ)/(ToptVJ-TminVJ))*
     &       ((TmaxVJ-Tlf)/(TmaxVJ-ToptVJ))**pwr 
      return
      end

!     ****************************************************************************
      real function fJQres(eJmx,alpha,Q,theta)
      AX = theta                                 !a term in J fn
      BX = alpha*Q+eJmx                          !b term in J fn
      CX = alpha*Q*eJmx                          !c term in J fn
      if((BX*BX-4.*AX*CX)>=0.0)then
          fJQres = (BX-SQRT(BX*BX-4.*AX*CX))/(2*AX)
      else
          fJQres = (BX)/(2*AX)                   !Weng 10/31/2008
      endif

      return
      end

!     *************************************************************************
      real function EnzK(Tk,Trefk,EnzK0,Rconst,Eactiv)

      temp1=(Eactiv/(Rconst* Trefk))*(1.-Trefk/Tk)
!      if (temp1<50.)then
      EnzK = EnzK0*EXP((Eactiv/(Rconst* Trefk))*(1.-Trefk/Tk))
!      else
!      EnzK = EnzK0*EXP(50.)                                          ! Weng 10/31/2008
!      endif

      return
      end

!     *************************************************************************
      real function sinbet(doy,lat,pi,timeh)
      real lat
!     sin(bet), bet = elevation angle of sun
!     calculations according to Goudriaan & van Laar 1994 P30
      rad = pi/180.
!     sine and cosine of latitude
      sinlat = sin(rad*lat)
      coslat = cos(rad*lat)
!     sine of maximum declination
      sindec=-sin(23.45*rad)*cos(2.0*pi*(doy+10.0)/365.0)
      cosdec=sqrt(1.-sindec*sindec)
!     terms A & B in Eq 3.3
      A = sinlat*sindec
      B = coslat*cosdec
      sinbet = A+B*cos(pi*(timeh-12.)/12.)
      return
      end

!     *************************************************************************
      subroutine yrday(doy,hour,lat,radsol,fbeam)
      real lat
      pi=3.14159256
      pidiv=pi/180.0
      slatx=lat*pidiv
      sindec=-sin(23.4*pidiv)*cos(2.0*pi*(doy+10.0)/365.0)
      cosdec=sqrt(1.-sindec*sindec)
      a=sin(slatx)*sindec
      b=cos(slatx)*cosdec
      sinbet=a+b*cos(2*pi*(hour-12.)/24.)
      solext=1370.0*(1.0+0.033*cos(2.0*pi*(doy-10.)/365.0))*sinbet
      
      tmprat=0.5 * radsol/solext  ! since radsol is PAR

      tmpR=0.847-1.61*sinbet+1.04*sinbet*sinbet
      tmpK=(1.47-tmpR)/1.66
      if(tmprat.le.0.22) fdiff=1.0
      if(tmprat.gt.0.22.and.tmprat.le.0.35) then
        fdiff=1.0-6.4*(tmprat-0.22)*(tmprat-0.22)
      endif
      if(tmprat.gt.0.35.and.tmprat.le.tmpK) then
        fdiff=1.47-1.66*tmprat
      endif
      if(tmprat.ge.tmpK) then
        fdiff=tmpR
      endif
      fbeam=1.0-fdiff
      if(fbeam.lt.0.0) fbeam=0.0
      return
      end
!     *************************************************************************
!     =========================================================================
!     subroutine for soil moisture. I didn't use this subroutine since the water
!     budget is not close.
      subroutine soilnew(wsmax,wsmin,rdepth,FRLEN,!constants specific to soil/plant
     &            rain,tair,transp,wcl,tsoil,RH,thksl,LAI, !inputs
     &            evap,runoff,wscontent,fwsoil,topfws,     !outputs
     &            omega,omega_S,WaterR,WaterS,SatFracL)    !outputs

      implicit none
!     soil traits
      real wsmax,wsmin,wsmaxL(10),wsminL(10) !from input percent x%
      real FLDCAP,WILTPT,FLDCAPL(10),WILTPTL(10) ! ie. 0.xx
!     plant traits
      real LAI,rdepth
      integer nfr
!     climate conditions
      real precp,rain ! mm/hour
      real tair,TSOIL,ts         ! updated every hour
!     output from canopy model
      real evap,transp,evaptr,TEVAP,AEVAP
!     output variables
      real wscontent,fwsoil,topfws,omega,topomega,omega_S
      real fw(10),ome(10),W_signal
      real WaterR,WaterS(10),SatFracL(10)
!     omega: (wscontent-wiltpt)/(fldcap-wiltpt)
      real RAWCL(10) ! omega of every layers
      real thksl(10),depth(10),wsc(10),WUPL(10),EVAPL(10),SRDT(10)
      real plantup(10)
      real Tsrdt
      real frlen(10) !fraction of root length in every layer
      real wcl(10) !volum ratio
      real fwcln(10) !  fraction of water in layers, like field capacity
      real wtdeficit(10),DWCL(10),Tr_ratio(10)
      real Twater,Twater1,Twater2,Tthk,dWaterS,netflux
      real wtneed,wtadd,twtadd,infilt,runoff,roff_layer,tr_allo

      real Dair,RH,Rsoil,Rd,density,sp_heat,psychro,la,P
      real esat
      real exchangeL,supply,demand,omegaL(10)
      integer i,j,k


      WILTPT =wsmin/100.0
      FLDCAP =wsmax/100.0
      WILTPTL=wsmin/100.0
      FLDCAPL=wsmax/100.0

      twater=0.0
      twater1=0.0
      twater2=0.0
      precp=rain
      do i=1,10
          wtdeficit(i)=0.0
          dwcl(i)=0.0
          evapl(i)=0.0
          WUPL(i)=0.0
          SRDT(i)=0.0
          DEPTH(i)=0.0
      enddo

!C ** Determine which layer has been reached by the root system. 
!    Layer volume (cm3)
      DEPTH(1)=10.0
      DO i=2,10
          DEPTH(i)=DEPTH(i-1)+THKSL(i)
      enddo
      do i=1,10
        IF(rdepth.GT.DEPTH(i)) nfr=i+1
      enddo
      IF (nfr.GT.10) nfr=10
      do i=1,10
          if(FLDCAPL(i).gt.wcl(i))wtdeficit(i)=FLDCAPL(i)-wcl(i)
      enddo

! *** water infiltration through layers
      infilt=precp  !mm/hour
!     Loop over all soil layers.
      TWTADD=0
      roff_layer=0.0
      do i=1,10 

         IF(infilt.GT.0.0)THEN
!           Add water to this layer, pass extra water to the next.
            WTADD=AMIN1(INFILT,wtdeficit(i)*thksl(i)*10.0) ! from cm to mm
!           change water content of this layer
            WCL(i)=(WCL(i)*(thksl(i)*10.0)+WTADD)/(thksl(i)*10.0)
            FWCLN(I)=WCL(I)       !  /VOLUM(I)! update fwcln of this layer
            TWTADD=TWTADD+WTADD      !calculating total added water to soil layers (mm)
            INFILT=INFILT-WTADD !update infilt
         END IF

!      produce runoff during infiltration
            if(infilt.GT.0.0)THEN
               roff_layer=roff_layer + INFILT*0.05*(i-1)
               INFILT=INFILT -  INFILT*0.05*(i-1)
            endif
      enddo

      if(precp.gt.0.0.and.wcl(1).gt.wcl(2))then
            supply=(wcl(1)-wcl(2))/3.0
            wcl(1)=wcl(1)-2.0*supply
            wcl(2)=wcl(2)+supply
      endif

!      runoff
      runoff=INFILT + roff_layer   !precp-TWTADD + roff_layer   !weng 10072006

!      water redistribution among soil layers
      do i=1,10
            wsc(i)=Amax1(0.00,(wcl(i)-wiltpt)*THKSL(i)*10.0)
            omegaL(i)=Amax1(0.001,(wcl(i)-WILTPT)/(FLDCAPL(i)-WILTPT))
      enddo
      supply=0.0
      demand=0.0
      do i=1,9
            if(omegaL(i).gt.0.3)then
                  supply=wsc(i)/360.0*omegaL(i)
                  demand=(FLDCAPL(i)-wcl(i+1))*THKSL(i+1)*10.0/360.0
     &                          *(1.0-omegaL(i+1))
                  exchangeL=AMIN1(supply,demand)
                  wsc(i)=wsc(i)- exchangeL
                  wsc(i+1)=wsc(i+1)+ exchangeL
                  wcl(i)=wsc(i)/(THKSL(i)*10.0)+wiltpt
                  wcl(i+1)=wsc(i+1)/(THKSL(i+1)*10.0)+wiltpt
            endif
      enddo

!     calculate evap demand by eq.24 of Seller et al. 1996 (demand)
!
!      if(wcl(1).LT.wiltpt)then
!           evap=0.0
!      else
!            Rsoil=40.1*exp(1.0/wcl(1))
!            Rsoil=exp(8.206-4.255*wcl(1))
!	write(*,*)wcl(1)
!            Rd   =20.5*exp(LAI/1.5)!LAI is added by Weng
!            P=101325.0  !Pa, atmospheric pressure
!            density=1.204 !kg/m3
!            la=(2.501-0.00236*Tair)*1000000.0 !J/kg
!            sp_heat=1012.0  !J/kg/K
!            psychro=1628.6*P/la
!
!            evap=1.0*esat(tair)*(1.0-RH/100.0)/
!     &         (Rsoil+Rd)*density*sp_heat/psychro/la*3600.0
!            evap=1.0*Dair/
!     &         (Rsoil+Rd)*density*sp_heat/psychro/la*3600.0
!      endif

!  *** Soil evaporation; SRDT(I) for contribution of each layer. 
!     Units here are g H2O m-2 layer-1 h-1.
      Twater=0
      do i=1,10
            wsc(i)=(wcl(i)-wiltpt)*THKSL(I)*10.0
            Twater=Twater+wsc(i)  ! total water in soils,mm
      enddo

      Tsrdt=0.0
      DO i=1,10
!            Fraction of SEVAP supplied by each soil layer
                  SRDT(I)=EXP(-4.73*(DEPTH(I)-THKSL(I)/2.0)/100.0)/1.987
!                  SRDT(I)=AMAX1(0.0,SRDT(I)*(wcl(i)-wiltpt)) !*THKSL(I))
                  Tsrdt=Tsrdt+SRDT(i)/(i*i)  ! to normalize SRDT(i)
      enddo

      do i=1,10
            SRDT(i)=SRDT(i)/Tsrdt
      enddo

      do i=1,10
         EVAPL(I)=Amax1(AMIN1(evap*SRDT(i),wsc(i)),0.0)  !mm
         DWCL(I)=EVAPL(I)/(THKSL(I)*10.0) !ratio
      enddo

!      update water content of every layer
      do i=1,10
            wcl(i)=wcl(i)-DWCL(i)
      enddo
!      the actual evapration
      evap=0.0      
      do i=1,10
         evap=evap+EVAPL(I)
      enddo
!     WATER UPTAKE by plant roots,Weng, 2.13.2006, a passive proccess
      Twater=0
      do i=1,nfr
            wsc(i)=(wcl(i)-wiltpt)*THKSL(I)*10.0
            Twater=Twater+AMAX1(wsc(i),0.0) ! total water in roots reached soil,mm
      enddo
      if(transp.gt.Twater/2.0)transp=Twater/2.0                  
      tr_allo=0.0
      do i=1,nfr
            tr_ratio(i)=FRLEN(i) !*(wcl(i)-wiltpt)) !*THKSL(I))
            tr_allo=tr_allo+tr_ratio(i)
      enddo

      do i=1,nfr
            plantup(i)=AMIN1(transp* tr_ratio(i)/tr_allo, wsc(i)) !mm            
            wupl(i)=plantup(i)/(thksl(i)*10.0)
          wcl(i)=wcl(i)-wupl(i)
      end do

      transp=0.0
      do i=1,nfr
            transp=transp+plantup(i)
      enddo

!     output (fwsoil,topfws,omega) which would be used by canopy model
      Twater=0
      Tthk=0
      do i=1,nfr
            Twater=Twater+wcl(i)*THKSL(I)*10.0 ! total water in soils,mm
            Tthk=Tthk+thksl(i)*10.0 !total thicknes of soil layers, mm
      enddo
      wscontent=Twater/Tthk
      if(wscontent.lt.WILTPT) wscontent=WILTPT+0.00001
      omega_S=(wscontent-WILTPT)/(FLDCAP-WILTPT)
      fwsoil=amin1(1.0,3.333*omega)
      topfws=amin1(1.0,(wcl(1)-WILTPT)/((FLDCAP-WILTPT)))
      if(fwsoil.lt.0.0) fwsoil=0.000001
      if(topfws.lt.0.0) topfws=0.000001
      if(omega.lt.0.0) omega=0.0000001
      Twater=Twater-WILTPT*Tthk
      WaterR=Twater+WILTPT*Tthk
      
!      a new approach for calculating fwsoil
      do i=1,10 !nfr
            ome(i)=(wcl(i)-WILTPT)/(FLDCAP-WILTPT)
          WaterS(i)=wcl(i)*THKSL(I)*10.0
            ome(i)=AMIN1(1.0,AMAX1(0.0,ome(i)))
            SatFracL(i)=ome(i)
            fw(i)=amin1(1.0,3.333*ome(i))
      enddo
      fwsoil=0.0
      omega=0.0
      do i=1,nfr
            fwsoil=fwsoil+fw(i)*frlen(i)
            omega=omega+ome(i)*frlen(i)
      enddo

      return
      end
!=================================================================