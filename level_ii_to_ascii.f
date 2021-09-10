      program level_ii_to_ascii 
c********0*********0*********0*********0*********0*********0*********
c
c This program derives a composite reflectivity product from a decompressed
c NEXRAD level II data file and outputs the result to a
c formatted text file.
c
c Author: Paul Harasti (NRL)
c (initial I/O techniques borrowed from Allen Zhao (NRL))
c
c June 2016
c
c********0*********0*********0*********0*********0*********0*********
c
      implicit none
c
c********0*********0*********0*********0*********0*********0*********
c
      include 'remap.inc'
c
      integer i,j,k
      integer l
      integer l2
      integer*4 ii,jj
      integer i1
      integer i2
      integer l4
      integer ir
      integer ip
      integer it
      integer*4 irdst1
      integer ierr
      integer packet_num
      integer data_packet_num
      integer*2 num_of_elv
      integer year
      integer month
      integer day
      integer hour
      integer minute
      integer second
      integer nid

      integer*2 vol_cov_pattern

      integer lprod_data_path
      integer lprod_data_fname
      integer lout_data_path
      integer lout_data_fname
      integer ptr_z,ptr_v,ptr_w

      integer*1  channel_id
      integer*1  message_type
      integer*1  ibyt1(131086)

      integer*2  itime(2)

      integer*2  ibyt2(65543)
      integer*2  message_size
      integer*2  obs_date
      integer*2  radial_num,ray_num
      integer*2  num_of_message
      integer*2  message_num
      integer*2  elv_num

      integer*2  num_of_ref_gates
      integer*2  num_of_vel_gates

      integer*2  num_of_rhohv_gates
      integer*2  num_of_phidp_gates
      integer*2  num_of_zdr_gates

      integer*2  sec_num_in_cut
      integer*2  ref_pointer
      integer*2  vel_pointer
      integer*2  spw_pointer
      integer*2  sys_calib(2)

c Counters to skip past super resolution azimuth and Z range
      integer azcount, zcount, zinc

      integer*4  data_date
      integer*4  data_time
      integer*4  fgetc
      integer*4  nblk

      character*1 moment_char1
      character*1 moment_char2
      character*1 moment_char3
      character*1 moment_char4

      character*4 moment_char

      integer*1 elev_num
      integer*1 rad_status
      integer*1 tilt_sector_num 

      integer*1 ref_control_flag
      integer*1 vel_control_flag

      integer*1 rhohv_control_flag
      integer*1 phidp_control_flag
      integer*1 zdr_control_flag

      integer*1 ibyt1_tmp

c Code to hand level II version formats
      integer*1 lvl2_ver_num
      character*1 ascii_lvl2_ver_num
      character*4 lvl2header
      integer count_cpf_factor

      integer*2 iazimth_angle(2)
      integer*2 ielv_angle(2)
      integer*2 iradelv
      integer*2 count
      integer*2 data_block_count
      integer*2 total_message_size
      integer*2 num_gates

      integer*2 spw_gate1_range
      integer*2 spw_gate_size
      integer*2 num_of_spw_gates

      integer*2 iunambiguous_range
      integer*2 inyquist_vel
      integer*2 iatmos_atten_factor

      integer*4 vol_const_pointer
      integer*4 elev_const_pointer
      integer*4 radial_const_pointer
      integer*4 moment_data_pointer

      integer*4 ref_data_pointer
      integer*4 vel_data_pointer
      integer*4 spw_data_pointer

      real*4 azmth_resolution

      real*4 moment_scale
      real*4 moment_offset

      real*4 ref_scale
      real*4 vel_scale
      real*4 spw_scale
      real*4 ref_offset
      real*4 vel_offset
      real*4 spw_offset

      logical ref_data
      logical vel_data
      logical spw_data

      logical bin_data

      real*4 azimth_angle
      real*4 elv_angle
      real*4 unambiguous_range
      real*4 sys_calib_const
      real*4 vel_resolution
      real*4 nyquist_vel
      real*4 atmos_atten_factor

      real ref
      real vel
      real spw

      real*4 radlat
      real*4 radlon
      real*4 radelv

c PH: Needed for QC

c     real hgt_z(nr,np,nt)
c     real range_z(nr,np,nt)
c     real range
c     real*4 max_r_z(nt)

      real deg2rad,ae

      character*1  achar1
      character*1  achar2
      character*1  linux_key
      character*1  byte_swap_key

      character*3  mcode(90)

      character*4  radid
      character*4  id

      character*8  rad_type,site_name,data_src

      character*14 yyyymmddhhmmss
      character*100 prod_data_path
      character*100 prod_data_fname
      character*100 out_data_path
      character*100 out_data_fname
      character*100 infile2
      character*100 outfile
      character*512 byte1

      logical file_exist
      logical linux
      logical diag
c toggle superob
      logical superob
c toggle to save only non-redundant data
      logical ref_save
      logical vel_save

      equivalence (byte1,ibyt1)
      equivalence (byte1,ibyt2)
      equivalence (itime(1),data_time)
      equivalence (sys_calib(1),sys_calib_const)

c Required for byte swapping of new message 31 data  

      integer*4  ibyt4(32772)

      real*4     rbyt4(32772)

      integer*1 dummyi2_byte1(2)
      integer*1 dummyr4_byte1(4)

      integer*2 dummy_ibyt2

      integer*4 dummy_ibyt4

      real*4 dummy_rbyt4

      equivalence (byte1,ibyt4)
      equivalence (byte1,rbyt4)
      equivalence (dummyi2_byte1(1),dummy_ibyt2)
      equivalence (dummyr4_byte1(1),dummy_rbyt4)
      equivalence (iazimth_angle(1), azimth_angle)
      equivalence (ielv_angle(1), elv_angle)


c********0*********0*********0*********0*********0*********0*********
c
c                 *************************
c                 ** Program starts here **
c                 *************************
c
c********0*********0*********0*********0*********0*********0*********
c Output diagnostics
      diag=.false.
c
c Set superob filtering of data. If full resolution is desired,
c set superob=.false. and "include remap_full_res.inc" as explained above
c at lines 65-68.
c
      superob=.true.
c
c********0*********0*********0*********0*********0*********0*********
c Constants
       deg2rad=acos(-1.)/180.
       ae=4.*6371./3.         ! 4/3 earth's mean radius (km)
c********0*********0*********0*********0*********0*********0*********
c     Define input and  output parameters.
c********0*********0*********0*********0*********0*********0*********
c
c     call system('sleep 1000')
      call getenv('RSID',radid)
      call getenv('FNAME',prod_data_fname)
      call getenv('DATA_PATH',prod_data_path)
      call getenv('OUT_DATA_PATH',out_data_path)
      call getenv('LINUX',linux_key)

      call slen(prod_data_path,lprod_data_path)
      call slen(prod_data_fname,lprod_data_fname)


      infile2=prod_data_path(1:lprod_data_path)//
     &        prod_data_fname(1:lprod_data_fname)

c
c********0*********0*********0*********0*********0*********0*********
c     Initialize arrays.
c********0*********0*********0*********0*********0*********0*********
c
      do ip=1,np
      do it=1,nt
         gridnyq(ip,it)=velmis
         do ir=1,nr
            gridref(ir,ip,it)=refmis
         enddo
         do ir=1,nrv
            gridvel(ir,ip,it)=velmis
            gridsw (ir,ip,it)=velmis
      enddo
      enddo
      enddo
c********0*********0*********0*********0*********0*********0*********
c Note: Because of the little Endian problem, byte swapping
c       needs to be considered if run on a linux machine.
c
c       To run on a LINUX system, set linux=.true.
c       Then the decoder does the byte swaping.
c
c********0*********0*********0*********0*********0*********0*********
c
      if(linux_key.eq."t") then
         linux=.true.
      else
         linux=.false.
      endif
c
c********0*********0*********0*********0*********0*********0*********
c    read in the station list and find out
c    the lat, lon, and stn height of the station
c    with the 4-character "radid".
c********0*********0*********0*********0*********0*********0*********
c
c********0*********0*********0*********0*********0*********0*********
c     Define input file name and open the input file if it exists.
c********0*********0*********0*********0*********0*********0*********
c
      inquire(file=infile2,exist=file_exist)
      if(.not.file_exist) then
         go to 9300
      endif
      open (20,file=infile2, access='direct',status='old',recl=1)
c********0*********0*********0*********0*********0*********0*********
c    Read in the first part of the data. This is a short message 
c    header for the whole volumn scan file. This message contains 
c    the year, month,day, hour, minute and seconds of the data
c    Note: This part occurs only once for a whole volumn scan.
c********0*********0*********0*********0*********0*********0*********
      ray_num=0
      do ii=1, 24
         irdst1=fgetc(20, achar1)     
         if (irdst1.eq.0) then
            byte1 (ii:ii)     = achar1
         elseif (irdst1.gt.0) then
            go to 9000
         else
            go to 9100
         endif
      enddo
      
c
c PH: Check to make sure Level II Format version number
c     is one of the ones expected (as of 2013).
c
      lvl2header(1:1)=char(ibyt1(1))
      lvl2header(2:2)=char(ibyt1(2))
      lvl2header(3:3)=char(ibyt1(3))
      lvl2header(4:4)=char(ibyt1(4))
              if(diag) then
      print*,"Level II Header: ", lvl2header
              endif
      if (lvl2header(1:4).ne."AR2V") go to 9600

      lvl2_ver_num=ibyt1(8)
      ascii_lvl2_ver_num=char(lvl2_ver_num)
      print*,"Level II File Version Number = ",
     &  ascii_lvl2_ver_num

      if (superob) then
       count_cpf_factor=1  
      else
      if (ascii_lvl2_ver_num.eq."3".or.
     &  ascii_lvl2_ver_num.eq."6") then
       count_cpf_factor=2
      else
       count_cpf_factor=1
      endif
      endif

      ii=13
      l4=(ii+3)/4
      data_date=ibyt4(l4)
      if(linux) call byte_swap_i4(data_date,1) 
      ii=17
      l4=(ii+3)/4
      data_time=ibyt4(l4)
      if(linux) call byte_swap_i4(data_time,1) 
c
c********0*********0*********0*********0*********0*********0*********
c     Unpack the data_date and data_time using unpkdt_4
c********0*********0*********0*********0*********0*********0*********
      data_time=data_time*0.001
      call unpkdt_4(data_date,data_time
     &           ,yyyymmddhhmmss,ierr)
      call print_date_time(yyyymmddhhmmss,year,month,day,hour
     &                    ,minute,second)

c********0*********0*********0*********0*********0*********0*********
c    Now, go through all packets. Each packet contains a header 
c    message, a digital header,and digital data in one radial.
c********0*********0*********0*********0*********0*********0*********
      packet_num=0
      data_packet_num=0

 100  continue

      packet_num=packet_num+1
      do ii=1, 16 
         irdst1=fgetc(20, achar1)     
         if (irdst1.eq.0) then
           byte1 (ii:ii)     = achar1
         elseif (irdst1.gt.0) then
            go to 9000
         else
            go to 9100
         endif
      enddo
      ii=13
      l2=(ii+1)/2
      message_size=ibyt2(l2)
      if (linux) call byte_swap_i2(message_size,1) 
      ii=15
      channel_id=ibyt1(ii)
      ii=16
      message_type=ibyt1(ii)
      if (message_size.lt.0) then
       jj=16+2*(message_size+65536)
      else
       jj=2432
       if (message_type.eq.31) jj=12+2*message_size
      endif
      total_message_size=jj
              if(diag) then
      print*,"Message Size = ",jj,message_type,message_size
              endif
      do ii=17, jj
         irdst1=fgetc(20, achar1)     
         if (irdst1.eq.0) then
               byte1 (ii:ii)     = achar1
         elseif (irdst1.gt.0) then
            go to 9000
         else
            go to 9100
         endif
      enddo

      if(message_type.eq.31) then

              data_packet_num=data_packet_num+1

              ii=61
              l4=(ii+3)/4
              vol_const_pointer=ibyt4(l4)
              if(linux) call byte_swap_i4(vol_const_pointer,1)
              if(diag) then
              print*,"VOL_CONST_POINTER= ",vol_const_pointer
              endif

              ii=65
              l4=(ii+3)/4
              elev_const_pointer=ibyt4(l4)
              if(linux) call byte_swap_i4(elev_const_pointer,1)
              if(diag) then
              print*,"ELEV_CONST_POINTER= ",elev_const_pointer
              endif

              ii=69
              l4=(ii+3)/4
              radial_const_pointer=ibyt4(l4)
              if(linux) call byte_swap_i4(radial_const_pointer,1)
              if(diag) then
              print*,"RADIAL_CONST_POINTER= ",radial_const_pointer
              endif

              if(diag) then
                 print *,'data_packet_num=',data_packet_num
              endif

              ii=33
              l4=(ii+3)/4
              data_time=ibyt4(l4)
              if(linux) call byte_swap_i4(data_time,1)
              data_time=data_time*0.001

              ii=vol_const_pointer+1+8+28
              l4=(ii+3)/4
              radlat=rbyt4(l4)
              if(linux) call byte_swap_r4(radlat,1)

              ii=vol_const_pointer+1+12+28
              l4=(ii+3)/4
              radlon=rbyt4(l4)
              if(linux) call byte_swap_r4(radlon,1)

              ii=vol_const_pointer+1+16+28
              l2=(ii+1)/2
              iradelv=ibyt2(l2)
              if(linux) call byte_swap_i2(iradelv,1)
              radelv=real(iradelv)
           
              ii=(vol_const_pointer+1+18+28)
              l2=(ii+1)/2
              iradelv=ibyt2(l2)
              if(linux) call byte_swap_i2(iradelv,1)
              radelv=radelv+real(iradelv)

              ii=(vol_const_pointer+1+40+28)
              l2=(ii+1)/2
              vol_cov_pattern=ibyt2(l2)
              if(linux) call byte_swap_i2(vol_cov_pattern,1)
c
c Set split scan end tilt per VCP
c 
c      if (vol_cov_pattern.ne.12.and.
c    &   vol_cov_pattern.ne.212.and.vol_cov_pattern.ne.121) then
c             split_scan_last_tilt=4
c      else
c             split_scan_last_tilt=6
c      endif

              ii=vol_const_pointer+1+20+28
              l4=(ii+3)/4
              sys_calib_const=rbyt4(l4)
              if(linux) call byte_swap_r4(sys_calib_const,1)

              ii=radial_const_pointer+1+16+28
              l2=(ii+1)/2
              inyquist_vel=ibyt2(l2)
              if(linux) call byte_swap_i2(inyquist_vel,1)
              nyquist_vel=real(inyquist_vel)*0.01

              ii=elev_const_pointer+1+6+28
              l2=(ii+1)/2
              iatmos_atten_factor=ibyt2(l2)
              if(linux) call byte_swap_i2(iatmos_atten_factor,1)
              atmos_atten_factor=real(iatmos_atten_factor)*0.001

              ii=25
              l2=(ii+1)/2
              num_of_message=ibyt2(l2)
              if(linux) call byte_swap_i2(num_of_message,1)

              ii=27
              l2=(ii+1)/2
              message_num=ibyt2(l2)
              if(linux) call byte_swap_i2(message_num,1)

              ii=19
              l2=(ii+1)/2
              obs_date=ibyt2(l2)
              if(linux) call byte_swap_i2(obs_date,1)

              ii=radial_const_pointer+1+6+28
              l2=(ii+1)/2
              iunambiguous_range=ibyt2(l2)
              if(linux) call byte_swap_i2(iunambiguous_range,1)
              unambiguous_range=real(iunambiguous_range)*0.1
             
              ii=39
              l2=(ii+1)/2
              radial_num=ibyt2(l2)
              if(linux) call byte_swap_i2(radial_num,1)
              ray_num=ray_num+radial_num

              ii=41
              l4=(ii+3)/4
              azimth_angle=rbyt4(l4)
              if(linux) call byte_swap_r4(azimth_angle,1)

              ii=49
              azmth_resolution=real(ibyt1(ii))*0.5

              if (superob) then
              if (radial_num.eq.1) azcount=0
              if (ibyt1(ii).lt.2.) then
               azmth_resolution=1.0
               if (mod(radial_num,2).eq.0) then
                goto 100
               else
                azcount=azcount+1
               endif
               radial_num=azcount
              endif
              endif

              rad_status=ibyt1(ii+1)

              ii=51
              elev_num=ibyt1(ii)
              tilt_sector_num=ibyt1(ii+1)

              ii=53
              l4=(ii+3)/4
              elv_angle=rbyt4(l4)
              if(linux) call byte_swap_r4(elv_angle,1)

              ii=59
              l2=(ii+1)/2
              data_block_count=ibyt2(l2)
              if(linux) call byte_swap_i2(data_block_count,1)
              if(diag) then
              print*,"Number of Data Blocks = ",data_block_count
              endif

              ref_data=.false.
              vel_data=.false.
              spw_data=.false.
              ref_data_pointer=0
              vel_data_pointer=0
              spw_data_pointer=0
              num_of_ref_gates=1
              num_of_vel_gates=1
              num_of_spw_gates=1
              ref_gate_size(elev_num)=0
              vel_gate_size=0
              spw_gate_size=0
              ref_gate1_range(elev_num)=minrang
              vel_gate1_range=minrang
              spw_gate1_range=minrang
              ref_scale=refmis
              vel_scale=velmis
              spw_scale=velmis
              ref_offset=refmis
              vel_offset=velmis
              spw_offset=velmis
              ref_control_flag=99
              vel_control_flag=99
              count=0


              do i=1,data_block_count-3

              ii=69+(i*4)
              l4=(ii+3)/4
              moment_data_pointer=ibyt4(l4)
              if(linux) call byte_swap_i4(moment_data_pointer,1)

c Note: To this point in the Message 31 level II data files,
c all header data are grouped into blocks which always contain
c an even number of header data, therefore, the formula
c "l2=(i+2)/2" and "l4=(i+3)/4" are always accurate.  However,
c from this point forward, the moment data pointer numbers can be 
c either even or odd, depending on the number of moment data
c gates found within each tilt. Since "l2=(i+2)/2" and "l4=(i+3)/4" 
c are not accurate for odd moment data pointer numbers,
c explicit assignment of 1 byte data to
c either 2 or 4 byte data is necessary, as follows: 
 
              ii=moment_data_pointer+1+20+28
              do jj=1,4
              dummyr4_byte1(jj)=ibyt1(ii+jj-1)
              enddo
              moment_scale=dummy_rbyt4
              if(linux) call byte_swap_r4(moment_scale,1)

              if (moment_scale.lt.0.001) moment_scale=0.

              ii=moment_data_pointer+1+24+28
              do jj=1,4
              dummyr4_byte1(jj)=ibyt1(ii+jj-1)
              enddo
              moment_offset=dummy_rbyt4
              if(linux) call byte_swap_r4(moment_offset,1)

              ii=moment_data_pointer+1+28
              moment_char1(1:1)=char(ibyt1(ii))
              ii=moment_data_pointer+1+28+1
              moment_char2(1:1)=char(ibyt1(ii))
              ii=moment_data_pointer+1+28+2
              moment_char3(1:1)=char(ibyt1(ii))
              ii=moment_data_pointer+1+28+3
              moment_char4(1:1)=char(ibyt1(ii))
              if(diag) then
              print*,moment_char1,moment_char2,moment_char3,moment_char4
              endif
              
               moment_char(1:4)=moment_char1(1:1)//moment_char2(1:1)//
     +moment_char3(1:1)//moment_char4(1:1)
              if(diag) then
              print*,"Moment = ", moment_char(1:4)
              endif

              if (moment_char(1:4).eq."DREF") then
               ref_data=.true.
               ref_data_pointer=moment_data_pointer
               ref_scale=moment_scale
               ref_offset=moment_offset
               count=count+1
              endif

              if (moment_char(1:4).eq."DVEL") then
               vel_data=.true.
               vel_data_pointer=moment_data_pointer
               vel_scale=moment_scale
               vel_offset=moment_offset
               count=count+1
              endif

              if (moment_char(1:3).eq."DSW") then
               spw_data=.true.
               spw_data_pointer=moment_data_pointer
               spw_scale=moment_scale
               spw_offset=moment_offset
               count=count+1
              endif

              if (moment_char(1:4).eq."DRHO") count=count+1

              if (moment_char(1:4).eq."DPHI") count=count+1

              if (moment_char(1:4).eq."DZDR") count=count+1

              enddo

              if (.not.ref_data.and..not.vel_data.and.
     +.not.spw_data) then
               print*,"Currupted Data Message - Skip1"
               goto 100
              endif

              if (count.ne.data_block_count-3) then
               print*,"Currupted Data Message - Skip2"
               print*,"count = ",count
               goto 100
              endif

              if (ref_data.and.vel_data) then
              if ((vel_data_pointer-ref_data_pointer).lt.100)
     +print*, "Pointer mismatch (ref/vel):",vel_data_pointer
     +,ref_data_pointer
              endif

              if (ref_data) then

              ii=ref_data_pointer+1+8+28
              dummyi2_byte1(1)=ibyt1(ii)
              dummyi2_byte1(2)=ibyt1(ii+1)
              num_of_ref_gates=dummy_ibyt2
              if(linux) call byte_swap_i2(num_of_ref_gates,1)

              ii=ref_data_pointer+1+10+28
              dummyi2_byte1(1)=ibyt1(ii)
              dummyi2_byte1(2)=ibyt1(ii+1)
              if(linux) call byte_swap_i2(dummy_ibyt2,1)
              ref_gate1_range(elev_num)=dummy_ibyt2

              ii=ref_data_pointer+1+12+28
              dummyi2_byte1(1)=ibyt1(ii)
              dummyi2_byte1(2)=ibyt1(ii+1)
              if(linux) call byte_swap_i2(dummy_ibyt2,1)
              ref_gate_size(elev_num)=dummy_ibyt2

              zinc=1
              if (superob) then
              if (ref_gate_size(elev_num).lt.1000)
     &         zinc=4
              endif

              ii=ref_data_pointer+1+18+28
              ref_control_flag=ibyt1(ii)

              endif

              if (vel_data) then

              ii=vel_data_pointer+1+8+28
              dummyi2_byte1(1)=ibyt1(ii)
              dummyi2_byte1(2)=ibyt1(ii+1)
              num_of_vel_gates=dummy_ibyt2
              if(linux) call byte_swap_i2(num_of_vel_gates,1)

              ii=vel_data_pointer+1+10+28
              dummyi2_byte1(1)=ibyt1(ii)
              dummyi2_byte1(2)=ibyt1(ii+1)
              vel_gate1_range=dummy_ibyt2
              if(linux) call byte_swap_i2(vel_gate1_range,1)

              ii=vel_data_pointer+1+12+28
              dummyi2_byte1(1)=ibyt1(ii)
              dummyi2_byte1(2)=ibyt1(ii+1)
              vel_gate_size=dummy_ibyt2
              if(linux) call byte_swap_i2(vel_gate_size,1)

              ii=vel_data_pointer+1+18+28
              vel_control_flag=ibyt1(ii)

              endif
              
              if (spw_data) then

              ii=spw_data_pointer+1+8+28
              dummyi2_byte1(1)=ibyt1(ii)
              dummyi2_byte1(2)=ibyt1(ii+1)
              num_of_spw_gates=dummy_ibyt2
              if(linux) call byte_swap_i2(num_of_spw_gates,1)

              ii=spw_data_pointer+1+10+28
              dummyi2_byte1(1)=ibyt1(ii)
              dummyi2_byte1(2)=ibyt1(ii+1)
              spw_gate1_range=dummy_ibyt2
              if(linux) call byte_swap_i2(spw_gate1_range,1)

              ii=spw_data_pointer+1+12+28
              dummyi2_byte1(1)=ibyt1(ii)
              dummyi2_byte1(2)=ibyt1(ii+1)
              spw_gate_size=dummy_ibyt2
              if(linux) call byte_swap_i2(spw_gate_size,1)

              endif


c********0*********0*********0*********0*********0*********0*********
c     Save azimth angles(degree*10), elevation angles(degree*100),
c     the number of radials on one tile, and the nyquist velocity
c     ((m/sec)*10) for output
c********0*********0*********0*********0*********0*********0*********
              elv_num=elev_num
              if(diag) then
              print*,"Elevation Number = ",elv_num
              endif

              iazmth_resolution(elv_num)=azmth_resolution*10.0
              phi(radial_num,elv_num)=azimth_angle*10.0
              the(elv_num)           =elv_angle*100.0
              nphi(elv_num)          =radial_num
              gridnyq(radial_num,elv_num)=nyquist_vel*10.0
              bins_z(elv_num)=num_of_ref_gates
              bins_v(elv_num)=num_of_vel_gates

      ref_save=.true.
      vel_save=.true.
c
c prevent processing and storage of redundant split_scan data
c Note: needs more work to handle the complex new corrupted file logic
c
c     if (ref_data.and.vel_data.and.elv_num.le.
c    &split_scan_last_tilt) ref_save=.false.
c     if (vol_cov_pattern.eq.121) then
c      if (vel_data) then
c      if (elv_num.le.3) vel_save=.false.
c      if (elv_num.gt.4.and.elv_num.le.7) vel_save=.false.
c      if (elv_num.gt.8.and.elv_num.le.10) vel_save=.false.
c      if (elv_num.gt.11.and.elv_num.le.13) vel_save=.false.
c      if (elv_num.gt.14.and.elv_num.le.16) vel_save=.false.
c     endif
c     if (ref_data) then
c      if (elv_num.gt.1.and.elv_num.le.4) ref_save=.false.
c      if (elv_num.gt.5.and.elv_num.le.8) ref_save=.false.
c      if (elv_num.gt.9.and.elv_num.le.11) ref_save=.false.
c      if (elv_num.gt.12.and.elv_num.le.14) ref_save=.false.
c      if (elv_num.eq.16) ref_save=.false.
c     endif
c     endif


c********0*********0*********0*********0*********0*********0*********
c     decode the Reflectivity Factor (dBZ) data
c********0*********0*********0*********0*********0*********0*********

      if(ref_save) then
      if(ref_data) then
                zcount=0
                i1=ref_data_pointer+28+28+1
                i2=i1+num_of_ref_gates-1
                if(diag) then
                   print *,'ref --- i1, i2=',i1,i2
                endif
                do ii=i1,i2,zinc
                   zcount=zcount+1
                   jj=zcount
                   if(ibyt1(ii).ne.0.and.ibyt1(ii).ne.1) then
                    if (ref_scale.ne.0) then
c ref data stored as byte, but read in as integer*1
                      ref=(real(iachar(char(ibyt1(ii))))-ref_offset)
     &/ref_scale
                    else
                      ref=real(iachar(char(ibyt1(ii))))
                    endif
                    gridref(jj,radial_num,elv_num)=ref*10.0
                   else
                      gridref(jj,radial_num,elv_num)=refmis
                   endif
                enddo

c max reflectivity range
c
              if (zinc.eq.4) then
               num_of_ref_gates=num_of_ref_gates/4
               bins_z(elv_num)=num_of_ref_gates
               ref_gate_size(elv_num)=1000
              endif

c     max_r_z(elv_num)=0.001*real(ref_gate1_range(elv_num)
c    &      +(num_of_ref_gates-1)*ref_gate_size(elv_num))

c calulate range and height of reflectivity sample volume
c
                do ii=1,num_of_ref_gates

                  range_z(ii,radial_num,elv_num)=0.001*
     &cos(elv_angle*deg2rad)*
     &real(ref_gate1_range(elv_num)+real(ii-1)*ref_gate_size(elv_num))

c      range=range_z(ii,radial_num,elv_num)
c      hgt_z(ii,radial_num,elv_num)=1000.*sqrt(ae**2+range**2+
c    &  2.*ae*range*sin(elv_angle*deg2rad))-ae

                enddo

              endif
              endif

c********0*********0*********0*********0*********0*********0*********
c     decode the Radial Velocity (m/sec) data
c********0*********0*********0*********0*********0*********0*********
       if(vel_save) then
       if(vel_data) then
                i1=vel_data_pointer+28+28+1
                i2=i1+num_of_vel_gates-1
                if(diag) then
                   print *,'vel --- i1, i2=',i1,i2
                endif
                do ii=i1,i2
                   jj=ii-i1+1
                   if(ibyt1(ii).ne.0.and.ibyt1(ii).ne.1) then
                    if (vel_scale.ne.0) then
c vel data stored as byte, but read in as integer*1
                      vel=(real(iachar(char(ibyt1(ii))))-vel_offset)
     &/vel_scale
                    else
                      vel=real(iachar(char(ibyt1(ii))))
                    endif
                    gridvel(jj,radial_num,elv_num)=vel*10.0
                   else
                      gridvel(jj,radial_num,elv_num)=velmis
                   endif
                enddo

              endif

c********0*********0*********0*********0*********0*********0*********
c     decode the Spectrum Width (m/sec) data
c********0*********0*********0*********0*********0*********0*********
       if(spw_data) then
                i1=spw_data_pointer+28+28+1
                i2=i1+num_of_spw_gates-1
                if(diag) then
                   print *,'spw --- i1, i2=',i1,i2
                endif
                do ii=i1,i2
                   jj=ii-i1+1
                   if(ibyt1(ii).ne.0.and.ibyt1(ii).ne.1) then
                    if (spw_scale.ne.0) then
c spw data stored as byte, but read in as integer*1
                      spw=(real(iachar(char(ibyt1(ii))))-spw_offset)
     &/spw_scale
                    else
                      spw=real(iachar(char(ibyt1(ii))))
                    endif
                    gridsw(jj,radial_num,elv_num)=spw*10.0
                   else
                      gridsw(jj,radial_num,elv_num)=velmis
                   endif
                enddo

              endif
              endif

c********0*********0*********0*********0*********0*********0*********
c     Unpack the data_date and data_time using unpkdt_2
c********0*********0*********0*********0*********0*********0*********
              call unpkdt_2(obs_date,data_time
     &                   ,yyyymmddhhmmss,ierr)
c********0*********0*********0*********0*********0*********0*********
c     Print out the message for debuging
c********0*********0*********0*********0*********0*********0*********
              if(diag) then

                 print *,'*************************************'
                 print *,'*****     Data Information      *****'
                 print *,'*************************************'
                 print *,'*    num_of_message=',num_of_message
                 print *,'*    message_num=',message_num
                 print *,'*    unambiguous_range=',unambiguous_range
                 print *,'*    azimth_angle=',azimth_angle
                 print *,'*    radial_num=',radial_num
                 print *,'*    rad_status (MSG 31)=',rad_status
                 print *,'*    elv_angle=',elv_angle
                 print *,'*    elev_num=',elev_num
             print *,'*    ref_gate1_range=',ref_gate1_range(elev_num)
                 print *,'*    vel_gate1_range=',vel_gate1_range
                 print *,'*    spw_gate1_range=',spw_gate1_range
                 print *,'*    ref_gate_size=',ref_gate_size(elev_num)
                 print *,'*    vel_gate_size=',vel_gate_size
                 print *,'*    spw_gate_size=',spw_gate_size
                 print *,'*    ref_offset=',ref_offset
                 print *,'*    vel_offset=',vel_offset
                 print *,'*    spw_offset=',spw_offset
                 print *,'*    ref_scale=',ref_scale
                 print *,'*    vel_scale=',vel_scale
                 print *,'*    spw_scale=',spw_scale
                 print *,'*    num_of_ref_gates=',num_of_ref_gates
                 print *,'*    num_of_vel_gates=',num_of_vel_gates
                 print *,'*    num_of_spw_gates=',num_of_spw_gates
                 print *,'*    sec_num_in_cut=',tilt_sector_num
                 print *,'*    sys_calib_const=',sys_calib_const
                 print *,'*    ref_pointer=',ref_data_pointer
                 print *,'*    vel_pointer=',vel_data_pointer
                 print *,'*    spw_pointer=',spw_data_pointer
                 print *,'*    azmth_resolution=',azmth_resolution
                 print *,'*    vol_cov_pattern=',vol_cov_pattern
                 print *,'*    nyquist_vel=',nyquist_vel
                 print *,'*    atmos_atten_factor=',atmos_atten_factor
                 if (ref_control_flag.eq.0) then
                  print*,"Reflectivity Resolution has not been reduced"
                 elseif (ref_control_flag.eq.1) then
                  print*,"Reflectivity has Super Resolution range only"
                 elseif (ref_control_flag.eq.2) then
                print*,"Reflectivity has Super Resolution azimuth only"
                 elseif (ref_control_flag.eq.3) then
                 print*,"Reflectivity Resolution has been fully reduced"
                 endif
                 if (vel_control_flag.eq.0) then
                print*,"Radial Velocity Resolution has not been reduced"
                 elseif (vel_control_flag.eq.1) then
               print*,"Radial Velocity has Super Resolution range only"
                 elseif (vel_control_flag.eq.2) then
             print*,"Radial Velocity has Super Resolution azimuth only"
                 elseif (vel_control_flag.eq.3) then
              print*,"Radial Velocity Resolution has been fully reduced"
                 endif
                 print *,'*************************************'
           endif
c end of message type cases
      endif

      go to 100
c
c********0*********0*********0*********0*********0*********0*********
c    If data is successfully decoded, check reflectivity for constant
c    power function artifacts (e.g., bull's eyes and sun strobes)
c    and write out data in linear binary format for later uses. 
c********0*********0*********0*********0*********0*********0*********
c
 9100 continue
      close(20)
              if(diag) then
      print *,'*    rad_status (MSG 31)=',rad_status
              endif
c
c PH: If level II file does not contain at least one complete tilt
c (rad_status=2) or one complete volume of tilts (rad_status=4)
c reject data as QC will not work otherwise.
c
     
      if(rad_status.eq.2.or.rad_status.eq.4) then
         print *,'* Finish reading and decoding the file'
         print *,'* Will write out the decoded data:'

         num_of_elv=elv_num

       do j=1,26
        do k=1,4
         if (radid(k:k).eq.lower(j)) radid(k:k)=upper(j)
        enddo
       enddo

         write(out_data_fname,1000)radid,year,month,day,hour,
     &minute,second
 1000    format(a4,i4.4,i2.2,i2.2,i2.2,i2.2,i2.2,'.txt')
         call slen(out_data_path,lout_data_path)
         call slen(out_data_fname,lout_data_fname)
         outfile=out_data_path(1:lout_data_path)//
     &           out_data_fname(1:lout_data_fname)
         call Lvl2_to_CompZ(outfile,radid
     &              ,year,month,day,hour,minute,second
     &              ,num_of_elv)

c
c********0*********0*********0*********0*********0*********0*********
c    job ends smoothly
c********0*********0*********0*********0*********0*********0*********
c
         stop
c
c********0*********0*********0*********0*********0*********0*********
c    job goes wrong. error messages
c********0*********0*********0*********0*********0*********0*********
c
      else
         print *,'******************** Warning message **********'
         print *,'* The file ',infile2, ' ended with suspect data '
         print *,'* and will not be assimilated.'
         print *,'******************** Warning message **********'
         stop 9100
      endif

 9000 continue
      print *,'******************** Warning message **********'
      print *,'* The last byte read indicates premature EOF.'
      print *,'* The data in ', infile2,'  will not be assimilated.'
      print *,'******************** Warning message **********'
      stop 9000
 9300 continue
      print *,'******************** Warning message **********'
      print *,'*  The radar product file: '
      print *,'*    ',infile2,'  does not exist.'
      print *,'******************** Warning message **********'
      stop 9300
 9600 continue
      print *,'******************** Warning message **********'
      print *,'* Level II file:'
      print *,'*    ',infile2
      print *,'* is corrupt or incomplete (missing header). '
      print *,'******************** Warning message **********'
      stop 9600
c********0*********0*********0*********0*********0*********0*********
      end
