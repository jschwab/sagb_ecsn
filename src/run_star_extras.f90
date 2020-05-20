! ***********************************************************************
!
!   Copyright (C) 2010-2019  Bill Paxton & The MESA Team
!
!   this file is part of mesa.
!
!   mesa is free software; you can redistribute it and/or modify
!   it under the terms of the gnu general library public license as published
!   by the free software foundation; either version 2 of the license, or
!   (at your option) any later version.
!
!   mesa is distributed in the hope that it will be useful,
!   but without any warranty; without even the implied warranty of
!   merchantability or fitness for a particular purpose.  see the
!   gnu library general public license for more details.
!
!   you should have received a copy of the gnu library general public license
!   along with this software; if not, write to the free software
!   foundation, inc., 59 temple place, suite 330, boston, ma 02111-1307 usa
!
! ***********************************************************************

      module run_star_extras

      use star_lib
      use star_def
      use const_def
      use math_lib

      implicit none

      real(dp) :: ms_t0, cheb_t0, ms_t1, cheb_t1, m_1DUP, mcore_TACHeB
      real(dp) :: m_1cign, t_1cign, m_cflame, t_ONe_core, mcore_2DUP
      real(dp) :: mcore_1TP, age_1TP
      real(dp) :: mcore_at_TP, age_at_TP
      integer :: TP_count
      logical :: in_LHe_peak

      include "test_suite_extras_def.inc"

      contains

      include "test_suite_extras.inc"

      subroutine extras_photo_read(id, iounit, ierr)
        integer, intent(in) :: id, iounit
        integer, intent(out) :: ierr
        type (star_info), pointer :: s
        ierr = 0

        call star_ptr(id, s, ierr)
        if (ierr /= 0) return

        select case (s% x_integer_ctrl(1))
        case(1)
           read(iounit) ms_t0, cheb_t0, ms_t1, cheb_t1, m_1DUP, mcore_TACHeB
        case(2)
           read(iounit) m_1cign, t_1cign, m_cflame, t_ONe_core, mcore_2DUP
        case(3)
           read(iounit) mcore_at_TP, age_at_TP
           read(iounit) mcore_1TP, age_1TP, TP_count, in_LHe_peak
        case(4)
           read(iounit) TP_count, in_LHe_peak
        end select

      end subroutine extras_photo_read

      subroutine extras_photo_write(id, iounit)
        integer, intent(in) :: id, iounit
        integer :: ierr
        type (star_info), pointer :: s
        ierr = 0

        call star_ptr(id, s, ierr)
        if (ierr /= 0) return

        select case (s% x_integer_ctrl(1))
        case(1)
           write(iounit) ms_t0, cheb_t0, ms_t1, cheb_t1, m_1DUP, mcore_TACHeB
        case(2)
           write(iounit) m_1cign, t_1cign, m_cflame, t_ONe_core, mcore_2DUP
        case(3)
           write(iounit) mcore_at_TP, age_at_TP
           write(iounit) mcore_1TP, age_1TP, TP_count, in_LHe_peak
        case(4)
           write(iounit) TP_count, in_LHe_peak
        end select

      end subroutine extras_photo_write

      subroutine extras_controls(id, ierr)
         integer, intent(in) :: id
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return

         ! this is the place to set any procedure pointers you want to change
         ! e.g., other_wind, other_mixing, other_energy  (see star_data.inc)
         s% other_photo_read => extras_photo_read
         s% other_photo_write => extras_photo_write

         ! the extras functions in this file will not be called
         ! unless you set their function pointers as done below.
         ! otherwise we use a null_ version which does nothing (except warn).

         s% extras_startup => extras_startup
         s% extras_start_step => extras_start_step
         s% extras_check_model => extras_check_model
         s% extras_finish_step => extras_finish_step
         s% extras_after_evolve => extras_after_evolve
         s% how_many_extra_history_columns => how_many_extra_history_columns
         s% data_for_extra_history_columns => data_for_extra_history_columns
         s% how_many_extra_profile_columns => how_many_extra_profile_columns
         s% data_for_extra_profile_columns => data_for_extra_profile_columns

         s% how_many_extra_history_header_items => how_many_extra_history_header_items
         s% data_for_extra_history_header_items => data_for_extra_history_header_items
         s% how_many_extra_profile_header_items => how_many_extra_profile_header_items
         s% data_for_extra_profile_header_items => data_for_extra_profile_header_items

      end subroutine extras_controls


      subroutine extras_startup(id, restart, ierr)
         integer, intent(in) :: id
         logical, intent(in) :: restart
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return

         call test_suite_startup(id, restart, ierr)

         if (.not. restart) then
            select case(s% x_integer_ctrl(1))
            case(1)
               ms_t0 = 0
               ms_t1 = 0
               cheb_t0 = 0
               cheb_t1 = 0
               m_1DUP = 1d99
               mcore_TACHeB = 0
            case(2)
               m_1cign = 0
               t_1cign = 0
               m_cflame = 0
               t_ONe_core = 0
               mcore_2DUP = 0
            case(3)
               mcore_1TP = 0
               age_1TP = 0
               mcore_at_TP = 0
               age_at_TP = 0
               TP_count = 0
               in_LHe_peak = .false.
            case(4)
               TP_count = 0
               in_LHe_peak = .false.
            end select
         end if

      end subroutine extras_startup


      integer function extras_start_step(id)
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         extras_start_step = 0
      end function extras_start_step


      ! returns either keep_going, retry, backup, or terminate.
      integer function extras_check_model(id)
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         extras_check_model = keep_going

         ! if you want to check multiple conditions, it can be useful
         ! to set a different termination code depending on which
         ! condition was triggered.  MESA provides 9 customizeable
         ! termination codes, named t_xtra1 .. t_xtra9.  You can
         ! customize the messages that will be printed upon exit by
         ! setting the corresponding termination_code_str value.
         ! termination_code_str(t_xtra1) = 'my termination condition'

         ! by default, indicate where (in the code) MESA terminated
         if (extras_check_model == terminate) s% termination_code = t_extras_check_model
      end function extras_check_model


      integer function how_many_extra_history_columns(id)
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         how_many_extra_history_columns = 0
      end function how_many_extra_history_columns


      subroutine data_for_extra_history_columns(id, n, names, vals, ierr)
         integer, intent(in) :: id, n
         character (len=maxlen_history_column_name) :: names(n)
         real(dp) :: vals(n)
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return

         ! note: do NOT add the extras names to history_columns.list
         ! the history_columns.list is only for the built-in history column options.
         ! it must not include the new column names you are adding here.


      end subroutine data_for_extra_history_columns


      integer function how_many_extra_profile_columns(id)
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         how_many_extra_profile_columns = 0
      end function how_many_extra_profile_columns


      subroutine data_for_extra_profile_columns(id, n, nz, names, vals, ierr)
         integer, intent(in) :: id, n, nz
         character (len=maxlen_profile_column_name) :: names(n)
         real(dp) :: vals(nz,n)
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         integer :: k
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return

         ! note: do NOT add the extra names to profile_columns.list
         ! the profile_columns.list is only for the built-in profile column options.
         ! it must not include the new column names you are adding here.

         ! here is an example for adding a profile column
         !if (n /= 1) stop 'data_for_extra_profile_columns'
         !names(1) = 'beta'
         !do k = 1, nz
         !   vals(k,1) = s% Pgas(k)/s% P(k)
         !end do

      end subroutine data_for_extra_profile_columns


      integer function how_many_extra_history_header_items(id)
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         how_many_extra_history_header_items = 0
      end function how_many_extra_history_header_items


      subroutine data_for_extra_history_header_items(id, n, names, vals, ierr)
         integer, intent(in) :: id, n
         character (len=maxlen_history_column_name) :: names(n)
         real(dp) :: vals(n)
         type(star_info), pointer :: s
         integer, intent(out) :: ierr
         ierr = 0
         call star_ptr(id,s,ierr)
         if(ierr/=0) return

         ! here is an example for adding an extra history header item
         ! also set how_many_extra_history_header_items
         ! names(1) = 'mixing_length_alpha'
         ! vals(1) = s% mixing_length_alpha

      end subroutine data_for_extra_history_header_items


      integer function how_many_extra_profile_header_items(id)
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         how_many_extra_profile_header_items = 0
      end function how_many_extra_profile_header_items


      subroutine data_for_extra_profile_header_items(id, n, names, vals, ierr)
         integer, intent(in) :: id, n
         character (len=maxlen_profile_column_name) :: names(n)
         real(dp) :: vals(n)
         type(star_info), pointer :: s
         integer, intent(out) :: ierr
         ierr = 0
         call star_ptr(id,s,ierr)
         if(ierr/=0) return

         ! here is an example for adding an extra profile header item
         ! also set how_many_extra_profile_header_items
         ! names(1) = 'mixing_length_alpha'
         ! vals(1) = s% mixing_length_alpha

      end subroutine data_for_extra_profile_header_items

      logical function has_ignited(s, k)
         use net_def
         use chem_def
         use chem_lib
         implicit none
         type (star_info), pointer,intent(in) :: s
         integer,intent(in) :: k
         real(dp) :: neAbun,naAbun,mgAbun,heAbun
         real(dp) :: netEng,ne_burn,o_burn
         
         has_ignited = .false.
         if(s% c_core_mass > 0d0) then
            if(s%m(k)/Msun < s%c_core_mass)THEN
               netEng = star_get_profile_output(s,'net_nuclear_energy',k)
         
               if(netEng >= 0.0)THEN
                  neAbun = s%xa(s%net_iso(chem_get_iso_id('ne20')),k)
                  naAbun = s%xa(s%net_iso(chem_get_iso_id('na23')),k)
                  mgAbun = s%xa(s%net_iso(chem_get_iso_id('mg24')),k)
                  heAbun = s%xa(s%net_iso(chem_get_iso_id('he4')),k)
                  if(neAbun > naAbun .and. naAbun > mgAbun .and. heAbun < 1d-8)THEN
                     has_ignited=.true.
                     return
                  end if
               end if
            end if
         end if
      end function has_ignited

      ! returns either keep_going or terminate.
      ! note: cannot request retry or backup; extras_check_model can do that.
      integer function extras_finish_step(id)
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s

         integer :: k
         
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         extras_finish_step = keep_going

         ! to save a profile,
            ! s% need_to_save_profiles_now = .true.
         ! to update the star log,
            ! s% need_to_update_history_now = .true.

         select case (s% x_integer_ctrl(1))
         case (1)

            ! measure MS lifetime
            if (ms_t0 .eq. 0) then
               if (s% power_h_burn .gt. 0.99 * s% L_surf) then
                  ms_t0 = s% star_age
                  write(*,*) 'started MS', ms_t0
               end if
            else
               if (ms_t1 .eq. 0) then
                  if (s% center_h1 .lt. 1d-4) then
                     ms_t1 = s% star_age
                     write(*,*) 'finished MS', ms_t1
                  end if
               end if
            end if

            ! measure CHeB lifetime
            if ((ms_t1 .ne. 0) .and. (cheb_t0 .eq. 0)) then
               if (s% power_he_burn .gt. 0.01 * s% L_surf) then
                  cheb_t0 = s% star_age
                  write(*,*) 'started CHeB', cheb_t0
               end if
            else
               if (cheb_t1 .eq. 0) then
                  if (s% center_he4 .lt. 1d-4) then
                     cheb_t1 = s% star_age
                     write(*,*) 'finished CHeB', cheb_t1
                     mcore_TACHeB = s% he_core_mass
                  end if
               end if
            end if

            ! measure extent of 1DUP (deepest extend of convective envelope post MS)
            if (ms_t1 .gt. 0) then
               ! check that mixing region is large (bigger than 10% of the star)
               if ((s% conv_mx1_top - s% conv_mx1_bot) .gt. 0.1) then
                  ! check that mixed region is above He core
                  if (s% conv_mx1_bot*s% star_mass .gt. s% he_core_mass) then
                     write(*,*) s% conv_mx1_bot * s% star_mass, s% he_core_mass, m_1DUP
                     m_1DUP = min(s% conv_mx1_bot * s% star_mass, m_1DUP)
                  end if
               end if
            end if

         case(2)

            ! check to see if carbon has ignited (following Farmer et al. 2015)
            do k=1, s% nz
               if (has_ignited(s, k)) then 
                  m_cflame = s% m(k) / Msun ! track where the flame is
               end if
            end do

            ! save first flame location
            if ((m_1cign .eq. 0) .and. (m_cflame .gt. 0)) then
               m_1cign = m_cflame
               write(*,*) 'carbon ignition at mass coordinate (Msun)', m_1cign
               t_1cign = s% star_age
            end if

            ! check if ONe core has formed (flame has reached center)
            if ((s% center_ne20 .gt. 0.1) .and. (s% eps_nuc(s% nz) .lt. 1)) then
               termination_code_str(t_xtra1) = 'ONe core has formed'
               s% termination_code = t_xtra1
               extras_finish_step = terminate

               t_ONe_core = s% star_age
               mcore_2DUP = s% he_core_mass ! assume 2DUP completed already
            end if

         case(3)
            
            ! record thermal pulses
            if (.not. in_LHe_peak) then
               ! check for peak
               if (s% power_he_burn .gt. 1e4) then
                  in_LHe_peak = .true.
                  TP_count = TP_count + 1
                  write(*,*) 'starting thermal pulse', TP_count
                  mcore_at_TP = s% he_core_mass
                  age_at_TP = s% star_age
                  if (TP_count == 1) then
                     mcore_1TP = s% he_core_mass
                     age_1TP = s% star_age
                  end if
               end if
            else
               if (s% power_h_burn/s% power_he_burn .gt. 10) in_LHe_peak = .false. ! pulse over
            end if

         case(4)

            ! record thermal pulses
            if (.not. in_LHe_peak) then
               ! check for peak
               if (s% power_he_burn .gt. 1e4) then
                  in_LHe_peak = .true.
                  TP_count = TP_count + 1
                  write(*,*) 'starting thermal pulse'
               end if
            else
               if (s% power_h_burn/s% power_he_burn .gt. 10) in_LHe_peak = .false. ! pulse over
            end if

            ! stop after one TP
            if (.not. in_LHe_peak) then ! pulse is over
               if (TP_count == 1) then
                  termination_code_str(t_xtra1) = 'one thermal pulse cycle complete'
                  s% termination_code = t_xtra1
                  extras_finish_step = terminate
               end if
            end if

         end select

      end function extras_finish_step


      subroutine extras_after_evolve(id, ierr)
         integer, intent(in) :: id
         integer, intent(out) :: ierr
         type (star_info), pointer :: s

         integer :: k

         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return

         include 'formats'

         write(*,*)
         select case (s% x_integer_ctrl(1))
         case (1)
            write(*,'(A60, F8.3)') '>>>> Main-sequence lifetime (Myr): ', (ms_t1 - ms_t0) / 1d6
            write(*,'(A60, F8.3)') '>>>> Deepest penetration of first dredge-up (Msun): ', m_1DUP
            write(*,'(A60, F8.3)') '>>>> Core He-burning lifetime (Myr): ', (cheb_t1 - cheb_t0) / 1d6
            write(*,'(A60, F8.3)') '>>>> H-free core mass at the end of He-core burning (Msun): ', mcore_TACHeB
         case (2)
            write(*,'(A60, F8.3)') '>>>> Location of first carbon ignition (Msun): ', m_1cign
            write(*,'(A60, F8.3)') '>>>> Duration of inward C-burning, flash and flame  (kyr): ', (t_ONe_core - t_1cign) / 1e3
         case (3)
            write(*,'(A60, F8.3)') '>>>> Core mass at first thermal pulse (Msun): ', mcore_1TP
            write(*,'(A60, F8.3)') '>>>> Age at first thermal pulse (Myr): ', age_1TP / 1d6
         end select
         write(*,*)

         call test_suite_after_evolve(id, ierr)

      end subroutine extras_after_evolve


      end module run_star_extras
