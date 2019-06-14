!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!   ___                               _            _     _                       _  !!!
!!!  / __|  ___   _ __    _ __   _  _  | |_   __ _  | |_  (_)  ___   _ _    __ _  | | !!!
!!! | (__  / _ \ | '  \  | '_ \ | || | |  _| / _` | |  _| | | / _ \ | ' \  / _` | | | !!!
!!!  \___| \___/ |_|_|_| | .__/  \_,_|  \__| \__,_|  \__| |_| \___/ |_||_| \__,_| |_| !!!
!!!  ___   _             |_|  _                                                       !!!
!!! | _ \ | |_    _  _   ___ (_)  __   ___                                            !!!
!!! |  _/ | ' \  | || | (_-< | | / _| (_-<                                            !!!
!!! |_|   |_||_|  \_, | /__/ |_| \__| /__/                                            !!!
!!!  _  _         |__/                               _                                !!!
!!! | || |  ___   _ __    ___  __ __ __  ___   _ _  | |__                             !!!
!!! | __ | / _ \ | '  \  / -_) \ V  V / / _ \ | '_| | / /                             !!!
!!! |_||_| \___/ |_|_|_| \___|  \_/\_/  \___/ |_|   |_\_\                             !!!
!!!                                                                                   !!!
!!! Author:       cndaqiang                                                           !!!
!!! ContactMe:    https://cndaqiang.github.io                                         !!! 
!!! Name:         module_mpi                                                          !!!
!!! Last-update:  2019-04-05                                                          !!!
!!! Build-time:   2019-04-05                                                          !!!
!!! What it is:   mpi headfile : mpi_init  mpi_end mpi's everything                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module m_mpi_my
      implicit none
      include "mpif.h"
      INTEGER :: node, np,mpi_ierr,mpi_status(mpi_status_size),my_COMM
      INTEGER :: front_node,next_node,master_node=0
      contains
      subroutine mpi_start()
              call MPI_INIT(mpi_ierr)
              call MPI_COMM_DUP(MPI_COMM_WORLD,my_COMM,mpi_ierr)
              call MPI_COMM_RANK(my_COMM,node,mpi_ierr)
              call MPI_COMM_SIZE(my_COMM,np,mpi_ierr)

              front_node=mod(node-1,np)
              next_node=mod(node+1,np)
      End subroutine mpi_start

      subroutine mpi_end()
              call MPI_FINALIZE(mpi_ierr)
      End subroutine mpi_end

end module m_mpi_my

