
MODULE module_autodiff

   INTEGER :: NumofTL=0,NumofAD=0,TimeTest=1
   INTEGER :: MAXIIT,IALL,JALL

   REAL,ALLOCATABLE,DIMENSION(:) :: keepadvecttendx,keepxxxxx,keepmoist,keepgridph_2A,&
   keepgridph_2B,keept_2save,keepgridww,keepgridw_2,keepgridu_2,keepgridv_2,&
   keepgridt_2,keepgridal,keepgridp,keepph_tend,keepcqu,keepcqv,keepcqw,keeprw_tend,&
   keepgridwwB,keepgridph_2C,keepgridph_2D,keepgridpB,keepgridt_2B,keepgridw_2B,&
   keepgridu_2B,keepgridv_2B,keept_saveE,keepgridt_2E,keepw_saveE,keepgridw_2E,&
   keepv_saveE,keepgridv_2E,keepu_saveE,keepgridu_2E,keepxyxx,keepww1,keepgridphp,&
   keepgridalt,keepph_save,keepc2a,keepcquB,keepcqvB,keepcqwB,keepgridalB,keepgridrw,&
   keepgridrv,keepgridru,keepgridt_2H,keeph_diabatic,keepgridt_2F,keepth_phy,keepalt,&
   keepmoist_tend,keeptracer_tend,keepmoistxyz,keepgridru_m,keepgridrv_m,keepxxyyy,&
   keepgridmutsB,keepmuave,keepgridmu_2,keepmu_tend,keepgridmutsA,keepgridmu_2B,&
   keepgridw_2www,keeppi_phy

CONTAINS

   SUBROUTINE ALLOCATING(ims,ime,kms,kme,jms,jme,num_moist,PARAM_FIRST_SCALAR,num_3d_m,num_tracer)

   INTEGER ims,ime,kms,kme,jms,jme,num_moist,PARAM_FIRST_SCALAR,num_3d_m,num_tracer

   IF(NumofAD.NE.1) RETURN

   JALL =(ime-ims+1)*(jme-jms+1)
   IALL =JALL*(kme-kms+1)

   ALLOCATE (keepgridph_2A(MAXIIT*IALL))
   ALLOCATE (keepgridph_2B(MAXIIT*IALL))
   ALLOCATE (keept_2save(MAXIIT*IALL))
   ALLOCATE (keepgridww(MAXIIT*IALL))
   ALLOCATE (keepgridw_2www(MAXIIT*IALL))
   ALLOCATE (keepgridw_2(MAXIIT*IALL))
   ALLOCATE (keepgridu_2(MAXIIT*IALL))
   ALLOCATE (keepgridv_2(MAXIIT*IALL))
   ALLOCATE (keepgridt_2(MAXIIT*IALL))
   ALLOCATE (keepgridal(MAXIIT*IALL))
   ALLOCATE (keepgridp(MAXIIT*IALL))
   ALLOCATE (keepph_tend(MAXIIT*IALL))

   ALLOCATE (keepmuave(MAXIIT*JALL))
   ALLOCATE (keepgridmu_2(MAXIIT*JALL))
   ALLOCATE (keepmu_tend(MAXIIT*JALL))
   ALLOCATE (keepgridmutsA(MAXIIT*JALL))
   ALLOCATE (keepgridmu_2B(3*JALL))
   ALLOCATE (keepgridmutsB(3*JALL))

   ALLOCATE (keepgridru_m(3*IALL))
   ALLOCATE (keepgridrv_m(3*IALL))
   ALLOCATE (keepgridu_2E(3*IALL))
   ALLOCATE (keepu_saveE(3*IALL))
   ALLOCATE (keepgridv_2E(3*IALL))
   ALLOCATE (keepv_saveE(3*IALL))
   ALLOCATE (keepgridw_2E(3*IALL))
   ALLOCATE (keepw_saveE(3*IALL))
   ALLOCATE (keepgridt_2E(3*IALL))
   ALLOCATE (keept_saveE(3*IALL))
   ALLOCATE (keepgridw_2B(3*IALL))
   ALLOCATE (keepgridu_2B(3*IALL))
   ALLOCATE (keepgridv_2B(3*IALL))
   ALLOCATE (keepgridt_2B(3*IALL))
   ALLOCATE (keepgridpB(3*IALL))
   ALLOCATE (keepgridph_2C(3*IALL))
   ALLOCATE (keepgridph_2D(3*IALL))
   ALLOCATE (keepgridwwB(3*IALL))
   ALLOCATE (keeprw_tend(3*IALL))
   ALLOCATE (keepcqu(3*IALL))
   ALLOCATE (keepcqv(3*IALL))
   ALLOCATE (keepcqw(3*IALL))

   ALLOCATE (keepcquB(3*IALL))
   ALLOCATE (keepcqvB(3*IALL))
   ALLOCATE (keepcqwB(3*IALL))
   ALLOCATE (keepww1(3*IALL))
   ALLOCATE (keepgridphp(3*IALL))
   ALLOCATE (keepgridalt(3*IALL))
   ALLOCATE (keepph_save(3*IALL))
   ALLOCATE (keepgridalB(3*IALL))
   ALLOCATE (keepgridrw(3*IALL))
   ALLOCATE (keepgridrv(3*IALL))
   ALLOCATE (keepgridru(3*IALL))
   ALLOCATE (keepgridt_2H(3*IALL))
   ALLOCATE (keepc2a(3*IALL))
   ALLOCATE (keepth_phy(IALL))
   ALLOCATE (keepalt(IALL))
   ALLOCATE (keeph_diabatic(IALL))
   ALLOCATE (keepgridt_2F(IALL))
   ALLOCATE (keeppi_phy(IALL))

   ALLOCATE (keeptracer_tend((num_tracer-PARAM_FIRST_SCALAR+1)*IALL))
   ALLOCATE (keepmoist_tend((num_3d_m-PARAM_FIRST_SCALAR+1)*IALL))
   ALLOCATE (keepmoist(num_moist*IALL))
   ALLOCATE (keepadvecttendx(18*IALL))
   ALLOCATE (keepmoistxyz(18*IALL))

!  ALLOCATE (keepxxxxx(18*IALL))
   ALLOCATE (keepxxxxx(6*IALL))

   ALLOCATE (keepxxyyy(18*IALL))
   ALLOCATE (keepxyxx(9*IALL))

   RETURN

   END SUBROUTINE ALLOCATING

   SUBROUTINE DEALLOCATING()

   IF(NumofAD.NE.NumofTL) RETURN
   NumofAD =0
   NumofTL =0

   DEALLOCATE (keeppi_phy)
   DEALLOCATE (keepgridt_2F)
   DEALLOCATE (keepgridt_2H)
   DEALLOCATE (keepgridph_2A)
   DEALLOCATE (keepgridph_2B)
   DEALLOCATE (keepgridph_2C)
   DEALLOCATE (keepgridph_2D)
   DEALLOCATE (keept_2save)
   DEALLOCATE (keepgridww)
   DEALLOCATE (keepgridwwB)
   DEALLOCATE (keepgridw_2www)
   DEALLOCATE (keepgridw_2)
   DEALLOCATE (keepgridu_2)
   DEALLOCATE (keepgridv_2)
   DEALLOCATE (keepgridt_2)
   DEALLOCATE (keepgridw_2B)
   DEALLOCATE (keepgridu_2B)
   DEALLOCATE (keepgridv_2B)
   DEALLOCATE (keepgridt_2B)
   DEALLOCATE (keepgridpB)
   DEALLOCATE (keepgridal)
   DEALLOCATE (keepgridalB)
   DEALLOCATE (keepgridp)
   DEALLOCATE (keepph_tend)
   DEALLOCATE (keepmuave)
   DEALLOCATE (keepgridmu_2)
   DEALLOCATE (keepgridmu_2B)
   DEALLOCATE (keepgridmutsA)
   DEALLOCATE (keepgridmutsB)
   DEALLOCATE (keepmu_tend)
   DEALLOCATE (keeprw_tend)
   DEALLOCATE (keepcquB)
   DEALLOCATE (keepcqvB)
   DEALLOCATE (keepgridrw)
   DEALLOCATE (keepgridrv)
   DEALLOCATE (keepgridru)
   DEALLOCATE (keepcqwB)
   DEALLOCATE (keepcqu)
   DEALLOCATE (keepcqv)
   DEALLOCATE (keepcqw)
   DEALLOCATE (keepww1)
   DEALLOCATE (keepth_phy)
   DEALLOCATE (keepgridphp)
   DEALLOCATE (keepgridalt)
   DEALLOCATE (keepph_save)
   DEALLOCATE (keepc2a)
   DEALLOCATE (keepgridu_2E)
   DEALLOCATE (keepu_saveE)
   DEALLOCATE (keepgridv_2E)
   DEALLOCATE (keepv_saveE)
   DEALLOCATE (keepgridw_2E)
   DEALLOCATE (keepw_saveE)
   DEALLOCATE (keepgridt_2E)
   DEALLOCATE (keept_saveE)
   DEALLOCATE (keepadvecttendx)
   DEALLOCATE (keepgridru_m)
   DEALLOCATE (keepgridrv_m)

   DEALLOCATE (keeptracer_tend)
   DEALLOCATE (keepmoist_tend)
   DEALLOCATE (keeph_diabatic)
   DEALLOCATE (keepmoistxyz)
   DEALLOCATE (keepmoist)
   DEALLOCATE (keepxxxxx)
   DEALLOCATE (keepxxyyy)
   DEALLOCATE (keepxyxx)
   DEALLOCATE (keepalt)

   RETURN

   END SUBROUTINE DEALLOCATING

   SUBROUTINE PUSHDATA(xstack,xdata,STATCKSTART,STATCKEND,DATASTART,DATAEND,&
   STACKLEN,DATALEN)

   INTEGER :: STATCKSTART,STATCKEND,DATASTART,DATAEND,STACKLEN,DATALEN
   REAL,DIMENSION(1:STACKLEN) :: xstack
   REAL,DIMENSION(1:DATALEN) :: xdata

   IF(DATAEND-DATASTART.NE.STATCKEND-STATCKSTART) THEN
     PRINT*, 'EXIT FROM PUSHDATA.'
     STOP
   END IF

   xstack(STATCKSTART:STATCKEND)=xdata(DATASTART:DATAEND)

   RETURN

   END SUBROUTINE PUSHDATA

   SUBROUTINE POPDATA(xstack,xdata,STATCKSTART,STATCKEND,DATASTART,DATAEND,&
   STACKLEN,DATALEN)

   INTEGER :: STATCKSTART,STATCKEND,DATASTART,DATAEND,STACKLEN,DATALEN
   REAL,DIMENSION(1:STACKLEN) :: xstack
   REAL,DIMENSION(1:DATALEN) :: xdata

   IF(DATAEND-DATASTART.NE.STATCKEND-STATCKSTART) THEN
     PRINT*, 'EXIT FROM POPDATA.'
     STOP
   END IF

   xdata(DATASTART:DATAEND)=xstack(STATCKSTART:STATCKEND)

   RETURN

   END SUBROUTINE POPDATA

   SUBROUTINE STARTANDEND(RANK,TOTAL,STATCKSTART,STATCKEND,STACKLEN,BASIC)

   INTEGER :: RANK,TOTAL,STATCKSTART,STATCKEND,STACKLEN,BASIC

   IF(RANK.GT.TOTAL) THEN
     PRINT*, 'EXIT FROM STARTANDEND!', RANK, TOTAL
     STOP
   END IF

   STATCKSTART=(RANK-1)*BASIC+1
   STATCKEND=RANK*BASIC
   STACKLEN=TOTAL*BASIC

   RETURN
   END SUBROUTINE STARTANDEND

END MODULE module_autodiff


