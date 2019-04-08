# fourier fortran

c2019/3/20
c123456789
         IMPLICIT COMPLEX (C)
         READ(*,*) A,B
         CJ = (0.0,1.0)
         PAI =4.0*ATAN(1.0)
         DO 1 t=-3.14,3.14,0.1
         CFF = (0.0,0.0)
         DO 2 n=-50,50,1
         XI=n
         IF(n.EQ.0) THEN
         CFF = CFF + (A+B)/2.0
         ELSE
         CP1 = B*(CEXP(CJ*XI*PAI/2.0))
         CP2 = CEXP(CJ*XI*PAI)
         CP3 = (-CJ*XI)*2.0*PAI
         CP = (CP1-CP2)/CP3
c
         CQ1 = A*(CEXP(CJ*XI*PAI/2.0))
         CQ2 = CEXP(CJ*XI*PAI/2.0)
         CQ3 = (-CJ*XI)*2.0*PAI
         CQ = (CQ1-CQ2)/CQ3
c
         CR1 = B*(CEXP(CJ*XI*PAI))
         CR2 = CEXP(CJ*XI*PAI)
         CR3 = (-CJ*XI)*2.0*PAI
         CR = (CR1-CR2)/CR3
c
         CFF = CFF + CP + CQ + CR
         END IF
 2       CONTINUE
         Ft = CABS(CFF)
         WRITE (*,*) t,Ft
 1       CONTINUE
         STOP
         END
