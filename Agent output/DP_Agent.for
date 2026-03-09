      SUBROUTINE UMAT(STRESS,STATEV,DDSDDE,SSE,SPD,SCD,
     & RPL,DDSDDT,DRPLDE,DRPLDT,
     & STRAN,DSTRAN,TIME,DTIME,TEMP,DTEMP,PREDEF,DPRED,
     & CMNAME,NDI,NSHR,NTENS,NSTATV,PROPS,NPROPS,COORDS,
     & DROT,PNEWDT,CELENT,DFGRD0,DFGRD1,NOEL,NPT,LAYER,KSPT,
     & KSTEP,KINC)
C
      INCLUDE 'ABA_PARAM.INC'
C
      CHARACTER*80 CMNAME
      DIMENSION STRESS(NTENS),STATEV(NSTATV),DDSDDE(NTENS,NTENS)
      DIMENSION STRAN(NTENS),DSTRAN(NTENS),PROPS(NPROPS)
      DIMENSION TIME(2),PREDEF(*),DPRED(*)
      DIMENSION COORDS(*),DROT(3,3),DFGRD0(3,3),DFGRD1(3,3)
C
C *** ---- MATERIAL PARAMETER ASSIGNMENT ----
C PROPS(1): Young's modulus E
C PROPS(2): Poisson's ratio nu
C PROPS(3): cohesion c
C PROPS(4): friction angle (phi, in degrees)
C PROPS(5): dilation angle (psi, in degrees)
C
      DOUBLE PRECISION E,NU,C0,PHI,PSI
      DOUBLE PRECISION SQRT3, SINPHI, COSPHI, SINPSI
      DOUBLE PRECISION KPHI, KC, KPSI
      DOUBLE PRECISION G, K, DUM
      DOUBLE PRECISION STRESS_TRIAL(6), STRESS_NEW(6)
      DOUBLE PRECISION DEV_TRIAL(6), DEV_NEW(6)
      DOUBLE PRECISION SIGM(6)
      DOUBLE PRECISION SIGM_M, J2, F, GFUNC
      DOUBLE PRECISION DSTRAN_E(6)
      DOUBLE PRECISION NORM_DEV, D_GAMMA
      DOUBLE PRECISION N(6), M(6)
      DOUBLE PRECISION A, B, C, DELTA
      DOUBLE PRECISION PLAST_STR, DPEQ
	  DOUBLE PRECISION LAMBDA
      INTEGER I, J, CONVERGED
      DOUBLE PRECISION TOL
      PARAMETER (TOL=1.0D-8)
C
C 1. Read material parameters
      E    = PROPS(1)
      NU   = PROPS(2)
	  C0   = PROPS(3)
	  PHI  = PROPS(4)
      PSI  = PROPS(5)
C
      SQRT3 = SQRT(3.0D0)
      SINPHI = DSIN(PHI*3.141592653589793D0/180.0D0)
      COSPHI = DCOS(PHI*3.141592653589793D0/180.0D0)
      SINPSI = DSIN(PSI*3.141592653589793D0/180.0D0)
C
      KPHI = 6.0D0*SINPHI/(SQRT3*(3.0D0-SINPHI))
      KC   = 6.0D0*C0*COSPHI/(SQRT3*(3.0D0-SINPHI))
      KPSI = 6.0D0*SINPSI/(SQRT3*(3.0D0-SINPSI))
C
      G = E/(2.0D0*(1.0D0+NU))
      K = E/(3.0D0*(1.0D0-2.0D0*NU))
C
C 2. Elastic trial step (predictor)
      DO I=1,6
         STRESS_TRIAL(I) = STRESS(I)
      ENDDO
C
      DO I=1,3
         STRESS_TRIAL(I) = STRESS_TRIAL(I) + K*(DSTRAN(1)+DSTRAN(2)+DSTRAN(3))
     &                     + 2.0D0*G*(DSTRAN(I) - (DSTRAN(1)+DSTRAN(2)+DSTRAN(3))/3.0D0)
      ENDDO
      DO I=4,6
         STRESS_TRIAL(I) = STRESS_TRIAL(I) + G*DSTRAN(I)
      ENDDO
C
C 3. Compute trial mean stress and deviatoric stress
      SIGM_M = (STRESS_TRIAL(1) + STRESS_TRIAL(2) + STRESS_TRIAL(3))/3.0D0
      DO I=1,3
         DEV_TRIAL(I) = STRESS_TRIAL(I) - SIGM_M
      ENDDO
      DEV_TRIAL(4) = STRESS_TRIAL(4)
      DEV_TRIAL(5) = STRESS_TRIAL(5)
      DEV_TRIAL(6) = STRESS_TRIAL(6)
      J2 = 0.5D0*(DEV_TRIAL(1)**2 + DEV_TRIAL(2)**2 + DEV_TRIAL(3)**2)
     &     + DEV_TRIAL(4)**2 + DEV_TRIAL(5)**2 + DEV_TRIAL(6)**2
      IF (J2 .GT. 0.0D0) THEN
         NORM_DEV = SQRT(J2)
      ELSE
         NORM_DEV = 0.0D0
      ENDIF
C
C 4. Evaluate yield function
      F = NORM_DEV + KPHI*SIGM_M - KC
C
      IF (F .LE. TOL) THEN
C --------- ELASTIC STEP ----------
         DO I=1,6
            STRESS(I) = STRESS_TRIAL(I)
         ENDDO
C
C Elastic stiffness (DDSDDE, plane stress/strain automatically handled by ABAQUS)
         DUM = E/((1.0D0+NU)*(1.0D0-2.0D0*NU))
         DO I=1,3
            DO J=1,3
               DDSDDE(I,J) = NU*DUM
            ENDDO
            DDSDDE(I,I) = (1.0D0-NU)*DUM
         ENDDO
         DO I=4,6
            DDSDDE(I,I) = 0.5D0*E/(1.0D0+NU)
         ENDDO
C
         RETURN
      ENDIF
C
C --------- PLASTIC STEP (return mapping, radial, 1 step) ----------
C Use closest-point projection (radial return in deviatoric space)
C Solve for plastic multiplier increment d_gamma
C
C 5. Compute flow direction
      DO I=1,6
         IF (NORM_DEV .GT. 0.0D0) THEN
            N(I) = DEV_TRIAL(I)/NORM_DEV
         ELSE
            N(I) = 0.0D0
         ENDIF
         M(I) = 1.0D0/3.0D0
      ENDDO
C
      A = 2.0D0*G + K*KPHI*KPSI
      B = NORM_DEV + KPHI*SIGM_M - KC
      D_GAMMA = B/A
      IF (D_GAMMA .LT. 0.0D0) D_GAMMA = 0.0D0
C
C 6. Update stress
      DO I=1,6
         STRESS_NEW(I) = STRESS_TRIAL(I)
     &                - 2.0D0*G*D_GAMMA*N(I)
     &                - K*KPSI*D_GAMMA*M(I)
      ENDDO
C
C 7. Update mean stress and deviatoric stress
      SIGM_M = (STRESS_NEW(1) + STRESS_NEW(2) + STRESS_NEW(3))/3.0D0
      DO I=1,3
         DEV_NEW(I) = STRESS_NEW(I) - SIGM_M
      ENDDO
      DEV_NEW(4) = STRESS_NEW(4)
      DEV_NEW(5) = STRESS_NEW(5)
      DEV_NEW(6) = STRESS_NEW(6)
C
C 8. Overwrite updated stress to output
      DO I=1,6
         STRESS(I) = STRESS_NEW(I)
      ENDDO
C
C 9. Update equivalent plastic strain (STATEV(1))
C    Increment: d_ep_eq = sqrt(2/3) * |plastic strain increment|
      DPEQ = D_GAMMA * SQRT(2.0D0/3.0D0)
      STATEV(1) = STATEV(1) + DPEQ
C
C 10. Return elastic stiffness as algorithmic tangent (for simplicity)
      LAMBDA = K - 2.0D0*G/3.0D0
      DO I=1,6
         DO J=1,6
            DDSDDE(I,J) = 0.0D0
         ENDDO
      ENDDO
      DO I=1,3
         DO J=1,3
            DDSDDE(I,J) = LAMBDA
         ENDDO
         DDSDDE(I,I) = DDSDDE(I,I) + 2.0D0*G
      ENDDO
      DO I=4,6
         DDSDDE(I,I) = G
      ENDDO
C
C 11. End subroutine
      RETURN
      END
