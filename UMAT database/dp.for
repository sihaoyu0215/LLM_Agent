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
C     ----- Parameter and variable declarations -----
      DOUBLE PRECISION E, NU, QF, KF, QPSI, TENSION_MAX, TENSION
      DOUBLE PRECISION G, K, SM, J2, TAU, DP_FI, D_LAMD
      DOUBLE PRECISION NEWTAU, RATIO, ALPHAP, TAUP, DP_HFAI
      DOUBLE PRECISION DP_SIG, SEQV, DEPEFF, TENSION_INPUT
	  DOUBLE PRECISION INC_EV
      DOUBLE PRECISION SD(NTENS)
	  DOUBLE PRECISION FRIC, COHE, DILA
      INTEGER IPLAS, I, J
C
C     ----- Read material parameters from PROPS array -----
C     PROPS(1): Young's modulus E
C     PROPS(2): Poisson's ratio NU
C     PROPS(3): kfai (cohesion)
C     PROPS(3): qfai (related to internal friction angle)
C     PROPS(5): qpsi (related to dilation angle)
C     PROPS(6): ten_f (tension limit input)
      E    = PROPS(1)
      NU   = PROPS(2)
	  COHE = PROPS(3)
      KF   = 6.0D0 * COHE * DCOS(FRIC) / (DSQRT(3.0D0) * (3.0D0 - DSIN(FRIC)))
      FRIC = PROPS(4) * 3.14159265453D0 / 180.0D0
      QF   = 6.0D0 * DSIN(FRIC) / (DSQRT(3.0D0) * (3.0D0 - DSIN(FRIC)))
      DILA = PROPS(5) * 3.14159265453D0 / 180.0D0
      QPSI = 6.0D0 * DSIN(DILA) / (DSQRT(3.0D0) * (3.0D0 - DSIN(DILA)))
      TENSION_INPUT = PROPS(6)

C
C     ----- Calculate elastic parameters -----
      G = E / (2.0D0*(1.0D0+NU))
      K = E / (3.0D0*(1.0D0-2.0D0*NU))
C
C     ----- Determine tension limit -----
      IF (QF.EQ.0.0D0) THEN
         TENSION = 0.0D0
      ELSE
         TENSION_MAX = KF / QF
         TENSION = MIN(TENSION_INPUT, TENSION_MAX)
      ENDIF
C
C     ----- Deviatoric stress computation -----
      SM = (STRESS(1) + STRESS(2) + STRESS(3))/3.0D0
      DO I=1,3
         SD(I) = STRESS(I) - SM
      ENDDO
      SD(4) = STRESS(4)
      SD(5) = STRESS(5)
      SD(6) = STRESS(6)
	
C     ELASTIC PREDICTOR	
	  INC_EV = DSTRAN(1) + DSTRAN(2) + DSTRAN(3)
      SM = SM + K * INC_EV;
      SD(1) = SD(1) + 2.0 * G * (DSTRAN(1) - INC_EV * 0.3333333333333)
      SD(2) = SD(2) + 2.0 * G * (DSTRAN(2) - INC_EV * 0.3333333333333)
      SD(3) = SD(3) + 2.0 * G * (DSTRAN(3) - INC_EV * 0.3333333333333)
      SD(4) = SD(4) + G * DSTRAN(4)
      SD(5) = SD(5) + G * DSTRAN(5)
      SD(6) = SD(6) + G * DSTRAN(6)
	  
C
      J2 = 0.5D0*(SD(1)**2 + SD(2)**2 + SD(3)**2) + SD(4)**2 + SD(5)**2 + SD(6)**2
      TAU = SQRT(J2)
      SEQV = TAU * SQRT(3.0D0)
C
C     ----- Drucker-Prager yield function evaluation -----
      DP_FI = TAU + QF*SM - KF
      DP_SIG = SM - TENSION
C
      IPLAS = 0
	  

C     ----- Return mapping algorithm -----
      IF (DP_SIG .LT. 0.0D0) THEN
C        Shear plastic flow
         IF (DP_FI .GT. 0.0D0) THEN
            IPLAS = 1
            D_LAMD = DP_FI/(G + K*QF*QPSI)
            SM = SM - K*QPSI*D_LAMD
            NEWTAU = KF - QF*SM
            RATIO = NEWTAU/TAU
            DO I=1,6
               SD(I) = SD(I)*RATIO
            ENDDO
            SEQV = SEQV*RATIO
            DEPEFF = D_LAMD*SQRT(1.0D0/3.0D0 + (2.0D0/9.0D0)*(QPSI**2))
            STATEV(1) = STATEV(1) + DEPEFF
         ENDIF
      ELSE
C        Check for tension plastic flow
         ALPHAP = SQRT(1.0D0 + QF**2) - QF
         TAUP = KF - QF*TENSION
         DP_HFAI = TAU - TAUP - ALPHAP*DP_SIG
         IF (DP_HFAI .GT. 0.0D0) THEN
C           Shear plastic flow
            IPLAS = 1
            D_LAMD = DP_FI/(G + K*QF*QPSI)
            SM = SM - K*QPSI*D_LAMD
            NEWTAU = KF - QF*SM
            RATIO = NEWTAU/TAU
            DO I=1,6
               SD(I) = SD(I)*RATIO
            ENDDO
            SEQV = SEQV*RATIO
            DEPEFF = D_LAMD*SQRT(1.0D0/3.0D0 + (2.0D0/9.0D0)*(QPSI**2))
            STATEV(1) = STATEV(1) + DEPEFF
         ELSE
C           Tension plastic flow
            IPLAS = 2
            D_LAMD = (SM - TENSION)/K
            SM = TENSION
            DEPEFF = D_LAMD*(1.0D0/3.0D0)*SQRT(2.0D0)
            STATEV(1) = STATEV(1) + DEPEFF
            DO I=1,3
               SD(I) = 0.0D0
            ENDDO
         ENDIF
      ENDIF
C
	  
	  
C     ----- Write updated stresses back -----
      DO I=1,3
         STRESS(I) = SD(I) + SM
      ENDDO
      STRESS(4) = SD(4)
      STRESS(5) = SD(5)
      STRESS(6) = SD(6)
C
C     ----- Elastic tangent stiffness matrix (for simplicity, always elastic here) -----
      DO I=1,NTENS
         DO J=1,NTENS
            DDSDDE(I,J) = 0.0D0
         ENDDO
      ENDDO
      DO I=1,3
         DDSDDE(I,I) = K + 4.0D0*G/3.0D0
         DO J=1,3
            IF (I .NE. J) DDSDDE(I,J) = K - 2.0D0*G/3.0D0
         ENDDO
      ENDDO
      DO I=4,6
         DDSDDE(I,I) = G
      ENDDO

C
      RETURN
      END
