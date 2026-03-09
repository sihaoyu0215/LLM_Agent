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
C     --- Material parameter mapping from PROPS ---
C     PROPS(1): G0         - Initial shear modulus
C     PROPS(2): NU         - Poisson's ratio
C     PROPS(3): MC         - Critical state stress ratio
C     PROPS(4): E0         - Initial void ratio
C     PROPS(5): EGAMMA     - CSL intercept at p_atm
C     PROPS(6): LAMBDA_C   - CSL slope
C     PROPS(7): XI         - CSL exponent
C     PROPS(8): D0         - Dilatancy parameter
C     PROPS(9): ND         - Phase transformation parameter
C     PROPS(10): H1        - Hardening parameter
C     PROPS(11): H2        - Hardening parameter
C     PROPS(12): NP        - Peak stress parameter
C     PROPS(13): PC0       - Initial consolidation pressure
C
      DOUBLE PRECISION G0,NU,MC,E0,EGAMMA,LAMBDA_C,XI,D0,ND,H1,H2,NP,PC0
      DOUBLE PRECISION PATM
      PARAMETER (PATM=101000.0D0) ! Atmospheric pressure in Pa
	  DOUBLE PRECISION H, e, PEEQ
C     --- Local variables ---
      DOUBLE PRECISION SIGTR(6), STRAIN_INC(6), SIGDEV(6), STRESS_OLD(6)
      DOUBLE PRECISION p, q, s_norm, PHI, PSI, DLAM, DLAM_TRY
      DOUBLE PRECISION R(6), N(6), DENOM, G, K, h_r, e_c, psi_s
      DOUBLE PRECISION Md, Mp, Kp, G_CUR, K_CUR
      DOUBLE PRECISION dH, dPEEQ, dE
      DOUBLE PRECISION A(6,6), Iden(6)
	  DOUBLE PRECISION H_OLD
	  DOUBLE PRECISION DSTATE, REL_STRESS_CHANGE, DSIG_NORM, STRESS_NORM
	  DOUBLE PRECISION DDSDDE_ELASTIC(6,6)
      INTEGER I,J, iter, max_iter
      LOGICAL PLASTIC
	  
C     --- Useful constants ---
      DOUBLE PRECISION ONE, TWO, THREE, SIX, ZERO, SQRT23, SMALL
      PARAMETER (ONE=1.D0, TWO=2.D0, THREE=3.D0, SIX=6.D0, ZERO=0.D0)
      PARAMETER (SQRT23=1.154700538D0, SMALL=1.D-6)
C
      G0       = PROPS(1)
      NU       = PROPS(2)
      MC       = PROPS(3)
      E0       = PROPS(4)
      EGAMMA   = PROPS(5)
      LAMBDA_C = PROPS(6)
      XI       = PROPS(7)
      D_0      = PROPS(8)
      ND       = PROPS(9)
      H1       = PROPS(10)
      H2       = PROPS(11)
      NP       = PROPS(12)
      PC0      = PROPS(13)
C
C     --- State variable mapping ---
C     STATEV(1): H      - Hardening variable
C     STATEV(2): e      - Current void ratio
C     STATEV(3): PEEQ   - Equivalent plastic strain
	  
      H    = STATEV(1)
      e    = STATEV(2)
      PEEQ = STATEV(3)
	  
      max_iter = 20

C     --- Initialize identity tensor in Voigt notation ---
      Iden(1)=ONE
      Iden(2)=ONE
      Iden(3)=ONE
      Iden(4)=ZERO
      Iden(5)=ZERO
      Iden(6)=ZERO
C
C     --- Compute current elastic moduli as a function of p ---
C     (Use trial mean stress for first estimate)
C
C     --- Compute trial elastic predictor ---
      DO I=1,6
         SIGTR(I) = -STRESS(I)
         STRAIN_INC(I) = - DSTRAN(I)
      END DO

C     --- Elastic moduli (G, K) depend on current mean stress ---
C     Compute trial mean stress p_tr for moduli update
      CALL STRESS_INVARIANTS(SIGTR, p, q, SIGDEV, s_norm)
      IF (p .LT. SMALL) p = SMALL
	  
	  IF (STATEV(2) .LT. 1.0E-6) THEN
	      e = E0
          H = 0.0001
	  END IF






C*****************************************************************
      G_CUR = G0 * ((2.973D0-E0)**2/(1.D0+E0)) * SQRT(p * PATM)
      K_CUR = TWO*(ONE+NU)/THREE/(ONE-TWO*NU) * G_CUR

C     --- Elastic stiffness matrix ---
      CALL ELASTIC_TANGENT(G_CUR, K_CUR, DDSDDE)
C
C     --- Elastic trial step ---
C
      DO I=1,6
         SIGTR(I) = SIGTR(I)
         DO J=1,6
            SIGTR(I) = SIGTR(I) + DDSDDE(I,J) * STRAIN_INC(J)
         END DO
      END DO
C
      CALL STRESS_INVARIANTS(SIGTR, p, q, SIGDEV, s_norm)
      IF (p .LT. SMALL) p = SMALL
	  





C*****************************************************************      
C     --- Yield function ---
      PHI = q - H*p
C
C     --- Check yield condition ---
      IF (PHI .LE. 0) THEN
C        --- Elastic step ---
         G_CUR = G0 * ((2.973D0-E0)**2/(1.D0+E0)) * SQRT(p * PATM)
         K_CUR = TWO*(ONE+NU)/THREE/(ONE-TWO*NU) * G_CUR
		 
		 
		 IF (H.LT. 1.0e-6) H = 0.0001
		 
		 STATEV(1) = H


      ELSE

C        --- Plastic step: Return mapping ---
         iter = 0
		 
		 DO I=1,6
            STRESS_OLD(I) = SIGTR(I)
         END DO
	     
	     e = e - (1 + E0) * (STRAIN_INC(1) + STRAIN_INC(2) + STRAIN_INC(3));
	     
         DO WHILE (iter .LT. max_iter)
            H_OLD = H

            iter = iter + 1
C           --- Update moduli at current p ---
            IF (p .LT. SMALL) p = SMALL
            G_CUR = G0 * ((2.973D0-E0)**2/(1.D0+E0)) * SQRT(p * PATM)
            K_CUR = TWO*(ONE+NU)/THREE/(ONE-TWO*NU) * G_CUR
            CALL ELASTIC_TANGENT(G_CUR, K_CUR, DDSDDE)
C      
C           --- State parameter update ---
            e_c = EGAMMA - LAMBDA_C*(p/PATM)**XI
            psi_s = e - e_c
            Md = MC * EXP(ND*psi_s)
            Mp = MC * EXP(-NP*psi_s)
            h_r  = H1 - H2 * E0
	   
C           Plastic modulus         
            Kp = h_r*G_CUR*(Mp/H - ONE)
C            IF(Kp .LT. SMALL) Kp = 1.0D6
C      
C           --- Stress invariants and deviator ---
            CALL STRESS_INVARIANTS(SIGTR, p, q, SIGDEV, s_norm)
            IF (p .LT. SMALL) p = SMALL
C      
C           --- Flow direction r_ij ---
            DO I=1,6
               R(I) = D_0*(Md - q/p)/THREE*Iden(I) + (THREE/TWO)*SIGDEV(I)/q
            END DO
C      
C           --- Loading direction n_ij ---
            DO I=1,6
               N(I) = -H/THREE*Iden(I) + (THREE/TWO)*SIGDEV(I)/q
            END DO
	   	 
C           --- Consistency parameter denominator (A) ---
            DENOM = ZERO
            DO I=1,6
               DO J=1,6
                  DENOM = DENOM + N(I)*DDSDDE(I,J)*R(J)
               END DO
            END DO
            DENOM = DENOM + Kp
   
C           --- Compute DLAM increment ---
            DLAM_TRY = PHI / DENOM
	   
C           --- Stress correction ---
            DO I=1,6
               DO J=1,6
                  SIGTR(I) = SIGTR(I) - DLAM_TRY * DDSDDE(I,J) * R(J)
               END DO
            END DO
            
C           --- Hardening variable update ---         
            H = H + DLAM_TRY * Kp / p
	   
C           --- Update p, q, etc. for next iteration ---
            CALL STRESS_INVARIANTS(SIGTR, p, q, SIGDEV, s_norm)
            IF (p .LT. SMALL) p = SMALL
            IF (q .LT. SMALL) q = SMALL
            PHI = q - H * p
			

            DSTATE = ABS(H - H_OLD) / ABS(H)
			DSIG_NORM = 0.D0
            STRESS_NORM = 0.D0
            DO I=1,6
                DSIG_NORM = DSIG_NORM + (SIGTR(I) - STRESS_OLD(I))**2
                STRESS_NORM = STRESS_NORM + SIGTR(I)**2
            END DO
            DSIG_NORM = SQRT(DSIG_NORM)
            STRESS_NORM = SQRT(STRESS_NORM)
            IF (STRESS_NORM .LT. SMALL) STRESS_NORM = SMALL
            REL_STRESS_CHANGE = DSIG_NORM / STRESS_NORM
			DO I=1,6
                STRESS_OLD(I) = SIGTR(I)
            END DO
	   	 
		 
C           --- Convergence check ---
            IF (PHI .LT. 1E-8 .AND. REL_STRESS_CHANGE .LT. 1E-6 .AND. DSTATE .LT. 1E-6) THEN 
               EXIT
            END IF
	   
         END DO  ! END PLASTIC ITERATION
	   
C        --- Update state variables ---
         dPEEQ = DLAM_TRY * SQRT23
         PEEQ = PEEQ + dPEEQ
		 
		 CALL ELASTIC_TANGENT(G_CUR, K_CUR, DDSDDE_ELASTIC)
		 
		 CALL CONSISTENT_TANGENT(DDSDDE_ELASTIC, R, N, Kp, DDSDDE)
		 
	  END IF  ! END PHI

C     --- Final stress update ---
      DO I=1,6
         STRESS(I) = -SIGTR(I)
      END DO

C     --- Store state variables ---
      STATEV(1) = H
      STATEV(2) = e
      STATEV(3) = PEEQ
C
C     --- End of UMAT ---
C
C     --- Subroutines ---
C

      END











C     --- Compute stress invariants: mean p, q, deviatoric stress ---
      SUBROUTINE STRESS_INVARIANTS(SIG, p, q, SDEV, snorm)
      DOUBLE PRECISION SIG(6), SDEV(6), p, q, snorm
      DOUBLE PRECISION smean
	  PARAMETER (ONE=1.D0, TWO=2.D0, THREE=3.D0, SIX=6.D0, ZERO=0.D0)
      INTEGER I
      smean = (SIG(1)+SIG(2)+SIG(3))/THREE
      p = smean
      SDEV(1) = SIG(1) - smean
      SDEV(2) = SIG(2) - smean
      SDEV(3) = SIG(3) - smean
      SDEV(4) = SIG(4)
      SDEV(5) = SIG(5)
      SDEV(6) = SIG(6)
      snorm = SDEV(1)**2 + SDEV(2)**2 + SDEV(3)**2
      snorm = snorm + TWO*(SDEV(4)**2 + SDEV(5)**2 + SDEV(6)**2)
      snorm = MAX(snorm, 0.D0)
      q = SQRT(1.5D0*snorm)
      RETURN
      END
C
C     --- Elastic tangent operator (6x6) ---
      SUBROUTINE ELASTIC_TANGENT(G, K, C)
      DOUBLE PRECISION G, K, C(6,6)
      DOUBLE PRECISION LAMBDA, MU
      PARAMETER (ONE=1.D0, TWO=2.D0, THREE=3.D0, SIX=6.D0, ZERO=0.D0)
      INTEGER I,J
      LAMBDA = K - TWO*G/THREE
      MU = G
      DO I=1,6
         DO J=1,6
            C(I,J) = 0.D0
         END DO
      END DO
      DO I=1,3
         DO J=1,3
            C(I,J) = LAMBDA
         END DO
         C(I,I) = C(I,I) + TWO*MU
      END DO
      DO I=4,6
         C(I,I) = MU
      END DO
      RETURN
      END
	  
C     --- Consistent tangent stiffness matrix ---
      SUBROUTINE CONSISTENT_TANGENT(DDSDDE_ELASTIC, R, N, Kp, DDSDDE)
      DOUBLE PRECISION DDSDDE_ELASTIC(6,6), R(6), N(6), Kp, DDSDDE(6,6)
      DOUBLE PRECISION DENOM_CONSISTENT, TEMP_VEC1(6), TEMP_VEC2(6)
      INTEGER I, J, K
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
C
C     --- (N : D^e : R + Kp) ---
      DENOM_CONSISTENT = ZERO
      DO I=1,6
         DO J=1,6
            DENOM_CONSISTENT = DENOM_CONSISTENT + N(I)*DDSDDE_ELASTIC(I,J)*R(J)
         END DO
      END DO
      DENOM_CONSISTENT = DENOM_CONSISTENT + Kp
C
C     --- temp_vec1 = D^e : R, temp_vec2 = N : D^e ---
      DO I=1,6
         TEMP_VEC1(I) = 0.D0
         DO K=1,6
            TEMP_VEC1(I) = TEMP_VEC1(I) + DDSDDE_ELASTIC(I,K)*R(K)
         END DO
      END DO
C
      DO J=1,6
         TEMP_VEC2(J) = 0.D0
         DO K=1,6
            TEMP_VEC2(J) = TEMP_VEC2(J) + N(K)*DDSDDE_ELASTIC(K,J)
         END DO
      END DO
C
C     --- D^ep = D^e - ( (D^e : R) ⊗ (N : D^e) ) / (N : D^e : R + Kp) ---
      DO I=1,6
         DO J=1,6
            DDSDDE(I,J) = DDSDDE_ELASTIC(I,J) - 
     &           (TEMP_VEC1(I) * TEMP_VEC2(J)) / DENOM_CONSISTENT
         END DO
      END DO
C
      RETURN
      END