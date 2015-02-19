PROGRAM BISECT
      IMPLICIT NOTE
      INTEGER K, KMAX
      REAL A, B, X,Y EPS, F
      PRINT*, 'INPUT A, B, EPS, KMAX'
      READ*, A, B, EPS, KMAX
10      FORMAT('THE INPUT DATA ARE'/&
      6X, 'A=', F6.2, 3X, 'B=', F6.2, 3X, 'EPS=', F10.6, 3X, 'KMAX= ',
      I2)
      PRINT 20
20    FORMAT(/'THE RESULTS ARE'/7X, 'K', 6X, 'X', 8X, 'F(X)', 7X, 'A',
7X, 'B')
      K = 1
      X = 0.5 * (A*B)
      Y = F(X)
      DO WHILE (K<= KMAX .AND. (B-A) > EPS)
      IF(Y<0) THEN
          A=X
      ELSE
          B=X
      ENDIF
      X = 0.5 * (A+B)
      Y = F(X)
30    FORMAT (5X, I3, 2F10.6, 2F9.4)
      PRINT 30, K, X,Y, A,B
      K=K+1
      END DO
      IF(K>KMAX) PRINT*, 'NO CONVERGENCE'
      END

      REAL FUNCTION F(X)
        REAL X
        F= -1.0/X-LOG(X) + 2.0
      END
      
