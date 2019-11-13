SUBROUTINE SVD(NM, M, N, A, W, MATU, U, MATV, V, IERR, RV1)
   !
   INTEGER I, J, K, L, M, N, II, I1, KK, K1, LL, L1, MN, NM, ITS, IERR
   REAL A(NM, N), W(N), U(NM, N), V(NM, N), RV1(N)
   !
   REAL C, F, G, H, S, X, Y, Z, SCALE, ANORM
   LOGICAL MATU, MATV
   !
   !     ЭTA ПOДПPOГPAMMA ECTЬ TPAHCЛЯЦИЯ AЛГOЛ-ПPOЦEДУPЫ SVD,
   !     OПУБЛИKOBAHHOЙ ГOЛУБOM И PAЙHШEM B ЖУPHAЛE NUMERISCHE
   !     MATHEMATIK, 14, 403-420(1970), A TAKЖE B KHИГE
   !     HANDBOOK FOR AUTOMATIC COMPUTATION  VOL.II-LINEAR
   !     ALGEBRA, 134-151 (1971).
   !
   !     ЭTA ПOДПPOГPAMMA BЫЧИCЛЯET CИHГУЛЯPHOE PAЗЛOЖEHИE
   !          T
   !     A=USV  ДEЙCTBИTEЛЬHOЙ ПPЯMOУГOЛЬHOЙ MATPИЦЫ A C PAЗME-
   !     PAMИ M И N. ПPИ ЭTOM ИCПOЛЬЗУЮTCЯ ДBУXДИAГOHAЛИЗAЦИЯ
   !     ПOCPEДCTBOM XAУCXOЛДEPOBЫX OTPAЖEHИЙ  И BAPИAHT
   !     QR-AЛГOPИTMA.
   !
   !     HA BXOДE.
   !
   !     NM   CЛEДУET ПOЛOЖИTЬ PABHЫM CTPOЧHOЙ PAЗMEPHOCTИ
   !          ДBУXMEPHЫX MACCИBOB ,ЗAЯBЛEHHOЙ B OПEPATOPE PAЗ-
   !          MEPHOCTИ BЫЗЫBAЮЩEЙ ПPOГPAMMЫ. ЗAMETИM, ЧTO NM
   !          ДOЛЖHO БЫTЬ H  MEHЬШE,ЧEM MAKCИMУM ИЗ M И N.
   !
   !     M    ЧИCЛO CTPOK A (И U).
   !
   !     N    ЧИCЛO CTOЛБЦOB A (И U) И ПOPЯДOK V.
   !
   !     A    COДEPЖИT ПPЯMOУГOЛЬHУЮ BXOДHУЮ MATPИЦУ, ДЛЯ
   !          KOTOPOЙ HAXOДИTCЯ  PAЗЛOЖEHИE.
   !
   !     MATU ДOЛЖEH ИMETЬ ЗHAЧEHИE .TRUE., ECЛИ HУЖHO
   !          BЫЧИCЛЯTЬ MATPИЦУ U ИЗ PAЗЛOЖEHИЯ, И ЗHAЧEHИE
   !          .FALSE. B ПPOTИBHOM CЛУЧAE.
   !
   !     MATV ДOЛЖEH ИMETЬ ЗHAЧEHИE .TRUE., ECЛИ HУЖHO
   !          BЫЧИCЛЯTЬ MATPИЦУ V ИЗ PAЗЛOЖEHИЯ, И ЗHAЧEHИE
   !          .FALSE. B ПPOTИBHOM CЛУЧAE.
   !
   !     HA BЫXOДE.
   !
   !     A    HE ИЗMEHЯETCЯ (ECЛИ HA EE MECTE HE ЗAПИCЫBAЮTCЯ
   !          U ЛИБO V).
   !
   !     W    COДEPЖИT N (HEOTPИЦATEЛЬHЫX) CИHГУЛЯPHЫX ЧИCEЛ
   !          A (ДИAГOHAЛЬHЫX ЭЛEMEHTOB S). OHИ HE УПOPЯДOЧEHЫ.
   !          ECЛИ ПPOИCXOДИT BЫXOД ПO OШИБKE, TO ДЛЯ ЗHAЧEHИЙ
   !          IERR+1, IERR+2,...,N CИHГУ ЛЯPHЫE ЧИCЛA ДOЛЖHЫ
   !          БЫTЬ BEPHЫ.
   !
   !     U    COДEPЖИT MATPИЦУ U (C OPTOГOHAЛЬHЫMИ CTOЛБЦAMИ)
   !          ИЗ PAЗЛOЖEHИЯ, ECЛИ ДЛЯ ПAPAMETPA MATU БЫЛO
   !          ЗAДAHO ЗHAЧEHИE .TRUE. B ПPOTИBHOM CЛУЧAE HA U
   !          ИCПOЛЬЗУETCЯ KAK BPEMEHHЫЙ MACCИB. U TAKЖE MOЖET
   !          COBПAДATЬ C A. ECЛИ ПPOИCXOДИT BЫXOД
   !          ПO OШИБKE, TO CTOЛБЦЫ U, COOTBETCTBУЮЩИE ИHДEKCAM
   !          BEPHЫX CИHГУЛЯPHЫX ЧИCEЛ, ДOЛЖHЫ БЫTЬ TAKЖE BEPHЫ.
   !
   !     V    COДEPЖИT MATPИЦУ V (OPTOГOHAЛЬHУЮ) ИЗ PAЗЛOЖEHИЯ,
   !          ECЛИ ДЛЯ ПAPAMETPA MATV БЫЛO ЗAДAHO ЗHAЧEHИE
   !          .TRUE. B ПPOTИBHOM CЛУЧAE HA V HE ПPOИЗBOДИTCЯ
   !          CCЫЛOK. V TAKЖE MOЖET COBПAДATЬ C  A, ECЛИ U HE
   !          BЫЧИCЛЯETCЯ. ECЛИ ПPOИCXOДИT BЫXOД ПO OШИБKE,
   !          TO CTOЛБЦЫ V, COOTBETCTBУЮЩИE ИHДEKCAM BEPHЫX
   !          CИHГУЛЯPHЫX ЧИCEЛ, ДOЛЖHЫ БЫTЬ TAKЖE BEPHЫ.
   !
   !     IERR PABHO
   !            0,  ECЛИ ПPOИCXOДИT HOPMAЛЬHЫЙ BЫXOД ИЗ ПOДПPOГ-
   !                PAMMЫ,
   !            K,  ECЛИ K-E CИHГУЛЯPHOE ЧИCЛO HE БЫЛO OПPEДE-
   !                ЛEHO ПOCЛE 30 ИTEPAЦИЙ.
   !
   !     RV1  ЭTO MACCИB ПPOMEЖУTOЧHOГO XPAHEHИЯ.
   !
   !     BOПPOCЫ И KOMMEHTAPИИ HУЖHO HAПPABЛЯTЬ ПO AДPECУ
   !     B.S.GARBOW, APPLIED MATEMATICS DIVISION, ARGONNE
   !     NATIONAL LABORATORY
   !
   !     ПOДПPOГPAMMA MOДИФИЦИPOBAHA C ЦEЛЬЮ ИCKЛЮЧИTЬ
   !     ПEPEMEHHYЮ MACHEP
   !
   IERR = 0
   DO I = 1, M
      DO 100 J = 1, N
         U(I, J) = A(I, J)
      100 CONTINUE
   end do
   !
   !     XAУCXOЛДEPOBO ПPИBEДEHИE K ДBУXДИAГOHAЛЬHOЙ ФOPME
   !
   G = 0.0
   SCALE = 0.0
   ANORM = 0.0
   !
   DO I = 1, N
      L = I + 1
      RV1(I) = SCALE * G
      G = 0.0
      S = 0.0
      SCALE = 0.0
      IF(I>M)GO TO 210
      !
      DO K = I, M
      SCALE = SCALE + ABS(U(K, I))
      end do
      IF(SCALE==0.0)GO TO 210
      !
      DO K = I, M
         U(K, I) = U(K, I) / SCALE
         S = S + U(K, I)**2
      end do
      !
      F = U(I, I)
      G = -SIGN(SQRT(S), F)
      H = F * G - S
      U(I, I) = F - G
      IF(I==N)GO TO 190
      !
      DO J = L, N
         S = 0.0
         DO K = I, M
         S = S + U(K, I) * U(K, J)
         end do
         F = S / H
         DO 150 K = I, M
            U(K, J) = U(K, J) + F * U(K, I)
         150    CONTINUE
      end do
      !
      190    DO K = I, M
      U(K, I) = SCALE * U(K, I)
      end do
      210    W(I) = SCALE * G
      G = 0.0
      S = 0.0
      SCALE = 0.0
      IF(I>M.OR.I==N)GO TO 290
      !
      DO K = L, N
      SCALE = SCALE + ABS(U(I, K))
      end do
      !
      IF(SCALE==0.0)GO TO 290
      !
      DO K = L, N
         U(I, K) = U(I, K) / SCALE
         S = S + U(I, K)**2
      end do
      !
      F = U(I, L)
      G = -SIGN(SQRT(S), F)
      H = F * G - S
      U(I, L) = F - G
      !
      DO K = L, N
      RV1(K) = U(I, K) / H
      end do
      !
      IF(I==M)GO TO 270
      !
      DO J = L, M
         S = 0.0
         DO K = L, N
         S = S + U(J, K) * U(I, K)
         end do
         DO 260 K = L, N
            U(J, K) = U(J, K) + S * RV1(K)
         260    CONTINUE
      end do
      !
      270    DO K = L, N
      U(I, K) = SCALE * U(I, K)
      end do
      !
      290    ANORM = AMAX1(ANORM, ABS(W(I)) + ABS(RV1(I)))
   end do
   !
   !     HAKOПЛEHИE ПPABOCTOPOHHИX ПPEOБPAЗOBAHИЙ
   !
   IF(.NOT.MATV)GO TO 410
   !
   !     ДЛЯ I=N C ШAГOM -1 ДO 1 BЫПOЛHИTЬ -
   !
   DO II = 1, N
      I = N + 1 - II
      IF(I==N)GO TO 390
      IF(G==0.0)GO TO 360
      !
      DO J = L, N
         !
         !     ДBOЙHOE ДEЛEHИE OБXOДИT BOЗMOЖHЫЙ MAШИHHЫЙ HYЛЬ
         !
      V(J, I) = (U(I, J) / U(I, L)) / G
      end do
      !
      DO J = L, N
         S = 0.0
         DO K = L, N
         S = S + U(I, K) * V(K, J)
         end do
         DO 350 K = L, N
            V(K, J) = V(K, J) + S * V(K, I)
         350 CONTINUE
      end do
      !
      360    DO J = L, N
         V(I, J) = 0.0
         V(J, I) = 0.0
      end do
      !
      390    V(I, I) = 1.0
      G = RV1(I)
      L = I
   end do
   !
   !     HAKOПЛEHИE ЛEBOCTOPOHHИX ПPEOБPAЗOBAHИЙ
   !
   410 IF(.NOT.MATU)GO TO 510
   !
   !     ДЛЯ I=MIN(N,M) C ШAГOM -1 ДO 1 BЫПOЛHИTЬ-
   !
   MN = N
   IF(M<N)MN = M
   !
   DO II = 1, MN
      I = MN + 1 - II
      L = I + 1
      G = W(I)
      IF(I==N)GO TO 430
      !
      DO J = L, N
      U(I, J) = 0.0
      end do
      !
      430    IF(G==0.0)GO TO 475
      IF(I==MN)GO TO 460
      !
      DO J = L, N
         S = 0.0
         DO K = L, M
         S = S + U(K, I) * U(K, J)
         end do
         !
         !     ДBOЙHOE  ДEЛEHИE OБXOДИT BOЗMOЖHЫЙ MAШИHHЫЙ HYЛЬ
         !
         F = (S / U(I, I)) / G
         !
         DO 450 K = I, M
            U(K, J) = U(K, J) + F * U(K, I)
         450    CONTINUE
      end do

      460    DO J = I, M
      U(J, I) = U(J, I) / G
      end do
      !
      GO TO 490
      !
      475    DO J = I, M
      U(J, I) = 0.0
      end do
      !
      490    U(I, I) = U(I, I) + 1.0
   end do
   !
   !     ДИAГOHAЛИЗAЦИЯ ДBУXДИAГOHAЛЬHOЙ ФOPMЫ ДЛЯ K=N C ШAГOM
   !     -1 ДO 1 BЫПOЛHИTЬ
   !
   510 DO KK = 1, N
      K1 = N - KK
      K = K1 + 1
      ITS = 0
      !
      !     ПPOBEPKA BOЗMOЖHOCTИ PACЩEПЛEHИЯ  ДЛЯ L=K
      !     C ШAГOM -1 ДO 1 BЫПOЛHИTЬ
      !
      520    DO LL = 1, K
         L1 = K - LL
         L = L1 + 1
         IF(ABS(RV1(L)) + ANORM==ANORM)GO TO 565
         !
         !     RV1(1) BCEГДA PABHO HУЛЮ. ПOЭTOMУ BЫXOДA
         !     ЧEPEЗ KOHEЦ ЦИKЛA HE БУДET
         !
         IF(ABS(W(L1)) + ANORM==ANORM)GO TO 540
      end do
      !
      !      ECЛИ L БOЛЬШE, ЧEM 1, TO RV1(L)
      !      ПPИCBAИBAETCЯ HУЛEBOE ЗHAЧEHИE
      !
      540    C = 0.0
      S = 1.0
      !
      DO I = L, K
         F = S * RV1(I)
         RV1(I) = C * RV1(I)
         IF(ABS(F) + ANORM==ANORM)GO TO 565
         G = W(I)
         H = SQRT(F * F + G * G)
         W(I) = H
         C = G / H
         S = -F / H
         IF(.NOT.MATU)GO TO 560
         !
         DO J = 1, M
            Y = U(J, L1)
            Z = U(J, I)
            U(J, L1) = Y * C + Z * S
            U(J, I) = -Y * S + Z * C
         end do
      560    CONTINUE
      end do
      !
      !     ПPOBEPKA CXOДИMOCTИ
      !
      565    Z = W(K)
      IF(L==K)GO TO 650
      !
      !     CДBИГ BЫБИPAETCЯ ИЗ HИЖHEГO УГЛOBOГO
      !     MИHOPA ПOPЯДKA 2
      !
      IF(ITS==30)GO TO 1000
      ITS = ITS + 1
      X = W(L)
      Y = W(K1)
      G = RV1(K1)
      H = RV1(K)
      F = ((Y - Z) * (Y + Z) + (G - H) * (G + H)) / (2.0 * H * Y)
      G = SQRT(F * F + 1.0)
      F = ((X - Z) * (X + Z) + H * (Y / (F + SIGN(G, F)) - H)) / X
      !
      !     CЛEДУЮЩEE QR-ПPEOБPAЗOBAHИE
      !
      C = 1.0
      S = 1.0
      !
      DO I1 = L, K1
         I = I1 + 1
         G = RV1(I)
         Y = W(I)
         H = S * G
         G = C * G
         Z = SQRT(F * F + H * H)
         RV1(I1) = Z
         C = F / Z
         S = H / Z
         F = X * C + G * S
         G = -X * S + G * C
         H = Y * S
         Y = Y * C
         IF(.NOT.MATV)GO TO 575
         !
         DO J = 1, N
            X = V(J, I1)
            Z = V(J, I)
            V(J, I1) = X * C + Z * S
            V(J, I) = -X * S + Z * C
         end do
         !
         575       Z = SQRT(F * F + H * H)
         W(I1) = Z
         !
         !     BPAЩEHИE MOЖET БЫTЬ ПPOИЗBOЛЬHЫM, ECЛИ Z PABHO HУЛЮ
         !
         IF(Z==0.0)GO TO 580
         C = F / Z
         S = H / Z
         580       F = C * G + S * Y
         X = -S * G + C * Y
         IF(.NOT.MATU)GO TO 600
         !
         DO J = 1, M
            Y = U(J, I1)
            Z = U(J, I)
            U(J, I1) = Y * C + Z * S
            U(J, I) = -Y * S + Z * C
         end do
      600    CONTINUE
      end do
      RV1(L) = 0.0
      RV1(K) = F
      W(K) = X
      GO TO 520
      !
      !     CXOДИMOCTЬ
      !
      650    IF(Z>=0.0)GO TO 700
      !
      !     W(K) ДEЛAETCЯ HEOTPИЦATEЛЬHЫM
      !
      W(K) = -Z
      IF(.NOT.MATV)GO TO 700
      !
      DO J = 1, N
      V(J, K) = -V(J, K)
      end do
   700 CONTINUE
   end do
   GO TO 1001
   !
   !     УCTAHOBИTЬ ЗHAЧEHИE ПPИЗHAKA OШИБKИ - ПOCЛE 30
   !     ИTEPAЦИЙ HET CXOMOCTИ K CИHГУЛЯPHOMУ ЧИCЛУ
   !
   1000 IERR = K
   1001 RETURN
END