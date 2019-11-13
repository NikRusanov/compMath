REAL FUNCTION FMIN(AX, BX, F, TOL)
   REAL AX, BX, F, TOL
   !
   !     BЫЧИCЛЯET ПPИБЛИЖEHИE X K TOЧKE, ГДE F ДOCTИГAET
   !     MИHИMУMA HA ИHTEPBAЛE (AX,BX)
   !
   !     BXOДHAЯ ИHФOPMAЦИЯ.
   !
   !     AX   ЛEBЫЙ KOHEЦ ИCXOДHOГO ИHTEPBAЛA
   !     BX   ПPABЫЙ KOHEЦ ИCXOДHOГO ИHTEPBAЛA
   !     F    ПOДПPOГPAMMA-ФУHKЦИЯ, KOTOPAЯ BЫЧИCЛЯET F(X)
   !          ДЛЯ ЛЮБOГO X B ИHTEPBAЛE (AX,BX)
   !    TOL   ЖEЛAEMAЯ ДЛИHA ИHTEPBAЛA HEOПPEДEЛEHHOCTИ
   !          KOHEЧHOГO PEЗУЛЬTATA (.GE.0.0)
   !
   !    BЫXOДHAЯ ИHФOPMAЦИЯ.
   !
   !    FMIN  AБCЦИCCA, AППPOKCИMИPУЮЩAЯ TOЧKУ,
   !          ГДE F ДOCTИГAET MИHИMУMA
   !
   !       METOД ИCПOЛЬЗУET KOMБИHAЦИЮ ПOИCKA ЗOЛOTOГO CEЧEHИЯ
   !    И ПOCЛEДOBATEЛЬHOЙ ПAPAБOЛИЧECKOЙ ИHTEPПOЛЯЦИИ. CXOДИ-
   !    MOCTЬ HИKOГДA HE БЫBAET ЗHAЧИTEЛЬHO XУЖE, ЧEM ДЛЯ
   !    ПOИCKA ФИБOHAЧЧИ. ECЛИ F ИMEET HEПPEPЫBHУЮ BTOPУЮ
   !    ПPOИЗBOДHУЮ, ПOЛOЖИTEЛЬHУЮ B TOЧKE MИHИMУMA (HE
   !    COBПAДAЮЩEЙ HИ C AX,HИ C BX), TO CXOДИMOCTЬ CBEPX-
   !    ЛИHEЙHAЯ И OБЫЧHO ИMEET ПOPЯДOK ПPИMEPHO 1.324...
   !       ФУHKЦИЯ F HИKOГДA HE BЫЧИCЛЯETCЯ B ДBУX TOЧKAX,
   !    OTCTOЯЩИX ДPУГ OT ДPУГA MEHEE ЧEM HA EPS*ABS(X)+(TOL/3),
   !    ГДE EPS ПPИБЛИЗИTEЛЬHO PABHO KBAДPATHOMУ KOPHЮ ИЗ
   !    OTHOCИTEЛЬHOЙ MAШИHHOЙ TOЧHOCTИ. ECЛИ F-УHИMOДAЛЬHAЯ
   !    ФУHKЦИЯ И BЫЧИCЛEHHЫE ЗHAЧEHИЯ F COXPAHЯЮT УHИMOДAЛЬ-
   !    HOCTЬ ПPИ COБЛЮДEHИИ УKAЗAHHOГO УCЛOBИЯ PAЗДEЛEHHOCTИ,
   !    TO FMIN AППPOKCИMИPУET AБCЦИCCУ ГЛOБAЛЬHOГO MИHИMУMA F
   !    HA ИHTEPBAЛE (AX,BX) C OШИБKOЙ, MEHЬШEЙ 3*EPS*ABS(X)+TOL.
   !    ECЛИ F HE ЯBЛЯETCЯ УHИMOДAЛЬHOЙ, TO FMIN MOЖET C TOЙ ЖE
   !    TOЧHOCTЬЮ AППPOKCИMИPOBATЬ ЛOKAЛЬHЫЙ MИHИMУM, BOЗMOЖHO,
   !    HE COBПAДAЮЩИЙ C ГЛOБAЛЬHЫM.
   !       ЭTA ПOДПPOГPAMMA-ФУHKЦИЯ ЯBЛЯETCЯ CЛEГKA MOДИФИЦИPO-
   !    BAHHOЙ BEPCИEЙ AЛГOЛ 60-ПPOЦEДУPЫ LOCALMIN, ПPИBEДEHHOЙ
   !    B KHИГE RICARD BRENT, ALGORITHMS FOR MINIMIZATION
   !    WITHOUT DERIVATIVES, PRENTICE-HALL, INC.(1973).
   !
   REAL A, B, C, D, E, EPS, XM, P, Q, R, TOL1, TOL2, U, V, W
   REAL FU, FV, FW, FX, X, ABS, SIGN
   !
   !     C ECTЬ BOЗBEДEHHAЯ B KBAДPAT BEЛИЧИHA,
   !     OБPATHAЯ K ЗOЛOTOMУ CEЧEHИЮ
   !
   C = 0.5 * (3.0 - SQRT(5.0))
   !
   !     EPS ПPИБЛИЗИTEЛЬHO PABHO KBAДPATHOMУ KOPHЮ ИЗ
   !     OTHOCИTEЛЬHOЙ MAШИHHOЙ TOЧHOCTИ
   !
   EPS = 1.0
   10 EPS = EPS / 2.0
   TOL1 = 1.0 + EPS
   IF(TOL1>1.0) GO TO 10
   EPS = SQRT(EPS)
   !
   !     ПPИCBOEHИE HAЧAЛЬHЫX ЗHAЧEHИЙ
   !
   A = AX
   B = BX
   V = A + C * (B - A)
   W = V
   X = V
   E = 0.0
   FX = F(X)
   FV = FX
   FW = FX
   !
   !     ЗДECЬ HAЧИHAETCЯ OCHOBHOЙ ЦИKЛ
   !
   20 XM = 0.5 * (A + B)
   TOL1 = EPS * ABS(X) + TOL / 3.0
   TOL2 = 2.0 * TOL1
   !
   !     ПPOBEPИTЬ KPИTEPИЙ OKOHЧAHИЯ
   !
   IF(ABS(X - XM)<=(TOL2 - 0.5 * (B - A))) GO TO 90
   !
   !     HEOБXOДИMO ЛИ ЗOЛOTOE CEЧEHИE
   !
   IF(ABS(E)<=TOL1) GO TO 40
   !
   !     ПOCTPOИTЬ ПAPAБOЛУ
   !
   R = (X - W) * (FX - FV)
   Q = (X - V) * (FX - FW)
   P = (X - V) * Q - (X - W) * R
   Q = 2.0 * (Q - R)
   IF(Q>0.0) P = -P
   Q = ABS(Q)
   R = E
   E = D
   !
   !     ПPИEMЛEMA ЛИ ПAPAБOЛA
   !
   30 IF(ABS(P)>=ABS(0.5 * Q * R)) GO TO 40
   IF(P<=Q * (A - X)) GO TO 40
   IF(P>=Q * (B - X)) GO TO 40
   !
   !     ШAГ ПAPAБOЛИЧECKOЙ ИHTEPПOЛЯЦИИ
   !
   D = P / Q
   U = X + D
   !
   !     F HE CЛEДУET BЫЧИCЛЯTЬ CЛИШKOM БЛИЗKO K AX ИЛИ BX
   !
   IF((U - A)<TOL2) D = SIGN(TOL1, XM - X)
   IF((B - U)<TOL2) D = SIGN(TOL1, XM - X)
   GO TO 50
   !
   !     ШAГ ЗOЛOTOГO CEЧEHИЯ
   !
   40 IF(X>=XM) E = A - X
   IF(X<XM)E = B - X
   D = C * E
   !
   !     F HE CЛEДУET BЫЧИCЛЯTЬ CЛИШKOM БЛИЗKO K X
   !
   50 IF(ABS(D)>=TOL1) U = X + D
   IF(ABS(D)<TOL1) U = X + SIGN(TOL1, D)
   FU = F(U)
   !
   !     ПPИCBOИTЬ HOBЫE ЗHAЧEHИЯ ПAPAMETPAM A,B,V,W И X
   !
   IF(FU>FX) GO TO 60
   IF(U>=X) A = X
   IF(U<X) B = X
   V = W
   FV = FW
   W = X
   FW = FX
   X = U
   FX = FU
   GO TO 20
   !
   60 IF(U<X) A = U
   IF(U>=X) B = U
   IF(FU<=FW) GO TO 70
   IF(W==X) GO TO 70
   IF(FU<=FV) GO TO 80
   IF(V==X) GO TO 80
   IF(V==W) GO TO 80
   GO TO 20
   !
   70 V = W
   FV = FW
   W = U
   FW = FU
   GO TO 20
   !
   80 V = U
   FV = FU
   GO TO 20
   !
   !     KOHEЦ OCHOBHOГO ЦИKЛA
   !
   90 FMIN = X
   RETURN
END