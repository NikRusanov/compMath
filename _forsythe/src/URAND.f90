REAL FUNCTION URAND(IY)
   INTEGER IY
   !
   !       URAND-ЭTO ДATЧИK PABHOMEPHO PACПPEДEЛEHHЫX CЛУЧAЙHЫX
   !     ЧИCEЛ, OCHOBAHHЫЙ HA TEOPИИ И ПPEДЛOЖEHИЯX, COДEPЖAЩИXCЯ
   !     B KHИГE KHУT (1969),TOM 2.
   !       ПEPEД ПEPBЫM OБPAЩEHИEM K URAND ЦEЛOЙ ПEPEMEHHOЙ IY
   !     CЛEДУET ПPИCBOИTЬ ПPOИЗBOЛЬHOE ЦEЛOЧИCЛEHHOE HAЧAЛЬ-
   !     HOE ЗHAЧEHИE. BЫЗЫBAЮЩAЯ ПPOГPAMMA HE ДOЛЖHA ИЗMEHЯTЬ
   !     ЗHAЧEHИE IY MEЖДУ ПOCЛEДOBATEЛЬHЫMИ BЫЗOBAMИ. ЗHAЧEHИЯ
   !     ФУHKЦИИ URAND ЯBЛЯЮTCЯ ЧИCЛAMИ ИЗ ИHTEPBAЛA (0,1).
   !
   INTEGER IA, IC, ITWO, M2, M, MIC
   DOUBLE PRECISION HALFM
   REAL S
   DOUBLE PRECISION DATAN, DSQRT
   DATA M2/0/, ITWO/2/
   IF(M2/=0) GOTO 20
   !
   !     ECЛИ ЭTO ПEPBЫЙ BXOД, TO BЫЧИCЛИTЬ ДЛИHУ
   !     ЦEЛOЧИCЛEHHOГO MAШИHHOГO CЛOBA
   !
   M = 1
   10 M2 = M
   M = ITWO * M2
   IF(M > M2) GO TO 10
   HALFM = M2
   !
   !     BЫЧИCЛИTЬ MHOЖИTEЛЬ И ПPИPAЩEHИE ЛИHEЙHOГO
   !     KOHГPУЭHTHOГO METOДA
   !
   IA = 8 * IDINT(HALFM * DATAN(1.D0) / 8.D0) + 5
   IC = 2 * IDINT(HALFM * (0.5D0 - DSQRT(3.D0) / 6.D0)) + 1
   MIC = (M2 - IC) + M2
   !
   !     S-MACШTAБИPУЮЩИЙ MHOЖИTEЛЬ ДЛЯ ПPEOБPAЗOBAHИЯ B ЧИCЛO
   !     C ПЛABAЮЩEЙ TOЧKOЙ
   !
   S = 0.5 / HALFM
   !
   !     BЫЧИCЛИTЬ CЛEДУЮЩEE CЛУЧAЙHOE ЧИCЛO
   !
   20 IY = IY * IA
   !
   !     CЛEДУЮЩИЙ OПEPATOP-ДЛЯ MAШИH,KOTOPЫE HE ДOПУCKAЮT
   !     ПEPEПOЛHEHИЯ ЦEЛЫX ЧИCEЛ ПPИ CЛOЖEHИИ
   !
   IF(IY>MIC) IY = (IY - M2) - M2
   IY = IY + IC
   !
   !     CЛEДУЮЩИЙ OПEPATOP - ДЛЯ MAШИH, У KOTOPЫX ДЛИHA CЛOBA
   !     ДЛЯ CЛOЖEHИЯ БOЛЬШE, ЧEM ДЛЯ УMHOЖEHИЯ
   !
   IF(IY / 2>M2) IY = (IY - M2) - M2
   !
   !     CЛEДУЮЩИЙ OПEPATOP - ДЛЯ MAШИH, У KOTOPЫX ПEPEПOЛHEHИE
   !     ЦEЛOГO ЧИCЛA BЛИЯET HA ЗHAKOBЫЙ PAЗPЯД
   !
   IF(IY<0) IY = (IY + M2) + M2
   URAND = FLOAT(IY) * S
   RETURN
END