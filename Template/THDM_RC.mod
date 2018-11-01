
(********************************************************************************)
(**  Standard Model Renormalization Constants Copy from SM.mod @Apr 2016 **)
(* The following definitions of renormalization constants
   are for the on-shell renormalization of the Standard Model in
   the scheme of A. Denner, Fortschr. d. Physik, 41 (1993) 4.

   The renormalization constants are not directly used by
   FeynArts, and hence do not restrict the generation of diagrams
   and amplitudes in any way. *)


Clear[RenConst]

RenConst[ dMf1[type_, j1_] ] := MassRC[F[type, {j1}]]

RenConst[ dZfL1[type_, j1_, j2_] ] :=
  FieldRC[F[type, {j1}], F[type, {j2}]][[1]]

RenConst[ dZfR1[type_, j1_, j2_] ] :=
  FieldRC[F[type, {j1}], F[type, {j2}]][[2]]

If[ dCKM1[] =!= 0,
RenConst[dCKM1[j1_, j2_]] := 1/4 Sum[
  (dZfL1[3, j1, gn] - Conjugate[dZfL1[3, gn, j1]]) CKM[gn, j2] -
  CKM[j1, gn] (dZfL1[4, gn, j2] - Conjugate[dZfL1[4, j2, gn]]), {gn, 3} ]
]

RenConst[ dMZsq1 ] := MassRC[V[2]]

RenConst[ dMWsq1 ] := MassRC[V[3]]

RenConst[ dZAA1 ] := FieldRC[V[1]]

RenConst[ dZAZ1 ] := FieldRC[V[1], V[2]]

RenConst[ dZZA1 ] := FieldRC[V[2], V[1]]

RenConst[ dZZZ1 ] := FieldRC[V[2]]

RenConst[ dZW1 ] := FieldRC[V[3]]

RenConst[ dSW1 ] := CW^2/SW/2 (dMZsq1/MZ^2 - dMWsq1/MW^2)

RenConst[ dZe1 ] := -1/2 (dZAA1 + SW/CW dZZA1)

RenConst[ dUW1 ] := FieldRC[U[3]] + dZW1/2

RenConst[ dUAA1 ] := FieldRC[U[1]] + dZAA1/2

RenConst[ dUAZ1 ] := FieldRC[U[1], U[2]]/2 + dZAZ1/2

RenConst[ dUZA1 ] := FieldRC[U[2], U[1]]/2

RenConst[ dUZZ1 ] := FieldRC[U[2]] + dZZZ1/2

(* Scalar Part Renormalization Constants *)
(* Added By Yongcheng Wu @Apr 2016      *)
(* The Field RC for Scalar part and RC of alpha beta are adapted from hep-ph/0408364 *)
(* Be Careful be the RC of beta, and off-dia term in Scalar RC matrix *)

RenConst[ dTh1 ] := -TadpoleRC[S[1]]

RenConst[ dTHH1 ] := -TadpoleRC[S[2]]

RenConst[ dTdeltaP1 ] := -dTHH1 SBA + dTh1 CBA

RenConst[ dTdeltaM1 ] := dTHH1 CBA + dTh1 SBA

RenConst[ dTab1 ] := SB (dTHH1 CA - dTh1 SA)

RenConst[ dMh0sq1 ] := MassRC[S[1]]

RenConst[ dMHHsq1 ] := MassRC[S[2]]

RenConst[ dMA0sq1 ] := MassRC[S[3]]

RenConst[ dMHpsq1 ] := MassRC[S[5]]

RenConst[ dZG0G01 ] := FieldRC[S[4]]

(************
RenConst[ dZG0A01 ] := FieldRC[ S[4], S[3] ] + 2 dTdelta1/(vev MA0^2)

RenConst[ dZA0G01 ] := FieldRC[ S[3], S[4] ] - 2 dTdelta1/(vev MA0^2)
*************)

RenConst[ dCA1 ] := - 1/(2 MA0^2) (SelfEnergy[S[3]->S[4],MA0] - SelfEnergy[S[3]->S[4],0])

RenConst[ dBeta1 ] := -1/(2 MA0^2) * ( SelfEnergy[ S[3]->S[4], MA0] + SelfEnergy[ S[3]->S[4],0 ] ) + dTdeltaP1/(MA0^2 vev)

RenConst[ dZA0A01 ] := FieldRC[ S[3] ]

RenConst[ dZGpGp1 ] := FieldRC[S[6]]

(************
RenConst[ dZGpHp1 ] := FieldRC[ -S[6], -S[5] ] + 2 dTdelta1/(vev MHp^2)  (** Be Careful about this, need to be checked further **)

RenConst[ dZHpGp1 ] := FieldRC[ -S[5], -S[6] ] - 2 dTdelta1/(vev MHp^2)
*************)

(* Choose one of following formular for dCHp1 *)
(* RenConst[ dCHp1 ] := 1/(MHp^2) ReTilde[SelfEnergy[S[5]->S[6],0]] - dTdeltaP1/(MHp^2 vev) + dBeta1 *)

RenConst[ dCHp1 ] := -1/(MHp^2) ReTilde[SelfEnergy[S[5]->S[6],MHp^2]] + dTdeltaP1/(MHp^2 vev) - dBeta1

RenConst[ dZHpHp1 ] := FieldRC[S[5]]

RenConst[ dZhh1 ] := FieldRC[S[1]]

(*** 
RenConst[ dZhH01 ] := FieldRC[ S[1], S[2] ] - 2 (S2A dTdelta1)/(vev S2B (Mh0^2 - MHH^2))

RenConst[ dZH0h1 ] := FieldRC[ S[2], S[1] ] - 2 (S2A dTdelta1)/(vev S2B (MHH^2 - Mh0^2))
***)

RenConst[ dCh1 ] := 1/4 * (FieldRC[ S[1], S[2] ] + FieldRC[ S[2], S[1] ])

RenConst[ dAlpha1 ] := 1/4 * (FieldRC[ S[2], S[1] ] - FieldRC[ S[1], S[2] ]) - S2A dTdeltaP1/(vev S2B (MHH^2 - Mh0^2))

RenConst[ dZH0H01 ] := FieldRC[S[2]]

RenConst[ dBA1 ] := dBeta1 - dAlpha1

RenConst[ dAB1 ] := dAlpha1 + dBeta1
