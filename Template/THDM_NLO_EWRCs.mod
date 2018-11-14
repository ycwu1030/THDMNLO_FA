
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

(*RenConst[ dSW1 ] := CW^2/SW/2 (dMZsq1/MZ^2 - dMWsq1/MW^2)*)

RenConst[ dZe1 ] := -1/2 (dZAA1 + SW/CW dZZA1)

RenConst[ dUW1 ] := FieldRC[U[3]] + dZW1/2

RenConst[ dUAA1 ] := FieldRC[U[1]] + dZAA1/2

RenConst[ dUAZ1 ] := FieldRC[U[1], U[2]]/2 + dZAZ1/2

RenConst[ dUZA1 ] := FieldRC[U[2], U[1]]/2

RenConst[ dUZZ1 ] := FieldRC[U[2]] + dZZZ1/2
