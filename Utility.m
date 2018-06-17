(* ::Package:: *)

(*Handling the Sum over generation*)
ExpandSums[x_, ___] := x /; FreeQ[x, SumOver];
ExpandSums[a_. IndexDelta[i_, j_] SumOver[i_, _], h___] := 
  ExpandSums[a /. i -> j, h];
ExpandSums[a_. IndexDelta[j_, i_] SumOver[i_, _], h___] := 
  ExpandSums[a /. i -> j, h];
ExpandSums[other_, h___] := ExpandSums[#, h] & /@ other;


(*The Chiral Projector*)
PL/:PL^2:=PL;
PL/:PL PR:=0;
PR/:PR^2:=PR;
PR/:PR PL:=0;


(*CKM Matrix*)
UU/:UU[i_,j_]UUC[i_,k_]:=IndexDelta[j,k];
