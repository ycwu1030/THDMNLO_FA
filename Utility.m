(* ::Package:: *)

(*Handling the Sum over generation*)
ExpandSums[x_, ___] := x /; FreeQ[x, SumOver];
ExpandSums[a_. IndexDelta[i_, j_] SumOver[i_, _], h___] := 
  ExpandSums[a /. i -> j, h];
ExpandSums[a_. IndexDelta[j_, i_] SumOver[i_, _], h___] := 
  ExpandSums[a /. i -> j, h];
ExpandSums[other_, h___] := ExpandSums[#, h] & /@ other;


(*The Chiral Projector*)
PL/:PL^n_Integer:=PL;
PL/:PL PR:=0;
PR/:PR^n_Integer:=PR;
PR/:PR PL:=0;


(*CKM Matrix*)
UL/:SumOver[k_,NF]UL[type_,i_,k_]ULC[type_,k_,j_]:=IndexDelta[i,j];
UL/:SumOver[k_,NF]UL[type_,k_,i_]ULC[type_,j_,k_]:=IndexDelta[i,j];
UR/:SumOver[k_,NF]UR[type_,i_,k_]URC[type_,k_,j_]:=IndexDelta[i,j];
UR/:SumOver[k_,NF]UR[type_,k_,i_]URC[type_,j_,k_]:=IndexDelta[i,j];

UL/:SumOver[k_,NF]UL[3,i_,k_]ULC[4,k_,j_]:=VCKM[i,j];
UL/:SumOver[k_,NF]UL[4,i_,k_]ULC[3,k_,j_]:=VCKMC[i,j];
