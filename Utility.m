(* ::Package:: *)





ExpandSums[x_, ___] := x /; FreeQ[x, SumOver];
ExpandSums[a_. IndexDelta[i_, j_] SumOver[i_, _], h___] := 
  ExpandSums[a /. i -> j, h];
ExpandSums[a_. IndexDelta[j_, i_] SumOver[i_, _], h___] := 
  ExpandSums[a /. i -> j, h];
ExpandSums[other_, h___] := ExpandSums[#, h] & /@ other;
