(* ::Package:: *)

(*Definition for Boson field renormalization constant*)
FieldNormalization[f_List]:={{1+1/2 dZ[ToExpression[ToString[f[[1]]]<>ToString[f[[1]]]]],1/2 dZ[ToExpression[ToString[f[[1]]]<>ToString[f[[2]]]]]},{1/2 dZ[ToExpression[ToString[f[[2]]]<>ToString[f[[1]]]]],1+1/2 dZ[ToExpression[ToString[f[[2]]]<>ToString[f[[2]]]]]}};
FieldNormalization[f_]:={1+1/2 dZ[ToExpression[ToString[f]<>ToString[f]]]};

RenormalizedBoson[f_,mu___] := (RenormalizationInfo[f,mu][[1]].RenormalizationInfo[f,mu][[2]])[[RenormalizationInfo[f,mu][[3]]]];


(*Definition for Chrial fermion field renormalization constant*)
FieldNormalizationFL[category_,flavor_]:=Block[{internal},
internal=Unique["jFL"];
{SumOver[internal,NF]*IndexDelta[flavor,internal]+SumOver[internal,NF]*dZfL[category,flavor,internal]/2,internal}]

FieldNormalizationFR[category_,flavor_]:=Block[{internal},
internal=Unique["jFR"];
{SumOver[internal,NF]*IndexDelta[flavor,internal]+SumOver[internal,NF]*dZfR[category,flavor,internal]/2,internal}]


RenormalizedFermion[f_,flavor_,c___]:=PL*Times@@RenormalizationInfoFL[f,flavor,c]+PR*Times@@RenormalizationInfoFR[f,flavor,c];
HCbar/:Renormalized[HCbar[f_],flavor_,c___]:=PR*Times@@RenormalizationInfoFL[HCbar[f],flavor,c]+PL*Times@@RenormalizationInfoFR[HCbar[f],flavor,c];
(*RenormalizedFL[f_,flavor_,c___]:=Times@@RenormalizationInfoFL[f,flavor,c];
RenormalizedFR[f_,flavor_,c___]:=Times@@RenormalizationInfoFR[f,flavor,c];*)
