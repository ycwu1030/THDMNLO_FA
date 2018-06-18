(* ::Package:: *)

(*Definition for Boson field renormalization constant*)
FieldNormalization[f_List]:={{1+1/2 r1 dZ[ToExpression[ToString[f[[1]]]<>ToString[f[[1]]]]],1/2 r1 dZ[ToExpression[ToString[f[[1]]]<>ToString[f[[2]]]]]},{1/2 r1 dZ[ToExpression[ToString[f[[2]]]<>ToString[f[[1]]]]],1+1/2 r1 dZ[ToExpression[ToString[f[[2]]]<>ToString[f[[2]]]]]}};
FieldNormalization[f_]:={1+1/2 r1 dZ[ToExpression[ToString[f]<>ToString[f]]]};

RenormalizedBoson[f_,mu___] := (RenormalizationInfo[f,mu][[1]].RenormalizationInfo[f,mu][[2]])[[RenormalizationInfo[f,mu][[3]]]];


(*Definition for Chrial fermion field renormalization constant*)
FieldNormalizationFL[category_,flavor_]:=Block[{internal},
internal=Unique["jFL"];
{SumOver[internal,NF]*IndexDelta[flavor,internal]+SumOver[internal,NF]*r1 dZfL[category,flavor,internal]/2,internal}]

FieldNormalizationFR[category_,flavor_]:=Block[{internal},
internal=Unique["jFR"];
{SumOver[internal,NF]*IndexDelta[flavor,internal]+SumOver[internal,NF]*r1 dZfR[category,flavor,internal]/2,internal}]


RenormalizedFermion[f_,flavor_,c___]:=PL*Times@@(RenormalizationInfoFL[f,flavor,c][[1;;2]])+PR*Times@@(RenormalizationInfoFR[f,flavor,c][[1;;2]]);
HCbar/:Renormalized[HCbar[f_],flavor_,c___]:=PR*Times@@(RenormalizationInfoFL[HCbar[f],flavor,c][[1;;2]])+PL*Times@@(RenormalizationInfoFR[HCbar[f],flavor,c][[1;;2]]);
(*RenormalizedFL[f_,flavor_,c___]:=Times@@RenormalizationInfoFL[f,flavor,c];
RenormalizedFR[f_,flavor_,c___]:=Times@@RenormalizationInfoFR[f,flavor,c];*)


RenormalizedField[f_,index___]:=Which[FieldType[f]==TypeFermion,RenormalizedFermion[f,index],FieldType[f]==TypeBoson,RenormalizedBoson[f,index]];


Renormalization[term_]:=term; (*If no special settings, it will not be renormalized*)
Renormalization[term1_+term2_]:=Renormalization[term1]+Renormalization[term2];
Renormalization[coeff_ term_]:=Renormalization[coeff] Renormalization[term];
Renormalization[coeff_/term_]:=Renormalization[coeff]/Renormalization[term];
Renormalization[term_^n_]:=Renormalization[term]^n;
Renormalization[f_[args___]]:=f@@(Renormalization/@{args});
QuantumField/: Renormalization[QuantumField[f_,args___]]:=RenormalizedField@@(Flatten[Join[{f},{args}]]);
