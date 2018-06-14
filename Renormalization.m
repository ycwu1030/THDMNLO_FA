(* ::Package:: *)





(*Definition for field renormalization constant*)
FieldNormalization[f_List]:={{1+1/2 dZ[ToExpression[ToString[f[[1]]]<>ToString[f[[1]]]]],1/2 dZ[ToExpression[ToString[f[[1]]]<>ToString[f[[2]]]]]},{1/2 dZ[ToExpression[ToString[f[[2]]]<>ToString[f[[1]]]]],1+1/2 dZ[ToExpression[ToString[f[[2]]]<>ToString[f[[2]]]]]}};
FieldNormalization[f_]:={1+1/2 dZ[ToExpression[ToString[f]<>ToString[f]]]};

Renormalized[f__] := (RenormalizationInfo[f][[1]].RenormalizationInfo[f][[2]])[[RenormalizationInfo[f][[3]]]];
