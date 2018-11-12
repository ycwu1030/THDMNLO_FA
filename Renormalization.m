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
HCbar/:RenormalizedFermion[HCbar[f_],flavor_,c___]:=PR*(Times@@(RenormalizationInfoFL[HCbar[f],flavor,c][[1;;2]])/.{dZfL[argc___]:>dZfLC[argc]})+PL*(Times@@(RenormalizationInfoFR[HCbar[f],flavor,c][[1;;2]])/.{dZfR[argc___]:>dZfRC[argc]});
(*RenormalizedFL[f_,flavor_,c___]:=Times@@RenormalizationInfoFL[f,flavor,c];
RenormalizedFR[f_,flavor_,c___]:=Times@@RenormalizationInfoFR[f,flavor,c];*)


(*Ghost Field Renormalization*)
RenormalizedGhost[f_,mu___] := (RenormalizationInfo[f,mu][[1]].RenormalizationInfo[f,mu][[2]])[[RenormalizationInfo[f,mu][[3]]]];


RenormalizedField[f_,index___]:=Which[FieldType[f]==TypeFermion,
RenormalizedFermion[f,index],
FieldType[f]==TypeBoson,
RenormalizedBoson[f,index],
FieldType[f]==TypeGhost,
RenormalizedGhost[f,index]
];


Renormalization[term_]:=term; (*If no special settings, it will not be renormalized*)
Renormalization[term1_+term2_]:=Renormalization[term1]+Renormalization[term2];
Renormalization[coeff_ term_]:=Renormalization[coeff] Renormalization[term];
Renormalization[coeff_/term_]:=Renormalization[coeff]/Renormalization[term];
Renormalization[term_^n_]:=Renormalization[term]^n;
Renormalization[f_[args___]]:=f@@(Renormalization/@{args});
QuantumField/: Renormalization[QuantumField[FCPartialD[args1___],FCPartialD[args2___],f_,index___]]:=RenormalizedField@@(Flatten[Join[{f},{index}]])/.{QuantumField[fR_,indexR___]:>QuantumField[FCPartialD[args1],FCPartialD[args2],fR,indexR]};
QuantumField/: Renormalization[QuantumField[FCPartialD[args___],f_,index___]]:=RenormalizedField@@(Flatten[Join[{f},{index}]])/.{QuantumField[fR_,indexR___]:>QuantumField[FCPartialD[args],fR,indexR]};
QuantumField/: Renormalization[QuantumField[f_,args___]]:=RenormalizedField@@(Flatten[Join[{f},{args}]]);


(*Utility handling Expansion and Momentum replacement of the Lagrangian*)
MomentumInsert={QuantumField[FCPartialD[LorentzIndex[mu_]],FCPartialD[LorentzIndex[nu_]],f_Symbol,args___]:>(-I FourVector[Subscript[p,f],mu])(-I FourVector[Subscript[p,f],nu])QuantumField[f,args],
QuantumField[FCPartialD[LorentzIndex[mu_]],FCPartialD[LorentzIndex[nu_]],Subscript[f_,r_],args___]:>(-I FourVector[Subscript[p,f],mu])(-I FourVector[Subscript[p,f],nu])QuantumField[Subscript[f,r],args],
QuantumField[FCPartialD[LorentzIndex[mu_]],f_Symbol,args___]:>-I FourVector[Subscript[p,f],mu]QuantumField[f,args],
QuantumField[FCPartialD[LorentzIndex[mu_]],Subscript[f_,r_],args___]:>-I FourVector[Subscript[p,f],mu]QuantumField[Subscript[f,r],args],
RightPartialD[LorentzIndex[mu_]]:>0
};
(*PreHandling[Lag_]:=ExpandPartialD[Lag]/.MomentumInsert;*)
$Calculated=0;
PrepareRenormalizedLag[Lag_] := Block[{Ltemp,Tree,CTs,Term,Lens,LTree,LCTs,i},
Ltemp=Expand[Lag];
If[Head[Ltemp]===Plus,Ltemp=List@@Ltemp];
Lens=Length[Ltemp];
Tree={};
CTs={};
Print["Calculating: ",Dynamic[$Calculated],"/",Lens];
For[i=1,i<=Lens,i++,
$Calculated=i;
Term=Normal[Series[Renormalization[Ltemp[[i]]],{r1,0,1}]];
Tree={Tree,Term/.{r1->0}/.MomentumInsert};
CTs={CTs,Coefficient[Term,r1]/.MomentumInsert};
];
LTree=Flatten[Tree];
LCTs=Flatten[CTs];
{LTree,LCTs}
]
