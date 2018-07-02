(* ::Package:: *)

(*Utility handling Expansion and Momentum replacement of the Lagrangian*)
MomentumInsert={QuantumField[FCPartialD[LorentzIndex[mu_]],f_,args___]:>-I FourVector[Subscript[p,f],mu]QuantumField[f,args],
QuantumField[FCPartialD[LorentzIndex[mu_]],Subscript[f_,r_],args___]:>-I FourVector[Subscript[p,f],mu]QuantumField[Subscript[f,r],args],
RightPartialD[LorentzIndex[mu_]]:>0
};
(*PreHandling[Lag_]:=ExpandPartialD[Lag]/.MomentumInsert;*)

PrepareRenormalizedLag[Lag_] := Normal[Series[ExpandPartialD[Renormalization[Lag]], {r1, 0, 1}]]/.MomentumInsert;


(*The function to extract the Feynman Rules*)
FRwithCT[Lag_,Fields_List]:=Block[{vertex,vertexCT},
vertex=Collect[(FunctionalD[I Lag/.{r1->0},Fields])/.{QuantumField[___]:>0, 0 . 0 ->0},{MHL2,MHH2,MHA2,MHp2,M2},Simplify[#,CW^2+SW^2==1]&];
vertexCT=Collect[(FunctionalD[D[I Lag,r1],Fields])/.{QuantumField[___]:>0, 0 . 0->0},{dMHL21,dMHH21,dMHA21,dMHp21,dM21,dMWsq1,dMZsq1,dZe1,dalpha1,dbeta1,_dMf,_dZ},Simplify[#,CW^2+SW^2==1]&];
If[PossibleZeroQ[vertexCT],FRVertexNULL[{Fields,vertex,vertexCT}],FRVertex[{Fields,vertex,vertexCT}]]
]
(*First For the Pure Scalar Couplings, 3 or 4 points*)
ScalarCouplings[Lag_,Fields_List,n_]:=Block[{vertex,PossibleN,tmp,fyrule,fields,i},
If[n!=3&&n!=4,vertex={},
vertex=Select[DeleteDuplicates[Sort/@Tuples[Fields,n]],Total[FieldCharge[#]] == 0 &];];
PossibleN=Length[vertex];
Print["Generating ",n,"-Scalar Vertex",Dynamic[calculated],"/",PossibleN];
fyrule={};
For[i=1,i<=PossibleN,i++,
  calculated=i;
  fields=vertex[[i]];
  tmp = FRwithCT[Lag,QuantumField/@fields];
  fyrule={fyrule,tmp};
];
fyrule=Flatten[fyrule];
{Select[fyrule,Head[#]==FRVertex&],Select[fyrule,Head[#]==FRVertexNULL&]}
];


(*Scalar Involved Gauge Coupling*)
(*SSVV g_{mu3 mu4}*)
SSVVCouplings[Lag_,SField_List,VField_List,index1_,index2_]:=Block[{vertex,vertexS,vertexV,PossibleN,i,Fields,tmp,fyrule},
vertexS=DeleteDuplicates[Sort/@Tuples[{SField,SField}]];
vertexV=DeleteDuplicates[Sort/@Tuples[{VField,VField}]];
vertex=Select[Flatten/@Tuples[{vertexS,vertexV}],Total[FieldCharge[#]] == 0 &];
(*vertex=Select[Total[FieldCharge[#]]==0&][DeleteDuplicates[Sort/@Tuples[{SField,SField,VField,VField}]]];*)
PossibleN=Length[vertex];
Print["Generating SSVV Vertex: ",Dynamic[calculated],"/",PossibleN];
fyrule={};
For[i=1,i<=PossibleN,i++,
	calculated=i;
	Fields=vertex[[i]];
	tmp=FRwithCT[Lag,{QuantumField[Fields[[1]]],QuantumField[Fields[[2]]],QuantumField[Fields[[3]],{index1}],QuantumField[Fields[[4]],{index2}]}];
	fyrule={fyrule,tmp};
];
fyrule=Flatten[fyrule];
{Select[fyrule,Head[#]==FRVertex&],Select[fyrule,Head[#]==FRVertexNULL&]}
]

(*SVV g_{mu2 mu3}*)
SVVCouplings[Lag_,SField_List,VField_List,index1_,index2_]:=Block[{vertex,vertexS,vertexV,PossibleN,i,Fields,tmp,fyrule},
vertexS=SField;
vertexV=DeleteDuplicates[Sort/@Tuples[{VField,VField}]];
vertex=Select[Flatten/@Tuples[{vertexS,vertexV}],Total[FieldCharge[#]] == 0 &];
PossibleN=Length[vertex];
Print["Generating SVV Vertex: ",Dynamic[calculated],"/",PossibleN];
fyrule={};
For[i=1,i<=PossibleN,i++,
	calculated=i;
	Fields=vertex[[i]];
	tmp=FRwithCT[Lag,{QuantumField[Fields[[1]]],QuantumField[Fields[[2]],{index1}],QuantumField[Fields[[3]],{index2}]}];
	fyrule={fyrule,tmp};
];
fyrule=Flatten[fyrule];
{Select[fyrule,Head[#]==FRVertex&],Select[fyrule,Head[#]==FRVertexNULL&]}
]

(*SSV  (p_1-p2)_mu3*)
SSVCouplings[Lag_,SField_List,VField_List,index_]:=Block[{vertex,vertexS,vertexV,PossibleN,i,Fields,tmp,fyrule},
vertexS=DeleteDuplicates[Sort/@Tuples[{SField,SField}]];
vertexV=VField;
vertex=Select[Flatten/@Tuples[{vertexS,vertexV}],Total[FieldCharge[#]] == 0 &];
PossibleN=Length[vertex];
Print["Generating SSV Vertex: ",Dynamic[calculated],"/",PossibleN];
fyrule={};
For[i=1,i<=PossibleN,i++,
	calculated=i;
	Fields=vertex[[i]];
	tmp=FRwithCT[Lag,{QuantumField[Fields[[1]]],QuantumField[Fields[[2]]],QuantumField[Fields[[3]],{index}]}];
	fyrule={fyrule,tmp/.{FourVector[Subscript[p,Fields[[1]]],index]:>1,FourVector[Subscript[p,Fields[[2]]],index]:>0}};
];
fyrule=Flatten[fyrule];
{Select[fyrule,Head[#]==FRVertex&],Select[fyrule,Head[#]==FRVertexNULL&]}
]


(*Yukawa Coupling *)
FFSCouplings[Lag_,FField_List,SField_List,flavorindex_List,colorindex___List]:=Block[{vertex,vertexF,vertexS,PossibleN,i,Fields,tmp,fyrule,indexes},
vertexF=DeleteDuplicates[Sort/@Tuples[{FField,HCbar/@FField}]];
vertexS=SField;
vertex=Select[Flatten/@Tuples[{vertexF,vertexS}],Total[FieldCharge[#]] == 0&];
PossibleN=Length[vertex];
Print["Generating FFS Vertex: ", Dynamic[calculated],"/",PossibleN];
fyrule={};
indexes=MapThread[List,{flavorindex,colorindex}];
For[i=1,i<=PossibleN,i++,
	calculated=i;
	Fields=vertex[[i]];
	tmp=FRwithCT[Lag,{QuantumField[Fields[[1]],{},indexes[[1]]],QuantumField[Fields[[2]],{},indexes[[2]]],QuantumField[Fields[[3]]]}];
	fyrule={fyrule,tmp};
];
fyrule=Flatten[fyrule];
fyrule=Cases[fyrule,F_[{{f1_,f2_,f3_},fy0_,fy1_}]:>F[{{f1,f2,f3},ExpandSums[Expand[fy0/.{SUNIndex[f_] :> f, SUNDelta -> IndexDelta}]],ExpandSums[Expand[fy1/.{SUNIndex[f_] :> f, SUNDelta -> IndexDelta}]]}]];
{Select[fyrule,Head[#]==FRVertex&],Select[fyrule,Head[#]==FRVertexNULL&]}
]
