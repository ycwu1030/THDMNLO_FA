(* ::Package:: *)

(*Utility handling Expansion and Momentum replacement of the Lagrangian*)
MomentumInsert={QuantumField[FCPartialD[LorentzIndex[mu_]],f_,args___]:>-I FourVector[Subscript[p,f],mu]QuantumField[f,args],
QuantumField[FCPartialD[LorentzIndex[mu_]],Subscript[f_,r_],args___]:>-I FourVector[Subscript[p,f],mu]QuantumField[Subscript[f,r],args],
RightPartialD[LorentzIndex[mu_]]:>0
};
PreHandling[Lag_]:=ExpandPartialD[Lag]/.MomentumInsert;


(*The function to extract the Feynman Rules*)
FRwithCT[Lag_,Fields_List]:=Block[{vertex,vertexCT},
vertex=Collect[(FunctionalD[I Lag/.{r1->0},Fields])/.{QuantumField[___]:>0},{MHL2,MHH2,MHA2,MHp2,M2},Simplify];
vertexCT=Collect[(FunctionalD[D[I Lag,r1],Fields])/.{QuantumField[___]:>0},{dMHL21,dMHH21,dMHA21,dMHp21,dM21,dMWsq1,dMZsq1,dZe1,dalpha1,dbeta1,_dMf,_dZ},Simplify];
If[PossibleZeroQ[vertexCT],FRVertexNULL[{Fields,vertex,vertexCT}],FRVertex[{Fields,vertex,vertexCT}]]
]
(*First For the Pure Scalar Couplings, 3 or 4 points*)
ScalarCouplings[Lag_,Fields_List,n_]:=Block[{vertex,PossibleN,tmp},
If[n!=3||n!=4,{},
vertex=Select[Total[FieldCharge[#]]==0&][DeleteDuplicates[Sort/@Tuples[Fields,n]]];
PossibleN=Length[vertex];
tmp=FRwithCT[Lag,#]&/@vertex;
tmp]
];


(*Scalar Involved Gauge Coupling*)
(*SSVV*)
SSVVCouplings[Lag_,SField_List,VField_List]:=Block[{vertex,PossibleN,i,tmp,fyrule},
vertex=Select[Total[FieldCharge[#]]==0&][DeleteDuplicates[Sort/@Tuples[{SField,SField,VField,VField}]]];
PossibleN=Length[vertex];
Print["Generating SSVV Vertex: ",Dynamic[calculated],"/",PossibleN];
fyrule={};
For[i=1,i<=PossibleN,i++,
	calculated=i;
	tmp=FRwithCT[I Lag,vertex[[i]]];
	fyrule={fyrule,tmp};
];
fyrule=Flatten[fyrule];
{Select[Head[#]==FRVertex&][fyrule],Select[Head[#]==FRVertexNULL&][fyrule]}
]
