(* ::Package:: *)

(*The function to get fields in that term*)
GetVertexFields[term_]:=Sort[
Flatten[
Cases[
Level[term,1],
QuantumField[args___]|QuantumField[args___]^n_
]/.{QuantumField[args___]^n_:>Table[QuantumField[args],{i,n}]}
]];
(*Identifier for Vertex Type*)
VT$SS=2;VT$SSS=3;VT$SSSS=4;
VT$SV=11;VT$SSV=12;VT$SVV=21;VT$SSVV=22;
VT$FFS=201;
VT$UUS=2001;
(*Lists for Holding Vertex FRs*)
FR$SS={};FR$SSS={};FR$SSSS={};
FR$SV={};FR$SSV={};FR$SVV={};FR$SSVV={};
FR$FFS={};FR$UUS={};
(* Following Function is really slow when the list is large.
AddingFRs[FRList_,newFR_]:=Block[{checkQ},
checkQ=Cases[FRList,FRVertex[newFR[[1]],args__]];
If[Length[checkQ]\[NotEqual]0,FRList=FRList/.{FRVertex[newFR[[1]],args_]\[RuleDelayed]FRVertex[newFR[[1]],Collect[args+newFR[[2]],{MHL2,MHH2,MHp2,MHA2,M2,CT$Order1,dMHL21,dMHH21,dMHp21,dMHA21,dZe1,dtheta1,dalpha1,_dMf,_dZ},Simplify[#,SW^2+CW^2\[Equal]1]&]]},AppendTo[FRList,FRVertex[newFR[[1]],newFR[[2]]]]];
];
SetAttributes[AddingFRs,HoldFirst];*)
(*Function to get the Vertex Type according to the Fields*)
GetVertexType[Fields_]:=Block[{Types,NB,NF,NV,NS,NG,BFields,VertexType},
Types=FieldType/@Fields;
NF=Count[Types,TypeFermion];
NG=Count[Types,TypeGhost];
NB=Count[Types,TypeBoson];
BFields=Select[Fields,(FieldType[#]==TypeBoson&)];
NV=Count[BFields,QuantumField[___,_LorentzIndex]];
NS=NB-NV;
VertexType=NS+NV*10+NF*100+NG*1000;
VertexType
];
(*Different Type of Vertex, using different function*)
GetFRScalar[LagTerm_,Fields_List]:=FRVertex[{Fields,FunctionalD[I LagTerm,Fields]}];
GetFRSV[LagTerm_,Fields_List]:=Block[{Scalar,Vector,SortedFields,FR},
Scalar=Cases[Fields,QuantumField[args_]];
Vector=Cases[Fields,QuantumField[args_,_LorentzIndex]:>QuantumField[args,LorentzIndex[rho]]];
SortedFields={Scalar[[1]],Vector[[1]]};
FR=FunctionalD[I LagTerm,SortedFields]/.{Pair[_LorentzIndex,Momentum[Subscript[__,__]]]:>1};
FRVertex[{SortedFields,FR}]
];
GetFRSSV[LagTerm_,Fields_List]:=Block[{Scalar,Vector,SortedFields,FR},
Scalar=Cases[Fields,QuantumField[args_]];
Vector=Cases[Fields,QuantumField[args_,_LorentzIndex]:>QuantumField[args,LorentzIndex[rho]]];
SortedFields={Scalar[[1]],Scalar[[2]],Vector[[1]]};
FR=FunctionalD[I LagTerm,SortedFields]/.{Pair[_LorentzIndex, 
        Momentum[Subscript[p, Cases[{Scalar[[1]]},QuantumField[Subscript[f_,___]]:>f][[1]]]]]:>1,Pair[_LorentzIndex, 
        Momentum[Subscript[p, Cases[{Scalar[[2]]},QuantumField[Subscript[f_,___]]:>f][[1]]]]]:>0};
FRVertex[{SortedFields,FR}]
];
GetFRSVV[LagTerm_,Fields_List]:=Block[{Scalar,Vector,SortedFields,FR},
Scalar=Cases[Fields,QuantumField[args_]];
Vector=Cases[Fields,QuantumField[args_,_LorentzIndex]];
SortedFields={Scalar[[1]],Vector[[1]]/.{LorentzIndex[args_]:>LorentzIndex[rho]},Vector[[2]]/.{LorentzIndex[args_]:>LorentzIndex[sigma]}};
FR=FunctionalD[I LagTerm,SortedFields];
FRVertex[{SortedFields,FR}]
];
GetFRSSVV[LagTerm_,Fields_List]:=Block[{Scalar,Vector,SortedFields,FR},
Scalar=Cases[Fields,QuantumField[args_]];
Vector=Cases[Fields,QuantumField[args_,_LorentzIndex]];
SortedFields={Scalar[[1]],Scalar[[2]],Vector[[1]]/.{LorentzIndex[args_]:>LorentzIndex[rho]},Vector[[2]]/.{LorentzIndex[args_]:>LorentzIndex[sigma]}};
FR=FunctionalD[I LagTerm,SortedFields];
FRVertex[{SortedFields,FR}]
];
GetFRFFS[LagTerm_,Fields_List]:=Block[{Scalar,Quarks,Leptons,Quark,Lepton,AntiQuark,AntiLepton,SortedFields,FR},
Scalar=Cases[Fields,QuantumField[args_]];
Quarks=Cases[Fields,QuantumField[args_,_SUNIndex,_SUNIndex]:>args];
Leptons=Cases[Fields,QuantumField[args_,_SUNIndex]:>args];
Quark=Select[Quarks,FreeQ[#,_HCbar]&];
Lepton=Select[Leptons,FreeQ[#,_HCbar]&];
AntiQuark=Select[Quarks,!FreeQ[#,_HCbar]&];
AntiLepton=Select[Leptons,!FreeQ[#,_HCbar]&];
If[Length[Quark]==1,
SortedFields={QuantumField[Quark[[1]],{},{i1,c1}],QuantumField[AntiQuark[[1]],{},{i2,c2}],Scalar[[1]]};,
SortedFields={QuantumField[Lepton[[1]],{},{i1}],QuantumField[AntiLepton[[1]],{},{i2}],Scalar[[1]]}
];
FR=FunctionalD[I LagTerm,SortedFields];
FRVertex[{SortedFields,ExpandSums[Expand[FR/.{SUNIndex[f_] :> f, SUNDelta -> IndexDelta}]]}]
];
GetFRUUS[LagTerm_,Fields_List]:=Block[{Scalar,Ghosts,Ghost,AntiGhost,SortedFields,FR},
Scalar=Select[Fields,FieldType[#]==TypeBoson&];
Ghosts=Cases[Select[Fields,FieldType[#]==TypeGhost&],QuantumField[args_]];
Ghost=Select[Ghosts,FreeQ[#,_HCbar]&];
AntiGhost=Select[Ghosts,!FreeQ[#,_HCbar]&];
SortedFields={Scalar[[1]],Ghost[[1]],AntiGhost[[1]]};
FR=FunctionalD[I LagTerm,SortedFields];
FRVertex[{SortedFields,FR}]
];
(*Get FR for only one term*)
GetFROneTerm[term_]:=Block[{Fields,VertexType,tmp},
Fields=GetVertexFields[term];
VertexType=GetVertexType[Fields];
Switch[VertexType,
VT$SS,tmp=GetFRScalar[term,Fields];FR$SS={FR$SS,tmp};,
VT$SSS,tmp=GetFRScalar[term,Fields];FR$SSS={FR$SSS,tmp};,
VT$SSSS,tmp=GetFRScalar[term,Fields];FR$SSSS={FR$SSSS,tmp};,
VT$SV,tmp=GetFRSV[term,Fields];FR$SV={FR$SV,tmp};,
VT$SSV,tmp=GetFRSSV[term,Fields];FR$SSV={FR$SSV,tmp};,
VT$SVV,tmp=GetFRSVV[term,Fields];FR$SVV={FR$SVV,tmp};,
VT$SSVV,tmp=GetFRSSVV[term,Fields];FR$SSVV={FR$SSVV,tmp};,
VT$FFS,tmp=GetFRFFS[term,Fields];FR$FFS={FR$FFS,tmp};,
VT$UUS,tmp=GetFRUUS[term,Fields];FR$UUS={FR$UUS,tmp};,
__,tmp=False
];
tmp
]
(*Small Function to expand the Lagrangian*)
ExpandToList[terms_]:=Block[{tmp},tmp=Expand[terms];If[Head[tmp]===Plus,tmp=List@@tmp];tmp];
(**)
GetFRsFromLagList[Lag_List]:=Block[{LagTree,LagCTs,Len},
LagTree=Flatten[ExpandToList/@Lag[[1]]];
LagCTs=Flatten[ExpandToList/@Lag[[2]]];
$Calculated=0;
Len=Length[LagTree];
Print["Calculating Feynman Rules for LO: ",Dynamic[$Calculated],"/",Len];
For[$Calculated=1,$Calculated<=Len,$Calculated++,
GetFROneTerm[LagTree[[$Calculated]]];
];
$Calculated=0;
Len=Length[LagCTs];
Print["Calculating Feynman Rules for CTs: ",Dynamic[$Calculated],"/",Len];
For[$Calculated=1,$Calculated<=Len,$Calculated++,
GetFROneTerm[CT$Order1 LagCTs[[$Calculated]]];
];
FR$SS=Flatten[FR$SS];FR$SSS=Flatten[FR$SSS];FR$SSSS=Flatten[FR$SSSS];
FR$SV=Flatten[FR$SV];FR$SSV=Flatten[FR$SSV];FR$SVV=Flatten[FR$SVV];FR$SSVV=Flatten[FR$SSVV];
FR$FFS=Flatten[FR$FFS];FR$UUS=Flatten[FR$UUS];
];
(*Arrange the List calculated from GetFRsFromLagList *)
CombineFRs[FR_List,Vertex_]:=Block[{All,LOs,CTs},
All=Plus@@Cases[FR,FRVertex[{Vertex,fr_}]:>fr];
LOs=Collect[All/.{CT$Order1->0},{MHL2,MHH2,MHA2,MHp2,M2},Simplify[#,SW^2+CW^2==1]&];
CTs=Collect[Coefficient[All,CT$Order1],{dMHL21,dMHH21,dMHA21,dMHp21,dZe1,dbeta1,dalpha1,_dMf,_dZ},Simplify[#,CW^2+SW^2==1]&];
FRVertex[{Vertex,LOs,CTs}]
];
RearrangeFRList[FR_]:=Block[{Vertices,tmp,NVertices,i},
Vertices=Union[Cases[FR,FRVertex[{ver_,fr_}]:>ver]];
NVertices=Length[Vertices];
tmp={};
$Calculated=0;
Print["Handling Vertices: ",Dynamic[$Calculated],"/",NVertices];
For[i=1,i<=NVertices,i++,
$Calculated=i;
tmp={tmp,CombineFRs[FR,Vertices[[i]]]};
];
FR=Flatten[tmp];
]
SetAttributes[RearrangeFRList,HoldFirst];
(*The function to extract the Feynman Rules*)
FRwithCT[Lag_List,Fields_List]:=Block[{vertex,vertexCT},
vertex=Collect[Plus@@((FunctionalD[I #,Fields]/.{QuantumField[args1___].QuantumField[args2___]:>0,QuantumField[args___]:>0})&/@Lag[[1]]),{MHL2,MHH2,MHp2},Simplify[#,CW^2+SW^2==1]&];
(*vertex=Collect[(FunctionalD[I Lag/.{r1->0},Fields])/.{QuantumField[___]:>0, 0 . 0 ->0},{MHL2,MHH2,MHA2,MHp2,M2},Simplify[#,CW^2+SW^2==1]&];*)
vertexCT=Collect[Plus@@((FunctionalD[I #,Fields]/.{QuantumField[args1___].QuantumField[args2___]:>0,QuantumField[args___]:>0})&/@Lag[[2]]),{dMHL21,dMHH21,dMHp21,dZe1,dtheta1,dalpha1,_dMf,_dZ},Simplify[#,CW^2+SW^2==1]&];
(*vertexCT=Collect[(FunctionalD[D[I Lag,r1],Fields])/.{QuantumField[___]:>0, 0 . 0->0},{dMHL21,dMHH21,dMHA21,dMHp21,dM21,dMWsq1,dMZsq1,dZe1,dalpha1,dbeta1,_dMf,_dZ},Simplify[#,CW^2+SW^2==1]&];*)
If[PossibleZeroQ[vertexCT]&&PossibleZeroQ[vertex],FRVertexNULL[{Fields,vertex,vertexCT}],FRVertex[{Fields,vertex,vertexCT}]]
]
(*First For the Pure Scalar Couplings, 3 or 4 points*)
ScalarCouplings[Lag_List,Fields_List,n_]:=Block[{vertex,PossibleN,tmp,fyrule,fields,i},
If[n!=3&&n!=4&&n!=2,vertex={},
vertex=Select[DeleteDuplicates[Sort/@Tuples[Fields,n]],Total[FieldCharge[#]] == 0 &];];
PossibleN=Length[vertex];
Print["Generating ",n,"-Scalar Vertex: ",Dynamic[calculated],"/",PossibleN];
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
SSVVCouplings[Lag_List,SField_List,VField_List,index1_,index2_]:=Block[{vertex,vertexS,vertexV,PossibleN,i,Fields,tmp,fyrule},
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
SVVCouplings[Lag_List,SField_List,VField_List,index1_,index2_]:=Block[{vertex,vertexS,vertexV,PossibleN,i,Fields,tmp,fyrule},
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
SSVCouplings[Lag_List,SField_List,VField_List,index_]:=Block[{vertex,vertexS,vertexV,PossibleN,i,Fields,tmp,fyrule},
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
	fyrule={fyrule,tmp/.{FourVector[Subscript[p,Fields[[1]]],index]:>1,FourVector[Subscript[p,Fields[[2]]],index]:>0}/.{Pair[_LorentzIndex, 
        Momentum[Subscript[p, Cases[{Fields[[1]]},Subscript[f_,___]:>f][[1]]]]]:>1,Pair[_LorentzIndex, 
        Momentum[Subscript[p, Cases[{Fields[[2]]},Subscript[f_,___]:>f][[1]]]]]:>0}};
];
fyrule=Flatten[fyrule];
{Select[fyrule,Head[#]==FRVertex&],Select[fyrule,Head[#]==FRVertexNULL&]}
]

SVCouplings[Lag_List,SField_List,VField_List,index_]:=Block[{vertex,vertexS,vertexV,PossibleN,i,Fields,tmp,fyrule},
vertexS=SField;
vertexV=VField;
vertex=Select[Flatten/@Tuples[{vertexS,vertexV}],Total[FieldCharge[#]] == 0 &];
PossibleN=Length[vertex];
Print["Generating SV Vertex: ",Dynamic[calculated],"/",PossibleN];
fyrule={};
For[i=1,i<=PossibleN,i++,
	calculated=i;
	Fields=vertex[[i]];
	tmp=FRwithCT[Lag,{QuantumField[Fields[[1]]],QuantumField[Fields[[2]],{index}]}];
	fyrule={fyrule,tmp/.{Pair[_LorentzIndex,Momentum[Subscript[__,__]]]:>1}};
];
fyrule=Flatten[fyrule];
{Select[fyrule,Head[#]==FRVertex&],Select[fyrule,Head[#]==FRVertexNULL&]}
]


(*Yukawa Coupling *)
FFSCouplings[Lag_List,FField_List,SField_List,flavorindex_List,colorindex___List]:=Block[{vertex,vertexF,vertexS,PossibleN,i,Fields,tmp,fyrule,indexes},
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


(*Ghost Coupling S-U-U *)
UUSCouplings[Lag_List,SField_List,GhostField_List]:=Block[{vertexU,vertex,PossibleN,fyrule,i,Fields,tmp},
vertexU=DeleteDuplicates[Tuples[{HCbar/@GhostField,GhostField}]];
vertex=Select[Flatten/@Tuples[{SField,vertexU}],Total[FieldCharge[#]] == 0&];
PossibleN=Length[vertex];
Print["Generating UUS Vertex: ", Dynamic[calculated],"/",PossibleN];
fyrule={};
For[i=1,i<=PossibleN,i++,
	calculated=i;
	Fields=vertex[[i]];
	tmp=FRwithCT[Lag,QuantumField/@Fields];
	fyrule={fyrule,tmp};
];
fyrule=Flatten[fyrule];
{Select[fyrule,Head[#]==FRVertex&],Select[fyrule,Head[#]==FRVertexNULL&]}
]
