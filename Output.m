(* ::Package:: *)

(*Functions that can be used to output the results to FeynArts model file format*)


(*The SM & 2HDM Field Code*)
FieldCode={QuantumField[Subscript[f_,__],args___]:>QuantumField[f,args],
QuantumField[HL]->S[1],
QuantumField[HH]->S[2],
QuantumField[HA]->S[3],
QuantumField[G0]->S[4],
QuantumField[Hm]->S[5],
QuantumField[Hp]->-S[5],
QuantumField[Gm]->S[6],
QuantumField[Gp]->-S[6],
QuantumField[GhostA]->U[1],
QuantumField[GhostZ]->U[2],
QuantumField[GhostWm]->U[3],
QuantumField[GhostWp]->U[4],
QuantumField[gamma,args___]:>V[1],
QuantumField[Z,args___]:>V[2],
QuantumField[Wm,args___]:>V[3],
QuantumField[Wp,args___]:>-V[3],
QuantumField[HCbar[f_], args___]:>-QuantumField[f, args],
QuantumField[FNu,{},{i_}]:>F[1,{i}],
QuantumField[Fe,{},{i_}]:>F[2,{i}],
QuantumField[FUp,{},{i_,c_}]:>F[3,{i,c}],
QuantumField[FDown,{},{i_,c_}]:>F[4,{i,c}]
};


(*Field Renormalization Constant*)
FieldRenormalizationConstantReplace={dZ[f_]:>ToExpression["dZ"<>ToString[f]<>"1"],
dZfL[cate_,flav1_,flav2_]:>dZfL1[cate,flav1,flav2],
dZfLC[cate_,flav1_,flav2_]:>Conjugate[dZfL1[cate,flav1,flav2]],
dZfR[cate_,flav1_,flav2_]:>dZfR1[cate,flav1,flav2],
dZfRC[cate_,flav1_,flav2_]:>Conjugate[dZfR1[cate,flav1,flav2]]
};


(*Some special replacement*)
SpecialReplacement={Pair[LorentzIndex[i_],LorentzIndex[j_]]:>1,
VCKM[args___]:>CKM[args],VCKMC[args___]:>Conjugate[CKM[args]],dVCKM[args___]:>dCKM1[args],dVCKMC[args___]:>Conjugate[dCKM1[args]],
(exprs_ SumOver[index_,range_]):>IndexSum[exprs,{index,range}],
RXi[Z]->GaugeXi[Z],RXi[Wp]->GaugeXi[W],RXi[Wm]->GaugeXi[W]
};


(*Output*)
ToFeynArtsFormat[Vertex_List,WithFermion_:False]:=Block[{i,totalN,expr,FAOutput,tmp},
totalN=Length[Vertex];
FAOutput={};
For[i=1,i<=totalN,i++,
expr=Vertex[[i]];
tmp=If[WithFermion,Cases[{expr},FRVertex[{{fields__},lo_,nlo_}]:>C[fields]=={{lo/.{PR->0,PL->1},nlo/.{PR->0,PL->1}},{lo/.{PR->1,PL->0},nlo/.{PR->1,PL->0}}}],Cases[{expr},FRVertex[{{fields__},lo_,nlo_}]:>C[fields]=={{lo,nlo}}]];
tmp=tmp//.FieldCode//.FieldRenormalizationConstantReplace//.SpecialReplacement;
FAOutput={FAOutput,tmp[[1]]};
];
Flatten[FAOutput]
]
(*To Files*)
(*Convert to Formatted Style*)
FormattedStyle[exp_]:=Block[{FieldList,FRs,Content,SList},
FieldList=exp/.{FRVertex[arg1_,arg2_]:>arg1};
FRs=exp/.{FRVertex[arg1_,arg2_]:>arg2};
Content=ToString[InputForm[FieldList]]<>"==\n\t";
SList=StringRiffle[ToString[InputForm[#]]&/@FRs,",\n\t"];
Content<>"{"<>SList<>"}"
];
ToFeynArtsModFile[filename_,Vertex_List,WithFermion_:False]:=Block[{SingleConvert,str,i,totalN,FAOutput,rulelist,Header,Content,Ender},
Vertex=ReleaseHold[Vertex];
totalN=Length[Vertex];
Header="M$CouplingMatrices = Join[ M$CouplingMatrices, {\n";
Ender="\n}];\n";
FAOutput=Header;
SingleConvert[exp_]:=Block[{expr,tmp},
expr=exp;
tmp=If[WithFermion,
Cases[{expr},FRVertex[{{fields__},lo_,nlo_}]:>FRVertex[C[fields],{{lo/.{PR->0,PL->1},nlo/.{PR->0,PL->1}},{lo/.{PR->1,PL->0},nlo/.{PR->1,PL->0}}}]],
Cases[{expr},FRVertex[{{fields__},lo_,nlo_}]:>FRVertex[C[fields],{{lo,nlo}}]]];
check=Cases[{expr},FRVertex[{{fields__},lo_,nlo_}]:>{{fields},lo,nlo}];
tmp=If[Length[check[[1,1]]]==2,(*Check whether we are dealing with 2-point vertex: either S-S or S-V*)
If[Length[Cases[check[[1,1]],QuantumField[__,_LorentzIndex]]]==1, (*If 1, it is S-V type vertex*)
{FRVertex[C@@check[[1,1]],{{0,check[[1,3]]/2},{0,-check[[1,3]]/2}}]},(*otherwise, it is S-S type vertex*){FRVertex[C@@check[[1,1]],{{0,-Coefficient[check[[1,3]]/.{Pair[_Momentum,_Momentum]:>pp2},pp2]},{0,check[[1,3]]/.{Pair[_Momentum,_Momentum]:>0}}}]}],
tmp];
tmp=tmp//.FieldCode//.FieldRenormalizationConstantReplace//.SpecialReplacement;
FormattedStyle[tmp[[1]]]
];
rulelist=StringRiffle[SingleConvert/@Vertex,",\n"];
FAOutput=FAOutput<>rulelist<>Ender;
str=OpenWrite[filename];
WriteString[str,FAOutput];
Close[str];
]
