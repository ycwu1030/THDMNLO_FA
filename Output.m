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
(exprs_ SumOver[index_,range_]):>IndexSum[exprs,{index,range}]
};


(*Output*)
ToFeynArtsFormat[Vertex_List,WithFermion_:False]:=Block[{i,totalN,expr,FAOutput,tmp},
totalN=Length[Vertex];
FAOutput={};
For[i=1,i<=totalN,i++,
expr=Vertex[[i]];
tmp=If[WithFermion,Cases[{expr},FRVertex[{{fields__},lo_,nlo_}]:>C[fields]=={{lo/.{PR->0,PL->1},nlo/.{PR->0,PL->1}},{lo/.{PR->1,PL->0},nlo/.{PR->1,PL->0}}}],Cases[{expr},FRVertex[{{fields__},lo_,nlo_}]:>C[fields]=={lo,nlo}]];
tmp=tmp//.FieldCode//.FieldRenormalizationConstantReplace//.SpecialReplacement;
FAOutput={FAOutput,tmp[[1]]};
];
Flatten[FAOutput]
]
