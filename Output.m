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


Sigma1PI={
TP$Sigma[fields_]:>-(TadpoleRC[fields]//.FieldCode),
(*ISSUE: Check whether we should modify the FieldRC to DSelfEnergy[f\[Rule]f,0] for Goldstone*)
Derivative[1][PP$Sigma[fields1_,fields2_]][m2_]:>-(FieldRC[fields1]//.FieldCode),
PP$Sigma[QuantumField[Subscript[HL,R]],QuantumField[Subscript[HL,R]]][MHL2]->(MassRC[QuantumField[Subscript[HL,R]]]//.FieldCode),
PP$Sigma[QuantumField[Subscript[HH,R]],QuantumField[Subscript[HH,R]]][MHH2]->(MassRC[QuantumField[Subscript[HH,R]]]//.FieldCode),
PP$Sigma[QuantumField[Subscript[HH,R]],QuantumField[Subscript[HL,R]]][MHH2]->(FieldRC[QuantumField[Subscript[HL,R]],QuantumField[Subscript[HH,R]]](MHL2-MHH2)/2//.FieldCode),
PP$Sigma[QuantumField[Subscript[HH,R]],QuantumField[Subscript[HL,R]]][MHL2]->(FieldRC[QuantumField[Subscript[HH,R]],QuantumField[Subscript[HL,R]]](MHH2-MHL2)/2//.FieldCode),
(*Note: 
As in this version, we didn't renormalize the Gauge Fixing term, so when calculate the renormalization constants related to the Goldstone, we need to treat it as massless (but it actually is massive),
So, we can't use FieldRC for G0-HA mixing and Gp-Hp mixing, because FormCalc will treat G0 Gp having mass MZ and MW. So need to directly use SelfEnergy.
*)
PP$Sigma[QuantumField[Subscript[HA,R]],QuantumField[Subscript[HA,R]]][MHA2]->(MassRC[QuantumField[Subscript[HA,R]]]//.FieldCode),
PP$Sigma[QuantumField[Subscript[G0,R]],QuantumField[Subscript[HA,R]]][MHA2]->(SelfEnergy[QuantumField[Subscript[HA,R]]->QuantumField[Subscript[G0,R]],MHA]//.FieldCode),
PP$Sigma[QuantumField[Subscript[G0,R]],QuantumField[Subscript[HA,R]]][0]->(SelfEnergy[QuantumField[Subscript[HA,R]]->QuantumField[Subscript[G0,R]],0]//.FieldCode),
PP$Sigma[QuantumField[Subscript[Hm,R]],QuantumField[Subscript[Hp,R]]][MHp2]->(MassRC[QuantumField[Subscript[Hm,R]]]//.FieldCode),
PP$Sigma[QuantumField[Subscript[Gm,R]],QuantumField[Subscript[Hp,R]]][MHp2]->(SelfEnergy[QuantumField[Subscript[Hm,R]]->QuantumField[Subscript[Gm,R]],MHp]//.FieldCode),
PP$Sigma[QuantumField[Subscript[Gp,R]],QuantumField[Subscript[Hm,R]]][0]->(SelfEnergy[QuantumField[Subscript[Hm,R]]->QuantumField[Subscript[Gm,R]],0]//.FieldCode)
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
RXi[Z]->GaugeXi[Z],RXi[Wp]->GaugeXi[W],RXi[Wm]->GaugeXi[W],RXi[W]->GaugeXi[W]
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


ToFeynArtsRenConst[filename_,RenConstList_]:=Block[{SingleConvert,RClist,str},
SingleConvert[exp_]:=Block[{RC,tmp},
RC=Cases[{exp},HoldPattern[args1_->args2_]:>{args1,Collect[args2,{_MassRC,_FieldRC,dTHL1,dTHH1},Simplify]}][[1]];
ToString[InputForm[RenConst[RC[[1]]//.FieldRenormalizationConstantReplace]]]<>" := "<>ToString[InputForm[RC[[2]]//.FieldRenormalizationConstantReplace//.Sigma1PI]]
];
RClist=StringRiffle[SingleConvert/@RenConstList,"\n\n"];
str=OpenWrite[filename];
WriteString[str,RClist];
Close[str];
];


LatexBar[s_String]:=Block[{pieces},
pieces=StringSplit[s,"_"];
"\\bar{"<>pieces[[1]]<>"}_"<>Rest[pieces]
];
LatexCode={QuantumField[Subscript[f_,__],args___]:>QuantumField[f,args],
QuantumField[HL]->"h ",
QuantumField[HH]->"H ",
QuantumField[HA]->"A ",
QuantumField[Hm]->"H^{-} ",
QuantumField[Hp]->"H^{+} ",
QuantumField[G0]->"G^{0} ",
QuantumField[Gm]->"G^{-} ",
QuantumField[Gp]->"G^{+} ",
QuantumField[GhostA]->"c_{\\gamma} ",
QuantumField[GhostZ]->"c_{Z} ",
QuantumField[GhostWm]->"c_{W^{-}} ",
QuantumField[GhostWp]->"c_{W^{+}} ",
QuantumField[gamma,args___]:>"\\gamma ",
QuantumField[Z,args___]:>"Z ",
QuantumField[Wm,args___]:>"W^{-} ",
QuantumField[Wp,args___]:>"W^{+} ",
QuantumField[HCbar[f_], args___]:>LatexBar[QuantumField[f,args]],
QuantumField[FNu,{},{i_}]:>"\\nu_{"<>ToString[i]<>"} ",
QuantumField[Fe,{},{i_}]:>"\\ell_{"<>ToString[i]<>"} ",
QuantumField[FUp,{},{i_,c_}]:>"u_{"<>ToString[i]<>"} ",
QuantumField[FDown,{},{i_,c_}]:>"d_{"<>ToString[i]<>"} "
};
LatexSymbol={
EL->e,
vev->v,
beta->\[Beta],
alpha->\[Alpha],
MHH2->



\!\(\*SubsuperscriptBox[\(m\), \(H\), \(2\)]\),
MHL2->



\!\(\*SubsuperscriptBox[\(m\), \(h\), \(2\)]\),
MHA2->



\!\(\*SubsuperscriptBox[\(m\), \(A\), \(2\)]\),
MHp2 -> 
\!\(\*SubsuperscriptBox[\(m\), \(p\), \(2\)]\),
SW->Subscript[s, W],
CW->Subscript[c, W],
PL->Subscript[P, L],
PR->Subscript[P, R],
IndexDelta[c1,c2]->1,
IndexDelta[i_,j_]:>KroneckerDelta[i,j],
VCKM[i_,j_]:>Subscript[V, i,j],
VCKMC[i_,j_]:>


\!\(\*SubsuperscriptBox[\(V\), \(i, j\), \(*\)]\),
Mf[__,i_]:>Subscript[m, Subscript[f, i]],
M2->Subscript[M, 2]
};
RotationMatrixSimplify[exp_]:=Block[{tmp},
tmp=exp//Simplify[#,CW^2+SW^2==1]&;
tmp//Expand
]


ConvertToSingleTerms[exp_]:=Block[{tmpexp,exphead,tmp},
tmpexp=Expand[exp];
exphead=Head[tmpexp];
If[exphead===Plus,
tmp=List@@tmpexp;,
tmp={tmpexp};
];
tmp
]
SingleTermLatexStyle[exp_]:=Block[{exphead,sign,tmp},
exphead=Head[exp];
If[exphead===Symbol||exphead===Integer,
  tmp=ToString[InputForm[TeXForm[exp]]];,
  sign=Sign[(List@@exp)[[1]]];
  If[sign===-1||sign===-I,
  tmp=ToString[InputForm[TeXForm[exp]]];,
  tmp="+"<>ToString[InputForm[TeXForm[exp]]];
  ];
];
tmp
];

FormattedLatexStyle[exp_]:=Block[{FieldList,FRs,Content,TermsList,SListFirst,SListRest},
FieldList=exp/.{FRVertex[arg1_,arg2_]:>arg1};
FRs=exp/.{FRVertex[arg1_,arg2_]:>arg2};
Content="g_{"<>StringRiffle[FieldList,""]<>"}=\n\t";
TermsList=ConvertToSingleTerms[FRs[[1]]];
SListFirst=ToString[InputForm[TeXForm[TermsList[[1]]]]];
SListRest=StringRiffle[SingleTermLatexStyle[#]&/@(Rest[TermsList]),"\n\t"];
Content<>SListFirst<>"\n\t"<>SListRest
];
ToLatexFile[filename_,Vertex_List,WithFermion_:False]:=Block[{SingleConvert,str,i,totalN,FAOutput,rulelist,Header,Content,Ender},
Vertex=ReleaseHold[Vertex];
totalN=Length[Vertex];
Header="\\begin{align}\n\\begin{autobreak}\n";
Ender="\n\\end{autobreak}\n\\end{align}\n";
FAOutput=Header;
SingleConvert[exp_]:=Block[{expr,tmp},
expr=exp;
tmp=Cases[{expr},FRVertex[{{fields__},lo_,nlo_}]:>FRVertex[{fields},{lo//RotationMatrixSimplify}]];
(*check=Cases[{expr},FRVertex[{{fields__},lo_,nlo_}]:>{{fields},lo,nlo}];*)
(*tmp=If[Length[check[[1,1]]]==2,(*Check whether we are dealing with 2-point vertex: either S-S or S-V*)
If[Length[Cases[check[[1,1]],QuantumField[__,_LorentzIndex]]]==1, (*If 1, it is S-V type vertex*)
{FRVertex[check[[1,1]],{{0,check[[1,3]]/2},{0,-check[[1,3]]/2}}]},
(*otherwise, it is S-S type vertex*)
{FRVertex[check[[1,1]],{{0,-Coefficient[check[[1,3]]/.{Pair[_Momentum,_Momentum]:>pp2},pp2]},{0,check[[1,3]]/.{Pair[_Momentum,_Momentum]:>0}}}]}
],
tmp];*)
tmp=tmp//.LatexCode//.LatexSymbol;
FormattedLatexStyle[tmp[[1]]]
];
rulelist=StringRiffle[SingleConvert/@Vertex,",\n\\end{autobreak}\n\\end{align}\n\\begin{align}\n\\begin{autobreak}\n"];
FAOutput=FAOutput<>rulelist<>Ender;
str=OpenWrite[filename];
WriteString[str,FAOutput];
Close[str];
]
