(* ::Package:: *)

(*Here We don't include SM part (except for the Higgs-related part), we adapt all the convention as in the SM.mod in FeynArts for SM*)


(*Scalar Potential*)
LSPFile="LagrangianData/LScalarPotentialPhysics.dat";
If[FileExistsQ[LSPFile],Print["Reading Potential Lagrangian From File: "<>LSPFile<>"......"];LScalarPotentialPhysics=Get[LSPFile];,
Print["Calculating the Potential Lagrangian......"];
Tm112=m112*Phi1C.Phi1;
Tm222=m222*Phi2C.Phi2;
Tm122=-m122*(Phi1C.Phi2+Phi2C.Phi1);
TLam1=Lam1/2*(Phi1C.Phi1)^2;
TLam2=Lam2/2*(Phi2C.Phi2)^2;
TLam3=Lam3*(Phi1C.Phi1)*(Phi2C.Phi2);
TLam4=Lam4*(Phi1C.Phi2)*(Phi2C.Phi1);
TLam5=Lam5/2*((Phi1C.Phi2)^2+(Phi2C.Phi1)^2);

VScalarPotential = Tm112 + Tm222 + Tm122 + TLam1 + TLam2 + TLam3 + TLam4 + TLam5;

(*Tree level reorganization, using as many physics parameters as possible *)
VHH = D[VScalarPotential,QuantumField[HH]]/.{QuantumField[___]:>0}//FullSimplify;
VHL = D[VScalarPotential,QuantumField[HL]]/.{QuantumField[___]:>0}//FullSimplify;
VMHH2 = D[VScalarPotential,{QuantumField[HH],2}]/.{QuantumField[___]:>0}//FullSimplify;
VMHL2 = D[VScalarPotential,{QuantumField[HL],2}]/.{QuantumField[___]:>0}//FullSimplify;
VMHA2 = D[VScalarPotential,{QuantumField[HA],2}]/.{QuantumField[___]:>0}//FullSimplify;
VMHp2 = D[VScalarPotential,QuantumField[Hp],QuantumField[Hm]]/.{QuantumField[___]:>0}//FullSimplify;
VHHHLMix = D[VScalarPotential,QuantumField[HH],QuantumField[HL]]/.{QuantumField[___]:>0}//FullSimplify;

Print["...... Generating the rules transforming from Lagrangian parameters to physics parameters ......"];
ReorganizationRules=Solve[{VHH==THH,VHL==THL,VMHH2==MHH2,VMHL2==MHL2,VMHA2==MHA2,VMHp2==MHp2,VHHHLMix==0,M2==m122/(Sin[beta] Cos[beta])},{m112,m222,m122,Lam1,Lam2,Lam3,Lam4,Lam5}];

Print["...... Simplify the parameter transformation rules. This will take long time, Be patient ......"];
ReorganizationRules=Collect[ReorganizationRules,{THH,THL,MHH2,MHL2,MHA2,MHp2,M2},Simplify];

Print["...... Collecting the new Scalar potential. This will take long time, Be patient ......"];
Tm112=Simplify[Tm112//.ReorganizationRules[[1]]];
Tm222=Simplify[Tm222//.ReorganizationRules[[1]]];
Tm122=Simplify[Tm122//.ReorganizationRules[[1]]];
TLam1=Simplify[TLam1//.ReorganizationRules[[1]]];
TLam2=Simplify[TLam2//.ReorganizationRules[[1]]];
TLam3=Simplify[TLam3//.ReorganizationRules[[1]]];
TLam4=Simplify[TLam4//.ReorganizationRules[[1]]];
TLam5=Simplify[TLam5//.ReorganizationRules[[1]]];

VScalarPotentialPhysics = Tm112 + Tm222 + Tm122 + TLam1 + TLam2 + TLam3 + TLam4 + TLam5;
LScalarPotentialPhysics = -VScalarPotentialPhysics;
Put[LScalarPotentialPhysics,LSPFile];
];


(*Scalar Kinetic*)
LSKFile="LagrangianData/LScalarKinetic.dat";
If[FileExistsQ[LSKFile],Print["Reading Scalar Kinetic Terms From File: "<>LSKFile<>"......"];LScalarKinetic=Get[LSKFile];,
PartialDSelf[f_,mu_]:=RightPartialD[mu].f;
CovariantDoubletD[f_List,mu_]:=(ExpandPartialD[(PartialDSelf[#,mu]&/@f)]/.{RightPartialD[LorentzIndex[nu_]]:>0})-I EL/(2 SW)(Plus@@(PauliSigma[]*Wi[mu])).f+I EL/(2 CW) (IdentityMatrix[2]*BB[mu]).f;
CovariantDoubletDC[f_List,mu_]:=(ExpandPartialD[(PartialDSelf[#,mu]&/@f)]/.{RightPartialD[LorentzIndex[nu_]]:>0})+I EL/(2 SW)(Plus@@(Conjugate[PauliSigma[]]*Wi[mu])).f-I EL/(2 CW) (IdentityMatrix[2]*BB[mu]).f;
Print["Calculating the Scalar Kinetic Terms......"];
LScalarKineticPre=Module[{mu},Simplify[CovariantDoubletDC[Phi1C,mu].CovariantDoubletD[Phi1,mu]+CovariantDoubletDC[Phi2C,mu].CovariantDoubletD[Phi2,mu]]];
LScalarKinetic=LScalarKineticPre//Expand;
LScalarKinetic=LScalarKinetic//.{QuantumField[FCPartialD[index_],F_,args___]^2:>-QuantumField[F,args] QuantumField[FCPartialD[index],FCPartialD[index],F,args],
QuantumField[FCPartialD[index_],F1_,args1___]QuantumField[FCPartialD[index_],F2_,args2___]:>-QuantumField[F1,args1] QuantumField[FCPartialD[index],FCPartialD[index],F2,args2]};
Put[LScalarKinetic,LSKFile];
];


(*Yukawa*)
YukawaFilePrefix="LagrangianData/LType";
isigma2=I {{0,-I},{I,0}};

If[FileExistsQ[YukawaFilePrefix<>"I.dat"],
Print["Reading Type-I Yukawa Lagrangian From File: "<>YukawaFilePrefix<>"I.dat"<>"......"];
LTypeI=Get[YukawaFilePrefix<>"I.dat"];,
Print["Calculating Type-I Yukawa Lagrangian......"];
LTypeIU=Module[{i,j,qc},(SumOver[i,NF]SumOver[j,NF]Sqrt[2] Gf[3,i,j]/(v2))((Bar[QL[i,qc]].(isigma2.Phi2C))uR[j,qc])+(SumOver[i,NF]SumOver[j,NF]Sqrt[2] GfC[3,i,j]/(v2))((QL[j,qc].(isigma2.Phi2))Bar[uR[i,qc]])];
LTypeID=Module[{i,j,qc},(SumOver[i,NF]SumOver[j,NF]Sqrt[2] Gf[4,i,j]/(v2))((Bar[QL[i,qc]].Phi2)dR[j,qc])+(SumOver[i,NF]SumOver[j,NF]Sqrt[2] GfC[4,i,j]/(v2))((QL[j,qc].Phi2C)Bar[dR[i,qc]])];
LTypeIL=Module[{i,j},(SumOver[i,NF]SumOver[j,NF]Sqrt[2] Gf[2,i,j]/(v2))((Bar[LL[i]].Phi2)eR[j]+(LL[j].Phi2C)Bar[eR[i]])(*//Simplify*)];
LTypeI=-(LTypeIU + LTypeID + LTypeIL);
Put[LTypeI,YukawaFilePrefix<>"I.dat"];
];

If[FileExistsQ[YukawaFilePrefix<>"II.dat"],
Print["Reading Type-II Yukawa Lagrangian From File: "<>YukawaFilePrefix<>"II.dat"<>"......"];
LTypeII=Get[YukawaFilePrefix<>"II.dat"];,
Print["Calculating Type-II Yukawa Lagrangian......"];
LTypeIIU=Module[{i,j,qc},(SumOver[i,NF]SumOver[j,NF]Sqrt[2] Gf[3,i,j]/(v2))((Bar[QL[i,qc]].(isigma2.Phi2C))uR[j,qc])+(SumOver[i,NF]SumOver[j,NF]Sqrt[2] GfC[3,i,j]/(v2))((QL[j,qc].(isigma2.Phi2))Bar[uR[i,qc]])];
LTypeIID=Module[{i,j,qc},(SumOver[i,NF]SumOver[j,NF]Sqrt[2] Gf[4,i,j]/(v1))((Bar[QL[i,qc]].Phi1)dR[j,qc])+(SumOver[i,NF]SumOver[j,NF]Sqrt[2] GfC[4,i,j]/(v1))((QL[j,qc].Phi1C)Bar[dR[i,qc]])];
LTypeIIL=Module[{i,j},(SumOver[i,NF]SumOver[j,NF]Sqrt[2] Gf[2,i,j]/(v1))((Bar[LL[i]].Phi1)eR[j]+(LL[j].Phi1C)Bar[eR[i]])(*//Simplify*)];
LTypeII=-(LTypeIIU + LTypeIID + LTypeIIL);
Put[LTypeII,YukawaFilePrefix<>"II.dat"];
];

If[FileExistsQ[YukawaFilePrefix<>"LS.dat"],
Print["Reading Type-LS Yukawa Lagrangian From File: "<>YukawaFilePrefix<>"LS.dat"<>"......"];
LTypeLS=Get[YukawaFilePrefix<>"LS.dat"];,
Print["Calculating Type-LS Yukawa Lagrangian......"];
LTypeLSU=Module[{i,j,qc},(SumOver[i,NF]SumOver[j,NF]Sqrt[2] Gf[3,i,j]/(v2))((Bar[QL[i,qc]].(isigma2.Phi2C))uR[j,qc])+(SumOver[i,NF]SumOver[j,NF]Sqrt[2] GfC[3,i,j]/(v2))((QL[j,qc].(isigma2.Phi2))Bar[uR[i,qc]])];
LTypeLSD=Module[{i,j,qc},(SumOver[i,NF]SumOver[j,NF]Sqrt[2] Gf[4,i,j]/(v2))((Bar[QL[i,qc]].Phi2)dR[j,qc])+(SumOver[i,NF]SumOver[j,NF]Sqrt[2] GfC[4,i,j]/(v2))((QL[j,qc].Phi2C)Bar[dR[i,qc]])];
LTypeLSL=Module[{i,j},(SumOver[i,NF]SumOver[j,NF]Sqrt[2] Gf[2,i,j]/(v1))((Bar[LL[i]].Phi1)eR[j]+(LL[j].Phi1C)Bar[eR[i]])(*//Simplify*)];
LTypeLS=-(LTypeLSU + LTypeLSD + LTypeLSL);
Put[LTypeLS,YukawaFilePrefix<>"LS.dat"];
];

If[FileExistsQ[YukawaFilePrefix<>"FL.dat"],
Print["Reading Type-FL Yukawa Lagrangian From Files: "<>YukawaFilePrefix<>"FL.dat"<>"......"];
LTypeFL=Get[YukawaFilePrefix<>"FL.dat"];,
Print["Calculating Type-FL Yukawa Lagrangian......"];
LTypeFLU=Module[{i,j,qc},(SumOver[i,NF]SumOver[j,NF]Sqrt[2] Gf[3,i,j]/(v2))((Bar[QL[i,qc]].(isigma2.Phi2C))uR[j,qc])+(SumOver[i,NF]SumOver[j,NF]Sqrt[2] GfC[3,i,j]/(v2))((QL[j,qc].(isigma2.Phi2))Bar[uR[i,qc]])];
LTypeFLD=Module[{i,j,qc},(SumOver[i,NF]SumOver[j,NF]Sqrt[2] Gf[4,i,j]/(v1))((Bar[QL[i,qc]].Phi1)dR[j,qc])+(SumOver[i,NF]SumOver[j,NF]Sqrt[2] GfC[4,i,j]/(v1))((QL[j,qc].Phi1C)Bar[dR[i,qc]])];
LTypeFLL=Module[{i,j},(SumOver[i,NF]SumOver[j,NF]Sqrt[2] Gf[2,i,j]/(v2))((Bar[LL[i]].Phi2)eR[j]+(LL[j].Phi2C)Bar[eR[i]])(*//Simplify*)];
LTypeFL=-(LTypeFLU + LTypeFLD + LTypeFLL);
Put[LTypeFL,YukawaFilePrefix<>"FL.dat"];
];

Gf[2,i_,j_]:=Mf[2,i]IndexDelta[i,j];
GfC[2,i_,j_]:=Mf[2,i]IndexDelta[i,j];
Gf[type_,i_,j_]:=Module[{k},SumOver[k,NF]Mf[type,k]ULC[type,i,k]UR[type,k,j]];
GfC[type_,i_,j_]:=Module[{k},SumOver[k,NF]Mf[type,k]URC[type,i,k]UL[type,k,j]];



(*Ghost Lagrangian: Only S-U-U part, it is assumed that this part has already been renormalized*)
LGhostFile="LagrangianData/LGhost.dat";
If[FileExistsQ[LGhostFile],Print["Reading Ghost Terms From File: "<>LGhostFile<>"......"];LGhost=Get[LGhostFile];,
Print["Calculating the Ghost Lagrangian......"];
GhostDD[args_]:=Coefficient[args,\[Theta]A] QuantumField[GhostA]+Coefficient[args,\[Theta]Z]QuantumField[GhostZ]+Coefficient[args,\[Theta]p]QuantumField[GhostWp]+Coefficient[args,\[Theta]m]QuantumField[GhostWm];
LGhost:=QuantumField[HCbar[GhostZ]]GhostDD[FZ/.{QuantumField[arg_]:>DGT[arg]}/.ScalarTransformation[[1]]]+QuantumField[HCbar[GhostWp]]GhostDD[FP/.{QuantumField[arg_]:>DGT[arg]}/.ScalarTransformation[[1]]]+QuantumField[HCbar[GhostWm]]GhostDD[FM/.{QuantumField[arg_]:>DGT[arg]}/.ScalarTransformation[[1]]];
Put[LGhost,LGhostFile];
]
