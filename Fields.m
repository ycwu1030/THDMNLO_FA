(* ::Package:: *)

Needs["FeynCalc`"];


(*Scalar Fields and their renormalization*)

(*Scalar Fields at Tree level*)
(*The two doublets and corresponding Charged Conjugate*)
Phi1={phip1,(v1+rho1+I eta1)/Sqrt[2]};
Phi2={phip2,(v2+rho2+I eta2)/Sqrt[2]};
Phi1C={phim1,(v1+rho1-I eta1)/Sqrt[2]};
Phi2C={phim2,(v2+rho2-I eta2)/Sqrt[2]};

(*Definition of the charged fields in terms of physics fields, by the rotation matrix of beta*)
{phip1,phip2}=RotationMatrix[beta].{QuantumField[Gp],QuantumField[Hp]};
{phim1,phim2}=RotationMatrix[beta].{QuantumField[Gm],QuantumField[Hm]};

(*Definition of the CP-odd fields in terms of physics fields, by the rotation matrix of beta*)
{eta1,eta2}=RotationMatrix[beta].{QuantumField[G0],QuantumField[HA]};

(*Definition of the CP-even fields in terms of physics fields, by the rotation matrix of alpha*)
{rho1,rho2}=RotationMatrix[alpha].{QuantumField[HH],QuantumField[HL]};

(*The vacuum expected value*)
v1=vev Cos[beta];
v2=vev Sin[beta];


(*Field Renormalization*)
HH /: RenormalizationInfo[HH] := {FieldNormalization[{HH,HL}], QuantumField/@(Subscript[#,R]&/@{HH, HL}), 1};
HL /: RenormalizationInfo[HL] := {FieldNormalization[{HH,HL}], QuantumField/@(Subscript[#,R]&/@{HH, HL}), 2};

G0/: RenormalizationInfo[G0]:={FieldNormalization[{G0,HA}],QuantumField/@(Subscript[#,R]&/@{G0,HA}),1};
HA/: RenormalizationInfo[HA]:={FieldNormalization[{G0,HA}],QuantumField/@(Subscript[#,R]&/@{G0,HA}),2};

Gp/: RenormalizationInfo[Gp]:={FieldNormalization[{Gp,Hp}],QuantumField/@(Subscript[#,R]&/@{Gp,Hp}),1};
Hp/: RenormalizationInfo[Hp]:={FieldNormalization[{Gp,Hp}],QuantumField/@(Subscript[#,R]&/@{Gp,Hp}),2};

Gm/: RenormalizationInfo[Gm]:={FieldNormalization[{Gp,Hp}],QuantumField/@(Subscript[#,R]&/@{Gm,Hm}),1};
Hm/: RenormalizationInfo[Hm]:={FieldNormalization[{Gp,Hp}],QuantumField/@(Subscript[#,R]&/@{Gm,Hm}),2};


(*Gauge Fields and their renormalization*)
(*Gauge covariant derivative: always use -1 convention Subscript[D, \[Mu]]=\!\(
\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\(-ig\)\)\[Sigma]^a/2Subsuperscript[W, \[Mu], a]+ig'Y/2Subscript[B, \[Mu]]*)
Wi[mu_]:={W1[mu],W2[mu],W3[mu]};
Bi[mu_]:=BB[mu];

W1[mu_]:=1/Sqrt[2](QuantumField[Wp,{mu}]+QuantumField[Wm,{mu}]);
W2[mu_]:=I/Sqrt[2](QuantumField[Wp,{mu}]-QuantumField[Wm,{mu}]);

W3[mu_]:=CW QuantumField[Z,{mu}]-SW QuantumField[gamma,{mu}];
BB[mu_]:=SW QuantumField[Z,{mu}]+CW QuantumField[gamma,{mu}];


(*Field Renormalization*)
Wp/: RenormalizationInfo[Wp,mu_:mu]:={FieldNormalization[{W,W}],{QuantumField[Subscript[Wp,R],{mu}],0},1};
Wm/: RenormalizationInfo[Wm,mu_:mu]:={FieldNormalization[{W,W}],{QuantumField[Subscript[Wm,R],{mu}],0},1};

Z/: RenormalizationInfo[Z,mu_:mu]:={FieldNormalization[{Z,A}],QuantumField[#,{mu}]&/@(Subscript[#,R]&/@{Z,gamma}),1};
gamma/: RenormalizationInfo[gamma,mu_:mu]:={FieldNormalization[{Z,A}],QuantumField[#,{mu}]&/@(Subscript[#,R]&/@{Z,gamma}),2};


(*Fermion Field and their renormalization*)

(*Here I used the SUNIndex place to hold the flavor and Color Index, *)
(*So they should be treated properly to avoid some automatical calculation within the SU(N) Group by FeynCalc*)
QL[i_,c_]:=PL*{SumOver[jUpL,NF]*UU[i,jUpL]*QuantumField[FUp,{},{jUpL,c}],SumOver[jDownL,NF]*UD[i,jDownL]*QuantumField[FDown[jDownL,c]]};
QLbar[i_,c_]:=PR*{SumOver[jUpLbar,NF]*UUC[i,jUpLbar]*QuantumField[FUpbar,{},{jUpLbar,c}],SumOver[jDownLbar,NF]*UDC[i,jDownLbar]*QuantumField[FDownbar,{},{jDownLbar,c}]};

UR[i_,c_]:=PR*SumOver[jUR,NF]*UU[i,jUR]*QuantumField[FUp,{},{jUR,c}];
URbar[i_,c_]:=PL*SumOver[jURbar,NF]*UUC[i,jURbar]*QuantumField[FUpbar,{},{jURbar,c}];

DR[i_,c_]:=PR*SumOver[jDR,NF]*UD[i,jDR]*QuantumField[FDown,{},{jDR,c}];
DRbar[i_,c_]:=PL*SumOver[jDRbar,NF]*UDC[i,jDRbar]*QuantumField[FDownbar,{},{jDRbar,c}];

LL[i_]:=PL*{QuantumField[FNu,{},{i}],QuantumField[Fe,{},{i}]};
LLbar[i_]:=PR*{QuantumField[FNubar,{},{i}],QuantumField[Febar,{},{i}]};

eR[i_]:=PR*QuantumField[Fe,{},{i}];
eRbar[i_]:=PL*QuantumField[Febar,{},{i}];
