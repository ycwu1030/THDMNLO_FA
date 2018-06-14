(* ::Package:: *)

Needs["FeynCalc`"]; (*Do we really need FeynCalc?*)


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
HH /: RenormalizationInfo[HH] := {FieldNormalization[{HH,HL}], QuantumField /@ Subscript[#,R]&/@{HH, HL}, 1};
HL /: RenormalizationInfo[HL] := {FieldNormalization[{HH,HL}], QuantumField /@ Subscript[#,R]&/@{HH, HL}, 2};

G0/: RenormalizationInfo[G0]:={FieldNormalization[{G0,HA}],QuantumField/@Subscript[#,R]&/@{G0,HA},1};
HA/: RenormalizationInfo[HA]:={FieldNormalization[{G0,HA}],QuantumField/@Subscript[#,R]&/@{G0,HA},2};

Gp/: RenormalizationInfo[Gp]:={FieldNormalization[{Gp,Hp}],QuantumField/@Subscript[#,R]&/@{Gp,Hp},1};
Hp/: RenormalizationInfo[Hp]:={FieldNormalization[{Gp,Hp}],QuantumField/@Subscript[#,R]&/@{Gp,Hp},2};

Gm/: RenormalizationInfo[Gm]:={FieldNormalization[{Gp,Hp}],QuantumField/@Subscript[#,R]&/@{Gm,Hm},1};
Hm/: RenormalizationInfo[Hm]:={FieldNormalization[{Gp,Hp}],QuantumField/@Subscript[#,R]&/@{Gm,Hm},2};


(*Gauge Fields and their renormalization*)
Wi={QuantumField[W[1],{mu}],QuantumField[W[2],{mu}],QuantumField[W[3],{mu}]};
Bi=QuantumField[B,{mu}]


