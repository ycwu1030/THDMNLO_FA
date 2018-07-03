(* ::Package:: *)

(*$Parameters=[];
RegisterParameter[p_]:=Block[{},]*)

(*Scalar Part Parameters*)
MHL2/: Renormalization[MHL2]:=MHL2+r1 dMHL21;
MHH2/: Renormalization[MHH2]:=MHH2+r1 dMHH21;
MHA2/: Renormalization[MHA2]:=MHA2+r1 dMHA21;
MHp2/: Renormalization[MHp2]:=MHp2+r1 dMHp21;
beta/: Renormalization[beta]:=beta+r1 dbeta1;
alpha/: Renormalization[alpha]:=alpha+r1 dalpha1;
M2/: Renormalization[M2]:=M2+r1 dM21;
THH/:Renormalization[THH]:=r1 dTHH1;
THL/:Renormalization[THL]:=r1 dTHL1;

(*SM model Parameters*)
MW2/: Renormalization[MW2]:=MW2 + r1 dMWsq1;
MZ2/: Renormalization[MZ2]:=MZ2 + r1 dMZsq1;
EL/: Renormalization[EL]:= (1+r1 dZe1)EL;
SW/: Renormalization[SW]:=SW+r1 dSW1;
vev /: Renormalization[vev]:=vev+r1 dvev1;
CW /: Renormalization[CW]:=CW+ r1 dCW1;
Mf/: Renormalization[Mf[type_,f1_]]:=Mf[type,f1]+ r1 dMf[type,f1];
VCKM/: Renormalization[VCKM[i_,j_]]:=VCKM[i,j] + r1 dVCKM[i,j];
VCKMC/: Renormalization[VCKMC[i_,j_]]:=VCKMC[i,j] + r1 dVCKMC[i,j];
