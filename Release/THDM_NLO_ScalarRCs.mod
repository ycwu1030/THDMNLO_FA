RenConst[dCW1] := (CW*(dMWsq1/MW2 - dMZsq1/MZ2))/2

RenConst[dSW1] := (CW^2*(-(dMWsq1/MW2) + dMZsq1/MZ2))/(2*SW)

RenConst[dvev1] := (-dZe1 + dMWsq1/(2*MW2) + dSW1/SW)*vev

RenConst[dTHL1] := -TadpoleRC[S[1]]

RenConst[dTHH1] := -TadpoleRC[S[2]]

RenConst[dMHL21] := -(dTHL1*(Cos[3*alpha - beta] + 3*Cos[alpha + beta])*Csc[2*beta])/(2*vev) + MassRC[S[1]] - (dTHH1*Cos[alpha - beta]*Csc[2*beta]*Sin[2*alpha])/vev

RenConst[dMHH21] := MassRC[S[2]] - (dTHL1*Csc[2*beta]*Sin[2*alpha]*Sin[alpha - beta])/vev + (dTHH1*Csc[2*beta]*(Sin[3*alpha - beta] - 3*Sin[alpha + beta]))/(2*vev)

RenConst[dZHLHL1] := FieldRC[S[1]]

RenConst[dZHLHH1] := -(((-MHH2 + MHL2)*FieldRC[S[1], S[2]])/(MHH2 - MHL2)) + (dTHL1*Cos[alpha - beta]*Csc[beta]*Sec[beta]*Sin[2*alpha])/((MHH2 - MHL2)*vev) + (dTHH1*Csc[beta]*Sec[beta]*Sin[2*alpha]*Sin[alpha - beta])/((MHH2 - MHL2)*vev)

RenConst[dZHHHL1] := FieldRC[S[2], S[1]] - (dTHL1*Cos[alpha - beta]*Csc[beta]*Sec[beta]*Sin[2*alpha])/((MHH2 - MHL2)*vev) - (dTHH1*Csc[beta]*Sec[beta]*Sin[2*alpha]*Sin[alpha - beta])/((MHH2 - MHL2)*vev)

RenConst[dZHHHH1] := FieldRC[S[2]]

RenConst[dMHA21] := -(dTHL1*(Cos[alpha - 3*beta] + 3*Cos[alpha + beta])*Csc[2*beta])/(2*vev) + MassRC[S[3]] - (dTHH1*Csc[2*beta]*(Sin[alpha - 3*beta] + 3*Sin[alpha + beta]))/(2*vev)

RenConst[dZHAHA1] := FieldRC[S[3]]

RenConst[dZHAG01] := (-2*dTHL1*Cos[alpha - beta])/(MHA2*vev) + ReTilde[SelfEnergy[S[3]->S[4],0]]*2/MHA2 - (2*dTHH1*Sin[alpha - beta])/(MHA2*vev)

RenConst[dZG0HA1] := (2*dTHL1*Cos[alpha - beta])/(MHA2*vev) - ReTilde[SelfEnergy[S[3]->S[4],MHA]]*2/MHA2 + (2*dTHH1*Sin[alpha - beta])/(MHA2*vev)

RenConst[dZG0G01] := FieldRC[S[4]]

RenConst[dMHp21] := -(dTHL1*(Cos[alpha - 3*beta] + 3*Cos[alpha + beta])*Csc[2*beta])/(2*vev) + MassRC[S[5]] - (dTHH1*Csc[2*beta]*(Sin[alpha - 3*beta] + 3*Sin[alpha + beta]))/(2*vev)

RenConst[dZHpHp1] := FieldRC[S[5]]

RenConst[dZHpGp1] := (-2*dTHL1*Cos[alpha - beta])/(MHp2*vev) + ReTilde[SelfEnergy[S[5]->S[6],0]]*2/MHp2 - (2*dTHH1*Sin[alpha - beta])/(MHp2*vev)

RenConst[dZGpHp1] := (2*dTHL1*Cos[alpha - beta])/(MHp2*vev) - ReTilde[SelfEnergy[S[5]->S[6],MHp]]*2/MHp2 + (2*dTHH1*Sin[alpha - beta])/(MHp2*vev)

RenConst[dZGpGp1] := FieldRC[S[6]]

RenConst[dalpha1] := (dZHHHL1 - dZHLHH1)/4

RenConst[dbeta1] := (dZG0HA1 - dZHAG01)/4

RenConst[dM21] := Divergence UVM21

UVM21I = (Alfa*M2*((MHH2 - MHL2)*Cos[2*alpha - beta] +
      (4*M2 + 18*MB2 + 18*MC2 + 18*MD2 + 6*ME2 - MHA2 - 2*MHp2 + 6*ML2 +
        6*MM2 + 18*MS2 + 18*MT2 + 18*MU2 - 6*MW2 - 3*MZ2)*Cos[beta] -
      4*M2*Cos[3*beta] + 6*MB2*Cos[3*beta] + 6*MC2*Cos[3*beta] +
      6*MD2*Cos[3*beta] + 2*ME2*Cos[3*beta] + MHA2*Cos[3*beta] +
      2*MHp2*Cos[3*beta] + 2*ML2*Cos[3*beta] + 2*MM2*Cos[3*beta] +
      6*MS2*Cos[3*beta] + 6*MT2*Cos[3*beta] + 6*MU2*Cos[3*beta] +
      6*MW2*Cos[3*beta] + 3*MZ2*Cos[3*beta] - MHH2*Cos[2*alpha + beta] +
      MHL2*Cos[2*alpha + beta])*Csc[beta]*Csc[2*beta])/(32*MW2*Pi*SW2);

UVM21II = (Alfa*M2*(4*M2 + 18*MB2 + 18*MC2 + 18*MD2 + 6*ME2 - MHA2 -
      2*MHp2 + 6*ML2 + 6*MM2 + 18*MS2 + 18*MT2 + 18*MU2 - 6*MW2 - 3*MZ2 +
      (MHH2 - MHL2)*Cos[2*(alpha - beta)] -
      8*(3*MB2 - 3*MC2 + 3*MD2 + ME2 + ML2 + MM2 + 3*MS2 - 3*MT2 - 3*MU2)*
       Cos[2*beta] - 4*M2*Cos[4*beta] + 6*MB2*Cos[4*beta] +
      6*MC2*Cos[4*beta] + 6*MD2*Cos[4*beta] + 2*ME2*Cos[4*beta] +
      MHA2*Cos[4*beta] + 2*MHp2*Cos[4*beta] + 2*ML2*Cos[4*beta] +
      2*MM2*Cos[4*beta] + 6*MS2*Cos[4*beta] + 6*MT2*Cos[4*beta] +
      6*MU2*Cos[4*beta] + 6*MW2*Cos[4*beta] + 3*MZ2*Cos[4*beta] -
      MHH2*Cos[2*(alpha + beta)] + MHL2*Cos[2*(alpha + beta)])*Csc[2*beta]^2)/
    (32*MW2*Pi*SW2);

UVM21LS = (Alfa*M2*(4*M2 + 18*MB2 + 18*MC2 + 18*MD2 + 6*ME2 - MHA2 -
      2*MHp2 + 6*ML2 + 6*MM2 + 18*MS2 + 18*MT2 + 18*MU2 - 6*MW2 - 3*MZ2 +
      (MHH2 - MHL2)*Cos[2*(alpha - beta)] +
      8*(3*MB2 + 3*MC2 + 3*MD2 - ME2 - ML2 - MM2 + 3*MS2 + 3*MT2 + 3*MU2)*
       Cos[2*beta] - 4*M2*Cos[4*beta] + 6*MB2*Cos[4*beta] +
      6*MC2*Cos[4*beta] + 6*MD2*Cos[4*beta] + 2*ME2*Cos[4*beta] +
      MHA2*Cos[4*beta] + 2*MHp2*Cos[4*beta] + 2*ML2*Cos[4*beta] +
      2*MM2*Cos[4*beta] + 6*MS2*Cos[4*beta] + 6*MT2*Cos[4*beta] +
      6*MU2*Cos[4*beta] + 6*MW2*Cos[4*beta] + 3*MZ2*Cos[4*beta] -
      MHH2*Cos[2*(alpha + beta)] + MHL2*Cos[2*(alpha + beta)])*Csc[2*beta]^2)/
    (32*MW2*Pi*SW2);

UVM21FL = (Alfa*M2*(4*M2 + 18*MB2 + 18*MC2 + 18*MD2 + 6*ME2 - MHA2 -
      2*MHp2 + 6*ML2 + 6*MM2 + 18*MS2 + 18*MT2 + 18*MU2 - 6*MW2 - 3*MZ2 +
      (MHH2 - MHL2)*Cos[2*(alpha - beta)] -
      8*(3*MB2 - 3*MC2 + 3*MD2 - ME2 - ML2 - MM2 + 3*MS2 - 3*MT2 - 3*MU2)*
       Cos[2*beta] - 4*M2*Cos[4*beta] + 6*MB2*Cos[4*beta] +
      6*MC2*Cos[4*beta] + 6*MD2*Cos[4*beta] + 2*ME2*Cos[4*beta] +
      MHA2*Cos[4*beta] + 2*MHp2*Cos[4*beta] + 2*ML2*Cos[4*beta] +
      2*MM2*Cos[4*beta] + 6*MS2*Cos[4*beta] + 6*MT2*Cos[4*beta] +
      6*MU2*Cos[4*beta] + 6*MW2*Cos[4*beta] + 3*MZ2*Cos[4*beta] -
      MHH2*Cos[2*(alpha + beta)] + MHL2*Cos[2*(alpha + beta)])*Csc[2*beta]^2)/
    (32*MW2*Pi*SW2);

UVM21::WRONGTYPE = "The Yukawa type `1` is not one of 1,2,3,4.";
UVM21 = Switch[M$YUKAWATYPE,1,UVM21I,2,UVM21II,3,UVM21FL,4,UVM21LS,_,Message[UVM21::WRONGTYPE,M$YUKAWATYPE];0];
