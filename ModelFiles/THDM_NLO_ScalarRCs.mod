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

RenConst[dZHAG01] := (-2*dTHL1*Cos[alpha - beta])/(MHA2*vev) + (2*SelfEnergy[S[3] -> S[4], 0])/MHA2 - (2*dTHH1*Sin[alpha - beta])/(MHA2*vev)

RenConst[dZG0HA1] := (2*dTHL1*Cos[alpha - beta])/(MHA2*vev) - (2*SelfEnergy[S[3] -> S[4], MHA])/MHA2 + (2*dTHH1*Sin[alpha - beta])/(MHA2*vev)

RenConst[dZG0G01] := FieldRC[S[4]]

RenConst[dMHp21] := -(dTHL1*(Cos[alpha - 3*beta] + 3*Cos[alpha + beta])*Csc[2*beta])/(2*vev) + MassRC[S[5]] - (dTHH1*Csc[2*beta]*(Sin[alpha - 3*beta] + 3*Sin[alpha + beta]))/(2*vev)

RenConst[dZHpHp1] := FieldRC[S[5]]

RenConst[dZHpGp1] := (-2*dTHL1*Cos[alpha - beta])/(MHp2*vev) + (2*SelfEnergy[S[5] -> S[6], 0])/MHp2 - (2*dTHH1*Sin[alpha - beta])/(MHp2*vev)

RenConst[dZGpHp1] := (2*dTHL1*Cos[alpha - beta])/(MHp2*vev) - (2*SelfEnergy[S[5] -> S[6], MHp])/MHp2 + (2*dTHH1*Sin[alpha - beta])/(MHp2*vev)

RenConst[dZGpGp1] := FieldRC[S[6]]

RenConst[dalpha1] := (dZHHHL1 - dZHLHH1)/4

RenConst[dbeta1] := (dZG0HA1 - dZHAG01)/4