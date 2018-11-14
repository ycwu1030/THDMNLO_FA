RenConst[dCW1] := (CW*(dMWsq1/MW2 - dMZsq1/MZ2))/2

RenConst[dSW1] := (CW^2*(-(dMWsq1/MW2) + dMZsq1/MZ2))/(2*SW)

RenConst[dvev1] := (-dZe1 + dMWsq1/(2*MW2) + dSW1/SW)*vev

RenConst[dTHL1] := -TadpoleRC[S[1]]

RenConst[dTHH1] := -TadpoleRC[S[2]]

RenConst[dMHL21] := -(Csc[2*beta]*(dTHL1*Cos[3*alpha - beta] + 3*dTHL1*Cos[alpha + beta] + dTHH1*Sin[3*alpha - beta] - 4*vev*Cos[beta]*MassRC[S[1]]*Sin[beta] + dTHH1*Sin[alpha + beta]))/(2*vev)

RenConst[dMHH21] := (dTHL1*Cos[3*alpha - beta]*Csc[2*beta] - dTHL1*Cos[alpha + beta]*Csc[2*beta] + 2*vev*MassRC[S[2]] + dTHH1*Csc[2*beta]*Sin[3*alpha - beta] - 3*dTHH1*Csc[2*beta]*Sin[alpha + beta])/(2*vev)

RenConst[dZHLHL1] := FieldRC[S[1]]

RenConst[dZHLHH1] := (-((-MHH2 + MHL2)*vev*FieldRC[S[1], S[2]]) + dTHL1*Cos[alpha - beta]*Csc[beta]*Sec[beta]*Sin[2*alpha] + dTHH1*Csc[beta]*Sec[beta]*Sin[2*alpha]*Sin[alpha - beta])/((MHH2 - MHL2)*vev)

RenConst[dZHHHL1] := ((MHH2 - MHL2)*vev*FieldRC[S[2], S[1]] - dTHL1*Cos[alpha - beta]*Csc[beta]*Sec[beta]*Sin[2*alpha] - dTHH1*Csc[beta]*Sec[beta]*Sin[2*alpha]*Sin[alpha - beta])/((MHH2 - MHL2)*vev)

RenConst[dZHHHH1] := FieldRC[S[2]]

RenConst[dMHA21] := (-(dTHL1*Cos[alpha - 3*beta]*Csc[2*beta]) - 3*dTHL1*Cos[alpha + beta]*Csc[2*beta] + 2*vev*MassRC[S[3]] - dTHH1*Csc[2*beta]*Sin[alpha - 3*beta] - 3*dTHH1*Csc[2*beta]*Sin[alpha + beta])/(2*vev)

RenConst[dZHAHA1] := FieldRC[S[3]]

RenConst[dZHAG01] := (-2*(dTHL1*Cos[alpha - beta] - (MHA2*vev*FieldRC[S[3], S[4]])/2 + dTHH1*Sin[alpha - beta]))/(MHA2*vev)

RenConst[dZG0HA1] := (2*(dTHL1*Cos[alpha - beta] + (MHA2*vev*FieldRC[S[4], S[3]])/2 + dTHH1*Sin[alpha - beta]))/(MHA2*vev)

RenConst[dZG0G01] := FieldRC[S[4]]

RenConst[dMHp21] := (-(dTHL1*Cos[alpha - 3*beta]*Csc[2*beta]) - 3*dTHL1*Cos[alpha + beta]*Csc[2*beta] + 2*vev*MassRC[S[5]] - dTHH1*Csc[2*beta]*Sin[alpha - 3*beta] - 3*dTHH1*Csc[2*beta]*Sin[alpha + beta])/(2*vev)

RenConst[dZHpHp1] := FieldRC[S[5]]

RenConst[dZHpGp1] := (-2*(dTHL1*Cos[alpha - beta] - (MHp2*vev*FieldRC[S[5], S[6]])/2 + dTHH1*Sin[alpha - beta]))/(MHp2*vev)

RenConst[dZGpHp1] := (2*(dTHL1*Cos[alpha - beta] + (MHp2*vev*FieldRC[S[6], S[5]])/2 + dTHH1*Sin[alpha - beta]))/(MHp2*vev)

RenConst[dZGpGp1] := FieldRC[S[6]]

RenConst[dalpha1] := (dZHHHL1 - dZHLHH1)/4

RenConst[dbeta1] := (dZG0HA1 - dZHAG01)/4