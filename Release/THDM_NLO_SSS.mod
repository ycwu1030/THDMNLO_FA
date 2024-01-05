M$CouplingMatrices = Join[ M$CouplingMatrices, {
C[S[4], S[4], S[2]]==
	{{((-I)*MHH2*Cos[alpha - beta])/vev, (I*dvev1*MHH2*Cos[alpha - beta])/vev^2 - (I*dMHH21*Cos[alpha - beta])/vev - (I*dZG0G01*MHH2*Cos[alpha - beta])/vev - ((I/2)*dZHHHH1*MHH2*Cos[alpha - beta])/vev + (I*dZHAG01*(MHA2 - MHH2)*Sin[alpha - beta])/vev + (I*dalpha1*MHH2*Sin[alpha - beta])/vev - (I*dbeta1*MHH2*Sin[alpha - beta])/vev + ((I/2)*dZHLHH1*MHL2*Sin[alpha - beta])/vev}},
C[S[4], S[4], S[1]]==
	{{(I*MHL2*Sin[alpha - beta])/vev, ((-I/2)*dZHHHL1*MHH2*Cos[alpha - beta])/vev + (I*dZHAG01*(MHA2 - MHL2)*Cos[alpha - beta])/vev + (I*dalpha1*MHL2*Cos[alpha - beta])/vev - (I*dbeta1*MHL2*Cos[alpha - beta])/vev - (I*dvev1*MHL2*Sin[alpha - beta])/vev^2 + (I*dMHL21*Sin[alpha - beta])/vev + (I*dZG0G01*MHL2*Sin[alpha - beta])/vev + ((I/2)*dZHLHL1*MHL2*Sin[alpha - beta])/vev}},
C[S[4], S[6], -S[5]]==
	{{0, (dZHAG01*(-MHA2 + MHp2))/(2*vev)}},
C[S[4], -S[6], S[5]]==
	{{0, (dZHAG01*(MHA2 - MHp2))/(2*vev)}},
C[S[4], S[3], S[2]]==
	{{(I*MHA2*Sin[alpha - beta])/vev - (I*MHH2*Sin[alpha - beta])/vev, (I*dalpha1*(MHA2 - MHH2)*Cos[alpha - beta])/vev - (I*dbeta1*(MHA2 - MHH2)*Cos[alpha - beta])/vev - ((I/2)*dZG0HA1*MHH2*Cos[alpha - beta])/vev + ((I/2)*dZHLHH1*(MHA2 - MHL2)*Cos[alpha - beta])/vev - (I*dvev1*(MHA2 - MHH2)*Sin[alpha - beta])/vev^2 + (I*dMHA21*Sin[alpha - beta])/vev - (I*dMHH21*Sin[alpha - beta])/vev + ((I/2)*dZG0G01*(MHA2 - MHH2)*Sin[alpha - beta])/vev + ((I/2)*dZHAHA1*(MHA2 - MHH2)*Sin[alpha - beta])/vev + ((I/2)*dZHHHH1*(MHA2 - MHH2)*Sin[alpha - beta])/vev + ((I/8)*dZHAG01*Csc[beta]*Sec[beta]*((2*MHA2 - MHH2)*Sin[alpha - 3*beta] + (4*M2 - 2*MHA2 - 3*MHH2)*Sin[alpha + beta]))/vev}},
C[S[4], S[3], S[1]]==
	{{(I*MHA2*Cos[alpha - beta])/vev - (I*MHL2*Cos[alpha - beta])/vev, ((-I)*dvev1*(MHA2 - MHL2)*Cos[alpha - beta])/vev^2 + (I*dMHA21*Cos[alpha - beta])/vev - (I*dMHL21*Cos[alpha - beta])/vev + ((I/2)*dZG0G01*(MHA2 - MHL2)*Cos[alpha - beta])/vev + ((I/2)*dZHAHA1*(MHA2 - MHL2)*Cos[alpha - beta])/vev + ((I/2)*dZHLHL1*(MHA2 - MHL2)*Cos[alpha - beta])/vev + ((I/8)*dZHAG01*((2*MHA2 - MHL2)*Cos[alpha - 3*beta] + (4*M2 - 2*MHA2 - 3*MHL2)*Cos[alpha + beta])*Csc[beta]*Sec[beta])/vev + ((I/2)*dZHHHL1*(MHA2 - MHH2)*Sin[alpha - beta])/vev - (I*dalpha1*(MHA2 - MHL2)*Sin[alpha - beta])/vev + (I*dbeta1*(MHA2 - MHL2)*Sin[alpha - beta])/vev + ((I/2)*dZG0HA1*MHL2*Sin[alpha - beta])/vev}},
C[S[6], -S[6], S[3]]==
	{{0, 0}},
C[S[6], -S[6], S[2]]==
	{{((-I)*MHH2*Cos[alpha - beta])/vev, (I*dvev1*MHH2*Cos[alpha - beta])/vev^2 - (I*dMHH21*Cos[alpha - beta])/vev - (I*dZGpGp1*MHH2*Cos[alpha - beta])/vev - ((I/2)*dZHHHH1*MHH2*Cos[alpha - beta])/vev + (I*dalpha1*MHH2*Sin[alpha - beta])/vev - (I*dbeta1*MHH2*Sin[alpha - beta])/vev + ((I/2)*dZHLHH1*MHL2*Sin[alpha - beta])/vev - (I*dZHpGp1*(MHH2 - MHp2)*Sin[alpha - beta])/vev}},
C[S[6], -S[6], S[1]]==
	{{(I*MHL2*Sin[alpha - beta])/vev, ((-I/2)*dZHHHL1*MHH2*Cos[alpha - beta])/vev + (I*dalpha1*MHL2*Cos[alpha - beta])/vev - (I*dbeta1*MHL2*Cos[alpha - beta])/vev - (I*dZHpGp1*(MHL2 - MHp2)*Cos[alpha - beta])/vev - (I*dvev1*MHL2*Sin[alpha - beta])/vev^2 + (I*dMHL21*Sin[alpha - beta])/vev + (I*dZGpGp1*MHL2*Sin[alpha - beta])/vev + ((I/2)*dZHLHL1*MHL2*Sin[alpha - beta])/vev}},
C[S[6], S[3], -S[5]]==
	{{-(MHA2/vev) + MHp2/vev, (dvev1*(MHA2 - MHp2))/vev^2 - dMHA21/vev + dMHp21/vev + (dZGpGp1*(-MHA2 + MHp2))/(2*vev) + (dZHAHA1*(-MHA2 + MHp2))/(2*vev) + (dZHpHp1*(-MHA2 + MHp2))/(2*vev)}},
C[S[6], S[2], -S[5]]==
	{{((-I)*MHH2*Sin[alpha - beta])/vev + (I*MHp2*Sin[alpha - beta])/vev, ((-I/2)*dZGpHp1*MHH2*Cos[alpha - beta])/vev - (I*dalpha1*(MHH2 - MHp2)*Cos[alpha - beta])/vev + (I*dbeta1*(MHH2 - MHp2)*Cos[alpha - beta])/vev - ((I/2)*dZHLHH1*(MHL2 - MHp2)*Cos[alpha - beta])/vev + (I*dvev1*(MHH2 - MHp2)*Sin[alpha - beta])/vev^2 - (I*dMHH21*Sin[alpha - beta])/vev + (I*dMHp21*Sin[alpha - beta])/vev - ((I/2)*dZGpGp1*(MHH2 - MHp2)*Sin[alpha - beta])/vev - ((I/2)*dZHHHH1*(MHH2 - MHp2)*Sin[alpha - beta])/vev - ((I/2)*dZHpHp1*(MHH2 - MHp2)*Sin[alpha - beta])/vev - ((I/8)*dZHpGp1*Csc[beta]*Sec[beta]*((MHH2 - 2*MHp2)*Sin[alpha - 3*beta] + (-4*M2 + 3*MHH2 + 2*MHp2)*Sin[alpha + beta]))/vev}},
C[S[6], S[1], -S[5]]==
	{{((-I)*MHL2*Cos[alpha - beta])/vev + (I*MHp2*Cos[alpha - beta])/vev, (I*dvev1*(MHL2 - MHp2)*Cos[alpha - beta])/vev^2 - (I*dMHL21*Cos[alpha - beta])/vev + (I*dMHp21*Cos[alpha - beta])/vev - ((I/2)*dZGpGp1*(MHL2 - MHp2)*Cos[alpha - beta])/vev - ((I/2)*dZHLHL1*(MHL2 - MHp2)*Cos[alpha - beta])/vev - ((I/2)*dZHpHp1*(MHL2 - MHp2)*Cos[alpha - beta])/vev - ((I/8)*dZHpGp1*((MHL2 - 2*MHp2)*Cos[alpha - 3*beta] + (-4*M2 + 3*MHL2 + 2*MHp2)*Cos[alpha + beta])*Csc[beta]*Sec[beta])/vev + ((I/2)*dZGpHp1*MHL2*Sin[alpha - beta])/vev - ((I/2)*dZHHHL1*(MHH2 - MHp2)*Sin[alpha - beta])/vev + (I*dalpha1*(MHL2 - MHp2)*Sin[alpha - beta])/vev - (I*dbeta1*(MHL2 - MHp2)*Sin[alpha - beta])/vev}},
C[-S[6], S[3], S[5]]==
	{{MHA2/vev - MHp2/vev, (dvev1*(-MHA2 + MHp2))/vev^2 + dMHA21/vev - dMHp21/vev + (dZGpGp1*(MHA2 - MHp2))/(2*vev) + (dZHAHA1*(MHA2 - MHp2))/(2*vev) + (dZHpHp1*(MHA2 - MHp2))/(2*vev)}},
C[-S[6], S[2], S[5]]==
	{{((-I)*MHH2*Sin[alpha - beta])/vev + (I*MHp2*Sin[alpha - beta])/vev, ((-I/2)*dZGpHp1*MHH2*Cos[alpha - beta])/vev - (I*dalpha1*(MHH2 - MHp2)*Cos[alpha - beta])/vev + (I*dbeta1*(MHH2 - MHp2)*Cos[alpha - beta])/vev - ((I/2)*dZHLHH1*(MHL2 - MHp2)*Cos[alpha - beta])/vev + (I*dvev1*(MHH2 - MHp2)*Sin[alpha - beta])/vev^2 - (I*dMHH21*Sin[alpha - beta])/vev + (I*dMHp21*Sin[alpha - beta])/vev - ((I/2)*dZGpGp1*(MHH2 - MHp2)*Sin[alpha - beta])/vev - ((I/2)*dZHHHH1*(MHH2 - MHp2)*Sin[alpha - beta])/vev - ((I/2)*dZHpHp1*(MHH2 - MHp2)*Sin[alpha - beta])/vev - ((I/8)*dZHpGp1*Csc[beta]*Sec[beta]*((MHH2 - 2*MHp2)*Sin[alpha - 3*beta] + (-4*M2 + 3*MHH2 + 2*MHp2)*Sin[alpha + beta]))/vev}},
C[-S[6], S[1], S[5]]==
	{{((-I)*MHL2*Cos[alpha - beta])/vev + (I*MHp2*Cos[alpha - beta])/vev, (I*dvev1*(MHL2 - MHp2)*Cos[alpha - beta])/vev^2 - (I*dMHL21*Cos[alpha - beta])/vev + (I*dMHp21*Cos[alpha - beta])/vev - ((I/2)*dZGpGp1*(MHL2 - MHp2)*Cos[alpha - beta])/vev - ((I/2)*dZHLHL1*(MHL2 - MHp2)*Cos[alpha - beta])/vev - ((I/2)*dZHpHp1*(MHL2 - MHp2)*Cos[alpha - beta])/vev - ((I/8)*dZHpGp1*((MHL2 - 2*MHp2)*Cos[alpha - 3*beta] + (-4*M2 + 3*MHL2 + 2*MHp2)*Cos[alpha + beta])*Csc[beta]*Sec[beta])/vev + ((I/2)*dZGpHp1*MHL2*Sin[alpha - beta])/vev - ((I/2)*dZHHHL1*(MHH2 - MHp2)*Sin[alpha - beta])/vev + (I*dalpha1*(MHL2 - MHp2)*Sin[alpha - beta])/vev - (I*dbeta1*(MHL2 - MHp2)*Sin[alpha - beta])/vev}},
C[S[3], S[3], S[2]]==
	{{((-2*I)*MHA2*Cos[alpha - beta])/vev + (I*M2*Csc[beta]*Sec[beta]*Sin[alpha + beta])/vev - ((I/4)*MHH2*Csc[beta]*Sec[beta]*(Sin[alpha - 3*beta] + 3*Sin[alpha + beta]))/vev, ((-2*I)*dMHA21*Cos[alpha - beta])/vev + ((I/4)*dalpha1*((2*MHA2 - MHH2)*Cos[alpha - 3*beta] + (4*M2 - 2*MHA2 - 3*MHH2)*Cos[alpha + beta])*Csc[beta]*Sec[beta])/vev + ((I/8)*dZHLHH1*((2*MHA2 - MHL2)*Cos[alpha - 3*beta] + (4*M2 - 2*MHA2 - 3*MHL2)*Cos[alpha + beta])*Csc[beta]*Sec[beta])/vev + (I*dZG0HA1*(MHA2 - MHH2)*Sin[alpha - beta])/vev - ((I/4)*dMHH21*Csc[beta]*Sec[beta]*(Sin[alpha - 3*beta] + 3*Sin[alpha + beta]))/vev + ((I/4)*dZHAHA1*Csc[beta]*Sec[beta]*((2*MHA2 - MHH2)*Sin[alpha - 3*beta] + (4*M2 - 2*MHA2 - 3*MHH2)*Sin[alpha + beta]))/vev + ((I/8)*dZHHHH1*Csc[beta]*Sec[beta]*((2*MHA2 - MHH2)*Sin[alpha - 3*beta] + (4*M2 - 2*MHA2 - 3*MHH2)*Sin[alpha + beta]))/vev + ((I/4)*Csc[beta]*Sec[beta]*(dvev1*(-2*MHA2 + MHH2)*Sin[alpha - 3*beta] + (dvev1*(-4*M2 + 2*MHA2 + 3*MHH2) + 4*dM21*vev)*Sin[alpha + beta]))/vev^2 + ((I/4)*dbeta1*Csc[2*beta]^2*((2*MHA2 - MHH2)*Sin[alpha - 5*beta] - 2*(6*M2 + 2*MHA2 - 7*MHH2)*Sin[alpha - beta] + (-4*M2 + 2*MHA2 + 3*MHH2)*Sin[alpha + 3*beta]))/vev}},
C[S[3], S[3], S[1]]==
	{{(I*M2*Cos[alpha + beta]*Csc[beta]*Sec[beta])/vev - ((I/4)*MHL2*(Cos[alpha - 3*beta] + 3*Cos[alpha + beta])*Csc[beta]*Sec[beta])/vev + ((2*I)*MHA2*Sin[alpha - beta])/vev, (I*dZG0HA1*(MHA2 - MHL2)*Cos[alpha - beta])/vev - ((I/4)*dMHL21*(Cos[alpha - 3*beta] + 3*Cos[alpha + beta])*Csc[beta]*Sec[beta])/vev + ((I/4)*dZHAHA1*((2*MHA2 - MHL2)*Cos[alpha - 3*beta] + (4*M2 - 2*MHA2 - 3*MHL2)*Cos[alpha + beta])*Csc[beta]*Sec[beta])/vev + ((I/8)*dZHLHL1*((2*MHA2 - MHL2)*Cos[alpha - 3*beta] + (4*M2 - 2*MHA2 - 3*MHL2)*Cos[alpha + beta])*Csc[beta]*Sec[beta])/vev + ((I/4)*(dvev1*(-2*MHA2 + MHL2)*Cos[alpha - 3*beta] + (dvev1*(-4*M2 + 2*MHA2 + 3*MHL2) + 4*dM21*vev)*Cos[alpha + beta])*Csc[beta]*Sec[beta])/vev^2 + ((I/16)*dbeta1*((2*MHA2 - MHL2)*Cos[alpha - 5*beta] - 2*(6*M2 + 2*MHA2 - 7*MHL2)*Cos[alpha - beta] + (-4*M2 + 2*MHA2 + 3*MHL2)*Cos[alpha + 3*beta])*Csc[beta]^2*Sec[beta]^2)/vev + ((2*I)*dMHA21*Sin[alpha - beta])/vev + ((I/8)*dZHHHL1*Csc[beta]*Sec[beta]*((2*MHA2 - MHH2)*Sin[alpha - 3*beta] + (4*M2 - 2*MHA2 - 3*MHH2)*Sin[alpha + beta]))/vev - ((I/4)*dalpha1*Csc[beta]*Sec[beta]*((2*MHA2 - MHL2)*Sin[alpha - 3*beta] + (4*M2 - 2*MHA2 - 3*MHL2)*Sin[alpha + beta]))/vev}},
C[S[3], S[5], -S[5]]==
	{{0, 0}},
C[S[2], S[2], S[2]]==
	{{(((3*I)/4)*MHH2*Csc[beta]*Sec[beta]*(Sin[3*alpha - beta] - 3*Sin[alpha + beta]))/vev + ((3*I)*M2*Csc[beta]*Sec[beta]*Sin[alpha - beta]^2*Sin[alpha + beta])/vev, (((3*I)/8)*dbeta1*(-5*M2 + 4*MHH2 + (M2 - MHH2)*Cos[2*alpha - 2*beta] + M2*Cos[4*beta] + 3*M2*Cos[2*(alpha + beta)] - 3*MHH2*Cos[2*(alpha + beta)])*Csc[beta]^2*Sec[beta]^2*Sin[alpha - beta])/vev + ((3*I)*dalpha1*(M2 + 3*(M2 - MHH2)*Csc[2*beta]*Sin[2*alpha])*Sin[alpha - beta])/vev + (((3*I)/2)*dZHLHH1*Csc[2*beta]*Sin[alpha - beta]*((3*M2 - 2*MHH2 - MHL2)*Sin[2*alpha] + M2*Sin[2*beta]))/vev + (((3*I)/4)*dMHH21*Csc[beta]*Sec[beta]*(Sin[3*alpha - beta] - 3*Sin[alpha + beta]))/vev + (((9*I)/8)*dZHHHH1*Csc[beta]*Sec[beta]*(M2*Sin[alpha - 3*beta] + (-M2 + MHH2)*Sin[3*alpha - beta] + (2*M2 - 3*MHH2)*Sin[alpha + beta]))/vev - (((3*I)/4)*Csc[beta]*Sec[beta]*((dvev1*M2 - dM21*vev)*Sin[alpha - 3*beta] + (-(dvev1*M2) + dvev1*MHH2 + dM21*vev)*Sin[3*alpha - beta] + (2*dvev1*M2 - 3*dvev1*MHH2 - 2*dM21*vev)*Sin[alpha + beta]))/vev^2}},
C[S[2], S[2], S[1]]==
	{{((I/4)*M2*(Cos[alpha - 3*beta] - 3*Cos[3*alpha - beta] + 2*Cos[alpha + beta])*Csc[beta]*Sec[beta])/vev - (I*MHL2*Csc[2*beta]*Sin[2*alpha]*Sin[alpha - beta])/vev - (I*MHH2*Csc[beta]*Sec[beta]*Sin[2*alpha]*Sin[alpha - beta])/vev, ((I/16)*dbeta1*(M2*Cos[alpha - 5*beta] + (3*M2 - 2*MHH2 - MHL2)*Cos[3*alpha - 3*beta] - 11*M2*Cos[alpha - beta] + 6*MHH2*Cos[alpha - beta] + 3*MHL2*Cos[alpha - beta] + 9*M2*Cos[3*alpha + beta] - 6*MHH2*Cos[3*alpha + beta] - 3*MHL2*Cos[3*alpha + beta] - 2*M2*Cos[alpha + 3*beta] + 2*MHH2*Cos[alpha + 3*beta] + MHL2*Cos[alpha + 3*beta])*Csc[beta]^2*Sec[beta]^2)/vev - (I*dMHL21*Csc[2*beta]*Sin[2*alpha]*Sin[alpha - beta])/vev - (I*dMHH21*Csc[beta]*Sec[beta]*Sin[2*alpha]*Sin[alpha - beta])/vev + ((I/2)*dZHLHH1*Cos[alpha - beta]*Csc[beta]*Sec[beta]*((3*M2 - MHH2 - 2*MHL2)*Sin[2*alpha] - M2*Sin[2*beta]))/vev + (I*dZHHHH1*Csc[2*beta]*Sin[alpha - beta]*((3*M2 - 2*MHH2 - MHL2)*Sin[2*alpha] + M2*Sin[2*beta]))/vev + ((I/2)*dZHLHL1*Csc[2*beta]*Sin[alpha - beta]*((3*M2 - 2*MHH2 - MHL2)*Sin[2*alpha] + M2*Sin[2*beta]))/vev - (I*Csc[2*beta]*Sin[alpha - beta]*((dvev1*(3*M2 - 2*MHH2 - MHL2) - 3*dM21*vev)*Sin[2*alpha] + (dvev1*M2 - dM21*vev)*Sin[2*beta]))/vev^2 + (((3*I)/8)*dZHHHL1*Csc[beta]*Sec[beta]*(M2*Sin[alpha - 3*beta] + (-M2 + MHH2)*Sin[3*alpha - beta] + (2*M2 - 3*MHH2)*Sin[alpha + beta]))/vev - ((I/4)*dalpha1*Csc[beta]*Sec[beta]*(M2*Sin[alpha - 3*beta] + (-9*M2 + 6*MHH2 + 3*MHL2)*Sin[3*alpha - beta] + (2*M2 - 2*MHH2 - MHL2)*Sin[alpha + beta]))/vev}},
C[S[2], S[1], S[1]]==
	{{((-I)*MHH2*Cos[alpha]*Sin[alpha]*(Cos[alpha]*Csc[beta] + Sec[beta]*Sin[alpha]))/vev - (I*MHL2*Cos[alpha - beta]*Csc[beta]*Sec[beta]*Sin[2*alpha])/vev + ((I/4)*M2*Csc[beta]*Sec[beta]*(Sin[alpha - 3*beta] + 3*Sin[3*alpha - beta] + 2*Sin[alpha + beta]))/vev, (((3*I)/8)*dZHLHH1*(M2*Cos[alpha - 3*beta] + (M2 - MHL2)*Cos[3*alpha - beta] + (2*M2 - 3*MHL2)*Cos[alpha + beta])*Csc[beta]*Sec[beta])/vev + ((I/4)*dalpha1*(M2*Cos[alpha - 3*beta] + (9*M2 - 3*MHH2 - 6*MHL2)*Cos[3*alpha - beta] + (2*M2 - MHH2 - 2*MHL2)*Cos[alpha + beta])*Csc[beta]*Sec[beta])/vev - (I*dMHH21*Cos[alpha]*Sin[alpha]*(Cos[alpha]*Csc[beta] + Sec[beta]*Sin[alpha]))/vev - (I*dMHL21*Cos[alpha - beta]*Csc[beta]*Sec[beta]*Sin[2*alpha])/vev + ((I/4)*dZHHHH1*Cos[alpha - beta]*Csc[beta]*Sec[beta]*((3*M2 - MHH2 - 2*MHL2)*Sin[2*alpha] - M2*Sin[2*beta]))/vev + ((I/2)*dZHLHL1*Cos[alpha - beta]*Csc[beta]*Sec[beta]*((3*M2 - MHH2 - 2*MHL2)*Sin[2*alpha] - M2*Sin[2*beta]))/vev + (I*dZHHHL1*Csc[2*beta]*Sin[alpha - beta]*((3*M2 - 2*MHH2 - MHL2)*Sin[2*alpha] + M2*Sin[2*beta]))/vev + ((I/2)*Cos[alpha - beta]*Csc[beta]*Sec[beta]*((dvev1*(-3*M2 + MHH2 + 2*MHL2) + 3*dM21*vev)*Sin[2*alpha] + (dvev1*M2 - dM21*vev)*Sin[2*beta]))/vev^2 + ((I/4)*dbeta1*Csc[2*beta]^2*(M2*Sin[alpha - 5*beta] + (-3*M2 + MHH2 + 2*MHL2)*Sin[3*alpha - 3*beta] - 11*M2*Sin[alpha - beta] + 3*MHH2*Sin[alpha - beta] + 6*MHL2*Sin[alpha - beta] - 9*M2*Sin[3*alpha + beta] + 3*MHH2*Sin[3*alpha + beta] + 6*MHL2*Sin[3*alpha + beta] - 2*M2*Sin[alpha + 3*beta] + MHH2*Sin[alpha + 3*beta] + 2*MHL2*Sin[alpha + 3*beta]))/vev}},
C[S[2], S[5], -S[5]]==
	{{((-2*I)*MHp2*Cos[alpha - beta])/vev + (I*M2*Csc[beta]*Sec[beta]*Sin[alpha + beta])/vev - ((I/4)*MHH2*Csc[beta]*Sec[beta]*(Sin[alpha - 3*beta] + 3*Sin[alpha + beta]))/vev, ((-2*I)*dMHp21*Cos[alpha - beta])/vev - ((I/4)*dalpha1*((MHH2 - 2*MHp2)*Cos[alpha - 3*beta] + (-4*M2 + 3*MHH2 + 2*MHp2)*Cos[alpha + beta])*Csc[beta]*Sec[beta])/vev - ((I/8)*dZHLHH1*((MHL2 - 2*MHp2)*Cos[alpha - 3*beta] + (-4*M2 + 3*MHL2 + 2*MHp2)*Cos[alpha + beta])*Csc[beta]*Sec[beta])/vev - (I*dZGpHp1*(MHH2 - MHp2)*Sin[alpha - beta])/vev - ((I/4)*dMHH21*Csc[beta]*Sec[beta]*(Sin[alpha - 3*beta] + 3*Sin[alpha + beta]))/vev - ((I/8)*dZHHHH1*Csc[beta]*Sec[beta]*((MHH2 - 2*MHp2)*Sin[alpha - 3*beta] + (-4*M2 + 3*MHH2 + 2*MHp2)*Sin[alpha + beta]))/vev - ((I/4)*dZHpHp1*Csc[beta]*Sec[beta]*((MHH2 - 2*MHp2)*Sin[alpha - 3*beta] + (-4*M2 + 3*MHH2 + 2*MHp2)*Sin[alpha + beta]))/vev + ((I/4)*Csc[beta]*Sec[beta]*(dvev1*(MHH2 - 2*MHp2)*Sin[alpha - 3*beta] + (dvev1*(-4*M2 + 3*MHH2 + 2*MHp2) + 4*dM21*vev)*Sin[alpha + beta]))/vev^2 - ((I/4)*dbeta1*Csc[2*beta]^2*((MHH2 - 2*MHp2)*Sin[alpha - 5*beta] + 2*(6*M2 - 7*MHH2 + 2*MHp2)*Sin[alpha - beta] + (4*M2 - 3*MHH2 - 2*MHp2)*Sin[alpha + 3*beta]))/vev}},
C[S[1], S[1], S[1]]==
	{{((3*I)*M2*Cos[alpha - beta]^2*Cos[alpha + beta]*Csc[beta]*Sec[beta])/vev - (((3*I)/4)*MHL2*(Cos[3*alpha - beta] + 3*Cos[alpha + beta])*Csc[beta]*Sec[beta])/vev, (((-3*I)/4)*dMHL21*(Cos[3*alpha - beta] + 3*Cos[alpha + beta])*Csc[beta]*Sec[beta])/vev + (((9*I)/8)*dZHLHL1*(M2*Cos[alpha - 3*beta] + (M2 - MHL2)*Cos[3*alpha - beta] + (2*M2 - 3*MHL2)*Cos[alpha + beta])*Csc[beta]*Sec[beta])/vev + (((3*I)/4)*((-(dvev1*M2) + dM21*vev)*Cos[alpha - 3*beta] + (-(dvev1*M2) + dvev1*MHL2 + dM21*vev)*Cos[3*alpha - beta] + (-2*dvev1*M2 + 3*dvev1*MHL2 + 2*dM21*vev)*Cos[alpha + beta])*Csc[beta]*Sec[beta])/vev^2 - (((3*I)/8)*dbeta1*Cos[alpha - beta]*(5*M2 - 4*MHL2 + (M2 - MHL2)*Cos[2*alpha - 2*beta] - M2*Cos[4*beta] + 3*M2*Cos[2*(alpha + beta)] - 3*MHL2*Cos[2*(alpha + beta)])*Csc[beta]^2*Sec[beta]^2)/vev + (((3*I)/4)*dZHHHL1*Cos[alpha - beta]*Csc[beta]*Sec[beta]*((3*M2 - MHH2 - 2*MHL2)*Sin[2*alpha] - M2*Sin[2*beta]))/vev - (((3*I)/2)*dalpha1*Cos[alpha - beta]*Csc[beta]*Sec[beta]*(3*(M2 - MHL2)*Sin[2*alpha] - M2*Sin[2*beta]))/vev}},
C[S[1], S[5], -S[5]]==
	{{(I*M2*Cos[alpha + beta]*Csc[beta]*Sec[beta])/vev - ((I/4)*MHL2*(Cos[alpha - 3*beta] + 3*Cos[alpha + beta])*Csc[beta]*Sec[beta])/vev + ((2*I)*MHp2*Sin[alpha - beta])/vev, ((-I)*dZGpHp1*(MHL2 - MHp2)*Cos[alpha - beta])/vev - ((I/4)*dMHL21*(Cos[alpha - 3*beta] + 3*Cos[alpha + beta])*Csc[beta]*Sec[beta])/vev - ((I/8)*dZHLHL1*((MHL2 - 2*MHp2)*Cos[alpha - 3*beta] + (-4*M2 + 3*MHL2 + 2*MHp2)*Cos[alpha + beta])*Csc[beta]*Sec[beta])/vev - ((I/4)*dZHpHp1*((MHL2 - 2*MHp2)*Cos[alpha - 3*beta] + (-4*M2 + 3*MHL2 + 2*MHp2)*Cos[alpha + beta])*Csc[beta]*Sec[beta])/vev + ((I/4)*(dvev1*(MHL2 - 2*MHp2)*Cos[alpha - 3*beta] + (dvev1*(-4*M2 + 3*MHL2 + 2*MHp2) + 4*dM21*vev)*Cos[alpha + beta])*Csc[beta]*Sec[beta])/vev^2 - ((I/16)*dbeta1*((MHL2 - 2*MHp2)*Cos[alpha - 5*beta] + 2*(6*M2 - 7*MHL2 + 2*MHp2)*Cos[alpha - beta] + (4*M2 - 3*MHL2 - 2*MHp2)*Cos[alpha + 3*beta])*Csc[beta]^2*Sec[beta]^2)/vev + ((2*I)*dMHp21*Sin[alpha - beta])/vev - ((I/8)*dZHHHL1*Csc[beta]*Sec[beta]*((MHH2 - 2*MHp2)*Sin[alpha - 3*beta] + (-4*M2 + 3*MHH2 + 2*MHp2)*Sin[alpha + beta]))/vev + ((I/4)*dalpha1*Csc[beta]*Sec[beta]*((MHL2 - 2*MHp2)*Sin[alpha - 3*beta] + (-4*M2 + 3*MHL2 + 2*MHp2)*Sin[alpha + beta]))/vev}}
}];