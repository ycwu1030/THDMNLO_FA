M$CouplingMatrices = Join[ M$CouplingMatrices, {
C[S[3], S[1], V[2]]==
	{{(EL*Cos[alpha - beta])/(2*CW*SW), (dZe1*EL*Cos[alpha - beta])/(2*CW*SW) + (dZHAHA1*EL*Cos[alpha - beta])/(4*CW*SW) + (dZHLHL1*EL*Cos[alpha - beta])/(4*CW*SW) + (dZZZ1*EL*Cos[alpha - beta])/(4*CW*SW) - (dalpha1*EL*Sin[alpha - beta])/(2*CW*SW) - (dZG0HA1*EL*Sin[alpha - beta])/(4*CW*SW) + (dZHHHL1*EL*Sin[alpha - beta])/(4*CW*SW) + (EL*((CW*dSW1 - dCW1*SW)*(-1 + 2*SW^2)*Cos[alpha - beta] + CW*dbeta1*SW*Sin[alpha - beta]))/(2*CW^2*SW^2)}},
C[S[3], S[1], V[1]]==
	{{0, (dZZA1*EL*Cos[alpha - beta])/(4*CW*SW)}},
C[S[1], -S[5], V[3]]==
	{{((-I/2)*EL*Cos[alpha - beta])/SW, ((-I/2)*dZe1*EL*Cos[alpha - beta])/SW - ((I/4)*dZHLHL1*EL*Cos[alpha - beta])/SW - ((I/4)*dZHpHp1*EL*Cos[alpha - beta])/SW - ((I/4)*dZWW1*EL*Cos[alpha - beta])/SW + ((I/2)*dalpha1*EL*Sin[alpha - beta])/SW + ((I/4)*dZGpHp1*EL*Sin[alpha - beta])/SW - ((I/4)*dZHHHL1*EL*Sin[alpha - beta])/SW + ((I/2)*EL*(dSW1*Cos[alpha - beta] - dbeta1*SW*Sin[alpha - beta]))/SW^2}},
C[S[1], S[5], -V[3]]==
	{{((I/2)*EL*Cos[alpha - beta])/SW, ((I/2)*dZe1*EL*Cos[alpha - beta])/SW + ((I/4)*dZHLHL1*EL*Cos[alpha - beta])/SW + ((I/4)*dZHpHp1*EL*Cos[alpha - beta])/SW + ((I/4)*dZWW1*EL*Cos[alpha - beta])/SW - ((I/2)*dalpha1*EL*Sin[alpha - beta])/SW - ((I/4)*dZGpHp1*EL*Sin[alpha - beta])/SW + ((I/4)*dZHHHL1*EL*Sin[alpha - beta])/SW + ((I/2)*EL*(-(dSW1*Cos[alpha - beta]) + dbeta1*SW*Sin[alpha - beta]))/SW^2}},
C[-S[6], S[1], V[3]]==
	{{((-I/2)*EL*Sin[alpha - beta])/SW, ((-I/2)*dalpha1*EL*Cos[alpha - beta])/SW + ((I/4)*dZHHHL1*EL*Cos[alpha - beta])/SW + ((I/4)*dZHpGp1*EL*Cos[alpha - beta])/SW - ((I/2)*dZe1*EL*Sin[alpha - beta])/SW - ((I/4)*dZGpGp1*EL*Sin[alpha - beta])/SW - ((I/4)*dZHLHL1*EL*Sin[alpha - beta])/SW - ((I/4)*dZWW1*EL*Sin[alpha - beta])/SW + ((I/2)*EL*(dbeta1*SW*Cos[alpha - beta] + dSW1*Sin[alpha - beta]))/SW^2}},
C[S[6], S[1], -V[3]]==
	{{((I/2)*EL*Sin[alpha - beta])/SW, ((I/2)*dalpha1*EL*Cos[alpha - beta])/SW - ((I/4)*dZHHHL1*EL*Cos[alpha - beta])/SW - ((I/4)*dZHpGp1*EL*Cos[alpha - beta])/SW + ((I/2)*dZe1*EL*Sin[alpha - beta])/SW + ((I/4)*dZGpGp1*EL*Sin[alpha - beta])/SW + ((I/4)*dZHLHL1*EL*Sin[alpha - beta])/SW + ((I/4)*dZWW1*EL*Sin[alpha - beta])/SW - ((I/2)*EL*(dbeta1*SW*Cos[alpha - beta] + dSW1*Sin[alpha - beta]))/SW^2}},
C[S[4], S[1], V[2]]==
	{{-(EL*Sin[alpha - beta])/(2*CW*SW), -(dalpha1*EL*Cos[alpha - beta])/(2*CW*SW) + (dZHAG01*EL*Cos[alpha - beta])/(4*CW*SW) + (dZHHHL1*EL*Cos[alpha - beta])/(4*CW*SW) - (dZe1*EL*Sin[alpha - beta])/(2*CW*SW) - (dZG0G01*EL*Sin[alpha - beta])/(4*CW*SW) - (dZHLHL1*EL*Sin[alpha - beta])/(4*CW*SW) - (dZZZ1*EL*Sin[alpha - beta])/(4*CW*SW) + (EL*(CW*dbeta1*SW*Cos[alpha - beta] - (CW*dSW1 - dCW1*SW)*(-1 + 2*SW^2)*Sin[alpha - beta]))/(2*CW^2*SW^2)}},
C[S[4], S[1], V[1]]==
	{{0, -(dZZA1*EL*Sin[alpha - beta])/(4*CW*SW)}},
C[S[3], S[2], V[2]]==
	{{(EL*Sin[alpha - beta])/(2*CW*SW), (dalpha1*EL*Cos[alpha - beta])/(2*CW*SW) + (dZG0HA1*EL*Cos[alpha - beta])/(4*CW*SW) + (dZHLHH1*EL*Cos[alpha - beta])/(4*CW*SW) + (dZe1*EL*Sin[alpha - beta])/(2*CW*SW) + (dZHAHA1*EL*Sin[alpha - beta])/(4*CW*SW) + (dZHHHH1*EL*Sin[alpha - beta])/(4*CW*SW) + (dZZZ1*EL*Sin[alpha - beta])/(4*CW*SW) - (EL*(CW*dbeta1*SW*Cos[alpha - beta] - (CW*dSW1 - dCW1*SW)*(-1 + 2*SW^2)*Sin[alpha - beta]))/(2*CW^2*SW^2)}},
C[S[3], S[2], V[1]]==
	{{0, (dZZA1*EL*Sin[alpha - beta])/(4*CW*SW)}},
C[S[2], -S[5], V[3]]==
	{{((-I/2)*EL*Sin[alpha - beta])/SW, ((-I/2)*dalpha1*EL*Cos[alpha - beta])/SW - ((I/4)*dZGpHp1*EL*Cos[alpha - beta])/SW - ((I/4)*dZHLHH1*EL*Cos[alpha - beta])/SW - ((I/2)*dZe1*EL*Sin[alpha - beta])/SW - ((I/4)*dZHHHH1*EL*Sin[alpha - beta])/SW - ((I/4)*dZHpHp1*EL*Sin[alpha - beta])/SW - ((I/4)*dZWW1*EL*Sin[alpha - beta])/SW + ((I/2)*EL*(dbeta1*SW*Cos[alpha - beta] + dSW1*Sin[alpha - beta]))/SW^2}},
C[S[2], S[5], -V[3]]==
	{{((I/2)*EL*Sin[alpha - beta])/SW, ((I/2)*dalpha1*EL*Cos[alpha - beta])/SW + ((I/4)*dZGpHp1*EL*Cos[alpha - beta])/SW + ((I/4)*dZHLHH1*EL*Cos[alpha - beta])/SW + ((I/2)*dZe1*EL*Sin[alpha - beta])/SW + ((I/4)*dZHHHH1*EL*Sin[alpha - beta])/SW + ((I/4)*dZHpHp1*EL*Sin[alpha - beta])/SW + ((I/4)*dZWW1*EL*Sin[alpha - beta])/SW - ((I/2)*EL*(dbeta1*SW*Cos[alpha - beta] + dSW1*Sin[alpha - beta]))/SW^2}},
C[-S[6], S[2], V[3]]==
	{{((I/2)*EL*Cos[alpha - beta])/SW, ((I/2)*dZe1*EL*Cos[alpha - beta])/SW + ((I/4)*dZGpGp1*EL*Cos[alpha - beta])/SW + ((I/4)*dZHHHH1*EL*Cos[alpha - beta])/SW + ((I/4)*dZWW1*EL*Cos[alpha - beta])/SW - ((I/2)*dalpha1*EL*Sin[alpha - beta])/SW - ((I/4)*dZHLHH1*EL*Sin[alpha - beta])/SW + ((I/4)*dZHpGp1*EL*Sin[alpha - beta])/SW + ((I/2)*EL*(-(dSW1*Cos[alpha - beta]) + dbeta1*SW*Sin[alpha - beta]))/SW^2}},
C[S[6], S[2], -V[3]]==
	{{((-I/2)*EL*Cos[alpha - beta])/SW, ((-I/2)*dZe1*EL*Cos[alpha - beta])/SW - ((I/4)*dZGpGp1*EL*Cos[alpha - beta])/SW - ((I/4)*dZHHHH1*EL*Cos[alpha - beta])/SW - ((I/4)*dZWW1*EL*Cos[alpha - beta])/SW + ((I/2)*dalpha1*EL*Sin[alpha - beta])/SW + ((I/4)*dZHLHH1*EL*Sin[alpha - beta])/SW - ((I/4)*dZHpGp1*EL*Sin[alpha - beta])/SW + ((I/2)*EL*(dSW1*Cos[alpha - beta] - dbeta1*SW*Sin[alpha - beta]))/SW^2}},
C[S[4], S[2], V[2]]==
	{{(EL*Cos[alpha - beta])/(2*CW*SW), (dZe1*EL*Cos[alpha - beta])/(2*CW*SW) + (dZG0G01*EL*Cos[alpha - beta])/(4*CW*SW) + (dZHHHH1*EL*Cos[alpha - beta])/(4*CW*SW) + (dZZZ1*EL*Cos[alpha - beta])/(4*CW*SW) - (dalpha1*EL*Sin[alpha - beta])/(2*CW*SW) + (dZHAG01*EL*Sin[alpha - beta])/(4*CW*SW) - (dZHLHH1*EL*Sin[alpha - beta])/(4*CW*SW) + (EL*((CW*dSW1 - dCW1*SW)*(-1 + 2*SW^2)*Cos[alpha - beta] + CW*dbeta1*SW*Sin[alpha - beta]))/(2*CW^2*SW^2)}},
C[S[4], S[2], V[1]]==
	{{0, (dZZA1*EL*Cos[alpha - beta])/(4*CW*SW)}},
C[S[3], -S[5], V[3]]==
	{{-EL/(2*SW), (dSW1*EL)/(2*SW^2) - (dZe1*EL)/(2*SW) - (dZHAHA1*EL)/(4*SW) - (dZHpHp1*EL)/(4*SW) - (dZWW1*EL)/(4*SW)}},
C[S[3], S[5], -V[3]]==
	{{-EL/(2*SW), (dSW1*EL)/(2*SW^2) - (dZe1*EL)/(2*SW) - (dZHAHA1*EL)/(4*SW) - (dZHpHp1*EL)/(4*SW) - (dZWW1*EL)/(4*SW)}},
C[-S[6], S[3], V[3]]==
	{{0, (dZG0HA1*EL)/(4*SW) + (dZHpGp1*EL)/(4*SW)}},
C[S[6], S[3], -V[3]]==
	{{0, (dZG0HA1*EL)/(4*SW) + (dZHpGp1*EL)/(4*SW)}},
C[S[5], -S[5], V[2]]==
	{{((I/2)*EL*(-1 + 2*SW^2))/(CW*SW), (I/2)*dZAZ1*EL + ((I/2)*EL*(CW*dSW1 - dCW1*SW))/(CW^2*SW^2) + ((I/2)*dZe1*EL*(-1 + 2*SW^2))/(CW*SW) + ((I/2)*dZHpHp1*EL*(-1 + 2*SW^2))/(CW*SW) + ((I/4)*dZZZ1*EL*(-1 + 2*SW^2))/(CW*SW)}},
C[S[5], -S[5], V[1]]==
	{{I*EL, (I/2)*dZAA1*EL + I*dZe1*EL + I*dZHpHp1*EL + ((I/4)*dZZA1*EL*(-1 + 2*SW^2))/(CW*SW)}},
C[S[6], -S[5], V[2]]==
	{{0, ((I/4)*dZGpHp1*EL*(-1 + 2*SW^2))/(CW*SW) + ((I/4)*dZHpGp1*EL*(-1 + 2*SW^2))/(CW*SW)}},
C[S[6], -S[5], V[1]]==
	{{0, (I/2)*dZGpHp1*EL + (I/2)*dZHpGp1*EL}},
C[S[4], -S[5], V[3]]==
	{{0, -(dZGpHp1*EL)/(4*SW) - (dZHAG01*EL)/(4*SW)}},
C[-S[6], S[5], V[2]]==
	{{0, ((-I/4)*dZGpHp1*EL*(-1 + 2*SW^2))/(CW*SW) - ((I/4)*dZHpGp1*EL*(-1 + 2*SW^2))/(CW*SW)}},
C[-S[6], S[5], V[1]]==
	{{0, (-I/2)*dZGpHp1*EL - (I/2)*dZHpGp1*EL}},
C[S[4], S[5], -V[3]]==
	{{0, -(dZGpHp1*EL)/(4*SW) - (dZHAG01*EL)/(4*SW)}},
C[S[6], -S[6], V[2]]==
	{{((I/2)*EL*(-1 + 2*SW^2))/(CW*SW), (I/2)*dZAZ1*EL + ((I/2)*EL*(CW*dSW1 - dCW1*SW))/(CW^2*SW^2) + ((I/2)*dZe1*EL*(-1 + 2*SW^2))/(CW*SW) + ((I/2)*dZGpGp1*EL*(-1 + 2*SW^2))/(CW*SW) + ((I/4)*dZZZ1*EL*(-1 + 2*SW^2))/(CW*SW)}},
C[S[6], -S[6], V[1]]==
	{{I*EL, (I/2)*dZAA1*EL + I*dZe1*EL + I*dZGpGp1*EL + ((I/4)*dZZA1*EL*(-1 + 2*SW^2))/(CW*SW)}},
C[S[4], -S[6], V[3]]==
	{{-EL/(2*SW), (dSW1*EL)/(2*SW^2) - (dZe1*EL)/(2*SW) - (dZG0G01*EL)/(4*SW) - (dZGpGp1*EL)/(4*SW) - (dZWW1*EL)/(4*SW)}},
C[S[4], S[6], -V[3]]==
	{{-EL/(2*SW), (dSW1*EL)/(2*SW^2) - (dZe1*EL)/(2*SW) - (dZG0G01*EL)/(4*SW) - (dZGpGp1*EL)/(4*SW) - (dZWW1*EL)/(4*SW)}}
}];
