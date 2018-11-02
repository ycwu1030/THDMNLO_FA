M$CouplingMatrices = Join[ M$CouplingMatrices, {
C[S[1], V[2], V[2]]==
	{{((-I/2)*EL^2*vev*Sin[alpha - beta])/(CW^2*SW^2), ((-I/2)*dalpha1*EL^2*vev*Cos[alpha - beta])/(CW^2*SW^2) + ((I/4)*dZHHHL1*EL^2*vev*Cos[alpha - beta])/(CW^2*SW^2) - (I*dZe1*EL^2*vev*Sin[alpha - beta])/(CW^2*SW^2) - ((I/4)*dZHLHL1*EL^2*vev*Sin[alpha - beta])/(CW^2*SW^2) - ((I/2)*dZZZ1*EL^2*vev*Sin[alpha - beta])/(CW^2*SW^2) + ((I/2)*EL^2*(CW*dbeta1*SW*vev*Cos[alpha - beta] - (2*dCW1*SW*(1 - 2*SW^2)*vev + CW*(dvev1*SW - 2*dSW1*vev + 4*dSW1*SW^2*vev))*Sin[alpha - beta]))/(CW^3*SW^3)}},
C[S[1], V[1], V[2]]==
	{{0, ((-I/4)*dZZA1*EL^2*vev*Sin[alpha - beta])/(CW^2*SW^2)}},
C[S[1], V[3], -V[3]]==
	{{((-I/2)*EL^2*vev*Sin[alpha - beta])/SW^2, ((-I/2)*dalpha1*EL^2*vev*Cos[alpha - beta])/SW^2 + ((I/4)*dZHHHL1*EL^2*vev*Cos[alpha - beta])/SW^2 - (I*dZe1*EL^2*vev*Sin[alpha - beta])/SW^2 - ((I/4)*dZHLHL1*EL^2*vev*Sin[alpha - beta])/SW^2 - ((I/2)*dZWW1*EL^2*vev*Sin[alpha - beta])/SW^2 + ((I/2)*EL^2*(dbeta1*SW*vev*Cos[alpha - beta] + (-(dvev1*SW) + 2*dSW1*vev)*Sin[alpha - beta]))/SW^3}},
C[S[2], V[2], V[2]]==
	{{((I/2)*EL^2*vev*Cos[alpha - beta])/(CW^2*SW^2), (I*dZe1*EL^2*vev*Cos[alpha - beta])/(CW^2*SW^2) + ((I/4)*dZHHHH1*EL^2*vev*Cos[alpha - beta])/(CW^2*SW^2) + ((I/2)*dZZZ1*EL^2*vev*Cos[alpha - beta])/(CW^2*SW^2) - ((I/2)*dalpha1*EL^2*vev*Sin[alpha - beta])/(CW^2*SW^2) - ((I/4)*dZHLHH1*EL^2*vev*Sin[alpha - beta])/(CW^2*SW^2) + ((I/2)*EL^2*((2*dCW1*SW*(1 - 2*SW^2)*vev + CW*(dvev1*SW - 2*dSW1*vev + 4*dSW1*SW^2*vev))*Cos[alpha - beta] + CW*dbeta1*SW*vev*Sin[alpha - beta]))/(CW^3*SW^3)}},
C[S[2], V[1], V[2]]==
	{{0, ((I/4)*dZZA1*EL^2*vev*Cos[alpha - beta])/(CW^2*SW^2)}},
C[S[2], V[3], -V[3]]==
	{{((I/2)*EL^2*vev*Cos[alpha - beta])/SW^2, (I*dZe1*EL^2*vev*Cos[alpha - beta])/SW^2 + ((I/4)*dZHHHH1*EL^2*vev*Cos[alpha - beta])/SW^2 + ((I/2)*dZWW1*EL^2*vev*Cos[alpha - beta])/SW^2 - ((I/2)*dalpha1*EL^2*vev*Sin[alpha - beta])/SW^2 - ((I/4)*dZHLHH1*EL^2*vev*Sin[alpha - beta])/SW^2 + ((I/2)*EL^2*((dvev1*SW - 2*dSW1*vev)*Cos[alpha - beta] + dbeta1*SW*vev*Sin[alpha - beta]))/SW^3}},
C[-S[5], V[3], V[2]]==
	{{0, ((-I/4)*dZGpHp1*EL^2*vev)/CW}},
C[-S[5], V[1], V[3]]==
	{{0, ((-I/4)*dZGpHp1*EL^2*vev)/SW}},
C[S[5], -V[3], V[2]]==
	{{0, ((-I/4)*dZGpHp1*EL^2*vev)/CW}},
C[S[5], V[1], -V[3]]==
	{{0, ((-I/4)*dZGpHp1*EL^2*vev)/SW}},
C[-S[6], V[3], V[2]]==
	{{((-I/2)*EL^2*vev)/CW, ((-I)*dZe1*EL^2*vev)/CW - ((I/4)*dZGpGp1*EL^2*vev)/CW - ((I/4)*dZWW1*EL^2*vev)/CW - ((I/4)*dZZZ1*EL^2*vev)/CW - ((I/4)*dZAZ1*EL^2*vev)/SW - ((I/2)*EL^2*(CW*dvev1 - dCW1*vev))/CW^2}},
C[-S[6], V[1], V[3]]==
	{{((-I/2)*EL^2*vev)/SW, ((-I/4)*dZZA1*EL^2*vev)/CW - ((I/4)*dZAA1*EL^2*vev)/SW - (I*dZe1*EL^2*vev)/SW - ((I/4)*dZGpGp1*EL^2*vev)/SW - ((I/4)*dZWW1*EL^2*vev)/SW - ((I/2)*EL^2*(dvev1*SW - dSW1*vev))/SW^2}},
C[S[6], -V[3], V[2]]==
	{{((-I/2)*EL^2*vev)/CW, ((-I)*dZe1*EL^2*vev)/CW - ((I/4)*dZGpGp1*EL^2*vev)/CW - ((I/4)*dZWW1*EL^2*vev)/CW - ((I/4)*dZZZ1*EL^2*vev)/CW - ((I/4)*dZAZ1*EL^2*vev)/SW - ((I/2)*EL^2*(CW*dvev1 - dCW1*vev))/CW^2}},
C[S[6], V[1], -V[3]]==
	{{((-I/2)*EL^2*vev)/SW, ((-I/4)*dZZA1*EL^2*vev)/CW - ((I/4)*dZAA1*EL^2*vev)/SW - (I*dZe1*EL^2*vev)/SW - ((I/4)*dZGpGp1*EL^2*vev)/SW - ((I/4)*dZWW1*EL^2*vev)/SW - ((I/2)*EL^2*(dvev1*SW - dSW1*vev))/SW^2}}
}];
