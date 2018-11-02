M$CouplingMatrices = Join[ M$CouplingMatrices, {
C[S[1], S[1], V[2], V[2]]==
	{{((I/2)*EL^2)/(CW^2*SW^2), (I*dZe1*EL^2)/(CW^2*SW^2) + ((I/2)*dZHLHL1*EL^2)/(CW^2*SW^2) + ((I/2)*dZZZ1*EL^2)/(CW^2*SW^2) + (I*EL^2*(CW*dSW1 - dCW1*SW)*(-1 + 2*SW^2))/(CW^3*SW^3)}},
C[S[1], S[1], V[1], V[2]]==
	{{0, ((I/4)*dZZA1*EL^2)/(CW^2*SW^2)}},
C[S[1], S[1], V[3], -V[3]]==
	{{((I/2)*EL^2)/SW^2, ((-I)*dSW1*EL^2)/SW^3 + (I*dZe1*EL^2)/SW^2 + ((I/2)*dZHLHL1*EL^2)/SW^2 + ((I/2)*dZWW1*EL^2)/SW^2}},
C[S[2], S[1], V[2], V[2]]==
	{{0, ((I/4)*dZHHHL1*EL^2)/(CW^2*SW^2) + ((I/4)*dZHLHH1*EL^2)/(CW^2*SW^2)}},
C[S[2], S[1], V[3], -V[3]]==
	{{0, ((I/4)*dZHHHL1*EL^2)/SW^2 + ((I/4)*dZHLHH1*EL^2)/SW^2}},
C[S[1], -S[5], V[3], V[2]]==
	{{((-I/2)*EL^2*Cos[alpha - beta])/CW, ((-I)*dZe1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZHLHL1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZHpHp1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZWW1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZZZ1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZAZ1*EL^2*Cos[alpha - beta])/SW + ((I/2)*dalpha1*EL^2*Sin[alpha - beta])/CW + ((I/4)*dZGpHp1*EL^2*Sin[alpha - beta])/CW - ((I/4)*dZHHHL1*EL^2*Sin[alpha - beta])/CW + ((I/2)*EL^2*(dCW1*Cos[alpha - beta] - CW*dbeta1*Sin[alpha - beta]))/CW^2}},
C[S[1], -S[5], V[1], V[3]]==
	{{((-I/2)*EL^2*Cos[alpha - beta])/SW, ((-I/4)*dZZA1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZAA1*EL^2*Cos[alpha - beta])/SW - (I*dZe1*EL^2*Cos[alpha - beta])/SW - ((I/4)*dZHLHL1*EL^2*Cos[alpha - beta])/SW - ((I/4)*dZHpHp1*EL^2*Cos[alpha - beta])/SW - ((I/4)*dZWW1*EL^2*Cos[alpha - beta])/SW + ((I/2)*dalpha1*EL^2*Sin[alpha - beta])/SW + ((I/4)*dZGpHp1*EL^2*Sin[alpha - beta])/SW - ((I/4)*dZHHHL1*EL^2*Sin[alpha - beta])/SW + ((I/2)*EL^2*(dSW1*Cos[alpha - beta] - dbeta1*SW*Sin[alpha - beta]))/SW^2}},
C[S[1], S[5], -V[3], V[2]]==
	{{((-I/2)*EL^2*Cos[alpha - beta])/CW, ((-I)*dZe1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZHLHL1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZHpHp1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZWW1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZZZ1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZAZ1*EL^2*Cos[alpha - beta])/SW + ((I/2)*dalpha1*EL^2*Sin[alpha - beta])/CW + ((I/4)*dZGpHp1*EL^2*Sin[alpha - beta])/CW - ((I/4)*dZHHHL1*EL^2*Sin[alpha - beta])/CW + ((I/2)*EL^2*(dCW1*Cos[alpha - beta] - CW*dbeta1*Sin[alpha - beta]))/CW^2}},
C[S[1], S[5], V[1], -V[3]]==
	{{((-I/2)*EL^2*Cos[alpha - beta])/SW, ((-I/4)*dZZA1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZAA1*EL^2*Cos[alpha - beta])/SW - (I*dZe1*EL^2*Cos[alpha - beta])/SW - ((I/4)*dZHLHL1*EL^2*Cos[alpha - beta])/SW - ((I/4)*dZHpHp1*EL^2*Cos[alpha - beta])/SW - ((I/4)*dZWW1*EL^2*Cos[alpha - beta])/SW + ((I/2)*dalpha1*EL^2*Sin[alpha - beta])/SW + ((I/4)*dZGpHp1*EL^2*Sin[alpha - beta])/SW - ((I/4)*dZHHHL1*EL^2*Sin[alpha - beta])/SW + ((I/2)*EL^2*(dSW1*Cos[alpha - beta] - dbeta1*SW*Sin[alpha - beta]))/SW^2}},
C[-S[6], S[1], V[3], V[2]]==
	{{((I/2)*EL^2*Sin[alpha - beta])/CW, ((I/2)*dalpha1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZHHHL1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZHpGp1*EL^2*Cos[alpha - beta])/CW + (I*dZe1*EL^2*Sin[alpha - beta])/CW + ((I/4)*dZGpGp1*EL^2*Sin[alpha - beta])/CW + ((I/4)*dZHLHL1*EL^2*Sin[alpha - beta])/CW + ((I/4)*dZWW1*EL^2*Sin[alpha - beta])/CW + ((I/4)*dZZZ1*EL^2*Sin[alpha - beta])/CW + ((I/4)*dZAZ1*EL^2*Sin[alpha - beta])/SW - ((I/2)*EL^2*(CW*dbeta1*Cos[alpha - beta] + dCW1*Sin[alpha - beta]))/CW^2}},
C[-S[6], S[1], V[1], V[3]]==
	{{((I/2)*EL^2*Sin[alpha - beta])/SW, ((I/2)*dalpha1*EL^2*Cos[alpha - beta])/SW - ((I/4)*dZHHHL1*EL^2*Cos[alpha - beta])/SW - ((I/4)*dZHpGp1*EL^2*Cos[alpha - beta])/SW + ((I/4)*dZZA1*EL^2*Sin[alpha - beta])/CW + ((I/4)*dZAA1*EL^2*Sin[alpha - beta])/SW + (I*dZe1*EL^2*Sin[alpha - beta])/SW + ((I/4)*dZGpGp1*EL^2*Sin[alpha - beta])/SW + ((I/4)*dZHLHL1*EL^2*Sin[alpha - beta])/SW + ((I/4)*dZWW1*EL^2*Sin[alpha - beta])/SW - ((I/2)*EL^2*(dbeta1*SW*Cos[alpha - beta] + dSW1*Sin[alpha - beta]))/SW^2}},
C[S[6], S[1], -V[3], V[2]]==
	{{((I/2)*EL^2*Sin[alpha - beta])/CW, ((I/2)*dalpha1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZHHHL1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZHpGp1*EL^2*Cos[alpha - beta])/CW + (I*dZe1*EL^2*Sin[alpha - beta])/CW + ((I/4)*dZGpGp1*EL^2*Sin[alpha - beta])/CW + ((I/4)*dZHLHL1*EL^2*Sin[alpha - beta])/CW + ((I/4)*dZWW1*EL^2*Sin[alpha - beta])/CW + ((I/4)*dZZZ1*EL^2*Sin[alpha - beta])/CW + ((I/4)*dZAZ1*EL^2*Sin[alpha - beta])/SW - ((I/2)*EL^2*(CW*dbeta1*Cos[alpha - beta] + dCW1*Sin[alpha - beta]))/CW^2}},
C[S[6], S[1], V[1], -V[3]]==
	{{((I/2)*EL^2*Sin[alpha - beta])/SW, ((I/2)*dalpha1*EL^2*Cos[alpha - beta])/SW - ((I/4)*dZHHHL1*EL^2*Cos[alpha - beta])/SW - ((I/4)*dZHpGp1*EL^2*Cos[alpha - beta])/SW + ((I/4)*dZZA1*EL^2*Sin[alpha - beta])/CW + ((I/4)*dZAA1*EL^2*Sin[alpha - beta])/SW + (I*dZe1*EL^2*Sin[alpha - beta])/SW + ((I/4)*dZGpGp1*EL^2*Sin[alpha - beta])/SW + ((I/4)*dZHLHL1*EL^2*Sin[alpha - beta])/SW + ((I/4)*dZWW1*EL^2*Sin[alpha - beta])/SW - ((I/2)*EL^2*(dbeta1*SW*Cos[alpha - beta] + dSW1*Sin[alpha - beta]))/SW^2}},
C[S[2], S[2], V[2], V[2]]==
	{{((I/2)*EL^2)/(CW^2*SW^2), (I*dZe1*EL^2)/(CW^2*SW^2) + ((I/2)*dZHHHH1*EL^2)/(CW^2*SW^2) + ((I/2)*dZZZ1*EL^2)/(CW^2*SW^2) + (I*EL^2*(CW*dSW1 - dCW1*SW)*(-1 + 2*SW^2))/(CW^3*SW^3)}},
C[S[2], S[2], V[1], V[2]]==
	{{0, ((I/4)*dZZA1*EL^2)/(CW^2*SW^2)}},
C[S[2], S[2], V[3], -V[3]]==
	{{((I/2)*EL^2)/SW^2, ((-I)*dSW1*EL^2)/SW^3 + (I*dZe1*EL^2)/SW^2 + ((I/2)*dZHHHH1*EL^2)/SW^2 + ((I/2)*dZWW1*EL^2)/SW^2}},
C[S[2], -S[5], V[3], V[2]]==
	{{((-I/2)*EL^2*Sin[alpha - beta])/CW, ((-I/2)*dalpha1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZGpHp1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZHLHH1*EL^2*Cos[alpha - beta])/CW - (I*dZe1*EL^2*Sin[alpha - beta])/CW - ((I/4)*dZHHHH1*EL^2*Sin[alpha - beta])/CW - ((I/4)*dZHpHp1*EL^2*Sin[alpha - beta])/CW - ((I/4)*dZWW1*EL^2*Sin[alpha - beta])/CW - ((I/4)*dZZZ1*EL^2*Sin[alpha - beta])/CW - ((I/4)*dZAZ1*EL^2*Sin[alpha - beta])/SW + ((I/2)*EL^2*(CW*dbeta1*Cos[alpha - beta] + dCW1*Sin[alpha - beta]))/CW^2}},
C[S[2], -S[5], V[1], V[3]]==
	{{((-I/2)*EL^2*Sin[alpha - beta])/SW, ((-I/2)*dalpha1*EL^2*Cos[alpha - beta])/SW - ((I/4)*dZGpHp1*EL^2*Cos[alpha - beta])/SW - ((I/4)*dZHLHH1*EL^2*Cos[alpha - beta])/SW - ((I/4)*dZZA1*EL^2*Sin[alpha - beta])/CW - ((I/4)*dZAA1*EL^2*Sin[alpha - beta])/SW - (I*dZe1*EL^2*Sin[alpha - beta])/SW - ((I/4)*dZHHHH1*EL^2*Sin[alpha - beta])/SW - ((I/4)*dZHpHp1*EL^2*Sin[alpha - beta])/SW - ((I/4)*dZWW1*EL^2*Sin[alpha - beta])/SW + ((I/2)*EL^2*(dbeta1*SW*Cos[alpha - beta] + dSW1*Sin[alpha - beta]))/SW^2}},
C[S[2], S[5], -V[3], V[2]]==
	{{((-I/2)*EL^2*Sin[alpha - beta])/CW, ((-I/2)*dalpha1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZGpHp1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZHLHH1*EL^2*Cos[alpha - beta])/CW - (I*dZe1*EL^2*Sin[alpha - beta])/CW - ((I/4)*dZHHHH1*EL^2*Sin[alpha - beta])/CW - ((I/4)*dZHpHp1*EL^2*Sin[alpha - beta])/CW - ((I/4)*dZWW1*EL^2*Sin[alpha - beta])/CW - ((I/4)*dZZZ1*EL^2*Sin[alpha - beta])/CW - ((I/4)*dZAZ1*EL^2*Sin[alpha - beta])/SW + ((I/2)*EL^2*(CW*dbeta1*Cos[alpha - beta] + dCW1*Sin[alpha - beta]))/CW^2}},
C[S[2], S[5], V[1], -V[3]]==
	{{((-I/2)*EL^2*Sin[alpha - beta])/SW, ((-I/2)*dalpha1*EL^2*Cos[alpha - beta])/SW - ((I/4)*dZGpHp1*EL^2*Cos[alpha - beta])/SW - ((I/4)*dZHLHH1*EL^2*Cos[alpha - beta])/SW - ((I/4)*dZZA1*EL^2*Sin[alpha - beta])/CW - ((I/4)*dZAA1*EL^2*Sin[alpha - beta])/SW - (I*dZe1*EL^2*Sin[alpha - beta])/SW - ((I/4)*dZHHHH1*EL^2*Sin[alpha - beta])/SW - ((I/4)*dZHpHp1*EL^2*Sin[alpha - beta])/SW - ((I/4)*dZWW1*EL^2*Sin[alpha - beta])/SW + ((I/2)*EL^2*(dbeta1*SW*Cos[alpha - beta] + dSW1*Sin[alpha - beta]))/SW^2}},
C[-S[6], S[2], V[3], V[2]]==
	{{((-I/2)*EL^2*Cos[alpha - beta])/CW, ((-I)*dZe1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZGpGp1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZHHHH1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZWW1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZZZ1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZAZ1*EL^2*Cos[alpha - beta])/SW + ((I/2)*dalpha1*EL^2*Sin[alpha - beta])/CW + ((I/4)*dZHLHH1*EL^2*Sin[alpha - beta])/CW - ((I/4)*dZHpGp1*EL^2*Sin[alpha - beta])/CW + ((I/2)*EL^2*(dCW1*Cos[alpha - beta] - CW*dbeta1*Sin[alpha - beta]))/CW^2}},
C[-S[6], S[2], V[1], V[3]]==
	{{((-I/2)*EL^2*Cos[alpha - beta])/SW, ((-I/4)*dZZA1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZAA1*EL^2*Cos[alpha - beta])/SW - (I*dZe1*EL^2*Cos[alpha - beta])/SW - ((I/4)*dZGpGp1*EL^2*Cos[alpha - beta])/SW - ((I/4)*dZHHHH1*EL^2*Cos[alpha - beta])/SW - ((I/4)*dZWW1*EL^2*Cos[alpha - beta])/SW + ((I/2)*dalpha1*EL^2*Sin[alpha - beta])/SW + ((I/4)*dZHLHH1*EL^2*Sin[alpha - beta])/SW - ((I/4)*dZHpGp1*EL^2*Sin[alpha - beta])/SW + ((I/2)*EL^2*(dSW1*Cos[alpha - beta] - dbeta1*SW*Sin[alpha - beta]))/SW^2}},
C[S[6], S[2], -V[3], V[2]]==
	{{((-I/2)*EL^2*Cos[alpha - beta])/CW, ((-I)*dZe1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZGpGp1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZHHHH1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZWW1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZZZ1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZAZ1*EL^2*Cos[alpha - beta])/SW + ((I/2)*dalpha1*EL^2*Sin[alpha - beta])/CW + ((I/4)*dZHLHH1*EL^2*Sin[alpha - beta])/CW - ((I/4)*dZHpGp1*EL^2*Sin[alpha - beta])/CW + ((I/2)*EL^2*(dCW1*Cos[alpha - beta] - CW*dbeta1*Sin[alpha - beta]))/CW^2}},
C[S[6], S[2], V[1], -V[3]]==
	{{((-I/2)*EL^2*Cos[alpha - beta])/SW, ((-I/4)*dZZA1*EL^2*Cos[alpha - beta])/CW - ((I/4)*dZAA1*EL^2*Cos[alpha - beta])/SW - (I*dZe1*EL^2*Cos[alpha - beta])/SW - ((I/4)*dZGpGp1*EL^2*Cos[alpha - beta])/SW - ((I/4)*dZHHHH1*EL^2*Cos[alpha - beta])/SW - ((I/4)*dZWW1*EL^2*Cos[alpha - beta])/SW + ((I/2)*dalpha1*EL^2*Sin[alpha - beta])/SW + ((I/4)*dZHLHH1*EL^2*Sin[alpha - beta])/SW - ((I/4)*dZHpGp1*EL^2*Sin[alpha - beta])/SW + ((I/2)*EL^2*(dSW1*Cos[alpha - beta] - dbeta1*SW*Sin[alpha - beta]))/SW^2}},
C[S[3], S[3], V[2], V[2]]==
	{{((I/2)*EL^2)/(CW^2*SW^2), (I*dZe1*EL^2)/(CW^2*SW^2) + ((I/2)*dZHAHA1*EL^2)/(CW^2*SW^2) + ((I/2)*dZZZ1*EL^2)/(CW^2*SW^2) + (I*EL^2*(CW*dSW1 - dCW1*SW)*(-1 + 2*SW^2))/(CW^3*SW^3)}},
C[S[3], S[3], V[1], V[2]]==
	{{0, ((I/4)*dZZA1*EL^2)/(CW^2*SW^2)}},
C[S[3], S[3], V[3], -V[3]]==
	{{((I/2)*EL^2)/SW^2, ((-I)*dSW1*EL^2)/SW^3 + (I*dZe1*EL^2)/SW^2 + ((I/2)*dZHAHA1*EL^2)/SW^2 + ((I/2)*dZWW1*EL^2)/SW^2}},
C[S[3], -S[5], V[3], V[2]]==
	{{-EL^2/(2*CW), (dCW1*EL^2)/(2*CW^2) - (dZe1*EL^2)/CW - (dZHAHA1*EL^2)/(4*CW) - (dZHpHp1*EL^2)/(4*CW) - (dZWW1*EL^2)/(4*CW) - (dZZZ1*EL^2)/(4*CW) - (dZAZ1*EL^2)/(4*SW)}},
C[S[3], -S[5], V[1], V[3]]==
	{{-EL^2/(2*SW), -(dZZA1*EL^2)/(4*CW) + (dSW1*EL^2)/(2*SW^2) - (dZAA1*EL^2)/(4*SW) - (dZe1*EL^2)/SW - (dZHAHA1*EL^2)/(4*SW) - (dZHpHp1*EL^2)/(4*SW) - (dZWW1*EL^2)/(4*SW)}},
C[S[3], S[5], -V[3], V[2]]==
	{{EL^2/(2*CW), -(dCW1*EL^2)/(2*CW^2) + (dZe1*EL^2)/CW + (dZHAHA1*EL^2)/(4*CW) + (dZHpHp1*EL^2)/(4*CW) + (dZWW1*EL^2)/(4*CW) + (dZZZ1*EL^2)/(4*CW) + (dZAZ1*EL^2)/(4*SW)}},
C[S[3], S[5], V[1], -V[3]]==
	{{EL^2/(2*SW), (dZZA1*EL^2)/(4*CW) - (dSW1*EL^2)/(2*SW^2) + (dZAA1*EL^2)/(4*SW) + (dZe1*EL^2)/SW + (dZHAHA1*EL^2)/(4*SW) + (dZHpHp1*EL^2)/(4*SW) + (dZWW1*EL^2)/(4*SW)}},
C[-S[6], S[3], V[3], V[2]]==
	{{0, -(dZG0HA1*EL^2)/(4*CW) - (dZHpGp1*EL^2)/(4*CW)}},
C[-S[6], S[3], V[1], V[3]]==
	{{0, -(dZG0HA1*EL^2)/(4*SW) - (dZHpGp1*EL^2)/(4*SW)}},
C[S[6], S[3], -V[3], V[2]]==
	{{0, (dZG0HA1*EL^2)/(4*CW) + (dZHpGp1*EL^2)/(4*CW)}},
C[S[6], S[3], V[1], -V[3]]==
	{{0, (dZG0HA1*EL^2)/(4*SW) + (dZHpGp1*EL^2)/(4*SW)}},
C[S[4], S[3], V[2], V[2]]==
	{{0, ((I/4)*dZG0HA1*EL^2)/(CW^2*SW^2) + ((I/4)*dZHAG01*EL^2)/(CW^2*SW^2)}},
C[S[4], S[3], V[3], -V[3]]==
	{{0, ((I/4)*dZG0HA1*EL^2)/SW^2 + ((I/4)*dZHAG01*EL^2)/SW^2}},
C[S[5], -S[5], V[2], V[2]]==
	{{((I/2)*(EL - 2*EL*SW^2)^2)/(CW^2*SW^2), (I*dZAZ1*EL^2*(-1 + 2*SW^2))/(CW*SW) + (I*EL^2*(CW*dSW1 - dCW1*SW)*(-1 + 2*SW^2))/(CW^3*SW^3) + (I*dZe1*(EL - 2*EL*SW^2)^2)/(CW^2*SW^2) + ((I/2)*dZHpHp1*(EL - 2*EL*SW^2)^2)/(CW^2*SW^2) + ((I/2)*dZZZ1*(EL - 2*EL*SW^2)^2)/(CW^2*SW^2)}},
C[S[5], -S[5], V[1], V[2]]==
	{{(I*EL^2*(-1 + 2*SW^2))/(CW*SW), I*dZAZ1*EL^2 + (I*EL^2*(CW*dSW1 - dCW1*SW))/(CW^2*SW^2) + ((I/2)*dZAA1*EL^2*(-1 + 2*SW^2))/(CW*SW) + ((2*I)*dZe1*EL^2*(-1 + 2*SW^2))/(CW*SW) + (I*dZHpHp1*EL^2*(-1 + 2*SW^2))/(CW*SW) + ((I/2)*dZZZ1*EL^2*(-1 + 2*SW^2))/(CW*SW) + ((I/4)*dZZA1*(EL - 2*EL*SW^2)^2)/(CW^2*SW^2)}},
C[S[5], -S[5], V[1], V[1]]==
	{{(2*I)*EL^2, (2*I)*dZAA1*EL^2 + (4*I)*dZe1*EL^2 + (2*I)*dZHpHp1*EL^2 + (I*dZZA1*EL^2*(-1 + 2*SW^2))/(CW*SW)}},
C[S[5], -S[5], V[3], -V[3]]==
	{{((I/2)*EL^2)/SW^2, ((-I)*dSW1*EL^2)/SW^3 + (I*dZe1*EL^2)/SW^2 + ((I/2)*dZHpHp1*EL^2)/SW^2 + ((I/2)*dZWW1*EL^2)/SW^2}},
C[S[6], -S[5], V[2], V[2]]==
	{{0, ((I/4)*dZGpHp1*(EL - 2*EL*SW^2)^2)/(CW^2*SW^2) + ((I/4)*dZHpGp1*(EL - 2*EL*SW^2)^2)/(CW^2*SW^2)}},
C[S[6], -S[5], V[1], V[2]]==
	{{0, ((I/2)*dZGpHp1*EL^2*(-1 + 2*SW^2))/(CW*SW) + ((I/2)*dZHpGp1*EL^2*(-1 + 2*SW^2))/(CW*SW)}},
C[S[6], -S[5], V[1], V[1]]==
	{{0, I*dZGpHp1*EL^2 + I*dZHpGp1*EL^2}},
C[S[6], -S[5], V[3], -V[3]]==
	{{0, ((I/4)*dZGpHp1*EL^2)/SW^2 + ((I/4)*dZHpGp1*EL^2)/SW^2}},
C[S[4], -S[5], V[3], V[2]]==
	{{0, -(dZGpHp1*EL^2)/(4*CW) - (dZHAG01*EL^2)/(4*CW)}},
C[S[4], -S[5], V[1], V[3]]==
	{{0, -(dZGpHp1*EL^2)/(4*SW) - (dZHAG01*EL^2)/(4*SW)}},
C[-S[6], S[5], V[2], V[2]]==
	{{0, ((I/4)*dZGpHp1*(EL - 2*EL*SW^2)^2)/(CW^2*SW^2) + ((I/4)*dZHpGp1*(EL - 2*EL*SW^2)^2)/(CW^2*SW^2)}},
C[-S[6], S[5], V[1], V[2]]==
	{{0, ((I/2)*dZGpHp1*EL^2*(-1 + 2*SW^2))/(CW*SW) + ((I/2)*dZHpGp1*EL^2*(-1 + 2*SW^2))/(CW*SW)}},
C[-S[6], S[5], V[1], V[1]]==
	{{0, I*dZGpHp1*EL^2 + I*dZHpGp1*EL^2}},
C[-S[6], S[5], V[3], -V[3]]==
	{{0, ((I/4)*dZGpHp1*EL^2)/SW^2 + ((I/4)*dZHpGp1*EL^2)/SW^2}},
C[S[4], S[5], -V[3], V[2]]==
	{{0, (dZGpHp1*EL^2)/(4*CW) + (dZHAG01*EL^2)/(4*CW)}},
C[S[4], S[5], V[1], -V[3]]==
	{{0, (dZGpHp1*EL^2)/(4*SW) + (dZHAG01*EL^2)/(4*SW)}},
C[S[6], -S[6], V[2], V[2]]==
	{{((I/2)*(EL - 2*EL*SW^2)^2)/(CW^2*SW^2), (I*dZAZ1*EL^2*(-1 + 2*SW^2))/(CW*SW) + (I*EL^2*(CW*dSW1 - dCW1*SW)*(-1 + 2*SW^2))/(CW^3*SW^3) + (I*dZe1*(EL - 2*EL*SW^2)^2)/(CW^2*SW^2) + ((I/2)*dZGpGp1*(EL - 2*EL*SW^2)^2)/(CW^2*SW^2) + ((I/2)*dZZZ1*(EL - 2*EL*SW^2)^2)/(CW^2*SW^2)}},
C[S[6], -S[6], V[1], V[2]]==
	{{(I*EL^2*(-1 + 2*SW^2))/(CW*SW), I*dZAZ1*EL^2 + (I*EL^2*(CW*dSW1 - dCW1*SW))/(CW^2*SW^2) + ((I/2)*dZAA1*EL^2*(-1 + 2*SW^2))/(CW*SW) + ((2*I)*dZe1*EL^2*(-1 + 2*SW^2))/(CW*SW) + (I*dZGpGp1*EL^2*(-1 + 2*SW^2))/(CW*SW) + ((I/2)*dZZZ1*EL^2*(-1 + 2*SW^2))/(CW*SW) + ((I/4)*dZZA1*(EL - 2*EL*SW^2)^2)/(CW^2*SW^2)}},
C[S[6], -S[6], V[1], V[1]]==
	{{(2*I)*EL^2, (2*I)*dZAA1*EL^2 + (4*I)*dZe1*EL^2 + (2*I)*dZGpGp1*EL^2 + (I*dZZA1*EL^2*(-1 + 2*SW^2))/(CW*SW)}},
C[S[6], -S[6], V[3], -V[3]]==
	{{((I/2)*EL^2)/SW^2, ((-I)*dSW1*EL^2)/SW^3 + (I*dZe1*EL^2)/SW^2 + ((I/2)*dZGpGp1*EL^2)/SW^2 + ((I/2)*dZWW1*EL^2)/SW^2}},
C[S[4], -S[6], V[3], V[2]]==
	{{-EL^2/(2*CW), (dCW1*EL^2)/(2*CW^2) - (dZe1*EL^2)/CW - (dZG0G01*EL^2)/(4*CW) - (dZGpGp1*EL^2)/(4*CW) - (dZWW1*EL^2)/(4*CW) - (dZZZ1*EL^2)/(4*CW) - (dZAZ1*EL^2)/(4*SW)}},
C[S[4], -S[6], V[1], V[3]]==
	{{-EL^2/(2*SW), -(dZZA1*EL^2)/(4*CW) + (dSW1*EL^2)/(2*SW^2) - (dZAA1*EL^2)/(4*SW) - (dZe1*EL^2)/SW - (dZG0G01*EL^2)/(4*SW) - (dZGpGp1*EL^2)/(4*SW) - (dZWW1*EL^2)/(4*SW)}},
C[S[4], S[6], -V[3], V[2]]==
	{{EL^2/(2*CW), -(dCW1*EL^2)/(2*CW^2) + (dZe1*EL^2)/CW + (dZG0G01*EL^2)/(4*CW) + (dZGpGp1*EL^2)/(4*CW) + (dZWW1*EL^2)/(4*CW) + (dZZZ1*EL^2)/(4*CW) + (dZAZ1*EL^2)/(4*SW)}},
C[S[4], S[6], V[1], -V[3]]==
	{{EL^2/(2*SW), (dZZA1*EL^2)/(4*CW) - (dSW1*EL^2)/(2*SW^2) + (dZAA1*EL^2)/(4*SW) + (dZe1*EL^2)/SW + (dZG0G01*EL^2)/(4*SW) + (dZGpGp1*EL^2)/(4*SW) + (dZWW1*EL^2)/(4*SW)}},
C[S[4], S[4], V[2], V[2]]==
	{{((I/2)*EL^2)/(CW^2*SW^2), (I*dZe1*EL^2)/(CW^2*SW^2) + ((I/2)*dZG0G01*EL^2)/(CW^2*SW^2) + ((I/2)*dZZZ1*EL^2)/(CW^2*SW^2) + (I*EL^2*(CW*dSW1 - dCW1*SW)*(-1 + 2*SW^2))/(CW^3*SW^3)}},
C[S[4], S[4], V[1], V[2]]==
	{{0, ((I/4)*dZZA1*EL^2)/(CW^2*SW^2)}},
C[S[4], S[4], V[3], -V[3]]==
	{{((I/2)*EL^2)/SW^2, ((-I)*dSW1*EL^2)/SW^3 + (I*dZe1*EL^2)/SW^2 + ((I/2)*dZG0G01*EL^2)/SW^2 + ((I/2)*dZWW1*EL^2)/SW^2}}
}];
