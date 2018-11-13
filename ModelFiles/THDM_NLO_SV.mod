M$CouplingMatrices = Join[ M$CouplingMatrices, {
C[S[4], V[1]]==
	{{0, (dZZA1*EL*vev)/(8*CW*SW)},
	{0, -(dZZA1*EL*vev)/(8*CW*SW)}},
C[S[4], V[2]]==
	{{0, ((dZe1*EL*vev)/(2*CW*SW) + (dZG0G01*EL*vev)/(4*CW*SW) + (dZZZ1*EL*vev)/(4*CW*SW) + (EL*(dCW1*SW*(1 - 2*SW^2)*vev + CW*(dvev1*SW + dSW1*(-1 + 2*SW^2)*vev)))/(2*CW^2*SW^2))/2},
	{0, (-(dZe1*EL*vev)/(2*CW*SW) - (dZG0G01*EL*vev)/(4*CW*SW) - (dZZZ1*EL*vev)/(4*CW*SW) - (EL*(dCW1*SW*(1 - 2*SW^2)*vev + CW*(dvev1*SW + dSW1*(-1 + 2*SW^2)*vev)))/(2*CW^2*SW^2))/2}},
C[S[6], -V[3]]==
	{{0, (((-I/2)*dZe1*EL*vev)/SW - ((I/4)*dZGpGp1*EL*vev)/SW - ((I/4)*dZWW1*EL*vev)/SW - ((I/2)*EL*(dvev1*SW - dSW1*vev))/SW^2)/2},
	{0, (((I/2)*dZe1*EL*vev)/SW + ((I/4)*dZGpGp1*EL*vev)/SW + ((I/4)*dZWW1*EL*vev)/SW + ((I/2)*EL*(dvev1*SW - dSW1*vev))/SW^2)/2}},
C[-S[6], V[3]]==
	{{0, (((I/2)*dZe1*EL*vev)/SW + ((I/4)*dZGpGp1*EL*vev)/SW + ((I/4)*dZWW1*EL*vev)/SW + ((I/2)*EL*(dvev1*SW - dSW1*vev))/SW^2)/2},
	{0, (((-I/2)*dZe1*EL*vev)/SW - ((I/4)*dZGpGp1*EL*vev)/SW - ((I/4)*dZWW1*EL*vev)/SW - ((I/2)*EL*(dvev1*SW - dSW1*vev))/SW^2)/2}},
C[S[3], V[2]]==
	{{0, (dZG0HA1*EL*vev)/(8*CW*SW)},
	{0, -(dZG0HA1*EL*vev)/(8*CW*SW)}},
C[S[5], -V[3]]==
	{{0, ((-I/8)*dZGpHp1*EL*vev)/SW},
	{0, ((I/8)*dZGpHp1*EL*vev)/SW}},
C[-S[5], V[3]]==
	{{0, ((I/8)*dZGpHp1*EL*vev)/SW},
	{0, ((-I/8)*dZGpHp1*EL*vev)/SW}}
}];
