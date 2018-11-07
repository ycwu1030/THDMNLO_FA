M$CouplingMatrices = Join[ M$CouplingMatrices, {
C[S[1], -U[2], U[2]]==
	{{((I/2)*EL*MZ*GaugeXi[Z]*Sin[alpha - beta])/(CW*SW), ((I/2)*dalpha1*EL*MZ*Cos[alpha - beta]*GaugeXi[Z])/(CW*SW) - ((I/4)*dZHHHL1*EL*MZ*Cos[alpha - beta]*GaugeXi[Z])/(CW*SW) + ((I/2)*dZe1*EL*MZ*GaugeXi[Z]*Sin[alpha - beta])/(CW*SW) + ((I/4)*dZHLHL1*EL*MZ*GaugeXi[Z]*Sin[alpha - beta])/(CW*SW) - ((I/2)*EL*MZ*GaugeXi[Z]*(CW*dbeta1*SW*Cos[alpha - beta] + (CW*dSW1 + dCW1*SW)*Sin[alpha - beta]))/(CW^2*SW^2)}},
C[S[1], -U[4], U[4]]==
	{{((I/2)*EL*MW*GaugeXi[W]*Sin[alpha - beta])/SW, ((I/2)*dalpha1*EL*MW*Cos[alpha - beta]*GaugeXi[W])/SW - ((I/4)*dZHHHL1*EL*MW*Cos[alpha - beta]*GaugeXi[W])/SW + ((I/2)*dZe1*EL*MW*GaugeXi[W]*Sin[alpha - beta])/SW + ((I/4)*dZHLHL1*EL*MW*GaugeXi[W]*Sin[alpha - beta])/SW - ((I/2)*EL*MW*GaugeXi[W]*(dbeta1*SW*Cos[alpha - beta] + dSW1*Sin[alpha - beta]))/SW^2}},
C[S[1], -U[3], U[3]]==
	{{((I/2)*EL*MW*GaugeXi[W]*Sin[alpha - beta])/SW, ((I/2)*dalpha1*EL*MW*Cos[alpha - beta]*GaugeXi[W])/SW - ((I/4)*dZHHHL1*EL*MW*Cos[alpha - beta]*GaugeXi[W])/SW + ((I/2)*dZe1*EL*MW*GaugeXi[W]*Sin[alpha - beta])/SW + ((I/4)*dZHLHL1*EL*MW*GaugeXi[W]*Sin[alpha - beta])/SW - ((I/2)*EL*MW*GaugeXi[W]*(dbeta1*SW*Cos[alpha - beta] + dSW1*Sin[alpha - beta]))/SW^2}},
C[S[2], -U[2], U[2]]==
	{{((-I/2)*EL*MZ*Cos[alpha - beta]*GaugeXi[Z])/(CW*SW), ((-I/2)*dZe1*EL*MZ*Cos[alpha - beta]*GaugeXi[Z])/(CW*SW) - ((I/4)*dZHHHH1*EL*MZ*Cos[alpha - beta]*GaugeXi[Z])/(CW*SW) + ((I/2)*dalpha1*EL*MZ*GaugeXi[Z]*Sin[alpha - beta])/(CW*SW) + ((I/4)*dZHLHH1*EL*MZ*GaugeXi[Z]*Sin[alpha - beta])/(CW*SW) + ((I/2)*EL*MZ*GaugeXi[Z]*((CW*dSW1 + dCW1*SW)*Cos[alpha - beta] - CW*dbeta1*SW*Sin[alpha - beta]))/(CW^2*SW^2)}},
C[S[2], -U[4], U[4]]==
	{{((-I/2)*EL*MW*Cos[alpha - beta]*GaugeXi[W])/SW, ((-I/2)*dZe1*EL*MW*Cos[alpha - beta]*GaugeXi[W])/SW - ((I/4)*dZHHHH1*EL*MW*Cos[alpha - beta]*GaugeXi[W])/SW + ((I/2)*dalpha1*EL*MW*GaugeXi[W]*Sin[alpha - beta])/SW + ((I/4)*dZHLHH1*EL*MW*GaugeXi[W]*Sin[alpha - beta])/SW + ((I/2)*EL*MW*GaugeXi[W]*(dSW1*Cos[alpha - beta] - dbeta1*SW*Sin[alpha - beta]))/SW^2}},
C[S[2], -U[3], U[3]]==
	{{((-I/2)*EL*MW*Cos[alpha - beta]*GaugeXi[W])/SW, ((-I/2)*dZe1*EL*MW*Cos[alpha - beta]*GaugeXi[W])/SW - ((I/4)*dZHHHH1*EL*MW*Cos[alpha - beta]*GaugeXi[W])/SW + ((I/2)*dalpha1*EL*MW*GaugeXi[W]*Sin[alpha - beta])/SW + ((I/4)*dZHLHH1*EL*MW*GaugeXi[W]*Sin[alpha - beta])/SW + ((I/2)*EL*MW*GaugeXi[W]*(dSW1*Cos[alpha - beta] - dbeta1*SW*Sin[alpha - beta]))/SW^2}},
C[S[3], -U[4], U[4]]==
	{{0, (dZG0HA1*EL*MW*GaugeXi[W])/(4*SW)}},
C[S[3], -U[3], U[3]]==
	{{0, -(dZG0HA1*EL*MW*GaugeXi[W])/(4*SW)}},
C[-S[5], -U[2], U[3]]==
	{{0, ((I/4)*dZGpHp1*EL*MZ*GaugeXi[Z])/SW}},
C[-S[5], -U[4], U[1]]==
	{{0, (I/2)*dZGpHp1*EL*MW*GaugeXi[W]}},
C[-S[5], -U[4], U[2]]==
	{{0, ((I/4)*dZGpHp1*EL*MW*(-1 + 2*SW^2)*GaugeXi[W])/(CW*SW)}},
C[S[5], -U[2], U[4]]==
	{{0, ((I/4)*dZGpHp1*EL*MZ*GaugeXi[Z])/SW}},
C[S[5], -U[3], U[1]]==
	{{0, (I/2)*dZGpHp1*EL*MW*GaugeXi[W]}},
C[S[5], -U[3], U[2]]==
	{{0, ((I/4)*dZGpHp1*EL*MW*(-1 + 2*SW^2)*GaugeXi[W])/(CW*SW)}},
C[-S[6], -U[2], U[3]]==
	{{((I/2)*EL*MZ*GaugeXi[Z])/SW, ((-I/2)*dSW1*EL*MZ*GaugeXi[Z])/SW^2 + ((I/2)*dZe1*EL*MZ*GaugeXi[Z])/SW + ((I/4)*dZGpGp1*EL*MZ*GaugeXi[Z])/SW}},
C[-S[6], -U[4], U[1]]==
	{{I*EL*MW*GaugeXi[W], I*dZe1*EL*MW*GaugeXi[W] + (I/2)*dZGpGp1*EL*MW*GaugeXi[W]}},
C[-S[6], -U[4], U[2]]==
	{{((I/2)*EL*MW*(-1 + 2*SW^2)*GaugeXi[W])/(CW*SW), ((I/2)*EL*MW*(CW*dSW1 - dCW1*SW)*GaugeXi[W])/(CW^2*SW^2) + ((I/2)*dZe1*EL*MW*(-1 + 2*SW^2)*GaugeXi[W])/(CW*SW) + ((I/4)*dZGpGp1*EL*MW*(-1 + 2*SW^2)*GaugeXi[W])/(CW*SW)}},
C[S[6], -U[2], U[4]]==
	{{((I/2)*EL*MZ*GaugeXi[Z])/SW, ((-I/2)*dSW1*EL*MZ*GaugeXi[Z])/SW^2 + ((I/2)*dZe1*EL*MZ*GaugeXi[Z])/SW + ((I/4)*dZGpGp1*EL*MZ*GaugeXi[Z])/SW}},
C[S[6], -U[3], U[1]]==
	{{I*EL*MW*GaugeXi[W], I*dZe1*EL*MW*GaugeXi[W] + (I/2)*dZGpGp1*EL*MW*GaugeXi[W]}},
C[S[6], -U[3], U[2]]==
	{{((I/2)*EL*MW*(-1 + 2*SW^2)*GaugeXi[W])/(CW*SW), ((I/2)*EL*MW*(CW*dSW1 - dCW1*SW)*GaugeXi[W])/(CW^2*SW^2) + ((I/2)*dZe1*EL*MW*(-1 + 2*SW^2)*GaugeXi[W])/(CW*SW) + ((I/4)*dZGpGp1*EL*MW*(-1 + 2*SW^2)*GaugeXi[W])/(CW*SW)}},
C[S[4], -U[4], U[4]]==
	{{(EL*MW*GaugeXi[W])/(2*SW), -(dSW1*EL*MW*GaugeXi[W])/(2*SW^2) + (dZe1*EL*MW*GaugeXi[W])/(2*SW) + (dZG0G01*EL*MW*GaugeXi[W])/(4*SW)}},
C[S[4], -U[3], U[3]]==
	{{-(EL*MW*GaugeXi[W])/(2*SW), (dSW1*EL*MW*GaugeXi[W])/(2*SW^2) - (dZe1*EL*MW*GaugeXi[W])/(2*SW) - (dZG0G01*EL*MW*GaugeXi[W])/(4*SW)}}
}];
