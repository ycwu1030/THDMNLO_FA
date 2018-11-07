M$CouplingMatrices = Join[ M$CouplingMatrices, {
C[S[1], S[1]]==
	{{0, (-I)*dZHLHL1},
	{0, (-I)*dMHL21 - I*dZHLHL1*MHL2}},
C[S[2], S[1]]==
	{{0, (-I/2)*dZHHHL1 - (I/2)*dZHLHH1},
	{0, (-I/2)*dZHHHL1*MHH2 - (I/2)*dZHLHH1*MHL2}},
C[S[2], S[2]]==
	{{0, (-I)*dZHHHH1},
	{0, (-I)*dMHH21 - I*dZHHHH1*MHH2}},
C[S[3], S[3]]==
	{{0, (-I)*dZHAHA1},
	{0, (-I)*dMHA21 - I*dZHAHA1*MHA2}},
C[S[4], S[3]]==
	{{0, (-I/2)*dZG0HA1 - (I/2)*dZHAG01},
	{0, (-I/2)*dZHAG01*MHA2 - (I*(dTHL1*Cos[alpha - beta] + dTHH1*Sin[alpha - beta]))/vev}},
C[S[5], -S[5]]==
	{{0, (-I)*dZHpHp1},
	{0, (-I)*dMHp21 - I*dZHpHp1*MHp2}},
C[S[6], -S[5]]==
	{{0, (-I/2)*dZGpHp1 - (I/2)*dZHpGp1},
	{0, (-I/2)*dZHpGp1*MHp2 - (I*(dTHL1*Cos[alpha - beta] + dTHH1*Sin[alpha - beta]))/vev}},
C[-S[6], S[5]]==
	{{0, (-I/2)*dZGpHp1 - (I/2)*dZHpGp1},
	{0, (-I/2)*dZHpGp1*MHp2 - (I*(dTHL1*Cos[alpha - beta] + dTHH1*Sin[alpha - beta]))/vev}},
C[S[6], -S[6]]==
	{{0, (-I)*dZGpGp1},
	{0, ((-I)*(dTHH1*Cos[alpha - beta] - dTHL1*Sin[alpha - beta]))/vev}},
C[S[4], S[4]]==
	{{0, (-I)*dZG0G01},
	{0, ((-I)*(dTHH1*Cos[alpha - beta] - dTHL1*Sin[alpha - beta]))/vev}}
}];
