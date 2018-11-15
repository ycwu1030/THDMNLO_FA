M$CouplingMatrices = Join[ M$CouplingMatrices, {
C[F[2, {i1}], -F[2, {i2}], S[4]]==
	{{-((IndexDelta[i2, i1]*Mf[2, i1])/vev), -(dZG0G01*IndexDelta[i2, i1]*Mf[2, i1])/(2*vev) + (-2*vev*dMf1[2, i1]*IndexDelta[i2, i1] - vev*Conjugate[dZfR1[2, i1, i2]]*Mf[2, i1] + 2*dvev1*IndexDelta[i2, i1]*Mf[2, i1] - vev*dZfL1[2, i2, i1]*Mf[2, i2])/(2*vev^2) + (dZHAG01*IndexDelta[i2, i1]*Mf[2, i1]*Tan[beta])/(2*vev)},
	{(IndexDelta[i2, i1]*Mf[2, i1])/vev, (dZG0G01*IndexDelta[i2, i1]*Mf[2, i1])/(2*vev) + (2*vev*dMf1[2, i1]*IndexDelta[i2, i1] + vev*Conjugate[dZfL1[2, i1, i2]]*Mf[2, i1] - 2*dvev1*IndexDelta[i2, i1]*Mf[2, i1] + vev*dZfR1[2, i2, i1]*Mf[2, i2])/(2*vev^2) - (dZHAG01*IndexDelta[i2, i1]*Mf[2, i1]*Tan[beta])/(2*vev)}},
C[F[2, {i1}], -F[2, {i2}], S[3]]==
	{{(IndexDelta[i2, i1]*Mf[2, i1]*Tan[beta])/vev, -(dZG0HA1*IndexDelta[i2, i1]*Mf[2, i1])/(2*vev) + (dbeta1*IndexDelta[i2, i1]*Mf[2, i1]*Sec[beta]^2)/vev + (dZHAHA1*IndexDelta[i2, i1]*Mf[2, i1]*Tan[beta])/(2*vev) + ((2*vev*dMf1[2, i1]*IndexDelta[i2, i1] + vev*Conjugate[dZfR1[2, i1, i2]]*Mf[2, i1] - 2*dvev1*IndexDelta[i2, i1]*Mf[2, i1] + vev*dZfL1[2, i2, i1]*Mf[2, i2])*Tan[beta])/(2*vev^2)},
	{-((IndexDelta[i2, i1]*Mf[2, i1]*Tan[beta])/vev), (dZG0HA1*IndexDelta[i2, i1]*Mf[2, i1])/(2*vev) - (dbeta1*IndexDelta[i2, i1]*Mf[2, i1]*Sec[beta]^2)/vev - (dZHAHA1*IndexDelta[i2, i1]*Mf[2, i1]*Tan[beta])/(2*vev) + ((-2*vev*dMf1[2, i1]*IndexDelta[i2, i1] - vev*Conjugate[dZfL1[2, i1, i2]]*Mf[2, i1] + 2*dvev1*IndexDelta[i2, i1]*Mf[2, i1] - vev*dZfR1[2, i2, i1]*Mf[2, i2])*Tan[beta])/(2*vev^2)}},
C[F[2, {i1}], -F[2, {i2}], S[2]]==
	{{((-I)*Cos[alpha]*IndexDelta[i2, i1]*Mf[2, i1]*Sec[beta])/vev, ((-I/2)*dZHHHH1*Cos[alpha]*IndexDelta[i2, i1]*Mf[2, i1]*Sec[beta])/vev - ((I/2)*Cos[alpha]*(2*vev*dMf1[2, i1]*IndexDelta[i2, i1] + vev*Conjugate[dZfR1[2, i1, i2]]*Mf[2, i1] - 2*dvev1*IndexDelta[i2, i1]*Mf[2, i1] + vev*dZfL1[2, i2, i1]*Mf[2, i2])*Sec[beta])/vev^2 + (I*dalpha1*IndexDelta[i2, i1]*Mf[2, i1]*Sec[beta]*Sin[alpha])/vev + ((I/2)*dZHLHH1*IndexDelta[i2, i1]*Mf[2, i1]*Sec[beta]*Sin[alpha])/vev - (I*dbeta1*Cos[alpha]*IndexDelta[i2, i1]*Mf[2, i1]*Sec[beta]*Tan[beta])/vev},
	{((-I)*Cos[alpha]*IndexDelta[i2, i1]*Mf[2, i1]*Sec[beta])/vev, ((-I/2)*dZHHHH1*Cos[alpha]*IndexDelta[i2, i1]*Mf[2, i1]*Sec[beta])/vev - ((I/2)*Cos[alpha]*(2*vev*dMf1[2, i1]*IndexDelta[i2, i1] + vev*Conjugate[dZfL1[2, i1, i2]]*Mf[2, i1] - 2*dvev1*IndexDelta[i2, i1]*Mf[2, i1] + vev*dZfR1[2, i2, i1]*Mf[2, i2])*Sec[beta])/vev^2 + (I*dalpha1*IndexDelta[i2, i1]*Mf[2, i1]*Sec[beta]*Sin[alpha])/vev + ((I/2)*dZHLHH1*IndexDelta[i2, i1]*Mf[2, i1]*Sec[beta]*Sin[alpha])/vev - (I*dbeta1*Cos[alpha]*IndexDelta[i2, i1]*Mf[2, i1]*Sec[beta]*Tan[beta])/vev}},
C[F[2, {i1}], -F[2, {i2}], S[1]]==
	{{(I*IndexDelta[i2, i1]*Mf[2, i1]*Sec[beta]*Sin[alpha])/vev, (I*dalpha1*Cos[alpha]*IndexDelta[i2, i1]*Mf[2, i1]*Sec[beta])/vev - ((I/2)*dZHHHL1*Cos[alpha]*IndexDelta[i2, i1]*Mf[2, i1]*Sec[beta])/vev + ((I/2)*dZHLHL1*IndexDelta[i2, i1]*Mf[2, i1]*Sec[beta]*Sin[alpha])/vev + ((I/2)*(2*vev*dMf1[2, i1]*IndexDelta[i2, i1] + vev*Conjugate[dZfR1[2, i1, i2]]*Mf[2, i1] - 2*dvev1*IndexDelta[i2, i1]*Mf[2, i1] + vev*dZfL1[2, i2, i1]*Mf[2, i2])*Sec[beta]*Sin[alpha])/vev^2 + (I*dbeta1*IndexDelta[i2, i1]*Mf[2, i1]*Sec[beta]*Sin[alpha]*Tan[beta])/vev},
	{(I*IndexDelta[i2, i1]*Mf[2, i1]*Sec[beta]*Sin[alpha])/vev, (I*dalpha1*Cos[alpha]*IndexDelta[i2, i1]*Mf[2, i1]*Sec[beta])/vev - ((I/2)*dZHHHL1*Cos[alpha]*IndexDelta[i2, i1]*Mf[2, i1]*Sec[beta])/vev + ((I/2)*dZHLHL1*IndexDelta[i2, i1]*Mf[2, i1]*Sec[beta]*Sin[alpha])/vev + ((I/2)*(2*vev*dMf1[2, i1]*IndexDelta[i2, i1] + vev*Conjugate[dZfL1[2, i1, i2]]*Mf[2, i1] - 2*dvev1*IndexDelta[i2, i1]*Mf[2, i1] + vev*dZfR1[2, i2, i1]*Mf[2, i2])*Sec[beta]*Sin[alpha])/vev^2 + (I*dbeta1*IndexDelta[i2, i1]*Mf[2, i1]*Sec[beta]*Sin[alpha]*Tan[beta])/vev}},
C[F[2, {i1}], -F[1, {i2}], -S[6]]==
	{{0, 0},
	{((-I)*Sqrt[2]*IndexDelta[i2, i1]*Mf[2, i1])/vev, ((-I)*dZGpGp1*IndexDelta[i2, i1]*Mf[2, i1])/(Sqrt[2]*vev) - (I*(2*vev*dMf1[2, i1]*IndexDelta[i2, i1] + vev*Conjugate[dZfL1[1, i1, i2]]*Mf[2, i1] - 2*dvev1*IndexDelta[i2, i1]*Mf[2, i1] + vev*dZfR1[2, i2, i1]*Mf[2, i2]))/(Sqrt[2]*vev^2) + (I*dZHpGp1*IndexDelta[i2, i1]*Mf[2, i1]*Tan[beta])/(Sqrt[2]*vev)}},
C[F[2, {i1}], -F[1, {i2}], -S[5]]==
	{{0, 0},
	{(I*Sqrt[2]*IndexDelta[i2, i1]*Mf[2, i1]*Tan[beta])/vev, ((-I)*dZGpHp1*IndexDelta[i2, i1]*Mf[2, i1])/(Sqrt[2]*vev) + (I*Sqrt[2]*dbeta1*IndexDelta[i2, i1]*Mf[2, i1]*Sec[beta]^2)/vev + (I*dZHpHp1*IndexDelta[i2, i1]*Mf[2, i1]*Tan[beta])/(Sqrt[2]*vev) + (I*(2*vev*dMf1[2, i1]*IndexDelta[i2, i1] + vev*Conjugate[dZfL1[1, i1, i2]]*Mf[2, i1] - 2*dvev1*IndexDelta[i2, i1]*Mf[2, i1] + vev*dZfR1[2, i2, i1]*Mf[2, i2])*Tan[beta])/(Sqrt[2]*vev^2)}},
C[F[1, {i1}], -F[2, {i2}], S[6]]==
	{{((-I)*Sqrt[2]*IndexDelta[i2, i1]*Mf[2, i1])/vev, ((-I)*dZGpGp1*IndexDelta[i2, i1]*Mf[2, i1])/(Sqrt[2]*vev) - (I*(2*vev*dMf1[2, i1]*IndexDelta[i2, i1] + vev*Conjugate[dZfR1[2, i1, i2]]*Mf[2, i1] - 2*dvev1*IndexDelta[i2, i1]*Mf[2, i1] + vev*dZfL1[1, i2, i1]*Mf[2, i2]))/(Sqrt[2]*vev^2) + (I*dZHpGp1*IndexDelta[i2, i1]*Mf[2, i1]*Tan[beta])/(Sqrt[2]*vev)},
	{0, 0}},
C[F[1, {i1}], -F[2, {i2}], S[5]]==
	{{(I*Sqrt[2]*IndexDelta[i2, i1]*Mf[2, i1]*Tan[beta])/vev, ((-I)*dZGpHp1*IndexDelta[i2, i1]*Mf[2, i1])/(Sqrt[2]*vev) + (I*Sqrt[2]*dbeta1*IndexDelta[i2, i1]*Mf[2, i1]*Sec[beta]^2)/vev + (I*dZHpHp1*IndexDelta[i2, i1]*Mf[2, i1]*Tan[beta])/(Sqrt[2]*vev) + (I*(2*vev*dMf1[2, i1]*IndexDelta[i2, i1] + vev*Conjugate[dZfR1[2, i1, i2]]*Mf[2, i1] - 2*dvev1*IndexDelta[i2, i1]*Mf[2, i1] + vev*dZfL1[1, i2, i1]*Mf[2, i2])*Tan[beta])/(Sqrt[2]*vev^2)},
	{0, 0}},
C[F[4, {i1, c1}], -F[4, {i2, c2}], S[4]]==
	{{-((IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1])/vev), -(dZG0G01*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1])/(2*vev) + (IndexDelta[c1, c2]*(-2*vev*dMf1[4, i1]*IndexDelta[i2, i1] - vev*Conjugate[dZfR1[4, i1, i2]]*Mf[4, i1] + 2*dvev1*IndexDelta[i2, i1]*Mf[4, i1] - vev*dZfL1[4, i2, i1]*Mf[4, i2]))/(2*vev^2) + (dZHAG01*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Tan[beta])/(2*vev)},
	{(IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1])/vev, (dZG0G01*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1])/(2*vev) + (IndexDelta[c1, c2]*(2*vev*dMf1[4, i1]*IndexDelta[i2, i1] + vev*Conjugate[dZfL1[4, i1, i2]]*Mf[4, i1] - 2*dvev1*IndexDelta[i2, i1]*Mf[4, i1] + vev*dZfR1[4, i2, i1]*Mf[4, i2]))/(2*vev^2) - (dZHAG01*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Tan[beta])/(2*vev)}},
C[F[4, {i1, c1}], -F[4, {i2, c2}], S[3]]==
	{{(IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Tan[beta])/vev, -(dZG0HA1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1])/(2*vev) + (dbeta1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]^2)/vev + (dZHAHA1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Tan[beta])/(2*vev) + (IndexDelta[c1, c2]*(2*vev*dMf1[4, i1]*IndexDelta[i2, i1] + vev*Conjugate[dZfR1[4, i1, i2]]*Mf[4, i1] - 2*dvev1*IndexDelta[i2, i1]*Mf[4, i1] + vev*dZfL1[4, i2, i1]*Mf[4, i2])*Tan[beta])/(2*vev^2)},
	{-((IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Tan[beta])/vev), (dZG0HA1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1])/(2*vev) - (dbeta1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]^2)/vev - (dZHAHA1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Tan[beta])/(2*vev) + (IndexDelta[c1, c2]*(-2*vev*dMf1[4, i1]*IndexDelta[i2, i1] - vev*Conjugate[dZfL1[4, i1, i2]]*Mf[4, i1] + 2*dvev1*IndexDelta[i2, i1]*Mf[4, i1] - vev*dZfR1[4, i2, i1]*Mf[4, i2])*Tan[beta])/(2*vev^2)}},
C[F[4, {i1, c1}], -F[4, {i2, c2}], S[2]]==
	{{((-I)*Cos[alpha]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta])/vev, ((-I/2)*dZHHHH1*Cos[alpha]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta])/vev - ((I/2)*Cos[alpha]*IndexDelta[c1, c2]*(2*vev*dMf1[4, i1]*IndexDelta[i2, i1] + vev*Conjugate[dZfR1[4, i1, i2]]*Mf[4, i1] - 2*dvev1*IndexDelta[i2, i1]*Mf[4, i1] + vev*dZfL1[4, i2, i1]*Mf[4, i2])*Sec[beta])/vev^2 + (I*dalpha1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]*Sin[alpha])/vev + ((I/2)*dZHLHH1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]*Sin[alpha])/vev - (I*dbeta1*Cos[alpha]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]*Tan[beta])/vev},
	{((-I)*Cos[alpha]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta])/vev, ((-I/2)*dZHHHH1*Cos[alpha]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta])/vev - ((I/2)*Cos[alpha]*IndexDelta[c1, c2]*(2*vev*dMf1[4, i1]*IndexDelta[i2, i1] + vev*Conjugate[dZfL1[4, i1, i2]]*Mf[4, i1] - 2*dvev1*IndexDelta[i2, i1]*Mf[4, i1] + vev*dZfR1[4, i2, i1]*Mf[4, i2])*Sec[beta])/vev^2 + (I*dalpha1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]*Sin[alpha])/vev + ((I/2)*dZHLHH1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]*Sin[alpha])/vev - (I*dbeta1*Cos[alpha]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]*Tan[beta])/vev}},
C[F[4, {i1, c1}], -F[4, {i2, c2}], S[1]]==
	{{(I*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]*Sin[alpha])/vev, (I*dalpha1*Cos[alpha]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta])/vev - ((I/2)*dZHHHL1*Cos[alpha]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta])/vev + ((I/2)*dZHLHL1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]*Sin[alpha])/vev + ((I/2)*IndexDelta[c1, c2]*(2*vev*dMf1[4, i1]*IndexDelta[i2, i1] + vev*Conjugate[dZfR1[4, i1, i2]]*Mf[4, i1] - 2*dvev1*IndexDelta[i2, i1]*Mf[4, i1] + vev*dZfL1[4, i2, i1]*Mf[4, i2])*Sec[beta]*Sin[alpha])/vev^2 + (I*dbeta1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]*Sin[alpha]*Tan[beta])/vev},
	{(I*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]*Sin[alpha])/vev, (I*dalpha1*Cos[alpha]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta])/vev - ((I/2)*dZHHHL1*Cos[alpha]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta])/vev + ((I/2)*dZHLHL1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]*Sin[alpha])/vev + ((I/2)*IndexDelta[c1, c2]*(2*vev*dMf1[4, i1]*IndexDelta[i2, i1] + vev*Conjugate[dZfL1[4, i1, i2]]*Mf[4, i1] - 2*dvev1*IndexDelta[i2, i1]*Mf[4, i1] + vev*dZfR1[4, i2, i1]*Mf[4, i2])*Sec[beta]*Sin[alpha])/vev^2 + (I*dbeta1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]*Sin[alpha]*Tan[beta])/vev}},
C[F[4, {i1, c1}], -F[3, {i2, c2}], -S[6]]==
	{{(I*Sqrt[2]*CKM[i2, i1]*IndexDelta[c1, c2]*Mf[3, i2])/vev, (I*dZGpGp1*CKM[i2, i1]*IndexDelta[c1, c2]*Mf[3, i2])/(Sqrt[2]*vev) + (I*dZHpGp1*CKM[i2, i1]*Cot[beta]*IndexDelta[c1, c2]*Mf[3, i2])/(Sqrt[2]*vev) + (I*IndexDelta[c1, c2]*(2*vev*CKM[i2, i1]*dMf1[3, i2] + IndexSum[vev*CKM[i2, jDownL$13174]*dZfL1[4, jDownL$13174, i1]*Mf[3, i2], {jDownL$13174, NF}] + IndexSum[vev*CKM[k$14556, i1]*Conjugate[dZfR1[3, k$14556, i2]]*Mf[3, k$14556], {k$14556, NF}] - 2*dvev1*CKM[i2, i1]*Mf[3, i2] + 2*vev*dCKM1[i2, i1]*Mf[3, i2]))/(Sqrt[2]*vev^2)},
	{((-I)*Sqrt[2]*CKM[i2, i1]*IndexDelta[c1, c2]*Mf[4, i1])/vev, ((-I)*dZGpGp1*CKM[i2, i1]*IndexDelta[c1, c2]*Mf[4, i1])/(Sqrt[2]*vev) + (I*IndexDelta[c1, c2]*(-2*vev*CKM[i2, i1]*dMf1[4, i1] + IndexSum[-(vev*CKM[jUpL$13177bar, i1]*Conjugate[dZfL1[3, jUpL$13177bar, i2]]*Mf[4, i1]), {jUpL$13177bar, NF}] + IndexSum[-(vev*CKM[i2, jDR$13178]*dZfR1[4, jDR$13178, i1]*Mf[4, jDR$13178]), {jDR$13178, NF}] + 2*dvev1*CKM[i2, i1]*Mf[4, i1] - 2*vev*dCKM1[i2, i1]*Mf[4, i1]))/(Sqrt[2]*vev^2) + (I*dZHpGp1*CKM[i2, i1]*IndexDelta[c1, c2]*Mf[4, i1]*Tan[beta])/(Sqrt[2]*vev)}},
C[F[4, {i1, c1}], -F[3, {i2, c2}], -S[5]]==
	{{(I*Sqrt[2]*CKM[i2, i1]*Cot[beta]*IndexDelta[c1, c2]*Mf[3, i2])/vev, (I*dZGpHp1*CKM[i2, i1]*IndexDelta[c1, c2]*Mf[3, i2])/(Sqrt[2]*vev) + (I*dZHpHp1*CKM[i2, i1]*Cot[beta]*IndexDelta[c1, c2]*Mf[3, i2])/(Sqrt[2]*vev) - (I*Sqrt[2]*dbeta1*CKM[i2, i1]*Csc[beta]^2*IndexDelta[c1, c2]*Mf[3, i2])/vev + (I*IndexDelta[c1, c2]*(2*vev*CKM[i2, i1]*Cot[beta]^2*dMf1[3, i2] + IndexSum[vev*CKM[i2, jDownL$13174]*Cot[beta]^2*dZfL1[4, jDownL$13174, i1]*Mf[3, i2], {jDownL$13174, NF}] + IndexSum[vev*CKM[k$14556, i1]*Conjugate[dZfR1[3, k$14556, i2]]*Cot[beta]^2*Mf[3, k$14556], {k$14556, NF}] - 2*dvev1*CKM[i2, i1]*Cot[beta]^2*Mf[3, i2] + 2*vev*Cot[beta]^2*dCKM1[i2, i1]*Mf[3, i2])*Tan[beta])/(Sqrt[2]*vev^2)},
	{(I*Sqrt[2]*CKM[i2, i1]*IndexDelta[c1, c2]*Mf[4, i1]*Tan[beta])/vev, ((-I)*dZGpHp1*CKM[i2, i1]*IndexDelta[c1, c2]*Mf[4, i1])/(Sqrt[2]*vev) + (I*Sqrt[2]*dbeta1*CKM[i2, i1]*IndexDelta[c1, c2]*Mf[4, i1]*Sec[beta]^2)/vev + (I*dZHpHp1*CKM[i2, i1]*IndexDelta[c1, c2]*Mf[4, i1]*Tan[beta])/(Sqrt[2]*vev) + (I*IndexDelta[c1, c2]*(2*vev*CKM[i2, i1]*dMf1[4, i1] + IndexSum[vev*CKM[jUpL$13177bar, i1]*Conjugate[dZfL1[3, jUpL$13177bar, i2]]*Mf[4, i1], {jUpL$13177bar, NF}] + IndexSum[vev*CKM[i2, jDR$13178]*dZfR1[4, jDR$13178, i1]*Mf[4, jDR$13178], {jDR$13178, NF}] - 2*dvev1*CKM[i2, i1]*Mf[4, i1] + 2*vev*dCKM1[i2, i1]*Mf[4, i1])*Tan[beta])/(Sqrt[2]*vev^2)}},
C[F[3, {i1, c1}], -F[4, {i2, c2}], S[6]]==
	{{((-I)*Sqrt[2]*Conjugate[CKM[i2, i1]]*IndexDelta[c1, c2]*Mf[4, i2])/vev, ((-I)*dZGpGp1*Conjugate[CKM[i2, i1]]*IndexDelta[c1, c2]*Mf[4, i2])/(Sqrt[2]*vev) + (I*IndexDelta[c1, c2]*(-2*vev*Conjugate[CKM[i2, i1]]*dMf1[4, i2] + IndexSum[-(vev*Conjugate[CKM[i2, jUpL$13179]]*dZfL1[3, jUpL$13179, i1]*Mf[4, i2]), {jUpL$13179, NF}] + IndexSum[-(vev*Conjugate[CKM[k$14557, i1]]*Conjugate[dZfR1[4, k$14557, i2]]*Mf[4, k$14557]), {k$14557, NF}] + 2*dvev1*Conjugate[CKM[i2, i1]]*Mf[4, i2] - 2*vev*Conjugate[dCKM1[i2, i1]]*Mf[4, i2]))/(Sqrt[2]*vev^2) + (I*dZHpGp1*Conjugate[CKM[i2, i1]]*IndexDelta[c1, c2]*Mf[4, i2]*Tan[beta])/(Sqrt[2]*vev)},
	{(I*Sqrt[2]*Conjugate[CKM[i2, i1]]*IndexDelta[c1, c2]*Mf[3, i1])/vev, (I*dZGpGp1*Conjugate[CKM[i2, i1]]*IndexDelta[c1, c2]*Mf[3, i1])/(Sqrt[2]*vev) + (I*dZHpGp1*Conjugate[CKM[i2, i1]]*Cot[beta]*IndexDelta[c1, c2]*Mf[3, i1])/(Sqrt[2]*vev) + (I*IndexDelta[c1, c2]*(2*vev*Conjugate[CKM[i2, i1]]*dMf1[3, i1] + IndexSum[vev*Conjugate[CKM[jDownL$13172bar, i1]]*Conjugate[dZfL1[4, jDownL$13172bar, i2]]*Mf[3, i1], {jDownL$13172bar, NF}] + IndexSum[vev*Conjugate[CKM[i2, jUR$13173]]*dZfR1[3, jUR$13173, i1]*Mf[3, jUR$13173], {jUR$13173, NF}] - 2*dvev1*Conjugate[CKM[i2, i1]]*Mf[3, i1] + 2*vev*Conjugate[dCKM1[i2, i1]]*Mf[3, i1]))/(Sqrt[2]*vev^2)}},
C[F[3, {i1, c1}], -F[4, {i2, c2}], S[5]]==
	{{(I*Sqrt[2]*Conjugate[CKM[i2, i1]]*IndexDelta[c1, c2]*Mf[4, i2]*Tan[beta])/vev, ((-I)*dZGpHp1*Conjugate[CKM[i2, i1]]*IndexDelta[c1, c2]*Mf[4, i2])/(Sqrt[2]*vev) + (I*Sqrt[2]*dbeta1*Conjugate[CKM[i2, i1]]*IndexDelta[c1, c2]*Mf[4, i2]*Sec[beta]^2)/vev + (I*dZHpHp1*Conjugate[CKM[i2, i1]]*IndexDelta[c1, c2]*Mf[4, i2]*Tan[beta])/(Sqrt[2]*vev) + (I*IndexDelta[c1, c2]*(2*vev*Conjugate[CKM[i2, i1]]*dMf1[4, i2] + IndexSum[vev*Conjugate[CKM[i2, jUpL$13179]]*dZfL1[3, jUpL$13179, i1]*Mf[4, i2], {jUpL$13179, NF}] + IndexSum[vev*Conjugate[CKM[k$14557, i1]]*Conjugate[dZfR1[4, k$14557, i2]]*Mf[4, k$14557], {k$14557, NF}] - 2*dvev1*Conjugate[CKM[i2, i1]]*Mf[4, i2] + 2*vev*Conjugate[dCKM1[i2, i1]]*Mf[4, i2])*Tan[beta])/(Sqrt[2]*vev^2)},
	{(I*Sqrt[2]*Conjugate[CKM[i2, i1]]*Cot[beta]*IndexDelta[c1, c2]*Mf[3, i1])/vev, (I*dZGpHp1*Conjugate[CKM[i2, i1]]*IndexDelta[c1, c2]*Mf[3, i1])/(Sqrt[2]*vev) + (I*dZHpHp1*Conjugate[CKM[i2, i1]]*Cot[beta]*IndexDelta[c1, c2]*Mf[3, i1])/(Sqrt[2]*vev) - (I*Sqrt[2]*dbeta1*Conjugate[CKM[i2, i1]]*Csc[beta]^2*IndexDelta[c1, c2]*Mf[3, i1])/vev + (I*IndexDelta[c1, c2]*(2*vev*Conjugate[CKM[i2, i1]]*Cot[beta]^2*dMf1[3, i1] + IndexSum[vev*Conjugate[CKM[jDownL$13172bar, i1]]*Conjugate[dZfL1[4, jDownL$13172bar, i2]]*Cot[beta]^2*Mf[3, i1], {jDownL$13172bar, NF}] + IndexSum[vev*Conjugate[CKM[i2, jUR$13173]]*Cot[beta]^2*dZfR1[3, jUR$13173, i1]*Mf[3, jUR$13173], {jUR$13173, NF}] - 2*dvev1*Conjugate[CKM[i2, i1]]*Cot[beta]^2*Mf[3, i1] + 2*vev*Conjugate[dCKM1[i2, i1]]*Cot[beta]^2*Mf[3, i1])*Tan[beta])/(Sqrt[2]*vev^2)}},
C[F[3, {i1, c1}], -F[3, {i2, c2}], S[4]]==
	{{(IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev, (dZG0G01*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/(2*vev) + (dZHAG01*Cot[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/(2*vev) + (IndexDelta[c1, c2]*(2*vev*dMf1[3, i1]*IndexDelta[i2, i1] + vev*Conjugate[dZfR1[3, i1, i2]]*Mf[3, i1] - 2*dvev1*IndexDelta[i2, i1]*Mf[3, i1] + vev*dZfL1[3, i2, i1]*Mf[3, i2]))/(2*vev^2)},
	{-((IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev), -(dZG0G01*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/(2*vev) - (dZHAG01*Cot[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/(2*vev) + (IndexDelta[c1, c2]*(-2*vev*dMf1[3, i1]*IndexDelta[i2, i1] - vev*Conjugate[dZfL1[3, i1, i2]]*Mf[3, i1] + 2*dvev1*IndexDelta[i2, i1]*Mf[3, i1] - vev*dZfR1[3, i2, i1]*Mf[3, i2]))/(2*vev^2)}},
C[F[3, {i1, c1}], -F[3, {i2, c2}], S[3]]==
	{{(Cot[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev, (dZG0HA1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/(2*vev) + (dZHAHA1*Cot[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/(2*vev) - (dbeta1*Csc[beta]^2*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev + (Cot[beta]*IndexDelta[c1, c2]*(2*vev*dMf1[3, i1]*IndexDelta[i2, i1] + vev*Conjugate[dZfR1[3, i1, i2]]*Mf[3, i1] - 2*dvev1*IndexDelta[i2, i1]*Mf[3, i1] + vev*dZfL1[3, i2, i1]*Mf[3, i2]))/(2*vev^2)},
	{-((Cot[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev), -(dZG0HA1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/(2*vev) - (dZHAHA1*Cot[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/(2*vev) + (dbeta1*Csc[beta]^2*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev + (Cot[beta]*IndexDelta[c1, c2]*(-2*vev*dMf1[3, i1]*IndexDelta[i2, i1] - vev*Conjugate[dZfL1[3, i1, i2]]*Mf[3, i1] + 2*dvev1*IndexDelta[i2, i1]*Mf[3, i1] - vev*dZfR1[3, i2, i1]*Mf[3, i2]))/(2*vev^2)}},
C[F[3, {i1, c1}], -F[3, {i2, c2}], S[2]]==
	{{((-I)*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1]*Sin[alpha])/vev, ((-I)*dalpha1*Cos[alpha]*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev - ((I/2)*dZHLHH1*Cos[alpha]*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev - ((I/2)*dZHHHH1*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1]*Sin[alpha])/vev + (I*dbeta1*Cot[beta]*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1]*Sin[alpha])/vev - ((I/2)*Csc[beta]*IndexDelta[c1, c2]*(2*vev*dMf1[3, i1]*IndexDelta[i2, i1] + vev*Conjugate[dZfR1[3, i1, i2]]*Mf[3, i1] - 2*dvev1*IndexDelta[i2, i1]*Mf[3, i1] + vev*dZfL1[3, i2, i1]*Mf[3, i2])*Sin[alpha])/vev^2},
	{((-I)*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1]*Sin[alpha])/vev, ((-I)*dalpha1*Cos[alpha]*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev - ((I/2)*dZHLHH1*Cos[alpha]*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev - ((I/2)*dZHHHH1*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1]*Sin[alpha])/vev + (I*dbeta1*Cot[beta]*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1]*Sin[alpha])/vev - ((I/2)*Csc[beta]*IndexDelta[c1, c2]*(2*vev*dMf1[3, i1]*IndexDelta[i2, i1] + vev*Conjugate[dZfL1[3, i1, i2]]*Mf[3, i1] - 2*dvev1*IndexDelta[i2, i1]*Mf[3, i1] + vev*dZfR1[3, i2, i1]*Mf[3, i2])*Sin[alpha])/vev^2}},
C[F[3, {i1, c1}], -F[3, {i2, c2}], S[1]]==
	{{((-I)*Cos[alpha]*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev, ((-I/2)*dZHLHL1*Cos[alpha]*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev + (I*dbeta1*Cos[alpha]*Cot[beta]*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev - ((I/2)*Cos[alpha]*Csc[beta]*IndexDelta[c1, c2]*(2*vev*dMf1[3, i1]*IndexDelta[i2, i1] + vev*Conjugate[dZfR1[3, i1, i2]]*Mf[3, i1] - 2*dvev1*IndexDelta[i2, i1]*Mf[3, i1] + vev*dZfL1[3, i2, i1]*Mf[3, i2]))/vev^2 + (I*dalpha1*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1]*Sin[alpha])/vev - ((I/2)*dZHHHL1*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1]*Sin[alpha])/vev},
	{((-I)*Cos[alpha]*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev, ((-I/2)*dZHLHL1*Cos[alpha]*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev + (I*dbeta1*Cos[alpha]*Cot[beta]*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev - ((I/2)*Cos[alpha]*Csc[beta]*IndexDelta[c1, c2]*(2*vev*dMf1[3, i1]*IndexDelta[i2, i1] + vev*Conjugate[dZfL1[3, i1, i2]]*Mf[3, i1] - 2*dvev1*IndexDelta[i2, i1]*Mf[3, i1] + vev*dZfR1[3, i2, i1]*Mf[3, i2]))/vev^2 + (I*dalpha1*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1]*Sin[alpha])/vev - ((I/2)*dZHHHL1*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1]*Sin[alpha])/vev}}
}];