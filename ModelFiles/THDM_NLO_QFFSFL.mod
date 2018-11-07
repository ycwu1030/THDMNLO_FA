M$CouplingMatrices = Join[ M$CouplingMatrices, {
C[F[3, {i1, c1}], -F[3, {i2, c2}], S[1]]==
	{{((-I)*Cos[alpha]*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev, ((-I)*Cos[alpha]*Csc[beta]*dMf[3, i1]*IndexDelta[c1, c2]*IndexDelta[i2, i1])/vev - ((I/2)*Conjugate[dZfR1[3, i1, i2]]*Cos[alpha]*Csc[beta]*IndexDelta[c1, c2]*Mf[3, i1])/vev + (I*dvev1*Cos[alpha]*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev^2 - ((I/2)*dZHLHL1*Cos[alpha]*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev + (I*dbeta1*Cos[alpha]*Cot[beta]*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev - ((I/2)*Cos[alpha]*Csc[beta]*dZfL1[3, i2, i1]*IndexDelta[c1, c2]*Mf[3, i2])/vev + (I*dalpha1*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1]*Sin[alpha])/vev - ((I/2)*dZHHHL1*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1]*Sin[alpha])/vev},
	{((-I)*Cos[alpha]*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev, ((-I)*Cos[alpha]*Csc[beta]*dMf[3, i1]*IndexDelta[c1, c2]*IndexDelta[i2, i1])/vev - ((I/2)*Conjugate[dZfL1[3, i1, i2]]*Cos[alpha]*Csc[beta]*IndexDelta[c1, c2]*Mf[3, i1])/vev + (I*dvev1*Cos[alpha]*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev^2 - ((I/2)*dZHLHL1*Cos[alpha]*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev + (I*dbeta1*Cos[alpha]*Cot[beta]*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev - ((I/2)*Cos[alpha]*Csc[beta]*dZfR1[3, i2, i1]*IndexDelta[c1, c2]*Mf[3, i2])/vev + (I*dalpha1*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1]*Sin[alpha])/vev - ((I/2)*dZHHHL1*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1]*Sin[alpha])/vev}},
C[F[3, {i1, c1}], -F[3, {i2, c2}], S[2]]==
	{{((-I)*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1]*Sin[alpha])/vev, ((-I)*dalpha1*Cos[alpha]*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev - ((I/2)*dZHLHH1*Cos[alpha]*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev - (I*Csc[beta]*dMf[3, i1]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Sin[alpha])/vev - ((I/2)*Conjugate[dZfR1[3, i1, i2]]*Csc[beta]*IndexDelta[c1, c2]*Mf[3, i1]*Sin[alpha])/vev + (I*dvev1*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1]*Sin[alpha])/vev^2 - ((I/2)*dZHHHH1*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1]*Sin[alpha])/vev + (I*dbeta1*Cot[beta]*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1]*Sin[alpha])/vev - ((I/2)*Csc[beta]*dZfL1[3, i2, i1]*IndexDelta[c1, c2]*Mf[3, i2]*Sin[alpha])/vev},
	{((-I)*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1]*Sin[alpha])/vev, ((-I)*dalpha1*Cos[alpha]*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev - ((I/2)*dZHLHH1*Cos[alpha]*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev - (I*Csc[beta]*dMf[3, i1]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Sin[alpha])/vev - ((I/2)*Conjugate[dZfL1[3, i1, i2]]*Csc[beta]*IndexDelta[c1, c2]*Mf[3, i1]*Sin[alpha])/vev + (I*dvev1*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1]*Sin[alpha])/vev^2 - ((I/2)*dZHHHH1*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1]*Sin[alpha])/vev + (I*dbeta1*Cot[beta]*Csc[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1]*Sin[alpha])/vev - ((I/2)*Csc[beta]*dZfR1[3, i2, i1]*IndexDelta[c1, c2]*Mf[3, i2]*Sin[alpha])/vev}},
C[F[3, {i1, c1}], -F[3, {i2, c2}], S[3]]==
	{{(Cot[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev, (Cot[beta]*dMf[3, i1]*IndexDelta[c1, c2]*IndexDelta[i2, i1])/vev + (Conjugate[dZfR1[3, i1, i2]]*Cot[beta]*IndexDelta[c1, c2]*Mf[3, i1])/(2*vev) + (dZG0HA1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/(2*vev) - (dvev1*Cot[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev^2 + (dZHAHA1*Cot[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/(2*vev) - (dbeta1*Csc[beta]^2*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev + (Cot[beta]*dZfL1[3, i2, i1]*IndexDelta[c1, c2]*Mf[3, i2])/(2*vev)},
	{-((Cot[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev), -((Cot[beta]*dMf[3, i1]*IndexDelta[c1, c2]*IndexDelta[i2, i1])/vev) - (Conjugate[dZfL1[3, i1, i2]]*Cot[beta]*IndexDelta[c1, c2]*Mf[3, i1])/(2*vev) - (dZG0HA1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/(2*vev) + (dvev1*Cot[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev^2 - (dZHAHA1*Cot[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/(2*vev) + (dbeta1*Csc[beta]^2*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev - (Cot[beta]*dZfR1[3, i2, i1]*IndexDelta[c1, c2]*Mf[3, i2])/(2*vev)}},
C[F[3, {i1, c1}], -F[3, {i2, c2}], S[4]]==
	{{(IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev, (dMf[3, i1]*IndexDelta[c1, c2]*IndexDelta[i2, i1])/vev + (Conjugate[dZfR1[3, i1, i2]]*IndexDelta[c1, c2]*Mf[3, i1])/(2*vev) - (dvev1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev^2 + (dZG0G01*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/(2*vev) + (dZHAG01*Cot[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/(2*vev) + (dZfL1[3, i2, i1]*IndexDelta[c1, c2]*Mf[3, i2])/(2*vev)},
	{-((IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev), -((dMf[3, i1]*IndexDelta[c1, c2]*IndexDelta[i2, i1])/vev) - (Conjugate[dZfL1[3, i1, i2]]*IndexDelta[c1, c2]*Mf[3, i1])/(2*vev) + (dvev1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/vev^2 - (dZG0G01*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/(2*vev) - (dZHAG01*Cot[beta]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[3, i1])/(2*vev) - (dZfR1[3, i2, i1]*IndexDelta[c1, c2]*Mf[3, i2])/(2*vev)}},
C[F[3, {i1, c1}], -F[4, {i2, c2}], S[5]]==
	{{(I*Sqrt[2]*Conjugate[CKM[i2, i1]]*IndexDelta[c1, c2]*Mf[4, i2]*Tan[beta])/vev, IndexSum[(I*Conjugate[CKM[i2, jUpL$14754]]*dZfL1[3, jUpL$14754, i1]*IndexDelta[c1, c2]*Mf[4, i2]*Tan[beta])/(Sqrt[2]*vev), {jUpL$14754, NF}] + IndexSum[(I*Conjugate[CKM[k$17273, i1]]*Conjugate[dZfR1[4, k$17273, i2]]*IndexDelta[c1, c2]*Mf[4, k$17273]*Tan[beta])/(Sqrt[2]*vev), {k$17273, NF}] + (I*Sqrt[2]*dbeta1*Conjugate[CKM[i2, i1]]*IndexDelta[c1, c2]*Mf[4, i2])/vev - (I*dZGpHp1*Conjugate[CKM[i2, i1]]*IndexDelta[c1, c2]*Mf[4, i2])/(Sqrt[2]*vev) + (I*Sqrt[2]*Conjugate[CKM[i2, i1]]*dMf[4, i2]*IndexDelta[c1, c2]*Tan[beta])/vev - (I*Sqrt[2]*dvev1*Conjugate[CKM[i2, i1]]*IndexDelta[c1, c2]*Mf[4, i2]*Tan[beta])/vev^2 + (I*dZHpHp1*Conjugate[CKM[i2, i1]]*IndexDelta[c1, c2]*Mf[4, i2]*Tan[beta])/(Sqrt[2]*vev) + (I*Sqrt[2]*Conjugate[dCKM1[i2, i1]]*IndexDelta[c1, c2]*Mf[4, i2]*Tan[beta])/vev + (I*Sqrt[2]*dbeta1*Conjugate[CKM[i2, i1]]*IndexDelta[c1, c2]*Mf[4, i2]*Tan[beta]^2)/vev},
	{(I*Sqrt[2]*Conjugate[CKM[i2, i1]]*Cot[beta]*IndexDelta[c1, c2]*Mf[3, i1])/vev, (I*Sqrt[2]*Conjugate[CKM[i2, i1]]*Cot[beta]*dMf[3, i1]*IndexDelta[c1, c2])/vev + IndexSum[(I*Conjugate[CKM[jDownL$14747bar, i1]]*Conjugate[dZfL1[4, jDownL$14747bar, i2]]*Cot[beta]*IndexDelta[c1, c2]*Mf[3, i1])/(Sqrt[2]*vev), {jDownL$14747bar, NF}] + IndexSum[(I*Conjugate[CKM[i2, jUR$14748]]*Cot[beta]*dZfR1[3, jUR$14748, i1]*IndexDelta[c1, c2]*Mf[3, jUR$14748])/(Sqrt[2]*vev), {jUR$14748, NF}] - (I*Sqrt[2]*dbeta1*Conjugate[CKM[i2, i1]]*IndexDelta[c1, c2]*Mf[3, i1])/vev + (I*dZGpHp1*Conjugate[CKM[i2, i1]]*IndexDelta[c1, c2]*Mf[3, i1])/(Sqrt[2]*vev) - (I*Sqrt[2]*dvev1*Conjugate[CKM[i2, i1]]*Cot[beta]*IndexDelta[c1, c2]*Mf[3, i1])/vev^2 + (I*dZHpHp1*Conjugate[CKM[i2, i1]]*Cot[beta]*IndexDelta[c1, c2]*Mf[3, i1])/(Sqrt[2]*vev) + (I*Sqrt[2]*Conjugate[dCKM1[i2, i1]]*Cot[beta]*IndexDelta[c1, c2]*Mf[3, i1])/vev - (I*Sqrt[2]*dbeta1*Conjugate[CKM[i2, i1]]*Cot[beta]^2*IndexDelta[c1, c2]*Mf[3, i1])/vev}},
C[F[3, {i1, c1}], -F[4, {i2, c2}], S[6]]==
	{{((-I)*Sqrt[2]*Conjugate[CKM[i2, i1]]*IndexDelta[c1, c2]*Mf[4, i2])/vev, ((-I)*Sqrt[2]*Conjugate[CKM[i2, i1]]*dMf[4, i2]*IndexDelta[c1, c2])/vev + IndexSum[((-I)*Conjugate[CKM[i2, jUpL$14754]]*dZfL1[3, jUpL$14754, i1]*IndexDelta[c1, c2]*Mf[4, i2])/(Sqrt[2]*vev), {jUpL$14754, NF}] + IndexSum[((-I)*Conjugate[CKM[k$17273, i1]]*Conjugate[dZfR1[4, k$17273, i2]]*IndexDelta[c1, c2]*Mf[4, k$17273])/(Sqrt[2]*vev), {k$17273, NF}] + (I*Sqrt[2]*dvev1*Conjugate[CKM[i2, i1]]*IndexDelta[c1, c2]*Mf[4, i2])/vev^2 - (I*dZGpGp1*Conjugate[CKM[i2, i1]]*IndexDelta[c1, c2]*Mf[4, i2])/(Sqrt[2]*vev) - (I*Sqrt[2]*Conjugate[dCKM1[i2, i1]]*IndexDelta[c1, c2]*Mf[4, i2])/vev + (I*dZHpGp1*Conjugate[CKM[i2, i1]]*IndexDelta[c1, c2]*Mf[4, i2]*Tan[beta])/(Sqrt[2]*vev)},
	{(I*Sqrt[2]*Conjugate[CKM[i2, i1]]*IndexDelta[c1, c2]*Mf[3, i1])/vev, (I*Sqrt[2]*Conjugate[CKM[i2, i1]]*dMf[3, i1]*IndexDelta[c1, c2])/vev + IndexSum[(I*Conjugate[CKM[jDownL$14747bar, i1]]*Conjugate[dZfL1[4, jDownL$14747bar, i2]]*IndexDelta[c1, c2]*Mf[3, i1])/(Sqrt[2]*vev), {jDownL$14747bar, NF}] + IndexSum[(I*Conjugate[CKM[i2, jUR$14748]]*dZfR1[3, jUR$14748, i1]*IndexDelta[c1, c2]*Mf[3, jUR$14748])/(Sqrt[2]*vev), {jUR$14748, NF}] - (I*Sqrt[2]*dvev1*Conjugate[CKM[i2, i1]]*IndexDelta[c1, c2]*Mf[3, i1])/vev^2 + (I*dZGpGp1*Conjugate[CKM[i2, i1]]*IndexDelta[c1, c2]*Mf[3, i1])/(Sqrt[2]*vev) + (I*Sqrt[2]*Conjugate[dCKM1[i2, i1]]*IndexDelta[c1, c2]*Mf[3, i1])/vev + (I*dZHpGp1*Conjugate[CKM[i2, i1]]*Cot[beta]*IndexDelta[c1, c2]*Mf[3, i1])/(Sqrt[2]*vev)}},
C[F[4, {i1, c1}], -F[3, {i2, c2}], -S[5]]==
	{{(I*Sqrt[2]*CKM[i2, i1]*Cot[beta]*IndexDelta[c1, c2]*Mf[3, i2])/vev, (I*Sqrt[2]*CKM[i2, i1]*Cot[beta]*dMf[3, i2]*IndexDelta[c1, c2])/vev + IndexSum[(I*CKM[i2, jDownL$14749]*Cot[beta]*dZfL1[4, jDownL$14749, i1]*IndexDelta[c1, c2]*Mf[3, i2])/(Sqrt[2]*vev), {jDownL$14749, NF}] + IndexSum[(I*CKM[k$17272, i1]*Conjugate[dZfR1[3, k$17272, i2]]*Cot[beta]*IndexDelta[c1, c2]*Mf[3, k$17272])/(Sqrt[2]*vev), {k$17272, NF}] - (I*Sqrt[2]*dbeta1*CKM[i2, i1]*IndexDelta[c1, c2]*Mf[3, i2])/vev + (I*dZGpHp1*CKM[i2, i1]*IndexDelta[c1, c2]*Mf[3, i2])/(Sqrt[2]*vev) - (I*Sqrt[2]*dvev1*CKM[i2, i1]*Cot[beta]*IndexDelta[c1, c2]*Mf[3, i2])/vev^2 + (I*dZHpHp1*CKM[i2, i1]*Cot[beta]*IndexDelta[c1, c2]*Mf[3, i2])/(Sqrt[2]*vev) - (I*Sqrt[2]*dbeta1*CKM[i2, i1]*Cot[beta]^2*IndexDelta[c1, c2]*Mf[3, i2])/vev + (I*Sqrt[2]*Cot[beta]*dCKM1[i2, i1]*IndexDelta[c1, c2]*Mf[3, i2])/vev},
	{(I*Sqrt[2]*CKM[i2, i1]*IndexDelta[c1, c2]*Mf[4, i1]*Tan[beta])/vev, IndexSum[(I*CKM[jUpL$14752bar, i1]*Conjugate[dZfL1[3, jUpL$14752bar, i2]]*IndexDelta[c1, c2]*Mf[4, i1]*Tan[beta])/(Sqrt[2]*vev), {jUpL$14752bar, NF}] + IndexSum[(I*CKM[i2, jDR$14753]*dZfR1[4, jDR$14753, i1]*IndexDelta[c1, c2]*Mf[4, jDR$14753]*Tan[beta])/(Sqrt[2]*vev), {jDR$14753, NF}] + (I*Sqrt[2]*dbeta1*CKM[i2, i1]*IndexDelta[c1, c2]*Mf[4, i1])/vev - (I*dZGpHp1*CKM[i2, i1]*IndexDelta[c1, c2]*Mf[4, i1])/(Sqrt[2]*vev) + (I*Sqrt[2]*CKM[i2, i1]*dMf[4, i1]*IndexDelta[c1, c2]*Tan[beta])/vev - (I*Sqrt[2]*dvev1*CKM[i2, i1]*IndexDelta[c1, c2]*Mf[4, i1]*Tan[beta])/vev^2 + (I*dZHpHp1*CKM[i2, i1]*IndexDelta[c1, c2]*Mf[4, i1]*Tan[beta])/(Sqrt[2]*vev) + (I*Sqrt[2]*dCKM1[i2, i1]*IndexDelta[c1, c2]*Mf[4, i1]*Tan[beta])/vev + (I*Sqrt[2]*dbeta1*CKM[i2, i1]*IndexDelta[c1, c2]*Mf[4, i1]*Tan[beta]^2)/vev}},
C[F[4, {i1, c1}], -F[3, {i2, c2}], -S[6]]==
	{{(I*Sqrt[2]*CKM[i2, i1]*IndexDelta[c1, c2]*Mf[3, i2])/vev, (I*Sqrt[2]*CKM[i2, i1]*dMf[3, i2]*IndexDelta[c1, c2])/vev + IndexSum[(I*CKM[i2, jDownL$14749]*dZfL1[4, jDownL$14749, i1]*IndexDelta[c1, c2]*Mf[3, i2])/(Sqrt[2]*vev), {jDownL$14749, NF}] + IndexSum[(I*CKM[k$17272, i1]*Conjugate[dZfR1[3, k$17272, i2]]*IndexDelta[c1, c2]*Mf[3, k$17272])/(Sqrt[2]*vev), {k$17272, NF}] - (I*Sqrt[2]*dvev1*CKM[i2, i1]*IndexDelta[c1, c2]*Mf[3, i2])/vev^2 + (I*dZGpGp1*CKM[i2, i1]*IndexDelta[c1, c2]*Mf[3, i2])/(Sqrt[2]*vev) + (I*dZHpGp1*CKM[i2, i1]*Cot[beta]*IndexDelta[c1, c2]*Mf[3, i2])/(Sqrt[2]*vev) + (I*Sqrt[2]*dCKM1[i2, i1]*IndexDelta[c1, c2]*Mf[3, i2])/vev},
	{((-I)*Sqrt[2]*CKM[i2, i1]*IndexDelta[c1, c2]*Mf[4, i1])/vev, ((-I)*Sqrt[2]*CKM[i2, i1]*dMf[4, i1]*IndexDelta[c1, c2])/vev + IndexSum[((-I)*CKM[jUpL$14752bar, i1]*Conjugate[dZfL1[3, jUpL$14752bar, i2]]*IndexDelta[c1, c2]*Mf[4, i1])/(Sqrt[2]*vev), {jUpL$14752bar, NF}] + IndexSum[((-I)*CKM[i2, jDR$14753]*dZfR1[4, jDR$14753, i1]*IndexDelta[c1, c2]*Mf[4, jDR$14753])/(Sqrt[2]*vev), {jDR$14753, NF}] + (I*Sqrt[2]*dvev1*CKM[i2, i1]*IndexDelta[c1, c2]*Mf[4, i1])/vev^2 - (I*dZGpGp1*CKM[i2, i1]*IndexDelta[c1, c2]*Mf[4, i1])/(Sqrt[2]*vev) - (I*Sqrt[2]*dCKM1[i2, i1]*IndexDelta[c1, c2]*Mf[4, i1])/vev + (I*dZHpGp1*CKM[i2, i1]*IndexDelta[c1, c2]*Mf[4, i1]*Tan[beta])/(Sqrt[2]*vev)}},
C[F[4, {i1, c1}], -F[4, {i2, c2}], S[1]]==
	{{(I*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]*Sin[alpha])/vev, (I*dalpha1*Cos[alpha]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta])/vev - ((I/2)*dZHHHL1*Cos[alpha]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta])/vev + (I*dMf[4, i1]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Sec[beta]*Sin[alpha])/vev + ((I/2)*Conjugate[dZfR1[4, i1, i2]]*IndexDelta[c1, c2]*Mf[4, i1]*Sec[beta]*Sin[alpha])/vev - (I*dvev1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]*Sin[alpha])/vev^2 + ((I/2)*dZHLHL1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]*Sin[alpha])/vev + ((I/2)*dZfL1[4, i2, i1]*IndexDelta[c1, c2]*Mf[4, i2]*Sec[beta]*Sin[alpha])/vev + (I*dbeta1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]*Sin[alpha]*Tan[beta])/vev},
	{(I*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]*Sin[alpha])/vev, (I*dalpha1*Cos[alpha]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta])/vev - ((I/2)*dZHHHL1*Cos[alpha]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta])/vev + (I*dMf[4, i1]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Sec[beta]*Sin[alpha])/vev + ((I/2)*Conjugate[dZfL1[4, i1, i2]]*IndexDelta[c1, c2]*Mf[4, i1]*Sec[beta]*Sin[alpha])/vev - (I*dvev1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]*Sin[alpha])/vev^2 + ((I/2)*dZHLHL1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]*Sin[alpha])/vev + ((I/2)*dZfR1[4, i2, i1]*IndexDelta[c1, c2]*Mf[4, i2]*Sec[beta]*Sin[alpha])/vev + (I*dbeta1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]*Sin[alpha]*Tan[beta])/vev}},
C[F[4, {i1, c1}], -F[4, {i2, c2}], S[2]]==
	{{((-I)*Cos[alpha]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta])/vev, ((-I)*Cos[alpha]*dMf[4, i1]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Sec[beta])/vev - ((I/2)*Conjugate[dZfR1[4, i1, i2]]*Cos[alpha]*IndexDelta[c1, c2]*Mf[4, i1]*Sec[beta])/vev + (I*dvev1*Cos[alpha]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta])/vev^2 - ((I/2)*dZHHHH1*Cos[alpha]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta])/vev - ((I/2)*Cos[alpha]*dZfL1[4, i2, i1]*IndexDelta[c1, c2]*Mf[4, i2]*Sec[beta])/vev + (I*dalpha1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]*Sin[alpha])/vev + ((I/2)*dZHLHH1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]*Sin[alpha])/vev - (I*dbeta1*Cos[alpha]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]*Tan[beta])/vev},
	{((-I)*Cos[alpha]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta])/vev, ((-I)*Cos[alpha]*dMf[4, i1]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Sec[beta])/vev - ((I/2)*Conjugate[dZfL1[4, i1, i2]]*Cos[alpha]*IndexDelta[c1, c2]*Mf[4, i1]*Sec[beta])/vev + (I*dvev1*Cos[alpha]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta])/vev^2 - ((I/2)*dZHHHH1*Cos[alpha]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta])/vev - ((I/2)*Cos[alpha]*dZfR1[4, i2, i1]*IndexDelta[c1, c2]*Mf[4, i2]*Sec[beta])/vev + (I*dalpha1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]*Sin[alpha])/vev + ((I/2)*dZHLHH1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]*Sin[alpha])/vev - (I*dbeta1*Cos[alpha]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]*Tan[beta])/vev}},
C[F[4, {i1, c1}], -F[4, {i2, c2}], S[3]]==
	{{(IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Tan[beta])/vev, -(dZG0HA1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1])/(2*vev) + (dbeta1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]^2)/vev + (dMf[4, i1]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Tan[beta])/vev + (Conjugate[dZfR1[4, i1, i2]]*IndexDelta[c1, c2]*Mf[4, i1]*Tan[beta])/(2*vev) - (dvev1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Tan[beta])/vev^2 + (dZHAHA1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Tan[beta])/(2*vev) + (dZfL1[4, i2, i1]*IndexDelta[c1, c2]*Mf[4, i2]*Tan[beta])/(2*vev)},
	{-((IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Tan[beta])/vev), (dZG0HA1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1])/(2*vev) - (dbeta1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]^2)/vev - (Conjugate[dZfL1[4, i1, i2]]*IndexDelta[c1, c2]*Mf[4, i1]*Sec[beta]^2*Sin[2*beta])/(4*vev) + (dvev1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Sec[beta]^2*Sin[2*beta])/(2*vev^2) - (dZfR1[4, i2, i1]*IndexDelta[c1, c2]*Mf[4, i2]*Sec[beta]^2*Sin[2*beta])/(4*vev) - (dMf[4, i1]*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Tan[beta])/vev - (dZHAHA1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Tan[beta])/(2*vev)}},
C[F[4, {i1, c1}], -F[4, {i2, c2}], S[4]]==
	{{-((IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1])/vev), -((dMf[4, i1]*IndexDelta[c1, c2]*IndexDelta[i2, i1])/vev) - (Conjugate[dZfR1[4, i1, i2]]*IndexDelta[c1, c2]*Mf[4, i1])/(2*vev) + (dvev1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1])/vev^2 - (dZG0G01*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1])/(2*vev) - (dZfL1[4, i2, i1]*IndexDelta[c1, c2]*Mf[4, i2])/(2*vev) + (dZHAG01*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Tan[beta])/(2*vev)},
	{(IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1])/vev, (dMf[4, i1]*IndexDelta[c1, c2]*IndexDelta[i2, i1])/vev + (Conjugate[dZfL1[4, i1, i2]]*IndexDelta[c1, c2]*Mf[4, i1])/(2*vev) - (dvev1*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1])/vev^2 + (dZG0G01*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1])/(2*vev) + (dZfR1[4, i2, i1]*IndexDelta[c1, c2]*Mf[4, i2])/(2*vev) - (dZHAG01*IndexDelta[c1, c2]*IndexDelta[i2, i1]*Mf[4, i1]*Tan[beta])/(2*vev)}}
}];
