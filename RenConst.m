(* ::Package:: *)

(*Generate the definition of Renormalization constants*)

(*SM part:*)
dCW1/: RenConst[dCW1]:=CW/2(dMWsq1/MW2-dMZsq1/MZ2);
dSW1/: RenConst[dSW1]:=CW^2/SW/2(dMZsq1/MZ2-dMWsq1/MW2);
dvev1/: RenConst[dvev1]:=vev(dMWsq1/MW2/2+dSW1/SW-dZe1);

(*Scalar Part:*)

(*TadPole*)
TadPoleRen[Lag_List,Field_List,TadPoleCT_List]:=Block[{LinearTerm,$NQ,i,TadPoleSolution,tmp},
LinearTerm=Plus@@Coefficient[Lag/.{QuantumField[args___]:>$NQ QuantumField[args]},$NQ]//Simplify;
Print[LinearTerm];
TadPoleSolution={};
For[i=1,i<=Length[Field],i++,
	tmp=Solve[LinearTerm+TP$Sigma[Field[[i]]]QuantumField[Field[[i]]]==0,TadPoleCT[[i]]]/.{QuantumField[Field[[i]]]:>1}//Simplify;
	tmp=tmp[[1]]/.{_QuantumField->0}//Simplify;
	TadPoleSolution={TadPoleSolution,tmp};
];
Flatten[TadPoleSolution]
]

TadPoleSolution=TadPoleRen[LSPRe[[2]],{Subscript[HL,R],Subscript[HH,R]},{dTHL1,dTHH1}];
dTHL1/:RenConst[dTHL1]:=(dTHL1/.TadPoleSolution);
dTHH1/:RenConst[dTHH1]:=(dTHH1/.TadPoleSolution);

(*Field Renormalization and Mass Renormalization*)

