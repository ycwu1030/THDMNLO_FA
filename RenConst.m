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
(*Print[LinearTerm];*)
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
ScalarPairRen[Vertex_List,Field_List,CT_List]:=Block[{FieldNoR,nField,PP,Msq,RenSolution,k},
FieldNoR=Cases[Field,Subscript[args_,R]:>args];
nField=Length[Field];
(*Get the inverse Propagator matrix*)
PP[p2_]:=Table[
Cases[Vertex,
FRVertex[{{QuantumField[Field[[i]]],QuantumField[Field[[j]]]}|{QuantumField[Field[[j]]],QuantumField[Field[[i]]]},lo_,nlo_}]
:>((lo+nlo+I PP$Sigma[Field[[i]],Field[[j]]][p2])/I/.{Pair[_Momentum,_Momentum]:>p2}//Simplify)
][[1]],
{i,1,nField},{j,1,nField}];
Print[PP[p2]];
(*Get the mass involved*)
Msq=Range[nField];
Print[Msq];
For[k=1,k<=nField,k++,
Msq[[k]]=-(PP[0][[k,k]])/.((#->0)&/@CT)/.{PP$Sigma[arg1_,arg2_][arg3_]:>0,dTHL1->0,dTHH1->0};
];
Print[Msq];
(*Get the solution for the renormalization constant*)
RenSolution=Solve[{
PP[Msq[[1]]][[1,1]]==0,
(*PP[Msq[[2]]][[2,2]]\[Equal]0,*)
PP'[Msq[[1]]][[1,1]]==1,
PP'[Msq[[2]]][[2,2]]==1,
PP[Msq[[1]]][[1,2]]==0,
PP[Msq[[2]]][[2,1]]==0
},CT][[1]];
Print[{
PP[Msq[[1]]][[1,1]]==0,
PP[Msq[[2]]][[2,2]]==0,
PP'[Msq[[1]]][[1,1]]==1,
PP'[Msq[[2]]][[2,2]]==1,
PP[Msq[[1]]][[1,2]]==0,
PP[Msq[[2]]][[2,1]]==0
}];
RenSolution
]
