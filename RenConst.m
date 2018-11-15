(* ::Package:: *)

(*Generate the definition of Renormalization constants*)
RenConstList={};
(*SM part:*)
AppendTo[RenConstList,dCW1->CW/2(dMWsq1/MW2-dMZsq1/MZ2)];
AppendTo[RenConstList,dSW1->CW^2/SW/2(dMZsq1/MZ2-dMWsq1/MW2)];
AppendTo[RenConstList,dvev1->vev(dMWsq1/MW2/2+dSW1/SW-dZe1)];

(*Scalar Part:*)

(*TadPole*)
TadPoleRen[Lag_List,Field_List,TadPoleCT_List]:=Block[{LinearTerm,$NQ,i,TadPoleSolution,tmp},
LinearTerm=Plus@@Coefficient[Lag/.{QuantumField[args___]:>$NQ QuantumField[args]},$NQ]//Simplify;
(*Print[LinearTerm];*)
TadPoleSolution={};
For[i=1,i<=Length[Field],i++,
	(*tmp=Solve[LinearTerm+TP$Sigma[Field[[i]]]QuantumField[Field[[i]]]==0,TadPoleCT[[i]]]/.{QuantumField[Field[[i]]]:>1}//Simplify;*)
	tmp=Solve[Coefficient[LinearTerm,QuantumField[Field[[i]]]]+TP$Sigma[QuantumField[Field[[i]]]]==0,TadPoleCT[[i]]];
	tmp=tmp[[1]](*/.{_QuantumField->0}*)//Simplify;
	TadPoleSolution={TadPoleSolution,tmp};
];
Flatten[TadPoleSolution]/.{}
]

TadPoleSolution=TadPoleRen[LSPRe[[2]],{Subscript[HL,R],Subscript[HH,R]},{dTHL1,dTHH1}];
(*dTHL1/:RenConst[dTHL1]:=(dTHL1/.TadPoleSolution);
dTHH1/:RenConst[dTHH1]:=(dTHH1/.TadPoleSolution);*)
RenConstList=Join[RenConstList,TadPoleSolution];

(*Field Renormalization and Mass Renormalization*)
ScalarPairRen[Vertex_List,FieldIn_List,CT_List]:=Block[{Field,nField,PP,Msq,EquationsForFieldNormalization,EquationsForMassNormalization,RenSolution,k},
nField=Length[FieldIn];
(*Handle the input Field to a good format*)
Field=If[Head[FieldIn[[1]]]=!=List,{{FieldIn[[1]],FieldIn[[1]]},{FieldIn[[2]],FieldIn[[2]]}},FieldIn];
Print[Field];
(*Get the inverse Propagator matrix*)
PP[p2_]:=Table[
Cases[Vertex,
FRVertex[{f:{QuantumField[Field[[i,1]]],QuantumField[Field[[j,2]]]}|{QuantumField[Field[[j,2]]],QuantumField[Field[[i,1]]]},lo_,nlo_}]
:>((lo+nlo+I (PP$Sigma@@f)[p2])/I/.{Pair[_Momentum,_Momentum]:>p2}//Simplify)
][[1]],
{i,1,nField},{j,1,nField}];
Print[PP[p2]];
(*Get the mass involved*)
Msq=Range[nField];
For[k=1,k<=nField,k++,
Msq[[k]]=-(PP[0][[k,k]])/.((#->0)&/@CT)/.{PP$Sigma[arg1_,arg2_][arg3_]:>0,dTHL1->0,dTHH1->0};
];
Print[Msq];
(*Get the solution for the renormalization constant*)
EquationsForFieldNormalization={
PP'[Msq[[1]]][[1,1]]==1,(*For first field renormalization*)
PP'[Msq[[2]]][[2,2]]==1, (* For second field renormalization *)
PP[Msq[[1]]][[1,2]]==0, (* For first-second field mixing at first field mass *)
PP[Msq[[2]]][[2,1]]==0 (* For first-second field mixing at second field mass *)
};
(*We only consider physical scalar mass normalization*)
(*For the goldstone, we didn't consider gauge fixing term renormalization, so for now, just ignore it, and put its mass at zero*)
EquationsForMassNormalization={};
For[k=1,k<=nField,k++,
If[Msq[[k]]=!=0,EquationsForMassNormalization={EquationsForMassNormalization,PP[Msq[[k]]][[k,k]]==0}];
];
RenSolution=Solve[Join[EquationsForFieldNormalization,Flatten[EquationsForMassNormalization]],CT][[1]];
RenSolution
]
