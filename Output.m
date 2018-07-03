(* ::Package:: *)

(*Functions that can be used to output the results to FeynArts model file format*)


(*The SM & 2HDM Field Code*)
FieldCode={QuantumField[Subscript[f_,__],args___]:>QuantumField[f,args],
QuantumField[HL]->S[1],
QuantumField[HH]->S[2],
QuantumField[HA]->S[3],
QuantumField[G0]->S[4],
QuantumField[Hm]->S[5],
QuantumField[Hp]->-S[5],
QuantumField[Gm]->S[6],
QuantumField[Gp]->-S[6],

QuantumField[gamma,args___]:>V[1],
QuantumField[Z,args___]:>V[2],
QuantumField[Wm,args___]:>V[3],
QuantumField[Wp,args___]:>-V[3],


QuantumField[HCbar[f_], args___]:>-QuantumField[f, args],
QuantumField[FNu,{},{i_}]:>F[1,{i}],
QuantumField[Fe,{},{i_}]:>F[2,{i}],
QuantumField[FUp,{},{i_,c_}]:>F[3,{i,c}],
QuantumField[FDown,{},{i_,c_}]:>F[4,{i,c}]
};


(*Field Renormalization Constant*)
FieldRenormalizationConstantReplace={dZ[f_]:>ToExpression["dZ"<>ToString[f]<>"1"],

};
