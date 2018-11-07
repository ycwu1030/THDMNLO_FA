(*
  THDM.mod
    Classes model file for the Two-Higgs-Doublet Model
    by Abdesslam Arhrib, Oliver Brein, and Thomas Hahn
    last modified 13 Aug 09 by Thomas Hahn

    Further Modified by Yongcheng Wu

This file contains the definition of the two-Higgs-doublet model
for FeynArts.  It needs the Generic model file Lorentz.gen.

When you change things, remember:

-- All particles are arranged in classes.  For single particle
   model definitions each particle lives in its own class.

-- For each class the common SelfConjugate behaviour and the
   IndexRange MUST be present in the definitions.

-- IMPORTANT: The coupling matrices MUST be declared in the
   SAME order as the Generic coupling.

This file introduces the following symbols:

  coupling constants and masses:
  ------------------------------
  EL:   electron charge (Thomson limit)
  CW, SW:   cosine and sine of weak mixing angle

  MW, MZ:   W, and Z masses
  Mh0, MHH, MA0, MHp: the Higgs masses

  MLE:    lepton class mass
  ME, MM, ML: lepton masses (e, mu, tau)

  MQU:    u-type quark class mass
  MU, MC, MT: u-type quark masses (up, charm, top)

  MQD:    d-type quark class mass
  MD, MS, MB: d-type quark masses (down, strange, bottom)

  CKM:    quark mixing matrix
      (set CKM = IndexDelta for no quark-mixing)

  CA, SA:   {Cos, Sin}[alpha]
  CB, SB, TB: {Cos, Sin, Tan}[beta]
  C2A, S2A: {Cos, Sin}[2 alpha]
  CAB, SAB: {Cos, Sin}[alpha + beta]
  CBA, SBA: {Cos, Sin}[beta - alpha]
      where alpha is the (h0, H0) mixing angle
      and tan[beta] is the ratio of the VEVs of
      the two Higgs doublets
*)


(* $HKSign is the sign in the SU(2) covariant derivative,
   i.e. D_\mu = \partial_\mu + $HKSign I g A^a_\mu \tau^a,
   so 1 = Haber-Kane, -1 = Denner conventions *)
(* We have changed the file to Denner conventions, the same as the SM.mod *)
(* Modified by Yongcheng Wu @Apr 2016  *)
(* If[ !ValueQ[$HKSign], $HKSign = 1 ] *)

vev = 2 MW SW/EL;
v = 2 MW SW/EL;

CalcProcessSelf[loop_, proc_, opt_] :=
 
 Block[ {amp, Neglect, FormSub = RCSub},
    ClearProcess[];
    amp = InsertFieldsHook[
        CreateTopologies[loop, Length[Flatten[{#}]] & /@ proc,
           ExcludeTopologies -> Internal],
        proc ];
    OptPaint[amp];
    amp = CreateFeynAmpHook[amp];
    Plus @@ CalcFeynAmp[amp] //.
         Abbr[] //. Subexpr[] //. 
   Abbr[] 
  ]

IndexRange[ Index[Generation] ] = Range[3];
IndexRange[ Index[Colour] ] = NoUnfold[Range[3]];

IndexStyle[ Index[Generation, i_Integer] ] := Alph[i + 8] 

(* Copy From SM.mod by Yongcheng Wu @Apr 2016 *)

ViolatesQ[ q__ ] := Plus[q] =!= 0


mdZfLR1[ type_, j1_, j2_ ] :=
  Mass[F[type, {j1}]]/2 dZfL1[type, j1, j2] +
    Mass[F[type, {j2}]]/2 Conjugate[dZfR1[type, j2, j1]]

mdZfRL1[ type_, j1_, j2_ ] :=
  Mass[F[type, {j1}]]/2 dZfR1[type, j1, j2] +
    Mass[F[type, {j2}]]/2 Conjugate[dZfL1[type, j2, j1]]


(* the leptonic field RCs are diagonal: *)

dZfL1[ type:1 | 2, j1_, j2_ ] :=
  IndexDelta[j1, j2] dZfL1[type, j1, j1] /; j1 =!= j2

dZfR1[ type:1 | 2, j1_, j2_ ] :=
  IndexDelta[j1, j2] dZfR1[type, j1, j1] /; j1 =!= j2


(* some short-hands for fermionic couplings: *)

FermionCharge[1] = 0;
FermionCharge[2] = -1;
FermionCharge[3] = 2/3;
FermionCharge[4] = -1/3

gR[ type_ ] :=
  -SW/CW FermionCharge[type];
gL[ type_ ] :=
  (If[ OddQ[type], 1/2, -1/2 ] - SW^2 FermionCharge[type])/(SW CW);
dgR[ type_ ] :=
  gR[type] (dZe1 + 1/(CW^2 SW) dSW1);
dgL[ type_ ] :=
  If[ OddQ[type], 1/2, -1/2 ]/(SW CW) *
    (dZe1 + (SW^2 - CW^2)/(CW^2 SW) dSW1) + dgR[type]

(* End of Copy*)


(* For the Particle Classes, Adding QuantumNumbers *)
(* in order to account for Soft-Photon radiation correstions *)
(* By Yongcheng Wu @Apr 2016 *)
M$ClassesDescription = {

  (* Neutrinos: I_3 = +1/2, Q = 0 *)
  F[1] == {
  SelfConjugate -> False,
  Indices -> {Index[Generation]},
  Mass -> 0,
  QuantumNumbers -> {0 Charge, LeptonNumber},
  PropagatorLabel -> ComposedChar["\\nu", Index[Generation]],
  PropagatorType -> Straight,
  PropagatorArrow -> Forward },

  (* massive Leptons: I_3 = -1/2, Q = -1 *)
  F[2] == {
  SelfConjugate -> False,
  Indices -> {Index[Generation]},
  Mass -> MLE,
  QuantumNumbers -> {-1 Charge, LeptonNumber},
  PropagatorLabel -> ComposedChar["e", Index[Generation]],
  PropagatorType -> Straight,
  PropagatorArrow -> Forward },

  (* Quarks (u): I_3 = +1/2, Q = +2/3 *)
  F[3] == {
  SelfConjugate -> False,
  Indices -> {Index[Generation], Index[Colour]},
  Mass -> MQU,
  QuantumNumbers -> {2/3 Charge, Sqrt[4/3] ColorCharge},
  PropagatorLabel -> ComposedChar["u", Index[Generation]],
  PropagatorType -> Straight,
  PropagatorArrow -> Forward },

  (* Quarks (d): I_3 = -1/2, Q = -1/3 *)
  F[4] == {
  SelfConjugate -> False,
  Indices -> {Index[Generation], Index[Colour]},
  Mass -> MQD,
  QuantumNumbers -> {-1/3 Charge, Sqrt[4/3] ColorCharge},
  PropagatorLabel -> ComposedChar["d", Index[Generation]],
  PropagatorType -> Straight, 
  PropagatorArrow -> Forward },

  (* Gauge bosons: Q = 0 *)
  V[1] == {
  SelfConjugate -> True,
  Indices -> {},
  Mass -> 0,
  PropagatorLabel -> "\\gamma",
  PropagatorType -> Sine,
  PropagatorArrow -> None },

  V[2] == {
  SelfConjugate -> True, 
  Indices -> {},
  Mass -> MZ,
  PropagatorLabel -> "Z",
  PropagatorType -> Sine,
  PropagatorArrow -> None },

  (* Gauge bosons: Q = -1 *)
  V[3] == {
  SelfConjugate -> False,
  Indices -> {},
  Mass -> MW,
  QuantumNumbers -> -Charge,
  PropagatorLabel -> "W",
  PropagatorType -> Sine,
  PropagatorArrow -> Forward },

  (* CP-even Higgs doublet: Q = 0 *)
  S[1] == {
  SelfConjugate -> True,
  Indices -> {},
  Mass -> Mh0,
  PropagatorLabel -> ComposedChar["h", Null, "0"],
  PropagatorType -> ScalarDash,
  PropagatorArrow -> None },

  S[2] == {
  SelfConjugate -> True,
  Indices -> {},
  Mass -> MHH,
  PropagatorLabel -> ComposedChar["H", Null, "0"],
  PropagatorType -> ScalarDash,
  PropagatorArrow -> None },

  (* CP-odd Higgs doublet: Q = 0 *)
  S[3] == {
  SelfConjugate -> True,
  Indices -> {},
  Mass -> MA0,
  PropagatorLabel -> ComposedChar["A", Null, "0"],
  PropagatorType -> ScalarDash,
  PropagatorArrow -> None },

  S[4] == {
  SelfConjugate -> True,
  Indices -> {},
  Mass -> MZ,
  PropagatorLabel -> ComposedChar["G", Null, "0"],
  PropagatorType -> ScalarDash,
  PropagatorArrow -> None },

  (* charged Higgs doublet: Q = -1 *)
  S[5] == {
  SelfConjugate -> False,
  Indices -> {},
  Mass -> MHp,
  QuantumNumbers -> -Charge,
  PropagatorLabel -> "H",
  PropagatorType -> ScalarDash,
  PropagatorArrow -> Forward },

  S[6] == {
  SelfConjugate -> False,
  Indices -> {},
  Mass -> MW,
  QuantumNumbers -> -Charge,
  PropagatorLabel -> "G",
  PropagatorType -> ScalarDash,
  PropagatorArrow -> Forward },

  (* Ghosts: Q = 0 *)
  U[1] == {
  SelfConjugate -> False,
  Indices -> {},
  Mass -> 0,
  QuantumNumbers -> GhostNumber,
  PropagatorLabel -> ComposedChar["u", "\\gamma"],
  PropagatorType -> GhostDash,
  PropagatorArrow -> Forward },

  U[2] == {
  SelfConjugate -> False,
  Indices -> {},
  Mass -> MZ,
  QuantumNumbers -> GhostNumber,
  PropagatorLabel -> ComposedChar["u", "Z"],
  PropagatorType -> GhostDash,
  PropagatorArrow -> Forward },

  (* Ghosts: Q = -1 *)
  U[3] == {
  SelfConjugate -> False,
  Indices -> {},
  Mass -> MW,
  QuantumNumbers -> {-1 Charge, GhostNumber},
  PropagatorLabel -> ComposedChar["u", "-"],
  PropagatorType -> GhostDash,
  PropagatorArrow -> Forward },

  (* Ghosts: Q = 1 *)
  U[4] == {
  SelfConjugate -> False,
  Indices -> {},
  Mass -> MW,
  QuantumNumbers -> {1 Charge, GhostNumber},
  PropagatorLabel -> ComposedChar["u", "+"],
  PropagatorType -> GhostDash,
  PropagatorArrow -> Forward }
}

MLE[1] = ME;
MLE[2] = MM;
MLE[3] = ML;
MQU[1] = MU;
MQU[2] = MC;
MQU[3] = MT;
MQD[1] = MD;
MQD[2] = MS;
MQD[3] = MB;
MQU[gen_, _] = MQU[gen];
MQD[gen_, _] = MQD[gen]

TheLabel[ F[1, {1}] ] = ComposedChar["\\nu", "e"]; 
TheLabel[ F[1, {2}] ] = ComposedChar["\\nu", "\\mu"]; 
TheLabel[ F[1, {3}] ] = ComposedChar["\\nu", "\\tau"]; 
TheLabel[ F[2, {1}] ] = "e"; 
TheLabel[ F[2, {2}] ] = "\\mu"; 
TheLabel[ F[2, {3}] ] = "\\tau";
TheLabel[ F[3, {1, ___}] ] = "u"; 
TheLabel[ F[3, {2, ___}] ] = "c";
TheLabel[ F[3, {3, ___}] ] = "t";
TheLabel[ F[4, {1, ___}] ] = "d"; 
TheLabel[ F[4, {2, ___}] ] = "s";
TheLabel[ F[4, {3, ___}] ] = "b"

(* Copy from SM.mod, the GaugeXi *)

GaugeXi[ V[1] ] = GaugeXi[A];
GaugeXi[ V[2] ] = GaugeXi[Z];
GaugeXi[ V[3] ] = GaugeXi[W];
GaugeXi[ S[1] ] = 1;
GaugeXi[ S[2] ] = GaugeXi[Z];
GaugeXi[ S[3] ] = GaugeXi[W];
GaugeXi[ U[1] ] = GaugeXi[A];
GaugeXi[ U[2] ] = GaugeXi[Z];
GaugeXi[ U[3] ] = GaugeXi[W];
GaugeXi[ U[4] ] = GaugeXi[W]

(* End of Copy *)

M$LastModelRules = {}


(* some short-hands for excluding classes of particles *)

NoGeneration1 = ExcludeParticles -> F[_, {1, ___}]

NoGeneration2 = ExcludeParticles -> F[_, {2, ___}]

NoGeneration3 = ExcludeParticles -> F[_, {3, ___}]

NoElectronHCoupling =
  ExcludeFieldPoints -> {
    FieldPoint[0][-F[2, {1}], F[2, {1}], S],
    FieldPoint[0][-F[2, {1}], F[1, {1}], S] }

NoLightFHCoupling =
  ExcludeFieldPoints -> {
    FieldPoint[_][-F[2], F[2], S],
    FieldPoint[_][-F[2], F[1], S],
    FieldPoint[_][-F[3, {1, ___}], F[3, {1, ___}], S],
    FieldPoint[_][-F[3, {2, ___}], F[3, {2, ___}], S],
    FieldPoint[_][-F[4], F[4], S],
    FieldPoint[_][-F[4], F[3, {1, ___}], S],
    FieldPoint[_][-F[4], F[3, {2, ___}], S] }

(* Re-Sorted by Yongcheng Wu According to the Type of the Vertex @Apr 2016 *)
(* Counter Terms for SM part Copy from SM.mod *)
(* Counter Terms for THDM part are added partially according to hep-ph/9701257 *)
(* and partially according to self-calculation by Yongcheng Wu and Han Yuan @Apr 2016 *)
(* Note that We have always chosen the Minus Sign for SU(2) Convariant Derivative which is the same as SM.mod *)

M$CouplingMatrices = {

(* V-V:  G(+) . { -g[mu, nu] mom^2, g[mu, nu], -mom[mu] mom[nu] } *)

  C[ -V[3], V[3] ] == I *
    { {0, dZW1, dZW2},
      {0, MW^2 dZW1 + dMWsq1, MW^2 dZW2 + dMWsq2 + dMWsq1 dZW1},
      {0, -dZW1, -dZW2} },

  C[ V[2], V[2] ] == I *
    { {0, dZZZ1, dZZZ2 + 1/4 dZAZ1^2},
      {0, MZ^2 dZZZ1 + dMZsq1, MZ^2 dZZZ2 + dMZsq2 + dMZsq1 dZZZ1},
      {0, -dZZZ1, -dZZZ2 - 1/4 dZAZ1^2} },

  C[ V[1], V[1] ] == I *
    { {0, dZAA1, dZAA2 + 1/4 dZZA1^2},
      {0, 0, 1/4 MZ^2 dZZA1^2},
      {0, -dZAA1, -dZAA2 - 1/4 dZZA1^2} },

  C[ V[1], V[2] ] == I *
    { {0, dZAZ1/2 + dZZA1/2,
        (dZAZ2 + dZZA2 + 1/2 dZZA1 dZZZ1 + 1/2 dZAZ1 dZAA1)/2},
      {0, MZ^2 dZZA1/2,
          (MZ^2 dZZA2 + 1/2 MZ^2 dZZZ1 dZZA1 + dMZsq1 dZZA1)/2},
      {0, -dZAZ1/2 - dZZA1/2,
          -(dZAZ2 + dZZA2 + 1/2 dZZA1 dZZZ1 + 1/2 dZAZ1 dZAA1)/2} },


  (* U-U:  G(+) . { -mom^2, 1 } *)

  C[ U[1], -U[1] ] == -I *
    { {0, -dZAA1/2 + dUAA1},
      {0, 0} },
  
  C[ U[2], -U[2] ] == -I *
    { {0, -dZZZ1/2 + dUZZ1},
      {0, GaugeXi[Z] (MZ^2 (-dZG01/2 + dUZZ1) + dMZsq1/2) } },
  
  C[ U[2], -U[1] ] == -I *
    { {0, -dZAZ1/2 + dUAZ1},
      {0, 0} },
  
  C[ U[1], -U[2] ] == -I *
    { {0, -dZZA1/2 + dUZA1},
      {0, GaugeXi[Z] MZ^2 dUZA1} },
  
  C[ U[3], -U[3] ] == -I *
    { {0, -dZW1/2 + dUW1},
      {0, GaugeXi[W] (MW^2 (-dZGp1/2 + dUW1) + dMWsq1/2) } },

  C[ U[4], -U[4] ] == -I *
    { {0, -dZW1/2 + dUW1},
      {0, GaugeXi[W] (MW^2 (-dZGp1/2 + dUW1) + dMWsq1/2) } },

  (* F-F:  G(+) . { slash[mom1] omega[-], slash[mom2] omega[+],
                    omega[-], omega[+] } *)

  C[ -F[1, {j1}], F[1, {j2}] ] == I *
    { {0, -AddHC[dZfL1[1, j1, j2]]},
      {0, AddHC[dZfR1[1, j1, j2]]},
      {0, 0},
      {0, 0} },

  C[ -F[2, {j1}], F[2, {j2}] ] == I *
    { {0, -AddHC[dZfL1[2, j1, j2]]},
      {0, AddHC[dZfR1[2, j1, j2]]},
      {0, -mdZfLR1[2, j1, j2] - IndexDelta[j1, j2] dMf1[2, j1]},
      {0, -mdZfRL1[2, j1, j2] - IndexDelta[j1, j2] dMf1[2, j1]} },

  C[ -F[3, {j1, o1}], F[3, {j2, o2}] ] == I IndexDelta[o1, o2] *
    { {0, -AddHC[dZfL1[3, j1, j2]]},
      {0, AddHC[dZfR1[3, j1, j2]]},
      {0, -mdZfLR1[3, j1, j2] - IndexDelta[j1, j2] dMf1[3, j1]},
      {0, -mdZfRL1[3, j1, j2] - IndexDelta[j1, j2] dMf1[3, j1]} },

  C[ -F[4, {j1, o1}], F[4, {j2, o2}] ] == I IndexDelta[o1, o2] *
    { {0, -AddHC[dZfL1[4, j1, j2]]},
      {0, AddHC[dZfR1[4, j1, j2]]},
      {0, -mdZfLR1[4, j1, j2] - IndexDelta[j1, j2] dMf1[4, j1]},
      {0, -mdZfRL1[4, j1, j2] - IndexDelta[j1, j2] dMf1[4, j1]} },


  (* V-V-V-V:  G(+) . { g[mu1, mu2] g[mu3, mu4],
                        g[mu1, mu4] g[mu2, mu3],
                        g[mu1, mu3] g[mu2, mu4] } *)

  C[ -V[3], -V[3], V[3], V[3] ] == I EL^2/SW^2 *
    { {2, 4 dZe1 - 4 dSW1/SW + 4 dZW1}, 
      {-1, -2 dZe1 + 2 dSW1/SW - 2 dZW1},
      {-1, -2 dZe1 + 2 dSW1/SW - 2*dZW1} },

  C[ -V[3], V[3], V[2], V[2] ] == -I EL^2 CW^2/SW^2 *
    { {2, 4 dZe1 - 4 dSW1/(SW CW^2) + 2 dZW1 + 2 dZZZ1 - 2 dZAZ1 SW/CW}, 
      {-1, -2 dZe1 + 2 dSW1/(SW CW^2) - dZW1 - dZZZ1 + dZAZ1 SW/CW},
      {-1, -2 dZe1 + 2 dSW1/(SW CW^2) - dZW1 - dZZZ1 + dZAZ1 SW/CW} },

  C[ -V[3], V[3], V[1], V[2] ] == I EL^2 CW/SW *
    { {2, 4 dZe1 - 2 dSW1/(SW CW^2) + 2 dZW1 +
            dZZZ1 + dZAA1 - SW/CW dZAZ1 - CW/SW dZZA1},
      {-1, -2 dZe1 + dSW1/(SW CW^2) - dZW1 -
            dZZZ1/2 - dZAA1/2 + SW/CW dZAZ1/2 + CW/SW dZZA1/2},
      {-1, -2 dZe1 + dSW1/(SW CW^2) - dZW1 -
            dZZZ1/2 - dZAA1/2 + SW/CW dZAZ1/2 + CW/SW dZZA1/2} },

  C[ -V[3], V[3], V[1], V[1] ] == -I EL^2 *
    { {2, 4 dZe1 + 2 dZW1 + 2 dZAA1 - 2 CW/SW dZZA1}, 
      {-1, -2 dZe1 - dZW1 - dZAA1 + CW/SW dZZA1},
      {-1, -2 dZe1 - dZW1 - dZAA1 + CW/SW dZZA1} },

  (* V-V-V:  G(-) . (g[mu1, mu2] (p2 - p1)_mu3 +
                     g[mu2, mu3] (p3 - p2)_mu1 +
                     g[mu3, mu1] (p1 - p3)_mu2) *)

  C[ V[1], -V[3], V[3] ] == -I EL *
    { {1, dZe1 + dZW1 + dZAA1/2 - CW/SW dZZA1/2} },

  C[ V[2], -V[3], V[3] ] == I EL CW/SW *
    { {1, dZe1 - dSW1/(SW CW^2) + dZW1 + dZZZ1/2 - SW/CW dZAZ1/2} },


  (* F-F-V:  G(-) . { gamma[mu3] omega[-], gamma[mu3] omega[+] } *)

  C[ -F[1, {j1}], F[1, {j2}], V[1] ] == I EL *
    { {0, gL[1] IndexDelta[j1, j2] dZZA1/2},
      {0, 0} },

  C[ -F[2, {j1}], F[2, {j2}], V[1] ] == I EL *
    { {-FermionCharge[2] IndexDelta[j1, j2],
        -FermionCharge[2] *
          (IndexDelta[j1, j2] (dZe1 + dZAA1/2) + AddHC[dZfL1[2, j1, j2]]) +
          gL[2] IndexDelta[j1, j2] dZZA1/2},
      {-FermionCharge[2] IndexDelta[j1, j2],
        -FermionCharge[2] *
          (IndexDelta[j1, j2] (dZe1 + dZAA1/2) + AddHC[dZfR1[2, j1, j2]]) +
          gR[2] IndexDelta[j1, j2] dZZA1/2} },

  C[ -F[3, {j1, o1}], F[3, {j2, o2}], V[1] ] == I EL IndexDelta[o1, o2] *
    { {-FermionCharge[3] IndexDelta[j1, j2],
        -FermionCharge[3] *
          (IndexDelta[j1, j2] (dZe1 + dZAA1/2) + AddHC[dZfL1[3, j1, j2]]) +
          gL[3] IndexDelta[j1, j2] dZZA1/2},
      {-FermionCharge[3] IndexDelta[j1, j2],
        -FermionCharge[3] *
          (IndexDelta[j1, j2] (dZe1 + dZAA1/2) + AddHC[dZfR1[3, j1, j2]]) +
          gR[3] IndexDelta[j1, j2] dZZA1/2} },

  C[ -F[4, {j1, o1}], F[4, {j2, o2}], V[1] ] == I EL IndexDelta[o1, o2] *
    { {-FermionCharge[4] IndexDelta[j1, j2],
        -FermionCharge[4] *
          (IndexDelta[j1, j2] (dZe1 + dZAA1/2) + AddHC[dZfL1[4, j1, j2]]) +
          gL[4] IndexDelta[j1, j2] dZZA1/2},
      {-FermionCharge[4] IndexDelta[j1, j2],
        -FermionCharge[4] *
          (IndexDelta[j1, j2] (dZe1 + dZAA1/2) + AddHC[dZfR1[4, j1, j2]]) +
          gR[4] IndexDelta[j1, j2] dZZA1/2} },

  C[ -F[1, {j1}], F[1, {j2}], V[2] ] == I EL *
    { {gL[1] IndexDelta[j1, j2],
        IndexDelta[j1, j2] (gL[1] dZZZ1/2 + dgL[1]) +
        gL[1] AddHC[dZfL1[1, j1, j2]]},
      {0, 0} },

  C[ -F[2, {j1}], F[2, {j2}], V[2] ] == I EL *
    { {gL[2] IndexDelta[j1, j2],
        IndexDelta[j1, j2] *
          (gL[2] dZZZ1/2 + dgL[2] - FermionCharge[2] dZAZ1/2) +
          gL[2] AddHC[dZfL1[2, j1, j2]]},
      {gR[2] IndexDelta[j1, j2],
        IndexDelta[j1, j2] *
          (gR[2] dZZZ1/2 + dgR[2] - FermionCharge[2] dZAZ1/2) +
          gR[2] AddHC[dZfR1[2, j1, j2]]} },

  C[ -F[3, {j1, o1}], F[3, {j2, o2}], V[2] ] == I EL IndexDelta[o1, o2] *
    { {gL[3] IndexDelta[j1, j2],
        IndexDelta[j1, j2] *
          (gL[3] dZZZ1/2 + dgL[3] - FermionCharge[3] dZAZ1/2) +
          gL[3] AddHC[dZfL1[3, j1, j2]]},
      {gR[3] IndexDelta[j1, j2],
        IndexDelta[j1, j2] *
          (gR[3] dZZZ1/2 + dgR[3] - FermionCharge[3] dZAZ1/2) +
          gR[3] AddHC[dZfR1[3, j1, j2]]} },

  C[ -F[4, {j1, o1}], F[4, {j2, o2}], V[2] ] == I EL IndexDelta[o1, o2] *
    { {gL[4] IndexDelta[j1, j2],
        IndexDelta[j1, j2] *
          (gL[4] dZZZ1/2 + dgL[4] - FermionCharge[4] dZAZ1/2) +
          gL[4] AddHC[dZfL1[4, j1, j2]]},
      {gR[4] IndexDelta[j1, j2],
        IndexDelta[j1, j2] *
          (gR[4] dZZZ1/2 + dgR[4] - FermionCharge[4] dZAZ1/2) +
          gR[4] AddHC[dZfR1[4, j1, j2]]} },

  C[ -F[1, {j1}], F[2, {j2}], -V[3] ] ==
    I EL/(Sqrt[2] SW) IndexDelta[j1, j2] *
    { {1, dZe1 - dSW1/SW + dZW1/2 +
            Conjugate[dZfL1[1, j1, j1]]/2 + dZfL1[2, j1, j1]/2,
          dZe2 - dSW2/SW +
            1/2 (dZW2 + Conjugate[dZfL2[1, j1, j1]] + dZfL2[2, j1, j1]) +
            (dSW1/SW)^2 - dSW1/SW dZe1 -
            1/8 (dZW1^2 + Conjugate[dZfL1[1, j1, j1]]^2 +
              dZfL1[2, j1, j1]^2) +
            (dZe1 - dSW1/SW) *
              1/2 (dZW1 + Conjugate[dZfL1[1, j1, j1]] + dZfL1[2, j1, j1]) +
            1/4 (dZW1 dZfL1[2, j1, j1] + dZW1 Conjugate[dZfL1[1, j1, j1]] +
                   Conjugate[dZfL1[1, j1, j1]] dZfL1[2, j1, j1]) },
      {0, 0, 0} },

  C[ -F[2, {j1}], F[1, {j2}], V[3] ] ==
    I EL/(Sqrt[2] SW) IndexDelta[j1, j2] *
    { {1, dZe1 - dSW1/SW + dZW1/2 +
            dZfL1[1, j1, j1]/2 + Conjugate[dZfL1[2, j1, j1]]/2,
          dZe2 - dSW2/SW +
            1/2 (dZW2 + dZfL2[1, j1, j1] + Conjugate[dZfL2[2, j1, j1]]) +
            (dSW1/SW)^2 - dSW1/SW dZe1 -
            1/8 (dZW1^2 + dZfL1[1, j1, j1]^2 +
              Conjugate[dZfL1[2, j1, j1]]^2) +
            (dZe1 - dSW1/SW) *
              1/2 (dZW1 + dZfL1[1, j1, j1] + Conjugate[dZfL1[2, j1, j1]]) +
      1/4 (dZW1 Conjugate[dZfL1[2, j1, j1]] + dZW1 dZfL1[1, j1, j1] +
              dZfL1[1, j1, j1] Conjugate[dZfL1[2, j1, j1]]) },
      {0, 0, 0} },

  C[ -F[3, {j1, o1}], F[4, {j2, o2}], -V[3] ] ==
    I EL/(Sqrt[2] SW) IndexDelta[o1, o2] *
    { {CKM[j1, j2],
        CKM[j1, j2] (dZe1 - dSW1/SW + dZW1/2) + dCKM1[j1, j2] +
        1/2 IndexSum[
          Conjugate[dZfL1[3, gn, j1]] CKM[gn, j2] +
          CKM[j1, gn] dZfL1[4, gn, j2],
        {gn, MaxGenerationIndex}]},
      {0, 0} },

  C[ -F[4, {j2, o2}], F[3, {j1, o1}], V[3] ] ==
    I EL/(Sqrt[2] SW) IndexDelta[o1, o2] *
    { {Conjugate[CKM[j1, j2]],
        Conjugate[CKM[j1, j2]] (dZe1 - dSW1/SW + dZW1/2) +
          Conjugate[dCKM1[j1, j2]] +
          1/2 IndexSum[
            Conjugate[dZfL1[4, gn, j2]] Conjugate[CKM[j1, gn]] +
            Conjugate[CKM[gn, j2]] dZfL1[3, gn, j1],
          {gn, MaxGenerationIndex}]},
      {0, 0} },


  (* U-U-V:  G(+) . { p1_mu3, p2_mu3 } *)

  C[ -U[3], U[3], V[1] ] == -I EL *
    { {1, dZe1 + dZAA1/2 - dZW1/2 + dUW1 - CW/SW dZZA1/2},
      {0, 0} },

  C[ -U[4], U[4], V[1] ] == I EL *
    { {1, dZe1 + dZAA1/2 - dZW1/2 + dUW1 - CW/SW dZZA1/2},
      {0, 0} },

  C[ -U[3], U[3], V[2] ] == I EL CW/SW *
    { {1, dZe1 - 1/(CW^2 SW) dSW1 + dZZZ1/2 - dZW1/2 + dUW1 - SW/CW dZAZ1/2},
      {0, 0} },

  C[ -U[4], U[4], V[2] ] == -I EL CW/SW *
    { {1, dZe1 - 1/(CW^2 SW) dSW1 + dZZZ1/2 - dZW1/2 + dUW1 - SW/CW dZAZ1/2},
      {0, 0} },

  C[ -U[3], U[2], V[3] ] == -I EL CW/SW *
    { {1, dZe1 - 1/(CW^2 SW) dSW1 + dUZZ1 - SW/CW dUAZ1},
      {0, 0} },

  C[ -U[2], U[3], -V[3] ] == -I EL *
    { {CW/SW,
       CW/SW (dZe1 - 1/(CW^2 SW) dSW1 + dZW1/2 - dZZZ1/2 + dUW1) + dZZA1/2},
      {0, 0} }, 

  C[ -U[4], U[2], -V[3] ] == I EL CW/SW *
    { {1, dZe1 - 1/(CW^2 SW) dSW1 + dUZZ1 - SW/CW dUAZ1},
      {0, 0} },

  C[ -U[2], U[4], V[3] ] == I EL *
    { {CW/SW,
       CW/SW (dZe1 - 1/(CW^2 SW) dSW1 + dZW1/2 - dZZZ1/2 + dUW1) + dZZA1/2},
      {0, 0} },

  C[ -U[3], U[1], V[3] ] == I EL *
    { {1, dZe1 + dUAA1 - CW/SW dUZA1},
      {0, 0} },

  C[ -U[1], U[3], -V[3] ] == I EL *
    { {1, dZe1 + dZW1/2 - dZAA1/2 + dUW1 + CW/SW dZAZ1/2},
      {0, 0} },

  C[ -U[4], U[1], -V[3] ] == -I EL *
    { {1, dZe1 + dUAA1 - CW/SW dUZA1},
      {0, 0} },

  C[ -U[1], U[4], V[3] ] == -I EL *
    { {1, dZe1 + dZW1/2 - dZAA1/2 + dUW1 + CW/SW dZAZ1/2},
      {0, 0} }
 
 }