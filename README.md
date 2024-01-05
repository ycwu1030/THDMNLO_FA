# THDMNLO_FA
This is the project containing all the components that we used to generate 2HDM EW-NLO counter-terms and corresponding Feynman rules in the format of FeynArts model files. 

The original files are too messy and un-readable. So here, I try to reorganize them in a form that is easy for reading and for further usage in other models if possible. 
## To Do List
1. Parallel Calculation in Mathematica

[comment]: # (1. Encapsulation)

[comment]: # (1. As a plug-in for FR?)

## Convention
The sign convention is

- $\eta_G$ = 1
- $\eta_e$ = 1
- $\eta$ = -1
- $\eta_Z$ = 1
- $\eta_\theta$ = -1
- $\eta'$ = 1
- $\eta_Y$ = 1

The definition of above symbol is according to [arXiv:1209.6213](https://arxiv.org/abs/1209.6213)

## Renormalization Scheme:
Counter terms and Renormalization constants are calculated and defined according to KOSY scheme (Phys.Rev.D 70(2004)115002)

## Usage
- Copy all the .mod files in ModelFiles and Template to some specific place where FeynArts can read.
- Do whatever you want in FeynArts.

If you want to regenerate all the THDM part model files: (in this way you may modify some part of the source code to accommodate your conventions or even change to your own models)

- Before running the notebook, please make two new directories:
    1. LagrangianData
    1. VertexRawData
- Running the notebook.
    - It will generate relevant model files in the ModelFiles directory
- Use these model file together with those in Template directory in FeynArts

## Contributors
- @HanYuanyuaner [Han Yuan](https://github.com/HanYuanyuaner)
- @ycwu1030 [Yongcheng Wu](https://github.com/ycwu1030)

## Citation
If you use this work, please cite: [arXiv:1808.02037](https://arxiv.org/abs/1808.02037)
