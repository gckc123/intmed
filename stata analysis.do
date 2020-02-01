cd "C:\Users\Gary\Documents\My programs\intmed\"
use "sim_data.dta", clear

paramed y, avar(x) mvar(m) a0(0) a1(1) m(1) yreg(linear) mreg(linear) bootstrap reps(10000)
paramed y, avar(x) mvar(m) a0(0) a1(1) m(1) yreg(linear) mreg(linear) nointer reps(10000)
