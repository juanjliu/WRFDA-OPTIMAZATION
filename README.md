Description
===========

This package is used to improve the performance of the adjoint model in the Weather Research and Forecasting plus (WRFPLUS).  There are five files as following:

```
dyn_em
frame
main
phys
share
```
 
Usage
=====

Step 1: Download the underlying version 

Users can download the WRFPLUS source code from http://www2.mmm.ucar.edu/wrf/users/wrfda/download/wrfplus.html , then Unzip and untar the WRFPLUS file.

Step 2: Download the optimazatiom code 

User can download the optimaztion code from this web : 
 ```
https://github.com/juanjliu/WRFDA-OPTIMAZATION.git
 ```
Step 3: Unzip and untar the WRFDA-OPTIMAZATION file 
 
 ```
  cd WRFPLUSV3   (from Step 1)
  Updating optimation codes in the five files of the underlying version 
 ``` 
Step 4: Installing WRFPLUS for 4D-Var Run

  ```
   cd WRFPLUSV3
   ./configure wrfplus
   ./compile em_real >& compile.out
  ``` 
  More detail Installing information, User can see users_guide_Chap6 from 
  ```
  http://www2.mmm.ucar.edu/wrf/users/wrfda/Docs/user_guide_V3.7/users_guide_chap6.htm#_Installing_WRFDA_for 
