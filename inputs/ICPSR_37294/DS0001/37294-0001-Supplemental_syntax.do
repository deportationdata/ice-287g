/*-------------------------------------------------------------------------*
 |                                                                         
 |            STATA SUPPLEMENTAL SYNTAX FILE FOR ICPSR 37294
 |       CENSUS OF STATE AND FEDERAL ADULT CORRECTIONAL FACILITIES,
 |                                  2012
 |
 |
 | This Stata missing value recode program is provided for optional use with
 | the Stata system version of this data file as distributed by ICPSR.
 | The program replaces user-defined numeric missing values (e.g., -9)
 | with generic system missing "."  Note that Stata allows you to specify
 | up to 27 unique missing value codes.  Only variables with user-defined
 | missing values are included in this program.
 |
 | To apply the missing value recodes, users need to first open the
 | Stata data file on their system, apply the missing value recodes if
 | desired, then save a new copy of the data file with the missing values
 | applied.  Users are strongly advised to use a different filename when
 | saving the new file.
 |
 *------------------------------------------------------------------------*/

replace V17 = . if (V17 == -8)
replace V19 = . if (V19 == -8)
replace V21 = . if (V21 == -9)
replace V23 = . if (V23 == -9)
replace V25 = . if (V25 == -9)
replace V26 = . if (V26 == -9)
replace V27 = . if (V27 == -9)
replace V28 = . if (V28 == -9)
replace V29 = . if (V29 == -9)


