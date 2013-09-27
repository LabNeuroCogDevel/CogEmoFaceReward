 #!/usr/bin/env bash
 #####
 #
 # Something strange is happening with matlab text output. 
 #
 # A variable number (3-4) of extra columns are being added before the final two columns
 # The for loop below rewrites lines that have too many columns to have only 
 #  the first 11 columns (0 to 10) and the last two columns ($#F-1 and $#F)
 # columns between 10 and $#F-1 are ignored/removed
 #
 # the columns that are removed are all one of three -- for 15 subjects (2 already fixed)
 #  for i in *txt; do perl -slane 'if($#F>12){print "@F[11..($#F-2)]"}' $i; done|sort|uniq -c                                                            
 #    2688 102 101 97 114
 #    2688 104 97 112 112 121
 #    2688 115 99 114 97 109
 #
 ####
 for f in subjects/*txt; do
   perl -i -laspe '$_=join("\t",@F[0..10,($#F-1)..$#F]) if($#F>12)' $f
 done
