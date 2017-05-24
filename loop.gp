if (count < $0) \
   i = count;\
   plot [t=lx:ux] [ly:uy] 'config.out' using 2:(0) every :::i::i,\
   'config.out' using 2:(totPotential($$2)) every :::i::i,\
   totPotential(t);\
   count = count + 1;\
   reread
