c CLASS = C
c  
c  
c  This file is generated automatically by the setparams utility.
c  It sets the number of processors and the class of the NPB
c  in this directory. Do not modify it by hand.
c  
        integer            na, nonzer, niter
        double precision   shift, rcond
        parameter(  na=150000,
     >              nonzer=15,
     >              niter=75,
     >              shift=110.,
     >              rcond=1.0d-1 )
        logical  convertdouble
        parameter (convertdouble = .false.)
        character compiletime*11
        parameter (compiletime='16 Dec 2016')
        character npbversion*5
        parameter (npbversion='3.3.1')
        character cs1*5
        parameter (cs1='ifort')
        character cs2*6
        parameter (cs2='$(F77)')
        character cs3*41
        parameter (cs3='-L/home/galliher/numactl/build/lib -lnuma')
        character cs4*6
        parameter (cs4='(none)')
        character cs5*46
        parameter (cs5='-heap-arrays -g -fpe0 -warn -traceback -deb...')
        character cs6*46
        parameter (cs6='-heap-arrays -g -fpe0 -warn -traceback -deb...')
        character cs7*6
        parameter (cs7='randi8')
