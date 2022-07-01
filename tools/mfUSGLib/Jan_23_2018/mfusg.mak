######################################################################
# platform  unix
# compiler: intel compiler

defines=-DPATH3D
incdir=
f90=ifort
#DEBUG
#fflags= /c /fpp /names:lowercase /Qvec-report1 /MTd /check:all /traceback
#RELEASE
fflags= /c /fpp /names:lowercase /Qvec-report1 /MT

static_lib=mfusg.lib

######################################################################

cleanall :
                del *.obj
                del *.mod
                del $(static_lib)

objects =       \
                gmodules.obj \
                lak_gag_sfr_modules.obj \
                pcgu7.obj \
                gwf2dpf1u1.obj \
                glo2p3du1.obj \
                sparse.obj \
                xmdlib_2.obj \
                xmd.obj \
                tvmu1.obj \
                tvmu2.obj \
                glo2btnu1.obj \
                gwf2evt8u1.obj \
                gwf2riv7u1.obj \
                gwf2hfb7u1.obj \
                gwf2chd7u1.obj \
                gwf2ghb7u1.obj \
                gwf2rch8u1.obj \
                gwf2drn7u1.obj \
                gwf2drt8u.obj \
                gwf2qrt8u.obj \
                gwf2mnw17.obj \
                gwf2mnw27.obj \
                gwf2sub7u1.obj \
                gwf2wel7u1.obj \
                gwf2fhb7u1.obj \
                gwf2str7u1.obj \
                gwf2bcf-lpf-u1.obj \
                glo2sgbu1.obj \
                disu2gncn1.obj \
                cln2basu1.obj \
                cln2props1.obj \
                parutl7.obj \
                utl7u1.obj \
                gwf2basu1.obj \
                gwf2sfr7u1.obj \
                gwf2lak7u1.obj \
                glo2basu1.obj \
                gwf2gag7u1.obj \
                gwf2ets8u1.obj \
                gwt2bndsu1.obj \
                gwt2dptu1.obj \
                density.obj \
                mfusg.obj \
                glo2sms-u1.obj

gmodules.obj:                   gmodules.f
                                $(f90) $(fflags) $(defines) gmodules.f

lak_gag_sfr_modules.obj:        lak_gag_sfr_modules.f
                                $(f90) $(fflags) $(defines) lak_gag_sfr_modules.f

xmdlib_2.obj:                   xmdlib_2.f
                                $(f90) $(fflags) $(defines) xmdlib_2.f

xmd.obj:                        xmd.f
                                $(f90) $(fflags) $(defines) xmd.f

sparse.obj:                     sparse.f
                                $(f90) $(fflags) $(defines) sparse.f

density.obj:                    density.f
                                $(f90) $(fflags) $(defines) density.f

tvmu2.obj:                      tvmu2.f
                                $(f90) $(fflags) $(defines) tvmu2.f

tvmu1.obj:                      tvmu1.f
                                $(f90) $(fflags) $(defines) tvmu1.f

gwt2dptu1.obj:                  gwt2dptu1.f
                                $(f90) $(fflags) $(defines) gwt2dptu1.f

gwt2bndsu1.obj:                 gwt2bndsu1.f
                                $(f90) $(fflags) $(defines) gwt2bndsu1.f

glo2btnu1.obj:                  glo2btnu1.f
                                $(f90) $(fflags) $(defines) glo2btnu1.f

gwf2evt8u1.obj:                 gwf2evt8u1.f
                                $(f90) $(fflags) $(defines) gwf2evt8u1.f

gwf2riv7u1.obj:                 gwf2riv7u1.f
                                $(f90) $(fflags) $(defines) gwf2riv7u1.f

gwf2hfb7u1.obj:                 gwf2hfb7u1.f
                                $(f90) $(fflags) $(defines) gwf2hfb7u1.f

gwf2chd7u1.obj:                 gwf2chd7u1.f
                                $(f90) $(fflags) $(defines) gwf2chd7u1.f

gwf2ghb7u1.obj:                 gwf2ghb7u1.f
                                $(f90) $(fflags) $(defines) gwf2ghb7u1.f

gwf2dpf1u1.obj:                 gwf2dpf1u1.f
                                $(f90) $(fflags) $(defines) gwf2dpf1u1.f

gwf2rch8u1.obj:                 gwf2rch8u1.f
                                $(f90) $(fflags) $(defines) gwf2rch8u1.f

gwf2drn7u1.obj:                 gwf2drn7u1.f
                                $(f90) $(fflags) $(defines) gwf2drn7u1.f

gwf2drt8u.obj:                  gwf2drt8u.f
                                $(f90) $(fflags) $(defines) gwf2drt8u.f

gwf2qrt8u.obj:                  gwf2qrt8u.f
                                $(f90) $(fflags) $(defines) gwf2qrt8u.f

gwf2mnw27.obj:                  gwf2mnw27.f
                                $(f90) $(fflags) $(defines) gwf2mnw27.f

gwf2mnw17.obj:                  gwf2mnw17.f
                                $(f90) $(fflags) $(defines) gwf2mnw17.f

gwf2sub7u1.obj:                 gwf2sub7u1.f
                                $(f90) $(fflags) $(defines) gwf2sub7u1.f

gwf2wel7u1.obj:                 gwf2wel7u1.f
                                $(f90) $(fflags) $(defines) gwf2wel7u1.f

gwf2str7u1.obj:                 gwf2str7u1.f
                                $(f90) $(fflags) $(defines) gwf2str7u1.f

gwf2fhb7u1.obj:                 gwf2fhb7u1.f
                                $(f90) $(fflags) $(defines) gwf2fhb7u1.f

gwf2bcf-lpf-u1.obj:             gwf2bcf-lpf-u1.f
                                $(f90) $(fflags) $(defines) gwf2bcf-lpf-u1.f

glo2sgbu1.obj:                  glo2sgbu1.f
                                $(f90) $(fflags) $(defines) glo2sgbu1.f

glo2p3du1.obj:                  glo2p3du1.f
                                $(f90) $(fflags) $(defines) glo2p3du1.f

glo2sms-u1.obj:                 glo2sms-u1.f
                                $(f90) $(fflags) $(defines) glo2sms-u1.f

disu2gncn1.obj:                 disu2gncn1.f
                                $(f90) $(fflags) $(defines) disu2gncn1.f

pcgu7.obj:                      pcgu7.f
                                $(f90) $(fflags) $(defines) pcgu7.f

parutl7.obj:                    parutl7.f
                                $(f90) $(fflags) $(defines) parutl7.f

utl7u1.obj:                     utl7u1.f global.mod
                                $(f90) $(fflags) $(defines) utl7u1.f

gwf2basu1.obj:                  gwf2basu1.f
                                $(f90) $(fflags) $(defines) gwf2basu1.f

gwf2sfr7u1.obj:                 gwf2sfr7u1.f
                                $(f90) $(fflags) $(defines) gwf2sfr7u1.f

gwf2lak7u1.obj:                 gwf2lak7u1.f
                                $(f90) $(fflags) $(defines) gwf2lak7u1.f

glo2basu1.obj:                  glo2basu1.f
                                $(f90) $(fflags) $(defines) glo2basu1.f

gwf2gag7u1.obj:                 gwf2gag7u1.f
                                $(f90) $(fflags) $(defines) gwf2gag7u1.f

cln2basu1.obj:                  cln2basu1.f
                                $(f90) $(fflags) $(defines) cln2basu1.f

cln2props1.obj:                 cln2props1.f
                                $(f90) $(fflags) $(defines) cln2props1.f

gwf2ets8u1.obj:                 gwf2ets8u1.f
                                $(f90) $(fflags) $(defines) gwf2ets8u1.f

mfusg.obj:                      mfusg.f
                                $(f90) $(fflags) $(defines) mfusg.f

##################################################################################

$(static_lib):  $(objects)
                lib /nologo /out:$(static_lib) $(objects)
