ptf $
*==============================================================================
* Texture2Par Main Input File
*==============================================================================
 IWFM                                      / Model Type
 welllog.dat                               / Well Log File
 nodezones.dat                             / Geologic Zones File
*------------------------------------------------------------------------------
* Model Settings
*------------------------------------------------------------------------------
 C2VSimFG.in                               / Simulation File
 ..\Preprocessor\C2VSimFG_Preprocessor.in  / PreprocSim File
 Groundwater\C2VSimFG_Groundwater1974_Orig.dat  / GW Template File
 zones.txt                                 / Node Pilot Point Zones File
*------------------------------------------------------------------------------
* Program Settings (True/False)
*------------------------------------------------------------------------------
True                                       / Output Node Files
*------------------------------------------------------------------------------
* Variogram Settings
* itype: 0-linear variogram, 1-spherical variogram, 2-exponential variogram
*------------------------------------------------------------------------------
 1                                         / Variogram Type (itype)
 1.0                                       / Sill
 1.0E5                                     / Range
 1.0E5                                     / Minimum Range
 0.0                                       / Anisotropy Angle
 0.0                                       / Nugget
 16                                        / Wells used in kriging
*------------------------------------------------------------------------------
* Aquifer Parameter Settings
*------------------------------------------------------------------------------
 $  KCk    $                               / KCk
 $  KFk    $                               / KFk
 0.93                                      / KHp
-0.62                                      / KVp
 1.0                                       / Syp
*------------------------------------------------------------------------------
* Pilot Points - X, Y, KCMin, DeltaKC, KFMin, DeltaKF, SsC, SsF, SyC, SyF, AnisoC, AnisoF, Zone
*------------------------------------------------------------------------------
1935403   14328727   $   KCMin01   $   $   DeltaKC01   $   $   KFMin01   $   $   DeltaKF01   $   $   SsC01   $   $   SsF01   $   $   SyC01   $   $   SyF01   $   $   anisoC01  $   $   anisoF01  $  1
1942119   14232482   $   KCMin02   $   $   DeltaKC02   $   $   KFMin02   $   $   DeltaKF02   $   $   SsC02   $   $   SsF02   $   $   SyC02   $   $   SyF02   $   $   anisoC02  $   $   anisoF02  $  1
1994976   14148377   $   KCMin03   $   $   DeltaKC03   $   $   KFMin03   $   $   DeltaKF03   $   $   SsC03   $   $   SsF03   $   $   SyC03   $   $   SyF03   $   $   anisoC03  $   $   anisoF03  $  1
2042971   14022622   $   KCMin04   $   $   DeltaKC04   $   $   KFMin04   $   $   DeltaKF04   $   $   SsC04   $   $   SsF04   $   $   SyC04   $   $   SyF04   $   $   anisoC04  $   $   anisoF04  $  1
2077205   13890260   $   KCMin05   $   $   DeltaKC05   $   $   KFMin05   $   $   DeltaKF05   $   $   SsC05   $   $   SsF05   $   $   SyC05   $   $   SyF05   $   $   anisoC05  $   $   anisoF05  $  1
1913334   13829031   $   KCMin06   $   $   DeltaKC06   $   $   KFMin06   $   $   DeltaKF06   $   $   SsC06   $   $   SsF06   $   $   SyC06   $   $   SyF06   $   $   anisoC06  $   $   anisoF06  $  1
1843389   14593533   $   KCMin07   $   $   DeltaKC07   $   $   KFMin07   $   $   DeltaKF07   $   $   SsC07   $   $   SsF07   $   $   SyC07   $   $   SyF07   $   $   anisoC07  $   $   anisoF07  $  1
1863614   14492088   $   KCMin08   $   $   DeltaKC08   $   $   KFMin08   $   $   DeltaKF08   $   $   SsC08   $   $   SsF08   $   $   SyC08   $   $   SyF08   $   $   anisoC08  $   $   anisoF08  $  1
1912677   14401034   $   KCMin09   $   $   DeltaKC09   $   $   KFMin09   $   $   DeltaKF09   $   $   SsC09   $   $   SsF09   $   $   SyC09   $   $   SyF09   $   $   anisoC09  $   $   anisoF09  $  1
2025249   14325101   $   KCMin10   $   $   DeltaKC10   $   $   KFMin10   $   $   DeltaKF10   $   $   SsC10   $   $   SsF10   $   $   SyC10   $   $   SyF10   $   $   anisoC10  $   $   anisoF10  $  1
2079693   14241380   $   KCMin11   $   $   DeltaKC11   $   $   KFMin11   $   $   DeltaKF11   $   $   SsC11   $   $   SsF11   $   $   SyC11   $   $   SyF11   $   $   anisoC11  $   $   anisoF11  $  1
2120342   14147253   $   KCMin12   $   $   DeltaKC12   $   $   KFMin12   $   $   DeltaKF12   $   $   SsC12   $   $   SsF12   $   $   SyC12   $   $   SyF12   $   $   anisoC12  $   $   anisoF12  $  1
2153399   14043435   $   KCMin13   $   $   DeltaKC13   $   $   KFMin13   $   $   DeltaKF13   $   $   SsC13   $   $   SsF13   $   $   SyC13   $   $   SyF13   $   $   anisoC13  $   $   anisoF13  $  1
2211514   13870122   $   KCMin14   $   $   DeltaKC14   $   $   KFMin14   $   $   DeltaKF14   $   $   SsC14   $   $   SsF14   $   $   SyC14   $   $   SyF14   $   $   anisoC14  $   $   anisoF14  $  1
1897751   14677041   $   KCMin15   $   $   DeltaKC15   $   $   KFMin15   $   $   DeltaKF15   $   $   SsC15   $   $   SsF15   $   $   SyC15   $   $   SyF15   $   $   anisoC15  $   $   anisoF15  $  1
1915468   14545545   $   KCMin16   $   $   DeltaKC16   $   $   KFMin16   $   $   DeltaKF16   $   $   SsC16   $   $   SsF16   $   $   SyC16   $   $   SyF16   $   $   anisoC16  $   $   anisoF16  $  1
1981631   14443352   $   KCMin17   $   $   DeltaKC17   $   $   KFMin17   $   $   DeltaKF17   $   $   SsC17   $   $   SsF17   $   $   SyC17   $   $   SyF17   $   $   anisoC17  $   $   anisoF17  $  1
1817413   14743104   $   KCMin18   $   $   DeltaKC18   $   $   KFMin18   $   $   DeltaKF18   $   $   SsC18   $   $   SsF18   $   $   SyC18   $   $   SyF18   $   $   anisoC18  $   $   anisoF18  $  1
1780964   14664178   $   KCMin19   $   $   DeltaKC19   $   $   KFMin19   $   $   DeltaKF19   $   $   SsC19   $   $   SsF19   $   $   SyC19   $   $   SyF19   $   $   anisoC19  $   $   anisoF19  $  1
1789382   14537428   $   KCMin20   $   $   DeltaKC20   $   $   KFMin20   $   $   DeltaKF20   $   $   SsC20   $   $   SsF20   $   $   SyC20   $   $   SyF20   $   $   anisoC20  $   $   anisoF20  $  1
1818421   14422245   $   KCMin21   $   $   DeltaKC21   $   $   KFMin21   $   $   DeltaKF21   $   $   SsC21   $   $   SsF21   $   $   SyC21   $   $   SyF21   $   $   anisoC21  $   $   anisoF21  $  1
1847051   14327674   $   KCMin22   $   $   DeltaKC22   $   $   KFMin22   $   $   DeltaKF22   $   $   SsC22   $   $   SsF22   $   $   SyC22   $   $   SyF22   $   $   anisoC22  $   $   anisoF22  $  1
1851607   14204294   $   KCMin23   $   $   DeltaKC23   $   $   KFMin23   $   $   DeltaKF23   $   $   SsC23   $   $   SsF23   $   $   SyC23   $   $   SyF23   $   $   anisoC23  $   $   anisoF23  $  1
1912664   14053510   $   KCMin24   $   $   DeltaKC24   $   $   KFMin24   $   $   DeltaKF24   $   $   SsC24   $   $   SsF24   $   $   SyC24   $   $   SyF24   $   $   anisoC24  $   $   anisoF24  $  1
1935784   13931198   $   KCMin25   $   $   DeltaKC25   $   $   KFMin25   $   $   DeltaKF25   $   $   SsC25   $   $   SsF25   $   $   SyC25   $   $   SyF25   $   $   anisoC25  $   $   anisoF25  $  1
2090659   13709402   $   KCMin26   $   $   DeltaKC26   $   $   KFMin26   $   $   DeltaKF26   $   $   SsC26   $   $   SsF26   $   $   SyC26   $   $   SyF26   $   $   anisoC26  $   $   anisoF26  $  1
2027114   13785390   $   KCMin27   $   $   DeltaKC27   $   $   KFMin27   $   $   DeltaKF27   $   $   SsC27   $   $   SsF27   $   $   SyC27   $   $   SyF27   $   $   anisoC27  $   $   anisoF27  $  1
2181640   13955018   $   KCMin28   $   $   DeltaKC28   $   $   KFMin28   $   $   DeltaKF28   $   $   SsC28   $   $   SsF28   $   $   SyC28   $   $   SyF28   $   $   anisoC28  $   $   anisoF28  $  1
2308730   13718489   $   KCMin29   $   $   DeltaKC29   $   $   KFMin29   $   $   DeltaKF29   $   $   SsC29   $   $   SsF29   $   $   SyC29   $   $   SyF29   $   $   anisoC29  $   $   anisoF29  $  1
2241655   13799685   $   KCMin30   $   $   DeltaKC30   $   $   KFMin30   $   $   DeltaKF30   $   $   SsC30   $   $   SsF30   $   $   SyC30   $   $   SyF30   $   $   anisoC30  $   $   anisoF30  $  1
2361684   13658474   $   KCMin31   $   $   DeltaKC31   $   $   KFMin31   $   $   DeltaKF31   $   $   SsC31   $   $   SsF31   $   $   SyC31   $   $   SyF31   $   $   anisoC31  $   $   anisoF31  $  1
2404048   13587868   $   KCMin32   $   $   DeltaKC32   $   $   KFMin32   $   $   DeltaKF32   $   $   SsC32   $   $   SsF32   $   $   SyC32   $   $   SyF32   $   $   anisoC32  $   $   anisoF32  $  1
2453472   13527853   $   KCMin33   $   $   DeltaKC33   $   $   KFMin33   $   $   DeltaKF33   $   $   SsC33   $   $   SsF33   $   $   SyC33   $   $   SyF33   $   $   anisoC33  $   $   anisoF33  $  1
2130767   13792625   $   KCMin34   $   $   DeltaKC34   $   $   KFMin34   $   $   DeltaKF34   $   $   SsC34   $   $   SsF34   $   $   SyC34   $   $   SyF34   $   $   anisoC34  $   $   anisoF34  $  1
2151949   13683186   $   KCMin35   $   $   DeltaKC35   $   $   KFMin35   $   $   DeltaKF35   $   $   SsC35   $   $   SsF35   $   $   SyC35   $   $   SyF35   $   $   anisoC35  $   $   anisoF35  $  1
2311215   13394280   $   KCMin36   $   $   DeltaKC36   $   $   KFMin36   $   $   DeltaKF36   $   $   SsC36   $   $   SsF36   $   $   SyC36   $   $   SyF36   $   $   anisoC36  $   $   anisoF36  $  1
2234071   13485141   $   KCMin37   $   $   DeltaKC37   $   $   KFMin37   $   $   DeltaKF37   $   $   SsC37   $   $   SsF37   $   $   SyC37   $   $   SyF37   $   $   anisoC37  $   $   anisoF37  $  1
2191534   13596662   $   KCMin38   $   $   DeltaKC38   $   $   KFMin38   $   $   DeltaKF38   $   $   SsC38   $   $   SsF38   $   $   SyC38   $   $   SyF38   $   $   anisoC38  $   $   anisoF38  $  1
2229615   13580808   $   KCMin39   $   $   DeltaKC39   $   $   KFMin39   $   $   DeltaKF39   $   $   SsC39   $   $   SsF39   $   $   SyC39   $   $   SyF39   $   $   anisoC39  $   $   anisoF39  $  1
2289630   13517263   $   KCMin40   $   $   DeltaKC40   $   $   KFMin40   $   $   DeltaKF40   $   $   SsC40   $   $   SsF40   $   $   SyC40   $   $   SyF40   $   $   anisoC40  $   $   anisoF40  $  1
2356705   13478429   $   KCMin41   $   $   DeltaKC41   $   $   KFMin41   $   $   DeltaKF41   $   $   SsC41   $   $   SsF41   $   $   SyC41   $   $   SyF41   $   $   anisoC41  $   $   anisoF41  $  1
2381417   13414884   $   KCMin42   $   $   DeltaKC42   $   $   KFMin42   $   $   DeltaKF42   $   $   SsC42   $   $   SsF42   $   $   SyC42   $   $   SyF42   $   $   anisoC42  $   $   anisoF42  $  1
2461165   13129509   $   KCMin43   $   $   DeltaKC43   $   $   KFMin43   $   $   DeltaKF43   $   $   SsC43   $   $   SsF43   $   $   SyC43   $   $   SyF43   $   $   anisoC43  $   $   anisoF43  $  1
2432171   13225753   $   KCMin44   $   $   DeltaKC44   $   $   KFMin44   $   $   DeltaKF44   $   $   SsC44   $   $   SsF44   $   $   SyC44   $   $   SyF44   $   $   anisoC44  $   $   anisoF44  $  1
2370652   13299714   $   KCMin45   $   $   DeltaKC45   $   $   KFMin45   $   $   DeltaKF45   $   $   SsC45   $   $   SsF45   $   $   SyC45   $   $   SyF45   $   $   anisoC45  $   $   anisoF45  $  1
2444962   13326627   $   KCMin46   $   $   DeltaKC46   $   $   KFMin46   $   $   DeltaKF46   $   $   SsC46   $   $   SsF46   $   $   SyC46   $   $   SyF46   $   $   anisoC46  $   $   anisoF46  $  1
2536750   13245431   $   KCMin47   $   $   DeltaKC47   $   $   KFMin47   $   $   DeltaKF47   $   $   SsC47   $   $   SsF47   $   $   SyC47   $   $   SyF47   $   $   anisoC47  $   $   anisoF47  $  1
2568522   13160704   $   KCMin48   $   $   DeltaKC48   $   $   KFMin48   $   $   DeltaKF48   $   $   SsC48   $   $   SsF48   $   $   SyC48   $   $   SyF48   $   $   anisoC48  $   $   anisoF48  $  1
2632067   13093628   $   KCMin49   $   $   DeltaKC49   $   $   KFMin49   $   $   DeltaKF49   $   $   SsC49   $   $   SsF49   $   $   SyC49   $   $   SyF49   $   $   anisoC49  $   $   anisoF49  $  1
2624342   13394876   $   KCMin50   $   $   DeltaKC50   $   $   KFMin50   $   $   DeltaKF50   $   $   SsC50   $   $   SsF50   $   $   SyC50   $   $   SyF50   $   $   anisoC50  $   $   anisoF50  $  1
2700592   13326627   $   KCMin51   $   $   DeltaKC51   $   $   KFMin51   $   $   DeltaKF51   $   $   SsC51   $   $   SsF51   $   $   SyC51   $   $   SyF51   $   $   anisoC51  $   $   anisoF51  $  1
2774728   13217188   $   KCMin52   $   $   DeltaKC52   $   $   KFMin52   $   $   DeltaKF52   $   $   SsC52   $   $   SsF52   $   $   SyC52   $   $   SyF52   $   $   anisoC52  $   $   anisoF52  $  1
2799440   13135992   $   KCMin53   $   $   DeltaKC53   $   $   KFMin53   $   $   DeltaKF53   $   $   SsC53   $   $   SsF53   $   $   SyC53   $   $   SyF53   $   $   anisoC53  $   $   anisoF53  $  1
2830112   13007224   $   KCMin54   $   $   DeltaKC54   $   $   KFMin54   $   $   DeltaKF54   $   $   SsC54   $   $   SsF54   $   $   SyC54   $   $   SyF54   $   $   anisoC54  $   $   anisoF54  $  1
2830112   12918389   $   KCMin55   $   $   DeltaKC55   $   $   KFMin55   $   $   DeltaKF55   $   $   SsC55   $   $   SsF55   $   $   SyC55   $   $   SyF55   $   $   anisoC55  $   $   anisoF55  $  1
2893483   12833836   $   KCMin56   $   $   DeltaKC56   $   $   KFMin56   $   $   DeltaKF56   $   $   SsC56   $   $   SsF56   $   $   SyC56   $   $   SyF56   $   $   anisoC56  $   $   anisoF56  $  1
2865938   12728854   $   KCMin57   $   $   DeltaKC57   $   $   KFMin57   $   $   DeltaKF57   $   $   SsC57   $   $   SsF57   $   $   SyC57   $   $   SyF57   $   $   anisoC57  $   $   anisoF57  $  1
2706029   12784293   $   KCMin58   $   $   DeltaKC58   $   $   KFMin58   $   $   DeltaKF58   $   $   SsC58   $   $   SsF58   $   $   SyC58   $   $   SyF58   $   $   anisoC58  $   $   anisoF58  $  1
2578556   12918266   $   KCMin59   $   $   DeltaKC59   $   $   KFMin59   $   $   DeltaKF59   $   $   SsC59   $   $   SsF59   $   $   SyC59   $   $   SyF59   $   $   anisoC59  $   $   anisoF59  $  1
2526424   13040564   $   KCMin60   $   $   DeltaKC60   $   $   KFMin60   $   $   DeltaKF60   $   $   SsC60   $   $   SsF60   $   $   SyC60   $   $   SyF60   $   $   anisoC60  $   $   anisoF60  $  1
2621477   13008902   $   KCMin61   $   $   DeltaKC61   $   $   KFMin61   $   $   DeltaKF61   $   $   SsC61   $   $   SsF61   $   $   SyC61   $   $   SyF61   $   $   anisoC61  $   $   anisoF61  $  1
2812112   12793554   $   KCMin62   $   $   DeltaKC62   $   $   KFMin62   $   $   DeltaKF62   $   $   SsC62   $   $   SsF62   $   $   SyC62   $   $   SyF62   $   $   anisoC62  $   $   anisoF62  $  1
2727385   12839448   $   KCMin63   $   $   DeltaKC63   $   $   KFMin63   $   $   DeltaKF63   $   $   SsC63   $   $   SsF63   $   $   SyC63   $   $   SyF63   $   $   anisoC63  $   $   anisoF63  $  1
2658486   12898487   $   KCMin64   $   $   DeltaKC64   $   $   KFMin64   $   $   DeltaKF64   $   $   SsC64   $   $   SsF64   $   $   SyC64   $   $   SyF64   $   $   anisoC64  $   $   anisoF64  $  1
2509956   13467839   $   KCMin65   $   $   DeltaKC65   $   $   KFMin65   $   $   DeltaKF65   $   $   SsC65   $   $   SsF65   $   $   SyC65   $   $   SyF65   $   $   anisoC65  $   $   anisoF65  $  1
*------------------------------------------------------------------------------
* Aquitard Pilot Points - X, Y, KCMin, DeltaKC, KFMin, DeltaKF, AnisoC, AnisoF, Zone
*------------------------------------------------------------------------------
2195014   13653100   $   KCMin66   $   $   DeltaKC66   $   $   KFMin66   $   $   DeltaKF66   $   $   anisoC66   $   $   anisoF66   $  1
2400038   13529132   $   KCMin67   $   $   DeltaKC67   $   $   KFMin67   $   $   DeltaKF67   $   $   anisoC67   $   $   anisoF67   $  1
2440566   13250203   $   KCMin68   $   $   DeltaKC68   $   $   KFMin68   $   $   DeltaKF68   $   $   anisoC68   $   $   anisoF68   $  1
2717111   13145307   $   KCMin69   $   $   DeltaKC69   $   $   KFMin69   $   $   DeltaKF69   $   $   anisoC69   $   $   anisoF69   $  1
2664663   12883067   $   KCMin70   $   $   DeltaKC70   $   $   KFMin70   $   $   DeltaKF70   $   $   anisoC70   $   $   anisoF70   $  1
2852999   12797243   $   KCMin71   $   $   DeltaKC71   $   $   KFMin71   $   $   DeltaKF71   $   $   anisoC71   $   $   anisoF71   $  1
*------------------------------------------------------------------------------
* EOF
*------------------------------------------------------------------------------
