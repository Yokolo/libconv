SUBROUTINE d_s1s0_symicomb8_p_10_ld_1u1_v1_t_f_t(n, ndat1, nx, ny, x, &
&y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -7
  integer(kind=4), parameter :: upfil = 8
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat1
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:nx - (1), 0:1, 0:ndat1 - (1)) &
&:: x
  real(kind=8), intent(out), dimension(0:1, 0:ny - (1), 0:ndat1 - (1))&
& :: y
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_lre_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_lro_fil = resh&
&ape((/ &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp, &
0.0_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_hre_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_hro_fil = resh&
&ape((/ &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp, &
0.0_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
  integer(kind=4), dimension(-7 - (8):8 - (-7) - (1)) :: mod_arr
  do l = -7 - (8), 8 - (-7) - (1), 1
    mod_arr(l) = modulo(l, n)
  end do
!$omp parallel  default(shared) private(i1, i2, l, et0, ot0, filt&
!$omp&er_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i2 = 0, ndat1 - (1), 1
    do i1 = 0,  -(-7) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = -0.24646355857433108358764E-7_wp
      filter_val1 = 0.1593875388117163824756E-8_wp
      filter_val2 = 0.44109215856612868615909E-7_wp
      filter_val3 = -0.2852535033969307505679E-8_wp
      et0 = et0 + (x(mod_arr(-7 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-7 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-7 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-7 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.40033729064957944938397E-6_wp
      filter_val1 = 0.156915542257710856911416E-6_wp
      filter_val2 = 0.652118308986595532963584E-6_wp
      filter_val3 = -0.276667298808735176085978E-6_wp
      et0 = et0 + (x(mod_arr(-6 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-6 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-6 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-6 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.14147525211040058739651301E-4_wp
      filter_val1 = 0.2075567562651587632761451E-5_wp
      filter_val2 = 0.24486891695262527278480373E-4_wp
      filter_val3 = -0.3318610754044407103712689E-5_wp
      et0 = et0 + (x(mod_arr(-5 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-5 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-5 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-5 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.239878289281775075165492659E-3_wp
      filter_val1 = 0.25057266882581997397910903E-4_wp
      filter_val2 = -0.464504573781934008099959241E-3_wp
      filter_val3 = -0.40668222535397420851105277E-4_wp
      et0 = et0 + (x(mod_arr(-4 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-4 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-4 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-4 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.1713514567376228224101768783E-2_wp
      filter_val1 = 0.1554774474923793899293724602E-2_wp
      filter_val2 = 0.3743663199398739083068656973E-2_wp
      filter_val3 = -0.272132773540999536127987988E-2_wp
      et0 = et0 + (x(mod_arr(-3 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-3 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-3 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-3 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.8020884755969165545893465146E-2_wp
      filter_val1 = -0.13367239421126794640208314967E-1_wp
      filter_val2 = -0.22857677317437822508458981792E-1_wp
      filter_val3 = 0.28206423027424364666613729847E-1_wp
      et0 = et0 + (x(mod_arr(-2 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-2 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-2 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-2 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.3131289721448565965922638841E-1_wp
      filter_val1 = 0.43793326416155009500101924188E-1_wp
      filter_val2 = 0.77858113707232306275457825041E-1_wp
      filter_val3 = -0.119506731011354509446152089555E0_wp
      et0 = et0 + (x(mod_arr(-1 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-1 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-1 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-1 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.425987982696947193304592242583E0_wp
      filter_val1 = -0.14743648317973476936220327736E-1_wp
      filter_val2 = -0.781104684144826958489597897908E0_wp
      filter_val3 = 0.43810509543421050421696718266E0_wp
      et0 = et0 + (x(mod_arr(0 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(0 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(0 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(0 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.416066936766212575361540821295E0_wp
      filter_val1 = 0.780460480250744028494513806689E0_wp
      filter_val2 = 0.49247868599583189247332328007E-1_wp
      filter_val3 = 0.403467221125547183465707078756E0_wp
      et0 = et0 + (x(mod_arr(1 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(1 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(1 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(1 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.138524775674051000848952544828E0_wp
      filter_val1 = -0.110230081678334468262552683806E0_wp
      filter_val2 = -0.39920047232423474172371930475E-1_wp
      filter_val3 = -0.52935814515665507351837970092E-1_wp
      et0 = et0 + (x(mod_arr(2 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(2 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(2 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(2 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.318354143459706681365834462E-1_wp
      filter_val1 = 0.2342461473539713439541159825E-1_wp
      filter_val2 = 0.15281781287179308494904296254E-1_wp
      filter_val3 = 0.7288836926603960462299666138E-2_wp
      et0 = et0 + (x(mod_arr(3 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(3 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(3 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(3 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.3454315576482209830827198831E-2_wp
      filter_val1 = -0.4329999533120824069665375511E-2_wp
      filter_val2 = -0.1969945431937324753826114662E-2_wp
      filter_val3 = -0.1960213959538483354691921355E-2_wp
      et0 = et0 + (x(mod_arr(4 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(4 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(4 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(4 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.3416290232098375589631534E-4_wp
      filter_val1 = 0.547310401099738937766344803E-3_wp
      filter_val2 = -0.11402461428743860613089559E-4_wp
      filter_val3 = 0.28479503466830096256502681E-3_wp
      et0 = et0 + (x(mod_arr(5 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(5 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(5 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(5 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.974884267415657697560756E-5_wp
      filter_val1 = -0.2848044279309673361077534E-4_wp
      filter_val2 = 0.5589042956883420231324354E-5_wp
      filter_val3 = -0.1716983589581506701252117E-4_wp
      et0 = et0 + (x(mod_arr(6 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(6 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(6 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(6 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.173933065619790394896889E-6_wp
      filter_val1 = -0.1557817271407449628766103E-5_wp
      filter_val2 = 0.9718640758826677836603E-7_wp
      filter_val3 = -0.877963062125028995526077E-6_wp
      et0 = et0 + (x(mod_arr(7 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(7 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(7 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(7 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.0_wp
      filter_val1 = -0.9225014992147408201492E-8_wp
      filter_val2 = 0.0_wp
      filter_val3 = -0.5154546456362229249344E-8_wp
      et0 = et0 + (x(mod_arr(8 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(8 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(8 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(8 + i1), 1, i2 + 0)) * (filter_val3)
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
    do i1 =  -(-7), n - (8) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = -0.24646355857433108358764E-7_wp
      filter_val1 = 0.1593875388117163824756E-8_wp
      filter_val2 = 0.44109215856612868615909E-7_wp
      filter_val3 = -0.2852535033969307505679E-8_wp
      et0 = et0 + (x(-7 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-7 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-7 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-7 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.40033729064957944938397E-6_wp
      filter_val1 = 0.156915542257710856911416E-6_wp
      filter_val2 = 0.652118308986595532963584E-6_wp
      filter_val3 = -0.276667298808735176085978E-6_wp
      et0 = et0 + (x(-6 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-6 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-6 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-6 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.14147525211040058739651301E-4_wp
      filter_val1 = 0.2075567562651587632761451E-5_wp
      filter_val2 = 0.24486891695262527278480373E-4_wp
      filter_val3 = -0.3318610754044407103712689E-5_wp
      et0 = et0 + (x(-5 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-5 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-5 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-5 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.239878289281775075165492659E-3_wp
      filter_val1 = 0.25057266882581997397910903E-4_wp
      filter_val2 = -0.464504573781934008099959241E-3_wp
      filter_val3 = -0.40668222535397420851105277E-4_wp
      et0 = et0 + (x(-4 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-4 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-4 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-4 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.1713514567376228224101768783E-2_wp
      filter_val1 = 0.1554774474923793899293724602E-2_wp
      filter_val2 = 0.3743663199398739083068656973E-2_wp
      filter_val3 = -0.272132773540999536127987988E-2_wp
      et0 = et0 + (x(-3 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-3 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-3 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-3 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.8020884755969165545893465146E-2_wp
      filter_val1 = -0.13367239421126794640208314967E-1_wp
      filter_val2 = -0.22857677317437822508458981792E-1_wp
      filter_val3 = 0.28206423027424364666613729847E-1_wp
      et0 = et0 + (x(-2 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-2 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-2 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-2 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.3131289721448565965922638841E-1_wp
      filter_val1 = 0.43793326416155009500101924188E-1_wp
      filter_val2 = 0.77858113707232306275457825041E-1_wp
      filter_val3 = -0.119506731011354509446152089555E0_wp
      et0 = et0 + (x(-1 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-1 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-1 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-1 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.425987982696947193304592242583E0_wp
      filter_val1 = -0.14743648317973476936220327736E-1_wp
      filter_val2 = -0.781104684144826958489597897908E0_wp
      filter_val3 = 0.43810509543421050421696718266E0_wp
      et0 = et0 + (x(0 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(0 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(0 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(0 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.416066936766212575361540821295E0_wp
      filter_val1 = 0.780460480250744028494513806689E0_wp
      filter_val2 = 0.49247868599583189247332328007E-1_wp
      filter_val3 = 0.403467221125547183465707078756E0_wp
      et0 = et0 + (x(1 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(1 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(1 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(1 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.138524775674051000848952544828E0_wp
      filter_val1 = -0.110230081678334468262552683806E0_wp
      filter_val2 = -0.39920047232423474172371930475E-1_wp
      filter_val3 = -0.52935814515665507351837970092E-1_wp
      et0 = et0 + (x(2 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(2 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(2 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(2 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.318354143459706681365834462E-1_wp
      filter_val1 = 0.2342461473539713439541159825E-1_wp
      filter_val2 = 0.15281781287179308494904296254E-1_wp
      filter_val3 = 0.7288836926603960462299666138E-2_wp
      et0 = et0 + (x(3 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(3 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(3 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(3 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.3454315576482209830827198831E-2_wp
      filter_val1 = -0.4329999533120824069665375511E-2_wp
      filter_val2 = -0.1969945431937324753826114662E-2_wp
      filter_val3 = -0.1960213959538483354691921355E-2_wp
      et0 = et0 + (x(4 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(4 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(4 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(4 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.3416290232098375589631534E-4_wp
      filter_val1 = 0.547310401099738937766344803E-3_wp
      filter_val2 = -0.11402461428743860613089559E-4_wp
      filter_val3 = 0.28479503466830096256502681E-3_wp
      et0 = et0 + (x(5 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(5 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(5 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(5 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.974884267415657697560756E-5_wp
      filter_val1 = -0.2848044279309673361077534E-4_wp
      filter_val2 = 0.5589042956883420231324354E-5_wp
      filter_val3 = -0.1716983589581506701252117E-4_wp
      et0 = et0 + (x(6 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(6 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(6 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(6 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.173933065619790394896889E-6_wp
      filter_val1 = -0.1557817271407449628766103E-5_wp
      filter_val2 = 0.9718640758826677836603E-7_wp
      filter_val3 = -0.877963062125028995526077E-6_wp
      et0 = et0 + (x(7 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(7 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(7 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(7 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.0_wp
      filter_val1 = -0.9225014992147408201492E-8_wp
      filter_val2 = 0.0_wp
      filter_val3 = -0.5154546456362229249344E-8_wp
      et0 = et0 + (x(8 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(8 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(8 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(8 + i1, 1, i2 + 0)) * (filter_val3)
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
    do i1 = n - (8), n - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = -0.24646355857433108358764E-7_wp
      filter_val1 = 0.1593875388117163824756E-8_wp
      filter_val2 = 0.44109215856612868615909E-7_wp
      filter_val3 = -0.2852535033969307505679E-8_wp
      et0 = et0 + (x(mod_arr(-7 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-7 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-7 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-7 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = -0.40033729064957944938397E-6_wp
      filter_val1 = 0.156915542257710856911416E-6_wp
      filter_val2 = 0.652118308986595532963584E-6_wp
      filter_val3 = -0.276667298808735176085978E-6_wp
      et0 = et0 + (x(mod_arr(-6 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-6 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-6 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-6 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = -0.14147525211040058739651301E-4_wp
      filter_val1 = 0.2075567562651587632761451E-5_wp
      filter_val2 = 0.24486891695262527278480373E-4_wp
      filter_val3 = -0.3318610754044407103712689E-5_wp
      et0 = et0 + (x(mod_arr(-5 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-5 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-5 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-5 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = 0.239878289281775075165492659E-3_wp
      filter_val1 = 0.25057266882581997397910903E-4_wp
      filter_val2 = -0.464504573781934008099959241E-3_wp
      filter_val3 = -0.40668222535397420851105277E-4_wp
      et0 = et0 + (x(mod_arr(-4 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-4 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-4 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-4 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = -0.1713514567376228224101768783E-2_wp
      filter_val1 = 0.1554774474923793899293724602E-2_wp
      filter_val2 = 0.3743663199398739083068656973E-2_wp
      filter_val3 = -0.272132773540999536127987988E-2_wp
      et0 = et0 + (x(mod_arr(-3 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-3 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-3 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-3 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = 0.8020884755969165545893465146E-2_wp
      filter_val1 = -0.13367239421126794640208314967E-1_wp
      filter_val2 = -0.22857677317437822508458981792E-1_wp
      filter_val3 = 0.28206423027424364666613729847E-1_wp
      et0 = et0 + (x(mod_arr(-2 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-2 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-2 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-2 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = -0.3131289721448565965922638841E-1_wp
      filter_val1 = 0.43793326416155009500101924188E-1_wp
      filter_val2 = 0.77858113707232306275457825041E-1_wp
      filter_val3 = -0.119506731011354509446152089555E0_wp
      et0 = et0 + (x(mod_arr(-1 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-1 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-1 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-1 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = 0.425987982696947193304592242583E0_wp
      filter_val1 = -0.14743648317973476936220327736E-1_wp
      filter_val2 = -0.781104684144826958489597897908E0_wp
      filter_val3 = 0.43810509543421050421696718266E0_wp
      et0 = et0 + (x(mod_arr(0 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(0 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(0 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(0 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = 0.416066936766212575361540821295E0_wp
      filter_val1 = 0.780460480250744028494513806689E0_wp
      filter_val2 = 0.49247868599583189247332328007E-1_wp
      filter_val3 = 0.403467221125547183465707078756E0_wp
      et0 = et0 + (x(mod_arr(1 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(1 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(1 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(1 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = -0.138524775674051000848952544828E0_wp
      filter_val1 = -0.110230081678334468262552683806E0_wp
      filter_val2 = -0.39920047232423474172371930475E-1_wp
      filter_val3 = -0.52935814515665507351837970092E-1_wp
      et0 = et0 + (x(mod_arr(2 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(2 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(2 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(2 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = 0.318354143459706681365834462E-1_wp
      filter_val1 = 0.2342461473539713439541159825E-1_wp
      filter_val2 = 0.15281781287179308494904296254E-1_wp
      filter_val3 = 0.7288836926603960462299666138E-2_wp
      et0 = et0 + (x(mod_arr(3 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(3 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(3 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(3 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = -0.3454315576482209830827198831E-2_wp
      filter_val1 = -0.4329999533120824069665375511E-2_wp
      filter_val2 = -0.1969945431937324753826114662E-2_wp
      filter_val3 = -0.1960213959538483354691921355E-2_wp
      et0 = et0 + (x(mod_arr(4 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(4 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(4 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(4 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = -0.3416290232098375589631534E-4_wp
      filter_val1 = 0.547310401099738937766344803E-3_wp
      filter_val2 = -0.11402461428743860613089559E-4_wp
      filter_val3 = 0.28479503466830096256502681E-3_wp
      et0 = et0 + (x(mod_arr(5 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(5 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(5 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(5 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = 0.974884267415657697560756E-5_wp
      filter_val1 = -0.2848044279309673361077534E-4_wp
      filter_val2 = 0.5589042956883420231324354E-5_wp
      filter_val3 = -0.1716983589581506701252117E-4_wp
      et0 = et0 + (x(mod_arr(6 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(6 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(6 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(6 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = 0.173933065619790394896889E-6_wp
      filter_val1 = -0.1557817271407449628766103E-5_wp
      filter_val2 = 0.9718640758826677836603E-7_wp
      filter_val3 = -0.877963062125028995526077E-6_wp
      et0 = et0 + (x(mod_arr(7 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(7 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(7 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(7 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = 0.0_wp
      filter_val1 = -0.9225014992147408201492E-8_wp
      filter_val2 = 0.0_wp
      filter_val3 = -0.5154546456362229249344E-8_wp
      et0 = et0 + (x(mod_arr(8 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(8 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(8 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(8 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_p_10_ld_1u1_v1_t_f_t
SUBROUTINE d_s1s0_symicomb8_p_10_ld_1u1_v1_t_f_t_cost(n, ndat1, cost)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat1
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * (ndat1)
END SUBROUTINE d_s1s0_symicomb8_p_10_ld_1u1_v1_t_f_t_cost
SUBROUTINE d_s1s0_symicomb8_p_10_a_ld_1u1_v1_t_f_t(n, ndat1, nx, ny, x&
&, y, a)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -7
  integer(kind=4), parameter :: upfil = 8
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat1
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:nx - (1), 0:1, 0:ndat1 - (1)) &
&:: x
  real(kind=8), intent(out), dimension(0:1, 0:ny - (1), 0:ndat1 - (1))&
& :: y
  real(kind=8), intent(in) :: a
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_lre_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_lro_fil = resh&
&ape((/ &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp, &
0.0_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_hre_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_hro_fil = resh&
&ape((/ &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp, &
0.0_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
  integer(kind=4), dimension(-7 - (8):8 - (-7) - (1)) :: mod_arr
  do l = -7 - (8), 8 - (-7) - (1), 1
    mod_arr(l) = modulo(l, n)
  end do
!$omp parallel  default(shared) private(i1, i2, l, et0, ot0, filt&
!$omp&er_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i2 = 0, ndat1 - (1), 1
    do i1 = 0,  -(-7) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = -0.24646355857433108358764E-7_wp
      filter_val1 = 0.1593875388117163824756E-8_wp
      filter_val2 = 0.44109215856612868615909E-7_wp
      filter_val3 = -0.2852535033969307505679E-8_wp
      et0 = et0 + (x(mod_arr(-7 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-7 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-7 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-7 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.40033729064957944938397E-6_wp
      filter_val1 = 0.156915542257710856911416E-6_wp
      filter_val2 = 0.652118308986595532963584E-6_wp
      filter_val3 = -0.276667298808735176085978E-6_wp
      et0 = et0 + (x(mod_arr(-6 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-6 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-6 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-6 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.14147525211040058739651301E-4_wp
      filter_val1 = 0.2075567562651587632761451E-5_wp
      filter_val2 = 0.24486891695262527278480373E-4_wp
      filter_val3 = -0.3318610754044407103712689E-5_wp
      et0 = et0 + (x(mod_arr(-5 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-5 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-5 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-5 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.239878289281775075165492659E-3_wp
      filter_val1 = 0.25057266882581997397910903E-4_wp
      filter_val2 = -0.464504573781934008099959241E-3_wp
      filter_val3 = -0.40668222535397420851105277E-4_wp
      et0 = et0 + (x(mod_arr(-4 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-4 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-4 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-4 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.1713514567376228224101768783E-2_wp
      filter_val1 = 0.1554774474923793899293724602E-2_wp
      filter_val2 = 0.3743663199398739083068656973E-2_wp
      filter_val3 = -0.272132773540999536127987988E-2_wp
      et0 = et0 + (x(mod_arr(-3 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-3 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-3 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-3 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.8020884755969165545893465146E-2_wp
      filter_val1 = -0.13367239421126794640208314967E-1_wp
      filter_val2 = -0.22857677317437822508458981792E-1_wp
      filter_val3 = 0.28206423027424364666613729847E-1_wp
      et0 = et0 + (x(mod_arr(-2 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-2 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-2 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-2 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.3131289721448565965922638841E-1_wp
      filter_val1 = 0.43793326416155009500101924188E-1_wp
      filter_val2 = 0.77858113707232306275457825041E-1_wp
      filter_val3 = -0.119506731011354509446152089555E0_wp
      et0 = et0 + (x(mod_arr(-1 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-1 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-1 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-1 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.425987982696947193304592242583E0_wp
      filter_val1 = -0.14743648317973476936220327736E-1_wp
      filter_val2 = -0.781104684144826958489597897908E0_wp
      filter_val3 = 0.43810509543421050421696718266E0_wp
      et0 = et0 + (x(mod_arr(0 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(0 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(0 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(0 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.416066936766212575361540821295E0_wp
      filter_val1 = 0.780460480250744028494513806689E0_wp
      filter_val2 = 0.49247868599583189247332328007E-1_wp
      filter_val3 = 0.403467221125547183465707078756E0_wp
      et0 = et0 + (x(mod_arr(1 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(1 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(1 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(1 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.138524775674051000848952544828E0_wp
      filter_val1 = -0.110230081678334468262552683806E0_wp
      filter_val2 = -0.39920047232423474172371930475E-1_wp
      filter_val3 = -0.52935814515665507351837970092E-1_wp
      et0 = et0 + (x(mod_arr(2 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(2 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(2 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(2 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.318354143459706681365834462E-1_wp
      filter_val1 = 0.2342461473539713439541159825E-1_wp
      filter_val2 = 0.15281781287179308494904296254E-1_wp
      filter_val3 = 0.7288836926603960462299666138E-2_wp
      et0 = et0 + (x(mod_arr(3 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(3 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(3 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(3 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.3454315576482209830827198831E-2_wp
      filter_val1 = -0.4329999533120824069665375511E-2_wp
      filter_val2 = -0.1969945431937324753826114662E-2_wp
      filter_val3 = -0.1960213959538483354691921355E-2_wp
      et0 = et0 + (x(mod_arr(4 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(4 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(4 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(4 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.3416290232098375589631534E-4_wp
      filter_val1 = 0.547310401099738937766344803E-3_wp
      filter_val2 = -0.11402461428743860613089559E-4_wp
      filter_val3 = 0.28479503466830096256502681E-3_wp
      et0 = et0 + (x(mod_arr(5 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(5 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(5 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(5 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.974884267415657697560756E-5_wp
      filter_val1 = -0.2848044279309673361077534E-4_wp
      filter_val2 = 0.5589042956883420231324354E-5_wp
      filter_val3 = -0.1716983589581506701252117E-4_wp
      et0 = et0 + (x(mod_arr(6 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(6 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(6 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(6 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.173933065619790394896889E-6_wp
      filter_val1 = -0.1557817271407449628766103E-5_wp
      filter_val2 = 0.9718640758826677836603E-7_wp
      filter_val3 = -0.877963062125028995526077E-6_wp
      et0 = et0 + (x(mod_arr(7 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(7 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(7 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(7 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.0_wp
      filter_val1 = -0.9225014992147408201492E-8_wp
      filter_val2 = 0.0_wp
      filter_val3 = -0.5154546456362229249344E-8_wp
      et0 = et0 + (x(mod_arr(8 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(8 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(8 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(8 + i1), 1, i2 + 0)) * (filter_val3)
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
    do i1 =  -(-7), n - (8) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = -0.24646355857433108358764E-7_wp
      filter_val1 = 0.1593875388117163824756E-8_wp
      filter_val2 = 0.44109215856612868615909E-7_wp
      filter_val3 = -0.2852535033969307505679E-8_wp
      et0 = et0 + (x(-7 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-7 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-7 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-7 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.40033729064957944938397E-6_wp
      filter_val1 = 0.156915542257710856911416E-6_wp
      filter_val2 = 0.652118308986595532963584E-6_wp
      filter_val3 = -0.276667298808735176085978E-6_wp
      et0 = et0 + (x(-6 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-6 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-6 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-6 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.14147525211040058739651301E-4_wp
      filter_val1 = 0.2075567562651587632761451E-5_wp
      filter_val2 = 0.24486891695262527278480373E-4_wp
      filter_val3 = -0.3318610754044407103712689E-5_wp
      et0 = et0 + (x(-5 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-5 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-5 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-5 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.239878289281775075165492659E-3_wp
      filter_val1 = 0.25057266882581997397910903E-4_wp
      filter_val2 = -0.464504573781934008099959241E-3_wp
      filter_val3 = -0.40668222535397420851105277E-4_wp
      et0 = et0 + (x(-4 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-4 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-4 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-4 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.1713514567376228224101768783E-2_wp
      filter_val1 = 0.1554774474923793899293724602E-2_wp
      filter_val2 = 0.3743663199398739083068656973E-2_wp
      filter_val3 = -0.272132773540999536127987988E-2_wp
      et0 = et0 + (x(-3 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-3 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-3 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-3 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.8020884755969165545893465146E-2_wp
      filter_val1 = -0.13367239421126794640208314967E-1_wp
      filter_val2 = -0.22857677317437822508458981792E-1_wp
      filter_val3 = 0.28206423027424364666613729847E-1_wp
      et0 = et0 + (x(-2 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-2 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-2 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-2 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.3131289721448565965922638841E-1_wp
      filter_val1 = 0.43793326416155009500101924188E-1_wp
      filter_val2 = 0.77858113707232306275457825041E-1_wp
      filter_val3 = -0.119506731011354509446152089555E0_wp
      et0 = et0 + (x(-1 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-1 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-1 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-1 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.425987982696947193304592242583E0_wp
      filter_val1 = -0.14743648317973476936220327736E-1_wp
      filter_val2 = -0.781104684144826958489597897908E0_wp
      filter_val3 = 0.43810509543421050421696718266E0_wp
      et0 = et0 + (x(0 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(0 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(0 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(0 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.416066936766212575361540821295E0_wp
      filter_val1 = 0.780460480250744028494513806689E0_wp
      filter_val2 = 0.49247868599583189247332328007E-1_wp
      filter_val3 = 0.403467221125547183465707078756E0_wp
      et0 = et0 + (x(1 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(1 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(1 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(1 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.138524775674051000848952544828E0_wp
      filter_val1 = -0.110230081678334468262552683806E0_wp
      filter_val2 = -0.39920047232423474172371930475E-1_wp
      filter_val3 = -0.52935814515665507351837970092E-1_wp
      et0 = et0 + (x(2 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(2 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(2 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(2 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.318354143459706681365834462E-1_wp
      filter_val1 = 0.2342461473539713439541159825E-1_wp
      filter_val2 = 0.15281781287179308494904296254E-1_wp
      filter_val3 = 0.7288836926603960462299666138E-2_wp
      et0 = et0 + (x(3 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(3 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(3 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(3 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.3454315576482209830827198831E-2_wp
      filter_val1 = -0.4329999533120824069665375511E-2_wp
      filter_val2 = -0.1969945431937324753826114662E-2_wp
      filter_val3 = -0.1960213959538483354691921355E-2_wp
      et0 = et0 + (x(4 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(4 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(4 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(4 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.3416290232098375589631534E-4_wp
      filter_val1 = 0.547310401099738937766344803E-3_wp
      filter_val2 = -0.11402461428743860613089559E-4_wp
      filter_val3 = 0.28479503466830096256502681E-3_wp
      et0 = et0 + (x(5 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(5 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(5 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(5 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.974884267415657697560756E-5_wp
      filter_val1 = -0.2848044279309673361077534E-4_wp
      filter_val2 = 0.5589042956883420231324354E-5_wp
      filter_val3 = -0.1716983589581506701252117E-4_wp
      et0 = et0 + (x(6 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(6 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(6 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(6 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.173933065619790394896889E-6_wp
      filter_val1 = -0.1557817271407449628766103E-5_wp
      filter_val2 = 0.9718640758826677836603E-7_wp
      filter_val3 = -0.877963062125028995526077E-6_wp
      et0 = et0 + (x(7 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(7 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(7 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(7 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.0_wp
      filter_val1 = -0.9225014992147408201492E-8_wp
      filter_val2 = 0.0_wp
      filter_val3 = -0.5154546456362229249344E-8_wp
      et0 = et0 + (x(8 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(8 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(8 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(8 + i1, 1, i2 + 0)) * (filter_val3)
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
    do i1 = n - (8), n - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = -0.24646355857433108358764E-7_wp
      filter_val1 = 0.1593875388117163824756E-8_wp
      filter_val2 = 0.44109215856612868615909E-7_wp
      filter_val3 = -0.2852535033969307505679E-8_wp
      et0 = et0 + (x(mod_arr(-7 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-7 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-7 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-7 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = -0.40033729064957944938397E-6_wp
      filter_val1 = 0.156915542257710856911416E-6_wp
      filter_val2 = 0.652118308986595532963584E-6_wp
      filter_val3 = -0.276667298808735176085978E-6_wp
      et0 = et0 + (x(mod_arr(-6 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-6 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-6 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-6 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = -0.14147525211040058739651301E-4_wp
      filter_val1 = 0.2075567562651587632761451E-5_wp
      filter_val2 = 0.24486891695262527278480373E-4_wp
      filter_val3 = -0.3318610754044407103712689E-5_wp
      et0 = et0 + (x(mod_arr(-5 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-5 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-5 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-5 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = 0.239878289281775075165492659E-3_wp
      filter_val1 = 0.25057266882581997397910903E-4_wp
      filter_val2 = -0.464504573781934008099959241E-3_wp
      filter_val3 = -0.40668222535397420851105277E-4_wp
      et0 = et0 + (x(mod_arr(-4 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-4 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-4 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-4 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = -0.1713514567376228224101768783E-2_wp
      filter_val1 = 0.1554774474923793899293724602E-2_wp
      filter_val2 = 0.3743663199398739083068656973E-2_wp
      filter_val3 = -0.272132773540999536127987988E-2_wp
      et0 = et0 + (x(mod_arr(-3 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-3 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-3 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-3 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = 0.8020884755969165545893465146E-2_wp
      filter_val1 = -0.13367239421126794640208314967E-1_wp
      filter_val2 = -0.22857677317437822508458981792E-1_wp
      filter_val3 = 0.28206423027424364666613729847E-1_wp
      et0 = et0 + (x(mod_arr(-2 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-2 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-2 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-2 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = -0.3131289721448565965922638841E-1_wp
      filter_val1 = 0.43793326416155009500101924188E-1_wp
      filter_val2 = 0.77858113707232306275457825041E-1_wp
      filter_val3 = -0.119506731011354509446152089555E0_wp
      et0 = et0 + (x(mod_arr(-1 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-1 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-1 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-1 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = 0.425987982696947193304592242583E0_wp
      filter_val1 = -0.14743648317973476936220327736E-1_wp
      filter_val2 = -0.781104684144826958489597897908E0_wp
      filter_val3 = 0.43810509543421050421696718266E0_wp
      et0 = et0 + (x(mod_arr(0 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(0 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(0 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(0 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = 0.416066936766212575361540821295E0_wp
      filter_val1 = 0.780460480250744028494513806689E0_wp
      filter_val2 = 0.49247868599583189247332328007E-1_wp
      filter_val3 = 0.403467221125547183465707078756E0_wp
      et0 = et0 + (x(mod_arr(1 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(1 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(1 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(1 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = -0.138524775674051000848952544828E0_wp
      filter_val1 = -0.110230081678334468262552683806E0_wp
      filter_val2 = -0.39920047232423474172371930475E-1_wp
      filter_val3 = -0.52935814515665507351837970092E-1_wp
      et0 = et0 + (x(mod_arr(2 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(2 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(2 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(2 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = 0.318354143459706681365834462E-1_wp
      filter_val1 = 0.2342461473539713439541159825E-1_wp
      filter_val2 = 0.15281781287179308494904296254E-1_wp
      filter_val3 = 0.7288836926603960462299666138E-2_wp
      et0 = et0 + (x(mod_arr(3 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(3 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(3 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(3 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = -0.3454315576482209830827198831E-2_wp
      filter_val1 = -0.4329999533120824069665375511E-2_wp
      filter_val2 = -0.1969945431937324753826114662E-2_wp
      filter_val3 = -0.1960213959538483354691921355E-2_wp
      et0 = et0 + (x(mod_arr(4 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(4 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(4 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(4 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = -0.3416290232098375589631534E-4_wp
      filter_val1 = 0.547310401099738937766344803E-3_wp
      filter_val2 = -0.11402461428743860613089559E-4_wp
      filter_val3 = 0.28479503466830096256502681E-3_wp
      et0 = et0 + (x(mod_arr(5 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(5 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(5 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(5 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = 0.974884267415657697560756E-5_wp
      filter_val1 = -0.2848044279309673361077534E-4_wp
      filter_val2 = 0.5589042956883420231324354E-5_wp
      filter_val3 = -0.1716983589581506701252117E-4_wp
      et0 = et0 + (x(mod_arr(6 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(6 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(6 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(6 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = 0.173933065619790394896889E-6_wp
      filter_val1 = -0.1557817271407449628766103E-5_wp
      filter_val2 = 0.9718640758826677836603E-7_wp
      filter_val3 = -0.877963062125028995526077E-6_wp
      et0 = et0 + (x(mod_arr(7 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(7 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(7 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(7 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = 0.0_wp
      filter_val1 = -0.9225014992147408201492E-8_wp
      filter_val2 = 0.0_wp
      filter_val3 = -0.5154546456362229249344E-8_wp
      et0 = et0 + (x(mod_arr(8 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(8 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(8 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(8 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_p_10_a_ld_1u1_v1_t_f_t
SUBROUTINE d_s1s0_symicomb8_p_10_a_ld_1u1_v1_t_f_t_cost(n, ndat1, cost&
&)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat1
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * (ndat1)
END SUBROUTINE d_s1s0_symicomb8_p_10_a_ld_1u1_v1_t_f_t_cost
SUBROUTINE d_s1s0_symicomb8_p_10_ay_ld_1u1_v1_t_f_t(n, ndat1, nx, ny, &
&x, y, a_y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -7
  integer(kind=4), parameter :: upfil = 8
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat1
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:nx - (1), 0:1, 0:ndat1 - (1)) &
&:: x
  real(kind=8), intent(inout), dimension(0:1, 0:ny - (1), 0:ndat1 - (1&
&)) :: y
  real(kind=8), intent(in) :: a_y
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_lre_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_lro_fil = resh&
&ape((/ &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp, &
0.0_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_hre_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_hro_fil = resh&
&ape((/ &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp, &
0.0_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
  integer(kind=4), dimension(-7 - (8):8 - (-7) - (1)) :: mod_arr
  do l = -7 - (8), 8 - (-7) - (1), 1
    mod_arr(l) = modulo(l, n)
  end do
!$omp parallel  default(shared) private(i1, i2, l, et0, ot0, filt&
!$omp&er_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i2 = 0, ndat1 - (1), 1
    do i1 = 0,  -(-7) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = -0.24646355857433108358764E-7_wp
      filter_val1 = 0.1593875388117163824756E-8_wp
      filter_val2 = 0.44109215856612868615909E-7_wp
      filter_val3 = -0.2852535033969307505679E-8_wp
      et0 = et0 + (x(mod_arr(-7 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-7 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-7 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-7 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.40033729064957944938397E-6_wp
      filter_val1 = 0.156915542257710856911416E-6_wp
      filter_val2 = 0.652118308986595532963584E-6_wp
      filter_val3 = -0.276667298808735176085978E-6_wp
      et0 = et0 + (x(mod_arr(-6 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-6 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-6 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-6 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.14147525211040058739651301E-4_wp
      filter_val1 = 0.2075567562651587632761451E-5_wp
      filter_val2 = 0.24486891695262527278480373E-4_wp
      filter_val3 = -0.3318610754044407103712689E-5_wp
      et0 = et0 + (x(mod_arr(-5 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-5 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-5 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-5 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.239878289281775075165492659E-3_wp
      filter_val1 = 0.25057266882581997397910903E-4_wp
      filter_val2 = -0.464504573781934008099959241E-3_wp
      filter_val3 = -0.40668222535397420851105277E-4_wp
      et0 = et0 + (x(mod_arr(-4 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-4 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-4 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-4 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.1713514567376228224101768783E-2_wp
      filter_val1 = 0.1554774474923793899293724602E-2_wp
      filter_val2 = 0.3743663199398739083068656973E-2_wp
      filter_val3 = -0.272132773540999536127987988E-2_wp
      et0 = et0 + (x(mod_arr(-3 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-3 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-3 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-3 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.8020884755969165545893465146E-2_wp
      filter_val1 = -0.13367239421126794640208314967E-1_wp
      filter_val2 = -0.22857677317437822508458981792E-1_wp
      filter_val3 = 0.28206423027424364666613729847E-1_wp
      et0 = et0 + (x(mod_arr(-2 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-2 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-2 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-2 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.3131289721448565965922638841E-1_wp
      filter_val1 = 0.43793326416155009500101924188E-1_wp
      filter_val2 = 0.77858113707232306275457825041E-1_wp
      filter_val3 = -0.119506731011354509446152089555E0_wp
      et0 = et0 + (x(mod_arr(-1 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-1 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-1 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-1 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.425987982696947193304592242583E0_wp
      filter_val1 = -0.14743648317973476936220327736E-1_wp
      filter_val2 = -0.781104684144826958489597897908E0_wp
      filter_val3 = 0.43810509543421050421696718266E0_wp
      et0 = et0 + (x(mod_arr(0 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(0 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(0 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(0 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.416066936766212575361540821295E0_wp
      filter_val1 = 0.780460480250744028494513806689E0_wp
      filter_val2 = 0.49247868599583189247332328007E-1_wp
      filter_val3 = 0.403467221125547183465707078756E0_wp
      et0 = et0 + (x(mod_arr(1 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(1 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(1 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(1 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.138524775674051000848952544828E0_wp
      filter_val1 = -0.110230081678334468262552683806E0_wp
      filter_val2 = -0.39920047232423474172371930475E-1_wp
      filter_val3 = -0.52935814515665507351837970092E-1_wp
      et0 = et0 + (x(mod_arr(2 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(2 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(2 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(2 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.318354143459706681365834462E-1_wp
      filter_val1 = 0.2342461473539713439541159825E-1_wp
      filter_val2 = 0.15281781287179308494904296254E-1_wp
      filter_val3 = 0.7288836926603960462299666138E-2_wp
      et0 = et0 + (x(mod_arr(3 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(3 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(3 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(3 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.3454315576482209830827198831E-2_wp
      filter_val1 = -0.4329999533120824069665375511E-2_wp
      filter_val2 = -0.1969945431937324753826114662E-2_wp
      filter_val3 = -0.1960213959538483354691921355E-2_wp
      et0 = et0 + (x(mod_arr(4 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(4 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(4 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(4 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.3416290232098375589631534E-4_wp
      filter_val1 = 0.547310401099738937766344803E-3_wp
      filter_val2 = -0.11402461428743860613089559E-4_wp
      filter_val3 = 0.28479503466830096256502681E-3_wp
      et0 = et0 + (x(mod_arr(5 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(5 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(5 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(5 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.974884267415657697560756E-5_wp
      filter_val1 = -0.2848044279309673361077534E-4_wp
      filter_val2 = 0.5589042956883420231324354E-5_wp
      filter_val3 = -0.1716983589581506701252117E-4_wp
      et0 = et0 + (x(mod_arr(6 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(6 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(6 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(6 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.173933065619790394896889E-6_wp
      filter_val1 = -0.1557817271407449628766103E-5_wp
      filter_val2 = 0.9718640758826677836603E-7_wp
      filter_val3 = -0.877963062125028995526077E-6_wp
      et0 = et0 + (x(mod_arr(7 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(7 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(7 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(7 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.0_wp
      filter_val1 = -0.9225014992147408201492E-8_wp
      filter_val2 = 0.0_wp
      filter_val3 = -0.5154546456362229249344E-8_wp
      et0 = et0 + (x(mod_arr(8 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(8 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(8 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(8 + i1), 1, i2 + 0)) * (filter_val3)
      et0 = et0 + (y(0, i1, i2 + 0)) * (a_y)
      ot0 = ot0 + (y(1, i1, i2 + 0)) * (a_y)
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
    do i1 =  -(-7), n - (8) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = -0.24646355857433108358764E-7_wp
      filter_val1 = 0.1593875388117163824756E-8_wp
      filter_val2 = 0.44109215856612868615909E-7_wp
      filter_val3 = -0.2852535033969307505679E-8_wp
      et0 = et0 + (x(-7 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-7 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-7 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-7 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.40033729064957944938397E-6_wp
      filter_val1 = 0.156915542257710856911416E-6_wp
      filter_val2 = 0.652118308986595532963584E-6_wp
      filter_val3 = -0.276667298808735176085978E-6_wp
      et0 = et0 + (x(-6 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-6 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-6 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-6 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.14147525211040058739651301E-4_wp
      filter_val1 = 0.2075567562651587632761451E-5_wp
      filter_val2 = 0.24486891695262527278480373E-4_wp
      filter_val3 = -0.3318610754044407103712689E-5_wp
      et0 = et0 + (x(-5 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-5 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-5 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-5 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.239878289281775075165492659E-3_wp
      filter_val1 = 0.25057266882581997397910903E-4_wp
      filter_val2 = -0.464504573781934008099959241E-3_wp
      filter_val3 = -0.40668222535397420851105277E-4_wp
      et0 = et0 + (x(-4 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-4 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-4 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-4 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.1713514567376228224101768783E-2_wp
      filter_val1 = 0.1554774474923793899293724602E-2_wp
      filter_val2 = 0.3743663199398739083068656973E-2_wp
      filter_val3 = -0.272132773540999536127987988E-2_wp
      et0 = et0 + (x(-3 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-3 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-3 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-3 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.8020884755969165545893465146E-2_wp
      filter_val1 = -0.13367239421126794640208314967E-1_wp
      filter_val2 = -0.22857677317437822508458981792E-1_wp
      filter_val3 = 0.28206423027424364666613729847E-1_wp
      et0 = et0 + (x(-2 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-2 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-2 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-2 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.3131289721448565965922638841E-1_wp
      filter_val1 = 0.43793326416155009500101924188E-1_wp
      filter_val2 = 0.77858113707232306275457825041E-1_wp
      filter_val3 = -0.119506731011354509446152089555E0_wp
      et0 = et0 + (x(-1 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-1 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-1 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-1 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.425987982696947193304592242583E0_wp
      filter_val1 = -0.14743648317973476936220327736E-1_wp
      filter_val2 = -0.781104684144826958489597897908E0_wp
      filter_val3 = 0.43810509543421050421696718266E0_wp
      et0 = et0 + (x(0 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(0 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(0 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(0 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.416066936766212575361540821295E0_wp
      filter_val1 = 0.780460480250744028494513806689E0_wp
      filter_val2 = 0.49247868599583189247332328007E-1_wp
      filter_val3 = 0.403467221125547183465707078756E0_wp
      et0 = et0 + (x(1 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(1 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(1 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(1 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.138524775674051000848952544828E0_wp
      filter_val1 = -0.110230081678334468262552683806E0_wp
      filter_val2 = -0.39920047232423474172371930475E-1_wp
      filter_val3 = -0.52935814515665507351837970092E-1_wp
      et0 = et0 + (x(2 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(2 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(2 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(2 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.318354143459706681365834462E-1_wp
      filter_val1 = 0.2342461473539713439541159825E-1_wp
      filter_val2 = 0.15281781287179308494904296254E-1_wp
      filter_val3 = 0.7288836926603960462299666138E-2_wp
      et0 = et0 + (x(3 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(3 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(3 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(3 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.3454315576482209830827198831E-2_wp
      filter_val1 = -0.4329999533120824069665375511E-2_wp
      filter_val2 = -0.1969945431937324753826114662E-2_wp
      filter_val3 = -0.1960213959538483354691921355E-2_wp
      et0 = et0 + (x(4 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(4 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(4 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(4 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.3416290232098375589631534E-4_wp
      filter_val1 = 0.547310401099738937766344803E-3_wp
      filter_val2 = -0.11402461428743860613089559E-4_wp
      filter_val3 = 0.28479503466830096256502681E-3_wp
      et0 = et0 + (x(5 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(5 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(5 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(5 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.974884267415657697560756E-5_wp
      filter_val1 = -0.2848044279309673361077534E-4_wp
      filter_val2 = 0.5589042956883420231324354E-5_wp
      filter_val3 = -0.1716983589581506701252117E-4_wp
      et0 = et0 + (x(6 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(6 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(6 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(6 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.173933065619790394896889E-6_wp
      filter_val1 = -0.1557817271407449628766103E-5_wp
      filter_val2 = 0.9718640758826677836603E-7_wp
      filter_val3 = -0.877963062125028995526077E-6_wp
      et0 = et0 + (x(7 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(7 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(7 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(7 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.0_wp
      filter_val1 = -0.9225014992147408201492E-8_wp
      filter_val2 = 0.0_wp
      filter_val3 = -0.5154546456362229249344E-8_wp
      et0 = et0 + (x(8 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(8 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(8 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(8 + i1, 1, i2 + 0)) * (filter_val3)
      et0 = et0 + (y(0, i1, i2 + 0)) * (a_y)
      ot0 = ot0 + (y(1, i1, i2 + 0)) * (a_y)
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
    do i1 = n - (8), n - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = -0.24646355857433108358764E-7_wp
      filter_val1 = 0.1593875388117163824756E-8_wp
      filter_val2 = 0.44109215856612868615909E-7_wp
      filter_val3 = -0.2852535033969307505679E-8_wp
      et0 = et0 + (x(mod_arr(-7 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-7 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-7 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-7 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = -0.40033729064957944938397E-6_wp
      filter_val1 = 0.156915542257710856911416E-6_wp
      filter_val2 = 0.652118308986595532963584E-6_wp
      filter_val3 = -0.276667298808735176085978E-6_wp
      et0 = et0 + (x(mod_arr(-6 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-6 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-6 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-6 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = -0.14147525211040058739651301E-4_wp
      filter_val1 = 0.2075567562651587632761451E-5_wp
      filter_val2 = 0.24486891695262527278480373E-4_wp
      filter_val3 = -0.3318610754044407103712689E-5_wp
      et0 = et0 + (x(mod_arr(-5 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-5 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-5 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-5 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = 0.239878289281775075165492659E-3_wp
      filter_val1 = 0.25057266882581997397910903E-4_wp
      filter_val2 = -0.464504573781934008099959241E-3_wp
      filter_val3 = -0.40668222535397420851105277E-4_wp
      et0 = et0 + (x(mod_arr(-4 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-4 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-4 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-4 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = -0.1713514567376228224101768783E-2_wp
      filter_val1 = 0.1554774474923793899293724602E-2_wp
      filter_val2 = 0.3743663199398739083068656973E-2_wp
      filter_val3 = -0.272132773540999536127987988E-2_wp
      et0 = et0 + (x(mod_arr(-3 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-3 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-3 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-3 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = 0.8020884755969165545893465146E-2_wp
      filter_val1 = -0.13367239421126794640208314967E-1_wp
      filter_val2 = -0.22857677317437822508458981792E-1_wp
      filter_val3 = 0.28206423027424364666613729847E-1_wp
      et0 = et0 + (x(mod_arr(-2 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-2 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-2 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-2 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = -0.3131289721448565965922638841E-1_wp
      filter_val1 = 0.43793326416155009500101924188E-1_wp
      filter_val2 = 0.77858113707232306275457825041E-1_wp
      filter_val3 = -0.119506731011354509446152089555E0_wp
      et0 = et0 + (x(mod_arr(-1 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-1 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-1 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-1 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = 0.425987982696947193304592242583E0_wp
      filter_val1 = -0.14743648317973476936220327736E-1_wp
      filter_val2 = -0.781104684144826958489597897908E0_wp
      filter_val3 = 0.43810509543421050421696718266E0_wp
      et0 = et0 + (x(mod_arr(0 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(0 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(0 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(0 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = 0.416066936766212575361540821295E0_wp
      filter_val1 = 0.780460480250744028494513806689E0_wp
      filter_val2 = 0.49247868599583189247332328007E-1_wp
      filter_val3 = 0.403467221125547183465707078756E0_wp
      et0 = et0 + (x(mod_arr(1 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(1 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(1 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(1 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = -0.138524775674051000848952544828E0_wp
      filter_val1 = -0.110230081678334468262552683806E0_wp
      filter_val2 = -0.39920047232423474172371930475E-1_wp
      filter_val3 = -0.52935814515665507351837970092E-1_wp
      et0 = et0 + (x(mod_arr(2 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(2 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(2 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(2 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = 0.318354143459706681365834462E-1_wp
      filter_val1 = 0.2342461473539713439541159825E-1_wp
      filter_val2 = 0.15281781287179308494904296254E-1_wp
      filter_val3 = 0.7288836926603960462299666138E-2_wp
      et0 = et0 + (x(mod_arr(3 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(3 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(3 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(3 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = -0.3454315576482209830827198831E-2_wp
      filter_val1 = -0.4329999533120824069665375511E-2_wp
      filter_val2 = -0.1969945431937324753826114662E-2_wp
      filter_val3 = -0.1960213959538483354691921355E-2_wp
      et0 = et0 + (x(mod_arr(4 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(4 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(4 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(4 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = -0.3416290232098375589631534E-4_wp
      filter_val1 = 0.547310401099738937766344803E-3_wp
      filter_val2 = -0.11402461428743860613089559E-4_wp
      filter_val3 = 0.28479503466830096256502681E-3_wp
      et0 = et0 + (x(mod_arr(5 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(5 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(5 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(5 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = 0.974884267415657697560756E-5_wp
      filter_val1 = -0.2848044279309673361077534E-4_wp
      filter_val2 = 0.5589042956883420231324354E-5_wp
      filter_val3 = -0.1716983589581506701252117E-4_wp
      et0 = et0 + (x(mod_arr(6 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(6 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(6 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(6 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = 0.173933065619790394896889E-6_wp
      filter_val1 = -0.1557817271407449628766103E-5_wp
      filter_val2 = 0.9718640758826677836603E-7_wp
      filter_val3 = -0.877963062125028995526077E-6_wp
      et0 = et0 + (x(mod_arr(7 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(7 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(7 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(7 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = 0.0_wp
      filter_val1 = -0.9225014992147408201492E-8_wp
      filter_val2 = 0.0_wp
      filter_val3 = -0.5154546456362229249344E-8_wp
      et0 = et0 + (x(mod_arr(8 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(8 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(8 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(8 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      et0 = et0 + (y(0, i1, i2 + 0)) * (a_y)
      ot0 = ot0 + (y(1, i1, i2 + 0)) * (a_y)
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_p_10_ay_ld_1u1_v1_t_f_t
SUBROUTINE d_s1s0_symicomb8_p_10_ay_ld_1u1_v1_t_f_t_cost(n, ndat1, cos&
&t)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat1
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * (ndat1)
END SUBROUTINE d_s1s0_symicomb8_p_10_ay_ld_1u1_v1_t_f_t_cost
SUBROUTINE d_s1s0_symicomb8_p_10_a_ay_ld_1u1_v1_t_f_t(n, ndat1, nx, ny&
&, x, y, a, a_y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -7
  integer(kind=4), parameter :: upfil = 8
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat1
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:nx - (1), 0:1, 0:ndat1 - (1)) &
&:: x
  real(kind=8), intent(inout), dimension(0:1, 0:ny - (1), 0:ndat1 - (1&
&)) :: y
  real(kind=8), intent(in) :: a
  real(kind=8), intent(in) :: a_y
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_lre_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_lro_fil = resh&
&ape((/ &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp, &
0.0_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_hre_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_hro_fil = resh&
&ape((/ &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp, &
0.0_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
  integer(kind=4), dimension(-7 - (8):8 - (-7) - (1)) :: mod_arr
  do l = -7 - (8), 8 - (-7) - (1), 1
    mod_arr(l) = modulo(l, n)
  end do
!$omp parallel  default(shared) private(i1, i2, l, et0, ot0, filt&
!$omp&er_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i2 = 0, ndat1 - (1), 1
    do i1 = 0,  -(-7) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = -0.24646355857433108358764E-7_wp
      filter_val1 = 0.1593875388117163824756E-8_wp
      filter_val2 = 0.44109215856612868615909E-7_wp
      filter_val3 = -0.2852535033969307505679E-8_wp
      et0 = et0 + (x(mod_arr(-7 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-7 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-7 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-7 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.40033729064957944938397E-6_wp
      filter_val1 = 0.156915542257710856911416E-6_wp
      filter_val2 = 0.652118308986595532963584E-6_wp
      filter_val3 = -0.276667298808735176085978E-6_wp
      et0 = et0 + (x(mod_arr(-6 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-6 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-6 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-6 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.14147525211040058739651301E-4_wp
      filter_val1 = 0.2075567562651587632761451E-5_wp
      filter_val2 = 0.24486891695262527278480373E-4_wp
      filter_val3 = -0.3318610754044407103712689E-5_wp
      et0 = et0 + (x(mod_arr(-5 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-5 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-5 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-5 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.239878289281775075165492659E-3_wp
      filter_val1 = 0.25057266882581997397910903E-4_wp
      filter_val2 = -0.464504573781934008099959241E-3_wp
      filter_val3 = -0.40668222535397420851105277E-4_wp
      et0 = et0 + (x(mod_arr(-4 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-4 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-4 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-4 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.1713514567376228224101768783E-2_wp
      filter_val1 = 0.1554774474923793899293724602E-2_wp
      filter_val2 = 0.3743663199398739083068656973E-2_wp
      filter_val3 = -0.272132773540999536127987988E-2_wp
      et0 = et0 + (x(mod_arr(-3 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-3 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-3 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-3 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.8020884755969165545893465146E-2_wp
      filter_val1 = -0.13367239421126794640208314967E-1_wp
      filter_val2 = -0.22857677317437822508458981792E-1_wp
      filter_val3 = 0.28206423027424364666613729847E-1_wp
      et0 = et0 + (x(mod_arr(-2 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-2 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-2 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-2 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.3131289721448565965922638841E-1_wp
      filter_val1 = 0.43793326416155009500101924188E-1_wp
      filter_val2 = 0.77858113707232306275457825041E-1_wp
      filter_val3 = -0.119506731011354509446152089555E0_wp
      et0 = et0 + (x(mod_arr(-1 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(-1 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(-1 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(-1 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.425987982696947193304592242583E0_wp
      filter_val1 = -0.14743648317973476936220327736E-1_wp
      filter_val2 = -0.781104684144826958489597897908E0_wp
      filter_val3 = 0.43810509543421050421696718266E0_wp
      et0 = et0 + (x(mod_arr(0 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(0 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(0 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(0 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.416066936766212575361540821295E0_wp
      filter_val1 = 0.780460480250744028494513806689E0_wp
      filter_val2 = 0.49247868599583189247332328007E-1_wp
      filter_val3 = 0.403467221125547183465707078756E0_wp
      et0 = et0 + (x(mod_arr(1 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(1 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(1 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(1 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.138524775674051000848952544828E0_wp
      filter_val1 = -0.110230081678334468262552683806E0_wp
      filter_val2 = -0.39920047232423474172371930475E-1_wp
      filter_val3 = -0.52935814515665507351837970092E-1_wp
      et0 = et0 + (x(mod_arr(2 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(2 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(2 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(2 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.318354143459706681365834462E-1_wp
      filter_val1 = 0.2342461473539713439541159825E-1_wp
      filter_val2 = 0.15281781287179308494904296254E-1_wp
      filter_val3 = 0.7288836926603960462299666138E-2_wp
      et0 = et0 + (x(mod_arr(3 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(3 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(3 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(3 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.3454315576482209830827198831E-2_wp
      filter_val1 = -0.4329999533120824069665375511E-2_wp
      filter_val2 = -0.1969945431937324753826114662E-2_wp
      filter_val3 = -0.1960213959538483354691921355E-2_wp
      et0 = et0 + (x(mod_arr(4 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(4 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(4 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(4 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.3416290232098375589631534E-4_wp
      filter_val1 = 0.547310401099738937766344803E-3_wp
      filter_val2 = -0.11402461428743860613089559E-4_wp
      filter_val3 = 0.28479503466830096256502681E-3_wp
      et0 = et0 + (x(mod_arr(5 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(5 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(5 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(5 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.974884267415657697560756E-5_wp
      filter_val1 = -0.2848044279309673361077534E-4_wp
      filter_val2 = 0.5589042956883420231324354E-5_wp
      filter_val3 = -0.1716983589581506701252117E-4_wp
      et0 = et0 + (x(mod_arr(6 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(6 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(6 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(6 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.173933065619790394896889E-6_wp
      filter_val1 = -0.1557817271407449628766103E-5_wp
      filter_val2 = 0.9718640758826677836603E-7_wp
      filter_val3 = -0.877963062125028995526077E-6_wp
      et0 = et0 + (x(mod_arr(7 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(7 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(7 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(7 + i1), 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.0_wp
      filter_val1 = -0.9225014992147408201492E-8_wp
      filter_val2 = 0.0_wp
      filter_val3 = -0.5154546456362229249344E-8_wp
      et0 = et0 + (x(mod_arr(8 + i1), 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(mod_arr(8 + i1), 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(mod_arr(8 + i1), 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(mod_arr(8 + i1), 1, i2 + 0)) * (filter_val3)
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      et0 = et0 + (y(0, i1, i2 + 0)) * (a_y)
      ot0 = ot0 + (y(1, i1, i2 + 0)) * (a_y)
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
    do i1 =  -(-7), n - (8) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = -0.24646355857433108358764E-7_wp
      filter_val1 = 0.1593875388117163824756E-8_wp
      filter_val2 = 0.44109215856612868615909E-7_wp
      filter_val3 = -0.2852535033969307505679E-8_wp
      et0 = et0 + (x(-7 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-7 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-7 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-7 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.40033729064957944938397E-6_wp
      filter_val1 = 0.156915542257710856911416E-6_wp
      filter_val2 = 0.652118308986595532963584E-6_wp
      filter_val3 = -0.276667298808735176085978E-6_wp
      et0 = et0 + (x(-6 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-6 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-6 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-6 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.14147525211040058739651301E-4_wp
      filter_val1 = 0.2075567562651587632761451E-5_wp
      filter_val2 = 0.24486891695262527278480373E-4_wp
      filter_val3 = -0.3318610754044407103712689E-5_wp
      et0 = et0 + (x(-5 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-5 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-5 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-5 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.239878289281775075165492659E-3_wp
      filter_val1 = 0.25057266882581997397910903E-4_wp
      filter_val2 = -0.464504573781934008099959241E-3_wp
      filter_val3 = -0.40668222535397420851105277E-4_wp
      et0 = et0 + (x(-4 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-4 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-4 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-4 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.1713514567376228224101768783E-2_wp
      filter_val1 = 0.1554774474923793899293724602E-2_wp
      filter_val2 = 0.3743663199398739083068656973E-2_wp
      filter_val3 = -0.272132773540999536127987988E-2_wp
      et0 = et0 + (x(-3 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-3 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-3 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-3 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.8020884755969165545893465146E-2_wp
      filter_val1 = -0.13367239421126794640208314967E-1_wp
      filter_val2 = -0.22857677317437822508458981792E-1_wp
      filter_val3 = 0.28206423027424364666613729847E-1_wp
      et0 = et0 + (x(-2 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-2 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-2 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-2 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.3131289721448565965922638841E-1_wp
      filter_val1 = 0.43793326416155009500101924188E-1_wp
      filter_val2 = 0.77858113707232306275457825041E-1_wp
      filter_val3 = -0.119506731011354509446152089555E0_wp
      et0 = et0 + (x(-1 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-1 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-1 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-1 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.425987982696947193304592242583E0_wp
      filter_val1 = -0.14743648317973476936220327736E-1_wp
      filter_val2 = -0.781104684144826958489597897908E0_wp
      filter_val3 = 0.43810509543421050421696718266E0_wp
      et0 = et0 + (x(0 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(0 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(0 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(0 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.416066936766212575361540821295E0_wp
      filter_val1 = 0.780460480250744028494513806689E0_wp
      filter_val2 = 0.49247868599583189247332328007E-1_wp
      filter_val3 = 0.403467221125547183465707078756E0_wp
      et0 = et0 + (x(1 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(1 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(1 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(1 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.138524775674051000848952544828E0_wp
      filter_val1 = -0.110230081678334468262552683806E0_wp
      filter_val2 = -0.39920047232423474172371930475E-1_wp
      filter_val3 = -0.52935814515665507351837970092E-1_wp
      et0 = et0 + (x(2 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(2 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(2 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(2 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.318354143459706681365834462E-1_wp
      filter_val1 = 0.2342461473539713439541159825E-1_wp
      filter_val2 = 0.15281781287179308494904296254E-1_wp
      filter_val3 = 0.7288836926603960462299666138E-2_wp
      et0 = et0 + (x(3 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(3 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(3 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(3 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.3454315576482209830827198831E-2_wp
      filter_val1 = -0.4329999533120824069665375511E-2_wp
      filter_val2 = -0.1969945431937324753826114662E-2_wp
      filter_val3 = -0.1960213959538483354691921355E-2_wp
      et0 = et0 + (x(4 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(4 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(4 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(4 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.3416290232098375589631534E-4_wp
      filter_val1 = 0.547310401099738937766344803E-3_wp
      filter_val2 = -0.11402461428743860613089559E-4_wp
      filter_val3 = 0.28479503466830096256502681E-3_wp
      et0 = et0 + (x(5 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(5 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(5 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(5 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.974884267415657697560756E-5_wp
      filter_val1 = -0.2848044279309673361077534E-4_wp
      filter_val2 = 0.5589042956883420231324354E-5_wp
      filter_val3 = -0.1716983589581506701252117E-4_wp
      et0 = et0 + (x(6 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(6 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(6 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(6 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.173933065619790394896889E-6_wp
      filter_val1 = -0.1557817271407449628766103E-5_wp
      filter_val2 = 0.9718640758826677836603E-7_wp
      filter_val3 = -0.877963062125028995526077E-6_wp
      et0 = et0 + (x(7 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(7 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(7 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(7 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.0_wp
      filter_val1 = -0.9225014992147408201492E-8_wp
      filter_val2 = 0.0_wp
      filter_val3 = -0.5154546456362229249344E-8_wp
      et0 = et0 + (x(8 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(8 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(8 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(8 + i1, 1, i2 + 0)) * (filter_val3)
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      et0 = et0 + (y(0, i1, i2 + 0)) * (a_y)
      ot0 = ot0 + (y(1, i1, i2 + 0)) * (a_y)
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
    do i1 = n - (8), n - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = -0.24646355857433108358764E-7_wp
      filter_val1 = 0.1593875388117163824756E-8_wp
      filter_val2 = 0.44109215856612868615909E-7_wp
      filter_val3 = -0.2852535033969307505679E-8_wp
      et0 = et0 + (x(mod_arr(-7 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-7 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-7 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-7 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = -0.40033729064957944938397E-6_wp
      filter_val1 = 0.156915542257710856911416E-6_wp
      filter_val2 = 0.652118308986595532963584E-6_wp
      filter_val3 = -0.276667298808735176085978E-6_wp
      et0 = et0 + (x(mod_arr(-6 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-6 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-6 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-6 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = -0.14147525211040058739651301E-4_wp
      filter_val1 = 0.2075567562651587632761451E-5_wp
      filter_val2 = 0.24486891695262527278480373E-4_wp
      filter_val3 = -0.3318610754044407103712689E-5_wp
      et0 = et0 + (x(mod_arr(-5 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-5 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-5 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-5 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = 0.239878289281775075165492659E-3_wp
      filter_val1 = 0.25057266882581997397910903E-4_wp
      filter_val2 = -0.464504573781934008099959241E-3_wp
      filter_val3 = -0.40668222535397420851105277E-4_wp
      et0 = et0 + (x(mod_arr(-4 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-4 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-4 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-4 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = -0.1713514567376228224101768783E-2_wp
      filter_val1 = 0.1554774474923793899293724602E-2_wp
      filter_val2 = 0.3743663199398739083068656973E-2_wp
      filter_val3 = -0.272132773540999536127987988E-2_wp
      et0 = et0 + (x(mod_arr(-3 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-3 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-3 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-3 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = 0.8020884755969165545893465146E-2_wp
      filter_val1 = -0.13367239421126794640208314967E-1_wp
      filter_val2 = -0.22857677317437822508458981792E-1_wp
      filter_val3 = 0.28206423027424364666613729847E-1_wp
      et0 = et0 + (x(mod_arr(-2 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-2 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-2 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-2 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = -0.3131289721448565965922638841E-1_wp
      filter_val1 = 0.43793326416155009500101924188E-1_wp
      filter_val2 = 0.77858113707232306275457825041E-1_wp
      filter_val3 = -0.119506731011354509446152089555E0_wp
      et0 = et0 + (x(mod_arr(-1 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(mod_arr(-1 + i1 - (n)), 0, i2 + 0)) * (filter_val&
&1)
      et0 = et0 + (x(mod_arr(-1 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&2)
      ot0 = ot0 + (x(mod_arr(-1 + i1 - (n)), 1, i2 + 0)) * (filter_val&
&3)
      filter_val0 = 0.425987982696947193304592242583E0_wp
      filter_val1 = -0.14743648317973476936220327736E-1_wp
      filter_val2 = -0.781104684144826958489597897908E0_wp
      filter_val3 = 0.43810509543421050421696718266E0_wp
      et0 = et0 + (x(mod_arr(0 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(0 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(0 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(0 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = 0.416066936766212575361540821295E0_wp
      filter_val1 = 0.780460480250744028494513806689E0_wp
      filter_val2 = 0.49247868599583189247332328007E-1_wp
      filter_val3 = 0.403467221125547183465707078756E0_wp
      et0 = et0 + (x(mod_arr(1 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(1 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(1 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(1 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = -0.138524775674051000848952544828E0_wp
      filter_val1 = -0.110230081678334468262552683806E0_wp
      filter_val2 = -0.39920047232423474172371930475E-1_wp
      filter_val3 = -0.52935814515665507351837970092E-1_wp
      et0 = et0 + (x(mod_arr(2 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(2 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(2 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(2 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = 0.318354143459706681365834462E-1_wp
      filter_val1 = 0.2342461473539713439541159825E-1_wp
      filter_val2 = 0.15281781287179308494904296254E-1_wp
      filter_val3 = 0.7288836926603960462299666138E-2_wp
      et0 = et0 + (x(mod_arr(3 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(3 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(3 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(3 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = -0.3454315576482209830827198831E-2_wp
      filter_val1 = -0.4329999533120824069665375511E-2_wp
      filter_val2 = -0.1969945431937324753826114662E-2_wp
      filter_val3 = -0.1960213959538483354691921355E-2_wp
      et0 = et0 + (x(mod_arr(4 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(4 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(4 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(4 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = -0.3416290232098375589631534E-4_wp
      filter_val1 = 0.547310401099738937766344803E-3_wp
      filter_val2 = -0.11402461428743860613089559E-4_wp
      filter_val3 = 0.28479503466830096256502681E-3_wp
      et0 = et0 + (x(mod_arr(5 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(5 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(5 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(5 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = 0.974884267415657697560756E-5_wp
      filter_val1 = -0.2848044279309673361077534E-4_wp
      filter_val2 = 0.5589042956883420231324354E-5_wp
      filter_val3 = -0.1716983589581506701252117E-4_wp
      et0 = et0 + (x(mod_arr(6 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(6 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(6 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(6 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = 0.173933065619790394896889E-6_wp
      filter_val1 = -0.1557817271407449628766103E-5_wp
      filter_val2 = 0.9718640758826677836603E-7_wp
      filter_val3 = -0.877963062125028995526077E-6_wp
      et0 = et0 + (x(mod_arr(7 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(7 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(7 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(7 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      filter_val0 = 0.0_wp
      filter_val1 = -0.9225014992147408201492E-8_wp
      filter_val2 = 0.0_wp
      filter_val3 = -0.5154546456362229249344E-8_wp
      et0 = et0 + (x(mod_arr(8 + i1 - (n)), 0, i2 + 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(mod_arr(8 + i1 - (n)), 0, i2 + 0)) * (filter_val1&
&)
      et0 = et0 + (x(mod_arr(8 + i1 - (n)), 1, i2 + 0)) * (filter_val2&
&)
      ot0 = ot0 + (x(mod_arr(8 + i1 - (n)), 1, i2 + 0)) * (filter_val3&
&)
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      et0 = et0 + (y(0, i1, i2 + 0)) * (a_y)
      ot0 = ot0 + (y(1, i1, i2 + 0)) * (a_y)
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_p_10_a_ay_ld_1u1_v1_t_f_t
SUBROUTINE d_s1s0_symicomb8_p_10_a_ay_ld_1u1_v1_t_f_t_cost(n, ndat1, c&
&ost)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat1
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * (ndat1)
END SUBROUTINE d_s1s0_symicomb8_p_10_a_ay_ld_1u1_v1_t_f_t_cost
SUBROUTINE d_s1s0_symicomb8_p_01_ld_0u1_v1_t_f_t(ndat0, n, nx, ny, x, &
&y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -7
  integer(kind=4), parameter :: upfil = 8
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:ndat0 - (1), 0:nx - (1), 0:1) &
&:: x
  real(kind=8), intent(out), dimension(0:ndat0 - (1), 0:1, 0:ny - (1))&
& :: y
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_lre_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_lro_fil = resh&
&ape((/ &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp, &
0.0_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_hre_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_hro_fil = resh&
&ape((/ &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp, &
0.0_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
  integer(kind=4), dimension(-7 - (8):8 - (-7) - (1)) :: mod_arr
  do l = -7 - (8), 8 - (-7) - (1), 1
    mod_arr(l) = modulo(l, n)
  end do
!$omp parallel  default(shared) private(i1, i2, l, et0, ot0, filt&
!$omp&er_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i1 = 0, ndat0 - (1), 1
    do i2 = 0,  -(-7) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = -0.24646355857433108358764E-7_wp
      filter_val1 = 0.1593875388117163824756E-8_wp
      filter_val2 = 0.44109215856612868615909E-7_wp
      filter_val3 = -0.2852535033969307505679E-8_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-7 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-7 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-7 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-7 + i2), 1)) * (filter_val3)
      filter_val0 = -0.40033729064957944938397E-6_wp
      filter_val1 = 0.156915542257710856911416E-6_wp
      filter_val2 = 0.652118308986595532963584E-6_wp
      filter_val3 = -0.276667298808735176085978E-6_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-6 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-6 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-6 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-6 + i2), 1)) * (filter_val3)
      filter_val0 = -0.14147525211040058739651301E-4_wp
      filter_val1 = 0.2075567562651587632761451E-5_wp
      filter_val2 = 0.24486891695262527278480373E-4_wp
      filter_val3 = -0.3318610754044407103712689E-5_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-5 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-5 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-5 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-5 + i2), 1)) * (filter_val3)
      filter_val0 = 0.239878289281775075165492659E-3_wp
      filter_val1 = 0.25057266882581997397910903E-4_wp
      filter_val2 = -0.464504573781934008099959241E-3_wp
      filter_val3 = -0.40668222535397420851105277E-4_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-4 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-4 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-4 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-4 + i2), 1)) * (filter_val3)
      filter_val0 = -0.1713514567376228224101768783E-2_wp
      filter_val1 = 0.1554774474923793899293724602E-2_wp
      filter_val2 = 0.3743663199398739083068656973E-2_wp
      filter_val3 = -0.272132773540999536127987988E-2_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-3 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-3 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-3 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-3 + i2), 1)) * (filter_val3)
      filter_val0 = 0.8020884755969165545893465146E-2_wp
      filter_val1 = -0.13367239421126794640208314967E-1_wp
      filter_val2 = -0.22857677317437822508458981792E-1_wp
      filter_val3 = 0.28206423027424364666613729847E-1_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-2 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-2 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-2 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-2 + i2), 1)) * (filter_val3)
      filter_val0 = -0.3131289721448565965922638841E-1_wp
      filter_val1 = 0.43793326416155009500101924188E-1_wp
      filter_val2 = 0.77858113707232306275457825041E-1_wp
      filter_val3 = -0.119506731011354509446152089555E0_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-1 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-1 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-1 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-1 + i2), 1)) * (filter_val3)
      filter_val0 = 0.425987982696947193304592242583E0_wp
      filter_val1 = -0.14743648317973476936220327736E-1_wp
      filter_val2 = -0.781104684144826958489597897908E0_wp
      filter_val3 = 0.43810509543421050421696718266E0_wp
      et0 = et0 + (x(i1 + 0, mod_arr(0 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(0 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(0 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(0 + i2), 1)) * (filter_val3)
      filter_val0 = 0.416066936766212575361540821295E0_wp
      filter_val1 = 0.780460480250744028494513806689E0_wp
      filter_val2 = 0.49247868599583189247332328007E-1_wp
      filter_val3 = 0.403467221125547183465707078756E0_wp
      et0 = et0 + (x(i1 + 0, mod_arr(1 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(1 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(1 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(1 + i2), 1)) * (filter_val3)
      filter_val0 = -0.138524775674051000848952544828E0_wp
      filter_val1 = -0.110230081678334468262552683806E0_wp
      filter_val2 = -0.39920047232423474172371930475E-1_wp
      filter_val3 = -0.52935814515665507351837970092E-1_wp
      et0 = et0 + (x(i1 + 0, mod_arr(2 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(2 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(2 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(2 + i2), 1)) * (filter_val3)
      filter_val0 = 0.318354143459706681365834462E-1_wp
      filter_val1 = 0.2342461473539713439541159825E-1_wp
      filter_val2 = 0.15281781287179308494904296254E-1_wp
      filter_val3 = 0.7288836926603960462299666138E-2_wp
      et0 = et0 + (x(i1 + 0, mod_arr(3 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(3 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(3 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(3 + i2), 1)) * (filter_val3)
      filter_val0 = -0.3454315576482209830827198831E-2_wp
      filter_val1 = -0.4329999533120824069665375511E-2_wp
      filter_val2 = -0.1969945431937324753826114662E-2_wp
      filter_val3 = -0.1960213959538483354691921355E-2_wp
      et0 = et0 + (x(i1 + 0, mod_arr(4 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(4 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(4 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(4 + i2), 1)) * (filter_val3)
      filter_val0 = -0.3416290232098375589631534E-4_wp
      filter_val1 = 0.547310401099738937766344803E-3_wp
      filter_val2 = -0.11402461428743860613089559E-4_wp
      filter_val3 = 0.28479503466830096256502681E-3_wp
      et0 = et0 + (x(i1 + 0, mod_arr(5 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(5 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(5 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(5 + i2), 1)) * (filter_val3)
      filter_val0 = 0.974884267415657697560756E-5_wp
      filter_val1 = -0.2848044279309673361077534E-4_wp
      filter_val2 = 0.5589042956883420231324354E-5_wp
      filter_val3 = -0.1716983589581506701252117E-4_wp
      et0 = et0 + (x(i1 + 0, mod_arr(6 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(6 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(6 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(6 + i2), 1)) * (filter_val3)
      filter_val0 = 0.173933065619790394896889E-6_wp
      filter_val1 = -0.1557817271407449628766103E-5_wp
      filter_val2 = 0.9718640758826677836603E-7_wp
      filter_val3 = -0.877963062125028995526077E-6_wp
      et0 = et0 + (x(i1 + 0, mod_arr(7 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(7 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(7 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(7 + i2), 1)) * (filter_val3)
      filter_val0 = 0.0_wp
      filter_val1 = -0.9225014992147408201492E-8_wp
      filter_val2 = 0.0_wp
      filter_val3 = -0.5154546456362229249344E-8_wp
      et0 = et0 + (x(i1 + 0, mod_arr(8 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(8 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(8 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(8 + i2), 1)) * (filter_val3)
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
    do i2 =  -(-7), n - (8) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = -0.24646355857433108358764E-7_wp
      filter_val1 = 0.1593875388117163824756E-8_wp
      filter_val2 = 0.44109215856612868615909E-7_wp
      filter_val3 = -0.2852535033969307505679E-8_wp
      et0 = et0 + (x(i1 + 0, -7 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -7 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -7 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -7 + i2, 1)) * (filter_val3)
      filter_val0 = -0.40033729064957944938397E-6_wp
      filter_val1 = 0.156915542257710856911416E-6_wp
      filter_val2 = 0.652118308986595532963584E-6_wp
      filter_val3 = -0.276667298808735176085978E-6_wp
      et0 = et0 + (x(i1 + 0, -6 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -6 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -6 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -6 + i2, 1)) * (filter_val3)
      filter_val0 = -0.14147525211040058739651301E-4_wp
      filter_val1 = 0.2075567562651587632761451E-5_wp
      filter_val2 = 0.24486891695262527278480373E-4_wp
      filter_val3 = -0.3318610754044407103712689E-5_wp
      et0 = et0 + (x(i1 + 0, -5 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -5 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -5 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -5 + i2, 1)) * (filter_val3)
      filter_val0 = 0.239878289281775075165492659E-3_wp
      filter_val1 = 0.25057266882581997397910903E-4_wp
      filter_val2 = -0.464504573781934008099959241E-3_wp
      filter_val3 = -0.40668222535397420851105277E-4_wp
      et0 = et0 + (x(i1 + 0, -4 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -4 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -4 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -4 + i2, 1)) * (filter_val3)
      filter_val0 = -0.1713514567376228224101768783E-2_wp
      filter_val1 = 0.1554774474923793899293724602E-2_wp
      filter_val2 = 0.3743663199398739083068656973E-2_wp
      filter_val3 = -0.272132773540999536127987988E-2_wp
      et0 = et0 + (x(i1 + 0, -3 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -3 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -3 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -3 + i2, 1)) * (filter_val3)
      filter_val0 = 0.8020884755969165545893465146E-2_wp
      filter_val1 = -0.13367239421126794640208314967E-1_wp
      filter_val2 = -0.22857677317437822508458981792E-1_wp
      filter_val3 = 0.28206423027424364666613729847E-1_wp
      et0 = et0 + (x(i1 + 0, -2 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -2 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -2 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -2 + i2, 1)) * (filter_val3)
      filter_val0 = -0.3131289721448565965922638841E-1_wp
      filter_val1 = 0.43793326416155009500101924188E-1_wp
      filter_val2 = 0.77858113707232306275457825041E-1_wp
      filter_val3 = -0.119506731011354509446152089555E0_wp
      et0 = et0 + (x(i1 + 0, -1 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -1 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -1 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -1 + i2, 1)) * (filter_val3)
      filter_val0 = 0.425987982696947193304592242583E0_wp
      filter_val1 = -0.14743648317973476936220327736E-1_wp
      filter_val2 = -0.781104684144826958489597897908E0_wp
      filter_val3 = 0.43810509543421050421696718266E0_wp
      et0 = et0 + (x(i1 + 0, 0 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 0 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 0 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 0 + i2, 1)) * (filter_val3)
      filter_val0 = 0.416066936766212575361540821295E0_wp
      filter_val1 = 0.780460480250744028494513806689E0_wp
      filter_val2 = 0.49247868599583189247332328007E-1_wp
      filter_val3 = 0.403467221125547183465707078756E0_wp
      et0 = et0 + (x(i1 + 0, 1 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 1 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 1 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 1 + i2, 1)) * (filter_val3)
      filter_val0 = -0.138524775674051000848952544828E0_wp
      filter_val1 = -0.110230081678334468262552683806E0_wp
      filter_val2 = -0.39920047232423474172371930475E-1_wp
      filter_val3 = -0.52935814515665507351837970092E-1_wp
      et0 = et0 + (x(i1 + 0, 2 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 2 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 2 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 2 + i2, 1)) * (filter_val3)
      filter_val0 = 0.318354143459706681365834462E-1_wp
      filter_val1 = 0.2342461473539713439541159825E-1_wp
      filter_val2 = 0.15281781287179308494904296254E-1_wp
      filter_val3 = 0.7288836926603960462299666138E-2_wp
      et0 = et0 + (x(i1 + 0, 3 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 3 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 3 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 3 + i2, 1)) * (filter_val3)
      filter_val0 = -0.3454315576482209830827198831E-2_wp
      filter_val1 = -0.4329999533120824069665375511E-2_wp
      filter_val2 = -0.1969945431937324753826114662E-2_wp
      filter_val3 = -0.1960213959538483354691921355E-2_wp
      et0 = et0 + (x(i1 + 0, 4 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 4 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 4 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 4 + i2, 1)) * (filter_val3)
      filter_val0 = -0.3416290232098375589631534E-4_wp
      filter_val1 = 0.547310401099738937766344803E-3_wp
      filter_val2 = -0.11402461428743860613089559E-4_wp
      filter_val3 = 0.28479503466830096256502681E-3_wp
      et0 = et0 + (x(i1 + 0, 5 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 5 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 5 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 5 + i2, 1)) * (filter_val3)
      filter_val0 = 0.974884267415657697560756E-5_wp
      filter_val1 = -0.2848044279309673361077534E-4_wp
      filter_val2 = 0.5589042956883420231324354E-5_wp
      filter_val3 = -0.1716983589581506701252117E-4_wp
      et0 = et0 + (x(i1 + 0, 6 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 6 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 6 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 6 + i2, 1)) * (filter_val3)
      filter_val0 = 0.173933065619790394896889E-6_wp
      filter_val1 = -0.1557817271407449628766103E-5_wp
      filter_val2 = 0.9718640758826677836603E-7_wp
      filter_val3 = -0.877963062125028995526077E-6_wp
      et0 = et0 + (x(i1 + 0, 7 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 7 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 7 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 7 + i2, 1)) * (filter_val3)
      filter_val0 = 0.0_wp
      filter_val1 = -0.9225014992147408201492E-8_wp
      filter_val2 = 0.0_wp
      filter_val3 = -0.5154546456362229249344E-8_wp
      et0 = et0 + (x(i1 + 0, 8 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 8 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 8 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 8 + i2, 1)) * (filter_val3)
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
    do i2 = n - (8), n - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = -0.24646355857433108358764E-7_wp
      filter_val1 = 0.1593875388117163824756E-8_wp
      filter_val2 = 0.44109215856612868615909E-7_wp
      filter_val3 = -0.2852535033969307505679E-8_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-7 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-7 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-7 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-7 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = -0.40033729064957944938397E-6_wp
      filter_val1 = 0.156915542257710856911416E-6_wp
      filter_val2 = 0.652118308986595532963584E-6_wp
      filter_val3 = -0.276667298808735176085978E-6_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-6 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-6 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-6 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-6 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = -0.14147525211040058739651301E-4_wp
      filter_val1 = 0.2075567562651587632761451E-5_wp
      filter_val2 = 0.24486891695262527278480373E-4_wp
      filter_val3 = -0.3318610754044407103712689E-5_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-5 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-5 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-5 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-5 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = 0.239878289281775075165492659E-3_wp
      filter_val1 = 0.25057266882581997397910903E-4_wp
      filter_val2 = -0.464504573781934008099959241E-3_wp
      filter_val3 = -0.40668222535397420851105277E-4_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-4 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-4 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-4 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-4 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = -0.1713514567376228224101768783E-2_wp
      filter_val1 = 0.1554774474923793899293724602E-2_wp
      filter_val2 = 0.3743663199398739083068656973E-2_wp
      filter_val3 = -0.272132773540999536127987988E-2_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-3 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-3 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-3 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-3 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = 0.8020884755969165545893465146E-2_wp
      filter_val1 = -0.13367239421126794640208314967E-1_wp
      filter_val2 = -0.22857677317437822508458981792E-1_wp
      filter_val3 = 0.28206423027424364666613729847E-1_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-2 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-2 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-2 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-2 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = -0.3131289721448565965922638841E-1_wp
      filter_val1 = 0.43793326416155009500101924188E-1_wp
      filter_val2 = 0.77858113707232306275457825041E-1_wp
      filter_val3 = -0.119506731011354509446152089555E0_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-1 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-1 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-1 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-1 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = 0.425987982696947193304592242583E0_wp
      filter_val1 = -0.14743648317973476936220327736E-1_wp
      filter_val2 = -0.781104684144826958489597897908E0_wp
      filter_val3 = 0.43810509543421050421696718266E0_wp
      et0 = et0 + (x(i1 + 0, mod_arr(0 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(0 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(0 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(0 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = 0.416066936766212575361540821295E0_wp
      filter_val1 = 0.780460480250744028494513806689E0_wp
      filter_val2 = 0.49247868599583189247332328007E-1_wp
      filter_val3 = 0.403467221125547183465707078756E0_wp
      et0 = et0 + (x(i1 + 0, mod_arr(1 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(1 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(1 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(1 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = -0.138524775674051000848952544828E0_wp
      filter_val1 = -0.110230081678334468262552683806E0_wp
      filter_val2 = -0.39920047232423474172371930475E-1_wp
      filter_val3 = -0.52935814515665507351837970092E-1_wp
      et0 = et0 + (x(i1 + 0, mod_arr(2 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(2 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(2 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(2 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = 0.318354143459706681365834462E-1_wp
      filter_val1 = 0.2342461473539713439541159825E-1_wp
      filter_val2 = 0.15281781287179308494904296254E-1_wp
      filter_val3 = 0.7288836926603960462299666138E-2_wp
      et0 = et0 + (x(i1 + 0, mod_arr(3 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(3 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(3 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(3 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = -0.3454315576482209830827198831E-2_wp
      filter_val1 = -0.4329999533120824069665375511E-2_wp
      filter_val2 = -0.1969945431937324753826114662E-2_wp
      filter_val3 = -0.1960213959538483354691921355E-2_wp
      et0 = et0 + (x(i1 + 0, mod_arr(4 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(4 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(4 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(4 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = -0.3416290232098375589631534E-4_wp
      filter_val1 = 0.547310401099738937766344803E-3_wp
      filter_val2 = -0.11402461428743860613089559E-4_wp
      filter_val3 = 0.28479503466830096256502681E-3_wp
      et0 = et0 + (x(i1 + 0, mod_arr(5 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(5 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(5 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(5 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = 0.974884267415657697560756E-5_wp
      filter_val1 = -0.2848044279309673361077534E-4_wp
      filter_val2 = 0.5589042956883420231324354E-5_wp
      filter_val3 = -0.1716983589581506701252117E-4_wp
      et0 = et0 + (x(i1 + 0, mod_arr(6 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(6 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(6 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(6 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = 0.173933065619790394896889E-6_wp
      filter_val1 = -0.1557817271407449628766103E-5_wp
      filter_val2 = 0.9718640758826677836603E-7_wp
      filter_val3 = -0.877963062125028995526077E-6_wp
      et0 = et0 + (x(i1 + 0, mod_arr(7 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(7 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(7 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(7 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = 0.0_wp
      filter_val1 = -0.9225014992147408201492E-8_wp
      filter_val2 = 0.0_wp
      filter_val3 = -0.5154546456362229249344E-8_wp
      et0 = et0 + (x(i1 + 0, mod_arr(8 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(8 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(8 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(8 + i2 - (n)), 1)) * (filter_val3&
&)
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_p_01_ld_0u1_v1_t_f_t
SUBROUTINE d_s1s0_symicomb8_p_01_ld_0u1_v1_t_f_t_cost(ndat0, n, cost)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * (ndat0)
END SUBROUTINE d_s1s0_symicomb8_p_01_ld_0u1_v1_t_f_t_cost
SUBROUTINE d_s1s0_symicomb8_p_01_a_ld_0u1_v1_t_f_t(ndat0, n, nx, ny, x&
&, y, a)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -7
  integer(kind=4), parameter :: upfil = 8
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:ndat0 - (1), 0:nx - (1), 0:1) &
&:: x
  real(kind=8), intent(out), dimension(0:ndat0 - (1), 0:1, 0:ny - (1))&
& :: y
  real(kind=8), intent(in) :: a
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_lre_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_lro_fil = resh&
&ape((/ &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp, &
0.0_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_hre_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_hro_fil = resh&
&ape((/ &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp, &
0.0_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
  integer(kind=4), dimension(-7 - (8):8 - (-7) - (1)) :: mod_arr
  do l = -7 - (8), 8 - (-7) - (1), 1
    mod_arr(l) = modulo(l, n)
  end do
!$omp parallel  default(shared) private(i1, i2, l, et0, ot0, filt&
!$omp&er_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i1 = 0, ndat0 - (1), 1
    do i2 = 0,  -(-7) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = -0.24646355857433108358764E-7_wp
      filter_val1 = 0.1593875388117163824756E-8_wp
      filter_val2 = 0.44109215856612868615909E-7_wp
      filter_val3 = -0.2852535033969307505679E-8_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-7 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-7 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-7 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-7 + i2), 1)) * (filter_val3)
      filter_val0 = -0.40033729064957944938397E-6_wp
      filter_val1 = 0.156915542257710856911416E-6_wp
      filter_val2 = 0.652118308986595532963584E-6_wp
      filter_val3 = -0.276667298808735176085978E-6_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-6 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-6 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-6 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-6 + i2), 1)) * (filter_val3)
      filter_val0 = -0.14147525211040058739651301E-4_wp
      filter_val1 = 0.2075567562651587632761451E-5_wp
      filter_val2 = 0.24486891695262527278480373E-4_wp
      filter_val3 = -0.3318610754044407103712689E-5_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-5 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-5 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-5 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-5 + i2), 1)) * (filter_val3)
      filter_val0 = 0.239878289281775075165492659E-3_wp
      filter_val1 = 0.25057266882581997397910903E-4_wp
      filter_val2 = -0.464504573781934008099959241E-3_wp
      filter_val3 = -0.40668222535397420851105277E-4_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-4 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-4 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-4 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-4 + i2), 1)) * (filter_val3)
      filter_val0 = -0.1713514567376228224101768783E-2_wp
      filter_val1 = 0.1554774474923793899293724602E-2_wp
      filter_val2 = 0.3743663199398739083068656973E-2_wp
      filter_val3 = -0.272132773540999536127987988E-2_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-3 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-3 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-3 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-3 + i2), 1)) * (filter_val3)
      filter_val0 = 0.8020884755969165545893465146E-2_wp
      filter_val1 = -0.13367239421126794640208314967E-1_wp
      filter_val2 = -0.22857677317437822508458981792E-1_wp
      filter_val3 = 0.28206423027424364666613729847E-1_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-2 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-2 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-2 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-2 + i2), 1)) * (filter_val3)
      filter_val0 = -0.3131289721448565965922638841E-1_wp
      filter_val1 = 0.43793326416155009500101924188E-1_wp
      filter_val2 = 0.77858113707232306275457825041E-1_wp
      filter_val3 = -0.119506731011354509446152089555E0_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-1 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-1 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-1 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-1 + i2), 1)) * (filter_val3)
      filter_val0 = 0.425987982696947193304592242583E0_wp
      filter_val1 = -0.14743648317973476936220327736E-1_wp
      filter_val2 = -0.781104684144826958489597897908E0_wp
      filter_val3 = 0.43810509543421050421696718266E0_wp
      et0 = et0 + (x(i1 + 0, mod_arr(0 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(0 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(0 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(0 + i2), 1)) * (filter_val3)
      filter_val0 = 0.416066936766212575361540821295E0_wp
      filter_val1 = 0.780460480250744028494513806689E0_wp
      filter_val2 = 0.49247868599583189247332328007E-1_wp
      filter_val3 = 0.403467221125547183465707078756E0_wp
      et0 = et0 + (x(i1 + 0, mod_arr(1 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(1 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(1 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(1 + i2), 1)) * (filter_val3)
      filter_val0 = -0.138524775674051000848952544828E0_wp
      filter_val1 = -0.110230081678334468262552683806E0_wp
      filter_val2 = -0.39920047232423474172371930475E-1_wp
      filter_val3 = -0.52935814515665507351837970092E-1_wp
      et0 = et0 + (x(i1 + 0, mod_arr(2 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(2 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(2 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(2 + i2), 1)) * (filter_val3)
      filter_val0 = 0.318354143459706681365834462E-1_wp
      filter_val1 = 0.2342461473539713439541159825E-1_wp
      filter_val2 = 0.15281781287179308494904296254E-1_wp
      filter_val3 = 0.7288836926603960462299666138E-2_wp
      et0 = et0 + (x(i1 + 0, mod_arr(3 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(3 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(3 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(3 + i2), 1)) * (filter_val3)
      filter_val0 = -0.3454315576482209830827198831E-2_wp
      filter_val1 = -0.4329999533120824069665375511E-2_wp
      filter_val2 = -0.1969945431937324753826114662E-2_wp
      filter_val3 = -0.1960213959538483354691921355E-2_wp
      et0 = et0 + (x(i1 + 0, mod_arr(4 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(4 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(4 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(4 + i2), 1)) * (filter_val3)
      filter_val0 = -0.3416290232098375589631534E-4_wp
      filter_val1 = 0.547310401099738937766344803E-3_wp
      filter_val2 = -0.11402461428743860613089559E-4_wp
      filter_val3 = 0.28479503466830096256502681E-3_wp
      et0 = et0 + (x(i1 + 0, mod_arr(5 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(5 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(5 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(5 + i2), 1)) * (filter_val3)
      filter_val0 = 0.974884267415657697560756E-5_wp
      filter_val1 = -0.2848044279309673361077534E-4_wp
      filter_val2 = 0.5589042956883420231324354E-5_wp
      filter_val3 = -0.1716983589581506701252117E-4_wp
      et0 = et0 + (x(i1 + 0, mod_arr(6 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(6 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(6 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(6 + i2), 1)) * (filter_val3)
      filter_val0 = 0.173933065619790394896889E-6_wp
      filter_val1 = -0.1557817271407449628766103E-5_wp
      filter_val2 = 0.9718640758826677836603E-7_wp
      filter_val3 = -0.877963062125028995526077E-6_wp
      et0 = et0 + (x(i1 + 0, mod_arr(7 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(7 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(7 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(7 + i2), 1)) * (filter_val3)
      filter_val0 = 0.0_wp
      filter_val1 = -0.9225014992147408201492E-8_wp
      filter_val2 = 0.0_wp
      filter_val3 = -0.5154546456362229249344E-8_wp
      et0 = et0 + (x(i1 + 0, mod_arr(8 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(8 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(8 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(8 + i2), 1)) * (filter_val3)
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
    do i2 =  -(-7), n - (8) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = -0.24646355857433108358764E-7_wp
      filter_val1 = 0.1593875388117163824756E-8_wp
      filter_val2 = 0.44109215856612868615909E-7_wp
      filter_val3 = -0.2852535033969307505679E-8_wp
      et0 = et0 + (x(i1 + 0, -7 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -7 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -7 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -7 + i2, 1)) * (filter_val3)
      filter_val0 = -0.40033729064957944938397E-6_wp
      filter_val1 = 0.156915542257710856911416E-6_wp
      filter_val2 = 0.652118308986595532963584E-6_wp
      filter_val3 = -0.276667298808735176085978E-6_wp
      et0 = et0 + (x(i1 + 0, -6 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -6 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -6 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -6 + i2, 1)) * (filter_val3)
      filter_val0 = -0.14147525211040058739651301E-4_wp
      filter_val1 = 0.2075567562651587632761451E-5_wp
      filter_val2 = 0.24486891695262527278480373E-4_wp
      filter_val3 = -0.3318610754044407103712689E-5_wp
      et0 = et0 + (x(i1 + 0, -5 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -5 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -5 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -5 + i2, 1)) * (filter_val3)
      filter_val0 = 0.239878289281775075165492659E-3_wp
      filter_val1 = 0.25057266882581997397910903E-4_wp
      filter_val2 = -0.464504573781934008099959241E-3_wp
      filter_val3 = -0.40668222535397420851105277E-4_wp
      et0 = et0 + (x(i1 + 0, -4 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -4 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -4 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -4 + i2, 1)) * (filter_val3)
      filter_val0 = -0.1713514567376228224101768783E-2_wp
      filter_val1 = 0.1554774474923793899293724602E-2_wp
      filter_val2 = 0.3743663199398739083068656973E-2_wp
      filter_val3 = -0.272132773540999536127987988E-2_wp
      et0 = et0 + (x(i1 + 0, -3 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -3 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -3 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -3 + i2, 1)) * (filter_val3)
      filter_val0 = 0.8020884755969165545893465146E-2_wp
      filter_val1 = -0.13367239421126794640208314967E-1_wp
      filter_val2 = -0.22857677317437822508458981792E-1_wp
      filter_val3 = 0.28206423027424364666613729847E-1_wp
      et0 = et0 + (x(i1 + 0, -2 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -2 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -2 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -2 + i2, 1)) * (filter_val3)
      filter_val0 = -0.3131289721448565965922638841E-1_wp
      filter_val1 = 0.43793326416155009500101924188E-1_wp
      filter_val2 = 0.77858113707232306275457825041E-1_wp
      filter_val3 = -0.119506731011354509446152089555E0_wp
      et0 = et0 + (x(i1 + 0, -1 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -1 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -1 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -1 + i2, 1)) * (filter_val3)
      filter_val0 = 0.425987982696947193304592242583E0_wp
      filter_val1 = -0.14743648317973476936220327736E-1_wp
      filter_val2 = -0.781104684144826958489597897908E0_wp
      filter_val3 = 0.43810509543421050421696718266E0_wp
      et0 = et0 + (x(i1 + 0, 0 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 0 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 0 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 0 + i2, 1)) * (filter_val3)
      filter_val0 = 0.416066936766212575361540821295E0_wp
      filter_val1 = 0.780460480250744028494513806689E0_wp
      filter_val2 = 0.49247868599583189247332328007E-1_wp
      filter_val3 = 0.403467221125547183465707078756E0_wp
      et0 = et0 + (x(i1 + 0, 1 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 1 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 1 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 1 + i2, 1)) * (filter_val3)
      filter_val0 = -0.138524775674051000848952544828E0_wp
      filter_val1 = -0.110230081678334468262552683806E0_wp
      filter_val2 = -0.39920047232423474172371930475E-1_wp
      filter_val3 = -0.52935814515665507351837970092E-1_wp
      et0 = et0 + (x(i1 + 0, 2 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 2 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 2 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 2 + i2, 1)) * (filter_val3)
      filter_val0 = 0.318354143459706681365834462E-1_wp
      filter_val1 = 0.2342461473539713439541159825E-1_wp
      filter_val2 = 0.15281781287179308494904296254E-1_wp
      filter_val3 = 0.7288836926603960462299666138E-2_wp
      et0 = et0 + (x(i1 + 0, 3 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 3 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 3 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 3 + i2, 1)) * (filter_val3)
      filter_val0 = -0.3454315576482209830827198831E-2_wp
      filter_val1 = -0.4329999533120824069665375511E-2_wp
      filter_val2 = -0.1969945431937324753826114662E-2_wp
      filter_val3 = -0.1960213959538483354691921355E-2_wp
      et0 = et0 + (x(i1 + 0, 4 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 4 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 4 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 4 + i2, 1)) * (filter_val3)
      filter_val0 = -0.3416290232098375589631534E-4_wp
      filter_val1 = 0.547310401099738937766344803E-3_wp
      filter_val2 = -0.11402461428743860613089559E-4_wp
      filter_val3 = 0.28479503466830096256502681E-3_wp
      et0 = et0 + (x(i1 + 0, 5 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 5 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 5 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 5 + i2, 1)) * (filter_val3)
      filter_val0 = 0.974884267415657697560756E-5_wp
      filter_val1 = -0.2848044279309673361077534E-4_wp
      filter_val2 = 0.5589042956883420231324354E-5_wp
      filter_val3 = -0.1716983589581506701252117E-4_wp
      et0 = et0 + (x(i1 + 0, 6 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 6 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 6 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 6 + i2, 1)) * (filter_val3)
      filter_val0 = 0.173933065619790394896889E-6_wp
      filter_val1 = -0.1557817271407449628766103E-5_wp
      filter_val2 = 0.9718640758826677836603E-7_wp
      filter_val3 = -0.877963062125028995526077E-6_wp
      et0 = et0 + (x(i1 + 0, 7 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 7 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 7 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 7 + i2, 1)) * (filter_val3)
      filter_val0 = 0.0_wp
      filter_val1 = -0.9225014992147408201492E-8_wp
      filter_val2 = 0.0_wp
      filter_val3 = -0.5154546456362229249344E-8_wp
      et0 = et0 + (x(i1 + 0, 8 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 8 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 8 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 8 + i2, 1)) * (filter_val3)
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
    do i2 = n - (8), n - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = -0.24646355857433108358764E-7_wp
      filter_val1 = 0.1593875388117163824756E-8_wp
      filter_val2 = 0.44109215856612868615909E-7_wp
      filter_val3 = -0.2852535033969307505679E-8_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-7 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-7 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-7 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-7 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = -0.40033729064957944938397E-6_wp
      filter_val1 = 0.156915542257710856911416E-6_wp
      filter_val2 = 0.652118308986595532963584E-6_wp
      filter_val3 = -0.276667298808735176085978E-6_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-6 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-6 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-6 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-6 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = -0.14147525211040058739651301E-4_wp
      filter_val1 = 0.2075567562651587632761451E-5_wp
      filter_val2 = 0.24486891695262527278480373E-4_wp
      filter_val3 = -0.3318610754044407103712689E-5_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-5 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-5 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-5 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-5 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = 0.239878289281775075165492659E-3_wp
      filter_val1 = 0.25057266882581997397910903E-4_wp
      filter_val2 = -0.464504573781934008099959241E-3_wp
      filter_val3 = -0.40668222535397420851105277E-4_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-4 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-4 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-4 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-4 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = -0.1713514567376228224101768783E-2_wp
      filter_val1 = 0.1554774474923793899293724602E-2_wp
      filter_val2 = 0.3743663199398739083068656973E-2_wp
      filter_val3 = -0.272132773540999536127987988E-2_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-3 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-3 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-3 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-3 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = 0.8020884755969165545893465146E-2_wp
      filter_val1 = -0.13367239421126794640208314967E-1_wp
      filter_val2 = -0.22857677317437822508458981792E-1_wp
      filter_val3 = 0.28206423027424364666613729847E-1_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-2 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-2 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-2 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-2 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = -0.3131289721448565965922638841E-1_wp
      filter_val1 = 0.43793326416155009500101924188E-1_wp
      filter_val2 = 0.77858113707232306275457825041E-1_wp
      filter_val3 = -0.119506731011354509446152089555E0_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-1 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-1 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-1 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-1 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = 0.425987982696947193304592242583E0_wp
      filter_val1 = -0.14743648317973476936220327736E-1_wp
      filter_val2 = -0.781104684144826958489597897908E0_wp
      filter_val3 = 0.43810509543421050421696718266E0_wp
      et0 = et0 + (x(i1 + 0, mod_arr(0 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(0 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(0 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(0 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = 0.416066936766212575361540821295E0_wp
      filter_val1 = 0.780460480250744028494513806689E0_wp
      filter_val2 = 0.49247868599583189247332328007E-1_wp
      filter_val3 = 0.403467221125547183465707078756E0_wp
      et0 = et0 + (x(i1 + 0, mod_arr(1 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(1 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(1 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(1 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = -0.138524775674051000848952544828E0_wp
      filter_val1 = -0.110230081678334468262552683806E0_wp
      filter_val2 = -0.39920047232423474172371930475E-1_wp
      filter_val3 = -0.52935814515665507351837970092E-1_wp
      et0 = et0 + (x(i1 + 0, mod_arr(2 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(2 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(2 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(2 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = 0.318354143459706681365834462E-1_wp
      filter_val1 = 0.2342461473539713439541159825E-1_wp
      filter_val2 = 0.15281781287179308494904296254E-1_wp
      filter_val3 = 0.7288836926603960462299666138E-2_wp
      et0 = et0 + (x(i1 + 0, mod_arr(3 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(3 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(3 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(3 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = -0.3454315576482209830827198831E-2_wp
      filter_val1 = -0.4329999533120824069665375511E-2_wp
      filter_val2 = -0.1969945431937324753826114662E-2_wp
      filter_val3 = -0.1960213959538483354691921355E-2_wp
      et0 = et0 + (x(i1 + 0, mod_arr(4 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(4 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(4 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(4 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = -0.3416290232098375589631534E-4_wp
      filter_val1 = 0.547310401099738937766344803E-3_wp
      filter_val2 = -0.11402461428743860613089559E-4_wp
      filter_val3 = 0.28479503466830096256502681E-3_wp
      et0 = et0 + (x(i1 + 0, mod_arr(5 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(5 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(5 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(5 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = 0.974884267415657697560756E-5_wp
      filter_val1 = -0.2848044279309673361077534E-4_wp
      filter_val2 = 0.5589042956883420231324354E-5_wp
      filter_val3 = -0.1716983589581506701252117E-4_wp
      et0 = et0 + (x(i1 + 0, mod_arr(6 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(6 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(6 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(6 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = 0.173933065619790394896889E-6_wp
      filter_val1 = -0.1557817271407449628766103E-5_wp
      filter_val2 = 0.9718640758826677836603E-7_wp
      filter_val3 = -0.877963062125028995526077E-6_wp
      et0 = et0 + (x(i1 + 0, mod_arr(7 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(7 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(7 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(7 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = 0.0_wp
      filter_val1 = -0.9225014992147408201492E-8_wp
      filter_val2 = 0.0_wp
      filter_val3 = -0.5154546456362229249344E-8_wp
      et0 = et0 + (x(i1 + 0, mod_arr(8 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(8 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(8 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(8 + i2 - (n)), 1)) * (filter_val3&
&)
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_p_01_a_ld_0u1_v1_t_f_t
SUBROUTINE d_s1s0_symicomb8_p_01_a_ld_0u1_v1_t_f_t_cost(ndat0, n, cost&
&)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * (ndat0)
END SUBROUTINE d_s1s0_symicomb8_p_01_a_ld_0u1_v1_t_f_t_cost
SUBROUTINE d_s1s0_symicomb8_p_01_ay_ld_0u1_v1_t_f_t(ndat0, n, nx, ny, &
&x, y, a_y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -7
  integer(kind=4), parameter :: upfil = 8
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:ndat0 - (1), 0:nx - (1), 0:1) &
&:: x
  real(kind=8), intent(inout), dimension(0:ndat0 - (1), 0:1, 0:ny - (1&
&)) :: y
  real(kind=8), intent(in) :: a_y
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_lre_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_lro_fil = resh&
&ape((/ &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp, &
0.0_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_hre_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_hro_fil = resh&
&ape((/ &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp, &
0.0_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
  integer(kind=4), dimension(-7 - (8):8 - (-7) - (1)) :: mod_arr
  do l = -7 - (8), 8 - (-7) - (1), 1
    mod_arr(l) = modulo(l, n)
  end do
!$omp parallel  default(shared) private(i1, i2, l, et0, ot0, filt&
!$omp&er_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i1 = 0, ndat0 - (1), 1
    do i2 = 0,  -(-7) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = -0.24646355857433108358764E-7_wp
      filter_val1 = 0.1593875388117163824756E-8_wp
      filter_val2 = 0.44109215856612868615909E-7_wp
      filter_val3 = -0.2852535033969307505679E-8_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-7 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-7 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-7 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-7 + i2), 1)) * (filter_val3)
      filter_val0 = -0.40033729064957944938397E-6_wp
      filter_val1 = 0.156915542257710856911416E-6_wp
      filter_val2 = 0.652118308986595532963584E-6_wp
      filter_val3 = -0.276667298808735176085978E-6_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-6 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-6 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-6 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-6 + i2), 1)) * (filter_val3)
      filter_val0 = -0.14147525211040058739651301E-4_wp
      filter_val1 = 0.2075567562651587632761451E-5_wp
      filter_val2 = 0.24486891695262527278480373E-4_wp
      filter_val3 = -0.3318610754044407103712689E-5_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-5 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-5 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-5 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-5 + i2), 1)) * (filter_val3)
      filter_val0 = 0.239878289281775075165492659E-3_wp
      filter_val1 = 0.25057266882581997397910903E-4_wp
      filter_val2 = -0.464504573781934008099959241E-3_wp
      filter_val3 = -0.40668222535397420851105277E-4_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-4 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-4 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-4 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-4 + i2), 1)) * (filter_val3)
      filter_val0 = -0.1713514567376228224101768783E-2_wp
      filter_val1 = 0.1554774474923793899293724602E-2_wp
      filter_val2 = 0.3743663199398739083068656973E-2_wp
      filter_val3 = -0.272132773540999536127987988E-2_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-3 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-3 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-3 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-3 + i2), 1)) * (filter_val3)
      filter_val0 = 0.8020884755969165545893465146E-2_wp
      filter_val1 = -0.13367239421126794640208314967E-1_wp
      filter_val2 = -0.22857677317437822508458981792E-1_wp
      filter_val3 = 0.28206423027424364666613729847E-1_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-2 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-2 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-2 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-2 + i2), 1)) * (filter_val3)
      filter_val0 = -0.3131289721448565965922638841E-1_wp
      filter_val1 = 0.43793326416155009500101924188E-1_wp
      filter_val2 = 0.77858113707232306275457825041E-1_wp
      filter_val3 = -0.119506731011354509446152089555E0_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-1 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-1 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-1 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-1 + i2), 1)) * (filter_val3)
      filter_val0 = 0.425987982696947193304592242583E0_wp
      filter_val1 = -0.14743648317973476936220327736E-1_wp
      filter_val2 = -0.781104684144826958489597897908E0_wp
      filter_val3 = 0.43810509543421050421696718266E0_wp
      et0 = et0 + (x(i1 + 0, mod_arr(0 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(0 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(0 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(0 + i2), 1)) * (filter_val3)
      filter_val0 = 0.416066936766212575361540821295E0_wp
      filter_val1 = 0.780460480250744028494513806689E0_wp
      filter_val2 = 0.49247868599583189247332328007E-1_wp
      filter_val3 = 0.403467221125547183465707078756E0_wp
      et0 = et0 + (x(i1 + 0, mod_arr(1 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(1 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(1 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(1 + i2), 1)) * (filter_val3)
      filter_val0 = -0.138524775674051000848952544828E0_wp
      filter_val1 = -0.110230081678334468262552683806E0_wp
      filter_val2 = -0.39920047232423474172371930475E-1_wp
      filter_val3 = -0.52935814515665507351837970092E-1_wp
      et0 = et0 + (x(i1 + 0, mod_arr(2 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(2 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(2 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(2 + i2), 1)) * (filter_val3)
      filter_val0 = 0.318354143459706681365834462E-1_wp
      filter_val1 = 0.2342461473539713439541159825E-1_wp
      filter_val2 = 0.15281781287179308494904296254E-1_wp
      filter_val3 = 0.7288836926603960462299666138E-2_wp
      et0 = et0 + (x(i1 + 0, mod_arr(3 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(3 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(3 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(3 + i2), 1)) * (filter_val3)
      filter_val0 = -0.3454315576482209830827198831E-2_wp
      filter_val1 = -0.4329999533120824069665375511E-2_wp
      filter_val2 = -0.1969945431937324753826114662E-2_wp
      filter_val3 = -0.1960213959538483354691921355E-2_wp
      et0 = et0 + (x(i1 + 0, mod_arr(4 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(4 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(4 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(4 + i2), 1)) * (filter_val3)
      filter_val0 = -0.3416290232098375589631534E-4_wp
      filter_val1 = 0.547310401099738937766344803E-3_wp
      filter_val2 = -0.11402461428743860613089559E-4_wp
      filter_val3 = 0.28479503466830096256502681E-3_wp
      et0 = et0 + (x(i1 + 0, mod_arr(5 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(5 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(5 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(5 + i2), 1)) * (filter_val3)
      filter_val0 = 0.974884267415657697560756E-5_wp
      filter_val1 = -0.2848044279309673361077534E-4_wp
      filter_val2 = 0.5589042956883420231324354E-5_wp
      filter_val3 = -0.1716983589581506701252117E-4_wp
      et0 = et0 + (x(i1 + 0, mod_arr(6 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(6 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(6 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(6 + i2), 1)) * (filter_val3)
      filter_val0 = 0.173933065619790394896889E-6_wp
      filter_val1 = -0.1557817271407449628766103E-5_wp
      filter_val2 = 0.9718640758826677836603E-7_wp
      filter_val3 = -0.877963062125028995526077E-6_wp
      et0 = et0 + (x(i1 + 0, mod_arr(7 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(7 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(7 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(7 + i2), 1)) * (filter_val3)
      filter_val0 = 0.0_wp
      filter_val1 = -0.9225014992147408201492E-8_wp
      filter_val2 = 0.0_wp
      filter_val3 = -0.5154546456362229249344E-8_wp
      et0 = et0 + (x(i1 + 0, mod_arr(8 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(8 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(8 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(8 + i2), 1)) * (filter_val3)
      et0 = et0 + (y(i1 + 0, 0, i2)) * (a_y)
      ot0 = ot0 + (y(i1 + 0, 1, i2)) * (a_y)
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
    do i2 =  -(-7), n - (8) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = -0.24646355857433108358764E-7_wp
      filter_val1 = 0.1593875388117163824756E-8_wp
      filter_val2 = 0.44109215856612868615909E-7_wp
      filter_val3 = -0.2852535033969307505679E-8_wp
      et0 = et0 + (x(i1 + 0, -7 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -7 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -7 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -7 + i2, 1)) * (filter_val3)
      filter_val0 = -0.40033729064957944938397E-6_wp
      filter_val1 = 0.156915542257710856911416E-6_wp
      filter_val2 = 0.652118308986595532963584E-6_wp
      filter_val3 = -0.276667298808735176085978E-6_wp
      et0 = et0 + (x(i1 + 0, -6 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -6 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -6 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -6 + i2, 1)) * (filter_val3)
      filter_val0 = -0.14147525211040058739651301E-4_wp
      filter_val1 = 0.2075567562651587632761451E-5_wp
      filter_val2 = 0.24486891695262527278480373E-4_wp
      filter_val3 = -0.3318610754044407103712689E-5_wp
      et0 = et0 + (x(i1 + 0, -5 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -5 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -5 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -5 + i2, 1)) * (filter_val3)
      filter_val0 = 0.239878289281775075165492659E-3_wp
      filter_val1 = 0.25057266882581997397910903E-4_wp
      filter_val2 = -0.464504573781934008099959241E-3_wp
      filter_val3 = -0.40668222535397420851105277E-4_wp
      et0 = et0 + (x(i1 + 0, -4 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -4 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -4 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -4 + i2, 1)) * (filter_val3)
      filter_val0 = -0.1713514567376228224101768783E-2_wp
      filter_val1 = 0.1554774474923793899293724602E-2_wp
      filter_val2 = 0.3743663199398739083068656973E-2_wp
      filter_val3 = -0.272132773540999536127987988E-2_wp
      et0 = et0 + (x(i1 + 0, -3 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -3 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -3 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -3 + i2, 1)) * (filter_val3)
      filter_val0 = 0.8020884755969165545893465146E-2_wp
      filter_val1 = -0.13367239421126794640208314967E-1_wp
      filter_val2 = -0.22857677317437822508458981792E-1_wp
      filter_val3 = 0.28206423027424364666613729847E-1_wp
      et0 = et0 + (x(i1 + 0, -2 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -2 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -2 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -2 + i2, 1)) * (filter_val3)
      filter_val0 = -0.3131289721448565965922638841E-1_wp
      filter_val1 = 0.43793326416155009500101924188E-1_wp
      filter_val2 = 0.77858113707232306275457825041E-1_wp
      filter_val3 = -0.119506731011354509446152089555E0_wp
      et0 = et0 + (x(i1 + 0, -1 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -1 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -1 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -1 + i2, 1)) * (filter_val3)
      filter_val0 = 0.425987982696947193304592242583E0_wp
      filter_val1 = -0.14743648317973476936220327736E-1_wp
      filter_val2 = -0.781104684144826958489597897908E0_wp
      filter_val3 = 0.43810509543421050421696718266E0_wp
      et0 = et0 + (x(i1 + 0, 0 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 0 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 0 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 0 + i2, 1)) * (filter_val3)
      filter_val0 = 0.416066936766212575361540821295E0_wp
      filter_val1 = 0.780460480250744028494513806689E0_wp
      filter_val2 = 0.49247868599583189247332328007E-1_wp
      filter_val3 = 0.403467221125547183465707078756E0_wp
      et0 = et0 + (x(i1 + 0, 1 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 1 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 1 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 1 + i2, 1)) * (filter_val3)
      filter_val0 = -0.138524775674051000848952544828E0_wp
      filter_val1 = -0.110230081678334468262552683806E0_wp
      filter_val2 = -0.39920047232423474172371930475E-1_wp
      filter_val3 = -0.52935814515665507351837970092E-1_wp
      et0 = et0 + (x(i1 + 0, 2 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 2 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 2 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 2 + i2, 1)) * (filter_val3)
      filter_val0 = 0.318354143459706681365834462E-1_wp
      filter_val1 = 0.2342461473539713439541159825E-1_wp
      filter_val2 = 0.15281781287179308494904296254E-1_wp
      filter_val3 = 0.7288836926603960462299666138E-2_wp
      et0 = et0 + (x(i1 + 0, 3 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 3 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 3 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 3 + i2, 1)) * (filter_val3)
      filter_val0 = -0.3454315576482209830827198831E-2_wp
      filter_val1 = -0.4329999533120824069665375511E-2_wp
      filter_val2 = -0.1969945431937324753826114662E-2_wp
      filter_val3 = -0.1960213959538483354691921355E-2_wp
      et0 = et0 + (x(i1 + 0, 4 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 4 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 4 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 4 + i2, 1)) * (filter_val3)
      filter_val0 = -0.3416290232098375589631534E-4_wp
      filter_val1 = 0.547310401099738937766344803E-3_wp
      filter_val2 = -0.11402461428743860613089559E-4_wp
      filter_val3 = 0.28479503466830096256502681E-3_wp
      et0 = et0 + (x(i1 + 0, 5 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 5 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 5 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 5 + i2, 1)) * (filter_val3)
      filter_val0 = 0.974884267415657697560756E-5_wp
      filter_val1 = -0.2848044279309673361077534E-4_wp
      filter_val2 = 0.5589042956883420231324354E-5_wp
      filter_val3 = -0.1716983589581506701252117E-4_wp
      et0 = et0 + (x(i1 + 0, 6 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 6 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 6 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 6 + i2, 1)) * (filter_val3)
      filter_val0 = 0.173933065619790394896889E-6_wp
      filter_val1 = -0.1557817271407449628766103E-5_wp
      filter_val2 = 0.9718640758826677836603E-7_wp
      filter_val3 = -0.877963062125028995526077E-6_wp
      et0 = et0 + (x(i1 + 0, 7 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 7 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 7 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 7 + i2, 1)) * (filter_val3)
      filter_val0 = 0.0_wp
      filter_val1 = -0.9225014992147408201492E-8_wp
      filter_val2 = 0.0_wp
      filter_val3 = -0.5154546456362229249344E-8_wp
      et0 = et0 + (x(i1 + 0, 8 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 8 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 8 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 8 + i2, 1)) * (filter_val3)
      et0 = et0 + (y(i1 + 0, 0, i2)) * (a_y)
      ot0 = ot0 + (y(i1 + 0, 1, i2)) * (a_y)
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
    do i2 = n - (8), n - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = -0.24646355857433108358764E-7_wp
      filter_val1 = 0.1593875388117163824756E-8_wp
      filter_val2 = 0.44109215856612868615909E-7_wp
      filter_val3 = -0.2852535033969307505679E-8_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-7 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-7 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-7 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-7 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = -0.40033729064957944938397E-6_wp
      filter_val1 = 0.156915542257710856911416E-6_wp
      filter_val2 = 0.652118308986595532963584E-6_wp
      filter_val3 = -0.276667298808735176085978E-6_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-6 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-6 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-6 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-6 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = -0.14147525211040058739651301E-4_wp
      filter_val1 = 0.2075567562651587632761451E-5_wp
      filter_val2 = 0.24486891695262527278480373E-4_wp
      filter_val3 = -0.3318610754044407103712689E-5_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-5 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-5 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-5 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-5 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = 0.239878289281775075165492659E-3_wp
      filter_val1 = 0.25057266882581997397910903E-4_wp
      filter_val2 = -0.464504573781934008099959241E-3_wp
      filter_val3 = -0.40668222535397420851105277E-4_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-4 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-4 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-4 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-4 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = -0.1713514567376228224101768783E-2_wp
      filter_val1 = 0.1554774474923793899293724602E-2_wp
      filter_val2 = 0.3743663199398739083068656973E-2_wp
      filter_val3 = -0.272132773540999536127987988E-2_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-3 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-3 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-3 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-3 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = 0.8020884755969165545893465146E-2_wp
      filter_val1 = -0.13367239421126794640208314967E-1_wp
      filter_val2 = -0.22857677317437822508458981792E-1_wp
      filter_val3 = 0.28206423027424364666613729847E-1_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-2 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-2 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-2 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-2 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = -0.3131289721448565965922638841E-1_wp
      filter_val1 = 0.43793326416155009500101924188E-1_wp
      filter_val2 = 0.77858113707232306275457825041E-1_wp
      filter_val3 = -0.119506731011354509446152089555E0_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-1 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-1 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-1 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-1 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = 0.425987982696947193304592242583E0_wp
      filter_val1 = -0.14743648317973476936220327736E-1_wp
      filter_val2 = -0.781104684144826958489597897908E0_wp
      filter_val3 = 0.43810509543421050421696718266E0_wp
      et0 = et0 + (x(i1 + 0, mod_arr(0 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(0 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(0 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(0 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = 0.416066936766212575361540821295E0_wp
      filter_val1 = 0.780460480250744028494513806689E0_wp
      filter_val2 = 0.49247868599583189247332328007E-1_wp
      filter_val3 = 0.403467221125547183465707078756E0_wp
      et0 = et0 + (x(i1 + 0, mod_arr(1 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(1 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(1 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(1 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = -0.138524775674051000848952544828E0_wp
      filter_val1 = -0.110230081678334468262552683806E0_wp
      filter_val2 = -0.39920047232423474172371930475E-1_wp
      filter_val3 = -0.52935814515665507351837970092E-1_wp
      et0 = et0 + (x(i1 + 0, mod_arr(2 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(2 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(2 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(2 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = 0.318354143459706681365834462E-1_wp
      filter_val1 = 0.2342461473539713439541159825E-1_wp
      filter_val2 = 0.15281781287179308494904296254E-1_wp
      filter_val3 = 0.7288836926603960462299666138E-2_wp
      et0 = et0 + (x(i1 + 0, mod_arr(3 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(3 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(3 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(3 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = -0.3454315576482209830827198831E-2_wp
      filter_val1 = -0.4329999533120824069665375511E-2_wp
      filter_val2 = -0.1969945431937324753826114662E-2_wp
      filter_val3 = -0.1960213959538483354691921355E-2_wp
      et0 = et0 + (x(i1 + 0, mod_arr(4 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(4 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(4 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(4 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = -0.3416290232098375589631534E-4_wp
      filter_val1 = 0.547310401099738937766344803E-3_wp
      filter_val2 = -0.11402461428743860613089559E-4_wp
      filter_val3 = 0.28479503466830096256502681E-3_wp
      et0 = et0 + (x(i1 + 0, mod_arr(5 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(5 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(5 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(5 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = 0.974884267415657697560756E-5_wp
      filter_val1 = -0.2848044279309673361077534E-4_wp
      filter_val2 = 0.5589042956883420231324354E-5_wp
      filter_val3 = -0.1716983589581506701252117E-4_wp
      et0 = et0 + (x(i1 + 0, mod_arr(6 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(6 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(6 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(6 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = 0.173933065619790394896889E-6_wp
      filter_val1 = -0.1557817271407449628766103E-5_wp
      filter_val2 = 0.9718640758826677836603E-7_wp
      filter_val3 = -0.877963062125028995526077E-6_wp
      et0 = et0 + (x(i1 + 0, mod_arr(7 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(7 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(7 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(7 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = 0.0_wp
      filter_val1 = -0.9225014992147408201492E-8_wp
      filter_val2 = 0.0_wp
      filter_val3 = -0.5154546456362229249344E-8_wp
      et0 = et0 + (x(i1 + 0, mod_arr(8 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(8 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(8 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(8 + i2 - (n)), 1)) * (filter_val3&
&)
      et0 = et0 + (y(i1 + 0, 0, i2)) * (a_y)
      ot0 = ot0 + (y(i1 + 0, 1, i2)) * (a_y)
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_p_01_ay_ld_0u1_v1_t_f_t
SUBROUTINE d_s1s0_symicomb8_p_01_ay_ld_0u1_v1_t_f_t_cost(ndat0, n, cos&
&t)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * (ndat0)
END SUBROUTINE d_s1s0_symicomb8_p_01_ay_ld_0u1_v1_t_f_t_cost
SUBROUTINE d_s1s0_symicomb8_p_01_a_ay_ld_0u1_v1_t_f_t(ndat0, n, nx, ny&
&, x, y, a, a_y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -7
  integer(kind=4), parameter :: upfil = 8
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:ndat0 - (1), 0:nx - (1), 0:1) &
&:: x
  real(kind=8), intent(inout), dimension(0:ndat0 - (1), 0:1, 0:ny - (1&
&)) :: y
  real(kind=8), intent(in) :: a
  real(kind=8), intent(in) :: a_y
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_lre_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_lro_fil = resh&
&ape((/ &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp, &
0.0_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_hre_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_hro_fil = resh&
&ape((/ &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp, &
0.0_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
  integer(kind=4), dimension(-7 - (8):8 - (-7) - (1)) :: mod_arr
  do l = -7 - (8), 8 - (-7) - (1), 1
    mod_arr(l) = modulo(l, n)
  end do
!$omp parallel  default(shared) private(i1, i2, l, et0, ot0, filt&
!$omp&er_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i1 = 0, ndat0 - (1), 1
    do i2 = 0,  -(-7) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = -0.24646355857433108358764E-7_wp
      filter_val1 = 0.1593875388117163824756E-8_wp
      filter_val2 = 0.44109215856612868615909E-7_wp
      filter_val3 = -0.2852535033969307505679E-8_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-7 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-7 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-7 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-7 + i2), 1)) * (filter_val3)
      filter_val0 = -0.40033729064957944938397E-6_wp
      filter_val1 = 0.156915542257710856911416E-6_wp
      filter_val2 = 0.652118308986595532963584E-6_wp
      filter_val3 = -0.276667298808735176085978E-6_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-6 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-6 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-6 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-6 + i2), 1)) * (filter_val3)
      filter_val0 = -0.14147525211040058739651301E-4_wp
      filter_val1 = 0.2075567562651587632761451E-5_wp
      filter_val2 = 0.24486891695262527278480373E-4_wp
      filter_val3 = -0.3318610754044407103712689E-5_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-5 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-5 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-5 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-5 + i2), 1)) * (filter_val3)
      filter_val0 = 0.239878289281775075165492659E-3_wp
      filter_val1 = 0.25057266882581997397910903E-4_wp
      filter_val2 = -0.464504573781934008099959241E-3_wp
      filter_val3 = -0.40668222535397420851105277E-4_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-4 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-4 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-4 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-4 + i2), 1)) * (filter_val3)
      filter_val0 = -0.1713514567376228224101768783E-2_wp
      filter_val1 = 0.1554774474923793899293724602E-2_wp
      filter_val2 = 0.3743663199398739083068656973E-2_wp
      filter_val3 = -0.272132773540999536127987988E-2_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-3 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-3 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-3 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-3 + i2), 1)) * (filter_val3)
      filter_val0 = 0.8020884755969165545893465146E-2_wp
      filter_val1 = -0.13367239421126794640208314967E-1_wp
      filter_val2 = -0.22857677317437822508458981792E-1_wp
      filter_val3 = 0.28206423027424364666613729847E-1_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-2 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-2 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-2 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-2 + i2), 1)) * (filter_val3)
      filter_val0 = -0.3131289721448565965922638841E-1_wp
      filter_val1 = 0.43793326416155009500101924188E-1_wp
      filter_val2 = 0.77858113707232306275457825041E-1_wp
      filter_val3 = -0.119506731011354509446152089555E0_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-1 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-1 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(-1 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-1 + i2), 1)) * (filter_val3)
      filter_val0 = 0.425987982696947193304592242583E0_wp
      filter_val1 = -0.14743648317973476936220327736E-1_wp
      filter_val2 = -0.781104684144826958489597897908E0_wp
      filter_val3 = 0.43810509543421050421696718266E0_wp
      et0 = et0 + (x(i1 + 0, mod_arr(0 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(0 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(0 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(0 + i2), 1)) * (filter_val3)
      filter_val0 = 0.416066936766212575361540821295E0_wp
      filter_val1 = 0.780460480250744028494513806689E0_wp
      filter_val2 = 0.49247868599583189247332328007E-1_wp
      filter_val3 = 0.403467221125547183465707078756E0_wp
      et0 = et0 + (x(i1 + 0, mod_arr(1 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(1 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(1 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(1 + i2), 1)) * (filter_val3)
      filter_val0 = -0.138524775674051000848952544828E0_wp
      filter_val1 = -0.110230081678334468262552683806E0_wp
      filter_val2 = -0.39920047232423474172371930475E-1_wp
      filter_val3 = -0.52935814515665507351837970092E-1_wp
      et0 = et0 + (x(i1 + 0, mod_arr(2 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(2 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(2 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(2 + i2), 1)) * (filter_val3)
      filter_val0 = 0.318354143459706681365834462E-1_wp
      filter_val1 = 0.2342461473539713439541159825E-1_wp
      filter_val2 = 0.15281781287179308494904296254E-1_wp
      filter_val3 = 0.7288836926603960462299666138E-2_wp
      et0 = et0 + (x(i1 + 0, mod_arr(3 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(3 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(3 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(3 + i2), 1)) * (filter_val3)
      filter_val0 = -0.3454315576482209830827198831E-2_wp
      filter_val1 = -0.4329999533120824069665375511E-2_wp
      filter_val2 = -0.1969945431937324753826114662E-2_wp
      filter_val3 = -0.1960213959538483354691921355E-2_wp
      et0 = et0 + (x(i1 + 0, mod_arr(4 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(4 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(4 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(4 + i2), 1)) * (filter_val3)
      filter_val0 = -0.3416290232098375589631534E-4_wp
      filter_val1 = 0.547310401099738937766344803E-3_wp
      filter_val2 = -0.11402461428743860613089559E-4_wp
      filter_val3 = 0.28479503466830096256502681E-3_wp
      et0 = et0 + (x(i1 + 0, mod_arr(5 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(5 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(5 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(5 + i2), 1)) * (filter_val3)
      filter_val0 = 0.974884267415657697560756E-5_wp
      filter_val1 = -0.2848044279309673361077534E-4_wp
      filter_val2 = 0.5589042956883420231324354E-5_wp
      filter_val3 = -0.1716983589581506701252117E-4_wp
      et0 = et0 + (x(i1 + 0, mod_arr(6 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(6 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(6 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(6 + i2), 1)) * (filter_val3)
      filter_val0 = 0.173933065619790394896889E-6_wp
      filter_val1 = -0.1557817271407449628766103E-5_wp
      filter_val2 = 0.9718640758826677836603E-7_wp
      filter_val3 = -0.877963062125028995526077E-6_wp
      et0 = et0 + (x(i1 + 0, mod_arr(7 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(7 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(7 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(7 + i2), 1)) * (filter_val3)
      filter_val0 = 0.0_wp
      filter_val1 = -0.9225014992147408201492E-8_wp
      filter_val2 = 0.0_wp
      filter_val3 = -0.5154546456362229249344E-8_wp
      et0 = et0 + (x(i1 + 0, mod_arr(8 + i2), 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(8 + i2), 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, mod_arr(8 + i2), 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(8 + i2), 1)) * (filter_val3)
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      et0 = et0 + (y(i1 + 0, 0, i2)) * (a_y)
      ot0 = ot0 + (y(i1 + 0, 1, i2)) * (a_y)
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
    do i2 =  -(-7), n - (8) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = -0.24646355857433108358764E-7_wp
      filter_val1 = 0.1593875388117163824756E-8_wp
      filter_val2 = 0.44109215856612868615909E-7_wp
      filter_val3 = -0.2852535033969307505679E-8_wp
      et0 = et0 + (x(i1 + 0, -7 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -7 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -7 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -7 + i2, 1)) * (filter_val3)
      filter_val0 = -0.40033729064957944938397E-6_wp
      filter_val1 = 0.156915542257710856911416E-6_wp
      filter_val2 = 0.652118308986595532963584E-6_wp
      filter_val3 = -0.276667298808735176085978E-6_wp
      et0 = et0 + (x(i1 + 0, -6 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -6 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -6 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -6 + i2, 1)) * (filter_val3)
      filter_val0 = -0.14147525211040058739651301E-4_wp
      filter_val1 = 0.2075567562651587632761451E-5_wp
      filter_val2 = 0.24486891695262527278480373E-4_wp
      filter_val3 = -0.3318610754044407103712689E-5_wp
      et0 = et0 + (x(i1 + 0, -5 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -5 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -5 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -5 + i2, 1)) * (filter_val3)
      filter_val0 = 0.239878289281775075165492659E-3_wp
      filter_val1 = 0.25057266882581997397910903E-4_wp
      filter_val2 = -0.464504573781934008099959241E-3_wp
      filter_val3 = -0.40668222535397420851105277E-4_wp
      et0 = et0 + (x(i1 + 0, -4 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -4 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -4 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -4 + i2, 1)) * (filter_val3)
      filter_val0 = -0.1713514567376228224101768783E-2_wp
      filter_val1 = 0.1554774474923793899293724602E-2_wp
      filter_val2 = 0.3743663199398739083068656973E-2_wp
      filter_val3 = -0.272132773540999536127987988E-2_wp
      et0 = et0 + (x(i1 + 0, -3 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -3 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -3 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -3 + i2, 1)) * (filter_val3)
      filter_val0 = 0.8020884755969165545893465146E-2_wp
      filter_val1 = -0.13367239421126794640208314967E-1_wp
      filter_val2 = -0.22857677317437822508458981792E-1_wp
      filter_val3 = 0.28206423027424364666613729847E-1_wp
      et0 = et0 + (x(i1 + 0, -2 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -2 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -2 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -2 + i2, 1)) * (filter_val3)
      filter_val0 = -0.3131289721448565965922638841E-1_wp
      filter_val1 = 0.43793326416155009500101924188E-1_wp
      filter_val2 = 0.77858113707232306275457825041E-1_wp
      filter_val3 = -0.119506731011354509446152089555E0_wp
      et0 = et0 + (x(i1 + 0, -1 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -1 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -1 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -1 + i2, 1)) * (filter_val3)
      filter_val0 = 0.425987982696947193304592242583E0_wp
      filter_val1 = -0.14743648317973476936220327736E-1_wp
      filter_val2 = -0.781104684144826958489597897908E0_wp
      filter_val3 = 0.43810509543421050421696718266E0_wp
      et0 = et0 + (x(i1 + 0, 0 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 0 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 0 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 0 + i2, 1)) * (filter_val3)
      filter_val0 = 0.416066936766212575361540821295E0_wp
      filter_val1 = 0.780460480250744028494513806689E0_wp
      filter_val2 = 0.49247868599583189247332328007E-1_wp
      filter_val3 = 0.403467221125547183465707078756E0_wp
      et0 = et0 + (x(i1 + 0, 1 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 1 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 1 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 1 + i2, 1)) * (filter_val3)
      filter_val0 = -0.138524775674051000848952544828E0_wp
      filter_val1 = -0.110230081678334468262552683806E0_wp
      filter_val2 = -0.39920047232423474172371930475E-1_wp
      filter_val3 = -0.52935814515665507351837970092E-1_wp
      et0 = et0 + (x(i1 + 0, 2 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 2 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 2 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 2 + i2, 1)) * (filter_val3)
      filter_val0 = 0.318354143459706681365834462E-1_wp
      filter_val1 = 0.2342461473539713439541159825E-1_wp
      filter_val2 = 0.15281781287179308494904296254E-1_wp
      filter_val3 = 0.7288836926603960462299666138E-2_wp
      et0 = et0 + (x(i1 + 0, 3 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 3 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 3 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 3 + i2, 1)) * (filter_val3)
      filter_val0 = -0.3454315576482209830827198831E-2_wp
      filter_val1 = -0.4329999533120824069665375511E-2_wp
      filter_val2 = -0.1969945431937324753826114662E-2_wp
      filter_val3 = -0.1960213959538483354691921355E-2_wp
      et0 = et0 + (x(i1 + 0, 4 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 4 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 4 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 4 + i2, 1)) * (filter_val3)
      filter_val0 = -0.3416290232098375589631534E-4_wp
      filter_val1 = 0.547310401099738937766344803E-3_wp
      filter_val2 = -0.11402461428743860613089559E-4_wp
      filter_val3 = 0.28479503466830096256502681E-3_wp
      et0 = et0 + (x(i1 + 0, 5 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 5 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 5 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 5 + i2, 1)) * (filter_val3)
      filter_val0 = 0.974884267415657697560756E-5_wp
      filter_val1 = -0.2848044279309673361077534E-4_wp
      filter_val2 = 0.5589042956883420231324354E-5_wp
      filter_val3 = -0.1716983589581506701252117E-4_wp
      et0 = et0 + (x(i1 + 0, 6 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 6 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 6 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 6 + i2, 1)) * (filter_val3)
      filter_val0 = 0.173933065619790394896889E-6_wp
      filter_val1 = -0.1557817271407449628766103E-5_wp
      filter_val2 = 0.9718640758826677836603E-7_wp
      filter_val3 = -0.877963062125028995526077E-6_wp
      et0 = et0 + (x(i1 + 0, 7 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 7 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 7 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 7 + i2, 1)) * (filter_val3)
      filter_val0 = 0.0_wp
      filter_val1 = -0.9225014992147408201492E-8_wp
      filter_val2 = 0.0_wp
      filter_val3 = -0.5154546456362229249344E-8_wp
      et0 = et0 + (x(i1 + 0, 8 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 8 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 8 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 8 + i2, 1)) * (filter_val3)
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      et0 = et0 + (y(i1 + 0, 0, i2)) * (a_y)
      ot0 = ot0 + (y(i1 + 0, 1, i2)) * (a_y)
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
    do i2 = n - (8), n - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = -0.24646355857433108358764E-7_wp
      filter_val1 = 0.1593875388117163824756E-8_wp
      filter_val2 = 0.44109215856612868615909E-7_wp
      filter_val3 = -0.2852535033969307505679E-8_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-7 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-7 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-7 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-7 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = -0.40033729064957944938397E-6_wp
      filter_val1 = 0.156915542257710856911416E-6_wp
      filter_val2 = 0.652118308986595532963584E-6_wp
      filter_val3 = -0.276667298808735176085978E-6_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-6 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-6 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-6 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-6 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = -0.14147525211040058739651301E-4_wp
      filter_val1 = 0.2075567562651587632761451E-5_wp
      filter_val2 = 0.24486891695262527278480373E-4_wp
      filter_val3 = -0.3318610754044407103712689E-5_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-5 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-5 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-5 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-5 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = 0.239878289281775075165492659E-3_wp
      filter_val1 = 0.25057266882581997397910903E-4_wp
      filter_val2 = -0.464504573781934008099959241E-3_wp
      filter_val3 = -0.40668222535397420851105277E-4_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-4 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-4 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-4 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-4 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = -0.1713514567376228224101768783E-2_wp
      filter_val1 = 0.1554774474923793899293724602E-2_wp
      filter_val2 = 0.3743663199398739083068656973E-2_wp
      filter_val3 = -0.272132773540999536127987988E-2_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-3 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-3 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-3 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-3 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = 0.8020884755969165545893465146E-2_wp
      filter_val1 = -0.13367239421126794640208314967E-1_wp
      filter_val2 = -0.22857677317437822508458981792E-1_wp
      filter_val3 = 0.28206423027424364666613729847E-1_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-2 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-2 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-2 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-2 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = -0.3131289721448565965922638841E-1_wp
      filter_val1 = 0.43793326416155009500101924188E-1_wp
      filter_val2 = 0.77858113707232306275457825041E-1_wp
      filter_val3 = -0.119506731011354509446152089555E0_wp
      et0 = et0 + (x(i1 + 0, mod_arr(-1 + i2 - (n)), 0)) * (filter_val&
&0)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-1 + i2 - (n)), 0)) * (filter_val&
&1)
      et0 = et0 + (x(i1 + 0, mod_arr(-1 + i2 - (n)), 1)) * (filter_val&
&2)
      ot0 = ot0 + (x(i1 + 0, mod_arr(-1 + i2 - (n)), 1)) * (filter_val&
&3)
      filter_val0 = 0.425987982696947193304592242583E0_wp
      filter_val1 = -0.14743648317973476936220327736E-1_wp
      filter_val2 = -0.781104684144826958489597897908E0_wp
      filter_val3 = 0.43810509543421050421696718266E0_wp
      et0 = et0 + (x(i1 + 0, mod_arr(0 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(0 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(0 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(0 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = 0.416066936766212575361540821295E0_wp
      filter_val1 = 0.780460480250744028494513806689E0_wp
      filter_val2 = 0.49247868599583189247332328007E-1_wp
      filter_val3 = 0.403467221125547183465707078756E0_wp
      et0 = et0 + (x(i1 + 0, mod_arr(1 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(1 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(1 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(1 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = -0.138524775674051000848952544828E0_wp
      filter_val1 = -0.110230081678334468262552683806E0_wp
      filter_val2 = -0.39920047232423474172371930475E-1_wp
      filter_val3 = -0.52935814515665507351837970092E-1_wp
      et0 = et0 + (x(i1 + 0, mod_arr(2 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(2 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(2 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(2 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = 0.318354143459706681365834462E-1_wp
      filter_val1 = 0.2342461473539713439541159825E-1_wp
      filter_val2 = 0.15281781287179308494904296254E-1_wp
      filter_val3 = 0.7288836926603960462299666138E-2_wp
      et0 = et0 + (x(i1 + 0, mod_arr(3 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(3 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(3 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(3 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = -0.3454315576482209830827198831E-2_wp
      filter_val1 = -0.4329999533120824069665375511E-2_wp
      filter_val2 = -0.1969945431937324753826114662E-2_wp
      filter_val3 = -0.1960213959538483354691921355E-2_wp
      et0 = et0 + (x(i1 + 0, mod_arr(4 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(4 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(4 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(4 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = -0.3416290232098375589631534E-4_wp
      filter_val1 = 0.547310401099738937766344803E-3_wp
      filter_val2 = -0.11402461428743860613089559E-4_wp
      filter_val3 = 0.28479503466830096256502681E-3_wp
      et0 = et0 + (x(i1 + 0, mod_arr(5 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(5 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(5 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(5 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = 0.974884267415657697560756E-5_wp
      filter_val1 = -0.2848044279309673361077534E-4_wp
      filter_val2 = 0.5589042956883420231324354E-5_wp
      filter_val3 = -0.1716983589581506701252117E-4_wp
      et0 = et0 + (x(i1 + 0, mod_arr(6 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(6 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(6 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(6 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = 0.173933065619790394896889E-6_wp
      filter_val1 = -0.1557817271407449628766103E-5_wp
      filter_val2 = 0.9718640758826677836603E-7_wp
      filter_val3 = -0.877963062125028995526077E-6_wp
      et0 = et0 + (x(i1 + 0, mod_arr(7 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(7 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(7 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(7 + i2 - (n)), 1)) * (filter_val3&
&)
      filter_val0 = 0.0_wp
      filter_val1 = -0.9225014992147408201492E-8_wp
      filter_val2 = 0.0_wp
      filter_val3 = -0.5154546456362229249344E-8_wp
      et0 = et0 + (x(i1 + 0, mod_arr(8 + i2 - (n)), 0)) * (filter_val0&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(8 + i2 - (n)), 0)) * (filter_val1&
&)
      et0 = et0 + (x(i1 + 0, mod_arr(8 + i2 - (n)), 1)) * (filter_val2&
&)
      ot0 = ot0 + (x(i1 + 0, mod_arr(8 + i2 - (n)), 1)) * (filter_val3&
&)
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      et0 = et0 + (y(i1 + 0, 0, i2)) * (a_y)
      ot0 = ot0 + (y(i1 + 0, 1, i2)) * (a_y)
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_p_01_a_ay_ld_0u1_v1_t_f_t
SUBROUTINE d_s1s0_symicomb8_p_01_a_ay_ld_0u1_v1_t_f_t_cost(ndat0, n, c&
&ost)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * (ndat0)
END SUBROUTINE d_s1s0_symicomb8_p_01_a_ay_ld_0u1_v1_t_f_t_cost
SUBROUTINE d_s1s0_symicomb8_p_201_ld_2u1_v1_t_f_t(ndat0, n, ndat2, nx,&
& ny, x, y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -7
  integer(kind=4), parameter :: upfil = 8
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat2
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:ndat0 - (1), 0:nx - (1), 0:1, &
&0:ndat2 - (1)) :: x
  real(kind=8), intent(out), dimension(0:ndat0 - (1), 0:1, 0:ny - (1),&
& 0:ndat2 - (1)) :: y
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_lre_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_lro_fil = resh&
&ape((/ &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp, &
0.0_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_hre_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_hro_fil = resh&
&ape((/ &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp, &
0.0_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: i3
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
  integer(kind=4), dimension(-7 - (8):8 - (-7) - (1)) :: mod_arr
  do l = -7 - (8), 8 - (-7) - (1), 1
    mod_arr(l) = modulo(l, n)
  end do
!$omp parallel  default(shared) private(i1, i2, i3, l, et0, ot0, &
!$omp&filter_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i3 = 0, ndat2 - (1), 1
    do i1 = 0, ndat0 - (1), 1
      do i2 = 0,  -(-7) - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        filter_val0 = -0.24646355857433108358764E-7_wp
        filter_val1 = 0.1593875388117163824756E-8_wp
        filter_val2 = 0.44109215856612868615909E-7_wp
        filter_val3 = -0.2852535033969307505679E-8_wp
        et0 = et0 + (x(i1, mod_arr(-7 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-7 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-7 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-7 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = -0.40033729064957944938397E-6_wp
        filter_val1 = 0.156915542257710856911416E-6_wp
        filter_val2 = 0.652118308986595532963584E-6_wp
        filter_val3 = -0.276667298808735176085978E-6_wp
        et0 = et0 + (x(i1, mod_arr(-6 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-6 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-6 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-6 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = -0.14147525211040058739651301E-4_wp
        filter_val1 = 0.2075567562651587632761451E-5_wp
        filter_val2 = 0.24486891695262527278480373E-4_wp
        filter_val3 = -0.3318610754044407103712689E-5_wp
        et0 = et0 + (x(i1, mod_arr(-5 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-5 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-5 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-5 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = 0.239878289281775075165492659E-3_wp
        filter_val1 = 0.25057266882581997397910903E-4_wp
        filter_val2 = -0.464504573781934008099959241E-3_wp
        filter_val3 = -0.40668222535397420851105277E-4_wp
        et0 = et0 + (x(i1, mod_arr(-4 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-4 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-4 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-4 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = -0.1713514567376228224101768783E-2_wp
        filter_val1 = 0.1554774474923793899293724602E-2_wp
        filter_val2 = 0.3743663199398739083068656973E-2_wp
        filter_val3 = -0.272132773540999536127987988E-2_wp
        et0 = et0 + (x(i1, mod_arr(-3 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-3 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-3 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-3 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = 0.8020884755969165545893465146E-2_wp
        filter_val1 = -0.13367239421126794640208314967E-1_wp
        filter_val2 = -0.22857677317437822508458981792E-1_wp
        filter_val3 = 0.28206423027424364666613729847E-1_wp
        et0 = et0 + (x(i1, mod_arr(-2 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-2 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-2 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-2 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = -0.3131289721448565965922638841E-1_wp
        filter_val1 = 0.43793326416155009500101924188E-1_wp
        filter_val2 = 0.77858113707232306275457825041E-1_wp
        filter_val3 = -0.119506731011354509446152089555E0_wp
        et0 = et0 + (x(i1, mod_arr(-1 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-1 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-1 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-1 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = 0.425987982696947193304592242583E0_wp
        filter_val1 = -0.14743648317973476936220327736E-1_wp
        filter_val2 = -0.781104684144826958489597897908E0_wp
        filter_val3 = 0.43810509543421050421696718266E0_wp
        et0 = et0 + (x(i1, mod_arr(0 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(0 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(0 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(0 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = 0.416066936766212575361540821295E0_wp
        filter_val1 = 0.780460480250744028494513806689E0_wp
        filter_val2 = 0.49247868599583189247332328007E-1_wp
        filter_val3 = 0.403467221125547183465707078756E0_wp
        et0 = et0 + (x(i1, mod_arr(1 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(1 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(1 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(1 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = -0.138524775674051000848952544828E0_wp
        filter_val1 = -0.110230081678334468262552683806E0_wp
        filter_val2 = -0.39920047232423474172371930475E-1_wp
        filter_val3 = -0.52935814515665507351837970092E-1_wp
        et0 = et0 + (x(i1, mod_arr(2 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(2 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(2 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(2 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = 0.318354143459706681365834462E-1_wp
        filter_val1 = 0.2342461473539713439541159825E-1_wp
        filter_val2 = 0.15281781287179308494904296254E-1_wp
        filter_val3 = 0.7288836926603960462299666138E-2_wp
        et0 = et0 + (x(i1, mod_arr(3 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(3 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(3 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(3 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = -0.3454315576482209830827198831E-2_wp
        filter_val1 = -0.4329999533120824069665375511E-2_wp
        filter_val2 = -0.1969945431937324753826114662E-2_wp
        filter_val3 = -0.1960213959538483354691921355E-2_wp
        et0 = et0 + (x(i1, mod_arr(4 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(4 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(4 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(4 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = -0.3416290232098375589631534E-4_wp
        filter_val1 = 0.547310401099738937766344803E-3_wp
        filter_val2 = -0.11402461428743860613089559E-4_wp
        filter_val3 = 0.28479503466830096256502681E-3_wp
        et0 = et0 + (x(i1, mod_arr(5 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(5 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(5 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(5 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = 0.974884267415657697560756E-5_wp
        filter_val1 = -0.2848044279309673361077534E-4_wp
        filter_val2 = 0.5589042956883420231324354E-5_wp
        filter_val3 = -0.1716983589581506701252117E-4_wp
        et0 = et0 + (x(i1, mod_arr(6 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(6 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(6 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(6 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = 0.173933065619790394896889E-6_wp
        filter_val1 = -0.1557817271407449628766103E-5_wp
        filter_val2 = 0.9718640758826677836603E-7_wp
        filter_val3 = -0.877963062125028995526077E-6_wp
        et0 = et0 + (x(i1, mod_arr(7 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(7 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(7 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(7 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = 0.0_wp
        filter_val1 = -0.9225014992147408201492E-8_wp
        filter_val2 = 0.0_wp
        filter_val3 = -0.5154546456362229249344E-8_wp
        et0 = et0 + (x(i1, mod_arr(8 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(8 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(8 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(8 + i2), 1, i3 + 0)) * (filter_val3&
&)
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
      do i2 =  -(-7), n - (8) - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        filter_val0 = -0.24646355857433108358764E-7_wp
        filter_val1 = 0.1593875388117163824756E-8_wp
        filter_val2 = 0.44109215856612868615909E-7_wp
        filter_val3 = -0.2852535033969307505679E-8_wp
        et0 = et0 + (x(i1, -7 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -7 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -7 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -7 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.40033729064957944938397E-6_wp
        filter_val1 = 0.156915542257710856911416E-6_wp
        filter_val2 = 0.652118308986595532963584E-6_wp
        filter_val3 = -0.276667298808735176085978E-6_wp
        et0 = et0 + (x(i1, -6 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -6 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -6 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -6 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.14147525211040058739651301E-4_wp
        filter_val1 = 0.2075567562651587632761451E-5_wp
        filter_val2 = 0.24486891695262527278480373E-4_wp
        filter_val3 = -0.3318610754044407103712689E-5_wp
        et0 = et0 + (x(i1, -5 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -5 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -5 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -5 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.239878289281775075165492659E-3_wp
        filter_val1 = 0.25057266882581997397910903E-4_wp
        filter_val2 = -0.464504573781934008099959241E-3_wp
        filter_val3 = -0.40668222535397420851105277E-4_wp
        et0 = et0 + (x(i1, -4 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -4 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -4 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -4 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.1713514567376228224101768783E-2_wp
        filter_val1 = 0.1554774474923793899293724602E-2_wp
        filter_val2 = 0.3743663199398739083068656973E-2_wp
        filter_val3 = -0.272132773540999536127987988E-2_wp
        et0 = et0 + (x(i1, -3 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -3 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -3 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -3 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.8020884755969165545893465146E-2_wp
        filter_val1 = -0.13367239421126794640208314967E-1_wp
        filter_val2 = -0.22857677317437822508458981792E-1_wp
        filter_val3 = 0.28206423027424364666613729847E-1_wp
        et0 = et0 + (x(i1, -2 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -2 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -2 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -2 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.3131289721448565965922638841E-1_wp
        filter_val1 = 0.43793326416155009500101924188E-1_wp
        filter_val2 = 0.77858113707232306275457825041E-1_wp
        filter_val3 = -0.119506731011354509446152089555E0_wp
        et0 = et0 + (x(i1, -1 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -1 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -1 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -1 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.425987982696947193304592242583E0_wp
        filter_val1 = -0.14743648317973476936220327736E-1_wp
        filter_val2 = -0.781104684144826958489597897908E0_wp
        filter_val3 = 0.43810509543421050421696718266E0_wp
        et0 = et0 + (x(i1, 0 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 0 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 0 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 0 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.416066936766212575361540821295E0_wp
        filter_val1 = 0.780460480250744028494513806689E0_wp
        filter_val2 = 0.49247868599583189247332328007E-1_wp
        filter_val3 = 0.403467221125547183465707078756E0_wp
        et0 = et0 + (x(i1, 1 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 1 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 1 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 1 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.138524775674051000848952544828E0_wp
        filter_val1 = -0.110230081678334468262552683806E0_wp
        filter_val2 = -0.39920047232423474172371930475E-1_wp
        filter_val3 = -0.52935814515665507351837970092E-1_wp
        et0 = et0 + (x(i1, 2 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 2 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 2 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 2 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.318354143459706681365834462E-1_wp
        filter_val1 = 0.2342461473539713439541159825E-1_wp
        filter_val2 = 0.15281781287179308494904296254E-1_wp
        filter_val3 = 0.7288836926603960462299666138E-2_wp
        et0 = et0 + (x(i1, 3 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 3 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 3 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 3 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.3454315576482209830827198831E-2_wp
        filter_val1 = -0.4329999533120824069665375511E-2_wp
        filter_val2 = -0.1969945431937324753826114662E-2_wp
        filter_val3 = -0.1960213959538483354691921355E-2_wp
        et0 = et0 + (x(i1, 4 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 4 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 4 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 4 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.3416290232098375589631534E-4_wp
        filter_val1 = 0.547310401099738937766344803E-3_wp
        filter_val2 = -0.11402461428743860613089559E-4_wp
        filter_val3 = 0.28479503466830096256502681E-3_wp
        et0 = et0 + (x(i1, 5 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 5 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 5 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 5 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.974884267415657697560756E-5_wp
        filter_val1 = -0.2848044279309673361077534E-4_wp
        filter_val2 = 0.5589042956883420231324354E-5_wp
        filter_val3 = -0.1716983589581506701252117E-4_wp
        et0 = et0 + (x(i1, 6 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 6 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 6 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 6 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.173933065619790394896889E-6_wp
        filter_val1 = -0.1557817271407449628766103E-5_wp
        filter_val2 = 0.9718640758826677836603E-7_wp
        filter_val3 = -0.877963062125028995526077E-6_wp
        et0 = et0 + (x(i1, 7 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 7 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 7 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 7 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.0_wp
        filter_val1 = -0.9225014992147408201492E-8_wp
        filter_val2 = 0.0_wp
        filter_val3 = -0.5154546456362229249344E-8_wp
        et0 = et0 + (x(i1, 8 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 8 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 8 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 8 + i2, 1, i3 + 0)) * (filter_val3)
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
      do i2 = n - (8), n - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        filter_val0 = -0.24646355857433108358764E-7_wp
        filter_val1 = 0.1593875388117163824756E-8_wp
        filter_val2 = 0.44109215856612868615909E-7_wp
        filter_val3 = -0.2852535033969307505679E-8_wp
        et0 = et0 + (x(i1, mod_arr(-7 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-7 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-7 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-7 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = -0.40033729064957944938397E-6_wp
        filter_val1 = 0.156915542257710856911416E-6_wp
        filter_val2 = 0.652118308986595532963584E-6_wp
        filter_val3 = -0.276667298808735176085978E-6_wp
        et0 = et0 + (x(i1, mod_arr(-6 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-6 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-6 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-6 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = -0.14147525211040058739651301E-4_wp
        filter_val1 = 0.2075567562651587632761451E-5_wp
        filter_val2 = 0.24486891695262527278480373E-4_wp
        filter_val3 = -0.3318610754044407103712689E-5_wp
        et0 = et0 + (x(i1, mod_arr(-5 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-5 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-5 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-5 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = 0.239878289281775075165492659E-3_wp
        filter_val1 = 0.25057266882581997397910903E-4_wp
        filter_val2 = -0.464504573781934008099959241E-3_wp
        filter_val3 = -0.40668222535397420851105277E-4_wp
        et0 = et0 + (x(i1, mod_arr(-4 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-4 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-4 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-4 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = -0.1713514567376228224101768783E-2_wp
        filter_val1 = 0.1554774474923793899293724602E-2_wp
        filter_val2 = 0.3743663199398739083068656973E-2_wp
        filter_val3 = -0.272132773540999536127987988E-2_wp
        et0 = et0 + (x(i1, mod_arr(-3 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-3 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-3 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-3 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = 0.8020884755969165545893465146E-2_wp
        filter_val1 = -0.13367239421126794640208314967E-1_wp
        filter_val2 = -0.22857677317437822508458981792E-1_wp
        filter_val3 = 0.28206423027424364666613729847E-1_wp
        et0 = et0 + (x(i1, mod_arr(-2 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-2 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-2 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-2 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = -0.3131289721448565965922638841E-1_wp
        filter_val1 = 0.43793326416155009500101924188E-1_wp
        filter_val2 = 0.77858113707232306275457825041E-1_wp
        filter_val3 = -0.119506731011354509446152089555E0_wp
        et0 = et0 + (x(i1, mod_arr(-1 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-1 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-1 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-1 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = 0.425987982696947193304592242583E0_wp
        filter_val1 = -0.14743648317973476936220327736E-1_wp
        filter_val2 = -0.781104684144826958489597897908E0_wp
        filter_val3 = 0.43810509543421050421696718266E0_wp
        et0 = et0 + (x(i1, mod_arr(0 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(0 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(0 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(0 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = 0.416066936766212575361540821295E0_wp
        filter_val1 = 0.780460480250744028494513806689E0_wp
        filter_val2 = 0.49247868599583189247332328007E-1_wp
        filter_val3 = 0.403467221125547183465707078756E0_wp
        et0 = et0 + (x(i1, mod_arr(1 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(1 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(1 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(1 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = -0.138524775674051000848952544828E0_wp
        filter_val1 = -0.110230081678334468262552683806E0_wp
        filter_val2 = -0.39920047232423474172371930475E-1_wp
        filter_val3 = -0.52935814515665507351837970092E-1_wp
        et0 = et0 + (x(i1, mod_arr(2 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(2 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(2 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(2 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = 0.318354143459706681365834462E-1_wp
        filter_val1 = 0.2342461473539713439541159825E-1_wp
        filter_val2 = 0.15281781287179308494904296254E-1_wp
        filter_val3 = 0.7288836926603960462299666138E-2_wp
        et0 = et0 + (x(i1, mod_arr(3 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(3 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(3 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(3 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = -0.3454315576482209830827198831E-2_wp
        filter_val1 = -0.4329999533120824069665375511E-2_wp
        filter_val2 = -0.1969945431937324753826114662E-2_wp
        filter_val3 = -0.1960213959538483354691921355E-2_wp
        et0 = et0 + (x(i1, mod_arr(4 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(4 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(4 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(4 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = -0.3416290232098375589631534E-4_wp
        filter_val1 = 0.547310401099738937766344803E-3_wp
        filter_val2 = -0.11402461428743860613089559E-4_wp
        filter_val3 = 0.28479503466830096256502681E-3_wp
        et0 = et0 + (x(i1, mod_arr(5 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(5 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(5 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(5 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = 0.974884267415657697560756E-5_wp
        filter_val1 = -0.2848044279309673361077534E-4_wp
        filter_val2 = 0.5589042956883420231324354E-5_wp
        filter_val3 = -0.1716983589581506701252117E-4_wp
        et0 = et0 + (x(i1, mod_arr(6 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(6 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(6 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(6 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = 0.173933065619790394896889E-6_wp
        filter_val1 = -0.1557817271407449628766103E-5_wp
        filter_val2 = 0.9718640758826677836603E-7_wp
        filter_val3 = -0.877963062125028995526077E-6_wp
        et0 = et0 + (x(i1, mod_arr(7 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(7 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(7 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(7 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = 0.0_wp
        filter_val1 = -0.9225014992147408201492E-8_wp
        filter_val2 = 0.0_wp
        filter_val3 = -0.5154546456362229249344E-8_wp
        et0 = et0 + (x(i1, mod_arr(8 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(8 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(8 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(8 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_p_201_ld_2u1_v1_t_f_t
SUBROUTINE d_s1s0_symicomb8_p_201_ld_2u1_v1_t_f_t_cost(ndat0, n, ndat2&
&, cost)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat2
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * ((ndat2) * (ndat0))
END SUBROUTINE d_s1s0_symicomb8_p_201_ld_2u1_v1_t_f_t_cost
SUBROUTINE d_s1s0_symicomb8_p_201_a_ld_2u1_v1_t_f_t(ndat0, n, ndat2, n&
&x, ny, x, y, a)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -7
  integer(kind=4), parameter :: upfil = 8
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat2
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:ndat0 - (1), 0:nx - (1), 0:1, &
&0:ndat2 - (1)) :: x
  real(kind=8), intent(out), dimension(0:ndat0 - (1), 0:1, 0:ny - (1),&
& 0:ndat2 - (1)) :: y
  real(kind=8), intent(in) :: a
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_lre_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_lro_fil = resh&
&ape((/ &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp, &
0.0_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_hre_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_hro_fil = resh&
&ape((/ &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp, &
0.0_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: i3
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
  integer(kind=4), dimension(-7 - (8):8 - (-7) - (1)) :: mod_arr
  do l = -7 - (8), 8 - (-7) - (1), 1
    mod_arr(l) = modulo(l, n)
  end do
!$omp parallel  default(shared) private(i1, i2, i3, l, et0, ot0, &
!$omp&filter_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i3 = 0, ndat2 - (1), 1
    do i1 = 0, ndat0 - (1), 1
      do i2 = 0,  -(-7) - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        filter_val0 = -0.24646355857433108358764E-7_wp
        filter_val1 = 0.1593875388117163824756E-8_wp
        filter_val2 = 0.44109215856612868615909E-7_wp
        filter_val3 = -0.2852535033969307505679E-8_wp
        et0 = et0 + (x(i1, mod_arr(-7 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-7 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-7 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-7 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = -0.40033729064957944938397E-6_wp
        filter_val1 = 0.156915542257710856911416E-6_wp
        filter_val2 = 0.652118308986595532963584E-6_wp
        filter_val3 = -0.276667298808735176085978E-6_wp
        et0 = et0 + (x(i1, mod_arr(-6 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-6 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-6 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-6 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = -0.14147525211040058739651301E-4_wp
        filter_val1 = 0.2075567562651587632761451E-5_wp
        filter_val2 = 0.24486891695262527278480373E-4_wp
        filter_val3 = -0.3318610754044407103712689E-5_wp
        et0 = et0 + (x(i1, mod_arr(-5 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-5 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-5 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-5 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = 0.239878289281775075165492659E-3_wp
        filter_val1 = 0.25057266882581997397910903E-4_wp
        filter_val2 = -0.464504573781934008099959241E-3_wp
        filter_val3 = -0.40668222535397420851105277E-4_wp
        et0 = et0 + (x(i1, mod_arr(-4 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-4 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-4 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-4 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = -0.1713514567376228224101768783E-2_wp
        filter_val1 = 0.1554774474923793899293724602E-2_wp
        filter_val2 = 0.3743663199398739083068656973E-2_wp
        filter_val3 = -0.272132773540999536127987988E-2_wp
        et0 = et0 + (x(i1, mod_arr(-3 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-3 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-3 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-3 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = 0.8020884755969165545893465146E-2_wp
        filter_val1 = -0.13367239421126794640208314967E-1_wp
        filter_val2 = -0.22857677317437822508458981792E-1_wp
        filter_val3 = 0.28206423027424364666613729847E-1_wp
        et0 = et0 + (x(i1, mod_arr(-2 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-2 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-2 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-2 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = -0.3131289721448565965922638841E-1_wp
        filter_val1 = 0.43793326416155009500101924188E-1_wp
        filter_val2 = 0.77858113707232306275457825041E-1_wp
        filter_val3 = -0.119506731011354509446152089555E0_wp
        et0 = et0 + (x(i1, mod_arr(-1 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-1 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-1 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-1 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = 0.425987982696947193304592242583E0_wp
        filter_val1 = -0.14743648317973476936220327736E-1_wp
        filter_val2 = -0.781104684144826958489597897908E0_wp
        filter_val3 = 0.43810509543421050421696718266E0_wp
        et0 = et0 + (x(i1, mod_arr(0 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(0 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(0 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(0 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = 0.416066936766212575361540821295E0_wp
        filter_val1 = 0.780460480250744028494513806689E0_wp
        filter_val2 = 0.49247868599583189247332328007E-1_wp
        filter_val3 = 0.403467221125547183465707078756E0_wp
        et0 = et0 + (x(i1, mod_arr(1 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(1 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(1 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(1 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = -0.138524775674051000848952544828E0_wp
        filter_val1 = -0.110230081678334468262552683806E0_wp
        filter_val2 = -0.39920047232423474172371930475E-1_wp
        filter_val3 = -0.52935814515665507351837970092E-1_wp
        et0 = et0 + (x(i1, mod_arr(2 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(2 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(2 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(2 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = 0.318354143459706681365834462E-1_wp
        filter_val1 = 0.2342461473539713439541159825E-1_wp
        filter_val2 = 0.15281781287179308494904296254E-1_wp
        filter_val3 = 0.7288836926603960462299666138E-2_wp
        et0 = et0 + (x(i1, mod_arr(3 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(3 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(3 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(3 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = -0.3454315576482209830827198831E-2_wp
        filter_val1 = -0.4329999533120824069665375511E-2_wp
        filter_val2 = -0.1969945431937324753826114662E-2_wp
        filter_val3 = -0.1960213959538483354691921355E-2_wp
        et0 = et0 + (x(i1, mod_arr(4 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(4 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(4 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(4 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = -0.3416290232098375589631534E-4_wp
        filter_val1 = 0.547310401099738937766344803E-3_wp
        filter_val2 = -0.11402461428743860613089559E-4_wp
        filter_val3 = 0.28479503466830096256502681E-3_wp
        et0 = et0 + (x(i1, mod_arr(5 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(5 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(5 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(5 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = 0.974884267415657697560756E-5_wp
        filter_val1 = -0.2848044279309673361077534E-4_wp
        filter_val2 = 0.5589042956883420231324354E-5_wp
        filter_val3 = -0.1716983589581506701252117E-4_wp
        et0 = et0 + (x(i1, mod_arr(6 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(6 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(6 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(6 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = 0.173933065619790394896889E-6_wp
        filter_val1 = -0.1557817271407449628766103E-5_wp
        filter_val2 = 0.9718640758826677836603E-7_wp
        filter_val3 = -0.877963062125028995526077E-6_wp
        et0 = et0 + (x(i1, mod_arr(7 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(7 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(7 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(7 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = 0.0_wp
        filter_val1 = -0.9225014992147408201492E-8_wp
        filter_val2 = 0.0_wp
        filter_val3 = -0.5154546456362229249344E-8_wp
        et0 = et0 + (x(i1, mod_arr(8 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(8 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(8 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(8 + i2), 1, i3 + 0)) * (filter_val3&
&)
        et0 = (et0) * (a)
        ot0 = (ot0) * (a)
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
      do i2 =  -(-7), n - (8) - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        filter_val0 = -0.24646355857433108358764E-7_wp
        filter_val1 = 0.1593875388117163824756E-8_wp
        filter_val2 = 0.44109215856612868615909E-7_wp
        filter_val3 = -0.2852535033969307505679E-8_wp
        et0 = et0 + (x(i1, -7 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -7 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -7 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -7 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.40033729064957944938397E-6_wp
        filter_val1 = 0.156915542257710856911416E-6_wp
        filter_val2 = 0.652118308986595532963584E-6_wp
        filter_val3 = -0.276667298808735176085978E-6_wp
        et0 = et0 + (x(i1, -6 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -6 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -6 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -6 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.14147525211040058739651301E-4_wp
        filter_val1 = 0.2075567562651587632761451E-5_wp
        filter_val2 = 0.24486891695262527278480373E-4_wp
        filter_val3 = -0.3318610754044407103712689E-5_wp
        et0 = et0 + (x(i1, -5 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -5 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -5 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -5 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.239878289281775075165492659E-3_wp
        filter_val1 = 0.25057266882581997397910903E-4_wp
        filter_val2 = -0.464504573781934008099959241E-3_wp
        filter_val3 = -0.40668222535397420851105277E-4_wp
        et0 = et0 + (x(i1, -4 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -4 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -4 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -4 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.1713514567376228224101768783E-2_wp
        filter_val1 = 0.1554774474923793899293724602E-2_wp
        filter_val2 = 0.3743663199398739083068656973E-2_wp
        filter_val3 = -0.272132773540999536127987988E-2_wp
        et0 = et0 + (x(i1, -3 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -3 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -3 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -3 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.8020884755969165545893465146E-2_wp
        filter_val1 = -0.13367239421126794640208314967E-1_wp
        filter_val2 = -0.22857677317437822508458981792E-1_wp
        filter_val3 = 0.28206423027424364666613729847E-1_wp
        et0 = et0 + (x(i1, -2 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -2 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -2 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -2 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.3131289721448565965922638841E-1_wp
        filter_val1 = 0.43793326416155009500101924188E-1_wp
        filter_val2 = 0.77858113707232306275457825041E-1_wp
        filter_val3 = -0.119506731011354509446152089555E0_wp
        et0 = et0 + (x(i1, -1 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -1 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -1 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -1 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.425987982696947193304592242583E0_wp
        filter_val1 = -0.14743648317973476936220327736E-1_wp
        filter_val2 = -0.781104684144826958489597897908E0_wp
        filter_val3 = 0.43810509543421050421696718266E0_wp
        et0 = et0 + (x(i1, 0 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 0 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 0 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 0 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.416066936766212575361540821295E0_wp
        filter_val1 = 0.780460480250744028494513806689E0_wp
        filter_val2 = 0.49247868599583189247332328007E-1_wp
        filter_val3 = 0.403467221125547183465707078756E0_wp
        et0 = et0 + (x(i1, 1 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 1 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 1 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 1 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.138524775674051000848952544828E0_wp
        filter_val1 = -0.110230081678334468262552683806E0_wp
        filter_val2 = -0.39920047232423474172371930475E-1_wp
        filter_val3 = -0.52935814515665507351837970092E-1_wp
        et0 = et0 + (x(i1, 2 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 2 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 2 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 2 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.318354143459706681365834462E-1_wp
        filter_val1 = 0.2342461473539713439541159825E-1_wp
        filter_val2 = 0.15281781287179308494904296254E-1_wp
        filter_val3 = 0.7288836926603960462299666138E-2_wp
        et0 = et0 + (x(i1, 3 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 3 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 3 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 3 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.3454315576482209830827198831E-2_wp
        filter_val1 = -0.4329999533120824069665375511E-2_wp
        filter_val2 = -0.1969945431937324753826114662E-2_wp
        filter_val3 = -0.1960213959538483354691921355E-2_wp
        et0 = et0 + (x(i1, 4 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 4 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 4 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 4 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.3416290232098375589631534E-4_wp
        filter_val1 = 0.547310401099738937766344803E-3_wp
        filter_val2 = -0.11402461428743860613089559E-4_wp
        filter_val3 = 0.28479503466830096256502681E-3_wp
        et0 = et0 + (x(i1, 5 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 5 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 5 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 5 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.974884267415657697560756E-5_wp
        filter_val1 = -0.2848044279309673361077534E-4_wp
        filter_val2 = 0.5589042956883420231324354E-5_wp
        filter_val3 = -0.1716983589581506701252117E-4_wp
        et0 = et0 + (x(i1, 6 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 6 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 6 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 6 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.173933065619790394896889E-6_wp
        filter_val1 = -0.1557817271407449628766103E-5_wp
        filter_val2 = 0.9718640758826677836603E-7_wp
        filter_val3 = -0.877963062125028995526077E-6_wp
        et0 = et0 + (x(i1, 7 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 7 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 7 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 7 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.0_wp
        filter_val1 = -0.9225014992147408201492E-8_wp
        filter_val2 = 0.0_wp
        filter_val3 = -0.5154546456362229249344E-8_wp
        et0 = et0 + (x(i1, 8 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 8 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 8 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 8 + i2, 1, i3 + 0)) * (filter_val3)
        et0 = (et0) * (a)
        ot0 = (ot0) * (a)
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
      do i2 = n - (8), n - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        filter_val0 = -0.24646355857433108358764E-7_wp
        filter_val1 = 0.1593875388117163824756E-8_wp
        filter_val2 = 0.44109215856612868615909E-7_wp
        filter_val3 = -0.2852535033969307505679E-8_wp
        et0 = et0 + (x(i1, mod_arr(-7 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-7 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-7 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-7 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = -0.40033729064957944938397E-6_wp
        filter_val1 = 0.156915542257710856911416E-6_wp
        filter_val2 = 0.652118308986595532963584E-6_wp
        filter_val3 = -0.276667298808735176085978E-6_wp
        et0 = et0 + (x(i1, mod_arr(-6 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-6 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-6 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-6 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = -0.14147525211040058739651301E-4_wp
        filter_val1 = 0.2075567562651587632761451E-5_wp
        filter_val2 = 0.24486891695262527278480373E-4_wp
        filter_val3 = -0.3318610754044407103712689E-5_wp
        et0 = et0 + (x(i1, mod_arr(-5 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-5 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-5 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-5 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = 0.239878289281775075165492659E-3_wp
        filter_val1 = 0.25057266882581997397910903E-4_wp
        filter_val2 = -0.464504573781934008099959241E-3_wp
        filter_val3 = -0.40668222535397420851105277E-4_wp
        et0 = et0 + (x(i1, mod_arr(-4 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-4 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-4 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-4 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = -0.1713514567376228224101768783E-2_wp
        filter_val1 = 0.1554774474923793899293724602E-2_wp
        filter_val2 = 0.3743663199398739083068656973E-2_wp
        filter_val3 = -0.272132773540999536127987988E-2_wp
        et0 = et0 + (x(i1, mod_arr(-3 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-3 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-3 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-3 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = 0.8020884755969165545893465146E-2_wp
        filter_val1 = -0.13367239421126794640208314967E-1_wp
        filter_val2 = -0.22857677317437822508458981792E-1_wp
        filter_val3 = 0.28206423027424364666613729847E-1_wp
        et0 = et0 + (x(i1, mod_arr(-2 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-2 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-2 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-2 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = -0.3131289721448565965922638841E-1_wp
        filter_val1 = 0.43793326416155009500101924188E-1_wp
        filter_val2 = 0.77858113707232306275457825041E-1_wp
        filter_val3 = -0.119506731011354509446152089555E0_wp
        et0 = et0 + (x(i1, mod_arr(-1 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-1 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-1 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-1 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = 0.425987982696947193304592242583E0_wp
        filter_val1 = -0.14743648317973476936220327736E-1_wp
        filter_val2 = -0.781104684144826958489597897908E0_wp
        filter_val3 = 0.43810509543421050421696718266E0_wp
        et0 = et0 + (x(i1, mod_arr(0 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(0 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(0 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(0 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = 0.416066936766212575361540821295E0_wp
        filter_val1 = 0.780460480250744028494513806689E0_wp
        filter_val2 = 0.49247868599583189247332328007E-1_wp
        filter_val3 = 0.403467221125547183465707078756E0_wp
        et0 = et0 + (x(i1, mod_arr(1 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(1 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(1 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(1 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = -0.138524775674051000848952544828E0_wp
        filter_val1 = -0.110230081678334468262552683806E0_wp
        filter_val2 = -0.39920047232423474172371930475E-1_wp
        filter_val3 = -0.52935814515665507351837970092E-1_wp
        et0 = et0 + (x(i1, mod_arr(2 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(2 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(2 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(2 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = 0.318354143459706681365834462E-1_wp
        filter_val1 = 0.2342461473539713439541159825E-1_wp
        filter_val2 = 0.15281781287179308494904296254E-1_wp
        filter_val3 = 0.7288836926603960462299666138E-2_wp
        et0 = et0 + (x(i1, mod_arr(3 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(3 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(3 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(3 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = -0.3454315576482209830827198831E-2_wp
        filter_val1 = -0.4329999533120824069665375511E-2_wp
        filter_val2 = -0.1969945431937324753826114662E-2_wp
        filter_val3 = -0.1960213959538483354691921355E-2_wp
        et0 = et0 + (x(i1, mod_arr(4 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(4 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(4 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(4 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = -0.3416290232098375589631534E-4_wp
        filter_val1 = 0.547310401099738937766344803E-3_wp
        filter_val2 = -0.11402461428743860613089559E-4_wp
        filter_val3 = 0.28479503466830096256502681E-3_wp
        et0 = et0 + (x(i1, mod_arr(5 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(5 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(5 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(5 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = 0.974884267415657697560756E-5_wp
        filter_val1 = -0.2848044279309673361077534E-4_wp
        filter_val2 = 0.5589042956883420231324354E-5_wp
        filter_val3 = -0.1716983589581506701252117E-4_wp
        et0 = et0 + (x(i1, mod_arr(6 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(6 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(6 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(6 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = 0.173933065619790394896889E-6_wp
        filter_val1 = -0.1557817271407449628766103E-5_wp
        filter_val2 = 0.9718640758826677836603E-7_wp
        filter_val3 = -0.877963062125028995526077E-6_wp
        et0 = et0 + (x(i1, mod_arr(7 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(7 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(7 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(7 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = 0.0_wp
        filter_val1 = -0.9225014992147408201492E-8_wp
        filter_val2 = 0.0_wp
        filter_val3 = -0.5154546456362229249344E-8_wp
        et0 = et0 + (x(i1, mod_arr(8 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(8 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(8 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(8 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        et0 = (et0) * (a)
        ot0 = (ot0) * (a)
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_p_201_a_ld_2u1_v1_t_f_t
SUBROUTINE d_s1s0_symicomb8_p_201_a_ld_2u1_v1_t_f_t_cost(ndat0, n, nda&
&t2, cost)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat2
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * ((ndat2) * (ndat0))
END SUBROUTINE d_s1s0_symicomb8_p_201_a_ld_2u1_v1_t_f_t_cost
SUBROUTINE d_s1s0_symicomb8_p_201_ay_ld_2u1_v1_t_f_t(ndat0, n, ndat2, &
&nx, ny, x, y, a_y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -7
  integer(kind=4), parameter :: upfil = 8
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat2
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:ndat0 - (1), 0:nx - (1), 0:1, &
&0:ndat2 - (1)) :: x
  real(kind=8), intent(inout), dimension(0:ndat0 - (1), 0:1, 0:ny - (1&
&), 0:ndat2 - (1)) :: y
  real(kind=8), intent(in) :: a_y
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_lre_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_lro_fil = resh&
&ape((/ &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp, &
0.0_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_hre_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_hro_fil = resh&
&ape((/ &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp, &
0.0_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: i3
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
  integer(kind=4), dimension(-7 - (8):8 - (-7) - (1)) :: mod_arr
  do l = -7 - (8), 8 - (-7) - (1), 1
    mod_arr(l) = modulo(l, n)
  end do
!$omp parallel  default(shared) private(i1, i2, i3, l, et0, ot0, &
!$omp&filter_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i3 = 0, ndat2 - (1), 1
    do i1 = 0, ndat0 - (1), 1
      do i2 = 0,  -(-7) - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        filter_val0 = -0.24646355857433108358764E-7_wp
        filter_val1 = 0.1593875388117163824756E-8_wp
        filter_val2 = 0.44109215856612868615909E-7_wp
        filter_val3 = -0.2852535033969307505679E-8_wp
        et0 = et0 + (x(i1, mod_arr(-7 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-7 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-7 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-7 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = -0.40033729064957944938397E-6_wp
        filter_val1 = 0.156915542257710856911416E-6_wp
        filter_val2 = 0.652118308986595532963584E-6_wp
        filter_val3 = -0.276667298808735176085978E-6_wp
        et0 = et0 + (x(i1, mod_arr(-6 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-6 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-6 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-6 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = -0.14147525211040058739651301E-4_wp
        filter_val1 = 0.2075567562651587632761451E-5_wp
        filter_val2 = 0.24486891695262527278480373E-4_wp
        filter_val3 = -0.3318610754044407103712689E-5_wp
        et0 = et0 + (x(i1, mod_arr(-5 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-5 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-5 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-5 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = 0.239878289281775075165492659E-3_wp
        filter_val1 = 0.25057266882581997397910903E-4_wp
        filter_val2 = -0.464504573781934008099959241E-3_wp
        filter_val3 = -0.40668222535397420851105277E-4_wp
        et0 = et0 + (x(i1, mod_arr(-4 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-4 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-4 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-4 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = -0.1713514567376228224101768783E-2_wp
        filter_val1 = 0.1554774474923793899293724602E-2_wp
        filter_val2 = 0.3743663199398739083068656973E-2_wp
        filter_val3 = -0.272132773540999536127987988E-2_wp
        et0 = et0 + (x(i1, mod_arr(-3 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-3 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-3 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-3 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = 0.8020884755969165545893465146E-2_wp
        filter_val1 = -0.13367239421126794640208314967E-1_wp
        filter_val2 = -0.22857677317437822508458981792E-1_wp
        filter_val3 = 0.28206423027424364666613729847E-1_wp
        et0 = et0 + (x(i1, mod_arr(-2 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-2 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-2 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-2 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = -0.3131289721448565965922638841E-1_wp
        filter_val1 = 0.43793326416155009500101924188E-1_wp
        filter_val2 = 0.77858113707232306275457825041E-1_wp
        filter_val3 = -0.119506731011354509446152089555E0_wp
        et0 = et0 + (x(i1, mod_arr(-1 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-1 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-1 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-1 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = 0.425987982696947193304592242583E0_wp
        filter_val1 = -0.14743648317973476936220327736E-1_wp
        filter_val2 = -0.781104684144826958489597897908E0_wp
        filter_val3 = 0.43810509543421050421696718266E0_wp
        et0 = et0 + (x(i1, mod_arr(0 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(0 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(0 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(0 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = 0.416066936766212575361540821295E0_wp
        filter_val1 = 0.780460480250744028494513806689E0_wp
        filter_val2 = 0.49247868599583189247332328007E-1_wp
        filter_val3 = 0.403467221125547183465707078756E0_wp
        et0 = et0 + (x(i1, mod_arr(1 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(1 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(1 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(1 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = -0.138524775674051000848952544828E0_wp
        filter_val1 = -0.110230081678334468262552683806E0_wp
        filter_val2 = -0.39920047232423474172371930475E-1_wp
        filter_val3 = -0.52935814515665507351837970092E-1_wp
        et0 = et0 + (x(i1, mod_arr(2 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(2 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(2 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(2 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = 0.318354143459706681365834462E-1_wp
        filter_val1 = 0.2342461473539713439541159825E-1_wp
        filter_val2 = 0.15281781287179308494904296254E-1_wp
        filter_val3 = 0.7288836926603960462299666138E-2_wp
        et0 = et0 + (x(i1, mod_arr(3 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(3 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(3 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(3 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = -0.3454315576482209830827198831E-2_wp
        filter_val1 = -0.4329999533120824069665375511E-2_wp
        filter_val2 = -0.1969945431937324753826114662E-2_wp
        filter_val3 = -0.1960213959538483354691921355E-2_wp
        et0 = et0 + (x(i1, mod_arr(4 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(4 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(4 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(4 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = -0.3416290232098375589631534E-4_wp
        filter_val1 = 0.547310401099738937766344803E-3_wp
        filter_val2 = -0.11402461428743860613089559E-4_wp
        filter_val3 = 0.28479503466830096256502681E-3_wp
        et0 = et0 + (x(i1, mod_arr(5 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(5 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(5 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(5 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = 0.974884267415657697560756E-5_wp
        filter_val1 = -0.2848044279309673361077534E-4_wp
        filter_val2 = 0.5589042956883420231324354E-5_wp
        filter_val3 = -0.1716983589581506701252117E-4_wp
        et0 = et0 + (x(i1, mod_arr(6 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(6 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(6 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(6 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = 0.173933065619790394896889E-6_wp
        filter_val1 = -0.1557817271407449628766103E-5_wp
        filter_val2 = 0.9718640758826677836603E-7_wp
        filter_val3 = -0.877963062125028995526077E-6_wp
        et0 = et0 + (x(i1, mod_arr(7 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(7 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(7 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(7 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = 0.0_wp
        filter_val1 = -0.9225014992147408201492E-8_wp
        filter_val2 = 0.0_wp
        filter_val3 = -0.5154546456362229249344E-8_wp
        et0 = et0 + (x(i1, mod_arr(8 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(8 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(8 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(8 + i2), 1, i3 + 0)) * (filter_val3&
&)
        et0 = et0 + (y(i1, 0, i2, i3 + 0)) * (a_y)
        ot0 = ot0 + (y(i1, 1, i2, i3 + 0)) * (a_y)
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
      do i2 =  -(-7), n - (8) - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        filter_val0 = -0.24646355857433108358764E-7_wp
        filter_val1 = 0.1593875388117163824756E-8_wp
        filter_val2 = 0.44109215856612868615909E-7_wp
        filter_val3 = -0.2852535033969307505679E-8_wp
        et0 = et0 + (x(i1, -7 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -7 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -7 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -7 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.40033729064957944938397E-6_wp
        filter_val1 = 0.156915542257710856911416E-6_wp
        filter_val2 = 0.652118308986595532963584E-6_wp
        filter_val3 = -0.276667298808735176085978E-6_wp
        et0 = et0 + (x(i1, -6 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -6 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -6 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -6 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.14147525211040058739651301E-4_wp
        filter_val1 = 0.2075567562651587632761451E-5_wp
        filter_val2 = 0.24486891695262527278480373E-4_wp
        filter_val3 = -0.3318610754044407103712689E-5_wp
        et0 = et0 + (x(i1, -5 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -5 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -5 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -5 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.239878289281775075165492659E-3_wp
        filter_val1 = 0.25057266882581997397910903E-4_wp
        filter_val2 = -0.464504573781934008099959241E-3_wp
        filter_val3 = -0.40668222535397420851105277E-4_wp
        et0 = et0 + (x(i1, -4 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -4 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -4 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -4 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.1713514567376228224101768783E-2_wp
        filter_val1 = 0.1554774474923793899293724602E-2_wp
        filter_val2 = 0.3743663199398739083068656973E-2_wp
        filter_val3 = -0.272132773540999536127987988E-2_wp
        et0 = et0 + (x(i1, -3 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -3 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -3 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -3 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.8020884755969165545893465146E-2_wp
        filter_val1 = -0.13367239421126794640208314967E-1_wp
        filter_val2 = -0.22857677317437822508458981792E-1_wp
        filter_val3 = 0.28206423027424364666613729847E-1_wp
        et0 = et0 + (x(i1, -2 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -2 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -2 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -2 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.3131289721448565965922638841E-1_wp
        filter_val1 = 0.43793326416155009500101924188E-1_wp
        filter_val2 = 0.77858113707232306275457825041E-1_wp
        filter_val3 = -0.119506731011354509446152089555E0_wp
        et0 = et0 + (x(i1, -1 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -1 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -1 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -1 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.425987982696947193304592242583E0_wp
        filter_val1 = -0.14743648317973476936220327736E-1_wp
        filter_val2 = -0.781104684144826958489597897908E0_wp
        filter_val3 = 0.43810509543421050421696718266E0_wp
        et0 = et0 + (x(i1, 0 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 0 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 0 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 0 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.416066936766212575361540821295E0_wp
        filter_val1 = 0.780460480250744028494513806689E0_wp
        filter_val2 = 0.49247868599583189247332328007E-1_wp
        filter_val3 = 0.403467221125547183465707078756E0_wp
        et0 = et0 + (x(i1, 1 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 1 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 1 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 1 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.138524775674051000848952544828E0_wp
        filter_val1 = -0.110230081678334468262552683806E0_wp
        filter_val2 = -0.39920047232423474172371930475E-1_wp
        filter_val3 = -0.52935814515665507351837970092E-1_wp
        et0 = et0 + (x(i1, 2 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 2 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 2 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 2 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.318354143459706681365834462E-1_wp
        filter_val1 = 0.2342461473539713439541159825E-1_wp
        filter_val2 = 0.15281781287179308494904296254E-1_wp
        filter_val3 = 0.7288836926603960462299666138E-2_wp
        et0 = et0 + (x(i1, 3 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 3 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 3 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 3 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.3454315576482209830827198831E-2_wp
        filter_val1 = -0.4329999533120824069665375511E-2_wp
        filter_val2 = -0.1969945431937324753826114662E-2_wp
        filter_val3 = -0.1960213959538483354691921355E-2_wp
        et0 = et0 + (x(i1, 4 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 4 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 4 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 4 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.3416290232098375589631534E-4_wp
        filter_val1 = 0.547310401099738937766344803E-3_wp
        filter_val2 = -0.11402461428743860613089559E-4_wp
        filter_val3 = 0.28479503466830096256502681E-3_wp
        et0 = et0 + (x(i1, 5 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 5 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 5 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 5 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.974884267415657697560756E-5_wp
        filter_val1 = -0.2848044279309673361077534E-4_wp
        filter_val2 = 0.5589042956883420231324354E-5_wp
        filter_val3 = -0.1716983589581506701252117E-4_wp
        et0 = et0 + (x(i1, 6 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 6 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 6 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 6 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.173933065619790394896889E-6_wp
        filter_val1 = -0.1557817271407449628766103E-5_wp
        filter_val2 = 0.9718640758826677836603E-7_wp
        filter_val3 = -0.877963062125028995526077E-6_wp
        et0 = et0 + (x(i1, 7 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 7 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 7 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 7 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.0_wp
        filter_val1 = -0.9225014992147408201492E-8_wp
        filter_val2 = 0.0_wp
        filter_val3 = -0.5154546456362229249344E-8_wp
        et0 = et0 + (x(i1, 8 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 8 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 8 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 8 + i2, 1, i3 + 0)) * (filter_val3)
        et0 = et0 + (y(i1, 0, i2, i3 + 0)) * (a_y)
        ot0 = ot0 + (y(i1, 1, i2, i3 + 0)) * (a_y)
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
      do i2 = n - (8), n - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        filter_val0 = -0.24646355857433108358764E-7_wp
        filter_val1 = 0.1593875388117163824756E-8_wp
        filter_val2 = 0.44109215856612868615909E-7_wp
        filter_val3 = -0.2852535033969307505679E-8_wp
        et0 = et0 + (x(i1, mod_arr(-7 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-7 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-7 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-7 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = -0.40033729064957944938397E-6_wp
        filter_val1 = 0.156915542257710856911416E-6_wp
        filter_val2 = 0.652118308986595532963584E-6_wp
        filter_val3 = -0.276667298808735176085978E-6_wp
        et0 = et0 + (x(i1, mod_arr(-6 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-6 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-6 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-6 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = -0.14147525211040058739651301E-4_wp
        filter_val1 = 0.2075567562651587632761451E-5_wp
        filter_val2 = 0.24486891695262527278480373E-4_wp
        filter_val3 = -0.3318610754044407103712689E-5_wp
        et0 = et0 + (x(i1, mod_arr(-5 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-5 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-5 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-5 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = 0.239878289281775075165492659E-3_wp
        filter_val1 = 0.25057266882581997397910903E-4_wp
        filter_val2 = -0.464504573781934008099959241E-3_wp
        filter_val3 = -0.40668222535397420851105277E-4_wp
        et0 = et0 + (x(i1, mod_arr(-4 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-4 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-4 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-4 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = -0.1713514567376228224101768783E-2_wp
        filter_val1 = 0.1554774474923793899293724602E-2_wp
        filter_val2 = 0.3743663199398739083068656973E-2_wp
        filter_val3 = -0.272132773540999536127987988E-2_wp
        et0 = et0 + (x(i1, mod_arr(-3 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-3 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-3 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-3 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = 0.8020884755969165545893465146E-2_wp
        filter_val1 = -0.13367239421126794640208314967E-1_wp
        filter_val2 = -0.22857677317437822508458981792E-1_wp
        filter_val3 = 0.28206423027424364666613729847E-1_wp
        et0 = et0 + (x(i1, mod_arr(-2 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-2 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-2 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-2 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = -0.3131289721448565965922638841E-1_wp
        filter_val1 = 0.43793326416155009500101924188E-1_wp
        filter_val2 = 0.77858113707232306275457825041E-1_wp
        filter_val3 = -0.119506731011354509446152089555E0_wp
        et0 = et0 + (x(i1, mod_arr(-1 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-1 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-1 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-1 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = 0.425987982696947193304592242583E0_wp
        filter_val1 = -0.14743648317973476936220327736E-1_wp
        filter_val2 = -0.781104684144826958489597897908E0_wp
        filter_val3 = 0.43810509543421050421696718266E0_wp
        et0 = et0 + (x(i1, mod_arr(0 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(0 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(0 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(0 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = 0.416066936766212575361540821295E0_wp
        filter_val1 = 0.780460480250744028494513806689E0_wp
        filter_val2 = 0.49247868599583189247332328007E-1_wp
        filter_val3 = 0.403467221125547183465707078756E0_wp
        et0 = et0 + (x(i1, mod_arr(1 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(1 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(1 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(1 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = -0.138524775674051000848952544828E0_wp
        filter_val1 = -0.110230081678334468262552683806E0_wp
        filter_val2 = -0.39920047232423474172371930475E-1_wp
        filter_val3 = -0.52935814515665507351837970092E-1_wp
        et0 = et0 + (x(i1, mod_arr(2 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(2 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(2 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(2 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = 0.318354143459706681365834462E-1_wp
        filter_val1 = 0.2342461473539713439541159825E-1_wp
        filter_val2 = 0.15281781287179308494904296254E-1_wp
        filter_val3 = 0.7288836926603960462299666138E-2_wp
        et0 = et0 + (x(i1, mod_arr(3 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(3 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(3 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(3 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = -0.3454315576482209830827198831E-2_wp
        filter_val1 = -0.4329999533120824069665375511E-2_wp
        filter_val2 = -0.1969945431937324753826114662E-2_wp
        filter_val3 = -0.1960213959538483354691921355E-2_wp
        et0 = et0 + (x(i1, mod_arr(4 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(4 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(4 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(4 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = -0.3416290232098375589631534E-4_wp
        filter_val1 = 0.547310401099738937766344803E-3_wp
        filter_val2 = -0.11402461428743860613089559E-4_wp
        filter_val3 = 0.28479503466830096256502681E-3_wp
        et0 = et0 + (x(i1, mod_arr(5 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(5 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(5 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(5 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = 0.974884267415657697560756E-5_wp
        filter_val1 = -0.2848044279309673361077534E-4_wp
        filter_val2 = 0.5589042956883420231324354E-5_wp
        filter_val3 = -0.1716983589581506701252117E-4_wp
        et0 = et0 + (x(i1, mod_arr(6 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(6 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(6 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(6 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = 0.173933065619790394896889E-6_wp
        filter_val1 = -0.1557817271407449628766103E-5_wp
        filter_val2 = 0.9718640758826677836603E-7_wp
        filter_val3 = -0.877963062125028995526077E-6_wp
        et0 = et0 + (x(i1, mod_arr(7 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(7 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(7 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(7 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = 0.0_wp
        filter_val1 = -0.9225014992147408201492E-8_wp
        filter_val2 = 0.0_wp
        filter_val3 = -0.5154546456362229249344E-8_wp
        et0 = et0 + (x(i1, mod_arr(8 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(8 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(8 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(8 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        et0 = et0 + (y(i1, 0, i2, i3 + 0)) * (a_y)
        ot0 = ot0 + (y(i1, 1, i2, i3 + 0)) * (a_y)
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_p_201_ay_ld_2u1_v1_t_f_t
SUBROUTINE d_s1s0_symicomb8_p_201_ay_ld_2u1_v1_t_f_t_cost(ndat0, n, nd&
&at2, cost)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat2
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * ((ndat2) * (ndat0))
END SUBROUTINE d_s1s0_symicomb8_p_201_ay_ld_2u1_v1_t_f_t_cost
SUBROUTINE d_s1s0_symicomb8_p_201_a_ay_ld_2u1_v1_t_f_t(ndat0, n, ndat2&
&, nx, ny, x, y, a, a_y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -7
  integer(kind=4), parameter :: upfil = 8
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat2
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:ndat0 - (1), 0:nx - (1), 0:1, &
&0:ndat2 - (1)) :: x
  real(kind=8), intent(inout), dimension(0:ndat0 - (1), 0:1, 0:ny - (1&
&), 0:ndat2 - (1)) :: y
  real(kind=8), intent(in) :: a
  real(kind=8), intent(in) :: a_y
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_lre_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_lro_fil = resh&
&ape((/ &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp, &
0.0_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_hre_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-7:8) :: symicomb8_hro_fil = resh&
&ape((/ &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp, &
0.0_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: i3
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
  integer(kind=4), dimension(-7 - (8):8 - (-7) - (1)) :: mod_arr
  do l = -7 - (8), 8 - (-7) - (1), 1
    mod_arr(l) = modulo(l, n)
  end do
!$omp parallel  default(shared) private(i1, i2, i3, l, et0, ot0, &
!$omp&filter_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i3 = 0, ndat2 - (1), 1
    do i1 = 0, ndat0 - (1), 1
      do i2 = 0,  -(-7) - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        filter_val0 = -0.24646355857433108358764E-7_wp
        filter_val1 = 0.1593875388117163824756E-8_wp
        filter_val2 = 0.44109215856612868615909E-7_wp
        filter_val3 = -0.2852535033969307505679E-8_wp
        et0 = et0 + (x(i1, mod_arr(-7 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-7 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-7 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-7 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = -0.40033729064957944938397E-6_wp
        filter_val1 = 0.156915542257710856911416E-6_wp
        filter_val2 = 0.652118308986595532963584E-6_wp
        filter_val3 = -0.276667298808735176085978E-6_wp
        et0 = et0 + (x(i1, mod_arr(-6 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-6 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-6 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-6 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = -0.14147525211040058739651301E-4_wp
        filter_val1 = 0.2075567562651587632761451E-5_wp
        filter_val2 = 0.24486891695262527278480373E-4_wp
        filter_val3 = -0.3318610754044407103712689E-5_wp
        et0 = et0 + (x(i1, mod_arr(-5 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-5 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-5 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-5 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = 0.239878289281775075165492659E-3_wp
        filter_val1 = 0.25057266882581997397910903E-4_wp
        filter_val2 = -0.464504573781934008099959241E-3_wp
        filter_val3 = -0.40668222535397420851105277E-4_wp
        et0 = et0 + (x(i1, mod_arr(-4 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-4 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-4 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-4 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = -0.1713514567376228224101768783E-2_wp
        filter_val1 = 0.1554774474923793899293724602E-2_wp
        filter_val2 = 0.3743663199398739083068656973E-2_wp
        filter_val3 = -0.272132773540999536127987988E-2_wp
        et0 = et0 + (x(i1, mod_arr(-3 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-3 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-3 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-3 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = 0.8020884755969165545893465146E-2_wp
        filter_val1 = -0.13367239421126794640208314967E-1_wp
        filter_val2 = -0.22857677317437822508458981792E-1_wp
        filter_val3 = 0.28206423027424364666613729847E-1_wp
        et0 = et0 + (x(i1, mod_arr(-2 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-2 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-2 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-2 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = -0.3131289721448565965922638841E-1_wp
        filter_val1 = 0.43793326416155009500101924188E-1_wp
        filter_val2 = 0.77858113707232306275457825041E-1_wp
        filter_val3 = -0.119506731011354509446152089555E0_wp
        et0 = et0 + (x(i1, mod_arr(-1 + i2), 0, i3 + 0)) * (filter_val&
&0)
        ot0 = ot0 + (x(i1, mod_arr(-1 + i2), 0, i3 + 0)) * (filter_val&
&1)
        et0 = et0 + (x(i1, mod_arr(-1 + i2), 1, i3 + 0)) * (filter_val&
&2)
        ot0 = ot0 + (x(i1, mod_arr(-1 + i2), 1, i3 + 0)) * (filter_val&
&3)
        filter_val0 = 0.425987982696947193304592242583E0_wp
        filter_val1 = -0.14743648317973476936220327736E-1_wp
        filter_val2 = -0.781104684144826958489597897908E0_wp
        filter_val3 = 0.43810509543421050421696718266E0_wp
        et0 = et0 + (x(i1, mod_arr(0 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(0 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(0 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(0 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = 0.416066936766212575361540821295E0_wp
        filter_val1 = 0.780460480250744028494513806689E0_wp
        filter_val2 = 0.49247868599583189247332328007E-1_wp
        filter_val3 = 0.403467221125547183465707078756E0_wp
        et0 = et0 + (x(i1, mod_arr(1 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(1 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(1 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(1 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = -0.138524775674051000848952544828E0_wp
        filter_val1 = -0.110230081678334468262552683806E0_wp
        filter_val2 = -0.39920047232423474172371930475E-1_wp
        filter_val3 = -0.52935814515665507351837970092E-1_wp
        et0 = et0 + (x(i1, mod_arr(2 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(2 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(2 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(2 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = 0.318354143459706681365834462E-1_wp
        filter_val1 = 0.2342461473539713439541159825E-1_wp
        filter_val2 = 0.15281781287179308494904296254E-1_wp
        filter_val3 = 0.7288836926603960462299666138E-2_wp
        et0 = et0 + (x(i1, mod_arr(3 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(3 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(3 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(3 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = -0.3454315576482209830827198831E-2_wp
        filter_val1 = -0.4329999533120824069665375511E-2_wp
        filter_val2 = -0.1969945431937324753826114662E-2_wp
        filter_val3 = -0.1960213959538483354691921355E-2_wp
        et0 = et0 + (x(i1, mod_arr(4 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(4 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(4 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(4 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = -0.3416290232098375589631534E-4_wp
        filter_val1 = 0.547310401099738937766344803E-3_wp
        filter_val2 = -0.11402461428743860613089559E-4_wp
        filter_val3 = 0.28479503466830096256502681E-3_wp
        et0 = et0 + (x(i1, mod_arr(5 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(5 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(5 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(5 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = 0.974884267415657697560756E-5_wp
        filter_val1 = -0.2848044279309673361077534E-4_wp
        filter_val2 = 0.5589042956883420231324354E-5_wp
        filter_val3 = -0.1716983589581506701252117E-4_wp
        et0 = et0 + (x(i1, mod_arr(6 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(6 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(6 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(6 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = 0.173933065619790394896889E-6_wp
        filter_val1 = -0.1557817271407449628766103E-5_wp
        filter_val2 = 0.9718640758826677836603E-7_wp
        filter_val3 = -0.877963062125028995526077E-6_wp
        et0 = et0 + (x(i1, mod_arr(7 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(7 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(7 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(7 + i2), 1, i3 + 0)) * (filter_val3&
&)
        filter_val0 = 0.0_wp
        filter_val1 = -0.9225014992147408201492E-8_wp
        filter_val2 = 0.0_wp
        filter_val3 = -0.5154546456362229249344E-8_wp
        et0 = et0 + (x(i1, mod_arr(8 + i2), 0, i3 + 0)) * (filter_val0&
&)
        ot0 = ot0 + (x(i1, mod_arr(8 + i2), 0, i3 + 0)) * (filter_val1&
&)
        et0 = et0 + (x(i1, mod_arr(8 + i2), 1, i3 + 0)) * (filter_val2&
&)
        ot0 = ot0 + (x(i1, mod_arr(8 + i2), 1, i3 + 0)) * (filter_val3&
&)
        et0 = (et0) * (a)
        ot0 = (ot0) * (a)
        et0 = et0 + (y(i1, 0, i2, i3 + 0)) * (a_y)
        ot0 = ot0 + (y(i1, 1, i2, i3 + 0)) * (a_y)
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
      do i2 =  -(-7), n - (8) - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        filter_val0 = -0.24646355857433108358764E-7_wp
        filter_val1 = 0.1593875388117163824756E-8_wp
        filter_val2 = 0.44109215856612868615909E-7_wp
        filter_val3 = -0.2852535033969307505679E-8_wp
        et0 = et0 + (x(i1, -7 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -7 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -7 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -7 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.40033729064957944938397E-6_wp
        filter_val1 = 0.156915542257710856911416E-6_wp
        filter_val2 = 0.652118308986595532963584E-6_wp
        filter_val3 = -0.276667298808735176085978E-6_wp
        et0 = et0 + (x(i1, -6 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -6 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -6 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -6 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.14147525211040058739651301E-4_wp
        filter_val1 = 0.2075567562651587632761451E-5_wp
        filter_val2 = 0.24486891695262527278480373E-4_wp
        filter_val3 = -0.3318610754044407103712689E-5_wp
        et0 = et0 + (x(i1, -5 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -5 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -5 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -5 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.239878289281775075165492659E-3_wp
        filter_val1 = 0.25057266882581997397910903E-4_wp
        filter_val2 = -0.464504573781934008099959241E-3_wp
        filter_val3 = -0.40668222535397420851105277E-4_wp
        et0 = et0 + (x(i1, -4 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -4 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -4 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -4 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.1713514567376228224101768783E-2_wp
        filter_val1 = 0.1554774474923793899293724602E-2_wp
        filter_val2 = 0.3743663199398739083068656973E-2_wp
        filter_val3 = -0.272132773540999536127987988E-2_wp
        et0 = et0 + (x(i1, -3 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -3 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -3 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -3 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.8020884755969165545893465146E-2_wp
        filter_val1 = -0.13367239421126794640208314967E-1_wp
        filter_val2 = -0.22857677317437822508458981792E-1_wp
        filter_val3 = 0.28206423027424364666613729847E-1_wp
        et0 = et0 + (x(i1, -2 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -2 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -2 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -2 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.3131289721448565965922638841E-1_wp
        filter_val1 = 0.43793326416155009500101924188E-1_wp
        filter_val2 = 0.77858113707232306275457825041E-1_wp
        filter_val3 = -0.119506731011354509446152089555E0_wp
        et0 = et0 + (x(i1, -1 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -1 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -1 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -1 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.425987982696947193304592242583E0_wp
        filter_val1 = -0.14743648317973476936220327736E-1_wp
        filter_val2 = -0.781104684144826958489597897908E0_wp
        filter_val3 = 0.43810509543421050421696718266E0_wp
        et0 = et0 + (x(i1, 0 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 0 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 0 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 0 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.416066936766212575361540821295E0_wp
        filter_val1 = 0.780460480250744028494513806689E0_wp
        filter_val2 = 0.49247868599583189247332328007E-1_wp
        filter_val3 = 0.403467221125547183465707078756E0_wp
        et0 = et0 + (x(i1, 1 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 1 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 1 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 1 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.138524775674051000848952544828E0_wp
        filter_val1 = -0.110230081678334468262552683806E0_wp
        filter_val2 = -0.39920047232423474172371930475E-1_wp
        filter_val3 = -0.52935814515665507351837970092E-1_wp
        et0 = et0 + (x(i1, 2 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 2 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 2 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 2 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.318354143459706681365834462E-1_wp
        filter_val1 = 0.2342461473539713439541159825E-1_wp
        filter_val2 = 0.15281781287179308494904296254E-1_wp
        filter_val3 = 0.7288836926603960462299666138E-2_wp
        et0 = et0 + (x(i1, 3 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 3 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 3 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 3 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.3454315576482209830827198831E-2_wp
        filter_val1 = -0.4329999533120824069665375511E-2_wp
        filter_val2 = -0.1969945431937324753826114662E-2_wp
        filter_val3 = -0.1960213959538483354691921355E-2_wp
        et0 = et0 + (x(i1, 4 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 4 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 4 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 4 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.3416290232098375589631534E-4_wp
        filter_val1 = 0.547310401099738937766344803E-3_wp
        filter_val2 = -0.11402461428743860613089559E-4_wp
        filter_val3 = 0.28479503466830096256502681E-3_wp
        et0 = et0 + (x(i1, 5 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 5 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 5 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 5 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.974884267415657697560756E-5_wp
        filter_val1 = -0.2848044279309673361077534E-4_wp
        filter_val2 = 0.5589042956883420231324354E-5_wp
        filter_val3 = -0.1716983589581506701252117E-4_wp
        et0 = et0 + (x(i1, 6 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 6 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 6 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 6 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.173933065619790394896889E-6_wp
        filter_val1 = -0.1557817271407449628766103E-5_wp
        filter_val2 = 0.9718640758826677836603E-7_wp
        filter_val3 = -0.877963062125028995526077E-6_wp
        et0 = et0 + (x(i1, 7 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 7 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 7 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 7 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.0_wp
        filter_val1 = -0.9225014992147408201492E-8_wp
        filter_val2 = 0.0_wp
        filter_val3 = -0.5154546456362229249344E-8_wp
        et0 = et0 + (x(i1, 8 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 8 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 8 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 8 + i2, 1, i3 + 0)) * (filter_val3)
        et0 = (et0) * (a)
        ot0 = (ot0) * (a)
        et0 = et0 + (y(i1, 0, i2, i3 + 0)) * (a_y)
        ot0 = ot0 + (y(i1, 1, i2, i3 + 0)) * (a_y)
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
      do i2 = n - (8), n - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        filter_val0 = -0.24646355857433108358764E-7_wp
        filter_val1 = 0.1593875388117163824756E-8_wp
        filter_val2 = 0.44109215856612868615909E-7_wp
        filter_val3 = -0.2852535033969307505679E-8_wp
        et0 = et0 + (x(i1, mod_arr(-7 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-7 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-7 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-7 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = -0.40033729064957944938397E-6_wp
        filter_val1 = 0.156915542257710856911416E-6_wp
        filter_val2 = 0.652118308986595532963584E-6_wp
        filter_val3 = -0.276667298808735176085978E-6_wp
        et0 = et0 + (x(i1, mod_arr(-6 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-6 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-6 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-6 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = -0.14147525211040058739651301E-4_wp
        filter_val1 = 0.2075567562651587632761451E-5_wp
        filter_val2 = 0.24486891695262527278480373E-4_wp
        filter_val3 = -0.3318610754044407103712689E-5_wp
        et0 = et0 + (x(i1, mod_arr(-5 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-5 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-5 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-5 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = 0.239878289281775075165492659E-3_wp
        filter_val1 = 0.25057266882581997397910903E-4_wp
        filter_val2 = -0.464504573781934008099959241E-3_wp
        filter_val3 = -0.40668222535397420851105277E-4_wp
        et0 = et0 + (x(i1, mod_arr(-4 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-4 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-4 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-4 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = -0.1713514567376228224101768783E-2_wp
        filter_val1 = 0.1554774474923793899293724602E-2_wp
        filter_val2 = 0.3743663199398739083068656973E-2_wp
        filter_val3 = -0.272132773540999536127987988E-2_wp
        et0 = et0 + (x(i1, mod_arr(-3 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-3 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-3 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-3 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = 0.8020884755969165545893465146E-2_wp
        filter_val1 = -0.13367239421126794640208314967E-1_wp
        filter_val2 = -0.22857677317437822508458981792E-1_wp
        filter_val3 = 0.28206423027424364666613729847E-1_wp
        et0 = et0 + (x(i1, mod_arr(-2 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-2 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-2 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-2 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = -0.3131289721448565965922638841E-1_wp
        filter_val1 = 0.43793326416155009500101924188E-1_wp
        filter_val2 = 0.77858113707232306275457825041E-1_wp
        filter_val3 = -0.119506731011354509446152089555E0_wp
        et0 = et0 + (x(i1, mod_arr(-1 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val0)
        ot0 = ot0 + (x(i1, mod_arr(-1 + i2 - (n)), 0, i3 + 0)) * (filt&
&er_val1)
        et0 = et0 + (x(i1, mod_arr(-1 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val2)
        ot0 = ot0 + (x(i1, mod_arr(-1 + i2 - (n)), 1, i3 + 0)) * (filt&
&er_val3)
        filter_val0 = 0.425987982696947193304592242583E0_wp
        filter_val1 = -0.14743648317973476936220327736E-1_wp
        filter_val2 = -0.781104684144826958489597897908E0_wp
        filter_val3 = 0.43810509543421050421696718266E0_wp
        et0 = et0 + (x(i1, mod_arr(0 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(0 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(0 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(0 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = 0.416066936766212575361540821295E0_wp
        filter_val1 = 0.780460480250744028494513806689E0_wp
        filter_val2 = 0.49247868599583189247332328007E-1_wp
        filter_val3 = 0.403467221125547183465707078756E0_wp
        et0 = et0 + (x(i1, mod_arr(1 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(1 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(1 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(1 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = -0.138524775674051000848952544828E0_wp
        filter_val1 = -0.110230081678334468262552683806E0_wp
        filter_val2 = -0.39920047232423474172371930475E-1_wp
        filter_val3 = -0.52935814515665507351837970092E-1_wp
        et0 = et0 + (x(i1, mod_arr(2 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(2 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(2 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(2 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = 0.318354143459706681365834462E-1_wp
        filter_val1 = 0.2342461473539713439541159825E-1_wp
        filter_val2 = 0.15281781287179308494904296254E-1_wp
        filter_val3 = 0.7288836926603960462299666138E-2_wp
        et0 = et0 + (x(i1, mod_arr(3 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(3 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(3 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(3 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = -0.3454315576482209830827198831E-2_wp
        filter_val1 = -0.4329999533120824069665375511E-2_wp
        filter_val2 = -0.1969945431937324753826114662E-2_wp
        filter_val3 = -0.1960213959538483354691921355E-2_wp
        et0 = et0 + (x(i1, mod_arr(4 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(4 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(4 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(4 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = -0.3416290232098375589631534E-4_wp
        filter_val1 = 0.547310401099738937766344803E-3_wp
        filter_val2 = -0.11402461428743860613089559E-4_wp
        filter_val3 = 0.28479503466830096256502681E-3_wp
        et0 = et0 + (x(i1, mod_arr(5 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(5 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(5 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(5 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = 0.974884267415657697560756E-5_wp
        filter_val1 = -0.2848044279309673361077534E-4_wp
        filter_val2 = 0.5589042956883420231324354E-5_wp
        filter_val3 = -0.1716983589581506701252117E-4_wp
        et0 = et0 + (x(i1, mod_arr(6 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(6 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(6 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(6 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = 0.173933065619790394896889E-6_wp
        filter_val1 = -0.1557817271407449628766103E-5_wp
        filter_val2 = 0.9718640758826677836603E-7_wp
        filter_val3 = -0.877963062125028995526077E-6_wp
        et0 = et0 + (x(i1, mod_arr(7 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(7 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(7 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(7 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        filter_val0 = 0.0_wp
        filter_val1 = -0.9225014992147408201492E-8_wp
        filter_val2 = 0.0_wp
        filter_val3 = -0.5154546456362229249344E-8_wp
        et0 = et0 + (x(i1, mod_arr(8 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val0)
        ot0 = ot0 + (x(i1, mod_arr(8 + i2 - (n)), 0, i3 + 0)) * (filte&
&r_val1)
        et0 = et0 + (x(i1, mod_arr(8 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val2)
        ot0 = ot0 + (x(i1, mod_arr(8 + i2 - (n)), 1, i3 + 0)) * (filte&
&r_val3)
        et0 = (et0) * (a)
        ot0 = (ot0) * (a)
        et0 = et0 + (y(i1, 0, i2, i3 + 0)) * (a_y)
        ot0 = ot0 + (y(i1, 1, i2, i3 + 0)) * (a_y)
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_p_201_a_ay_ld_2u1_v1_t_f_t
SUBROUTINE d_s1s0_symicomb8_p_201_a_ay_ld_2u1_v1_t_f_t_cost(ndat0, n, &
&ndat2, cost)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat2
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * ((ndat2) * (ndat0))
END SUBROUTINE d_s1s0_symicomb8_p_201_a_ay_ld_2u1_v1_t_f_t_cost
SUBROUTINE d_s1s0_symicomb8_fg_10_ld_1u1_v1_f_f_t(n, ndat1, nx, ny, x,&
& y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -8
  integer(kind=4), parameter :: upfil = 7
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat1
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:nx - (1), 0:1, 0:ndat1 - (1)) &
&:: x
  real(kind=8), intent(out), dimension(0:1,  -(7):ny - (7) - (1), 0:nd&
&at1 - (1)) :: y
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lre_fil = resh&
&ape((/ &
0.0_wp, &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lro_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hre_fil = resh&
&ape((/ &
0.0_wp, &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hro_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
!$omp parallel  default(shared) private(i1, i2, l, et0, ot0, filt&
!$omp&er_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i2 = 0, ndat1 - (1), 1
    do i1 =  -(7),  -(-8) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      do l = max( -(i1), -8), 7, 1
        filter_val0 = symicomb8_lro_fil(l)
        filter_val1 = symicomb8_lre_fil(l)
        filter_val2 = symicomb8_hro_fil(l)
        filter_val3 = symicomb8_hre_fil(l)
        et0 = et0 + (x(l + i1, 0, i2 + 0)) * (filter_val0)
        ot0 = ot0 + (x(l + i1, 0, i2 + 0)) * (filter_val1)
        et0 = et0 + (x(l + i1, 1, i2 + 0)) * (filter_val2)
        ot0 = ot0 + (x(l + i1, 1, i2 + 0)) * (filter_val3)
      end do
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
    do i1 =  -(-8), n - (7) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = 0.1593875388117163824756E-8_wp
      filter_val1 = 0.0_wp
      filter_val2 = -0.2852535033969307505679E-8_wp
      filter_val3 = 0.0_wp
      et0 = et0 + (x(-8 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-8 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-8 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-8 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.156915542257710856911416E-6_wp
      filter_val1 = -0.24646355857433108358764E-7_wp
      filter_val2 = -0.276667298808735176085978E-6_wp
      filter_val3 = 0.44109215856612868615909E-7_wp
      et0 = et0 + (x(-7 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-7 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-7 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-7 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.2075567562651587632761451E-5_wp
      filter_val1 = -0.40033729064957944938397E-6_wp
      filter_val2 = -0.3318610754044407103712689E-5_wp
      filter_val3 = 0.652118308986595532963584E-6_wp
      et0 = et0 + (x(-6 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-6 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-6 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-6 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.25057266882581997397910903E-4_wp
      filter_val1 = -0.14147525211040058739651301E-4_wp
      filter_val2 = -0.40668222535397420851105277E-4_wp
      filter_val3 = 0.24486891695262527278480373E-4_wp
      et0 = et0 + (x(-5 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-5 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-5 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-5 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.1554774474923793899293724602E-2_wp
      filter_val1 = 0.239878289281775075165492659E-3_wp
      filter_val2 = -0.272132773540999536127987988E-2_wp
      filter_val3 = -0.464504573781934008099959241E-3_wp
      et0 = et0 + (x(-4 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-4 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-4 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-4 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.13367239421126794640208314967E-1_wp
      filter_val1 = -0.1713514567376228224101768783E-2_wp
      filter_val2 = 0.28206423027424364666613729847E-1_wp
      filter_val3 = 0.3743663199398739083068656973E-2_wp
      et0 = et0 + (x(-3 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-3 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-3 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-3 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.43793326416155009500101924188E-1_wp
      filter_val1 = 0.8020884755969165545893465146E-2_wp
      filter_val2 = -0.119506731011354509446152089555E0_wp
      filter_val3 = -0.22857677317437822508458981792E-1_wp
      et0 = et0 + (x(-2 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-2 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-2 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-2 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.14743648317973476936220327736E-1_wp
      filter_val1 = -0.3131289721448565965922638841E-1_wp
      filter_val2 = 0.43810509543421050421696718266E0_wp
      filter_val3 = 0.77858113707232306275457825041E-1_wp
      et0 = et0 + (x(-1 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-1 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-1 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-1 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.780460480250744028494513806689E0_wp
      filter_val1 = 0.425987982696947193304592242583E0_wp
      filter_val2 = 0.403467221125547183465707078756E0_wp
      filter_val3 = -0.781104684144826958489597897908E0_wp
      et0 = et0 + (x(0 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(0 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(0 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(0 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.110230081678334468262552683806E0_wp
      filter_val1 = 0.416066936766212575361540821295E0_wp
      filter_val2 = -0.52935814515665507351837970092E-1_wp
      filter_val3 = 0.49247868599583189247332328007E-1_wp
      et0 = et0 + (x(1 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(1 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(1 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(1 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.2342461473539713439541159825E-1_wp
      filter_val1 = -0.138524775674051000848952544828E0_wp
      filter_val2 = 0.7288836926603960462299666138E-2_wp
      filter_val3 = -0.39920047232423474172371930475E-1_wp
      et0 = et0 + (x(2 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(2 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(2 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(2 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.4329999533120824069665375511E-2_wp
      filter_val1 = 0.318354143459706681365834462E-1_wp
      filter_val2 = -0.1960213959538483354691921355E-2_wp
      filter_val3 = 0.15281781287179308494904296254E-1_wp
      et0 = et0 + (x(3 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(3 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(3 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(3 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.547310401099738937766344803E-3_wp
      filter_val1 = -0.3454315576482209830827198831E-2_wp
      filter_val2 = 0.28479503466830096256502681E-3_wp
      filter_val3 = -0.1969945431937324753826114662E-2_wp
      et0 = et0 + (x(4 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(4 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(4 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(4 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.2848044279309673361077534E-4_wp
      filter_val1 = -0.3416290232098375589631534E-4_wp
      filter_val2 = -0.1716983589581506701252117E-4_wp
      filter_val3 = -0.11402461428743860613089559E-4_wp
      et0 = et0 + (x(5 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(5 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(5 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(5 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.1557817271407449628766103E-5_wp
      filter_val1 = 0.974884267415657697560756E-5_wp
      filter_val2 = -0.877963062125028995526077E-6_wp
      filter_val3 = 0.5589042956883420231324354E-5_wp
      et0 = et0 + (x(6 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(6 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(6 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(6 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.9225014992147408201492E-8_wp
      filter_val1 = 0.173933065619790394896889E-6_wp
      filter_val2 = -0.5154546456362229249344E-8_wp
      filter_val3 = 0.9718640758826677836603E-7_wp
      et0 = et0 + (x(7 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(7 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(7 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(7 + i1, 1, i2 + 0)) * (filter_val3)
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
    do i1 = n - (7), n - (-8) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      do l = -8, min(7, n - (1) - (i1)), 1
        filter_val0 = symicomb8_lro_fil(l)
        filter_val1 = symicomb8_lre_fil(l)
        filter_val2 = symicomb8_hro_fil(l)
        filter_val3 = symicomb8_hre_fil(l)
        et0 = et0 + (x(l + i1, 0, i2 + 0)) * (filter_val0)
        ot0 = ot0 + (x(l + i1, 0, i2 + 0)) * (filter_val1)
        et0 = et0 + (x(l + i1, 1, i2 + 0)) * (filter_val2)
        ot0 = ot0 + (x(l + i1, 1, i2 + 0)) * (filter_val3)
      end do
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_fg_10_ld_1u1_v1_f_f_t
SUBROUTINE d_s1s0_symicomb8_fg_10_ld_1u1_v1_f_f_t_cost(n, ndat1, cost)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat1
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * (ndat1)
END SUBROUTINE d_s1s0_symicomb8_fg_10_ld_1u1_v1_f_f_t_cost
SUBROUTINE d_s1s0_symicomb8_fg_10_a_ld_1u1_v1_f_f_t(n, ndat1, nx, ny, &
&x, y, a)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -8
  integer(kind=4), parameter :: upfil = 7
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat1
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:nx - (1), 0:1, 0:ndat1 - (1)) &
&:: x
  real(kind=8), intent(out), dimension(0:1,  -(7):ny - (7) - (1), 0:nd&
&at1 - (1)) :: y
  real(kind=8), intent(in) :: a
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lre_fil = resh&
&ape((/ &
0.0_wp, &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lro_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hre_fil = resh&
&ape((/ &
0.0_wp, &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hro_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
!$omp parallel  default(shared) private(i1, i2, l, et0, ot0, filt&
!$omp&er_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i2 = 0, ndat1 - (1), 1
    do i1 =  -(7),  -(-8) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      do l = max( -(i1), -8), 7, 1
        filter_val0 = symicomb8_lro_fil(l)
        filter_val1 = symicomb8_lre_fil(l)
        filter_val2 = symicomb8_hro_fil(l)
        filter_val3 = symicomb8_hre_fil(l)
        et0 = et0 + (x(l + i1, 0, i2 + 0)) * (filter_val0)
        ot0 = ot0 + (x(l + i1, 0, i2 + 0)) * (filter_val1)
        et0 = et0 + (x(l + i1, 1, i2 + 0)) * (filter_val2)
        ot0 = ot0 + (x(l + i1, 1, i2 + 0)) * (filter_val3)
      end do
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
    do i1 =  -(-8), n - (7) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = 0.1593875388117163824756E-8_wp
      filter_val1 = 0.0_wp
      filter_val2 = -0.2852535033969307505679E-8_wp
      filter_val3 = 0.0_wp
      et0 = et0 + (x(-8 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-8 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-8 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-8 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.156915542257710856911416E-6_wp
      filter_val1 = -0.24646355857433108358764E-7_wp
      filter_val2 = -0.276667298808735176085978E-6_wp
      filter_val3 = 0.44109215856612868615909E-7_wp
      et0 = et0 + (x(-7 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-7 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-7 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-7 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.2075567562651587632761451E-5_wp
      filter_val1 = -0.40033729064957944938397E-6_wp
      filter_val2 = -0.3318610754044407103712689E-5_wp
      filter_val3 = 0.652118308986595532963584E-6_wp
      et0 = et0 + (x(-6 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-6 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-6 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-6 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.25057266882581997397910903E-4_wp
      filter_val1 = -0.14147525211040058739651301E-4_wp
      filter_val2 = -0.40668222535397420851105277E-4_wp
      filter_val3 = 0.24486891695262527278480373E-4_wp
      et0 = et0 + (x(-5 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-5 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-5 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-5 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.1554774474923793899293724602E-2_wp
      filter_val1 = 0.239878289281775075165492659E-3_wp
      filter_val2 = -0.272132773540999536127987988E-2_wp
      filter_val3 = -0.464504573781934008099959241E-3_wp
      et0 = et0 + (x(-4 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-4 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-4 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-4 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.13367239421126794640208314967E-1_wp
      filter_val1 = -0.1713514567376228224101768783E-2_wp
      filter_val2 = 0.28206423027424364666613729847E-1_wp
      filter_val3 = 0.3743663199398739083068656973E-2_wp
      et0 = et0 + (x(-3 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-3 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-3 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-3 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.43793326416155009500101924188E-1_wp
      filter_val1 = 0.8020884755969165545893465146E-2_wp
      filter_val2 = -0.119506731011354509446152089555E0_wp
      filter_val3 = -0.22857677317437822508458981792E-1_wp
      et0 = et0 + (x(-2 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-2 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-2 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-2 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.14743648317973476936220327736E-1_wp
      filter_val1 = -0.3131289721448565965922638841E-1_wp
      filter_val2 = 0.43810509543421050421696718266E0_wp
      filter_val3 = 0.77858113707232306275457825041E-1_wp
      et0 = et0 + (x(-1 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-1 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-1 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-1 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.780460480250744028494513806689E0_wp
      filter_val1 = 0.425987982696947193304592242583E0_wp
      filter_val2 = 0.403467221125547183465707078756E0_wp
      filter_val3 = -0.781104684144826958489597897908E0_wp
      et0 = et0 + (x(0 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(0 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(0 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(0 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.110230081678334468262552683806E0_wp
      filter_val1 = 0.416066936766212575361540821295E0_wp
      filter_val2 = -0.52935814515665507351837970092E-1_wp
      filter_val3 = 0.49247868599583189247332328007E-1_wp
      et0 = et0 + (x(1 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(1 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(1 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(1 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.2342461473539713439541159825E-1_wp
      filter_val1 = -0.138524775674051000848952544828E0_wp
      filter_val2 = 0.7288836926603960462299666138E-2_wp
      filter_val3 = -0.39920047232423474172371930475E-1_wp
      et0 = et0 + (x(2 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(2 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(2 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(2 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.4329999533120824069665375511E-2_wp
      filter_val1 = 0.318354143459706681365834462E-1_wp
      filter_val2 = -0.1960213959538483354691921355E-2_wp
      filter_val3 = 0.15281781287179308494904296254E-1_wp
      et0 = et0 + (x(3 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(3 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(3 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(3 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.547310401099738937766344803E-3_wp
      filter_val1 = -0.3454315576482209830827198831E-2_wp
      filter_val2 = 0.28479503466830096256502681E-3_wp
      filter_val3 = -0.1969945431937324753826114662E-2_wp
      et0 = et0 + (x(4 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(4 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(4 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(4 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.2848044279309673361077534E-4_wp
      filter_val1 = -0.3416290232098375589631534E-4_wp
      filter_val2 = -0.1716983589581506701252117E-4_wp
      filter_val3 = -0.11402461428743860613089559E-4_wp
      et0 = et0 + (x(5 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(5 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(5 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(5 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.1557817271407449628766103E-5_wp
      filter_val1 = 0.974884267415657697560756E-5_wp
      filter_val2 = -0.877963062125028995526077E-6_wp
      filter_val3 = 0.5589042956883420231324354E-5_wp
      et0 = et0 + (x(6 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(6 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(6 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(6 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.9225014992147408201492E-8_wp
      filter_val1 = 0.173933065619790394896889E-6_wp
      filter_val2 = -0.5154546456362229249344E-8_wp
      filter_val3 = 0.9718640758826677836603E-7_wp
      et0 = et0 + (x(7 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(7 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(7 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(7 + i1, 1, i2 + 0)) * (filter_val3)
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
    do i1 = n - (7), n - (-8) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      do l = -8, min(7, n - (1) - (i1)), 1
        filter_val0 = symicomb8_lro_fil(l)
        filter_val1 = symicomb8_lre_fil(l)
        filter_val2 = symicomb8_hro_fil(l)
        filter_val3 = symicomb8_hre_fil(l)
        et0 = et0 + (x(l + i1, 0, i2 + 0)) * (filter_val0)
        ot0 = ot0 + (x(l + i1, 0, i2 + 0)) * (filter_val1)
        et0 = et0 + (x(l + i1, 1, i2 + 0)) * (filter_val2)
        ot0 = ot0 + (x(l + i1, 1, i2 + 0)) * (filter_val3)
      end do
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_fg_10_a_ld_1u1_v1_f_f_t
SUBROUTINE d_s1s0_symicomb8_fg_10_a_ld_1u1_v1_f_f_t_cost(n, ndat1, cos&
&t)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat1
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * (ndat1)
END SUBROUTINE d_s1s0_symicomb8_fg_10_a_ld_1u1_v1_f_f_t_cost
SUBROUTINE d_s1s0_symicomb8_fg_10_ay_ld_1u1_v1_f_f_t(n, ndat1, nx, ny,&
& x, y, a_y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -8
  integer(kind=4), parameter :: upfil = 7
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat1
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:nx - (1), 0:1, 0:ndat1 - (1)) &
&:: x
  real(kind=8), intent(inout), dimension(0:1,  -(7):ny - (7) - (1), 0:&
&ndat1 - (1)) :: y
  real(kind=8), intent(in) :: a_y
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lre_fil = resh&
&ape((/ &
0.0_wp, &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lro_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hre_fil = resh&
&ape((/ &
0.0_wp, &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hro_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
!$omp parallel  default(shared) private(i1, i2, l, et0, ot0, filt&
!$omp&er_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i2 = 0, ndat1 - (1), 1
    do i1 =  -(7),  -(-8) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      do l = max( -(i1), -8), 7, 1
        filter_val0 = symicomb8_lro_fil(l)
        filter_val1 = symicomb8_lre_fil(l)
        filter_val2 = symicomb8_hro_fil(l)
        filter_val3 = symicomb8_hre_fil(l)
        et0 = et0 + (x(l + i1, 0, i2 + 0)) * (filter_val0)
        ot0 = ot0 + (x(l + i1, 0, i2 + 0)) * (filter_val1)
        et0 = et0 + (x(l + i1, 1, i2 + 0)) * (filter_val2)
        ot0 = ot0 + (x(l + i1, 1, i2 + 0)) * (filter_val3)
      end do
      et0 = et0 + (y(0, i1, i2 + 0)) * (a_y)
      ot0 = ot0 + (y(1, i1, i2 + 0)) * (a_y)
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
    do i1 =  -(-8), n - (7) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = 0.1593875388117163824756E-8_wp
      filter_val1 = 0.0_wp
      filter_val2 = -0.2852535033969307505679E-8_wp
      filter_val3 = 0.0_wp
      et0 = et0 + (x(-8 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-8 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-8 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-8 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.156915542257710856911416E-6_wp
      filter_val1 = -0.24646355857433108358764E-7_wp
      filter_val2 = -0.276667298808735176085978E-6_wp
      filter_val3 = 0.44109215856612868615909E-7_wp
      et0 = et0 + (x(-7 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-7 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-7 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-7 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.2075567562651587632761451E-5_wp
      filter_val1 = -0.40033729064957944938397E-6_wp
      filter_val2 = -0.3318610754044407103712689E-5_wp
      filter_val3 = 0.652118308986595532963584E-6_wp
      et0 = et0 + (x(-6 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-6 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-6 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-6 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.25057266882581997397910903E-4_wp
      filter_val1 = -0.14147525211040058739651301E-4_wp
      filter_val2 = -0.40668222535397420851105277E-4_wp
      filter_val3 = 0.24486891695262527278480373E-4_wp
      et0 = et0 + (x(-5 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-5 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-5 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-5 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.1554774474923793899293724602E-2_wp
      filter_val1 = 0.239878289281775075165492659E-3_wp
      filter_val2 = -0.272132773540999536127987988E-2_wp
      filter_val3 = -0.464504573781934008099959241E-3_wp
      et0 = et0 + (x(-4 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-4 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-4 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-4 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.13367239421126794640208314967E-1_wp
      filter_val1 = -0.1713514567376228224101768783E-2_wp
      filter_val2 = 0.28206423027424364666613729847E-1_wp
      filter_val3 = 0.3743663199398739083068656973E-2_wp
      et0 = et0 + (x(-3 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-3 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-3 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-3 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.43793326416155009500101924188E-1_wp
      filter_val1 = 0.8020884755969165545893465146E-2_wp
      filter_val2 = -0.119506731011354509446152089555E0_wp
      filter_val3 = -0.22857677317437822508458981792E-1_wp
      et0 = et0 + (x(-2 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-2 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-2 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-2 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.14743648317973476936220327736E-1_wp
      filter_val1 = -0.3131289721448565965922638841E-1_wp
      filter_val2 = 0.43810509543421050421696718266E0_wp
      filter_val3 = 0.77858113707232306275457825041E-1_wp
      et0 = et0 + (x(-1 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-1 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-1 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-1 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.780460480250744028494513806689E0_wp
      filter_val1 = 0.425987982696947193304592242583E0_wp
      filter_val2 = 0.403467221125547183465707078756E0_wp
      filter_val3 = -0.781104684144826958489597897908E0_wp
      et0 = et0 + (x(0 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(0 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(0 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(0 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.110230081678334468262552683806E0_wp
      filter_val1 = 0.416066936766212575361540821295E0_wp
      filter_val2 = -0.52935814515665507351837970092E-1_wp
      filter_val3 = 0.49247868599583189247332328007E-1_wp
      et0 = et0 + (x(1 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(1 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(1 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(1 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.2342461473539713439541159825E-1_wp
      filter_val1 = -0.138524775674051000848952544828E0_wp
      filter_val2 = 0.7288836926603960462299666138E-2_wp
      filter_val3 = -0.39920047232423474172371930475E-1_wp
      et0 = et0 + (x(2 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(2 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(2 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(2 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.4329999533120824069665375511E-2_wp
      filter_val1 = 0.318354143459706681365834462E-1_wp
      filter_val2 = -0.1960213959538483354691921355E-2_wp
      filter_val3 = 0.15281781287179308494904296254E-1_wp
      et0 = et0 + (x(3 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(3 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(3 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(3 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.547310401099738937766344803E-3_wp
      filter_val1 = -0.3454315576482209830827198831E-2_wp
      filter_val2 = 0.28479503466830096256502681E-3_wp
      filter_val3 = -0.1969945431937324753826114662E-2_wp
      et0 = et0 + (x(4 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(4 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(4 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(4 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.2848044279309673361077534E-4_wp
      filter_val1 = -0.3416290232098375589631534E-4_wp
      filter_val2 = -0.1716983589581506701252117E-4_wp
      filter_val3 = -0.11402461428743860613089559E-4_wp
      et0 = et0 + (x(5 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(5 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(5 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(5 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.1557817271407449628766103E-5_wp
      filter_val1 = 0.974884267415657697560756E-5_wp
      filter_val2 = -0.877963062125028995526077E-6_wp
      filter_val3 = 0.5589042956883420231324354E-5_wp
      et0 = et0 + (x(6 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(6 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(6 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(6 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.9225014992147408201492E-8_wp
      filter_val1 = 0.173933065619790394896889E-6_wp
      filter_val2 = -0.5154546456362229249344E-8_wp
      filter_val3 = 0.9718640758826677836603E-7_wp
      et0 = et0 + (x(7 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(7 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(7 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(7 + i1, 1, i2 + 0)) * (filter_val3)
      et0 = et0 + (y(0, i1, i2 + 0)) * (a_y)
      ot0 = ot0 + (y(1, i1, i2 + 0)) * (a_y)
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
    do i1 = n - (7), n - (-8) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      do l = -8, min(7, n - (1) - (i1)), 1
        filter_val0 = symicomb8_lro_fil(l)
        filter_val1 = symicomb8_lre_fil(l)
        filter_val2 = symicomb8_hro_fil(l)
        filter_val3 = symicomb8_hre_fil(l)
        et0 = et0 + (x(l + i1, 0, i2 + 0)) * (filter_val0)
        ot0 = ot0 + (x(l + i1, 0, i2 + 0)) * (filter_val1)
        et0 = et0 + (x(l + i1, 1, i2 + 0)) * (filter_val2)
        ot0 = ot0 + (x(l + i1, 1, i2 + 0)) * (filter_val3)
      end do
      et0 = et0 + (y(0, i1, i2 + 0)) * (a_y)
      ot0 = ot0 + (y(1, i1, i2 + 0)) * (a_y)
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_fg_10_ay_ld_1u1_v1_f_f_t
SUBROUTINE d_s1s0_symicomb8_fg_10_ay_ld_1u1_v1_f_f_t_cost(n, ndat1, co&
&st)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat1
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * (ndat1)
END SUBROUTINE d_s1s0_symicomb8_fg_10_ay_ld_1u1_v1_f_f_t_cost
SUBROUTINE d_s1s0_symicomb8_fg_10_a_ay_ld_1u1_v1_f_f_t(n, ndat1, nx, n&
&y, x, y, a, a_y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -8
  integer(kind=4), parameter :: upfil = 7
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat1
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:nx - (1), 0:1, 0:ndat1 - (1)) &
&:: x
  real(kind=8), intent(inout), dimension(0:1,  -(7):ny - (7) - (1), 0:&
&ndat1 - (1)) :: y
  real(kind=8), intent(in) :: a
  real(kind=8), intent(in) :: a_y
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lre_fil = resh&
&ape((/ &
0.0_wp, &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lro_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hre_fil = resh&
&ape((/ &
0.0_wp, &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hro_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
!$omp parallel  default(shared) private(i1, i2, l, et0, ot0, filt&
!$omp&er_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i2 = 0, ndat1 - (1), 1
    do i1 =  -(7),  -(-8) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      do l = max( -(i1), -8), 7, 1
        filter_val0 = symicomb8_lro_fil(l)
        filter_val1 = symicomb8_lre_fil(l)
        filter_val2 = symicomb8_hro_fil(l)
        filter_val3 = symicomb8_hre_fil(l)
        et0 = et0 + (x(l + i1, 0, i2 + 0)) * (filter_val0)
        ot0 = ot0 + (x(l + i1, 0, i2 + 0)) * (filter_val1)
        et0 = et0 + (x(l + i1, 1, i2 + 0)) * (filter_val2)
        ot0 = ot0 + (x(l + i1, 1, i2 + 0)) * (filter_val3)
      end do
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      et0 = et0 + (y(0, i1, i2 + 0)) * (a_y)
      ot0 = ot0 + (y(1, i1, i2 + 0)) * (a_y)
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
    do i1 =  -(-8), n - (7) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = 0.1593875388117163824756E-8_wp
      filter_val1 = 0.0_wp
      filter_val2 = -0.2852535033969307505679E-8_wp
      filter_val3 = 0.0_wp
      et0 = et0 + (x(-8 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-8 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-8 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-8 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.156915542257710856911416E-6_wp
      filter_val1 = -0.24646355857433108358764E-7_wp
      filter_val2 = -0.276667298808735176085978E-6_wp
      filter_val3 = 0.44109215856612868615909E-7_wp
      et0 = et0 + (x(-7 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-7 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-7 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-7 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.2075567562651587632761451E-5_wp
      filter_val1 = -0.40033729064957944938397E-6_wp
      filter_val2 = -0.3318610754044407103712689E-5_wp
      filter_val3 = 0.652118308986595532963584E-6_wp
      et0 = et0 + (x(-6 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-6 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-6 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-6 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.25057266882581997397910903E-4_wp
      filter_val1 = -0.14147525211040058739651301E-4_wp
      filter_val2 = -0.40668222535397420851105277E-4_wp
      filter_val3 = 0.24486891695262527278480373E-4_wp
      et0 = et0 + (x(-5 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-5 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-5 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-5 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.1554774474923793899293724602E-2_wp
      filter_val1 = 0.239878289281775075165492659E-3_wp
      filter_val2 = -0.272132773540999536127987988E-2_wp
      filter_val3 = -0.464504573781934008099959241E-3_wp
      et0 = et0 + (x(-4 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-4 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-4 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-4 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.13367239421126794640208314967E-1_wp
      filter_val1 = -0.1713514567376228224101768783E-2_wp
      filter_val2 = 0.28206423027424364666613729847E-1_wp
      filter_val3 = 0.3743663199398739083068656973E-2_wp
      et0 = et0 + (x(-3 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-3 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-3 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-3 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.43793326416155009500101924188E-1_wp
      filter_val1 = 0.8020884755969165545893465146E-2_wp
      filter_val2 = -0.119506731011354509446152089555E0_wp
      filter_val3 = -0.22857677317437822508458981792E-1_wp
      et0 = et0 + (x(-2 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-2 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-2 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-2 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.14743648317973476936220327736E-1_wp
      filter_val1 = -0.3131289721448565965922638841E-1_wp
      filter_val2 = 0.43810509543421050421696718266E0_wp
      filter_val3 = 0.77858113707232306275457825041E-1_wp
      et0 = et0 + (x(-1 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-1 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-1 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-1 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.780460480250744028494513806689E0_wp
      filter_val1 = 0.425987982696947193304592242583E0_wp
      filter_val2 = 0.403467221125547183465707078756E0_wp
      filter_val3 = -0.781104684144826958489597897908E0_wp
      et0 = et0 + (x(0 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(0 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(0 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(0 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.110230081678334468262552683806E0_wp
      filter_val1 = 0.416066936766212575361540821295E0_wp
      filter_val2 = -0.52935814515665507351837970092E-1_wp
      filter_val3 = 0.49247868599583189247332328007E-1_wp
      et0 = et0 + (x(1 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(1 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(1 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(1 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.2342461473539713439541159825E-1_wp
      filter_val1 = -0.138524775674051000848952544828E0_wp
      filter_val2 = 0.7288836926603960462299666138E-2_wp
      filter_val3 = -0.39920047232423474172371930475E-1_wp
      et0 = et0 + (x(2 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(2 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(2 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(2 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.4329999533120824069665375511E-2_wp
      filter_val1 = 0.318354143459706681365834462E-1_wp
      filter_val2 = -0.1960213959538483354691921355E-2_wp
      filter_val3 = 0.15281781287179308494904296254E-1_wp
      et0 = et0 + (x(3 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(3 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(3 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(3 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.547310401099738937766344803E-3_wp
      filter_val1 = -0.3454315576482209830827198831E-2_wp
      filter_val2 = 0.28479503466830096256502681E-3_wp
      filter_val3 = -0.1969945431937324753826114662E-2_wp
      et0 = et0 + (x(4 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(4 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(4 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(4 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.2848044279309673361077534E-4_wp
      filter_val1 = -0.3416290232098375589631534E-4_wp
      filter_val2 = -0.1716983589581506701252117E-4_wp
      filter_val3 = -0.11402461428743860613089559E-4_wp
      et0 = et0 + (x(5 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(5 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(5 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(5 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.1557817271407449628766103E-5_wp
      filter_val1 = 0.974884267415657697560756E-5_wp
      filter_val2 = -0.877963062125028995526077E-6_wp
      filter_val3 = 0.5589042956883420231324354E-5_wp
      et0 = et0 + (x(6 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(6 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(6 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(6 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.9225014992147408201492E-8_wp
      filter_val1 = 0.173933065619790394896889E-6_wp
      filter_val2 = -0.5154546456362229249344E-8_wp
      filter_val3 = 0.9718640758826677836603E-7_wp
      et0 = et0 + (x(7 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(7 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(7 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(7 + i1, 1, i2 + 0)) * (filter_val3)
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      et0 = et0 + (y(0, i1, i2 + 0)) * (a_y)
      ot0 = ot0 + (y(1, i1, i2 + 0)) * (a_y)
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
    do i1 = n - (7), n - (-8) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      do l = -8, min(7, n - (1) - (i1)), 1
        filter_val0 = symicomb8_lro_fil(l)
        filter_val1 = symicomb8_lre_fil(l)
        filter_val2 = symicomb8_hro_fil(l)
        filter_val3 = symicomb8_hre_fil(l)
        et0 = et0 + (x(l + i1, 0, i2 + 0)) * (filter_val0)
        ot0 = ot0 + (x(l + i1, 0, i2 + 0)) * (filter_val1)
        et0 = et0 + (x(l + i1, 1, i2 + 0)) * (filter_val2)
        ot0 = ot0 + (x(l + i1, 1, i2 + 0)) * (filter_val3)
      end do
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      et0 = et0 + (y(0, i1, i2 + 0)) * (a_y)
      ot0 = ot0 + (y(1, i1, i2 + 0)) * (a_y)
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_fg_10_a_ay_ld_1u1_v1_f_f_t
SUBROUTINE d_s1s0_symicomb8_fg_10_a_ay_ld_1u1_v1_f_f_t_cost(n, ndat1, &
&cost)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat1
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * (ndat1)
END SUBROUTINE d_s1s0_symicomb8_fg_10_a_ay_ld_1u1_v1_f_f_t_cost
SUBROUTINE d_s1s0_symicomb8_fg_01_ld_0u1_v1_f_f_t(ndat0, n, nx, ny, x,&
& y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -8
  integer(kind=4), parameter :: upfil = 7
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:ndat0 - (1), 0:nx - (1), 0:1) &
&:: x
  real(kind=8), intent(out), dimension(0:ndat0 - (1), 0:1,  -(7):ny - &
&(7) - (1)) :: y
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lre_fil = resh&
&ape((/ &
0.0_wp, &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lro_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hre_fil = resh&
&ape((/ &
0.0_wp, &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hro_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
!$omp parallel  default(shared) private(i1, i2, l, et0, ot0, filt&
!$omp&er_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i1 = 0, ndat0 - (1), 1
    do i2 =  -(7),  -(-8) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      do l = max( -(i2), -8), 7, 1
        filter_val0 = symicomb8_lro_fil(l)
        filter_val1 = symicomb8_lre_fil(l)
        filter_val2 = symicomb8_hro_fil(l)
        filter_val3 = symicomb8_hre_fil(l)
        et0 = et0 + (x(i1 + 0, l + i2, 0)) * (filter_val0)
        ot0 = ot0 + (x(i1 + 0, l + i2, 0)) * (filter_val1)
        et0 = et0 + (x(i1 + 0, l + i2, 1)) * (filter_val2)
        ot0 = ot0 + (x(i1 + 0, l + i2, 1)) * (filter_val3)
      end do
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
    do i2 =  -(-8), n - (7) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = 0.1593875388117163824756E-8_wp
      filter_val1 = 0.0_wp
      filter_val2 = -0.2852535033969307505679E-8_wp
      filter_val3 = 0.0_wp
      et0 = et0 + (x(i1 + 0, -8 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -8 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -8 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -8 + i2, 1)) * (filter_val3)
      filter_val0 = 0.156915542257710856911416E-6_wp
      filter_val1 = -0.24646355857433108358764E-7_wp
      filter_val2 = -0.276667298808735176085978E-6_wp
      filter_val3 = 0.44109215856612868615909E-7_wp
      et0 = et0 + (x(i1 + 0, -7 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -7 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -7 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -7 + i2, 1)) * (filter_val3)
      filter_val0 = 0.2075567562651587632761451E-5_wp
      filter_val1 = -0.40033729064957944938397E-6_wp
      filter_val2 = -0.3318610754044407103712689E-5_wp
      filter_val3 = 0.652118308986595532963584E-6_wp
      et0 = et0 + (x(i1 + 0, -6 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -6 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -6 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -6 + i2, 1)) * (filter_val3)
      filter_val0 = 0.25057266882581997397910903E-4_wp
      filter_val1 = -0.14147525211040058739651301E-4_wp
      filter_val2 = -0.40668222535397420851105277E-4_wp
      filter_val3 = 0.24486891695262527278480373E-4_wp
      et0 = et0 + (x(i1 + 0, -5 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -5 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -5 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -5 + i2, 1)) * (filter_val3)
      filter_val0 = 0.1554774474923793899293724602E-2_wp
      filter_val1 = 0.239878289281775075165492659E-3_wp
      filter_val2 = -0.272132773540999536127987988E-2_wp
      filter_val3 = -0.464504573781934008099959241E-3_wp
      et0 = et0 + (x(i1 + 0, -4 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -4 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -4 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -4 + i2, 1)) * (filter_val3)
      filter_val0 = -0.13367239421126794640208314967E-1_wp
      filter_val1 = -0.1713514567376228224101768783E-2_wp
      filter_val2 = 0.28206423027424364666613729847E-1_wp
      filter_val3 = 0.3743663199398739083068656973E-2_wp
      et0 = et0 + (x(i1 + 0, -3 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -3 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -3 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -3 + i2, 1)) * (filter_val3)
      filter_val0 = 0.43793326416155009500101924188E-1_wp
      filter_val1 = 0.8020884755969165545893465146E-2_wp
      filter_val2 = -0.119506731011354509446152089555E0_wp
      filter_val3 = -0.22857677317437822508458981792E-1_wp
      et0 = et0 + (x(i1 + 0, -2 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -2 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -2 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -2 + i2, 1)) * (filter_val3)
      filter_val0 = -0.14743648317973476936220327736E-1_wp
      filter_val1 = -0.3131289721448565965922638841E-1_wp
      filter_val2 = 0.43810509543421050421696718266E0_wp
      filter_val3 = 0.77858113707232306275457825041E-1_wp
      et0 = et0 + (x(i1 + 0, -1 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -1 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -1 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -1 + i2, 1)) * (filter_val3)
      filter_val0 = 0.780460480250744028494513806689E0_wp
      filter_val1 = 0.425987982696947193304592242583E0_wp
      filter_val2 = 0.403467221125547183465707078756E0_wp
      filter_val3 = -0.781104684144826958489597897908E0_wp
      et0 = et0 + (x(i1 + 0, 0 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 0 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 0 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 0 + i2, 1)) * (filter_val3)
      filter_val0 = -0.110230081678334468262552683806E0_wp
      filter_val1 = 0.416066936766212575361540821295E0_wp
      filter_val2 = -0.52935814515665507351837970092E-1_wp
      filter_val3 = 0.49247868599583189247332328007E-1_wp
      et0 = et0 + (x(i1 + 0, 1 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 1 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 1 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 1 + i2, 1)) * (filter_val3)
      filter_val0 = 0.2342461473539713439541159825E-1_wp
      filter_val1 = -0.138524775674051000848952544828E0_wp
      filter_val2 = 0.7288836926603960462299666138E-2_wp
      filter_val3 = -0.39920047232423474172371930475E-1_wp
      et0 = et0 + (x(i1 + 0, 2 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 2 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 2 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 2 + i2, 1)) * (filter_val3)
      filter_val0 = -0.4329999533120824069665375511E-2_wp
      filter_val1 = 0.318354143459706681365834462E-1_wp
      filter_val2 = -0.1960213959538483354691921355E-2_wp
      filter_val3 = 0.15281781287179308494904296254E-1_wp
      et0 = et0 + (x(i1 + 0, 3 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 3 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 3 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 3 + i2, 1)) * (filter_val3)
      filter_val0 = 0.547310401099738937766344803E-3_wp
      filter_val1 = -0.3454315576482209830827198831E-2_wp
      filter_val2 = 0.28479503466830096256502681E-3_wp
      filter_val3 = -0.1969945431937324753826114662E-2_wp
      et0 = et0 + (x(i1 + 0, 4 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 4 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 4 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 4 + i2, 1)) * (filter_val3)
      filter_val0 = -0.2848044279309673361077534E-4_wp
      filter_val1 = -0.3416290232098375589631534E-4_wp
      filter_val2 = -0.1716983589581506701252117E-4_wp
      filter_val3 = -0.11402461428743860613089559E-4_wp
      et0 = et0 + (x(i1 + 0, 5 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 5 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 5 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 5 + i2, 1)) * (filter_val3)
      filter_val0 = -0.1557817271407449628766103E-5_wp
      filter_val1 = 0.974884267415657697560756E-5_wp
      filter_val2 = -0.877963062125028995526077E-6_wp
      filter_val3 = 0.5589042956883420231324354E-5_wp
      et0 = et0 + (x(i1 + 0, 6 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 6 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 6 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 6 + i2, 1)) * (filter_val3)
      filter_val0 = -0.9225014992147408201492E-8_wp
      filter_val1 = 0.173933065619790394896889E-6_wp
      filter_val2 = -0.5154546456362229249344E-8_wp
      filter_val3 = 0.9718640758826677836603E-7_wp
      et0 = et0 + (x(i1 + 0, 7 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 7 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 7 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 7 + i2, 1)) * (filter_val3)
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
    do i2 = n - (7), n - (-8) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      do l = -8, min(7, n - (1) - (i2)), 1
        filter_val0 = symicomb8_lro_fil(l)
        filter_val1 = symicomb8_lre_fil(l)
        filter_val2 = symicomb8_hro_fil(l)
        filter_val3 = symicomb8_hre_fil(l)
        et0 = et0 + (x(i1 + 0, l + i2, 0)) * (filter_val0)
        ot0 = ot0 + (x(i1 + 0, l + i2, 0)) * (filter_val1)
        et0 = et0 + (x(i1 + 0, l + i2, 1)) * (filter_val2)
        ot0 = ot0 + (x(i1 + 0, l + i2, 1)) * (filter_val3)
      end do
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_fg_01_ld_0u1_v1_f_f_t
SUBROUTINE d_s1s0_symicomb8_fg_01_ld_0u1_v1_f_f_t_cost(ndat0, n, cost)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * (ndat0)
END SUBROUTINE d_s1s0_symicomb8_fg_01_ld_0u1_v1_f_f_t_cost
SUBROUTINE d_s1s0_symicomb8_fg_01_a_ld_0u1_v1_f_f_t(ndat0, n, nx, ny, &
&x, y, a)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -8
  integer(kind=4), parameter :: upfil = 7
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:ndat0 - (1), 0:nx - (1), 0:1) &
&:: x
  real(kind=8), intent(out), dimension(0:ndat0 - (1), 0:1,  -(7):ny - &
&(7) - (1)) :: y
  real(kind=8), intent(in) :: a
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lre_fil = resh&
&ape((/ &
0.0_wp, &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lro_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hre_fil = resh&
&ape((/ &
0.0_wp, &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hro_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
!$omp parallel  default(shared) private(i1, i2, l, et0, ot0, filt&
!$omp&er_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i1 = 0, ndat0 - (1), 1
    do i2 =  -(7),  -(-8) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      do l = max( -(i2), -8), 7, 1
        filter_val0 = symicomb8_lro_fil(l)
        filter_val1 = symicomb8_lre_fil(l)
        filter_val2 = symicomb8_hro_fil(l)
        filter_val3 = symicomb8_hre_fil(l)
        et0 = et0 + (x(i1 + 0, l + i2, 0)) * (filter_val0)
        ot0 = ot0 + (x(i1 + 0, l + i2, 0)) * (filter_val1)
        et0 = et0 + (x(i1 + 0, l + i2, 1)) * (filter_val2)
        ot0 = ot0 + (x(i1 + 0, l + i2, 1)) * (filter_val3)
      end do
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
    do i2 =  -(-8), n - (7) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = 0.1593875388117163824756E-8_wp
      filter_val1 = 0.0_wp
      filter_val2 = -0.2852535033969307505679E-8_wp
      filter_val3 = 0.0_wp
      et0 = et0 + (x(i1 + 0, -8 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -8 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -8 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -8 + i2, 1)) * (filter_val3)
      filter_val0 = 0.156915542257710856911416E-6_wp
      filter_val1 = -0.24646355857433108358764E-7_wp
      filter_val2 = -0.276667298808735176085978E-6_wp
      filter_val3 = 0.44109215856612868615909E-7_wp
      et0 = et0 + (x(i1 + 0, -7 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -7 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -7 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -7 + i2, 1)) * (filter_val3)
      filter_val0 = 0.2075567562651587632761451E-5_wp
      filter_val1 = -0.40033729064957944938397E-6_wp
      filter_val2 = -0.3318610754044407103712689E-5_wp
      filter_val3 = 0.652118308986595532963584E-6_wp
      et0 = et0 + (x(i1 + 0, -6 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -6 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -6 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -6 + i2, 1)) * (filter_val3)
      filter_val0 = 0.25057266882581997397910903E-4_wp
      filter_val1 = -0.14147525211040058739651301E-4_wp
      filter_val2 = -0.40668222535397420851105277E-4_wp
      filter_val3 = 0.24486891695262527278480373E-4_wp
      et0 = et0 + (x(i1 + 0, -5 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -5 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -5 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -5 + i2, 1)) * (filter_val3)
      filter_val0 = 0.1554774474923793899293724602E-2_wp
      filter_val1 = 0.239878289281775075165492659E-3_wp
      filter_val2 = -0.272132773540999536127987988E-2_wp
      filter_val3 = -0.464504573781934008099959241E-3_wp
      et0 = et0 + (x(i1 + 0, -4 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -4 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -4 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -4 + i2, 1)) * (filter_val3)
      filter_val0 = -0.13367239421126794640208314967E-1_wp
      filter_val1 = -0.1713514567376228224101768783E-2_wp
      filter_val2 = 0.28206423027424364666613729847E-1_wp
      filter_val3 = 0.3743663199398739083068656973E-2_wp
      et0 = et0 + (x(i1 + 0, -3 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -3 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -3 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -3 + i2, 1)) * (filter_val3)
      filter_val0 = 0.43793326416155009500101924188E-1_wp
      filter_val1 = 0.8020884755969165545893465146E-2_wp
      filter_val2 = -0.119506731011354509446152089555E0_wp
      filter_val3 = -0.22857677317437822508458981792E-1_wp
      et0 = et0 + (x(i1 + 0, -2 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -2 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -2 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -2 + i2, 1)) * (filter_val3)
      filter_val0 = -0.14743648317973476936220327736E-1_wp
      filter_val1 = -0.3131289721448565965922638841E-1_wp
      filter_val2 = 0.43810509543421050421696718266E0_wp
      filter_val3 = 0.77858113707232306275457825041E-1_wp
      et0 = et0 + (x(i1 + 0, -1 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -1 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -1 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -1 + i2, 1)) * (filter_val3)
      filter_val0 = 0.780460480250744028494513806689E0_wp
      filter_val1 = 0.425987982696947193304592242583E0_wp
      filter_val2 = 0.403467221125547183465707078756E0_wp
      filter_val3 = -0.781104684144826958489597897908E0_wp
      et0 = et0 + (x(i1 + 0, 0 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 0 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 0 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 0 + i2, 1)) * (filter_val3)
      filter_val0 = -0.110230081678334468262552683806E0_wp
      filter_val1 = 0.416066936766212575361540821295E0_wp
      filter_val2 = -0.52935814515665507351837970092E-1_wp
      filter_val3 = 0.49247868599583189247332328007E-1_wp
      et0 = et0 + (x(i1 + 0, 1 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 1 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 1 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 1 + i2, 1)) * (filter_val3)
      filter_val0 = 0.2342461473539713439541159825E-1_wp
      filter_val1 = -0.138524775674051000848952544828E0_wp
      filter_val2 = 0.7288836926603960462299666138E-2_wp
      filter_val3 = -0.39920047232423474172371930475E-1_wp
      et0 = et0 + (x(i1 + 0, 2 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 2 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 2 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 2 + i2, 1)) * (filter_val3)
      filter_val0 = -0.4329999533120824069665375511E-2_wp
      filter_val1 = 0.318354143459706681365834462E-1_wp
      filter_val2 = -0.1960213959538483354691921355E-2_wp
      filter_val3 = 0.15281781287179308494904296254E-1_wp
      et0 = et0 + (x(i1 + 0, 3 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 3 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 3 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 3 + i2, 1)) * (filter_val3)
      filter_val0 = 0.547310401099738937766344803E-3_wp
      filter_val1 = -0.3454315576482209830827198831E-2_wp
      filter_val2 = 0.28479503466830096256502681E-3_wp
      filter_val3 = -0.1969945431937324753826114662E-2_wp
      et0 = et0 + (x(i1 + 0, 4 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 4 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 4 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 4 + i2, 1)) * (filter_val3)
      filter_val0 = -0.2848044279309673361077534E-4_wp
      filter_val1 = -0.3416290232098375589631534E-4_wp
      filter_val2 = -0.1716983589581506701252117E-4_wp
      filter_val3 = -0.11402461428743860613089559E-4_wp
      et0 = et0 + (x(i1 + 0, 5 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 5 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 5 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 5 + i2, 1)) * (filter_val3)
      filter_val0 = -0.1557817271407449628766103E-5_wp
      filter_val1 = 0.974884267415657697560756E-5_wp
      filter_val2 = -0.877963062125028995526077E-6_wp
      filter_val3 = 0.5589042956883420231324354E-5_wp
      et0 = et0 + (x(i1 + 0, 6 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 6 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 6 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 6 + i2, 1)) * (filter_val3)
      filter_val0 = -0.9225014992147408201492E-8_wp
      filter_val1 = 0.173933065619790394896889E-6_wp
      filter_val2 = -0.5154546456362229249344E-8_wp
      filter_val3 = 0.9718640758826677836603E-7_wp
      et0 = et0 + (x(i1 + 0, 7 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 7 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 7 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 7 + i2, 1)) * (filter_val3)
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
    do i2 = n - (7), n - (-8) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      do l = -8, min(7, n - (1) - (i2)), 1
        filter_val0 = symicomb8_lro_fil(l)
        filter_val1 = symicomb8_lre_fil(l)
        filter_val2 = symicomb8_hro_fil(l)
        filter_val3 = symicomb8_hre_fil(l)
        et0 = et0 + (x(i1 + 0, l + i2, 0)) * (filter_val0)
        ot0 = ot0 + (x(i1 + 0, l + i2, 0)) * (filter_val1)
        et0 = et0 + (x(i1 + 0, l + i2, 1)) * (filter_val2)
        ot0 = ot0 + (x(i1 + 0, l + i2, 1)) * (filter_val3)
      end do
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_fg_01_a_ld_0u1_v1_f_f_t
SUBROUTINE d_s1s0_symicomb8_fg_01_a_ld_0u1_v1_f_f_t_cost(ndat0, n, cos&
&t)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * (ndat0)
END SUBROUTINE d_s1s0_symicomb8_fg_01_a_ld_0u1_v1_f_f_t_cost
SUBROUTINE d_s1s0_symicomb8_fg_01_ay_ld_0u1_v1_f_f_t(ndat0, n, nx, ny,&
& x, y, a_y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -8
  integer(kind=4), parameter :: upfil = 7
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:ndat0 - (1), 0:nx - (1), 0:1) &
&:: x
  real(kind=8), intent(inout), dimension(0:ndat0 - (1), 0:1,  -(7):ny &
&- (7) - (1)) :: y
  real(kind=8), intent(in) :: a_y
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lre_fil = resh&
&ape((/ &
0.0_wp, &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lro_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hre_fil = resh&
&ape((/ &
0.0_wp, &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hro_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
!$omp parallel  default(shared) private(i1, i2, l, et0, ot0, filt&
!$omp&er_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i1 = 0, ndat0 - (1), 1
    do i2 =  -(7),  -(-8) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      do l = max( -(i2), -8), 7, 1
        filter_val0 = symicomb8_lro_fil(l)
        filter_val1 = symicomb8_lre_fil(l)
        filter_val2 = symicomb8_hro_fil(l)
        filter_val3 = symicomb8_hre_fil(l)
        et0 = et0 + (x(i1 + 0, l + i2, 0)) * (filter_val0)
        ot0 = ot0 + (x(i1 + 0, l + i2, 0)) * (filter_val1)
        et0 = et0 + (x(i1 + 0, l + i2, 1)) * (filter_val2)
        ot0 = ot0 + (x(i1 + 0, l + i2, 1)) * (filter_val3)
      end do
      et0 = et0 + (y(i1 + 0, 0, i2)) * (a_y)
      ot0 = ot0 + (y(i1 + 0, 1, i2)) * (a_y)
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
    do i2 =  -(-8), n - (7) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = 0.1593875388117163824756E-8_wp
      filter_val1 = 0.0_wp
      filter_val2 = -0.2852535033969307505679E-8_wp
      filter_val3 = 0.0_wp
      et0 = et0 + (x(i1 + 0, -8 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -8 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -8 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -8 + i2, 1)) * (filter_val3)
      filter_val0 = 0.156915542257710856911416E-6_wp
      filter_val1 = -0.24646355857433108358764E-7_wp
      filter_val2 = -0.276667298808735176085978E-6_wp
      filter_val3 = 0.44109215856612868615909E-7_wp
      et0 = et0 + (x(i1 + 0, -7 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -7 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -7 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -7 + i2, 1)) * (filter_val3)
      filter_val0 = 0.2075567562651587632761451E-5_wp
      filter_val1 = -0.40033729064957944938397E-6_wp
      filter_val2 = -0.3318610754044407103712689E-5_wp
      filter_val3 = 0.652118308986595532963584E-6_wp
      et0 = et0 + (x(i1 + 0, -6 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -6 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -6 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -6 + i2, 1)) * (filter_val3)
      filter_val0 = 0.25057266882581997397910903E-4_wp
      filter_val1 = -0.14147525211040058739651301E-4_wp
      filter_val2 = -0.40668222535397420851105277E-4_wp
      filter_val3 = 0.24486891695262527278480373E-4_wp
      et0 = et0 + (x(i1 + 0, -5 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -5 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -5 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -5 + i2, 1)) * (filter_val3)
      filter_val0 = 0.1554774474923793899293724602E-2_wp
      filter_val1 = 0.239878289281775075165492659E-3_wp
      filter_val2 = -0.272132773540999536127987988E-2_wp
      filter_val3 = -0.464504573781934008099959241E-3_wp
      et0 = et0 + (x(i1 + 0, -4 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -4 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -4 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -4 + i2, 1)) * (filter_val3)
      filter_val0 = -0.13367239421126794640208314967E-1_wp
      filter_val1 = -0.1713514567376228224101768783E-2_wp
      filter_val2 = 0.28206423027424364666613729847E-1_wp
      filter_val3 = 0.3743663199398739083068656973E-2_wp
      et0 = et0 + (x(i1 + 0, -3 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -3 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -3 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -3 + i2, 1)) * (filter_val3)
      filter_val0 = 0.43793326416155009500101924188E-1_wp
      filter_val1 = 0.8020884755969165545893465146E-2_wp
      filter_val2 = -0.119506731011354509446152089555E0_wp
      filter_val3 = -0.22857677317437822508458981792E-1_wp
      et0 = et0 + (x(i1 + 0, -2 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -2 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -2 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -2 + i2, 1)) * (filter_val3)
      filter_val0 = -0.14743648317973476936220327736E-1_wp
      filter_val1 = -0.3131289721448565965922638841E-1_wp
      filter_val2 = 0.43810509543421050421696718266E0_wp
      filter_val3 = 0.77858113707232306275457825041E-1_wp
      et0 = et0 + (x(i1 + 0, -1 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -1 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -1 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -1 + i2, 1)) * (filter_val3)
      filter_val0 = 0.780460480250744028494513806689E0_wp
      filter_val1 = 0.425987982696947193304592242583E0_wp
      filter_val2 = 0.403467221125547183465707078756E0_wp
      filter_val3 = -0.781104684144826958489597897908E0_wp
      et0 = et0 + (x(i1 + 0, 0 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 0 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 0 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 0 + i2, 1)) * (filter_val3)
      filter_val0 = -0.110230081678334468262552683806E0_wp
      filter_val1 = 0.416066936766212575361540821295E0_wp
      filter_val2 = -0.52935814515665507351837970092E-1_wp
      filter_val3 = 0.49247868599583189247332328007E-1_wp
      et0 = et0 + (x(i1 + 0, 1 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 1 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 1 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 1 + i2, 1)) * (filter_val3)
      filter_val0 = 0.2342461473539713439541159825E-1_wp
      filter_val1 = -0.138524775674051000848952544828E0_wp
      filter_val2 = 0.7288836926603960462299666138E-2_wp
      filter_val3 = -0.39920047232423474172371930475E-1_wp
      et0 = et0 + (x(i1 + 0, 2 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 2 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 2 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 2 + i2, 1)) * (filter_val3)
      filter_val0 = -0.4329999533120824069665375511E-2_wp
      filter_val1 = 0.318354143459706681365834462E-1_wp
      filter_val2 = -0.1960213959538483354691921355E-2_wp
      filter_val3 = 0.15281781287179308494904296254E-1_wp
      et0 = et0 + (x(i1 + 0, 3 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 3 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 3 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 3 + i2, 1)) * (filter_val3)
      filter_val0 = 0.547310401099738937766344803E-3_wp
      filter_val1 = -0.3454315576482209830827198831E-2_wp
      filter_val2 = 0.28479503466830096256502681E-3_wp
      filter_val3 = -0.1969945431937324753826114662E-2_wp
      et0 = et0 + (x(i1 + 0, 4 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 4 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 4 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 4 + i2, 1)) * (filter_val3)
      filter_val0 = -0.2848044279309673361077534E-4_wp
      filter_val1 = -0.3416290232098375589631534E-4_wp
      filter_val2 = -0.1716983589581506701252117E-4_wp
      filter_val3 = -0.11402461428743860613089559E-4_wp
      et0 = et0 + (x(i1 + 0, 5 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 5 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 5 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 5 + i2, 1)) * (filter_val3)
      filter_val0 = -0.1557817271407449628766103E-5_wp
      filter_val1 = 0.974884267415657697560756E-5_wp
      filter_val2 = -0.877963062125028995526077E-6_wp
      filter_val3 = 0.5589042956883420231324354E-5_wp
      et0 = et0 + (x(i1 + 0, 6 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 6 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 6 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 6 + i2, 1)) * (filter_val3)
      filter_val0 = -0.9225014992147408201492E-8_wp
      filter_val1 = 0.173933065619790394896889E-6_wp
      filter_val2 = -0.5154546456362229249344E-8_wp
      filter_val3 = 0.9718640758826677836603E-7_wp
      et0 = et0 + (x(i1 + 0, 7 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 7 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 7 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 7 + i2, 1)) * (filter_val3)
      et0 = et0 + (y(i1 + 0, 0, i2)) * (a_y)
      ot0 = ot0 + (y(i1 + 0, 1, i2)) * (a_y)
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
    do i2 = n - (7), n - (-8) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      do l = -8, min(7, n - (1) - (i2)), 1
        filter_val0 = symicomb8_lro_fil(l)
        filter_val1 = symicomb8_lre_fil(l)
        filter_val2 = symicomb8_hro_fil(l)
        filter_val3 = symicomb8_hre_fil(l)
        et0 = et0 + (x(i1 + 0, l + i2, 0)) * (filter_val0)
        ot0 = ot0 + (x(i1 + 0, l + i2, 0)) * (filter_val1)
        et0 = et0 + (x(i1 + 0, l + i2, 1)) * (filter_val2)
        ot0 = ot0 + (x(i1 + 0, l + i2, 1)) * (filter_val3)
      end do
      et0 = et0 + (y(i1 + 0, 0, i2)) * (a_y)
      ot0 = ot0 + (y(i1 + 0, 1, i2)) * (a_y)
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_fg_01_ay_ld_0u1_v1_f_f_t
SUBROUTINE d_s1s0_symicomb8_fg_01_ay_ld_0u1_v1_f_f_t_cost(ndat0, n, co&
&st)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * (ndat0)
END SUBROUTINE d_s1s0_symicomb8_fg_01_ay_ld_0u1_v1_f_f_t_cost
SUBROUTINE d_s1s0_symicomb8_fg_01_a_ay_ld_0u1_v1_f_f_t(ndat0, n, nx, n&
&y, x, y, a, a_y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -8
  integer(kind=4), parameter :: upfil = 7
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:ndat0 - (1), 0:nx - (1), 0:1) &
&:: x
  real(kind=8), intent(inout), dimension(0:ndat0 - (1), 0:1,  -(7):ny &
&- (7) - (1)) :: y
  real(kind=8), intent(in) :: a
  real(kind=8), intent(in) :: a_y
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lre_fil = resh&
&ape((/ &
0.0_wp, &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lro_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hre_fil = resh&
&ape((/ &
0.0_wp, &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hro_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
!$omp parallel  default(shared) private(i1, i2, l, et0, ot0, filt&
!$omp&er_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i1 = 0, ndat0 - (1), 1
    do i2 =  -(7),  -(-8) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      do l = max( -(i2), -8), 7, 1
        filter_val0 = symicomb8_lro_fil(l)
        filter_val1 = symicomb8_lre_fil(l)
        filter_val2 = symicomb8_hro_fil(l)
        filter_val3 = symicomb8_hre_fil(l)
        et0 = et0 + (x(i1 + 0, l + i2, 0)) * (filter_val0)
        ot0 = ot0 + (x(i1 + 0, l + i2, 0)) * (filter_val1)
        et0 = et0 + (x(i1 + 0, l + i2, 1)) * (filter_val2)
        ot0 = ot0 + (x(i1 + 0, l + i2, 1)) * (filter_val3)
      end do
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      et0 = et0 + (y(i1 + 0, 0, i2)) * (a_y)
      ot0 = ot0 + (y(i1 + 0, 1, i2)) * (a_y)
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
    do i2 =  -(-8), n - (7) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = 0.1593875388117163824756E-8_wp
      filter_val1 = 0.0_wp
      filter_val2 = -0.2852535033969307505679E-8_wp
      filter_val3 = 0.0_wp
      et0 = et0 + (x(i1 + 0, -8 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -8 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -8 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -8 + i2, 1)) * (filter_val3)
      filter_val0 = 0.156915542257710856911416E-6_wp
      filter_val1 = -0.24646355857433108358764E-7_wp
      filter_val2 = -0.276667298808735176085978E-6_wp
      filter_val3 = 0.44109215856612868615909E-7_wp
      et0 = et0 + (x(i1 + 0, -7 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -7 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -7 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -7 + i2, 1)) * (filter_val3)
      filter_val0 = 0.2075567562651587632761451E-5_wp
      filter_val1 = -0.40033729064957944938397E-6_wp
      filter_val2 = -0.3318610754044407103712689E-5_wp
      filter_val3 = 0.652118308986595532963584E-6_wp
      et0 = et0 + (x(i1 + 0, -6 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -6 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -6 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -6 + i2, 1)) * (filter_val3)
      filter_val0 = 0.25057266882581997397910903E-4_wp
      filter_val1 = -0.14147525211040058739651301E-4_wp
      filter_val2 = -0.40668222535397420851105277E-4_wp
      filter_val3 = 0.24486891695262527278480373E-4_wp
      et0 = et0 + (x(i1 + 0, -5 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -5 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -5 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -5 + i2, 1)) * (filter_val3)
      filter_val0 = 0.1554774474923793899293724602E-2_wp
      filter_val1 = 0.239878289281775075165492659E-3_wp
      filter_val2 = -0.272132773540999536127987988E-2_wp
      filter_val3 = -0.464504573781934008099959241E-3_wp
      et0 = et0 + (x(i1 + 0, -4 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -4 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -4 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -4 + i2, 1)) * (filter_val3)
      filter_val0 = -0.13367239421126794640208314967E-1_wp
      filter_val1 = -0.1713514567376228224101768783E-2_wp
      filter_val2 = 0.28206423027424364666613729847E-1_wp
      filter_val3 = 0.3743663199398739083068656973E-2_wp
      et0 = et0 + (x(i1 + 0, -3 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -3 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -3 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -3 + i2, 1)) * (filter_val3)
      filter_val0 = 0.43793326416155009500101924188E-1_wp
      filter_val1 = 0.8020884755969165545893465146E-2_wp
      filter_val2 = -0.119506731011354509446152089555E0_wp
      filter_val3 = -0.22857677317437822508458981792E-1_wp
      et0 = et0 + (x(i1 + 0, -2 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -2 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -2 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -2 + i2, 1)) * (filter_val3)
      filter_val0 = -0.14743648317973476936220327736E-1_wp
      filter_val1 = -0.3131289721448565965922638841E-1_wp
      filter_val2 = 0.43810509543421050421696718266E0_wp
      filter_val3 = 0.77858113707232306275457825041E-1_wp
      et0 = et0 + (x(i1 + 0, -1 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -1 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -1 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -1 + i2, 1)) * (filter_val3)
      filter_val0 = 0.780460480250744028494513806689E0_wp
      filter_val1 = 0.425987982696947193304592242583E0_wp
      filter_val2 = 0.403467221125547183465707078756E0_wp
      filter_val3 = -0.781104684144826958489597897908E0_wp
      et0 = et0 + (x(i1 + 0, 0 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 0 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 0 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 0 + i2, 1)) * (filter_val3)
      filter_val0 = -0.110230081678334468262552683806E0_wp
      filter_val1 = 0.416066936766212575361540821295E0_wp
      filter_val2 = -0.52935814515665507351837970092E-1_wp
      filter_val3 = 0.49247868599583189247332328007E-1_wp
      et0 = et0 + (x(i1 + 0, 1 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 1 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 1 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 1 + i2, 1)) * (filter_val3)
      filter_val0 = 0.2342461473539713439541159825E-1_wp
      filter_val1 = -0.138524775674051000848952544828E0_wp
      filter_val2 = 0.7288836926603960462299666138E-2_wp
      filter_val3 = -0.39920047232423474172371930475E-1_wp
      et0 = et0 + (x(i1 + 0, 2 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 2 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 2 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 2 + i2, 1)) * (filter_val3)
      filter_val0 = -0.4329999533120824069665375511E-2_wp
      filter_val1 = 0.318354143459706681365834462E-1_wp
      filter_val2 = -0.1960213959538483354691921355E-2_wp
      filter_val3 = 0.15281781287179308494904296254E-1_wp
      et0 = et0 + (x(i1 + 0, 3 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 3 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 3 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 3 + i2, 1)) * (filter_val3)
      filter_val0 = 0.547310401099738937766344803E-3_wp
      filter_val1 = -0.3454315576482209830827198831E-2_wp
      filter_val2 = 0.28479503466830096256502681E-3_wp
      filter_val3 = -0.1969945431937324753826114662E-2_wp
      et0 = et0 + (x(i1 + 0, 4 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 4 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 4 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 4 + i2, 1)) * (filter_val3)
      filter_val0 = -0.2848044279309673361077534E-4_wp
      filter_val1 = -0.3416290232098375589631534E-4_wp
      filter_val2 = -0.1716983589581506701252117E-4_wp
      filter_val3 = -0.11402461428743860613089559E-4_wp
      et0 = et0 + (x(i1 + 0, 5 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 5 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 5 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 5 + i2, 1)) * (filter_val3)
      filter_val0 = -0.1557817271407449628766103E-5_wp
      filter_val1 = 0.974884267415657697560756E-5_wp
      filter_val2 = -0.877963062125028995526077E-6_wp
      filter_val3 = 0.5589042956883420231324354E-5_wp
      et0 = et0 + (x(i1 + 0, 6 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 6 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 6 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 6 + i2, 1)) * (filter_val3)
      filter_val0 = -0.9225014992147408201492E-8_wp
      filter_val1 = 0.173933065619790394896889E-6_wp
      filter_val2 = -0.5154546456362229249344E-8_wp
      filter_val3 = 0.9718640758826677836603E-7_wp
      et0 = et0 + (x(i1 + 0, 7 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 7 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 7 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 7 + i2, 1)) * (filter_val3)
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      et0 = et0 + (y(i1 + 0, 0, i2)) * (a_y)
      ot0 = ot0 + (y(i1 + 0, 1, i2)) * (a_y)
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
    do i2 = n - (7), n - (-8) - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      do l = -8, min(7, n - (1) - (i2)), 1
        filter_val0 = symicomb8_lro_fil(l)
        filter_val1 = symicomb8_lre_fil(l)
        filter_val2 = symicomb8_hro_fil(l)
        filter_val3 = symicomb8_hre_fil(l)
        et0 = et0 + (x(i1 + 0, l + i2, 0)) * (filter_val0)
        ot0 = ot0 + (x(i1 + 0, l + i2, 0)) * (filter_val1)
        et0 = et0 + (x(i1 + 0, l + i2, 1)) * (filter_val2)
        ot0 = ot0 + (x(i1 + 0, l + i2, 1)) * (filter_val3)
      end do
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      et0 = et0 + (y(i1 + 0, 0, i2)) * (a_y)
      ot0 = ot0 + (y(i1 + 0, 1, i2)) * (a_y)
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_fg_01_a_ay_ld_0u1_v1_f_f_t
SUBROUTINE d_s1s0_symicomb8_fg_01_a_ay_ld_0u1_v1_f_f_t_cost(ndat0, n, &
&cost)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * (ndat0)
END SUBROUTINE d_s1s0_symicomb8_fg_01_a_ay_ld_0u1_v1_f_f_t_cost
SUBROUTINE d_s1s0_symicomb8_fg_201_ld_2u1_v1_f_f_t(ndat0, n, ndat2, nx&
&, ny, x, y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -8
  integer(kind=4), parameter :: upfil = 7
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat2
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:ndat0 - (1), 0:nx - (1), 0:1, &
&0:ndat2 - (1)) :: x
  real(kind=8), intent(out), dimension(0:ndat0 - (1), 0:1,  -(7):ny - &
&(7) - (1), 0:ndat2 - (1)) :: y
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lre_fil = resh&
&ape((/ &
0.0_wp, &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lro_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hre_fil = resh&
&ape((/ &
0.0_wp, &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hro_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: i3
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
!$omp parallel  default(shared) private(i1, i2, i3, l, et0, ot0, &
!$omp&filter_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i3 = 0, ndat2 - (1), 1
    do i1 = 0, ndat0 - (1), 1
      do i2 =  -(7),  -(-8) - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        do l = max( -(i2), -8), 7, 1
          filter_val0 = symicomb8_lro_fil(l)
          filter_val1 = symicomb8_lre_fil(l)
          filter_val2 = symicomb8_hro_fil(l)
          filter_val3 = symicomb8_hre_fil(l)
          et0 = et0 + (x(i1, l + i2, 0, i3 + 0)) * (filter_val0)
          ot0 = ot0 + (x(i1, l + i2, 0, i3 + 0)) * (filter_val1)
          et0 = et0 + (x(i1, l + i2, 1, i3 + 0)) * (filter_val2)
          ot0 = ot0 + (x(i1, l + i2, 1, i3 + 0)) * (filter_val3)
        end do
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
      do i2 =  -(-8), n - (7) - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        filter_val0 = 0.1593875388117163824756E-8_wp
        filter_val1 = 0.0_wp
        filter_val2 = -0.2852535033969307505679E-8_wp
        filter_val3 = 0.0_wp
        et0 = et0 + (x(i1, -8 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -8 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -8 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -8 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.156915542257710856911416E-6_wp
        filter_val1 = -0.24646355857433108358764E-7_wp
        filter_val2 = -0.276667298808735176085978E-6_wp
        filter_val3 = 0.44109215856612868615909E-7_wp
        et0 = et0 + (x(i1, -7 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -7 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -7 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -7 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.2075567562651587632761451E-5_wp
        filter_val1 = -0.40033729064957944938397E-6_wp
        filter_val2 = -0.3318610754044407103712689E-5_wp
        filter_val3 = 0.652118308986595532963584E-6_wp
        et0 = et0 + (x(i1, -6 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -6 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -6 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -6 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.25057266882581997397910903E-4_wp
        filter_val1 = -0.14147525211040058739651301E-4_wp
        filter_val2 = -0.40668222535397420851105277E-4_wp
        filter_val3 = 0.24486891695262527278480373E-4_wp
        et0 = et0 + (x(i1, -5 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -5 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -5 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -5 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.1554774474923793899293724602E-2_wp
        filter_val1 = 0.239878289281775075165492659E-3_wp
        filter_val2 = -0.272132773540999536127987988E-2_wp
        filter_val3 = -0.464504573781934008099959241E-3_wp
        et0 = et0 + (x(i1, -4 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -4 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -4 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -4 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.13367239421126794640208314967E-1_wp
        filter_val1 = -0.1713514567376228224101768783E-2_wp
        filter_val2 = 0.28206423027424364666613729847E-1_wp
        filter_val3 = 0.3743663199398739083068656973E-2_wp
        et0 = et0 + (x(i1, -3 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -3 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -3 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -3 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.43793326416155009500101924188E-1_wp
        filter_val1 = 0.8020884755969165545893465146E-2_wp
        filter_val2 = -0.119506731011354509446152089555E0_wp
        filter_val3 = -0.22857677317437822508458981792E-1_wp
        et0 = et0 + (x(i1, -2 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -2 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -2 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -2 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.14743648317973476936220327736E-1_wp
        filter_val1 = -0.3131289721448565965922638841E-1_wp
        filter_val2 = 0.43810509543421050421696718266E0_wp
        filter_val3 = 0.77858113707232306275457825041E-1_wp
        et0 = et0 + (x(i1, -1 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -1 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -1 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -1 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.780460480250744028494513806689E0_wp
        filter_val1 = 0.425987982696947193304592242583E0_wp
        filter_val2 = 0.403467221125547183465707078756E0_wp
        filter_val3 = -0.781104684144826958489597897908E0_wp
        et0 = et0 + (x(i1, 0 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 0 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 0 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 0 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.110230081678334468262552683806E0_wp
        filter_val1 = 0.416066936766212575361540821295E0_wp
        filter_val2 = -0.52935814515665507351837970092E-1_wp
        filter_val3 = 0.49247868599583189247332328007E-1_wp
        et0 = et0 + (x(i1, 1 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 1 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 1 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 1 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.2342461473539713439541159825E-1_wp
        filter_val1 = -0.138524775674051000848952544828E0_wp
        filter_val2 = 0.7288836926603960462299666138E-2_wp
        filter_val3 = -0.39920047232423474172371930475E-1_wp
        et0 = et0 + (x(i1, 2 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 2 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 2 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 2 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.4329999533120824069665375511E-2_wp
        filter_val1 = 0.318354143459706681365834462E-1_wp
        filter_val2 = -0.1960213959538483354691921355E-2_wp
        filter_val3 = 0.15281781287179308494904296254E-1_wp
        et0 = et0 + (x(i1, 3 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 3 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 3 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 3 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.547310401099738937766344803E-3_wp
        filter_val1 = -0.3454315576482209830827198831E-2_wp
        filter_val2 = 0.28479503466830096256502681E-3_wp
        filter_val3 = -0.1969945431937324753826114662E-2_wp
        et0 = et0 + (x(i1, 4 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 4 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 4 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 4 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.2848044279309673361077534E-4_wp
        filter_val1 = -0.3416290232098375589631534E-4_wp
        filter_val2 = -0.1716983589581506701252117E-4_wp
        filter_val3 = -0.11402461428743860613089559E-4_wp
        et0 = et0 + (x(i1, 5 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 5 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 5 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 5 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.1557817271407449628766103E-5_wp
        filter_val1 = 0.974884267415657697560756E-5_wp
        filter_val2 = -0.877963062125028995526077E-6_wp
        filter_val3 = 0.5589042956883420231324354E-5_wp
        et0 = et0 + (x(i1, 6 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 6 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 6 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 6 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.9225014992147408201492E-8_wp
        filter_val1 = 0.173933065619790394896889E-6_wp
        filter_val2 = -0.5154546456362229249344E-8_wp
        filter_val3 = 0.9718640758826677836603E-7_wp
        et0 = et0 + (x(i1, 7 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 7 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 7 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 7 + i2, 1, i3 + 0)) * (filter_val3)
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
      do i2 = n - (7), n - (-8) - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        do l = -8, min(7, n - (1) - (i2)), 1
          filter_val0 = symicomb8_lro_fil(l)
          filter_val1 = symicomb8_lre_fil(l)
          filter_val2 = symicomb8_hro_fil(l)
          filter_val3 = symicomb8_hre_fil(l)
          et0 = et0 + (x(i1, l + i2, 0, i3 + 0)) * (filter_val0)
          ot0 = ot0 + (x(i1, l + i2, 0, i3 + 0)) * (filter_val1)
          et0 = et0 + (x(i1, l + i2, 1, i3 + 0)) * (filter_val2)
          ot0 = ot0 + (x(i1, l + i2, 1, i3 + 0)) * (filter_val3)
        end do
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_fg_201_ld_2u1_v1_f_f_t
SUBROUTINE d_s1s0_symicomb8_fg_201_ld_2u1_v1_f_f_t_cost(ndat0, n, ndat&
&2, cost)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat2
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * ((ndat2) * (ndat0))
END SUBROUTINE d_s1s0_symicomb8_fg_201_ld_2u1_v1_f_f_t_cost
SUBROUTINE d_s1s0_symicomb8_fg_201_a_ld_2u1_v1_f_f_t(ndat0, n, ndat2, &
&nx, ny, x, y, a)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -8
  integer(kind=4), parameter :: upfil = 7
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat2
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:ndat0 - (1), 0:nx - (1), 0:1, &
&0:ndat2 - (1)) :: x
  real(kind=8), intent(out), dimension(0:ndat0 - (1), 0:1,  -(7):ny - &
&(7) - (1), 0:ndat2 - (1)) :: y
  real(kind=8), intent(in) :: a
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lre_fil = resh&
&ape((/ &
0.0_wp, &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lro_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hre_fil = resh&
&ape((/ &
0.0_wp, &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hro_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: i3
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
!$omp parallel  default(shared) private(i1, i2, i3, l, et0, ot0, &
!$omp&filter_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i3 = 0, ndat2 - (1), 1
    do i1 = 0, ndat0 - (1), 1
      do i2 =  -(7),  -(-8) - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        do l = max( -(i2), -8), 7, 1
          filter_val0 = symicomb8_lro_fil(l)
          filter_val1 = symicomb8_lre_fil(l)
          filter_val2 = symicomb8_hro_fil(l)
          filter_val3 = symicomb8_hre_fil(l)
          et0 = et0 + (x(i1, l + i2, 0, i3 + 0)) * (filter_val0)
          ot0 = ot0 + (x(i1, l + i2, 0, i3 + 0)) * (filter_val1)
          et0 = et0 + (x(i1, l + i2, 1, i3 + 0)) * (filter_val2)
          ot0 = ot0 + (x(i1, l + i2, 1, i3 + 0)) * (filter_val3)
        end do
        et0 = (et0) * (a)
        ot0 = (ot0) * (a)
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
      do i2 =  -(-8), n - (7) - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        filter_val0 = 0.1593875388117163824756E-8_wp
        filter_val1 = 0.0_wp
        filter_val2 = -0.2852535033969307505679E-8_wp
        filter_val3 = 0.0_wp
        et0 = et0 + (x(i1, -8 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -8 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -8 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -8 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.156915542257710856911416E-6_wp
        filter_val1 = -0.24646355857433108358764E-7_wp
        filter_val2 = -0.276667298808735176085978E-6_wp
        filter_val3 = 0.44109215856612868615909E-7_wp
        et0 = et0 + (x(i1, -7 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -7 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -7 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -7 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.2075567562651587632761451E-5_wp
        filter_val1 = -0.40033729064957944938397E-6_wp
        filter_val2 = -0.3318610754044407103712689E-5_wp
        filter_val3 = 0.652118308986595532963584E-6_wp
        et0 = et0 + (x(i1, -6 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -6 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -6 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -6 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.25057266882581997397910903E-4_wp
        filter_val1 = -0.14147525211040058739651301E-4_wp
        filter_val2 = -0.40668222535397420851105277E-4_wp
        filter_val3 = 0.24486891695262527278480373E-4_wp
        et0 = et0 + (x(i1, -5 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -5 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -5 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -5 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.1554774474923793899293724602E-2_wp
        filter_val1 = 0.239878289281775075165492659E-3_wp
        filter_val2 = -0.272132773540999536127987988E-2_wp
        filter_val3 = -0.464504573781934008099959241E-3_wp
        et0 = et0 + (x(i1, -4 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -4 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -4 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -4 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.13367239421126794640208314967E-1_wp
        filter_val1 = -0.1713514567376228224101768783E-2_wp
        filter_val2 = 0.28206423027424364666613729847E-1_wp
        filter_val3 = 0.3743663199398739083068656973E-2_wp
        et0 = et0 + (x(i1, -3 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -3 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -3 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -3 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.43793326416155009500101924188E-1_wp
        filter_val1 = 0.8020884755969165545893465146E-2_wp
        filter_val2 = -0.119506731011354509446152089555E0_wp
        filter_val3 = -0.22857677317437822508458981792E-1_wp
        et0 = et0 + (x(i1, -2 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -2 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -2 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -2 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.14743648317973476936220327736E-1_wp
        filter_val1 = -0.3131289721448565965922638841E-1_wp
        filter_val2 = 0.43810509543421050421696718266E0_wp
        filter_val3 = 0.77858113707232306275457825041E-1_wp
        et0 = et0 + (x(i1, -1 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -1 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -1 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -1 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.780460480250744028494513806689E0_wp
        filter_val1 = 0.425987982696947193304592242583E0_wp
        filter_val2 = 0.403467221125547183465707078756E0_wp
        filter_val3 = -0.781104684144826958489597897908E0_wp
        et0 = et0 + (x(i1, 0 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 0 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 0 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 0 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.110230081678334468262552683806E0_wp
        filter_val1 = 0.416066936766212575361540821295E0_wp
        filter_val2 = -0.52935814515665507351837970092E-1_wp
        filter_val3 = 0.49247868599583189247332328007E-1_wp
        et0 = et0 + (x(i1, 1 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 1 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 1 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 1 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.2342461473539713439541159825E-1_wp
        filter_val1 = -0.138524775674051000848952544828E0_wp
        filter_val2 = 0.7288836926603960462299666138E-2_wp
        filter_val3 = -0.39920047232423474172371930475E-1_wp
        et0 = et0 + (x(i1, 2 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 2 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 2 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 2 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.4329999533120824069665375511E-2_wp
        filter_val1 = 0.318354143459706681365834462E-1_wp
        filter_val2 = -0.1960213959538483354691921355E-2_wp
        filter_val3 = 0.15281781287179308494904296254E-1_wp
        et0 = et0 + (x(i1, 3 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 3 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 3 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 3 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.547310401099738937766344803E-3_wp
        filter_val1 = -0.3454315576482209830827198831E-2_wp
        filter_val2 = 0.28479503466830096256502681E-3_wp
        filter_val3 = -0.1969945431937324753826114662E-2_wp
        et0 = et0 + (x(i1, 4 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 4 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 4 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 4 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.2848044279309673361077534E-4_wp
        filter_val1 = -0.3416290232098375589631534E-4_wp
        filter_val2 = -0.1716983589581506701252117E-4_wp
        filter_val3 = -0.11402461428743860613089559E-4_wp
        et0 = et0 + (x(i1, 5 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 5 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 5 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 5 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.1557817271407449628766103E-5_wp
        filter_val1 = 0.974884267415657697560756E-5_wp
        filter_val2 = -0.877963062125028995526077E-6_wp
        filter_val3 = 0.5589042956883420231324354E-5_wp
        et0 = et0 + (x(i1, 6 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 6 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 6 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 6 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.9225014992147408201492E-8_wp
        filter_val1 = 0.173933065619790394896889E-6_wp
        filter_val2 = -0.5154546456362229249344E-8_wp
        filter_val3 = 0.9718640758826677836603E-7_wp
        et0 = et0 + (x(i1, 7 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 7 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 7 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 7 + i2, 1, i3 + 0)) * (filter_val3)
        et0 = (et0) * (a)
        ot0 = (ot0) * (a)
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
      do i2 = n - (7), n - (-8) - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        do l = -8, min(7, n - (1) - (i2)), 1
          filter_val0 = symicomb8_lro_fil(l)
          filter_val1 = symicomb8_lre_fil(l)
          filter_val2 = symicomb8_hro_fil(l)
          filter_val3 = symicomb8_hre_fil(l)
          et0 = et0 + (x(i1, l + i2, 0, i3 + 0)) * (filter_val0)
          ot0 = ot0 + (x(i1, l + i2, 0, i3 + 0)) * (filter_val1)
          et0 = et0 + (x(i1, l + i2, 1, i3 + 0)) * (filter_val2)
          ot0 = ot0 + (x(i1, l + i2, 1, i3 + 0)) * (filter_val3)
        end do
        et0 = (et0) * (a)
        ot0 = (ot0) * (a)
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_fg_201_a_ld_2u1_v1_f_f_t
SUBROUTINE d_s1s0_symicomb8_fg_201_a_ld_2u1_v1_f_f_t_cost(ndat0, n, nd&
&at2, cost)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat2
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * ((ndat2) * (ndat0))
END SUBROUTINE d_s1s0_symicomb8_fg_201_a_ld_2u1_v1_f_f_t_cost
SUBROUTINE d_s1s0_symicomb8_fg_201_ay_ld_2u1_v1_f_f_t(ndat0, n, ndat2,&
& nx, ny, x, y, a_y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -8
  integer(kind=4), parameter :: upfil = 7
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat2
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:ndat0 - (1), 0:nx - (1), 0:1, &
&0:ndat2 - (1)) :: x
  real(kind=8), intent(inout), dimension(0:ndat0 - (1), 0:1,  -(7):ny &
&- (7) - (1), 0:ndat2 - (1)) :: y
  real(kind=8), intent(in) :: a_y
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lre_fil = resh&
&ape((/ &
0.0_wp, &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lro_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hre_fil = resh&
&ape((/ &
0.0_wp, &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hro_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: i3
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
!$omp parallel  default(shared) private(i1, i2, i3, l, et0, ot0, &
!$omp&filter_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i3 = 0, ndat2 - (1), 1
    do i1 = 0, ndat0 - (1), 1
      do i2 =  -(7),  -(-8) - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        do l = max( -(i2), -8), 7, 1
          filter_val0 = symicomb8_lro_fil(l)
          filter_val1 = symicomb8_lre_fil(l)
          filter_val2 = symicomb8_hro_fil(l)
          filter_val3 = symicomb8_hre_fil(l)
          et0 = et0 + (x(i1, l + i2, 0, i3 + 0)) * (filter_val0)
          ot0 = ot0 + (x(i1, l + i2, 0, i3 + 0)) * (filter_val1)
          et0 = et0 + (x(i1, l + i2, 1, i3 + 0)) * (filter_val2)
          ot0 = ot0 + (x(i1, l + i2, 1, i3 + 0)) * (filter_val3)
        end do
        et0 = et0 + (y(i1, 0, i2, i3 + 0)) * (a_y)
        ot0 = ot0 + (y(i1, 1, i2, i3 + 0)) * (a_y)
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
      do i2 =  -(-8), n - (7) - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        filter_val0 = 0.1593875388117163824756E-8_wp
        filter_val1 = 0.0_wp
        filter_val2 = -0.2852535033969307505679E-8_wp
        filter_val3 = 0.0_wp
        et0 = et0 + (x(i1, -8 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -8 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -8 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -8 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.156915542257710856911416E-6_wp
        filter_val1 = -0.24646355857433108358764E-7_wp
        filter_val2 = -0.276667298808735176085978E-6_wp
        filter_val3 = 0.44109215856612868615909E-7_wp
        et0 = et0 + (x(i1, -7 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -7 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -7 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -7 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.2075567562651587632761451E-5_wp
        filter_val1 = -0.40033729064957944938397E-6_wp
        filter_val2 = -0.3318610754044407103712689E-5_wp
        filter_val3 = 0.652118308986595532963584E-6_wp
        et0 = et0 + (x(i1, -6 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -6 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -6 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -6 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.25057266882581997397910903E-4_wp
        filter_val1 = -0.14147525211040058739651301E-4_wp
        filter_val2 = -0.40668222535397420851105277E-4_wp
        filter_val3 = 0.24486891695262527278480373E-4_wp
        et0 = et0 + (x(i1, -5 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -5 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -5 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -5 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.1554774474923793899293724602E-2_wp
        filter_val1 = 0.239878289281775075165492659E-3_wp
        filter_val2 = -0.272132773540999536127987988E-2_wp
        filter_val3 = -0.464504573781934008099959241E-3_wp
        et0 = et0 + (x(i1, -4 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -4 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -4 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -4 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.13367239421126794640208314967E-1_wp
        filter_val1 = -0.1713514567376228224101768783E-2_wp
        filter_val2 = 0.28206423027424364666613729847E-1_wp
        filter_val3 = 0.3743663199398739083068656973E-2_wp
        et0 = et0 + (x(i1, -3 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -3 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -3 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -3 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.43793326416155009500101924188E-1_wp
        filter_val1 = 0.8020884755969165545893465146E-2_wp
        filter_val2 = -0.119506731011354509446152089555E0_wp
        filter_val3 = -0.22857677317437822508458981792E-1_wp
        et0 = et0 + (x(i1, -2 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -2 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -2 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -2 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.14743648317973476936220327736E-1_wp
        filter_val1 = -0.3131289721448565965922638841E-1_wp
        filter_val2 = 0.43810509543421050421696718266E0_wp
        filter_val3 = 0.77858113707232306275457825041E-1_wp
        et0 = et0 + (x(i1, -1 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -1 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -1 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -1 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.780460480250744028494513806689E0_wp
        filter_val1 = 0.425987982696947193304592242583E0_wp
        filter_val2 = 0.403467221125547183465707078756E0_wp
        filter_val3 = -0.781104684144826958489597897908E0_wp
        et0 = et0 + (x(i1, 0 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 0 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 0 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 0 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.110230081678334468262552683806E0_wp
        filter_val1 = 0.416066936766212575361540821295E0_wp
        filter_val2 = -0.52935814515665507351837970092E-1_wp
        filter_val3 = 0.49247868599583189247332328007E-1_wp
        et0 = et0 + (x(i1, 1 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 1 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 1 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 1 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.2342461473539713439541159825E-1_wp
        filter_val1 = -0.138524775674051000848952544828E0_wp
        filter_val2 = 0.7288836926603960462299666138E-2_wp
        filter_val3 = -0.39920047232423474172371930475E-1_wp
        et0 = et0 + (x(i1, 2 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 2 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 2 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 2 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.4329999533120824069665375511E-2_wp
        filter_val1 = 0.318354143459706681365834462E-1_wp
        filter_val2 = -0.1960213959538483354691921355E-2_wp
        filter_val3 = 0.15281781287179308494904296254E-1_wp
        et0 = et0 + (x(i1, 3 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 3 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 3 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 3 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.547310401099738937766344803E-3_wp
        filter_val1 = -0.3454315576482209830827198831E-2_wp
        filter_val2 = 0.28479503466830096256502681E-3_wp
        filter_val3 = -0.1969945431937324753826114662E-2_wp
        et0 = et0 + (x(i1, 4 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 4 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 4 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 4 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.2848044279309673361077534E-4_wp
        filter_val1 = -0.3416290232098375589631534E-4_wp
        filter_val2 = -0.1716983589581506701252117E-4_wp
        filter_val3 = -0.11402461428743860613089559E-4_wp
        et0 = et0 + (x(i1, 5 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 5 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 5 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 5 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.1557817271407449628766103E-5_wp
        filter_val1 = 0.974884267415657697560756E-5_wp
        filter_val2 = -0.877963062125028995526077E-6_wp
        filter_val3 = 0.5589042956883420231324354E-5_wp
        et0 = et0 + (x(i1, 6 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 6 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 6 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 6 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.9225014992147408201492E-8_wp
        filter_val1 = 0.173933065619790394896889E-6_wp
        filter_val2 = -0.5154546456362229249344E-8_wp
        filter_val3 = 0.9718640758826677836603E-7_wp
        et0 = et0 + (x(i1, 7 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 7 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 7 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 7 + i2, 1, i3 + 0)) * (filter_val3)
        et0 = et0 + (y(i1, 0, i2, i3 + 0)) * (a_y)
        ot0 = ot0 + (y(i1, 1, i2, i3 + 0)) * (a_y)
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
      do i2 = n - (7), n - (-8) - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        do l = -8, min(7, n - (1) - (i2)), 1
          filter_val0 = symicomb8_lro_fil(l)
          filter_val1 = symicomb8_lre_fil(l)
          filter_val2 = symicomb8_hro_fil(l)
          filter_val3 = symicomb8_hre_fil(l)
          et0 = et0 + (x(i1, l + i2, 0, i3 + 0)) * (filter_val0)
          ot0 = ot0 + (x(i1, l + i2, 0, i3 + 0)) * (filter_val1)
          et0 = et0 + (x(i1, l + i2, 1, i3 + 0)) * (filter_val2)
          ot0 = ot0 + (x(i1, l + i2, 1, i3 + 0)) * (filter_val3)
        end do
        et0 = et0 + (y(i1, 0, i2, i3 + 0)) * (a_y)
        ot0 = ot0 + (y(i1, 1, i2, i3 + 0)) * (a_y)
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_fg_201_ay_ld_2u1_v1_f_f_t
SUBROUTINE d_s1s0_symicomb8_fg_201_ay_ld_2u1_v1_f_f_t_cost(ndat0, n, n&
&dat2, cost)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat2
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * ((ndat2) * (ndat0))
END SUBROUTINE d_s1s0_symicomb8_fg_201_ay_ld_2u1_v1_f_f_t_cost
SUBROUTINE d_s1s0_symicomb8_fg_201_a_ay_ld_2u1_v1_f_f_t(ndat0, n, ndat&
&2, nx, ny, x, y, a, a_y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -8
  integer(kind=4), parameter :: upfil = 7
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat2
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:ndat0 - (1), 0:nx - (1), 0:1, &
&0:ndat2 - (1)) :: x
  real(kind=8), intent(inout), dimension(0:ndat0 - (1), 0:1,  -(7):ny &
&- (7) - (1), 0:ndat2 - (1)) :: y
  real(kind=8), intent(in) :: a
  real(kind=8), intent(in) :: a_y
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lre_fil = resh&
&ape((/ &
0.0_wp, &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lro_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hre_fil = resh&
&ape((/ &
0.0_wp, &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hro_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: i3
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
!$omp parallel  default(shared) private(i1, i2, i3, l, et0, ot0, &
!$omp&filter_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i3 = 0, ndat2 - (1), 1
    do i1 = 0, ndat0 - (1), 1
      do i2 =  -(7),  -(-8) - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        do l = max( -(i2), -8), 7, 1
          filter_val0 = symicomb8_lro_fil(l)
          filter_val1 = symicomb8_lre_fil(l)
          filter_val2 = symicomb8_hro_fil(l)
          filter_val3 = symicomb8_hre_fil(l)
          et0 = et0 + (x(i1, l + i2, 0, i3 + 0)) * (filter_val0)
          ot0 = ot0 + (x(i1, l + i2, 0, i3 + 0)) * (filter_val1)
          et0 = et0 + (x(i1, l + i2, 1, i3 + 0)) * (filter_val2)
          ot0 = ot0 + (x(i1, l + i2, 1, i3 + 0)) * (filter_val3)
        end do
        et0 = (et0) * (a)
        ot0 = (ot0) * (a)
        et0 = et0 + (y(i1, 0, i2, i3 + 0)) * (a_y)
        ot0 = ot0 + (y(i1, 1, i2, i3 + 0)) * (a_y)
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
      do i2 =  -(-8), n - (7) - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        filter_val0 = 0.1593875388117163824756E-8_wp
        filter_val1 = 0.0_wp
        filter_val2 = -0.2852535033969307505679E-8_wp
        filter_val3 = 0.0_wp
        et0 = et0 + (x(i1, -8 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -8 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -8 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -8 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.156915542257710856911416E-6_wp
        filter_val1 = -0.24646355857433108358764E-7_wp
        filter_val2 = -0.276667298808735176085978E-6_wp
        filter_val3 = 0.44109215856612868615909E-7_wp
        et0 = et0 + (x(i1, -7 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -7 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -7 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -7 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.2075567562651587632761451E-5_wp
        filter_val1 = -0.40033729064957944938397E-6_wp
        filter_val2 = -0.3318610754044407103712689E-5_wp
        filter_val3 = 0.652118308986595532963584E-6_wp
        et0 = et0 + (x(i1, -6 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -6 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -6 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -6 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.25057266882581997397910903E-4_wp
        filter_val1 = -0.14147525211040058739651301E-4_wp
        filter_val2 = -0.40668222535397420851105277E-4_wp
        filter_val3 = 0.24486891695262527278480373E-4_wp
        et0 = et0 + (x(i1, -5 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -5 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -5 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -5 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.1554774474923793899293724602E-2_wp
        filter_val1 = 0.239878289281775075165492659E-3_wp
        filter_val2 = -0.272132773540999536127987988E-2_wp
        filter_val3 = -0.464504573781934008099959241E-3_wp
        et0 = et0 + (x(i1, -4 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -4 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -4 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -4 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.13367239421126794640208314967E-1_wp
        filter_val1 = -0.1713514567376228224101768783E-2_wp
        filter_val2 = 0.28206423027424364666613729847E-1_wp
        filter_val3 = 0.3743663199398739083068656973E-2_wp
        et0 = et0 + (x(i1, -3 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -3 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -3 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -3 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.43793326416155009500101924188E-1_wp
        filter_val1 = 0.8020884755969165545893465146E-2_wp
        filter_val2 = -0.119506731011354509446152089555E0_wp
        filter_val3 = -0.22857677317437822508458981792E-1_wp
        et0 = et0 + (x(i1, -2 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -2 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -2 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -2 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.14743648317973476936220327736E-1_wp
        filter_val1 = -0.3131289721448565965922638841E-1_wp
        filter_val2 = 0.43810509543421050421696718266E0_wp
        filter_val3 = 0.77858113707232306275457825041E-1_wp
        et0 = et0 + (x(i1, -1 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -1 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -1 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -1 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.780460480250744028494513806689E0_wp
        filter_val1 = 0.425987982696947193304592242583E0_wp
        filter_val2 = 0.403467221125547183465707078756E0_wp
        filter_val3 = -0.781104684144826958489597897908E0_wp
        et0 = et0 + (x(i1, 0 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 0 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 0 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 0 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.110230081678334468262552683806E0_wp
        filter_val1 = 0.416066936766212575361540821295E0_wp
        filter_val2 = -0.52935814515665507351837970092E-1_wp
        filter_val3 = 0.49247868599583189247332328007E-1_wp
        et0 = et0 + (x(i1, 1 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 1 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 1 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 1 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.2342461473539713439541159825E-1_wp
        filter_val1 = -0.138524775674051000848952544828E0_wp
        filter_val2 = 0.7288836926603960462299666138E-2_wp
        filter_val3 = -0.39920047232423474172371930475E-1_wp
        et0 = et0 + (x(i1, 2 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 2 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 2 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 2 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.4329999533120824069665375511E-2_wp
        filter_val1 = 0.318354143459706681365834462E-1_wp
        filter_val2 = -0.1960213959538483354691921355E-2_wp
        filter_val3 = 0.15281781287179308494904296254E-1_wp
        et0 = et0 + (x(i1, 3 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 3 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 3 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 3 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.547310401099738937766344803E-3_wp
        filter_val1 = -0.3454315576482209830827198831E-2_wp
        filter_val2 = 0.28479503466830096256502681E-3_wp
        filter_val3 = -0.1969945431937324753826114662E-2_wp
        et0 = et0 + (x(i1, 4 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 4 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 4 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 4 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.2848044279309673361077534E-4_wp
        filter_val1 = -0.3416290232098375589631534E-4_wp
        filter_val2 = -0.1716983589581506701252117E-4_wp
        filter_val3 = -0.11402461428743860613089559E-4_wp
        et0 = et0 + (x(i1, 5 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 5 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 5 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 5 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.1557817271407449628766103E-5_wp
        filter_val1 = 0.974884267415657697560756E-5_wp
        filter_val2 = -0.877963062125028995526077E-6_wp
        filter_val3 = 0.5589042956883420231324354E-5_wp
        et0 = et0 + (x(i1, 6 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 6 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 6 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 6 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.9225014992147408201492E-8_wp
        filter_val1 = 0.173933065619790394896889E-6_wp
        filter_val2 = -0.5154546456362229249344E-8_wp
        filter_val3 = 0.9718640758826677836603E-7_wp
        et0 = et0 + (x(i1, 7 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 7 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 7 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 7 + i2, 1, i3 + 0)) * (filter_val3)
        et0 = (et0) * (a)
        ot0 = (ot0) * (a)
        et0 = et0 + (y(i1, 0, i2, i3 + 0)) * (a_y)
        ot0 = ot0 + (y(i1, 1, i2, i3 + 0)) * (a_y)
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
      do i2 = n - (7), n - (-8) - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        do l = -8, min(7, n - (1) - (i2)), 1
          filter_val0 = symicomb8_lro_fil(l)
          filter_val1 = symicomb8_lre_fil(l)
          filter_val2 = symicomb8_hro_fil(l)
          filter_val3 = symicomb8_hre_fil(l)
          et0 = et0 + (x(i1, l + i2, 0, i3 + 0)) * (filter_val0)
          ot0 = ot0 + (x(i1, l + i2, 0, i3 + 0)) * (filter_val1)
          et0 = et0 + (x(i1, l + i2, 1, i3 + 0)) * (filter_val2)
          ot0 = ot0 + (x(i1, l + i2, 1, i3 + 0)) * (filter_val3)
        end do
        et0 = (et0) * (a)
        ot0 = (ot0) * (a)
        et0 = et0 + (y(i1, 0, i2, i3 + 0)) * (a_y)
        ot0 = ot0 + (y(i1, 1, i2, i3 + 0)) * (a_y)
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_fg_201_a_ay_ld_2u1_v1_f_f_t
SUBROUTINE d_s1s0_symicomb8_fg_201_a_ay_ld_2u1_v1_f_f_t_cost(ndat0, n,&
& ndat2, cost)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat2
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * ((ndat2) * (ndat0))
END SUBROUTINE d_s1s0_symicomb8_fg_201_a_ay_ld_2u1_v1_f_f_t_cost
SUBROUTINE d_s1s0_symicomb8_fs_10_ld_1u1_v1_f_f_t(n, ndat1, nx, ny, x,&
& y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -8
  integer(kind=4), parameter :: upfil = 7
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat1
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(-8:nx + -8 - (1), 0:1, 0:ndat1 -&
& (1)) :: x
  real(kind=8), intent(out), dimension(0:1, 0:ny - (1), 0:ndat1 - (1))&
& :: y
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lre_fil = resh&
&ape((/ &
0.0_wp, &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lro_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hre_fil = resh&
&ape((/ &
0.0_wp, &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hro_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
!$omp parallel  default(shared) private(i1, i2, l, et0, ot0, filt&
!$omp&er_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i2 = 0, ndat1 - (1), 1
    do i1 = 0, n - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = 0.1593875388117163824756E-8_wp
      filter_val1 = 0.0_wp
      filter_val2 = -0.2852535033969307505679E-8_wp
      filter_val3 = 0.0_wp
      et0 = et0 + (x(-8 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-8 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-8 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-8 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.156915542257710856911416E-6_wp
      filter_val1 = -0.24646355857433108358764E-7_wp
      filter_val2 = -0.276667298808735176085978E-6_wp
      filter_val3 = 0.44109215856612868615909E-7_wp
      et0 = et0 + (x(-7 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-7 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-7 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-7 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.2075567562651587632761451E-5_wp
      filter_val1 = -0.40033729064957944938397E-6_wp
      filter_val2 = -0.3318610754044407103712689E-5_wp
      filter_val3 = 0.652118308986595532963584E-6_wp
      et0 = et0 + (x(-6 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-6 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-6 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-6 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.25057266882581997397910903E-4_wp
      filter_val1 = -0.14147525211040058739651301E-4_wp
      filter_val2 = -0.40668222535397420851105277E-4_wp
      filter_val3 = 0.24486891695262527278480373E-4_wp
      et0 = et0 + (x(-5 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-5 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-5 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-5 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.1554774474923793899293724602E-2_wp
      filter_val1 = 0.239878289281775075165492659E-3_wp
      filter_val2 = -0.272132773540999536127987988E-2_wp
      filter_val3 = -0.464504573781934008099959241E-3_wp
      et0 = et0 + (x(-4 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-4 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-4 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-4 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.13367239421126794640208314967E-1_wp
      filter_val1 = -0.1713514567376228224101768783E-2_wp
      filter_val2 = 0.28206423027424364666613729847E-1_wp
      filter_val3 = 0.3743663199398739083068656973E-2_wp
      et0 = et0 + (x(-3 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-3 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-3 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-3 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.43793326416155009500101924188E-1_wp
      filter_val1 = 0.8020884755969165545893465146E-2_wp
      filter_val2 = -0.119506731011354509446152089555E0_wp
      filter_val3 = -0.22857677317437822508458981792E-1_wp
      et0 = et0 + (x(-2 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-2 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-2 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-2 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.14743648317973476936220327736E-1_wp
      filter_val1 = -0.3131289721448565965922638841E-1_wp
      filter_val2 = 0.43810509543421050421696718266E0_wp
      filter_val3 = 0.77858113707232306275457825041E-1_wp
      et0 = et0 + (x(-1 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-1 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-1 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-1 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.780460480250744028494513806689E0_wp
      filter_val1 = 0.425987982696947193304592242583E0_wp
      filter_val2 = 0.403467221125547183465707078756E0_wp
      filter_val3 = -0.781104684144826958489597897908E0_wp
      et0 = et0 + (x(0 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(0 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(0 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(0 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.110230081678334468262552683806E0_wp
      filter_val1 = 0.416066936766212575361540821295E0_wp
      filter_val2 = -0.52935814515665507351837970092E-1_wp
      filter_val3 = 0.49247868599583189247332328007E-1_wp
      et0 = et0 + (x(1 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(1 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(1 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(1 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.2342461473539713439541159825E-1_wp
      filter_val1 = -0.138524775674051000848952544828E0_wp
      filter_val2 = 0.7288836926603960462299666138E-2_wp
      filter_val3 = -0.39920047232423474172371930475E-1_wp
      et0 = et0 + (x(2 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(2 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(2 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(2 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.4329999533120824069665375511E-2_wp
      filter_val1 = 0.318354143459706681365834462E-1_wp
      filter_val2 = -0.1960213959538483354691921355E-2_wp
      filter_val3 = 0.15281781287179308494904296254E-1_wp
      et0 = et0 + (x(3 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(3 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(3 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(3 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.547310401099738937766344803E-3_wp
      filter_val1 = -0.3454315576482209830827198831E-2_wp
      filter_val2 = 0.28479503466830096256502681E-3_wp
      filter_val3 = -0.1969945431937324753826114662E-2_wp
      et0 = et0 + (x(4 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(4 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(4 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(4 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.2848044279309673361077534E-4_wp
      filter_val1 = -0.3416290232098375589631534E-4_wp
      filter_val2 = -0.1716983589581506701252117E-4_wp
      filter_val3 = -0.11402461428743860613089559E-4_wp
      et0 = et0 + (x(5 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(5 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(5 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(5 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.1557817271407449628766103E-5_wp
      filter_val1 = 0.974884267415657697560756E-5_wp
      filter_val2 = -0.877963062125028995526077E-6_wp
      filter_val3 = 0.5589042956883420231324354E-5_wp
      et0 = et0 + (x(6 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(6 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(6 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(6 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.9225014992147408201492E-8_wp
      filter_val1 = 0.173933065619790394896889E-6_wp
      filter_val2 = -0.5154546456362229249344E-8_wp
      filter_val3 = 0.9718640758826677836603E-7_wp
      et0 = et0 + (x(7 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(7 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(7 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(7 + i1, 1, i2 + 0)) * (filter_val3)
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_fs_10_ld_1u1_v1_f_f_t
SUBROUTINE d_s1s0_symicomb8_fs_10_ld_1u1_v1_f_f_t_cost(n, ndat1, cost)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat1
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * (ndat1)
END SUBROUTINE d_s1s0_symicomb8_fs_10_ld_1u1_v1_f_f_t_cost
SUBROUTINE d_s1s0_symicomb8_fs_10_a_ld_1u1_v1_f_f_t(n, ndat1, nx, ny, &
&x, y, a)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -8
  integer(kind=4), parameter :: upfil = 7
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat1
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(-8:nx + -8 - (1), 0:1, 0:ndat1 -&
& (1)) :: x
  real(kind=8), intent(out), dimension(0:1, 0:ny - (1), 0:ndat1 - (1))&
& :: y
  real(kind=8), intent(in) :: a
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lre_fil = resh&
&ape((/ &
0.0_wp, &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lro_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hre_fil = resh&
&ape((/ &
0.0_wp, &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hro_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
!$omp parallel  default(shared) private(i1, i2, l, et0, ot0, filt&
!$omp&er_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i2 = 0, ndat1 - (1), 1
    do i1 = 0, n - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = 0.1593875388117163824756E-8_wp
      filter_val1 = 0.0_wp
      filter_val2 = -0.2852535033969307505679E-8_wp
      filter_val3 = 0.0_wp
      et0 = et0 + (x(-8 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-8 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-8 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-8 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.156915542257710856911416E-6_wp
      filter_val1 = -0.24646355857433108358764E-7_wp
      filter_val2 = -0.276667298808735176085978E-6_wp
      filter_val3 = 0.44109215856612868615909E-7_wp
      et0 = et0 + (x(-7 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-7 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-7 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-7 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.2075567562651587632761451E-5_wp
      filter_val1 = -0.40033729064957944938397E-6_wp
      filter_val2 = -0.3318610754044407103712689E-5_wp
      filter_val3 = 0.652118308986595532963584E-6_wp
      et0 = et0 + (x(-6 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-6 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-6 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-6 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.25057266882581997397910903E-4_wp
      filter_val1 = -0.14147525211040058739651301E-4_wp
      filter_val2 = -0.40668222535397420851105277E-4_wp
      filter_val3 = 0.24486891695262527278480373E-4_wp
      et0 = et0 + (x(-5 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-5 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-5 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-5 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.1554774474923793899293724602E-2_wp
      filter_val1 = 0.239878289281775075165492659E-3_wp
      filter_val2 = -0.272132773540999536127987988E-2_wp
      filter_val3 = -0.464504573781934008099959241E-3_wp
      et0 = et0 + (x(-4 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-4 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-4 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-4 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.13367239421126794640208314967E-1_wp
      filter_val1 = -0.1713514567376228224101768783E-2_wp
      filter_val2 = 0.28206423027424364666613729847E-1_wp
      filter_val3 = 0.3743663199398739083068656973E-2_wp
      et0 = et0 + (x(-3 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-3 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-3 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-3 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.43793326416155009500101924188E-1_wp
      filter_val1 = 0.8020884755969165545893465146E-2_wp
      filter_val2 = -0.119506731011354509446152089555E0_wp
      filter_val3 = -0.22857677317437822508458981792E-1_wp
      et0 = et0 + (x(-2 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-2 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-2 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-2 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.14743648317973476936220327736E-1_wp
      filter_val1 = -0.3131289721448565965922638841E-1_wp
      filter_val2 = 0.43810509543421050421696718266E0_wp
      filter_val3 = 0.77858113707232306275457825041E-1_wp
      et0 = et0 + (x(-1 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-1 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-1 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-1 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.780460480250744028494513806689E0_wp
      filter_val1 = 0.425987982696947193304592242583E0_wp
      filter_val2 = 0.403467221125547183465707078756E0_wp
      filter_val3 = -0.781104684144826958489597897908E0_wp
      et0 = et0 + (x(0 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(0 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(0 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(0 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.110230081678334468262552683806E0_wp
      filter_val1 = 0.416066936766212575361540821295E0_wp
      filter_val2 = -0.52935814515665507351837970092E-1_wp
      filter_val3 = 0.49247868599583189247332328007E-1_wp
      et0 = et0 + (x(1 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(1 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(1 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(1 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.2342461473539713439541159825E-1_wp
      filter_val1 = -0.138524775674051000848952544828E0_wp
      filter_val2 = 0.7288836926603960462299666138E-2_wp
      filter_val3 = -0.39920047232423474172371930475E-1_wp
      et0 = et0 + (x(2 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(2 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(2 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(2 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.4329999533120824069665375511E-2_wp
      filter_val1 = 0.318354143459706681365834462E-1_wp
      filter_val2 = -0.1960213959538483354691921355E-2_wp
      filter_val3 = 0.15281781287179308494904296254E-1_wp
      et0 = et0 + (x(3 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(3 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(3 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(3 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.547310401099738937766344803E-3_wp
      filter_val1 = -0.3454315576482209830827198831E-2_wp
      filter_val2 = 0.28479503466830096256502681E-3_wp
      filter_val3 = -0.1969945431937324753826114662E-2_wp
      et0 = et0 + (x(4 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(4 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(4 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(4 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.2848044279309673361077534E-4_wp
      filter_val1 = -0.3416290232098375589631534E-4_wp
      filter_val2 = -0.1716983589581506701252117E-4_wp
      filter_val3 = -0.11402461428743860613089559E-4_wp
      et0 = et0 + (x(5 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(5 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(5 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(5 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.1557817271407449628766103E-5_wp
      filter_val1 = 0.974884267415657697560756E-5_wp
      filter_val2 = -0.877963062125028995526077E-6_wp
      filter_val3 = 0.5589042956883420231324354E-5_wp
      et0 = et0 + (x(6 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(6 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(6 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(6 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.9225014992147408201492E-8_wp
      filter_val1 = 0.173933065619790394896889E-6_wp
      filter_val2 = -0.5154546456362229249344E-8_wp
      filter_val3 = 0.9718640758826677836603E-7_wp
      et0 = et0 + (x(7 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(7 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(7 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(7 + i1, 1, i2 + 0)) * (filter_val3)
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_fs_10_a_ld_1u1_v1_f_f_t
SUBROUTINE d_s1s0_symicomb8_fs_10_a_ld_1u1_v1_f_f_t_cost(n, ndat1, cos&
&t)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat1
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * (ndat1)
END SUBROUTINE d_s1s0_symicomb8_fs_10_a_ld_1u1_v1_f_f_t_cost
SUBROUTINE d_s1s0_symicomb8_fs_10_ay_ld_1u1_v1_f_f_t(n, ndat1, nx, ny,&
& x, y, a_y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -8
  integer(kind=4), parameter :: upfil = 7
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat1
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(-8:nx + -8 - (1), 0:1, 0:ndat1 -&
& (1)) :: x
  real(kind=8), intent(inout), dimension(0:1, 0:ny - (1), 0:ndat1 - (1&
&)) :: y
  real(kind=8), intent(in) :: a_y
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lre_fil = resh&
&ape((/ &
0.0_wp, &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lro_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hre_fil = resh&
&ape((/ &
0.0_wp, &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hro_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
!$omp parallel  default(shared) private(i1, i2, l, et0, ot0, filt&
!$omp&er_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i2 = 0, ndat1 - (1), 1
    do i1 = 0, n - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = 0.1593875388117163824756E-8_wp
      filter_val1 = 0.0_wp
      filter_val2 = -0.2852535033969307505679E-8_wp
      filter_val3 = 0.0_wp
      et0 = et0 + (x(-8 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-8 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-8 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-8 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.156915542257710856911416E-6_wp
      filter_val1 = -0.24646355857433108358764E-7_wp
      filter_val2 = -0.276667298808735176085978E-6_wp
      filter_val3 = 0.44109215856612868615909E-7_wp
      et0 = et0 + (x(-7 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-7 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-7 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-7 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.2075567562651587632761451E-5_wp
      filter_val1 = -0.40033729064957944938397E-6_wp
      filter_val2 = -0.3318610754044407103712689E-5_wp
      filter_val3 = 0.652118308986595532963584E-6_wp
      et0 = et0 + (x(-6 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-6 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-6 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-6 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.25057266882581997397910903E-4_wp
      filter_val1 = -0.14147525211040058739651301E-4_wp
      filter_val2 = -0.40668222535397420851105277E-4_wp
      filter_val3 = 0.24486891695262527278480373E-4_wp
      et0 = et0 + (x(-5 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-5 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-5 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-5 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.1554774474923793899293724602E-2_wp
      filter_val1 = 0.239878289281775075165492659E-3_wp
      filter_val2 = -0.272132773540999536127987988E-2_wp
      filter_val3 = -0.464504573781934008099959241E-3_wp
      et0 = et0 + (x(-4 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-4 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-4 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-4 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.13367239421126794640208314967E-1_wp
      filter_val1 = -0.1713514567376228224101768783E-2_wp
      filter_val2 = 0.28206423027424364666613729847E-1_wp
      filter_val3 = 0.3743663199398739083068656973E-2_wp
      et0 = et0 + (x(-3 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-3 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-3 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-3 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.43793326416155009500101924188E-1_wp
      filter_val1 = 0.8020884755969165545893465146E-2_wp
      filter_val2 = -0.119506731011354509446152089555E0_wp
      filter_val3 = -0.22857677317437822508458981792E-1_wp
      et0 = et0 + (x(-2 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-2 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-2 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-2 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.14743648317973476936220327736E-1_wp
      filter_val1 = -0.3131289721448565965922638841E-1_wp
      filter_val2 = 0.43810509543421050421696718266E0_wp
      filter_val3 = 0.77858113707232306275457825041E-1_wp
      et0 = et0 + (x(-1 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-1 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-1 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-1 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.780460480250744028494513806689E0_wp
      filter_val1 = 0.425987982696947193304592242583E0_wp
      filter_val2 = 0.403467221125547183465707078756E0_wp
      filter_val3 = -0.781104684144826958489597897908E0_wp
      et0 = et0 + (x(0 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(0 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(0 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(0 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.110230081678334468262552683806E0_wp
      filter_val1 = 0.416066936766212575361540821295E0_wp
      filter_val2 = -0.52935814515665507351837970092E-1_wp
      filter_val3 = 0.49247868599583189247332328007E-1_wp
      et0 = et0 + (x(1 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(1 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(1 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(1 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.2342461473539713439541159825E-1_wp
      filter_val1 = -0.138524775674051000848952544828E0_wp
      filter_val2 = 0.7288836926603960462299666138E-2_wp
      filter_val3 = -0.39920047232423474172371930475E-1_wp
      et0 = et0 + (x(2 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(2 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(2 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(2 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.4329999533120824069665375511E-2_wp
      filter_val1 = 0.318354143459706681365834462E-1_wp
      filter_val2 = -0.1960213959538483354691921355E-2_wp
      filter_val3 = 0.15281781287179308494904296254E-1_wp
      et0 = et0 + (x(3 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(3 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(3 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(3 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.547310401099738937766344803E-3_wp
      filter_val1 = -0.3454315576482209830827198831E-2_wp
      filter_val2 = 0.28479503466830096256502681E-3_wp
      filter_val3 = -0.1969945431937324753826114662E-2_wp
      et0 = et0 + (x(4 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(4 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(4 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(4 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.2848044279309673361077534E-4_wp
      filter_val1 = -0.3416290232098375589631534E-4_wp
      filter_val2 = -0.1716983589581506701252117E-4_wp
      filter_val3 = -0.11402461428743860613089559E-4_wp
      et0 = et0 + (x(5 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(5 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(5 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(5 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.1557817271407449628766103E-5_wp
      filter_val1 = 0.974884267415657697560756E-5_wp
      filter_val2 = -0.877963062125028995526077E-6_wp
      filter_val3 = 0.5589042956883420231324354E-5_wp
      et0 = et0 + (x(6 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(6 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(6 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(6 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.9225014992147408201492E-8_wp
      filter_val1 = 0.173933065619790394896889E-6_wp
      filter_val2 = -0.5154546456362229249344E-8_wp
      filter_val3 = 0.9718640758826677836603E-7_wp
      et0 = et0 + (x(7 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(7 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(7 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(7 + i1, 1, i2 + 0)) * (filter_val3)
      et0 = et0 + (y(0, i1, i2 + 0)) * (a_y)
      ot0 = ot0 + (y(1, i1, i2 + 0)) * (a_y)
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_fs_10_ay_ld_1u1_v1_f_f_t
SUBROUTINE d_s1s0_symicomb8_fs_10_ay_ld_1u1_v1_f_f_t_cost(n, ndat1, co&
&st)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat1
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * (ndat1)
END SUBROUTINE d_s1s0_symicomb8_fs_10_ay_ld_1u1_v1_f_f_t_cost
SUBROUTINE d_s1s0_symicomb8_fs_10_a_ay_ld_1u1_v1_f_f_t(n, ndat1, nx, n&
&y, x, y, a, a_y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -8
  integer(kind=4), parameter :: upfil = 7
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat1
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(-8:nx + -8 - (1), 0:1, 0:ndat1 -&
& (1)) :: x
  real(kind=8), intent(inout), dimension(0:1, 0:ny - (1), 0:ndat1 - (1&
&)) :: y
  real(kind=8), intent(in) :: a
  real(kind=8), intent(in) :: a_y
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lre_fil = resh&
&ape((/ &
0.0_wp, &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lro_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hre_fil = resh&
&ape((/ &
0.0_wp, &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hro_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
!$omp parallel  default(shared) private(i1, i2, l, et0, ot0, filt&
!$omp&er_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i2 = 0, ndat1 - (1), 1
    do i1 = 0, n - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = 0.1593875388117163824756E-8_wp
      filter_val1 = 0.0_wp
      filter_val2 = -0.2852535033969307505679E-8_wp
      filter_val3 = 0.0_wp
      et0 = et0 + (x(-8 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-8 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-8 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-8 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.156915542257710856911416E-6_wp
      filter_val1 = -0.24646355857433108358764E-7_wp
      filter_val2 = -0.276667298808735176085978E-6_wp
      filter_val3 = 0.44109215856612868615909E-7_wp
      et0 = et0 + (x(-7 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-7 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-7 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-7 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.2075567562651587632761451E-5_wp
      filter_val1 = -0.40033729064957944938397E-6_wp
      filter_val2 = -0.3318610754044407103712689E-5_wp
      filter_val3 = 0.652118308986595532963584E-6_wp
      et0 = et0 + (x(-6 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-6 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-6 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-6 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.25057266882581997397910903E-4_wp
      filter_val1 = -0.14147525211040058739651301E-4_wp
      filter_val2 = -0.40668222535397420851105277E-4_wp
      filter_val3 = 0.24486891695262527278480373E-4_wp
      et0 = et0 + (x(-5 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-5 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-5 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-5 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.1554774474923793899293724602E-2_wp
      filter_val1 = 0.239878289281775075165492659E-3_wp
      filter_val2 = -0.272132773540999536127987988E-2_wp
      filter_val3 = -0.464504573781934008099959241E-3_wp
      et0 = et0 + (x(-4 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-4 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-4 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-4 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.13367239421126794640208314967E-1_wp
      filter_val1 = -0.1713514567376228224101768783E-2_wp
      filter_val2 = 0.28206423027424364666613729847E-1_wp
      filter_val3 = 0.3743663199398739083068656973E-2_wp
      et0 = et0 + (x(-3 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-3 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-3 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-3 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.43793326416155009500101924188E-1_wp
      filter_val1 = 0.8020884755969165545893465146E-2_wp
      filter_val2 = -0.119506731011354509446152089555E0_wp
      filter_val3 = -0.22857677317437822508458981792E-1_wp
      et0 = et0 + (x(-2 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-2 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-2 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-2 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.14743648317973476936220327736E-1_wp
      filter_val1 = -0.3131289721448565965922638841E-1_wp
      filter_val2 = 0.43810509543421050421696718266E0_wp
      filter_val3 = 0.77858113707232306275457825041E-1_wp
      et0 = et0 + (x(-1 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(-1 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(-1 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(-1 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.780460480250744028494513806689E0_wp
      filter_val1 = 0.425987982696947193304592242583E0_wp
      filter_val2 = 0.403467221125547183465707078756E0_wp
      filter_val3 = -0.781104684144826958489597897908E0_wp
      et0 = et0 + (x(0 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(0 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(0 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(0 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.110230081678334468262552683806E0_wp
      filter_val1 = 0.416066936766212575361540821295E0_wp
      filter_val2 = -0.52935814515665507351837970092E-1_wp
      filter_val3 = 0.49247868599583189247332328007E-1_wp
      et0 = et0 + (x(1 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(1 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(1 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(1 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.2342461473539713439541159825E-1_wp
      filter_val1 = -0.138524775674051000848952544828E0_wp
      filter_val2 = 0.7288836926603960462299666138E-2_wp
      filter_val3 = -0.39920047232423474172371930475E-1_wp
      et0 = et0 + (x(2 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(2 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(2 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(2 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.4329999533120824069665375511E-2_wp
      filter_val1 = 0.318354143459706681365834462E-1_wp
      filter_val2 = -0.1960213959538483354691921355E-2_wp
      filter_val3 = 0.15281781287179308494904296254E-1_wp
      et0 = et0 + (x(3 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(3 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(3 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(3 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = 0.547310401099738937766344803E-3_wp
      filter_val1 = -0.3454315576482209830827198831E-2_wp
      filter_val2 = 0.28479503466830096256502681E-3_wp
      filter_val3 = -0.1969945431937324753826114662E-2_wp
      et0 = et0 + (x(4 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(4 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(4 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(4 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.2848044279309673361077534E-4_wp
      filter_val1 = -0.3416290232098375589631534E-4_wp
      filter_val2 = -0.1716983589581506701252117E-4_wp
      filter_val3 = -0.11402461428743860613089559E-4_wp
      et0 = et0 + (x(5 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(5 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(5 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(5 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.1557817271407449628766103E-5_wp
      filter_val1 = 0.974884267415657697560756E-5_wp
      filter_val2 = -0.877963062125028995526077E-6_wp
      filter_val3 = 0.5589042956883420231324354E-5_wp
      et0 = et0 + (x(6 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(6 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(6 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(6 + i1, 1, i2 + 0)) * (filter_val3)
      filter_val0 = -0.9225014992147408201492E-8_wp
      filter_val1 = 0.173933065619790394896889E-6_wp
      filter_val2 = -0.5154546456362229249344E-8_wp
      filter_val3 = 0.9718640758826677836603E-7_wp
      et0 = et0 + (x(7 + i1, 0, i2 + 0)) * (filter_val0)
      ot0 = ot0 + (x(7 + i1, 0, i2 + 0)) * (filter_val1)
      et0 = et0 + (x(7 + i1, 1, i2 + 0)) * (filter_val2)
      ot0 = ot0 + (x(7 + i1, 1, i2 + 0)) * (filter_val3)
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      et0 = et0 + (y(0, i1, i2 + 0)) * (a_y)
      ot0 = ot0 + (y(1, i1, i2 + 0)) * (a_y)
      y(0, i1, i2 + 0) = et0
      y(1, i1, i2 + 0) = ot0
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_fs_10_a_ay_ld_1u1_v1_f_f_t
SUBROUTINE d_s1s0_symicomb8_fs_10_a_ay_ld_1u1_v1_f_f_t_cost(n, ndat1, &
&cost)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat1
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * (ndat1)
END SUBROUTINE d_s1s0_symicomb8_fs_10_a_ay_ld_1u1_v1_f_f_t_cost
SUBROUTINE d_s1s0_symicomb8_fs_01_ld_0u1_v1_f_f_t(ndat0, n, nx, ny, x,&
& y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -8
  integer(kind=4), parameter :: upfil = 7
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:ndat0 - (1), -8:nx + -8 - (1),&
& 0:1) :: x
  real(kind=8), intent(out), dimension(0:ndat0 - (1), 0:1, 0:ny - (1))&
& :: y
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lre_fil = resh&
&ape((/ &
0.0_wp, &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lro_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hre_fil = resh&
&ape((/ &
0.0_wp, &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hro_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
!$omp parallel  default(shared) private(i1, i2, l, et0, ot0, filt&
!$omp&er_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i1 = 0, ndat0 - (1), 1
    do i2 = 0, n - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = 0.1593875388117163824756E-8_wp
      filter_val1 = 0.0_wp
      filter_val2 = -0.2852535033969307505679E-8_wp
      filter_val3 = 0.0_wp
      et0 = et0 + (x(i1 + 0, -8 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -8 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -8 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -8 + i2, 1)) * (filter_val3)
      filter_val0 = 0.156915542257710856911416E-6_wp
      filter_val1 = -0.24646355857433108358764E-7_wp
      filter_val2 = -0.276667298808735176085978E-6_wp
      filter_val3 = 0.44109215856612868615909E-7_wp
      et0 = et0 + (x(i1 + 0, -7 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -7 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -7 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -7 + i2, 1)) * (filter_val3)
      filter_val0 = 0.2075567562651587632761451E-5_wp
      filter_val1 = -0.40033729064957944938397E-6_wp
      filter_val2 = -0.3318610754044407103712689E-5_wp
      filter_val3 = 0.652118308986595532963584E-6_wp
      et0 = et0 + (x(i1 + 0, -6 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -6 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -6 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -6 + i2, 1)) * (filter_val3)
      filter_val0 = 0.25057266882581997397910903E-4_wp
      filter_val1 = -0.14147525211040058739651301E-4_wp
      filter_val2 = -0.40668222535397420851105277E-4_wp
      filter_val3 = 0.24486891695262527278480373E-4_wp
      et0 = et0 + (x(i1 + 0, -5 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -5 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -5 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -5 + i2, 1)) * (filter_val3)
      filter_val0 = 0.1554774474923793899293724602E-2_wp
      filter_val1 = 0.239878289281775075165492659E-3_wp
      filter_val2 = -0.272132773540999536127987988E-2_wp
      filter_val3 = -0.464504573781934008099959241E-3_wp
      et0 = et0 + (x(i1 + 0, -4 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -4 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -4 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -4 + i2, 1)) * (filter_val3)
      filter_val0 = -0.13367239421126794640208314967E-1_wp
      filter_val1 = -0.1713514567376228224101768783E-2_wp
      filter_val2 = 0.28206423027424364666613729847E-1_wp
      filter_val3 = 0.3743663199398739083068656973E-2_wp
      et0 = et0 + (x(i1 + 0, -3 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -3 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -3 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -3 + i2, 1)) * (filter_val3)
      filter_val0 = 0.43793326416155009500101924188E-1_wp
      filter_val1 = 0.8020884755969165545893465146E-2_wp
      filter_val2 = -0.119506731011354509446152089555E0_wp
      filter_val3 = -0.22857677317437822508458981792E-1_wp
      et0 = et0 + (x(i1 + 0, -2 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -2 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -2 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -2 + i2, 1)) * (filter_val3)
      filter_val0 = -0.14743648317973476936220327736E-1_wp
      filter_val1 = -0.3131289721448565965922638841E-1_wp
      filter_val2 = 0.43810509543421050421696718266E0_wp
      filter_val3 = 0.77858113707232306275457825041E-1_wp
      et0 = et0 + (x(i1 + 0, -1 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -1 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -1 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -1 + i2, 1)) * (filter_val3)
      filter_val0 = 0.780460480250744028494513806689E0_wp
      filter_val1 = 0.425987982696947193304592242583E0_wp
      filter_val2 = 0.403467221125547183465707078756E0_wp
      filter_val3 = -0.781104684144826958489597897908E0_wp
      et0 = et0 + (x(i1 + 0, 0 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 0 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 0 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 0 + i2, 1)) * (filter_val3)
      filter_val0 = -0.110230081678334468262552683806E0_wp
      filter_val1 = 0.416066936766212575361540821295E0_wp
      filter_val2 = -0.52935814515665507351837970092E-1_wp
      filter_val3 = 0.49247868599583189247332328007E-1_wp
      et0 = et0 + (x(i1 + 0, 1 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 1 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 1 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 1 + i2, 1)) * (filter_val3)
      filter_val0 = 0.2342461473539713439541159825E-1_wp
      filter_val1 = -0.138524775674051000848952544828E0_wp
      filter_val2 = 0.7288836926603960462299666138E-2_wp
      filter_val3 = -0.39920047232423474172371930475E-1_wp
      et0 = et0 + (x(i1 + 0, 2 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 2 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 2 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 2 + i2, 1)) * (filter_val3)
      filter_val0 = -0.4329999533120824069665375511E-2_wp
      filter_val1 = 0.318354143459706681365834462E-1_wp
      filter_val2 = -0.1960213959538483354691921355E-2_wp
      filter_val3 = 0.15281781287179308494904296254E-1_wp
      et0 = et0 + (x(i1 + 0, 3 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 3 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 3 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 3 + i2, 1)) * (filter_val3)
      filter_val0 = 0.547310401099738937766344803E-3_wp
      filter_val1 = -0.3454315576482209830827198831E-2_wp
      filter_val2 = 0.28479503466830096256502681E-3_wp
      filter_val3 = -0.1969945431937324753826114662E-2_wp
      et0 = et0 + (x(i1 + 0, 4 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 4 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 4 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 4 + i2, 1)) * (filter_val3)
      filter_val0 = -0.2848044279309673361077534E-4_wp
      filter_val1 = -0.3416290232098375589631534E-4_wp
      filter_val2 = -0.1716983589581506701252117E-4_wp
      filter_val3 = -0.11402461428743860613089559E-4_wp
      et0 = et0 + (x(i1 + 0, 5 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 5 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 5 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 5 + i2, 1)) * (filter_val3)
      filter_val0 = -0.1557817271407449628766103E-5_wp
      filter_val1 = 0.974884267415657697560756E-5_wp
      filter_val2 = -0.877963062125028995526077E-6_wp
      filter_val3 = 0.5589042956883420231324354E-5_wp
      et0 = et0 + (x(i1 + 0, 6 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 6 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 6 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 6 + i2, 1)) * (filter_val3)
      filter_val0 = -0.9225014992147408201492E-8_wp
      filter_val1 = 0.173933065619790394896889E-6_wp
      filter_val2 = -0.5154546456362229249344E-8_wp
      filter_val3 = 0.9718640758826677836603E-7_wp
      et0 = et0 + (x(i1 + 0, 7 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 7 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 7 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 7 + i2, 1)) * (filter_val3)
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_fs_01_ld_0u1_v1_f_f_t
SUBROUTINE d_s1s0_symicomb8_fs_01_ld_0u1_v1_f_f_t_cost(ndat0, n, cost)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * (ndat0)
END SUBROUTINE d_s1s0_symicomb8_fs_01_ld_0u1_v1_f_f_t_cost
SUBROUTINE d_s1s0_symicomb8_fs_01_a_ld_0u1_v1_f_f_t(ndat0, n, nx, ny, &
&x, y, a)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -8
  integer(kind=4), parameter :: upfil = 7
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:ndat0 - (1), -8:nx + -8 - (1),&
& 0:1) :: x
  real(kind=8), intent(out), dimension(0:ndat0 - (1), 0:1, 0:ny - (1))&
& :: y
  real(kind=8), intent(in) :: a
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lre_fil = resh&
&ape((/ &
0.0_wp, &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lro_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hre_fil = resh&
&ape((/ &
0.0_wp, &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hro_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
!$omp parallel  default(shared) private(i1, i2, l, et0, ot0, filt&
!$omp&er_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i1 = 0, ndat0 - (1), 1
    do i2 = 0, n - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = 0.1593875388117163824756E-8_wp
      filter_val1 = 0.0_wp
      filter_val2 = -0.2852535033969307505679E-8_wp
      filter_val3 = 0.0_wp
      et0 = et0 + (x(i1 + 0, -8 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -8 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -8 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -8 + i2, 1)) * (filter_val3)
      filter_val0 = 0.156915542257710856911416E-6_wp
      filter_val1 = -0.24646355857433108358764E-7_wp
      filter_val2 = -0.276667298808735176085978E-6_wp
      filter_val3 = 0.44109215856612868615909E-7_wp
      et0 = et0 + (x(i1 + 0, -7 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -7 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -7 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -7 + i2, 1)) * (filter_val3)
      filter_val0 = 0.2075567562651587632761451E-5_wp
      filter_val1 = -0.40033729064957944938397E-6_wp
      filter_val2 = -0.3318610754044407103712689E-5_wp
      filter_val3 = 0.652118308986595532963584E-6_wp
      et0 = et0 + (x(i1 + 0, -6 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -6 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -6 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -6 + i2, 1)) * (filter_val3)
      filter_val0 = 0.25057266882581997397910903E-4_wp
      filter_val1 = -0.14147525211040058739651301E-4_wp
      filter_val2 = -0.40668222535397420851105277E-4_wp
      filter_val3 = 0.24486891695262527278480373E-4_wp
      et0 = et0 + (x(i1 + 0, -5 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -5 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -5 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -5 + i2, 1)) * (filter_val3)
      filter_val0 = 0.1554774474923793899293724602E-2_wp
      filter_val1 = 0.239878289281775075165492659E-3_wp
      filter_val2 = -0.272132773540999536127987988E-2_wp
      filter_val3 = -0.464504573781934008099959241E-3_wp
      et0 = et0 + (x(i1 + 0, -4 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -4 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -4 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -4 + i2, 1)) * (filter_val3)
      filter_val0 = -0.13367239421126794640208314967E-1_wp
      filter_val1 = -0.1713514567376228224101768783E-2_wp
      filter_val2 = 0.28206423027424364666613729847E-1_wp
      filter_val3 = 0.3743663199398739083068656973E-2_wp
      et0 = et0 + (x(i1 + 0, -3 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -3 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -3 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -3 + i2, 1)) * (filter_val3)
      filter_val0 = 0.43793326416155009500101924188E-1_wp
      filter_val1 = 0.8020884755969165545893465146E-2_wp
      filter_val2 = -0.119506731011354509446152089555E0_wp
      filter_val3 = -0.22857677317437822508458981792E-1_wp
      et0 = et0 + (x(i1 + 0, -2 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -2 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -2 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -2 + i2, 1)) * (filter_val3)
      filter_val0 = -0.14743648317973476936220327736E-1_wp
      filter_val1 = -0.3131289721448565965922638841E-1_wp
      filter_val2 = 0.43810509543421050421696718266E0_wp
      filter_val3 = 0.77858113707232306275457825041E-1_wp
      et0 = et0 + (x(i1 + 0, -1 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -1 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -1 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -1 + i2, 1)) * (filter_val3)
      filter_val0 = 0.780460480250744028494513806689E0_wp
      filter_val1 = 0.425987982696947193304592242583E0_wp
      filter_val2 = 0.403467221125547183465707078756E0_wp
      filter_val3 = -0.781104684144826958489597897908E0_wp
      et0 = et0 + (x(i1 + 0, 0 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 0 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 0 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 0 + i2, 1)) * (filter_val3)
      filter_val0 = -0.110230081678334468262552683806E0_wp
      filter_val1 = 0.416066936766212575361540821295E0_wp
      filter_val2 = -0.52935814515665507351837970092E-1_wp
      filter_val3 = 0.49247868599583189247332328007E-1_wp
      et0 = et0 + (x(i1 + 0, 1 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 1 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 1 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 1 + i2, 1)) * (filter_val3)
      filter_val0 = 0.2342461473539713439541159825E-1_wp
      filter_val1 = -0.138524775674051000848952544828E0_wp
      filter_val2 = 0.7288836926603960462299666138E-2_wp
      filter_val3 = -0.39920047232423474172371930475E-1_wp
      et0 = et0 + (x(i1 + 0, 2 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 2 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 2 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 2 + i2, 1)) * (filter_val3)
      filter_val0 = -0.4329999533120824069665375511E-2_wp
      filter_val1 = 0.318354143459706681365834462E-1_wp
      filter_val2 = -0.1960213959538483354691921355E-2_wp
      filter_val3 = 0.15281781287179308494904296254E-1_wp
      et0 = et0 + (x(i1 + 0, 3 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 3 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 3 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 3 + i2, 1)) * (filter_val3)
      filter_val0 = 0.547310401099738937766344803E-3_wp
      filter_val1 = -0.3454315576482209830827198831E-2_wp
      filter_val2 = 0.28479503466830096256502681E-3_wp
      filter_val3 = -0.1969945431937324753826114662E-2_wp
      et0 = et0 + (x(i1 + 0, 4 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 4 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 4 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 4 + i2, 1)) * (filter_val3)
      filter_val0 = -0.2848044279309673361077534E-4_wp
      filter_val1 = -0.3416290232098375589631534E-4_wp
      filter_val2 = -0.1716983589581506701252117E-4_wp
      filter_val3 = -0.11402461428743860613089559E-4_wp
      et0 = et0 + (x(i1 + 0, 5 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 5 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 5 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 5 + i2, 1)) * (filter_val3)
      filter_val0 = -0.1557817271407449628766103E-5_wp
      filter_val1 = 0.974884267415657697560756E-5_wp
      filter_val2 = -0.877963062125028995526077E-6_wp
      filter_val3 = 0.5589042956883420231324354E-5_wp
      et0 = et0 + (x(i1 + 0, 6 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 6 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 6 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 6 + i2, 1)) * (filter_val3)
      filter_val0 = -0.9225014992147408201492E-8_wp
      filter_val1 = 0.173933065619790394896889E-6_wp
      filter_val2 = -0.5154546456362229249344E-8_wp
      filter_val3 = 0.9718640758826677836603E-7_wp
      et0 = et0 + (x(i1 + 0, 7 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 7 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 7 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 7 + i2, 1)) * (filter_val3)
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_fs_01_a_ld_0u1_v1_f_f_t
SUBROUTINE d_s1s0_symicomb8_fs_01_a_ld_0u1_v1_f_f_t_cost(ndat0, n, cos&
&t)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * (ndat0)
END SUBROUTINE d_s1s0_symicomb8_fs_01_a_ld_0u1_v1_f_f_t_cost
SUBROUTINE d_s1s0_symicomb8_fs_01_ay_ld_0u1_v1_f_f_t(ndat0, n, nx, ny,&
& x, y, a_y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -8
  integer(kind=4), parameter :: upfil = 7
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:ndat0 - (1), -8:nx + -8 - (1),&
& 0:1) :: x
  real(kind=8), intent(inout), dimension(0:ndat0 - (1), 0:1, 0:ny - (1&
&)) :: y
  real(kind=8), intent(in) :: a_y
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lre_fil = resh&
&ape((/ &
0.0_wp, &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lro_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hre_fil = resh&
&ape((/ &
0.0_wp, &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hro_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
!$omp parallel  default(shared) private(i1, i2, l, et0, ot0, filt&
!$omp&er_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i1 = 0, ndat0 - (1), 1
    do i2 = 0, n - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = 0.1593875388117163824756E-8_wp
      filter_val1 = 0.0_wp
      filter_val2 = -0.2852535033969307505679E-8_wp
      filter_val3 = 0.0_wp
      et0 = et0 + (x(i1 + 0, -8 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -8 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -8 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -8 + i2, 1)) * (filter_val3)
      filter_val0 = 0.156915542257710856911416E-6_wp
      filter_val1 = -0.24646355857433108358764E-7_wp
      filter_val2 = -0.276667298808735176085978E-6_wp
      filter_val3 = 0.44109215856612868615909E-7_wp
      et0 = et0 + (x(i1 + 0, -7 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -7 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -7 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -7 + i2, 1)) * (filter_val3)
      filter_val0 = 0.2075567562651587632761451E-5_wp
      filter_val1 = -0.40033729064957944938397E-6_wp
      filter_val2 = -0.3318610754044407103712689E-5_wp
      filter_val3 = 0.652118308986595532963584E-6_wp
      et0 = et0 + (x(i1 + 0, -6 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -6 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -6 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -6 + i2, 1)) * (filter_val3)
      filter_val0 = 0.25057266882581997397910903E-4_wp
      filter_val1 = -0.14147525211040058739651301E-4_wp
      filter_val2 = -0.40668222535397420851105277E-4_wp
      filter_val3 = 0.24486891695262527278480373E-4_wp
      et0 = et0 + (x(i1 + 0, -5 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -5 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -5 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -5 + i2, 1)) * (filter_val3)
      filter_val0 = 0.1554774474923793899293724602E-2_wp
      filter_val1 = 0.239878289281775075165492659E-3_wp
      filter_val2 = -0.272132773540999536127987988E-2_wp
      filter_val3 = -0.464504573781934008099959241E-3_wp
      et0 = et0 + (x(i1 + 0, -4 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -4 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -4 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -4 + i2, 1)) * (filter_val3)
      filter_val0 = -0.13367239421126794640208314967E-1_wp
      filter_val1 = -0.1713514567376228224101768783E-2_wp
      filter_val2 = 0.28206423027424364666613729847E-1_wp
      filter_val3 = 0.3743663199398739083068656973E-2_wp
      et0 = et0 + (x(i1 + 0, -3 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -3 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -3 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -3 + i2, 1)) * (filter_val3)
      filter_val0 = 0.43793326416155009500101924188E-1_wp
      filter_val1 = 0.8020884755969165545893465146E-2_wp
      filter_val2 = -0.119506731011354509446152089555E0_wp
      filter_val3 = -0.22857677317437822508458981792E-1_wp
      et0 = et0 + (x(i1 + 0, -2 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -2 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -2 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -2 + i2, 1)) * (filter_val3)
      filter_val0 = -0.14743648317973476936220327736E-1_wp
      filter_val1 = -0.3131289721448565965922638841E-1_wp
      filter_val2 = 0.43810509543421050421696718266E0_wp
      filter_val3 = 0.77858113707232306275457825041E-1_wp
      et0 = et0 + (x(i1 + 0, -1 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -1 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -1 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -1 + i2, 1)) * (filter_val3)
      filter_val0 = 0.780460480250744028494513806689E0_wp
      filter_val1 = 0.425987982696947193304592242583E0_wp
      filter_val2 = 0.403467221125547183465707078756E0_wp
      filter_val3 = -0.781104684144826958489597897908E0_wp
      et0 = et0 + (x(i1 + 0, 0 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 0 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 0 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 0 + i2, 1)) * (filter_val3)
      filter_val0 = -0.110230081678334468262552683806E0_wp
      filter_val1 = 0.416066936766212575361540821295E0_wp
      filter_val2 = -0.52935814515665507351837970092E-1_wp
      filter_val3 = 0.49247868599583189247332328007E-1_wp
      et0 = et0 + (x(i1 + 0, 1 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 1 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 1 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 1 + i2, 1)) * (filter_val3)
      filter_val0 = 0.2342461473539713439541159825E-1_wp
      filter_val1 = -0.138524775674051000848952544828E0_wp
      filter_val2 = 0.7288836926603960462299666138E-2_wp
      filter_val3 = -0.39920047232423474172371930475E-1_wp
      et0 = et0 + (x(i1 + 0, 2 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 2 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 2 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 2 + i2, 1)) * (filter_val3)
      filter_val0 = -0.4329999533120824069665375511E-2_wp
      filter_val1 = 0.318354143459706681365834462E-1_wp
      filter_val2 = -0.1960213959538483354691921355E-2_wp
      filter_val3 = 0.15281781287179308494904296254E-1_wp
      et0 = et0 + (x(i1 + 0, 3 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 3 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 3 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 3 + i2, 1)) * (filter_val3)
      filter_val0 = 0.547310401099738937766344803E-3_wp
      filter_val1 = -0.3454315576482209830827198831E-2_wp
      filter_val2 = 0.28479503466830096256502681E-3_wp
      filter_val3 = -0.1969945431937324753826114662E-2_wp
      et0 = et0 + (x(i1 + 0, 4 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 4 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 4 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 4 + i2, 1)) * (filter_val3)
      filter_val0 = -0.2848044279309673361077534E-4_wp
      filter_val1 = -0.3416290232098375589631534E-4_wp
      filter_val2 = -0.1716983589581506701252117E-4_wp
      filter_val3 = -0.11402461428743860613089559E-4_wp
      et0 = et0 + (x(i1 + 0, 5 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 5 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 5 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 5 + i2, 1)) * (filter_val3)
      filter_val0 = -0.1557817271407449628766103E-5_wp
      filter_val1 = 0.974884267415657697560756E-5_wp
      filter_val2 = -0.877963062125028995526077E-6_wp
      filter_val3 = 0.5589042956883420231324354E-5_wp
      et0 = et0 + (x(i1 + 0, 6 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 6 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 6 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 6 + i2, 1)) * (filter_val3)
      filter_val0 = -0.9225014992147408201492E-8_wp
      filter_val1 = 0.173933065619790394896889E-6_wp
      filter_val2 = -0.5154546456362229249344E-8_wp
      filter_val3 = 0.9718640758826677836603E-7_wp
      et0 = et0 + (x(i1 + 0, 7 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 7 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 7 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 7 + i2, 1)) * (filter_val3)
      et0 = et0 + (y(i1 + 0, 0, i2)) * (a_y)
      ot0 = ot0 + (y(i1 + 0, 1, i2)) * (a_y)
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_fs_01_ay_ld_0u1_v1_f_f_t
SUBROUTINE d_s1s0_symicomb8_fs_01_ay_ld_0u1_v1_f_f_t_cost(ndat0, n, co&
&st)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * (ndat0)
END SUBROUTINE d_s1s0_symicomb8_fs_01_ay_ld_0u1_v1_f_f_t_cost
SUBROUTINE d_s1s0_symicomb8_fs_01_a_ay_ld_0u1_v1_f_f_t(ndat0, n, nx, n&
&y, x, y, a, a_y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -8
  integer(kind=4), parameter :: upfil = 7
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:ndat0 - (1), -8:nx + -8 - (1),&
& 0:1) :: x
  real(kind=8), intent(inout), dimension(0:ndat0 - (1), 0:1, 0:ny - (1&
&)) :: y
  real(kind=8), intent(in) :: a
  real(kind=8), intent(in) :: a_y
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lre_fil = resh&
&ape((/ &
0.0_wp, &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lro_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hre_fil = resh&
&ape((/ &
0.0_wp, &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hro_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
!$omp parallel  default(shared) private(i1, i2, l, et0, ot0, filt&
!$omp&er_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i1 = 0, ndat0 - (1), 1
    do i2 = 0, n - (1), 1
      et0 = 0.0_wp
      ot0 = 0.0_wp
      filter_val0 = 0.1593875388117163824756E-8_wp
      filter_val1 = 0.0_wp
      filter_val2 = -0.2852535033969307505679E-8_wp
      filter_val3 = 0.0_wp
      et0 = et0 + (x(i1 + 0, -8 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -8 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -8 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -8 + i2, 1)) * (filter_val3)
      filter_val0 = 0.156915542257710856911416E-6_wp
      filter_val1 = -0.24646355857433108358764E-7_wp
      filter_val2 = -0.276667298808735176085978E-6_wp
      filter_val3 = 0.44109215856612868615909E-7_wp
      et0 = et0 + (x(i1 + 0, -7 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -7 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -7 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -7 + i2, 1)) * (filter_val3)
      filter_val0 = 0.2075567562651587632761451E-5_wp
      filter_val1 = -0.40033729064957944938397E-6_wp
      filter_val2 = -0.3318610754044407103712689E-5_wp
      filter_val3 = 0.652118308986595532963584E-6_wp
      et0 = et0 + (x(i1 + 0, -6 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -6 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -6 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -6 + i2, 1)) * (filter_val3)
      filter_val0 = 0.25057266882581997397910903E-4_wp
      filter_val1 = -0.14147525211040058739651301E-4_wp
      filter_val2 = -0.40668222535397420851105277E-4_wp
      filter_val3 = 0.24486891695262527278480373E-4_wp
      et0 = et0 + (x(i1 + 0, -5 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -5 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -5 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -5 + i2, 1)) * (filter_val3)
      filter_val0 = 0.1554774474923793899293724602E-2_wp
      filter_val1 = 0.239878289281775075165492659E-3_wp
      filter_val2 = -0.272132773540999536127987988E-2_wp
      filter_val3 = -0.464504573781934008099959241E-3_wp
      et0 = et0 + (x(i1 + 0, -4 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -4 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -4 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -4 + i2, 1)) * (filter_val3)
      filter_val0 = -0.13367239421126794640208314967E-1_wp
      filter_val1 = -0.1713514567376228224101768783E-2_wp
      filter_val2 = 0.28206423027424364666613729847E-1_wp
      filter_val3 = 0.3743663199398739083068656973E-2_wp
      et0 = et0 + (x(i1 + 0, -3 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -3 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -3 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -3 + i2, 1)) * (filter_val3)
      filter_val0 = 0.43793326416155009500101924188E-1_wp
      filter_val1 = 0.8020884755969165545893465146E-2_wp
      filter_val2 = -0.119506731011354509446152089555E0_wp
      filter_val3 = -0.22857677317437822508458981792E-1_wp
      et0 = et0 + (x(i1 + 0, -2 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -2 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -2 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -2 + i2, 1)) * (filter_val3)
      filter_val0 = -0.14743648317973476936220327736E-1_wp
      filter_val1 = -0.3131289721448565965922638841E-1_wp
      filter_val2 = 0.43810509543421050421696718266E0_wp
      filter_val3 = 0.77858113707232306275457825041E-1_wp
      et0 = et0 + (x(i1 + 0, -1 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, -1 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, -1 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, -1 + i2, 1)) * (filter_val3)
      filter_val0 = 0.780460480250744028494513806689E0_wp
      filter_val1 = 0.425987982696947193304592242583E0_wp
      filter_val2 = 0.403467221125547183465707078756E0_wp
      filter_val3 = -0.781104684144826958489597897908E0_wp
      et0 = et0 + (x(i1 + 0, 0 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 0 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 0 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 0 + i2, 1)) * (filter_val3)
      filter_val0 = -0.110230081678334468262552683806E0_wp
      filter_val1 = 0.416066936766212575361540821295E0_wp
      filter_val2 = -0.52935814515665507351837970092E-1_wp
      filter_val3 = 0.49247868599583189247332328007E-1_wp
      et0 = et0 + (x(i1 + 0, 1 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 1 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 1 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 1 + i2, 1)) * (filter_val3)
      filter_val0 = 0.2342461473539713439541159825E-1_wp
      filter_val1 = -0.138524775674051000848952544828E0_wp
      filter_val2 = 0.7288836926603960462299666138E-2_wp
      filter_val3 = -0.39920047232423474172371930475E-1_wp
      et0 = et0 + (x(i1 + 0, 2 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 2 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 2 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 2 + i2, 1)) * (filter_val3)
      filter_val0 = -0.4329999533120824069665375511E-2_wp
      filter_val1 = 0.318354143459706681365834462E-1_wp
      filter_val2 = -0.1960213959538483354691921355E-2_wp
      filter_val3 = 0.15281781287179308494904296254E-1_wp
      et0 = et0 + (x(i1 + 0, 3 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 3 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 3 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 3 + i2, 1)) * (filter_val3)
      filter_val0 = 0.547310401099738937766344803E-3_wp
      filter_val1 = -0.3454315576482209830827198831E-2_wp
      filter_val2 = 0.28479503466830096256502681E-3_wp
      filter_val3 = -0.1969945431937324753826114662E-2_wp
      et0 = et0 + (x(i1 + 0, 4 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 4 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 4 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 4 + i2, 1)) * (filter_val3)
      filter_val0 = -0.2848044279309673361077534E-4_wp
      filter_val1 = -0.3416290232098375589631534E-4_wp
      filter_val2 = -0.1716983589581506701252117E-4_wp
      filter_val3 = -0.11402461428743860613089559E-4_wp
      et0 = et0 + (x(i1 + 0, 5 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 5 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 5 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 5 + i2, 1)) * (filter_val3)
      filter_val0 = -0.1557817271407449628766103E-5_wp
      filter_val1 = 0.974884267415657697560756E-5_wp
      filter_val2 = -0.877963062125028995526077E-6_wp
      filter_val3 = 0.5589042956883420231324354E-5_wp
      et0 = et0 + (x(i1 + 0, 6 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 6 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 6 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 6 + i2, 1)) * (filter_val3)
      filter_val0 = -0.9225014992147408201492E-8_wp
      filter_val1 = 0.173933065619790394896889E-6_wp
      filter_val2 = -0.5154546456362229249344E-8_wp
      filter_val3 = 0.9718640758826677836603E-7_wp
      et0 = et0 + (x(i1 + 0, 7 + i2, 0)) * (filter_val0)
      ot0 = ot0 + (x(i1 + 0, 7 + i2, 0)) * (filter_val1)
      et0 = et0 + (x(i1 + 0, 7 + i2, 1)) * (filter_val2)
      ot0 = ot0 + (x(i1 + 0, 7 + i2, 1)) * (filter_val3)
      et0 = (et0) * (a)
      ot0 = (ot0) * (a)
      et0 = et0 + (y(i1 + 0, 0, i2)) * (a_y)
      ot0 = ot0 + (y(i1 + 0, 1, i2)) * (a_y)
      y(i1 + 0, 0, i2) = et0
      y(i1 + 0, 1, i2) = ot0
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_fs_01_a_ay_ld_0u1_v1_f_f_t
SUBROUTINE d_s1s0_symicomb8_fs_01_a_ay_ld_0u1_v1_f_f_t_cost(ndat0, n, &
&cost)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * (ndat0)
END SUBROUTINE d_s1s0_symicomb8_fs_01_a_ay_ld_0u1_v1_f_f_t_cost
SUBROUTINE d_s1s0_symicomb8_fs_201_ld_2u1_v1_f_f_t(ndat0, n, ndat2, nx&
&, ny, x, y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -8
  integer(kind=4), parameter :: upfil = 7
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat2
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:ndat0 - (1), -8:nx + -8 - (1),&
& 0:1, 0:ndat2 - (1)) :: x
  real(kind=8), intent(out), dimension(0:ndat0 - (1), 0:1, 0:ny - (1),&
& 0:ndat2 - (1)) :: y
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lre_fil = resh&
&ape((/ &
0.0_wp, &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lro_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hre_fil = resh&
&ape((/ &
0.0_wp, &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hro_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: i3
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
!$omp parallel  default(shared) private(i1, i2, i3, l, et0, ot0, &
!$omp&filter_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i3 = 0, ndat2 - (1), 1
    do i1 = 0, ndat0 - (1), 1
      do i2 = 0, n - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        filter_val0 = 0.1593875388117163824756E-8_wp
        filter_val1 = 0.0_wp
        filter_val2 = -0.2852535033969307505679E-8_wp
        filter_val3 = 0.0_wp
        et0 = et0 + (x(i1, -8 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -8 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -8 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -8 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.156915542257710856911416E-6_wp
        filter_val1 = -0.24646355857433108358764E-7_wp
        filter_val2 = -0.276667298808735176085978E-6_wp
        filter_val3 = 0.44109215856612868615909E-7_wp
        et0 = et0 + (x(i1, -7 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -7 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -7 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -7 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.2075567562651587632761451E-5_wp
        filter_val1 = -0.40033729064957944938397E-6_wp
        filter_val2 = -0.3318610754044407103712689E-5_wp
        filter_val3 = 0.652118308986595532963584E-6_wp
        et0 = et0 + (x(i1, -6 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -6 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -6 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -6 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.25057266882581997397910903E-4_wp
        filter_val1 = -0.14147525211040058739651301E-4_wp
        filter_val2 = -0.40668222535397420851105277E-4_wp
        filter_val3 = 0.24486891695262527278480373E-4_wp
        et0 = et0 + (x(i1, -5 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -5 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -5 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -5 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.1554774474923793899293724602E-2_wp
        filter_val1 = 0.239878289281775075165492659E-3_wp
        filter_val2 = -0.272132773540999536127987988E-2_wp
        filter_val3 = -0.464504573781934008099959241E-3_wp
        et0 = et0 + (x(i1, -4 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -4 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -4 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -4 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.13367239421126794640208314967E-1_wp
        filter_val1 = -0.1713514567376228224101768783E-2_wp
        filter_val2 = 0.28206423027424364666613729847E-1_wp
        filter_val3 = 0.3743663199398739083068656973E-2_wp
        et0 = et0 + (x(i1, -3 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -3 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -3 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -3 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.43793326416155009500101924188E-1_wp
        filter_val1 = 0.8020884755969165545893465146E-2_wp
        filter_val2 = -0.119506731011354509446152089555E0_wp
        filter_val3 = -0.22857677317437822508458981792E-1_wp
        et0 = et0 + (x(i1, -2 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -2 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -2 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -2 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.14743648317973476936220327736E-1_wp
        filter_val1 = -0.3131289721448565965922638841E-1_wp
        filter_val2 = 0.43810509543421050421696718266E0_wp
        filter_val3 = 0.77858113707232306275457825041E-1_wp
        et0 = et0 + (x(i1, -1 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -1 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -1 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -1 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.780460480250744028494513806689E0_wp
        filter_val1 = 0.425987982696947193304592242583E0_wp
        filter_val2 = 0.403467221125547183465707078756E0_wp
        filter_val3 = -0.781104684144826958489597897908E0_wp
        et0 = et0 + (x(i1, 0 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 0 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 0 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 0 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.110230081678334468262552683806E0_wp
        filter_val1 = 0.416066936766212575361540821295E0_wp
        filter_val2 = -0.52935814515665507351837970092E-1_wp
        filter_val3 = 0.49247868599583189247332328007E-1_wp
        et0 = et0 + (x(i1, 1 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 1 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 1 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 1 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.2342461473539713439541159825E-1_wp
        filter_val1 = -0.138524775674051000848952544828E0_wp
        filter_val2 = 0.7288836926603960462299666138E-2_wp
        filter_val3 = -0.39920047232423474172371930475E-1_wp
        et0 = et0 + (x(i1, 2 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 2 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 2 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 2 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.4329999533120824069665375511E-2_wp
        filter_val1 = 0.318354143459706681365834462E-1_wp
        filter_val2 = -0.1960213959538483354691921355E-2_wp
        filter_val3 = 0.15281781287179308494904296254E-1_wp
        et0 = et0 + (x(i1, 3 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 3 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 3 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 3 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.547310401099738937766344803E-3_wp
        filter_val1 = -0.3454315576482209830827198831E-2_wp
        filter_val2 = 0.28479503466830096256502681E-3_wp
        filter_val3 = -0.1969945431937324753826114662E-2_wp
        et0 = et0 + (x(i1, 4 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 4 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 4 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 4 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.2848044279309673361077534E-4_wp
        filter_val1 = -0.3416290232098375589631534E-4_wp
        filter_val2 = -0.1716983589581506701252117E-4_wp
        filter_val3 = -0.11402461428743860613089559E-4_wp
        et0 = et0 + (x(i1, 5 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 5 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 5 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 5 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.1557817271407449628766103E-5_wp
        filter_val1 = 0.974884267415657697560756E-5_wp
        filter_val2 = -0.877963062125028995526077E-6_wp
        filter_val3 = 0.5589042956883420231324354E-5_wp
        et0 = et0 + (x(i1, 6 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 6 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 6 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 6 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.9225014992147408201492E-8_wp
        filter_val1 = 0.173933065619790394896889E-6_wp
        filter_val2 = -0.5154546456362229249344E-8_wp
        filter_val3 = 0.9718640758826677836603E-7_wp
        et0 = et0 + (x(i1, 7 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 7 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 7 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 7 + i2, 1, i3 + 0)) * (filter_val3)
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_fs_201_ld_2u1_v1_f_f_t
SUBROUTINE d_s1s0_symicomb8_fs_201_ld_2u1_v1_f_f_t_cost(ndat0, n, ndat&
&2, cost)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat2
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * ((ndat2) * (ndat0))
END SUBROUTINE d_s1s0_symicomb8_fs_201_ld_2u1_v1_f_f_t_cost
SUBROUTINE d_s1s0_symicomb8_fs_201_a_ld_2u1_v1_f_f_t(ndat0, n, ndat2, &
&nx, ny, x, y, a)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -8
  integer(kind=4), parameter :: upfil = 7
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat2
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:ndat0 - (1), -8:nx + -8 - (1),&
& 0:1, 0:ndat2 - (1)) :: x
  real(kind=8), intent(out), dimension(0:ndat0 - (1), 0:1, 0:ny - (1),&
& 0:ndat2 - (1)) :: y
  real(kind=8), intent(in) :: a
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lre_fil = resh&
&ape((/ &
0.0_wp, &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lro_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hre_fil = resh&
&ape((/ &
0.0_wp, &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hro_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: i3
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
!$omp parallel  default(shared) private(i1, i2, i3, l, et0, ot0, &
!$omp&filter_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i3 = 0, ndat2 - (1), 1
    do i1 = 0, ndat0 - (1), 1
      do i2 = 0, n - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        filter_val0 = 0.1593875388117163824756E-8_wp
        filter_val1 = 0.0_wp
        filter_val2 = -0.2852535033969307505679E-8_wp
        filter_val3 = 0.0_wp
        et0 = et0 + (x(i1, -8 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -8 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -8 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -8 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.156915542257710856911416E-6_wp
        filter_val1 = -0.24646355857433108358764E-7_wp
        filter_val2 = -0.276667298808735176085978E-6_wp
        filter_val3 = 0.44109215856612868615909E-7_wp
        et0 = et0 + (x(i1, -7 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -7 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -7 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -7 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.2075567562651587632761451E-5_wp
        filter_val1 = -0.40033729064957944938397E-6_wp
        filter_val2 = -0.3318610754044407103712689E-5_wp
        filter_val3 = 0.652118308986595532963584E-6_wp
        et0 = et0 + (x(i1, -6 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -6 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -6 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -6 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.25057266882581997397910903E-4_wp
        filter_val1 = -0.14147525211040058739651301E-4_wp
        filter_val2 = -0.40668222535397420851105277E-4_wp
        filter_val3 = 0.24486891695262527278480373E-4_wp
        et0 = et0 + (x(i1, -5 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -5 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -5 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -5 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.1554774474923793899293724602E-2_wp
        filter_val1 = 0.239878289281775075165492659E-3_wp
        filter_val2 = -0.272132773540999536127987988E-2_wp
        filter_val3 = -0.464504573781934008099959241E-3_wp
        et0 = et0 + (x(i1, -4 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -4 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -4 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -4 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.13367239421126794640208314967E-1_wp
        filter_val1 = -0.1713514567376228224101768783E-2_wp
        filter_val2 = 0.28206423027424364666613729847E-1_wp
        filter_val3 = 0.3743663199398739083068656973E-2_wp
        et0 = et0 + (x(i1, -3 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -3 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -3 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -3 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.43793326416155009500101924188E-1_wp
        filter_val1 = 0.8020884755969165545893465146E-2_wp
        filter_val2 = -0.119506731011354509446152089555E0_wp
        filter_val3 = -0.22857677317437822508458981792E-1_wp
        et0 = et0 + (x(i1, -2 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -2 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -2 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -2 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.14743648317973476936220327736E-1_wp
        filter_val1 = -0.3131289721448565965922638841E-1_wp
        filter_val2 = 0.43810509543421050421696718266E0_wp
        filter_val3 = 0.77858113707232306275457825041E-1_wp
        et0 = et0 + (x(i1, -1 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -1 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -1 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -1 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.780460480250744028494513806689E0_wp
        filter_val1 = 0.425987982696947193304592242583E0_wp
        filter_val2 = 0.403467221125547183465707078756E0_wp
        filter_val3 = -0.781104684144826958489597897908E0_wp
        et0 = et0 + (x(i1, 0 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 0 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 0 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 0 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.110230081678334468262552683806E0_wp
        filter_val1 = 0.416066936766212575361540821295E0_wp
        filter_val2 = -0.52935814515665507351837970092E-1_wp
        filter_val3 = 0.49247868599583189247332328007E-1_wp
        et0 = et0 + (x(i1, 1 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 1 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 1 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 1 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.2342461473539713439541159825E-1_wp
        filter_val1 = -0.138524775674051000848952544828E0_wp
        filter_val2 = 0.7288836926603960462299666138E-2_wp
        filter_val3 = -0.39920047232423474172371930475E-1_wp
        et0 = et0 + (x(i1, 2 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 2 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 2 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 2 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.4329999533120824069665375511E-2_wp
        filter_val1 = 0.318354143459706681365834462E-1_wp
        filter_val2 = -0.1960213959538483354691921355E-2_wp
        filter_val3 = 0.15281781287179308494904296254E-1_wp
        et0 = et0 + (x(i1, 3 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 3 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 3 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 3 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.547310401099738937766344803E-3_wp
        filter_val1 = -0.3454315576482209830827198831E-2_wp
        filter_val2 = 0.28479503466830096256502681E-3_wp
        filter_val3 = -0.1969945431937324753826114662E-2_wp
        et0 = et0 + (x(i1, 4 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 4 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 4 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 4 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.2848044279309673361077534E-4_wp
        filter_val1 = -0.3416290232098375589631534E-4_wp
        filter_val2 = -0.1716983589581506701252117E-4_wp
        filter_val3 = -0.11402461428743860613089559E-4_wp
        et0 = et0 + (x(i1, 5 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 5 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 5 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 5 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.1557817271407449628766103E-5_wp
        filter_val1 = 0.974884267415657697560756E-5_wp
        filter_val2 = -0.877963062125028995526077E-6_wp
        filter_val3 = 0.5589042956883420231324354E-5_wp
        et0 = et0 + (x(i1, 6 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 6 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 6 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 6 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.9225014992147408201492E-8_wp
        filter_val1 = 0.173933065619790394896889E-6_wp
        filter_val2 = -0.5154546456362229249344E-8_wp
        filter_val3 = 0.9718640758826677836603E-7_wp
        et0 = et0 + (x(i1, 7 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 7 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 7 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 7 + i2, 1, i3 + 0)) * (filter_val3)
        et0 = (et0) * (a)
        ot0 = (ot0) * (a)
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_fs_201_a_ld_2u1_v1_f_f_t
SUBROUTINE d_s1s0_symicomb8_fs_201_a_ld_2u1_v1_f_f_t_cost(ndat0, n, nd&
&at2, cost)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat2
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * ((ndat2) * (ndat0))
END SUBROUTINE d_s1s0_symicomb8_fs_201_a_ld_2u1_v1_f_f_t_cost
SUBROUTINE d_s1s0_symicomb8_fs_201_ay_ld_2u1_v1_f_f_t(ndat0, n, ndat2,&
& nx, ny, x, y, a_y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -8
  integer(kind=4), parameter :: upfil = 7
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat2
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:ndat0 - (1), -8:nx + -8 - (1),&
& 0:1, 0:ndat2 - (1)) :: x
  real(kind=8), intent(inout), dimension(0:ndat0 - (1), 0:1, 0:ny - (1&
&), 0:ndat2 - (1)) :: y
  real(kind=8), intent(in) :: a_y
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lre_fil = resh&
&ape((/ &
0.0_wp, &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lro_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hre_fil = resh&
&ape((/ &
0.0_wp, &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hro_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: i3
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
!$omp parallel  default(shared) private(i1, i2, i3, l, et0, ot0, &
!$omp&filter_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i3 = 0, ndat2 - (1), 1
    do i1 = 0, ndat0 - (1), 1
      do i2 = 0, n - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        filter_val0 = 0.1593875388117163824756E-8_wp
        filter_val1 = 0.0_wp
        filter_val2 = -0.2852535033969307505679E-8_wp
        filter_val3 = 0.0_wp
        et0 = et0 + (x(i1, -8 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -8 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -8 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -8 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.156915542257710856911416E-6_wp
        filter_val1 = -0.24646355857433108358764E-7_wp
        filter_val2 = -0.276667298808735176085978E-6_wp
        filter_val3 = 0.44109215856612868615909E-7_wp
        et0 = et0 + (x(i1, -7 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -7 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -7 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -7 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.2075567562651587632761451E-5_wp
        filter_val1 = -0.40033729064957944938397E-6_wp
        filter_val2 = -0.3318610754044407103712689E-5_wp
        filter_val3 = 0.652118308986595532963584E-6_wp
        et0 = et0 + (x(i1, -6 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -6 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -6 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -6 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.25057266882581997397910903E-4_wp
        filter_val1 = -0.14147525211040058739651301E-4_wp
        filter_val2 = -0.40668222535397420851105277E-4_wp
        filter_val3 = 0.24486891695262527278480373E-4_wp
        et0 = et0 + (x(i1, -5 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -5 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -5 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -5 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.1554774474923793899293724602E-2_wp
        filter_val1 = 0.239878289281775075165492659E-3_wp
        filter_val2 = -0.272132773540999536127987988E-2_wp
        filter_val3 = -0.464504573781934008099959241E-3_wp
        et0 = et0 + (x(i1, -4 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -4 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -4 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -4 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.13367239421126794640208314967E-1_wp
        filter_val1 = -0.1713514567376228224101768783E-2_wp
        filter_val2 = 0.28206423027424364666613729847E-1_wp
        filter_val3 = 0.3743663199398739083068656973E-2_wp
        et0 = et0 + (x(i1, -3 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -3 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -3 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -3 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.43793326416155009500101924188E-1_wp
        filter_val1 = 0.8020884755969165545893465146E-2_wp
        filter_val2 = -0.119506731011354509446152089555E0_wp
        filter_val3 = -0.22857677317437822508458981792E-1_wp
        et0 = et0 + (x(i1, -2 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -2 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -2 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -2 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.14743648317973476936220327736E-1_wp
        filter_val1 = -0.3131289721448565965922638841E-1_wp
        filter_val2 = 0.43810509543421050421696718266E0_wp
        filter_val3 = 0.77858113707232306275457825041E-1_wp
        et0 = et0 + (x(i1, -1 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -1 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -1 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -1 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.780460480250744028494513806689E0_wp
        filter_val1 = 0.425987982696947193304592242583E0_wp
        filter_val2 = 0.403467221125547183465707078756E0_wp
        filter_val3 = -0.781104684144826958489597897908E0_wp
        et0 = et0 + (x(i1, 0 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 0 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 0 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 0 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.110230081678334468262552683806E0_wp
        filter_val1 = 0.416066936766212575361540821295E0_wp
        filter_val2 = -0.52935814515665507351837970092E-1_wp
        filter_val3 = 0.49247868599583189247332328007E-1_wp
        et0 = et0 + (x(i1, 1 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 1 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 1 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 1 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.2342461473539713439541159825E-1_wp
        filter_val1 = -0.138524775674051000848952544828E0_wp
        filter_val2 = 0.7288836926603960462299666138E-2_wp
        filter_val3 = -0.39920047232423474172371930475E-1_wp
        et0 = et0 + (x(i1, 2 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 2 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 2 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 2 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.4329999533120824069665375511E-2_wp
        filter_val1 = 0.318354143459706681365834462E-1_wp
        filter_val2 = -0.1960213959538483354691921355E-2_wp
        filter_val3 = 0.15281781287179308494904296254E-1_wp
        et0 = et0 + (x(i1, 3 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 3 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 3 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 3 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.547310401099738937766344803E-3_wp
        filter_val1 = -0.3454315576482209830827198831E-2_wp
        filter_val2 = 0.28479503466830096256502681E-3_wp
        filter_val3 = -0.1969945431937324753826114662E-2_wp
        et0 = et0 + (x(i1, 4 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 4 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 4 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 4 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.2848044279309673361077534E-4_wp
        filter_val1 = -0.3416290232098375589631534E-4_wp
        filter_val2 = -0.1716983589581506701252117E-4_wp
        filter_val3 = -0.11402461428743860613089559E-4_wp
        et0 = et0 + (x(i1, 5 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 5 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 5 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 5 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.1557817271407449628766103E-5_wp
        filter_val1 = 0.974884267415657697560756E-5_wp
        filter_val2 = -0.877963062125028995526077E-6_wp
        filter_val3 = 0.5589042956883420231324354E-5_wp
        et0 = et0 + (x(i1, 6 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 6 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 6 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 6 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.9225014992147408201492E-8_wp
        filter_val1 = 0.173933065619790394896889E-6_wp
        filter_val2 = -0.5154546456362229249344E-8_wp
        filter_val3 = 0.9718640758826677836603E-7_wp
        et0 = et0 + (x(i1, 7 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 7 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 7 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 7 + i2, 1, i3 + 0)) * (filter_val3)
        et0 = et0 + (y(i1, 0, i2, i3 + 0)) * (a_y)
        ot0 = ot0 + (y(i1, 1, i2, i3 + 0)) * (a_y)
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_fs_201_ay_ld_2u1_v1_f_f_t
SUBROUTINE d_s1s0_symicomb8_fs_201_ay_ld_2u1_v1_f_f_t_cost(ndat0, n, n&
&dat2, cost)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat2
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * ((ndat2) * (ndat0))
END SUBROUTINE d_s1s0_symicomb8_fs_201_ay_ld_2u1_v1_f_f_t_cost
SUBROUTINE d_s1s0_symicomb8_fs_201_a_ay_ld_2u1_v1_f_f_t(ndat0, n, ndat&
&2, nx, ny, x, y, a, a_y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), parameter :: lowfil = -8
  integer(kind=4), parameter :: upfil = 7
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat2
  integer(kind=4), intent(in) :: nx
  integer(kind=4), intent(in) :: ny
  real(kind=8), intent(in), dimension(0:ndat0 - (1), -8:nx + -8 - (1),&
& 0:1, 0:ndat2 - (1)) :: x
  real(kind=8), intent(inout), dimension(0:ndat0 - (1), 0:1, 0:ny - (1&
&), 0:ndat2 - (1)) :: y
  real(kind=8), intent(in) :: a
  real(kind=8), intent(in) :: a_y
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lre_fil = resh&
&ape((/ &
0.0_wp, &
-0.24646355857433108358764E-7_wp, &
-0.40033729064957944938397E-6_wp, &
-0.14147525211040058739651301E-4_wp, &
0.239878289281775075165492659E-3_wp, &
-0.1713514567376228224101768783E-2_wp, &
0.8020884755969165545893465146E-2_wp, &
-0.3131289721448565965922638841E-1_wp, &
0.425987982696947193304592242583E0_wp, &
0.416066936766212575361540821295E0_wp, &
-0.138524775674051000848952544828E0_wp, &
0.318354143459706681365834462E-1_wp, &
-0.3454315576482209830827198831E-2_wp, &
-0.3416290232098375589631534E-4_wp, &
0.974884267415657697560756E-5_wp, &
0.173933065619790394896889E-6_wp /), shape(symicomb8_lre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_lro_fil = resh&
&ape((/ &
0.1593875388117163824756E-8_wp, &
0.156915542257710856911416E-6_wp, &
0.2075567562651587632761451E-5_wp, &
0.25057266882581997397910903E-4_wp, &
0.1554774474923793899293724602E-2_wp, &
-0.13367239421126794640208314967E-1_wp, &
0.43793326416155009500101924188E-1_wp, &
-0.14743648317973476936220327736E-1_wp, &
0.780460480250744028494513806689E0_wp, &
-0.110230081678334468262552683806E0_wp, &
0.2342461473539713439541159825E-1_wp, &
-0.4329999533120824069665375511E-2_wp, &
0.547310401099738937766344803E-3_wp, &
-0.2848044279309673361077534E-4_wp, &
-0.1557817271407449628766103E-5_wp, &
-0.9225014992147408201492E-8_wp /), shape(symicomb8_lro_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hre_fil = resh&
&ape((/ &
0.0_wp, &
0.44109215856612868615909E-7_wp, &
0.652118308986595532963584E-6_wp, &
0.24486891695262527278480373E-4_wp, &
-0.464504573781934008099959241E-3_wp, &
0.3743663199398739083068656973E-2_wp, &
-0.22857677317437822508458981792E-1_wp, &
0.77858113707232306275457825041E-1_wp, &
-0.781104684144826958489597897908E0_wp, &
0.49247868599583189247332328007E-1_wp, &
-0.39920047232423474172371930475E-1_wp, &
0.15281781287179308494904296254E-1_wp, &
-0.1969945431937324753826114662E-2_wp, &
-0.11402461428743860613089559E-4_wp, &
0.5589042956883420231324354E-5_wp, &
0.9718640758826677836603E-7_wp /), shape(symicomb8_hre_fil))
  real(kind=8), parameter, dimension(-8:7) :: symicomb8_hro_fil = resh&
&ape((/ &
-0.2852535033969307505679E-8_wp, &
-0.276667298808735176085978E-6_wp, &
-0.3318610754044407103712689E-5_wp, &
-0.40668222535397420851105277E-4_wp, &
-0.272132773540999536127987988E-2_wp, &
0.28206423027424364666613729847E-1_wp, &
-0.119506731011354509446152089555E0_wp, &
0.43810509543421050421696718266E0_wp, &
0.403467221125547183465707078756E0_wp, &
-0.52935814515665507351837970092E-1_wp, &
0.7288836926603960462299666138E-2_wp, &
-0.1960213959538483354691921355E-2_wp, &
0.28479503466830096256502681E-3_wp, &
-0.1716983589581506701252117E-4_wp, &
-0.877963062125028995526077E-6_wp, &
-0.5154546456362229249344E-8_wp /), shape(symicomb8_hro_fil))
  integer(kind=4) :: i1
  integer(kind=4) :: i2
  integer(kind=4) :: i3
  integer(kind=4) :: l
  real(kind=8) :: et0
  real(kind=8) :: ot0
  real(kind=8) :: filter_val0
  real(kind=8) :: filter_val1
  real(kind=8) :: filter_val2
  real(kind=8) :: filter_val3
!$omp parallel  default(shared) private(i1, i2, i3, l, et0, ot0, &
!$omp&filter_val0, filter_val1, filter_val2, filter_val3)
!$omp do 
  do i3 = 0, ndat2 - (1), 1
    do i1 = 0, ndat0 - (1), 1
      do i2 = 0, n - (1), 1
        et0 = 0.0_wp
        ot0 = 0.0_wp
        filter_val0 = 0.1593875388117163824756E-8_wp
        filter_val1 = 0.0_wp
        filter_val2 = -0.2852535033969307505679E-8_wp
        filter_val3 = 0.0_wp
        et0 = et0 + (x(i1, -8 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -8 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -8 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -8 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.156915542257710856911416E-6_wp
        filter_val1 = -0.24646355857433108358764E-7_wp
        filter_val2 = -0.276667298808735176085978E-6_wp
        filter_val3 = 0.44109215856612868615909E-7_wp
        et0 = et0 + (x(i1, -7 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -7 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -7 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -7 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.2075567562651587632761451E-5_wp
        filter_val1 = -0.40033729064957944938397E-6_wp
        filter_val2 = -0.3318610754044407103712689E-5_wp
        filter_val3 = 0.652118308986595532963584E-6_wp
        et0 = et0 + (x(i1, -6 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -6 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -6 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -6 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.25057266882581997397910903E-4_wp
        filter_val1 = -0.14147525211040058739651301E-4_wp
        filter_val2 = -0.40668222535397420851105277E-4_wp
        filter_val3 = 0.24486891695262527278480373E-4_wp
        et0 = et0 + (x(i1, -5 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -5 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -5 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -5 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.1554774474923793899293724602E-2_wp
        filter_val1 = 0.239878289281775075165492659E-3_wp
        filter_val2 = -0.272132773540999536127987988E-2_wp
        filter_val3 = -0.464504573781934008099959241E-3_wp
        et0 = et0 + (x(i1, -4 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -4 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -4 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -4 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.13367239421126794640208314967E-1_wp
        filter_val1 = -0.1713514567376228224101768783E-2_wp
        filter_val2 = 0.28206423027424364666613729847E-1_wp
        filter_val3 = 0.3743663199398739083068656973E-2_wp
        et0 = et0 + (x(i1, -3 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -3 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -3 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -3 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.43793326416155009500101924188E-1_wp
        filter_val1 = 0.8020884755969165545893465146E-2_wp
        filter_val2 = -0.119506731011354509446152089555E0_wp
        filter_val3 = -0.22857677317437822508458981792E-1_wp
        et0 = et0 + (x(i1, -2 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -2 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -2 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -2 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.14743648317973476936220327736E-1_wp
        filter_val1 = -0.3131289721448565965922638841E-1_wp
        filter_val2 = 0.43810509543421050421696718266E0_wp
        filter_val3 = 0.77858113707232306275457825041E-1_wp
        et0 = et0 + (x(i1, -1 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, -1 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, -1 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, -1 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.780460480250744028494513806689E0_wp
        filter_val1 = 0.425987982696947193304592242583E0_wp
        filter_val2 = 0.403467221125547183465707078756E0_wp
        filter_val3 = -0.781104684144826958489597897908E0_wp
        et0 = et0 + (x(i1, 0 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 0 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 0 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 0 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.110230081678334468262552683806E0_wp
        filter_val1 = 0.416066936766212575361540821295E0_wp
        filter_val2 = -0.52935814515665507351837970092E-1_wp
        filter_val3 = 0.49247868599583189247332328007E-1_wp
        et0 = et0 + (x(i1, 1 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 1 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 1 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 1 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.2342461473539713439541159825E-1_wp
        filter_val1 = -0.138524775674051000848952544828E0_wp
        filter_val2 = 0.7288836926603960462299666138E-2_wp
        filter_val3 = -0.39920047232423474172371930475E-1_wp
        et0 = et0 + (x(i1, 2 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 2 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 2 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 2 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.4329999533120824069665375511E-2_wp
        filter_val1 = 0.318354143459706681365834462E-1_wp
        filter_val2 = -0.1960213959538483354691921355E-2_wp
        filter_val3 = 0.15281781287179308494904296254E-1_wp
        et0 = et0 + (x(i1, 3 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 3 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 3 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 3 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = 0.547310401099738937766344803E-3_wp
        filter_val1 = -0.3454315576482209830827198831E-2_wp
        filter_val2 = 0.28479503466830096256502681E-3_wp
        filter_val3 = -0.1969945431937324753826114662E-2_wp
        et0 = et0 + (x(i1, 4 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 4 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 4 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 4 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.2848044279309673361077534E-4_wp
        filter_val1 = -0.3416290232098375589631534E-4_wp
        filter_val2 = -0.1716983589581506701252117E-4_wp
        filter_val3 = -0.11402461428743860613089559E-4_wp
        et0 = et0 + (x(i1, 5 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 5 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 5 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 5 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.1557817271407449628766103E-5_wp
        filter_val1 = 0.974884267415657697560756E-5_wp
        filter_val2 = -0.877963062125028995526077E-6_wp
        filter_val3 = 0.5589042956883420231324354E-5_wp
        et0 = et0 + (x(i1, 6 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 6 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 6 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 6 + i2, 1, i3 + 0)) * (filter_val3)
        filter_val0 = -0.9225014992147408201492E-8_wp
        filter_val1 = 0.173933065619790394896889E-6_wp
        filter_val2 = -0.5154546456362229249344E-8_wp
        filter_val3 = 0.9718640758826677836603E-7_wp
        et0 = et0 + (x(i1, 7 + i2, 0, i3 + 0)) * (filter_val0)
        ot0 = ot0 + (x(i1, 7 + i2, 0, i3 + 0)) * (filter_val1)
        et0 = et0 + (x(i1, 7 + i2, 1, i3 + 0)) * (filter_val2)
        ot0 = ot0 + (x(i1, 7 + i2, 1, i3 + 0)) * (filter_val3)
        et0 = (et0) * (a)
        ot0 = (ot0) * (a)
        et0 = et0 + (y(i1, 0, i2, i3 + 0)) * (a_y)
        ot0 = ot0 + (y(i1, 1, i2, i3 + 0)) * (a_y)
        y(i1, 0, i2, i3 + 0) = et0
        y(i1, 1, i2, i3 + 0) = ot0
      end do
    end do
  end do
!$omp end do 
!$omp end parallel 
END SUBROUTINE d_s1s0_symicomb8_fs_201_a_ay_ld_2u1_v1_f_f_t
SUBROUTINE d_s1s0_symicomb8_fs_201_a_ay_ld_2u1_v1_f_f_t_cost(ndat0, n,&
& ndat2, cost)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: ndat0
  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: ndat2
  integer(kind=4), intent(out) :: cost
  cost = ((n) * (128)) * ((ndat2) * (ndat0))
END SUBROUTINE d_s1s0_symicomb8_fs_201_a_ay_ld_2u1_v1_f_f_t_cost
SUBROUTINE d_s1s0_1d_symicomb8_cost(d, idim, n, bc, nx, ny, narr, x, y&
&, a, a_y, cost)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: d
  integer(kind=4), intent(in) :: idim
  integer(kind=4), intent(in), dimension(0:d - (1)) :: n
  integer(kind=4), intent(in) :: bc
  integer(kind=4), intent(in), dimension(0:d - (1)) :: nx
  integer(kind=4), intent(in), dimension(0:d - (1)) :: ny
  integer(kind=4), intent(in) :: narr
  real(kind=8), intent(in), dimension(*) :: x
  real(kind=8), intent(inout), dimension(*) :: y
  real(kind=8), intent(in) :: a
  real(kind=8), intent(in) :: a_y
  integer(kind=4), intent(out) :: cost
  integer(kind=4) :: i
  integer(kind=4) :: ndat_left
  integer(kind=4) :: ndat_right
  integer(kind=4) :: c
  integer(kind=4) :: nti
  integer(kind=4) :: nto
  integer(kind=4) :: j
  nti = nx(idim)
  nto = ny(idim)
  nti = (nti) * (2)
  nto = (nto) * (2)
  if (idim == 0) then
    ndat_right = 1
    do i = 1, d - (1), 1
      ndat_right = (ndat_right) * (n(i))
      ndat_right = (ndat_right) * (2)
    end do
    nti = (nti) * (ndat_right)
    nto = (nto) * (ndat_right)
    select case (bc)
      case (0)
        if (a == 1.0_wp) then
          if (a_y == 0.0_wp) then
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_p_10_ld_1u1_v1_t_f_t_cost(n(idim),&
& ndat_right,  c)
              cost = cost + c
            end do
          else
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_p_10_ay_ld_1u1_v1_t_f_t_cost(n(idi&
&m), ndat_right,  c)
              cost = cost + c
            end do
          end if
        else
          if (a_y == 0.0_wp) then
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_p_10_a_ld_1u1_v1_t_f_t_cost(n(idim&
&), ndat_right,  c)
              cost = cost + c
            end do
          else
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_p_10_a_ay_ld_1u1_v1_t_f_t_cost(n(i&
&dim), ndat_right,  c)
              cost = cost + c
            end do
          end if
        end if
      case (1)
        if (a == 1.0_wp) then
          if (a_y == 0.0_wp) then
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fg_10_ld_1u1_v1_f_f_t_cost(n(idim)&
&, ndat_right,  c)
              cost = cost + c
            end do
          else
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fg_10_ay_ld_1u1_v1_f_f_t_cost(n(id&
&im), ndat_right,  c)
              cost = cost + c
            end do
          end if
        else
          if (a_y == 0.0_wp) then
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fg_10_a_ld_1u1_v1_f_f_t_cost(n(idi&
&m), ndat_right,  c)
              cost = cost + c
            end do
          else
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fg_10_a_ay_ld_1u1_v1_f_f_t_cost(n(&
&idim), ndat_right,  c)
              cost = cost + c
            end do
          end if
        end if
      case (-1)
        if (a == 1.0_wp) then
          if (a_y == 0.0_wp) then
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fs_10_ld_1u1_v1_f_f_t_cost(n(idim)&
&, ndat_right,  c)
              cost = cost + c
            end do
          else
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fs_10_ay_ld_1u1_v1_f_f_t_cost(n(id&
&im), ndat_right,  c)
              cost = cost + c
            end do
          end if
        else
          if (a_y == 0.0_wp) then
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fs_10_a_ld_1u1_v1_f_f_t_cost(n(idi&
&m), ndat_right,  c)
              cost = cost + c
            end do
          else
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fs_10_a_ay_ld_1u1_v1_f_f_t_cost(n(&
&idim), ndat_right,  c)
              cost = cost + c
            end do
          end if
        end if
    end select
  else if (idim == d - (1)) then
    ndat_left = 1
    do i = 0, d - (2), 1
      ndat_left = (ndat_left) * (n(i))
      ndat_left = (ndat_left) * (2)
    end do
    nti = (nti) * (ndat_left)
    nto = (nto) * (ndat_left)
    select case (bc)
      case (0)
        if (a == 1.0_wp) then
          if (a_y == 0.0_wp) then
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_p_01_ld_0u1_v1_t_f_t_cost(ndat_lef&
&t, n(idim),  c)
              cost = cost + c
            end do
          else
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_p_01_ay_ld_0u1_v1_t_f_t_cost(ndat_&
&left, n(idim),  c)
              cost = cost + c
            end do
          end if
        else
          if (a_y == 0.0_wp) then
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_p_01_a_ld_0u1_v1_t_f_t_cost(ndat_l&
&eft, n(idim),  c)
              cost = cost + c
            end do
          else
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_p_01_a_ay_ld_0u1_v1_t_f_t_cost(nda&
&t_left, n(idim),  c)
              cost = cost + c
            end do
          end if
        end if
      case (1)
        if (a == 1.0_wp) then
          if (a_y == 0.0_wp) then
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fg_01_ld_0u1_v1_f_f_t_cost(ndat_le&
&ft, n(idim),  c)
              cost = cost + c
            end do
          else
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fg_01_ay_ld_0u1_v1_f_f_t_cost(ndat&
&_left, n(idim),  c)
              cost = cost + c
            end do
          end if
        else
          if (a_y == 0.0_wp) then
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fg_01_a_ld_0u1_v1_f_f_t_cost(ndat_&
&left, n(idim),  c)
              cost = cost + c
            end do
          else
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fg_01_a_ay_ld_0u1_v1_f_f_t_cost(nd&
&at_left, n(idim),  c)
              cost = cost + c
            end do
          end if
        end if
      case (-1)
        if (a == 1.0_wp) then
          if (a_y == 0.0_wp) then
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fs_01_ld_0u1_v1_f_f_t_cost(ndat_le&
&ft, n(idim),  c)
              cost = cost + c
            end do
          else
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fs_01_ay_ld_0u1_v1_f_f_t_cost(ndat&
&_left, n(idim),  c)
              cost = cost + c
            end do
          end if
        else
          if (a_y == 0.0_wp) then
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fs_01_a_ld_0u1_v1_f_f_t_cost(ndat_&
&left, n(idim),  c)
              cost = cost + c
            end do
          else
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fs_01_a_ay_ld_0u1_v1_f_f_t_cost(nd&
&at_left, n(idim),  c)
              cost = cost + c
            end do
          end if
        end if
    end select
  else
    ndat_left = 1
    ndat_right = 1
    do i = 0, idim - (1), 1
      ndat_left = (ndat_left) * (n(i))
      ndat_left = (ndat_left) * (2)
    end do
    do i = idim + 1, d - (1), 1
      ndat_right = (ndat_right) * (n(i))
      ndat_right = (ndat_right) * (2)
    end do
    nti = ((nti) * (ndat_left)) * (ndat_right)
    nto = ((nto) * (ndat_left)) * (ndat_right)
    select case (bc)
      case (0)
        if (a == 1.0_wp) then
          if (a_y == 0.0_wp) then
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_p_201_ld_2u1_v1_t_f_t_cost(ndat_le&
&ft, n(idim), ndat_right,  c)
              cost = cost + c
            end do
          else
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_p_201_ay_ld_2u1_v1_t_f_t_cost(ndat&
&_left, n(idim), ndat_right,  c)
              cost = cost + c
            end do
          end if
        else
          if (a_y == 0.0_wp) then
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_p_201_a_ld_2u1_v1_t_f_t_cost(ndat_&
&left, n(idim), ndat_right,  c)
              cost = cost + c
            end do
          else
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_p_201_a_ay_ld_2u1_v1_t_f_t_cost(nd&
&at_left, n(idim), ndat_right,  c)
              cost = cost + c
            end do
          end if
        end if
      case (1)
        if (a == 1.0_wp) then
          if (a_y == 0.0_wp) then
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fg_201_ld_2u1_v1_f_f_t_cost(ndat_l&
&eft, n(idim), ndat_right,  c)
              cost = cost + c
            end do
          else
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fg_201_ay_ld_2u1_v1_f_f_t_cost(nda&
&t_left, n(idim), ndat_right,  c)
              cost = cost + c
            end do
          end if
        else
          if (a_y == 0.0_wp) then
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fg_201_a_ld_2u1_v1_f_f_t_cost(ndat&
&_left, n(idim), ndat_right,  c)
              cost = cost + c
            end do
          else
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fg_201_a_ay_ld_2u1_v1_f_f_t_cost(n&
&dat_left, n(idim), ndat_right,  c)
              cost = cost + c
            end do
          end if
        end if
      case (-1)
        if (a == 1.0_wp) then
          if (a_y == 0.0_wp) then
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fs_201_ld_2u1_v1_f_f_t_cost(ndat_l&
&eft, n(idim), ndat_right,  c)
              cost = cost + c
            end do
          else
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fs_201_ay_ld_2u1_v1_f_f_t_cost(nda&
&t_left, n(idim), ndat_right,  c)
              cost = cost + c
            end do
          end if
        else
          if (a_y == 0.0_wp) then
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fs_201_a_ld_2u1_v1_f_f_t_cost(ndat&
&_left, n(idim), ndat_right,  c)
              cost = cost + c
            end do
          else
            cost = 0
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fs_201_a_ay_ld_2u1_v1_f_f_t_cost(n&
&dat_left, n(idim), ndat_right,  c)
              cost = cost + c
            end do
          end if
        end if
    end select
  end if
END SUBROUTINE d_s1s0_1d_symicomb8_cost
SUBROUTINE d_s1s0_1d_symicomb8(d, idim, n, bc, nx, ny, narr, x, y, a, &
&a_y)
  integer, parameter :: wp=kind(1.0d0)
  integer(kind=4), intent(in) :: d
  integer(kind=4), intent(in) :: idim
  integer(kind=4), intent(in), dimension(0:d - (1)) :: n
  integer(kind=4), intent(in) :: bc
  integer(kind=4), intent(in), dimension(0:d - (1)) :: nx
  integer(kind=4), intent(in), dimension(0:d - (1)) :: ny
  integer(kind=4), intent(in) :: narr
  real(kind=8), intent(in), dimension(*) :: x
  real(kind=8), intent(inout), dimension(*) :: y
  real(kind=8), intent(in) :: a
  real(kind=8), intent(in) :: a_y
  integer(kind=4) :: i
  integer(kind=4) :: ndat_left
  integer(kind=4) :: ndat_right
  integer(kind=4) :: nti
  integer(kind=4) :: nto
  integer(kind=4) :: j
  nti = nx(idim)
  nto = ny(idim)
  nti = (nti) * (2)
  nto = (nto) * (2)
  if (idim == 0) then
    ndat_right = 1
    do i = 1, d - (1), 1
      ndat_right = (ndat_right) * (nx(i))
      ndat_right = (ndat_right) * (2)
    end do
    nti = (nti) * (ndat_right)
    nto = (nto) * (ndat_right)
    select case (bc)
      case (0)
        if (a == 1.0_wp) then
          if (a_y == 0.0_wp) then
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_p_10_ld_1u1_v1_t_f_t(n(idim), ndat&
&_right, nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto) * (j) + 1))
            end do
          else
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_p_10_ay_ld_1u1_v1_t_f_t(n(idim), n&
&dat_right, nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto) * (j) + 1&
&), a_y)
            end do
          end if
        else
          if (a_y == 0.0_wp) then
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_p_10_a_ld_1u1_v1_t_f_t(n(idim), nd&
&at_right, nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto) * (j) + 1)&
&, a)
            end do
          else
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_p_10_a_ay_ld_1u1_v1_t_f_t(n(idim),&
& ndat_right, nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto) * (j) +&
& 1), a, a_y)
            end do
          end if
        end if
      case (1)
        if (a == 1.0_wp) then
          if (a_y == 0.0_wp) then
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fg_10_ld_1u1_v1_f_f_t(n(idim), nda&
&t_right, nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto) * (j) + 1))
            end do
          else
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fg_10_ay_ld_1u1_v1_f_f_t(n(idim), &
&ndat_right, nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto) * (j) + &
&1), a_y)
            end do
          end if
        else
          if (a_y == 0.0_wp) then
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fg_10_a_ld_1u1_v1_f_f_t(n(idim), n&
&dat_right, nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto) * (j) + 1&
&), a)
            end do
          else
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fg_10_a_ay_ld_1u1_v1_f_f_t(n(idim)&
&, ndat_right, nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto) * (j) &
&+ 1), a, a_y)
            end do
          end if
        end if
      case (-1)
        if (a == 1.0_wp) then
          if (a_y == 0.0_wp) then
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fs_10_ld_1u1_v1_f_f_t(n(idim), nda&
&t_right, nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto) * (j) + 1))
            end do
          else
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fs_10_ay_ld_1u1_v1_f_f_t(n(idim), &
&ndat_right, nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto) * (j) + &
&1), a_y)
            end do
          end if
        else
          if (a_y == 0.0_wp) then
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fs_10_a_ld_1u1_v1_f_f_t(n(idim), n&
&dat_right, nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto) * (j) + 1&
&), a)
            end do
          else
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fs_10_a_ay_ld_1u1_v1_f_f_t(n(idim)&
&, ndat_right, nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto) * (j) &
&+ 1), a, a_y)
            end do
          end if
        end if
    end select
  else if (idim == d - (1)) then
    ndat_left = 1
    do i = 0, d - (2), 1
      ndat_left = (ndat_left) * (nx(i))
      ndat_left = (ndat_left) * (2)
    end do
    nti = (nti) * (ndat_left)
    nto = (nto) * (ndat_left)
    select case (bc)
      case (0)
        if (a == 1.0_wp) then
          if (a_y == 0.0_wp) then
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_p_01_ld_0u1_v1_t_f_t(ndat_left, n(&
&idim), nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto) * (j) + 1))
            end do
          else
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_p_01_ay_ld_0u1_v1_t_f_t(ndat_left,&
& n(idim), nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto) * (j) + 1)&
&, a_y)
            end do
          end if
        else
          if (a_y == 0.0_wp) then
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_p_01_a_ld_0u1_v1_t_f_t(ndat_left, &
&n(idim), nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto) * (j) + 1),&
& a)
            end do
          else
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_p_01_a_ay_ld_0u1_v1_t_f_t(ndat_lef&
&t, n(idim), nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto) * (j) + &
&1), a, a_y)
            end do
          end if
        end if
      case (1)
        if (a == 1.0_wp) then
          if (a_y == 0.0_wp) then
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fg_01_ld_0u1_v1_f_f_t(ndat_left, n&
&(idim), nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto) * (j) + 1))
            end do
          else
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fg_01_ay_ld_0u1_v1_f_f_t(ndat_left&
&, n(idim), nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto) * (j) + 1&
&), a_y)
            end do
          end if
        else
          if (a_y == 0.0_wp) then
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fg_01_a_ld_0u1_v1_f_f_t(ndat_left,&
& n(idim), nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto) * (j) + 1)&
&, a)
            end do
          else
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fg_01_a_ay_ld_0u1_v1_f_f_t(ndat_le&
&ft, n(idim), nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto) * (j) +&
& 1), a, a_y)
            end do
          end if
        end if
      case (-1)
        if (a == 1.0_wp) then
          if (a_y == 0.0_wp) then
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fs_01_ld_0u1_v1_f_f_t(ndat_left, n&
&(idim), nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto) * (j) + 1))
            end do
          else
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fs_01_ay_ld_0u1_v1_f_f_t(ndat_left&
&, n(idim), nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto) * (j) + 1&
&), a_y)
            end do
          end if
        else
          if (a_y == 0.0_wp) then
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fs_01_a_ld_0u1_v1_f_f_t(ndat_left,&
& n(idim), nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto) * (j) + 1)&
&, a)
            end do
          else
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fs_01_a_ay_ld_0u1_v1_f_f_t(ndat_le&
&ft, n(idim), nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto) * (j) +&
& 1), a, a_y)
            end do
          end if
        end if
    end select
  else
    ndat_left = 1
    ndat_right = 1
    do i = 0, idim - (1), 1
      ndat_left = (ndat_left) * (nx(i))
      ndat_left = (ndat_left) * (2)
    end do
    do i = idim + 1, d - (1), 1
      ndat_right = (ndat_right) * (nx(i))
      ndat_right = (ndat_right) * (2)
    end do
    nti = ((nti) * (ndat_left)) * (ndat_right)
    nto = ((nto) * (ndat_left)) * (ndat_right)
    select case (bc)
      case (0)
        if (a == 1.0_wp) then
          if (a_y == 0.0_wp) then
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_p_201_ld_2u1_v1_t_f_t(ndat_left, n&
&(idim), ndat_right, nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto) &
&* (j) + 1))
            end do
          else
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_p_201_ay_ld_2u1_v1_t_f_t(ndat_left&
&, n(idim), ndat_right, nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nt&
&o) * (j) + 1), a_y)
            end do
          end if
        else
          if (a_y == 0.0_wp) then
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_p_201_a_ld_2u1_v1_t_f_t(ndat_left,&
& n(idim), ndat_right, nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto&
&) * (j) + 1), a)
            end do
          else
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_p_201_a_ay_ld_2u1_v1_t_f_t(ndat_le&
&ft, n(idim), ndat_right, nx(idim), ny(idim),  x((nti) * (j) + 1),  y((&
&nto) * (j) + 1), a, a_y)
            end do
          end if
        end if
      case (1)
        if (a == 1.0_wp) then
          if (a_y == 0.0_wp) then
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fg_201_ld_2u1_v1_f_f_t(ndat_left, &
&n(idim), ndat_right, nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto)&
& * (j) + 1))
            end do
          else
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fg_201_ay_ld_2u1_v1_f_f_t(ndat_lef&
&t, n(idim), ndat_right, nx(idim), ny(idim),  x((nti) * (j) + 1),  y((n&
&to) * (j) + 1), a_y)
            end do
          end if
        else
          if (a_y == 0.0_wp) then
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fg_201_a_ld_2u1_v1_f_f_t(ndat_left&
&, n(idim), ndat_right, nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nt&
&o) * (j) + 1), a)
            end do
          else
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fg_201_a_ay_ld_2u1_v1_f_f_t(ndat_l&
&eft, n(idim), ndat_right, nx(idim), ny(idim),  x((nti) * (j) + 1),  y(&
&(nto) * (j) + 1), a, a_y)
            end do
          end if
        end if
      case (-1)
        if (a == 1.0_wp) then
          if (a_y == 0.0_wp) then
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fs_201_ld_2u1_v1_f_f_t(ndat_left, &
&n(idim), ndat_right, nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nto)&
& * (j) + 1))
            end do
          else
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fs_201_ay_ld_2u1_v1_f_f_t(ndat_lef&
&t, n(idim), ndat_right, nx(idim), ny(idim),  x((nti) * (j) + 1),  y((n&
&to) * (j) + 1), a_y)
            end do
          end if
        else
          if (a_y == 0.0_wp) then
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fs_201_a_ld_2u1_v1_f_f_t(ndat_left&
&, n(idim), ndat_right, nx(idim), ny(idim),  x((nti) * (j) + 1),  y((nt&
&o) * (j) + 1), a)
            end do
          else
            do j = 0, narr - (1), 1
              call d_s1s0_symicomb8_fs_201_a_ay_ld_2u1_v1_f_f_t(ndat_l&
&eft, n(idim), ndat_right, nx(idim), ny(idim),  x((nti) * (j) + 1),  y(&
&(nto) * (j) + 1), a, a_y)
            end do
          end if
        end if
    end select
  end if
END SUBROUTINE d_s1s0_1d_symicomb8
