      REM MODIFIED 2020-06-18
      
   20 REM Maze
   30 REM
   40 REM By David Lawrence, September 1986
   50 :
   60 REM IF PAGE > &1100 THEN PROCreloc
      
      WALL = 0
      PATH = 1
      
   70 :
   80 MODE 1 : HIMEM = PAGE + &2E00
   90 PROCinit
  100 REM REPEAT
  110 PROCreset
  120 PROCmaze
  130 REM PROCgame
  140 PROCend
  150 REM UNTIL done
  160 REM MODE 7
  170 *FX 200, 0
  180 END
  190 :
  200 DEF PROCinit
  210 VDU 19, 1, 4; 0; 19, 2, 7; 0; 19, 3, 4; 0;
  220 VDU 23; 8202; 0; 0; 0;
  230 DIM D%(4, 2), L%(13)
  240 FOR I% = 1 TO 4
  250   READ D%(I%, 1), D%(I%, 2) : NEXT
  260 FOR I% = 1 TO 13 : READ L%(I%) : NEXT
  270 VDU 23, 224, 255, 255, 255, 255, 255, 255, 255, 255
  280 VDU 23, 225, 0, 0, 0, 0, 0, 0, 0, 0
  290 VDU 23, 226, 0, 0, 60, 60, 60, 60, 0, 0
  300 VDU 23, 227, 129, 66, 36, 24, 24, 36, 66, 129
  310 VDU 23, 228, 255, 129, 189, 189, 185, 189, 189, 189
  320 VDU 23, 229, 24, 60, 24, 255, 153, 60, 36, 102
  330 done = FALSE : M% = PAGE + &2E00
  340 rnd = RND(-TIME)
  350 *FX 200, 1
  360 ENDPROC
  370 :
  380 DEF PROCpoke(xp%, yp%, by%)
  390 ?(M% + xp% + yp% * 21) = by%
  400 ENDPROC
  410 :
  420 DEF FNpeek(xp%, yp%)
  430 = ?(M% + xp% + yp% * 21)
  440 :
  450 DEF PROCreset
  460 exit = FALSE : box = FALSE : S% = 6
  470 FOR I% = 0 TO 500 STEP 4 : I%!M% = WALL
  480 NEXT
  490 out = FALSE : f% = 1
  500 VDU 26, 12
  510 GCOL 0, 2 : MOVE 16, 16 : PLOT 1, 0, 288
  520 PLOT 1, 1248, 0 : PLOT 1, 0, -288
  530 PLOT 1, -1248, 0
  540 MOVE 272, 16 : PLOT 1, 0, 288
  550 COLOUR 2 : PRINT TAB(1, 30);"Scans:6"
  560 ENDPROC
  570 :
  580 DEF PROCmaze
  590 PRINT TAB(0, 2); "Please wait, constructing maze>";SPC(99);"<=Finished when";
  600 PRINT SPC(27);"arrow gets here.";TAB(30, 2);
  610 X% = 1 : Y% = 1 : xm% = 2 : ym% = 2
  620 PROCpoke(2, 2, PATH) : N% = 99
      
  630 FOR N% = 1 TO 99
        
  640   REPEAT : ok% = TRUE
          
  650     REPEAT
  660       D% = RND(4) : xx% = X% + D%(D%, 1)
  670       yy% = Y% + D%(D%, 2)
  680     UNTIL xx% > 0 AND xx% < 11 AND yy% > 0 AND yy% < 11
          
  690     IF FNpeek(xx% * 2, yy% * 2) = PATH THEN PROCjump
  700   UNTIL ok%
        
  710   FOR I% = 1 TO 2
  720     PROCpoke(X% * 2 + I% * D%(D%, 1), Y% * 2 + I% * D%(D%, 2), PATH)
  730   NEXT
        
  740   X% = xx% : Y% = yy% : VDU 32, 62, 8
  750   IF X% > xm% xm% = X%
  760   IF Y% > ym% ym% = Y%
  770 NEXT
      
  780 PROCpoke(RND(10) * 2, 1, 4)
  790 X% = RND(10) * 2 : Y% = 20 : D% = RND(3) + 1
  800 REPEAT
  810   tx% = RND(10) * 2 : ty% = RND(10) * 2
  820 UNTIL tx% <> X% AND ty% <> Y%
  830 PROCpoke(tx%, ty%, 2)
  840 ENDPROC
  850 :
  860 DEF PROCjump
  870 REPEAT
  880   X% = RND(xm%) : Y% = RND(ym%) : ok% = FALSE
  890 UNTIL FNpeek(X% * 2, Y% * 2) = PATH
  900 ENDPROC
  910 :
  920 DEF PROCgame
  930 TIME = 0
  940 REPEAT
  950   PROCview(X%, Y%, D%)
  960   R% = FNpeek(X%, Y%)
  970   IF R% = 2 PROCboxfound
  980   IF R% = 4 PROCexitfound
  990   IF NOT out PROCmove
 1000 UNTIL out
 1010 ENDPROC
 1020 :
 1030 DEF PROCview(x%, y%, d%)
 1040 VDU 26, 28, 0, 21, 39, 0, 12, 26
 1050 VDU 29, 640; 800;
 1060 GCOL 0, 2
 1070 MOVE 639, 200 : PLOT 1, 0, -680
 1080 PLOT 1, -1279, 0 : PLOT 1, 0, 680
 1090 PLOT 1, 1279, 0
 1100 VDU 24, -634; -476; 634; 196;
 1110 xa% = D%(d%, 1) : ya% = D%(d%, 2)
 1120 N% = 2 : x1% = L%(1)
 1130 REPEAT
 1140   x2% = L%(N%) : R% = FNpeek(x%, y%)
 1150   N% = FNroom(x%, y%, d%, R%)
 1160   x1% = x2% : x% = x% + xa% : y% = y% + ya%
 1170 UNTIL N% = 13 OR N% = 0
 1180 ENDPROC
 1190 :
 1200 DEF FNroom(x%, y%, d%, R%)
 1210 IF R% = 0 THEN = 0
 1220 y1% = -x1% * .75 : y2% = -x2% * .75
 1230 y3% = x2% * .3125 : y4% = x1% * .3125
 1240 FOR i% = -1 TO 1
 1250   PROCside(x%, y%, i%)
 1260 NEXT
 1270 IF FNpeek(x% + D%(d%, 1), y% + D%(d%, 2)) = 0 PROCendwall
 1280 IF R% = 2 PROCbox
 1290 IF R% = 3 PROCcross
 1300 IF R% = 4 PROCexit
 1310 = N% + 1
 1320 :
 1330 DEF PROCside(ax%, ay%, i%)
 1340 ax% = ax% + D%(FNturn(d% - i%), 1)
 1350 ay% = ay% + D%(FNturn(d% - i%), 2)
 1360 r1% = FNpeek(ax%, ay%)
 1370 IF r1% > 0 AND i% = 0 ENDPROC
 1380 IF r1% = 0 PROCwall(i%) : ENDPROC
 1390 PROCopen(i%)
 1400 IF r1% = 3 PROCsidecross(i%)
 1410 ENDPROC
 1420 :
 1430 DEF FNturn( q% ) = ( q% + 1 ) MOD 4 + 1
 1440 :
 1450 DEF PROCwall( i% )
 1460 IF i% = 0 ENDPROC
 1470 GCOL 0, d% + i%
 1480 IF f% MOVE x1% * i%, y1% : MOVE x1% * i%, y4% : PLOT 85, x2% * i%, y2% : PLOT 85, x2% * i%, y3%
 1490 GCOL 0,2
 1500 MOVE x1% * i%, y1% : DRAW x1% * i%, y4%
 1510 DRAW x2% * i%, y3% : DRAW x2% * i%, y2%
 1520 DRAW x1% * i%, y1%
 1530 ENDPROC
 1540 :
 1550 DEF PROCendwall
 1560 GCOL 0, d%
 1570 IF f% MOVE -x2%, y2% : MOVE x2%, y2% : PLOT 85, -x2%, y3% : PLOT 85, x2%, y3%
 1580 GCOL 0,2
 1590 MOVE -x2%, y2% : DRAW x2%, y2%
 1600 DRAW x2%, y3% : DRAW -x2%, y3%
 1610 DRAW -x2%, y2%
 1620 N% = 0
 1630 ENDPROC
 1640 :
 1650 DEF PROCopen(i%)
 1660 GCOL 0, d%
 1670 IF f% MOVE x2% * i%, y2% : MOVE x1% * i%, y2% : PLOT 85, x2% * i%, y3% : PLOT 85, x1% * i%, y3%
 1680 GCOL 0, 2
 1690 MOVE x2% * i%, y2% : DRAW x1% * i%, y2%
 1700 DRAW x1% * i%, y3% : DRAW x2% * i%, y3%
 1710 DRAW x2% * i%, y2%
 1720 ENDPROC
 1730 :
 1740 DEF PROCcross
 1750 GCOL 0, 2
 1760 MOVE -x2%, y2% : DRAW x1%, y1%
 1770 MOVE -x1%, y1% : DRAW x2%, y2%
 1780 ENDPROC
 1790 :
 1800 DEF PROCsidecross( i% )
 1810 GCOL 0, 2
 1820 MOVE x2% * i%, y2%
 1830 DRAW x1% * i%, (7 * y2% + y1%) / 8
 1840 ENDPROC
 1850 :
 1860 DEF PROCbox
 1870 GCOL 0, 1
 1880 wi = x2% / 1.5 : he = x2% / 10
 1890 de = x2% / 6 : ba = x2% / 1.9
 1900 MOVE -wi / 2, (y2% + y1%) / 2
 1910 PLOT 0, wi, 0
 1920 PLOT 81, -wi, -he : PLOT 81, wi, 0
 1930 PLOT 0, -wi, he : PLOT 0, wi, 0
 1940 PLOT 85, -ba / 2, (y2% + y1%) / 2 + de
 1950 PLOT 81, ba, 0
 1960 GCOL 0, 2
 1970 MOVE -wi / 2, (y2% + y1%) / 2
 1980 PLOT 1, wi, 0
 1990 PLOT 1, 0, -he : PLOT 1, -wi, 0
 2000 PLOT 1, 0, he
 2010 DRAW -ba / 2, (y2% + y1%) / 2 + de
 2020 PLOT 1, ba, 0 : DRAW wi / 2, (y2% + y1%) / 2
 2030 ENDPROC
 2040 :
 2050 DEF PROCexit
 2060 IF D% <> 4 ENDPROC
 2070 wi = x2% / 1.5 : he = x2% / 1.2
 2080 GCOL 0, 0
 2090 IF f% MOVE -wi / 2, y% : PLOT 0, wi, 0 : PLOT 81, -wi, he : PLOT 81, wi, 0
 2100 GCOL 0, 2 : MOVE -wi / 2, y2%
 2110 PLOT 1, 0, he
 2120 PLOT 1, wi, 0 : PLOT 1, 0, -he
 2130 ENDPROC
 2140 :
 2150 DEF PROCmove
 2160 REPEAT : go% = TRUE : *FX 21
 2170   REPEAT : k$ = GET$
 2180   UNTIL INSTR("FCSZX/'", k$)
 2190   IF k$ = "F" f% = 1 - f% : SOUND 1, -10, 200, 1
 2200   IF k$ = "C" go% = FNdocross
 2210   IF k$ = "S" go% = FNscan
 2220   IF k$ = "Z" D% = D% - 1 : IF D% = 0 D% = 4
 2230   IF k$ = "X" D% = D% + 1 : IF D% = 5 D% = 1
 2240   IF k$ = "/" D% = D% + 2 : IF D% > 4 D% = D% - 4
 2250   IF k$ = "'" go% = FNforward
 2260 UNTIL go%
 2270 ENDPROC
 2280 :
 2290 DEF FNforward
 2300 ax% = X% + D%(D%, 1) : ay% = Y% + D%(D%, 2)
 2310 IF FNpeek(ax%, ay%) = 0 THEN = 0
 2320 X% = ax% : Y% = ay%
 2330 = 1
 2340 :
 2350 DEF FNdocross
 2360 p% = FNpeek(X%, Y%)
 2370 IF p% = 3 OR p% = 4 THEN = 0
 2380 PROCpoke(X%, Y%, 3)
 2390 GCOL 0, 2
 2400 MOVE -484, -363 : DRAW 774, -580
 2410 MOVE -774, -580 : DRAW 484, -363
 2420 = 0
 2430 :
 2440 DEF FNscan
 2450 LOCAL I%, J%
 2460 IF S% = 0 SOUND 1, -15, 20, 1 : = 0
 2470 S% = S% - 1 : PRINT TAB(7, 30);S%
 2480 VDU 28, 1, 30, 7, 23
 2490 old% = FNpeek(X%, Y%)
 2500 PROCpoke(X%, Y%, 5)
 2510 FOR I% = X% - 3 TO X% + 3
 2520   FOR J% = Y% - 3 TO Y% + 3
 2530     IF I% < 1 OR I% > 21 OR J% < 1 OR J% > 21 ch% = 225 ELSE ch% = 224 + FNpeek(I%, J%)
 2540     io% = I% - X% + 3 : jo% = J% - Y% + 3 : VDU 31
 2550     IF D% = 1 VDU io%, 6 - jo%
 2560     IF D% = 2 VDU 6 - jo%, 6 - io%
 2570     IF D% = 3 VDU 6 - io%, jo%
 2580     IF D% = 4 VDU jo%, io%
 2590     VDU ch%
 2600   NEXT : NEXT
 2610 VDU 26
 2620 PROCpoke(X%, Y%, old%)
 2630 = 0
 2640 :
 2650 DEF PROCboxfound
 2660 VDU 28, 9, 30, 38, 23
 2670 PRINT "Well done! - You have found"'"the treasure."'
 2680 IF exit PRINT "Now get back to the exit again"
 2690 IF NOT exit PRINT "Now go and find the exit"
 2700 PRINT ' "  Press <SPACE> to continue."
 2710 REPEAT
 2720 UNTIL INKEY -99 : VDU 12, 26
 2730 box = TRUE : PROCpoke(X%, Y%, 1)
 2740 ENDPROC
 2750 :
 2760 DEF PROCexitfound
 2770 IF exit AND NOT box ENDPROC
 2780 VDU 28, 9, 30, 38, 23
 2790 IF box out = TRUE : ENDPROC
 2800 PRINT "Well done! - You have found"'"the exit."'
 2810 PRINT "However, you have not found"'"the treasure. GO AND FIND IT!"
 2820 PRINT "  Press <SPACE> to continue."
 2830 REPEAT
 2840 UNTIL INKEY -99
 2850 VDU 12, 26 : exit = TRUE
 2860 ENDPROC
 2870 :
 2880 DEF PROCend
 2890 t% = TIME
 2900 VDU 28, 0, 21, 39, 0, 12, 26
 2910 VDU 28, 9, 21, 30, 0
      
 2920 FOR i% = 21 TO 1 STEP -1
 2930   FOR j% = 1 TO 21
 2940     VDU 224 + FNpeek(j%, i%)
 2950   NEXT : PRINT
 2960 NEXT
      
 2970 VDU 28, 9, 30, 38, 23
 2980 PRINT "Well done! - You have found"''"the exit.  ";
 2990 PRINT "You took >";t% DIV 360000 MOD 24;":";t% DIV 6000 MOD 60;
 3000 PRINT ".";t% DIV 100 MOD 60;"<"
 3010 PRINT '' "do you want another gane (Y/N)"
 3020 REPEAT
 3030   k$ = GET$
 3040 UNTIL k$ = "Y" OR k$ = "N"
 3050 IF k$ = "N" done = TRUE
 3060 ENDPROC
 3070 :
 3080 DEF PROCreloc
 3090 PRINT "Relocating..."
 3100 *TAPE
 3110 *KEY0 FOR X% = 0 TO (TOP - PAGE) STEP 4 : X%!&1100 = X%!PAGE : NEXT : PAGE = &1100|MOLD|MRUN|F|M
 3120 VDU 21 : *FX 138, 0, 128
 3130 END
 3140 :
 3150 DATA 0, 1, 1, 0, 0, -1, -1, 0
 3160 DATA 774, 484, 300, 208, 156
 3170 DATA 120, 96, 76, 56, 40, 28, 16, 0
