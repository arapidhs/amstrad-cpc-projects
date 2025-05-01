10 ' debug messages and debug grid scanning
20 dbgscan=1
30 RANDOMIZE TIME
40 ON ERROR GOTO 2890
50 ON BREAK GOSUB 2940
60 MODE 1:INK 0,0:INK 1,26:PAPER 0:PEN 1:BORDER 0
70 '
80 PRINT "Select mode (0-Mode0 1-Mode1 2-Mode2)";
90 a$="":WHILE a$="":a$=INKEY$:WEND
100 IF a$<>"0" AND a$<>"1" AND a$<>"2" THEN a$="1"
110 smd=VAL(a$)
120 cols=20*(2^smd):rows=25:hcols=INT(cols/2):hrows=INT(rows/2)
130 PRINT"Select move delay (1=0.5s 2=1s else=0s)";
140 a$="":WHILE a$="":a$=INKEY$:WEND
150 IF a$="1" THEN dly=500 ELSE IF a$="2" THEN dly=1000 ELSE dly=0
160 '
170 ' Screen initialization and colors
180 ' cbg bg color, cl1,cl2 cpu colors, ctx text color
190 IF smd=2 THEN INK 0,0:INK 1,26:INK 2,26:INK 3,26:cbg=0:cl1=1:cl2=1:ctx=1 ELSE INK 0,0:INK 1,2:INK 2,6:INK 3,26:cbg=0:cl1=1:cl2=2:ctx=3
200 PAPER cbg:BORDER cbg:PEN ctx:MODE smd
210 ms$="Loading...":tmp=INT(LEN(ms$)/2):tmpx=MAX(1,hcols-tmp):LOCATE tmpx,hrows:PEN ctx:PRINT ms$
220 '
230 ' Setup blocks, cpu1 cpu2 empty block, highlight block
240 IF smd=2 THEN b1$=CHR$(143):b2$=CHR$(206) ELSE b1$=CHR$(207):b2$=CHR$(207)
250 SYMBOL 240,0,0,60,60,60,60,0,0
260 eb$=CHR$(32):hb$=CHR$(240)
270 '
280 '
290 ' Initialize cpu/player stats
300 ' position, sum xy, avg xy, min and max xy
310 ' selected pos., last occupied pos.
320 ial=6:ist=0:ism=1:ivg=2:imn=3:imx=4:isl=5:ilt=6
330 DIM st(ial,1):DIM st1(ial,1):DIM st2(ial,1):
340 DIM st$(ial):RESTORE 2960:FOR i=0 TO ial:READ st$(i):NEXT
350 '
360 ' Battle probabilities array - btl():
370 ' 0-2 friendly block min,max,avg
380 ' 3-5 opposing min,max,avg
390 ' 6-8 empty min,max,avg
400 bsz=8:frn=0:frx=1:fra=2:opn=3:opx=4:opa=5:emn=6:emx=7:ema=8
410 DIM btl(bsz)
420 btl(frn)=0.05:btl(frx)=0.1:btl(fra)=(btl(frn)+btl(frx))/2
430 btl(opn)=-0.1:btl(opx)=-0.05:btl(opa)=(btl(opn)+btl(opx))/2
440 btl(emn)=0.01:btl(emx)=-0.03:btl(ema)=(btl(emn)+btl(emx))/2
450 '
460 ' Attacker and defense thresholds
470 attthres=0.3:defthres=0.3
480 '
490 ' Cumulative personality probabilities: pnprb()
500 ' 1 norm: 0.25 prob, 2 att: 0.25, 3 rnd: 0.25, 4 def: 0.25
510 psz=3:pnrm=0:patt=1:prnd=2:pdef=3
520 DIM pnprb(psz):pnprb(pnrm)=0.25:pnprb(patt)=pnprb(pnrm)+0.25:pnprb(prnd)=pnprb(patt)+0.25:pnprb(pdef)=pnprb(prnd)+0.25
530 '
540 ' Personality names pn$()
550 ' Normal, 1 Attacker, 2 Random, 3 Defender
560 psz=3
570 DIM pn$(psz)
580 IF smd=0 THEN RESTORE 2980 ELSE RESTORE 2970
590 FOR i=0 TO psz:READ pn$(i):NEXT
600 '
610 ' Assign personalities based on personality probabilities: pnprb()
620 r=RND:IF r<pnprb(pnrm) THEN pn1=pnrm ELSE IF r<pnprb(patt) THEN pn1=patt ELSE IF r<pnprb(prnd) THEN pn1=prnd ELSE pn1=pdef
630 r=RND:IF r<pnprb(pnrm) THEN pn2=pnrm ELSE IF r<pnprb(patt) THEN pn2=patt ELSE IF r<pnprb(prnd) THEN pn2=prnd ELSE pn2=pdef
640 '
650 ' Wait for key press
660 CLS:IF smd=0 THEN ms$="Press key to start" ELSE ms$="Press any key to start":GOSUB 2560:CLS
730 '
740 ' print personalities
750 id1$="CPU 1":id2$="CPU 2"
760 c1$=id1$+": "+pn$(pn1):c2$=id2$+": "+pn$(pn2)
770 c1x=MAX(LEN(c1$),LEN(c2$))+2:c1y=1:c2x=c1x:c2y=rows' aligned x location to print status on cpu rows
780 sx=1:sy=2:' location of status line
790 LOCATE 1,1:PEN cl1:PRINT c1$
800 LOCATE 1,rows:PEN cl2:PRINT c2$
801 '
802 ' Initialize grid and starting positions
803 GOSUB 2720:DIM grd(gw,gh):grd(0,0)=-1:' setup grid dimensions
804 GOSUB 2640:' draw grid border
805 GOSUB 2790:' define and draw starting positions and stats
806 blmax=(gw+gh)*2:DIM bls(blmax,1)
807 '
810 GOSUB 2510:' print block counts
820 '
830 ' Game LOOP
840 turn=1:trn=0
850 WHILE c1+c2<gw*gh
860 '
870 ' CPU turn
880 trn=trn+1:trs=0
890 IF turn=1 THEN id=id1:opp=id2:cpuclr=cl1:b$=b1$:pn=pn1:clx=c1x:cly=c1y:FOR i=0 TO ial:st(i,0)=st1(i,0):st(i,1)=st1(i,1):NEXT
900 IF turn=2 THEN id=id2:opp=id1:cpuclr=cl2:b$=b2$:pn=pn2:clx=c2x:cly=c2y:FOR i=0 TO ial:st(i,0)=st2(i,0):st(i,1)=st2(i,1):NEXT
910 LOCATE sx,sy:PRINT STRING$(cols," ")
920 prg=ROUND((c1+c2)/(gwh)*100,2)
930 PEN ctx:LOCATE sx,sy:PRINT "Turn";trn;STR$(prg);"%"
940 IF smd<>0 THEN PEN cpuclr:ms$="Thinking.." ELSE ms$=""
950 IF smd<>0 THEN  LOCATE clx,cly:PRINT ms$;
960 ON pn+1 GOSUB 1390,1430,1470,1670
970 ' Process move or fg
980 mv=0:fg=0:IF grd(tx,ty)=0 THEN mv=1 ELSE IF grd(tx,ty)=opp THEN fg=1
990 IF mv=1 THEN GOTO 1000 ELSE IF fg=1 THEN GOTO 1040 ELSE GOTO 1260
1000 GOSUB 1910:' update st after move
1010 IF smd<>0 THEN LOCATE clx,cly:PRINT SPC(LEN(ms$))
1020 IF smd<>0 THEN ms$="Moved to"+STR$(tx)+","+STR$(ty) ELSE ms$=""
1030 GOTO 1110:' print results to Screen
1040 ' Simple fg resolution
1050 GOSUB 2030:' resolve fg
1060 GOSUB 2070:' update start after fg
1070 IF smd<>0 THEN LOCATE clx,cly:PRINT SPC(LEN(ms$))
1080 IF smd<>0 THEN GOTO 1110 ELSE ms$="":GOTO 1110
1090 GOTO 1110:' print results to screen
1100 IF wn=1 THEN ms$="Won at"+STR$(tx)+","+STR$(ty) ELSE ms$="Lost at"+STR$(tx)+","+STR$(ty)
1110 ' print move or fg results to screen
1120 IF mv=1 or wn=1 THEN grd(tx,ty)=id
1130 IF mv=1 THEN hx=tx:hy=ty:GOSUB 2440:SOUND 1,200,20,15:GOSUB 2440:' highlight, play sound highlight
1140 IF fg=1 AND wn=1 THEN hx=tx:hy=ty:GOSUB 2440:SOUND 1,142,20,15:SOUND 1,95,20,15:GOSUB 2440:' highlight, play sound highlight
1150 IF fg=1 AND wn=0 THEN hx=tx:hy=ty:GOSUB 2440:SOUND 1,95,20,15:SOUND 1,125,20,15:GOSUB 2440:' highlight, play sound highlight
1160 IF grd(tx,ty)=id THEN LOCATE ofx+tx,ofy+ty:PEN cpuclr:PRINT b$;
1170 IF LEN(ms$)>0 THEN PEN cpuclr:LOCATE clx,cly:PRINT ms$
1180 GOSUB 2510:' print block counts
1190 GOSUB 2610:' delay routine
1200 LOCATE clx,cly:PRINT SPC(LEN(ms$))
1210 IF c1+c2>=gwh OR c1=0 OR c2=0 THEN GOTO 1300
1220 IF turn=1 THEN turn=2 ELSE turn=1
1230 trs=1
1240 WEND
1250 '
1260 ' error: no valid move found
1270 IF smd<>0 THEN ms$="Error: no valid move found" ELSE ms$="Err: no move found"
1280 GOSUB 2560:GOTO 1350:' print error message and exit
1290 '
1300 ' end game
1310 IF smd<>0 THEN ms$="Game Over: "+id1$+":"+STR$(c1)+" "+id2$+":"+STR$(c2) ELSE ms$="Game Over:"+MID$(STR$(c1),2)+"/"+MID$(STR$(c2),2)
1320 GOSUB 2560
1330 IF c1>c2 THEN ms$="CPU 1 wins!" ELSE IF c1<c2 THEN ms$="CPU 2 wins!" ELSE ms$="Draw!"
1340 GOSUB 2560
1350 CLS:ms$="Play again? (Y:N)":LOCATE hcols-INT(LEN(ms$)/2),hrows:INPUT"Play Again? (Y:N)",a$
1360 IF UPPER$(a$)="Y" THEN RUN ELSE GOTO 2940
1370 END
1380 '
1390 ' CPU Normal
1400 GOSUB 1470
1410 RETURN
1420 '
1430 ' CPU Attacker
1440 GOSUB 1470
1450 RETURN
1460 '
1470 ' CPU Random
1480 bx=0:by=0:tx=0:ty=0:bl=0
1490 GOSUB 1710:' populate bls with all valid moves
1500 IF bl=0 THEN GOTO 1650:' no valid move found, we should never reach this state normally
1510 r=INT(RND*bl)+1:bx=bls(r,0):by=bls(r,1)
1520 ' We found a random valid block next we need to find a valid random target
1530 IF dbgscan=1 THEN SOUND 1,1000,2,15:hx=bx:hy=by:GOSUB 2440:' highlight selected valid block
1540 bl=0
1550 FOR dx=-1 TO 1:FOR dy=-1 TO 1
1560 IF dx=0 AND dy=0 THEN GOTO 1610
1570 nx=bx+dx:ny=by+dy
1580 IF nx<1 OR nx>gw OR ny<1 OR ny>gh THEN GOTO 1610
1590 IF grd(nx,ny)<>0 AND grd(nx,ny)<>opp THEN GOTO 1610
1600 bl=bl+1:bls(bl,0)=nx:bls(bl,1)=ny
1610 NEXT dy
1620 NEXT dx
1630 IF bl=0 THEN GOTO 1650:' no valid target found, we should never normally reach this state
1640 r=INT(RND*bl)+1:tx=bls(r,0):ty=bls(r,1)
1650 RETURN
1660 '
1670 ' CPU Defender
1680 GOSUB 1470
1690 RETURN
1700 '
1710 ' Populate bls with all valid moves
1720 bl=0
1730 minx=st(imn,0):miny=st(imn,1):maxx=st(imx,0):maxy=st(imx,1)
1740 FOR x=minx TO maxx:FOR y=miny TO maxy
1750 IF  dbgscan=1 THEN SOUND 1,1000,2,15:hx=x:hy=y:GOSUB 2440:' highlight grd scanning
1760 IF bl=blmax THEN GOTO 1890:' full list populated, return
1770 IF grd(x,y)<>id THEN GOTO 1870
1780 FOR dx=-1 TO 1:FOR dy=-1 TO 1
1790 IF dx=0 AND dy=0 THEN GOTO 1850
1800 nx=x+dx:ny=y+dy
1810 IF nx<1 OR nx>gw OR ny<1 OR ny>gh THEN GOTO 1850
1820 IF grd(nx,ny)<>0 AND grd(nx,ny)<>opp THEN GOTO 1850
1830 bl=bl+1:' found valid target at nx,ny
1840 bls(bl,0)=x:bls(bl,1)=y:GOTO 1870:' valid block found move to next
1850 NEXT dy
1860 NEXT dx
1870 NEXT y
1880 NEXT x
1890 RETURN
1900 '
1910 ' update st after move
1920 IF id=id1 THEN 1930 ELSE 1970
1930 c1=c1+1:st1(ism,0)=st1(ism,0)+tx:st1(ism,1)=st1(ism,1)+ty:st1(ivg,0)=INT(st1(ism,0)/c1):st1(ivg,1)=INT(st1(ism,1)/c1)
1940 st1(imn,0)=MIN(st1(imn,0),tx):st1(imn,1)=MIN(st1(imn,1),ty):st1(imx,0)=MAX(st1(imx,0),tx):st1(imx,1)=MAX(st1(imx,1),ty)
1950 st1(ilt,0)=tx:st1(ilt,1)=ty:st1(isl,0)=bx:st1(isl,1)=by
1960 FOR i=0 TO ial:st(i,0)=st1(i,0):st(i,1)=st1(i,1):NEXT:GOTO 2010
1970 c2=c2+1:st2(ism,0)=st2(ism,0)+tx:st2(ism,1)=st2(ism,1)+ty:st2(ivg,0)=INT(st2(ism,0)/c2):st2(ivg,1)=INT(st2(ism,1)/c2)
1980 st2(imn,0)=MIN(st2(imn,0),tx):st2(imn,1)=MIN(st2(imn,1),ty):st2(imx,0)=MAX(st2(imx,0),tx):st2(imx,1)=MAX(st2(imx,1),ty)
1990 st2(ilt,0)=tx:st2(ilt,1)=ty:st2(isl,0)=bx:st2(isl,1)=by
2000 FOR i=0 TO ial:st(i,0)=st2(i,0):st(i,1)=st2(i,1):NEXT:GOTO 2010
2010 RETURN
2020 '
2030 ' resolve fg
2040 wn=0:r=RND:IF r>0.5 THEN wn=1:grd(tx,ty)=id
2050 RETURN
2060 '
2070 ' update st after fg
2080 IF fg=1 THEN IF id=id1 THEN st1(isl,0)=bx:st1(isl,1)=by ELSE st2(isl,0)=bx:st2(isl,1)=by
2090 IF wn=0 THEN GOTO 2270
2100 IF id=id1 THEN GOTO 2110 ELSE GOTO 2190
2110 c1=c1+1:st1(ism,0)=st1(ism,0)+tx:st1(ism,1)=st1(ism,1)+ty:st1(ivg,0)=INT(st1(ism,0)/c1):st1(ivg,1)=INT(st1(ism,1)/c1)
2120 st1(imn,0)=MIN(st1(imn,0),tx):st1(imn,1)=MIN(st1(imn,1),ty):st1(imx,0)=MAX(st1(imx,0),tx):st1(imx,1)=MAX(st1(imx,1),ty)
2130 st1(ilt,0)=tx:st1(ilt,1)=ty:st1(isl,0)=bx:st1(isl,1)=by
2140 FOR i=0 TO ial:st(i,0)=st1(i,0):st(i,1)=st1(i,1):NEXT
2150 c2=c2-1:IF c2>0 THEN st2(ism,0)=st2(ism,0)-tx:st2(ism,1)=st2(ism,1)-ty:st2(ivg,0)=INT(st2(ism,0)/c2):st2(ivg,1)=INT(st2(ism,1)/c2)
2990 '
2160 ' calculate min max x y if needed
2170 IF st2(imn,0)=tx OR st2(imn,1)=ty OR st2(imx,0)=tx OR st2(imx,1)=ty THEN calcid=id2:GOSUB 2290
2180 GOTO 2270
2190 c2=c2+1:st2(ism,0)=st2(ism,0)+tx:st2(ism,1)=st2(ism,1)+ty:st2(ivg,0)=INT(st2(ism,0)/c2):st2(ivg,1)=INT(st2(ism,1)/c2)
2200 st2(imn,0)=MIN(st2(imn,0),tx):st2(imn,1)=MIN(st2(imn,1),ty):st2(imx,0)=MAX(st2(imx,0),tx):st2(imx,1)=MAX(st2(imx,1),ty)
2210 st2(ilt,0)=tx:st2(ilt,1)=ty:st2(isl,0)=bx:st2(isl,1)=by
2220 FOR i=0 TO ial:st(i,0)=st2(i,0):st(i,1)=st2(i,1):NEXT
2230 c1=c1-1:IF c1>0 THEN st1(ism,0)=st1(ism,0)-tx:st1(ism,1)=st1(ism,1)-ty:st1(ivg,0)=INT(st1(ism,0)/c1):st1(ivg,1)=INT(st1(ism,1)/c1)
2240 '
2250 ' calculate min max x y if needed
2260 IF st1(imn,0)=tx OR st1(imn,1)=ty OR st1(imx,0)=tx OR st1(imx,1)=ty THEN calcid=id1:GOSUB 2290
2270 RETURN
2280 '
2290 ' recalculate minx maxx miny maxy
2300 IF calcid<>id1 AND calcid<>id2 THEN GOTO 2420
2310 IF calcid=id1 THEN cminx=st1(imn,0):cmaxx=st1(imx,0):cminy=st1(imn,1):cmaxy=st1(imx,1) ELSE cminx=st2(imn,0):cmaxx=st2(imx,0):cminy=st2(imn,1):cmaxy=st2(imx,1)
2320 imnx=0:imxx=0:imny=0:imxy=0
2330 FOR cx=cminx TO cmaxx:FOR cy=cminy TO cmaxy
2340 IF grd(cx,cy)<>calcid THEN GOTO 2390
2350 IF imnx=0 THEN imnx=cx ELSE imnx=MIN(imnx,cx)
2360 IF imxx=0 THEN imxx=cx ELSE imxx=MAX(imxx,cx)
2370 IF imny=0 THEN imny=cy ELSE imny=MIN(imny,cy)
2380 IF imxy=0 THEN imxy=cy ELSE imxy=MAX(imxy,cy)
2390 NEXT cy
2400 NEXT cx
2410 IF calcid=id1 THEN st1(imn,0)=imnx:st1(imn,1)=imny:st1(imx,0)=imxx:st1(imx,1)=imxy ELSE st2(imn,0)=imnx:st2(imn,1)=imny:st2(imx,0)=imxx:st2(imx,1)=imxy
2420 RETURN
2430 '
2440 ' highlight
2450 IF hx<1 OR hy<1 OR hx>gw OR hy>gh THEN RETURN
2460 LOCATE ofx+hx,ofy+hy:PEN cpuclr:PRINT hb$
2470 IF grd(hx,hy)=id1 THEN PEN cl1:a$=b1$ ELSE IF grd(hx,hy)=id2 THEN PEN cl2:a$=b2$ ELSE a$=eb$
2480 LOCATE ofx+hx,ofy+hy:PRINT a$
2490 RETURN
2500 '
2510 ' print block counts
2520 PEN cl1:LOCATE cols-3,c1y:PRINT USING "####";c1;
2530 PEN cl2:LOCATE cols-3,c2y:PRINT USING "####";c2;
2540 RETURN
2550 '
2560 ' print centered message
2570 tmp=INT(LEN(ms$)/2):tmpx=MAX(1,hcols-tmp):LOCATE tmpx,hrows:PEN ctx:PRINT ms$:tmp=LEN(ms$)
2580 CALL &BB18:LOCATE tmpx,hrows:PRINT SPC(tmp)
2590 RETURN
2600 '
2610 ' delay routine
2620 IF dly>0 THEN FOR i=0 TO dly:NEXT:RETURN' delay routine
2630 '
2640 ' draw grid border
2650 ' draw top and bottom horizontal
2660 PEN ctx:LOCATE ofx,ofy:PRINT CHR$(150)+STRING$(gw,CHR$(154))+CHR$(156)
2670 LOCATE ofx,ofy+gh+1:PRINT CHR$(147)+STRING$(gw,CHR$(154))+CHR$(153)
2680 ' draw verticals
2690 a$=CHR$(149):FOR i=ofy+1 TO gh+ofy:LOCATE ofx,i:PRINT a$;:LOCATE ofx+gw+1,i:PRINT a$:NEXT
2700 RETURN
2710 '
2720 ' Define grid size
2730 gw=MAX(INT(cols/3),INT(RND*cols)+2):gw=MIN(gw,cols-2):hgw=INT(gw/2):' -2 cols for the vertical grid lines
2740 gh=MAX(INT(rows/3),INT(RND*rows)+2):gh=MIN(gh,rows-5):hgh=INT(gh/2):' -5 rows for 2 cpu rows,1 status line and and 2 grid rows
2750 gwh=gw*gh
2760 ofx=INT((cols-gw)/2):ofy=INT((rows-gh)/2)+1:' locate offsets: ofy+1 row to leave a blank line for the status line
2770 RETURN
2780 '
2790 ' Define starting positions
2800 st1x=hgw+(gw MOD 2):st1y=1:st2x=hgw+(gw MOD 2):st2y=gh
2810 id1=1:id2=2:c1=1:c2=1
2820 FOR i=0 TO ial:st1(i,0)=st1x:st1(i,1)=st1y:st2(i,0)=st2x:st2(i,1)=st2y:NEXT
2830 grd(st1x,st1y)=id1:grd(st2x,st2y)=id2
2831 st1(ism,0)=st1x:st1(ism,1)=st1y:st1(ivg,0)=st1x:st1(ivg,1)=st1y:st1(imn,0)=st1x:st1(imn,1)=st1y:st1(imx,0)=st1x:st1(imx,1)=st1y:st1(isl,0)=st1x:st1(isl,1)=st1y
2832 st2(ism,0)=st2x:st2(ism,1)=st2y:st2(ivg,0)=st2x:st2(ivg,1)=st2y:st2(imn,0)=st2x:st2(imn,1)=st2y:st2(imx,0)=st2x:st2(imx,1)=st2y:st2(isl,0)=st2x:st2(isl,1)=st2y
2840 LOCATE  ofx+st1x,ofy+st1y:PEN cl1:PRINT b1$
2850 LOCATE  ofx+st2x,ofy+st2y:PEN cl2:PRINT b2$
2860 p=0.1:sb=INT((gwh/2)*p):' starting blocks formula
2861 ' randomly select starting blocks for reach player
2862 WHILE c1<sb OR c2<sb
2863 ' Player 1 block selection
2864 IF c1<sb THEN
2865 tmpx=INT(RND*gw)+1:tmpy=INT(RND*(gh/2))+1
2866 IF grd(tmpx,tmpy)<>0 THEN 2870
2867 grd(tmpx,tmpy)=id1:c1=c1+1:st1(ism,0)=st1(ism,0)+tmpx:st1(ism,1)=st1(ism,1)+tmpy
2868 st1(ivg,0)=INT(st1(ism,0)/c1):st1(ivg,1)=INT(st1(ism,1)/c1):st1(imn,0)=MIN(st1(imn,0),tmpx):st1(imn,1)=MIN(st1(imn,1),tmpy):st1(imx,0)=MAX(st1(imx,0),tmpx):st1(imx,1)=MAX(st1(imx,1),tmpy)
2869 SOUND 1,MAX(100,1500-(c1+c2)*10),2,10:LOCATE ofx+tmpx,ofy+tmpy:PEN cl1:PRINT b1$
2870 ' Player 2 block selection
2871 IF c2<sb THEN
2872 tmpx=INT(RND*gw)+1:tmpy=INT(RND*(gh/2))+1:tmpy=MIN(gh,INT(RND*(gh/2))+(gh/2)+1)
2873 IF grd(tmpx,tmpy)<>0 THEN 2878
2874 grd(tmpx,tmpy)=id2:c2=c2+1:st2(ism,0)=st2(ism,0)+tmpx:st2(ism,1)=st2(ism,1)+tmpy
2875 st2(ivg,0)=INT(st2(ism,0)/c1):st2(ivg,1)=INT(st2(ism,1)/c1):st2(imn,0)=MIN(st2(imn,0),tmpx):st2(imn,1)=MIN(st2(imn,1),tmpy):st2(imx,0)=MAX(st2(imx,0),tmpx):st2(imx,1)=MAX(st2(imx,1),tmpy)
2876 SOUND 1,MAX(100,1500-(c1+c2)*15),2,10:LOCATE ofx+tmpx,ofy+tmpy:PEN cl2:PRINT b2$
2878 WEND
2879 RETURN
2880 '
2890 ' error handling
2900 ms$="Error with code"+STR$(ERR)+" at line"+STR$(ERL):GOSUB 2560
2910 MODE 2:INK 0,1:INK 1,26:PEN 1:PAPER 0:BORDER 1:LOCATE 1,1:PRINT ms$
2920 IF ERR=5 THEN PRINT"Improper Argument"
2930 END
2940 INK 0,1:INK 1,26:PAPER 0:PEN 1:BORDER 1:MODE 2:END
2950 ' DATA
2960 DATA "start","sum","avg","min","max","sel","last":' sel is last selected position, last is last occupied position
2970 DATA "Normal","Attacker","Random","Defender":' Personality names
2980 DATA "Nrm","Att","Rnd","Def":' Shorthands for Personality names
