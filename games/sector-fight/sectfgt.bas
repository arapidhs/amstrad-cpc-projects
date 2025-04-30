10 ' debug messages and debug grid scanning
20 dbgscan=1
30 RANDOMIZE TIME
40 ON ERROR GOTO 2870
50 ON BREAK GOSUB 2920
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
340 DIM st$(ial):RESTORE 2940:FOR i=0 TO ial:READ st$(i):NEXT
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
580 IF smd=0 THEN RESTORE 2960 ELSE RESTORE 2950
590 FOR i=0 TO psz:READ pn$(i):NEXT
600 '
610 ' Assign personalities based on personality probabilities: pnprb()
620 r=RND:IF r<pnprb(pnrm) THEN pn1=pnrm ELSE IF r<pnprb(patt) THEN pn1=patt ELSE IF r<pnprb(prnd) THEN pn1=prnd ELSE pn1=pdef
630 r=RND:IF r<pnprb(pnrm) THEN pn2=pnrm ELSE IF r<pnprb(patt) THEN pn2=patt ELSE IF r<pnprb(prnd) THEN pn2=prnd ELSE pn2=pdef
640 '
650 ' Initialize grd
660 GOSUB 2790:' Define grid size and starting positions
670 DIM grd(gw,gh):grd(0,0)=-1:blmax=(gw+gh)*2:DIM bls(blmax,1)
680 id1=1:id2=2:c1=1:c2=1
690 FOR i=0 TO ial:st1(i,0)=start1x:st1(i,1)=start1y:st2(i,0)=start2x:st2(i,1)=start2y:NEXT
700 grd(start1x,start1y)=id1:grd(start2x,start2y)=id2
710 calcid=id1:GOSUB 2360:' calculate min max for x,y
720 calcid=id2:GOSUB 2360
730 '
740 ' Wait for key press
750 CLS:IF smd=0 THEN ms$="Press key to start" ELSE ms$="Press any key to start"
760 GOSUB 2630
770 CLS
780 '
790 ' print personalities
800 maxpn=MAX(LEN(pn$(pn1)),LEN(pn$(pn2)))
810 id1$="CPU 1":id2$="CPU 2":c1x=maxpn+MAX(LEN(id1$),LEN(id2$))+4:c1y=1:c2x=c1x:c2y=rows' aligned x location to print status on cpu rows
820 sx=1:sy=2:' location of status line
830 LOCATE 1,1:PEN cl1:PRINT id1$;": ";pn$(pn1)
840 LOCATE 1,rows:PEN cl2:PRINT id2$;": ";pn$(pn2)
850 GOSUB 2580:' print block counts
860 GOSUB 2710:' draw grd BORDER
870 LOCATE  ofx+start1x,ofy+start1y:PEN cl1:PRINT b1$
880 LOCATE  ofx+start2x,ofy+start2y:PEN cl2:PRINT b2$
890 '
900 ' Game LOOP
910 turn=1:trn=0
920 WHILE c1+c2<gw*gh
930 '
940 ' CPU turn
950 trn=trn+1:trs=0
960 IF turn=1 THEN id=id1:opp=id2:cpuclr=cl1:b$=b1$:pn=pn1:clx=c1x:cly=c1y:FOR i=0 TO ial:st(i,0)=st1(i,0):st(i,1)=st1(i,1):NEXT
970 IF turn=2 THEN id=id2:opp=id1:cpuclr=cl2:b$=b2$:pn=pn2:clx=c2x:cly=c2y:FOR i=0 TO ial:st(i,0)=st2(i,0):st(i,1)=st2(i,1):NEXT
980 LOCATE sx,sy:PRINT STRING$(cols," ")
990 prg=ROUND((c1+c2)/(gwh)*100,2)
1000 PEN ctx:LOCATE sx,sy:PRINT "Turn";trn;STR$(prg);"%"
1010 IF smd<>0 THEN PEN cpuclr:ms$="Thinking.." ELSE ms$=""
1020 IF smd<>0 THEN  LOCATE clx,cly:PRINT ms$;
1030 ON pn+1 GOSUB 1460,1500,1540,1740
1040 ' Process move or fg
1050 mv=0:fg=0:IF grd(tx,ty)=0 THEN mv=1 ELSE IF grd(tx,ty)=opp THEN fg=1
1060 IF mv=1 THEN GOTO 1070 ELSE IF fg=1 THEN GOTO 1110 ELSE GOTO 1330
1070 GOSUB 1980:' update st after move
1080 IF smd<>0 THEN LOCATE clx,cly:PRINT SPC(LEN(ms$))
1090 IF smd<>0 THEN ms$="Moved to"+STR$(tx)+","+STR$(ty) ELSE ms$=""
1100 GOTO 1180:' print results to Screen
1110 ' Simple fg resolution
1120 GOSUB 2100:' resolve fg
1130 GOSUB 2140:' update start after fg
1140 IF smd<>0 THEN LOCATE clx,cly:PRINT SPC(LEN(ms$))
1150 IF smd<>0 THEN GOTO 1180 ELSE ms$="":GOTO 1180
1160 GOTO 1180:' print results to screen
1170 IF wn=1 THEN ms$="Won at"+STR$(tx)+","+STR$(ty) ELSE ms$="Lost at"+STR$(tx)+","+STR$(ty)
1180 ' print move or fg results to screen
1190 IF mv=1 or wn=1 THEN grd(tx,ty)=id
1200 IF mv=1 THEN hx=tx:hy=ty:GOSUB 2510:SOUND 1,200,20,15:GOSUB 2510:' highlight, play sound highlight
1210 IF fg=1 AND wn=1 THEN hx=tx:hy=ty:GOSUB 2510:SOUND 1,142,20,15:SOUND 1,95,20,15:GOSUB 2510:' highlight, play sound highlight
1220 IF fg=1 AND wn=0 THEN hx=tx:hy=ty:GOSUB 2510:SOUND 1,95,20,15:SOUND 1,125,20,15:GOSUB 2510:' highlight, play sound highlight
1230 IF grd(tx,ty)=id THEN LOCATE ofx+tx,ofy+ty:PEN cpuclr:PRINT b$;
1240 IF LEN(ms$)>0 THEN PEN cpuclr:LOCATE clx,cly:PRINT ms$
1250 GOSUB 2580:' print block counts
1260 GOSUB 2680:' delay routine
1270 LOCATE clx,cly:PRINT SPC(LEN(ms$))
1280 IF c1+c2>=gwh OR c1=0 OR c2=0 THEN GOTO 1370
1290 IF turn=1 THEN turn=2 ELSE turn=1
1300 trs=1
1310 WEND
1320 '
1330 ' error: no valid move found
1340 IF smd<>0 THEN ms$="Error: no valid move found" ELSE ms$="Err: no move found"
1350 GOSUB 2630:GOTO 1420:' print error message and exit
1360 '
1370 ' end game
1380 IF smd<>0 THEN ms$="Game Over: "+id1$+":"+STR$(c1)+" "+id2$+":"+STR$(c2) ELSE ms$="Game Over:"+MID$(STR$(c1),2)+"/"+MID$(STR$(c2),2)
1390 GOSUB 2630
1400 IF c1>c2 THEN ms$="CPU 1 wins!" ELSE IF c1<c2 THEN ms$="CPU 2 wins!" ELSE ms$="Draw!"
1410 GOSUB 2630
1420 CLS:ms$="Play again? (Y:N)":LOCATE hcols-INT(LEN(ms$)/2),hrows:INPUT"Play Again? (Y:N)",a$
1430 IF UPPER$(a$)="Y" THEN RUN ELSE GOTO 2920
1440 END
1450 '
1460 ' CPU Normal
1470 GOSUB 1540
1480 RETURN
1490 '
1500 ' CPU Attacker
1510 GOSUB 1540
1520 RETURN
1530 '
1540 ' CPU Random
1550 bx=0:by=0:tx=0:ty=0:bl=0
1560 GOSUB 1780:' populate bls with all valid moves
1570 IF bl=0 THEN GOTO 1720:' no valid move found, we should never reach this state normally
1580 r=INT(RND*bl)+1:bx=bls(r,0):by=bls(r,1)
1590 ' We found a random valid block next we need to find a valid random target
1600 IF dbgscan=1 THEN SOUND 1,1000,2,15:hx=bx:hy=by:GOSUB 2510:' highlight selected valid block
1610 bl=0
1620 FOR dx=-1 TO 1:FOR dy=-1 TO 1
1630 IF dx=0 AND dy=0 THEN GOTO 1680
1640 nx=bx+dx:ny=by+dy
1650 IF nx<1 OR nx>gw OR ny<1 OR ny>gh THEN GOTO 1680
1660 IF grd(nx,ny)<>0 AND grd(nx,ny)<>opp THEN GOTO 1680
1670 bl=bl+1:bls(bl,0)=nx:bls(bl,1)=ny
1680 NEXT dy
1690 NEXT dx
1700 IF bl=0 THEN GOTO 1720:' no valid target found, we should never normally reach this state
1710 r=INT(RND*bl)+1:tx=bls(r,0):ty=bls(r,1)
1720 RETURN
1730 '
1740 ' CPU Defender
1750 GOSUB 1540
1760 RETURN
1770 '
1780 ' Populate bls with all valid moves
1790 bl=0
1800 minx=st(imn,0):miny=st(imn,1):maxx=st(imx,0):maxy=st(imx,1)
1810 FOR x=minx TO maxx:FOR y=miny TO maxy
1820 IF  dbgscan=1 THEN SOUND 1,1000,2,15:hx=x:hy=y:GOSUB 2510:' highlight grd scanning
1830 IF bl=blmax THEN GOTO 1960:' full list populated, return
1840 IF grd(x,y)<>id THEN GOTO 1940
1850 FOR dx=-1 TO 1:FOR dy=-1 TO 1
1860 IF dx=0 AND dy=0 THEN GOTO 1920
1870 nx=x+dx:ny=y+dy
1880 IF nx<1 OR nx>gw OR ny<1 OR ny>gh THEN GOTO 1920
1890 IF grd(nx,ny)<>0 AND grd(nx,ny)<>opp THEN GOTO 1920
1900 bl=bl+1:' found valid target at nx,ny
1910 bls(bl,0)=x:bls(bl,1)=y:GOTO 1940:' valid block found move to next
1920 NEXT dy
1930 NEXT dx
1940 NEXT y
1950 NEXT x
1960 RETURN
1970 '
1980 ' update st after move
1990 IF id=id1 THEN 2000 ELSE 2040
2000 c1=c1+1:st1(ism,0)=st1(ism,0)+tx:st1(ism,1)=st1(ism,1)+ty:st1(ivg,0)=INT(st1(ism,0)/c1):st1(ivg,1)=INT(st1(ism,1)/c1)
2010 st1(imn,0)=MIN(st1(imn,0),tx):st1(imn,1)=MIN(st1(imn,1),ty):st1(imx,0)=MAX(st1(imx,0),tx):st1(imx,1)=MAX(st1(imx,1),ty)
2020 st1(ilt,0)=tx:st1(ilt,1)=ty:st1(isl,0)=bx:st1(isl,1)=by
2030 FOR i=0 TO ial:st(i,0)=st1(i,0):st(i,1)=st1(i,1):NEXT:GOTO 2080
2040 c2=c2+1:st2(ism,0)=st2(ism,0)+tx:st2(ism,1)=st2(ism,1)+ty:st2(ivg,0)=INT(st2(ism,0)/c2):st2(ivg,1)=INT(st2(ism,1)/c2)
2050 st2(imn,0)=MIN(st2(imn,0),tx):st2(imn,1)=MIN(st2(imn,1),ty):st2(imx,0)=MAX(st2(imx,0),tx):st2(imx,1)=MAX(st2(imx,1),ty)
2060 st2(ilt,0)=tx:st2(ilt,1)=ty:st2(isl,0)=bx:st2(isl,1)=by
2070 FOR i=0 TO ial:st(i,0)=st2(i,0):st(i,1)=st2(i,1):NEXT:GOTO 2080
2080 RETURN
2090 '
2100 ' resolve fg
2110 wn=0:r=RND:IF r>0.5 THEN wn=1:grd(tx,ty)=id
2120 RETURN
2130 '
2140 ' update st after fg
2150 IF fg=1 THEN IF id=id1 THEN st1(isl,0)=bx:st1(isl,1)=by ELSE st2(isl,0)=bx:st2(isl,1)=by
2160 IF wn=0 THEN GOTO 2340
2170 IF id=id1 THEN GOTO 2180 ELSE GOTO 2260
2180 c1=c1+1:st1(ism,0)=st1(ism,0)+tx:st1(ism,1)=st1(ism,1)+ty:st1(ivg,0)=INT(st1(ism,0)/c1):st1(ivg,1)=INT(st1(ism,1)/c1)
2190 st1(imn,0)=MIN(st1(imn,0),tx):st1(imn,1)=MIN(st1(imn,1),ty):st1(imx,0)=MAX(st1(imx,0),tx):st1(imx,1)=MAX(st1(imx,1),ty)
2200 st1(ilt,0)=tx:st1(ilt,1)=ty:st1(isl,0)=bx:st1(isl,1)=by
2210 FOR i=0 TO ial:st(i,0)=st1(i,0):st(i,1)=st1(i,1):NEXT
2220 c2=c2-1:IF c2>0 THEN st2(ism,0)=st2(ism,0)-tx:st2(ism,1)=st2(ism,1)-ty:st2(ivg,0)=INT(st2(ism,0)/c2):st2(ivg,1)=INT(st2(ism,1)/c2)
2970 '
2230 ' calculate min max x y if needed
2240 IF st2(imn,0)=tx OR st2(imn,1)=ty OR st2(imx,0)=tx OR st2(imx,1)=ty THEN calcid=id2:GOSUB 2360
2250 GOTO 2340
2260 c2=c2+1:st2(ism,0)=st2(ism,0)+tx:st2(ism,1)=st2(ism,1)+ty:st2(ivg,0)=INT(st2(ism,0)/c2):st2(ivg,1)=INT(st2(ism,1)/c2)
2270 st2(imn,0)=MIN(st2(imn,0),tx):st2(imn,1)=MIN(st2(imn,1),ty):st2(imx,0)=MAX(st2(imx,0),tx):st2(imx,1)=MAX(st2(imx,1),ty)
2280 st2(ilt,0)=tx:st2(ilt,1)=ty:st2(isl,0)=bx:st2(isl,1)=by
2290 FOR i=0 TO ial:st(i,0)=st2(i,0):st(i,1)=st2(i,1):NEXT
2300 c1=c1-1:IF c1>0 THEN st1(ism,0)=st1(ism,0)-tx:st1(ism,1)=st1(ism,1)-ty:st1(ivg,0)=INT(st1(ism,0)/c1):st1(ivg,1)=INT(st1(ism,1)/c1)
2310 '
2320 ' calculate min max x y if needed
2330 IF st1(imn,0)=tx OR st1(imn,1)=ty OR st1(imx,0)=tx OR st1(imx,1)=ty THEN calcid=id1:GOSUB 2360
2340 RETURN
2350 '
2360 ' recalculate minx maxx miny maxy
2370 IF calcid<>id1 AND calcid<>id2 THEN GOTO 2490
2380 IF calcid=id1 THEN cminx=st1(imn,0):cmaxx=st1(imx,0):cminy=st1(imn,1):cmaxy=st1(imx,1) ELSE cminx=st2(imn,0):cmaxx=st2(imx,0):cminy=st2(imn,1):cmaxy=st2(imx,1)
2390 imnx=0:imxx=0:imny=0:imxy=0
2400 FOR cx=cminx TO cmaxx:FOR cy=cminy TO cmaxy
2410 IF grd(cx,cy)<>calcid THEN GOTO 2460
2420 IF imnx=0 THEN imnx=cx ELSE imnx=MIN(imnx,cx)
2430 IF imxx=0 THEN imxx=cx ELSE imxx=MAX(imxx,cx)
2440 IF imny=0 THEN imny=cy ELSE imny=MIN(imny,cy)
2450 IF imxy=0 THEN imxy=cy ELSE imxy=MAX(imxy,cy)
2460 NEXT cy
2470 NEXT cx
2480 IF calcid=id1 THEN st1(imn,0)=imnx:st1(imn,1)=imny:st1(imx,0)=imxx:st1(imx,1)=imxy ELSE st2(imn,0)=imnx:st2(imn,1)=imny:st2(imx,0)=imxx:st2(imx,1)=imxy
2490 RETURN
2500 '
2510 ' highlight
2520 IF hx<1 OR hy<1 OR hx>gw OR hy>gh THEN RETURN
2530 LOCATE ofx+hx,ofy+hy:PEN cpuclr:PRINT hb$
2540 IF grd(hx,hy)=id1 THEN PEN cl1:c$=b1$ ELSE IF grd(hx,hy)=id2 THEN PEN cl2:c$=b2$ ELSE c$=eb$
2550 LOCATE ofx+hx,ofy+hy:PRINT c$
2560 RETURN
2570 '
2580 ' print block counts
2590 PEN cl1:LOCATE cols-3,c1y:PRINT USING "####";c1;
2600 PEN cl2:LOCATE cols-3,c2y:PRINT USING "####";c2;
2610 RETURN
2620 '
2630 ' print centered message
2640 tmp=INT(LEN(ms$)/2):tmpx=MAX(1,hcols-tmp):LOCATE tmpx,hrows:PEN ctx:PRINT ms$:tmp=LEN(ms$)
2650 CALL &BB18:LOCATE tmpx,hrows:PRINT SPC(tmp)
2660 RETURN
2670 '
2680 ' delay routine
2690 IF dly>0 THEN FOR i=0 TO dly:NEXT:RETURN' delay routine
2700 '
2710 ' draw grid border
2720 ' draw top and bottom horizontal
2730 PEN ctx:LOCATE ofx,ofy:PRINT CHR$(150)+STRING$(gw,CHR$(154))+CHR$(156)
2740 LOCATE ofx,ofy+gh+1:PRINT CHR$(147)+STRING$(gw,CHR$(154))+CHR$(153)
2750 ' draw verticals
2760 a$=CHR$(149):FOR i=ofy+1 TO gh+ofy:LOCATE ofx,i:PRINT a$;:LOCATE ofx+gw+1,i:PRINT a$:NEXT
2770 RETURN
2780 '
2790 ' Define grid size and starting positions
2800 gw=MAX(INT(cols/3),INT(RND*cols)+2):gw=MIN(gw,cols-2):hgw=INT(gw/2):' -2 cols for the vertical grid lines
2810 gh=MAX(INT(rows/3),INT(RND*rows)+2):gh=MIN(gh,rows-5):hgh=INT(gh/2):' -5 rows for 2 cpu rows,1 status line and and 2 grid rows
2820 gwh=gw*gh
2830 ofx=INT((cols-gw)/2):ofy=INT((rows-gh)/2)+1:' locate offsets: ofy+1 row to leave a blank line for the status line
2840 start1x=hgw+(gw MOD 2):start1y=1:start2x=hgw+(gw MOD 2):start2y=gh
2845 p=0.2:sb=INT((gwh/2)*p)-1:' starting blocks formula -1p for the starting position in the center
2850 RETURN
2860 '
2870 ' error handling
2880 ms$="Error with code"+STR$(ERR)+" at line"+STR$(ERL):GOSUB 2630
2890 MODE 2:INK 0,1:INK 1,26:PEN 1:PAPER 0:BORDER 1:LOCATE 1,1:PRINT ms$
2900 IF ERR=5 THEN PRINT"Improper Argument"
2910 END
2920 INK 0,1:INK 1,26:PAPER 0:PEN 1:BORDER 1:MODE 2:END
2930 ' DATA
2940 DATA "start","sum","avg","min","max","sel","last":' sel is last selected position, last is last occupied position
2950 DATA "Normal","Attacker","Random","Defender":' Personality names
2960 DATA "Nrm","Att","Rnd","Def":' Shorthands for Personality names
