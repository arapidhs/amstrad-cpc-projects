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
660 GOSUB 1460:' Define grid size and starting positions
670 DIM grd(gw,gh):grd(0,0)=-1:blmax=(gw+gh)*2:DIM bls(blmax,1)
680 id1=1:id2=2:c1=1:c2=1
690 FOR i=0 TO ial:st1(i,0)=start1x:st1(i,1)=start1y:st2(i,0)=start2x:st2(i,1)=start2y:NEXT
700 grd(start1x,start1y)=id1:grd(start2x,start2y)=id2
710 calcid=id1:GOSUB 2510:' calculate min max for x,y
720 calcid=id2:GOSUB 2510
730 '
740 ' Wait for key press
750 CLS:IF smd=0 THEN ms$="Press key to start" ELSE ms$="Press any key to start"
760 GOSUB 2710
770 CLS
780 '
790 ' print personalities
800 maxpn=MAX(LEN(pn$(pn1)),LEN(pn$(pn2)))
810 id1$="CPU 1":id2$="CPU 2":c1x=maxpn+MAX(LEN(id1$),LEN(id2$))+4:c1y=1:c2x=c1x:c2y=rows' aligned x location to print status on cpu rows
820 sx=1:sy=2:' location of status line
830 LOCATE 1,1:PEN cl1:PRINT id1$;": ";pn$(pn1)
840 LOCATE 1,rows:PEN cl2:PRINT id2$;": ";pn$(pn2)
850 GOSUB 2660:' print block counts
860 GOSUB 2790:' draw grd BORDER
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
1030 ON pn+1 GOSUB 1540,1580,1620,1820
1040 ' Process move or fg
1050 mv=0:fg=0:IF grd(tx,ty)=0 THEN mv=1 ELSE IF grd(tx,ty)=opp THEN fg=1
1060 IF mv=1 THEN GOTO 1070 ELSE IF fg=1 THEN GOTO 1110 ELSE GOTO 1330
1070 GOSUB 2130:' update st after move
1080 IF smd<>0 THEN LOCATE clx,cly:PRINT SPC(LEN(ms$))
1090 IF smd<>0 THEN ms$="Moved to"+STR$(tx)+","+STR$(ty) ELSE ms$=""
1100 GOTO 1180:' print results to Screen
1110 ' Simple fg resolution
1120 GOSUB 2250:' resolve fg
1130 GOSUB 2290:' update start after fg
1140 IF smd<>0 THEN LOCATE clx,cly:PRINT SPC(LEN(ms$))
1150 IF smd<>0 THEN GOTO 1180 ELSE ms$="":GOTO 1180
1160 GOTO 1180:' print results to screen
1170 IF wn=1 THEN ms$="Won at"+STR$(tx)+","+STR$(ty) ELSE ms$="Lost at"+STR$(tx)+","+STR$(ty)
1180 ' print move or fg results to screen
1190 IF mv=1 or wn=1 THEN grd(tx,ty)=id
1200 IF mv=1 THEN hx=tx:hy=ty:GOSUB 2060:SOUND 1,200,20,15:GOSUB 2060:' highlight, play sound highlight
1210 IF fg=1 AND wn=1 THEN hx=tx:hy=ty:GOSUB 2060:SOUND 1,142,20,15:SOUND 1,95,20,15:GOSUB 2060:' highlight, play sound highlight
1220 IF fg=1 AND wn=0 THEN hx=tx:hy=ty:GOSUB 2060:SOUND 1,95,20,15:SOUND 1,125,20,15:GOSUB 2060:' highlight, play sound highlight
1230 IF grd(tx,ty)=id THEN LOCATE ofx+tx,ofy+ty:PEN cpuclr:PRINT b$;
1240 IF LEN(ms$)>0 THEN PEN cpuclr:LOCATE clx,cly:PRINT ms$
1250 GOSUB 2660:' print block counts
1260 GOSUB 2760:' delay routine
1270 LOCATE clx,cly:PRINT SPC(LEN(ms$))
1280 IF c1+c2>=gwh OR c1=0 OR c2=0 THEN GOTO 1370
1290 IF turn=1 THEN turn=2 ELSE turn=1
1300 trs=1
1310 WEND
1320 '
1330 ' error: no valid move found
1340 IF smd<>0 THEN ms$="Error: no valid move found" ELSE ms$="Err: no move found"
1350 GOSUB 2710:GOTO 1420:' print error message and exit
1360 '
1370 ' end game
1380 IF smd<>0 THEN ms$="Game Over: "+id1$+":"+STR$(c1)+" "+id2$+":"+STR$(c2) ELSE ms$="Game Over:"+MID$(STR$(c1),2)+"/"+MID$(STR$(c2),2)
1390 GOSUB 2710
1400 IF c1>c2 THEN ms$="CPU 1 wins!" ELSE IF c1<c2 THEN ms$="CPU 2 wins!" ELSE ms$="Draw!"
1410 GOSUB 2710
1420 CLS:ms$="Play again? (Y:N)":LOCATE hcols-INT(LEN(ms$)/2),hrows:INPUT"Play Again? (Y:N)",a$
1430 IF UPPER$(a$)="Y" THEN RUN ELSE GOTO 2920
1440 END
1450 '
1460 ' Define grid size and starting positions
1470 gw=MAX(INT(cols/3),INT(RND*cols)+2):gw=MIN(gw,cols-2):hgw=INT(gw/2):' -2 cols for the vertical grid lines
1480 gh=MAX(INT(rows/3),INT(RND*rows)+2):gh=MIN(gh,rows-5):hgh=INT(gh/2):' -5 rows for 2 cpu rows,1 status line and and 2 grid rows
1490 gwh=gw*gh
1500 ofx=INT((cols-gw)/2):ofy=INT((rows-gh)/2)+1:' locate offsets: ofy+1 row to leave a blank line for the status line
1510 start1x=hgw+(gw MOD 2):start1y=1:start2x=hgw+(gw MOD 2):start2y=gh
1520 RETURN
1530 '
1540 ' CPU Normal
1550 GOSUB 1620
1560 RETURN
1570 '
1580 ' CPU Attacker
1590 GOSUB 1620
1600 RETURN
1610 '
1620 ' CPU Random
1630 bx=0:by=0:tx=0:ty=0:bl=0
1640 GOSUB 1860:' populate bls with all valid moves
1650 IF bl=0 THEN GOTO 1800:' no valid move found, we should never reach this state normally
1660 r=INT(RND*bl)+1:bx=bls(r,0):by=bls(r,1)
1670 ' We found a random valid block next we need to find a valid random target
1680 IF dbgscan=1 THEN SOUND 1,1000,2,15:hx=bx:hy=by:GOSUB 2060:' highlight selected valid block
1690 bl=0
1700 FOR dx=-1 TO 1:FOR dy=-1 TO 1
1710 IF dx=0 AND dy=0 THEN GOTO 1760
1720 nx=bx+dx:ny=by+dy
1730 IF nx<1 OR nx>gw OR ny<1 OR ny>gh THEN GOTO 1760
1740 IF grd(nx,ny)<>0 AND grd(nx,ny)<>opp THEN GOTO 1760
1750 bl=bl+1:bls(bl,0)=nx:bls(bl,1)=ny
1760 NEXT dy
1770 NEXT dx
1780 IF bl=0 THEN GOTO 1800:' no valid target found, we should never normally reach this state
1790 r=INT(RND*bl)+1:tx=bls(r,0):ty=bls(r,1)
1800 RETURN
1810 '
1820 ' CPU Defender
1830 GOSUB 1620
1840 RETURN
1850 '
1860 ' Populate bls with all valid moves
1870 bl=0
1880 minx=st(imn,0):miny=st(imn,1):maxx=st(imx,0):maxy=st(imx,1)
1890 FOR x=minx TO maxx:FOR y=miny TO maxy
1900 IF  dbgscan=1 THEN SOUND 1,1000,2,15:hx=x:hy=y:GOSUB 2060:' highlight grd scanning
1910 IF bl=blmax THEN GOTO 2040:' full list populated, return
1920 IF grd(x,y)<>id THEN GOTO 2020
1930 FOR dx=-1 TO 1:FOR dy=-1 TO 1
1940 IF dx=0 AND dy=0 THEN GOTO 2000
1950 nx=x+dx:ny=y+dy
1960 IF nx<1 OR nx>gw OR ny<1 OR ny>gh THEN GOTO 2000
1970 IF grd(nx,ny)<>0 AND grd(nx,ny)<>opp THEN GOTO 2000
1980 bl=bl+1:' found valid target at nx,ny
1990 bls(bl,0)=x:bls(bl,1)=y:GOTO 2020:' valid block found move to next
2000 NEXT dy
2010 NEXT dx
2020 NEXT y
2030 NEXT x
2040 RETURN
2050 '
2060 ' highlight
2070 IF hx<1 OR hy<1 OR hx>gw OR hy>gh THEN RETURN
2080 LOCATE ofx+hx,ofy+hy:PEN cpuclr:PRINT hb$
2090 IF grd(hx,hy)=id1 THEN PEN cl1:c$=b1$ ELSE IF grd(hx,hy)=id2 THEN PEN cl2:c$=b2$ ELSE c$=eb$
2100 LOCATE ofx+hx,ofy+hy:PRINT c$
2110 RETURN
2120 '
2130 ' update st after move
2140 IF id=id1 THEN 2150 ELSE 2190
2150 c1=c1+1:st1(ism,0)=st1(ism,0)+tx:st1(ism,1)=st1(ism,1)+ty:st1(ivg,0)=INT(st1(ism,0)/c1):st1(ivg,1)=INT(st1(ism,1)/c1)
2160 st1(imn,0)=MIN(st1(imn,0),tx):st1(imn,1)=MIN(st1(imn,1),ty):st1(imx,0)=MAX(st1(imx,0),tx):st1(imx,1)=MAX(st1(imx,1),ty)
2170 st1(ilt,0)=tx:st1(ilt,1)=ty:st1(isl,0)=bx:st1(isl,1)=by
2180 FOR i=0 TO ial:st(i,0)=st1(i,0):st(i,1)=st1(i,1):NEXT:GOTO 2230
2190 c2=c2+1:st2(ism,0)=st2(ism,0)+tx:st2(ism,1)=st2(ism,1)+ty:st2(ivg,0)=INT(st2(ism,0)/c2):st2(ivg,1)=INT(st2(ism,1)/c2)
2200 st2(imn,0)=MIN(st2(imn,0),tx):st2(imn,1)=MIN(st2(imn,1),ty):st2(imx,0)=MAX(st2(imx,0),tx):st2(imx,1)=MAX(st2(imx,1),ty)
2210 st2(ilt,0)=tx:st2(ilt,1)=ty:st2(isl,0)=bx:st2(isl,1)=by
2220 FOR i=0 TO ial:st(i,0)=st2(i,0):st(i,1)=st2(i,1):NEXT:GOTO 2230
2230 RETURN
2240 '
2250 ' resolve fg
2260 wn=0:r=RND:IF r>0.5 THEN wn=1:grd(tx,ty)=id
2270 RETURN
2280 '
2290 ' update st after fg
2300 IF fg=1 THEN IF id=id1 THEN st1(isl,0)=bx:st1(isl,1)=by ELSE st2(isl,0)=bx:st2(isl,1)=by
2310 IF wn=0 THEN GOTO 2490
2320 IF id=id1 THEN GOTO 2330 ELSE GOTO 2410
2330 c1=c1+1:st1(ism,0)=st1(ism,0)+tx:st1(ism,1)=st1(ism,1)+ty:st1(ivg,0)=INT(st1(ism,0)/c1):st1(ivg,1)=INT(st1(ism,1)/c1)
2340 st1(imn,0)=MIN(st1(imn,0),tx):st1(imn,1)=MIN(st1(imn,1),ty):st1(imx,0)=MAX(st1(imx,0),tx):st1(imx,1)=MAX(st1(imx,1),ty)
2350 st1(ilt,0)=tx:st1(ilt,1)=ty:st1(isl,0)=bx:st1(isl,1)=by
2360 FOR i=0 TO ial:st(i,0)=st1(i,0):st(i,1)=st1(i,1):NEXT
2370 c2=c2-1:IF c2>0 THEN st2(ism,0)=st2(ism,0)-tx:st2(ism,1)=st2(ism,1)-ty:st2(ivg,0)=INT(st2(ism,0)/c2):st2(ivg,1)=INT(st2(ism,1)/c2)
2970 '
2380 ' calculate min max x y if needed
2390 IF st2(imn,0)=tx OR st2(imn,1)=ty OR st2(imx,0)=tx OR st2(imx,1)=ty THEN calcid=id2:GOSUB 2510
2400 GOTO 2490
2410 c2=c2+1:st2(ism,0)=st2(ism,0)+tx:st2(ism,1)=st2(ism,1)+ty:st2(ivg,0)=INT(st2(ism,0)/c2):st2(ivg,1)=INT(st2(ism,1)/c2)
2420 st2(imn,0)=MIN(st2(imn,0),tx):st2(imn,1)=MIN(st2(imn,1),ty):st2(imx,0)=MAX(st2(imx,0),tx):st2(imx,1)=MAX(st2(imx,1),ty)
2430 st2(ilt,0)=tx:st2(ilt,1)=ty:st2(isl,0)=bx:st2(isl,1)=by
2440 FOR i=0 TO ial:st(i,0)=st2(i,0):st(i,1)=st2(i,1):NEXT
2450 c1=c1-1:IF c1>0 THEN st1(ism,0)=st1(ism,0)-tx:st1(ism,1)=st1(ism,1)-ty:st1(ivg,0)=INT(st1(ism,0)/c1):st1(ivg,1)=INT(st1(ism,1)/c1)
2460 '
2470 ' calculate min max x y if needed
2480 IF st1(imn,0)=tx OR st1(imn,1)=ty OR st1(imx,0)=tx OR st1(imx,1)=ty THEN calcid=id1:GOSUB 2510
2490 RETURN
2500 '
2510 ' recalculate minx maxx miny maxy
2520 IF calcid<>id1 AND calcid<>id2 THEN GOTO 2640
2530 IF calcid=id1 THEN cminx=st1(imn,0):cmaxx=st1(imx,0):cminy=st1(imn,1):cmaxy=st1(imx,1) ELSE cminx=st2(imn,0):cmaxx=st2(imx,0):cminy=st2(imn,1):cmaxy=st2(imx,1)
2540 imnx=0:imxx=0:imny=0:imxy=0
2550 FOR cx=cminx TO cmaxx:FOR cy=cminy TO cmaxy
2560 IF grd(cx,cy)<>calcid THEN GOTO 2610
2570 IF imnx=0 THEN imnx=cx ELSE imnx=MIN(imnx,cx)
2580 IF imxx=0 THEN imxx=cx ELSE imxx=MAX(imxx,cx)
2590 IF imny=0 THEN imny=cy ELSE imny=MIN(imny,cy)
2600 IF imxy=0 THEN imxy=cy ELSE imxy=MAX(imxy,cy)
2610 NEXT cy
2620 NEXT cx
2630 IF calcid=id1 THEN st1(imn,0)=imnx:st1(imn,1)=imny:st1(imx,0)=imxx:st1(imx,1)=imxy ELSE st2(imn,0)=imnx:st2(imn,1)=imny:st2(imx,0)=imxx:st2(imx,1)=imxy
2640 RETURN
2650 '
2660 ' print block counts
2670 PEN cl1:LOCATE cols-3,c1y:PRINT USING "####";c1;
2680 PEN cl2:LOCATE cols-3,c2y:PRINT USING "####";c2;
2690 RETURN
2700 '
2710 ' print centered message
2720 tmp=INT(LEN(ms$)/2):tmpx=MAX(1,hcols-tmp):LOCATE tmpx,hrows:PEN ctx:PRINT ms$:tmp=LEN(ms$)
2730 CALL &BB18:LOCATE tmpx,hrows:PRINT SPC(tmp)
2740 RETURN
2750 '
2760 ' delay routine
2770 IF dly>0 THEN FOR i=0 TO dly:NEXT:RETURN' delay routine
2780 '
2790 ' draw grid border
2800 ' draw top and bottom horizontal
2810 PEN ctx:LOCATE ofx,ofy:PRINT CHR$(150)+STRING$(gw,CHR$(154))+CHR$(156)
2820 LOCATE ofx,ofy+gh+1:PRINT CHR$(147)+STRING$(gw,CHR$(154))+CHR$(153)
2830 ' draw verticals
2840 a$=CHR$(149):FOR i=ofy+1 TO gh+ofy:LOCATE ofx,i:PRINT a$;:LOCATE ofx+gw+1,i:PRINT a$:NEXT
2850 RETURN
2860 '
2870 ' error handling
2880 ms$="Error with code"+STR$(ERR)+" at line"+STR$(ERL):GOSUB 2710
2890 MODE 2:INK 0,1:INK 1,26:PEN 1:PAPER 0:BORDER 1:LOCATE 1,1:PRINT ms$
2900 IF ERR=5 THEN PRINT"Improper Argument"
2910 END
2920 INK 0,1:INK 1,26:PAPER 0:PEN 1:BORDER 1:MODE 2
2930 ' DATA
2940 DATA "start","sum","avg","min","max","sel","last":' sel is last selected position, last is last occupied position
2950 DATA "Normal","Attacker","Random","Defender":' Personality names
2960 DATA "Nrm","Att","Rnd","Def":' Shorthands for Personality names
