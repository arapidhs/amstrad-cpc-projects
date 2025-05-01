10 ' debug messages and debug grid scanning
20 dbgscan=1
30 RANDOMIZE TIME
40 ON ERROR GOTO 2880
50 ON BREAK GOSUB 2930
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
340 DIM st$(ial):RESTORE 2950:FOR i=0 TO ial:READ st$(i):NEXT
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
580 IF smd=0 THEN RESTORE 2970 ELSE RESTORE 2960
590 FOR i=0 TO psz:READ pn$(i):NEXT
600 '
610 ' Assign personalities based on personality probabilities: pnprb()
620 r=RND:IF r<pnprb(pnrm) THEN pn1=pnrm ELSE IF r<pnprb(patt) THEN pn1=patt ELSE IF r<pnprb(prnd) THEN pn1=prnd ELSE pn1=pdef
630 r=RND:IF r<pnprb(pnrm) THEN pn2=pnrm ELSE IF r<pnprb(patt) THEN pn2=patt ELSE IF r<pnprb(prnd) THEN pn2=prnd ELSE pn2=pdef
640 '
650 ' Wait for key press
660 CLS:IF smd=0 THEN ms$="Press key to start" ELSE ms$="Press any key to start":GOSUB 2360:CLS
670 '
680 ' print personalities
690 id1$="CPU 1":id2$="CPU 2"
700 c1$=id1$+": "+pn$(pn1):c2$=id2$+": "+pn$(pn2)
710 c1x=MAX(LEN(c1$),LEN(c2$))+2:c1y=1:c2x=c1x:c2y=rows' aligned x location to print status on cpu rows
720 sx=1:sy=2:' location of status line
730 LOCATE 1,1:PEN cl1:PRINT c1$
740 LOCATE 1,rows:PEN cl2:PRINT c2$
750 '
760 ' Initialize grid and starting positions
770 GOSUB 2520:DIM grd(gw,gh):grd(0,0)=-1:' setup grid dimensions
780 GOSUB 2440:' draw grid border
790 GOSUB 2590:' define and draw starting positions and stats
800 blmax=(gw+gh)*2:DIM bls1(blmax,1):blc1=0:DIM bls2(blmax,1):blc1=2:'lists of valid moves for player 1of and 2of, blc1,2 point to the end of fthe list
810 '
820 GOSUB 2310:' print block counts
830 '
840 ' Game LOOP
850 turn=1:trn=0
860 WHILE c1+c2<gw*gh
870 '
880 ' CPU turn
890 trn=trn+1:trs=0
900 IF turn=1 THEN id=id1:opp=id2:cpuclr=cl1:b$=b1$:pn=pn1:clx=c1x:cly=c1y:FOR i=0 TO ial:st(i,0)=st1(i,0):st(i,1)=st1(i,1):NEXT
910 IF turn=2 THEN id=id2:opp=id1:cpuclr=cl2:b$=b2$:pn=pn2:clx=c2x:cly=c2y:FOR i=0 TO ial:st(i,0)=st2(i,0):st(i,1)=st2(i,1):NEXT
920 LOCATE sx,sy:PRINT STRING$(cols," ")
930 prg=ROUND((c1+c2)/(gwh)*100,2)
940 PEN ctx:LOCATE sx,sy:PRINT "Turn";trn;STR$(prg);"%":PEN cpuclr:mst$="...":LOCATE clx,cly:PRINT mst$;
950 '
960 'Process cpu action based on personality
970 act=0:ON pn+1 GOSUB 1270,1310,1350,1580
980 LOCATE clx,cly:PRINT SPC(LEN(mst$));
990 ' act 0 is no move found, 1 is move, 2 fight won, 3 fight loss
1000 IF act=0 THEN THEN GOTO 1140:'no valid move found, end game
1010 ' print move or fg results to screen
1020 IF act=1 THEN hx=tx:hy=ty:GOSUB 2240:SOUND 1,200,20,15:GOSUB 2240:' move highlight, play sound highlight
1030 IF act=2 THEN hx=tx:hy=ty:GOSUB 2240:SOUND 1,142,20,15:SOUND 1,95,20,15:GOSUB 2240:' fight won highlight, play sound highlight
1040 IF act=3 THEN hx=tx:hy=ty:GOSUB 2240:SOUND 1,95,20,15:SOUND 1,125,20,15:GOSUB 2240:' fight lost highlight, play sound highlight
1050 IF act=1 OR act=2 THEN grd(tx,ty)=id
1060 IF grd(tx,ty)=id THEN LOCATE ofx+tx,ofy+ty:PEN cpuclr:PRINT b$;
1070 GOSUB 2310:' print block counts
1080 GOSUB 2410:' delay routine
1090 IF c1+c2>=gwh OR c1=0 OR c2=0 THEN GOTO 1180
1100 IF turn=1 THEN turn=2 ELSE turn=1
1110 trs=1
1120 WEND
1130 '
1140 ' error: no valid move found
1150 IF smd<>0 THEN ms$="Error: no valid move found" ELSE ms$="Err: no move found"
1160 GOSUB 2360:GOTO 1230:' print error message and exit
1170 '
1180 ' end game
1190 IF smd<>0 THEN ms$="Game Over: "+id1$+":"+STR$(c1)+" "+id2$+":"+STR$(c2) ELSE ms$="Game Over:"+MID$(STR$(c1),2)+"/"+MID$(STR$(c2),2)
1200 GOSUB 2360
1210 IF c1>c2 THEN ms$="CPU 1 wins!" ELSE IF c1<c2 THEN ms$="CPU 2 wins!" ELSE ms$="Draw!"
1220 GOSUB 2360
1230 CLS:ms$="Play again? (Y:N)":LOCATE hcols-INT(LEN(ms$)/2),hrows:INPUT"Play Again? (Y:N)",a$
1240 IF UPPER$(a$)="Y" THEN RUN ELSE GOTO 2930
1250 END
1260 '
1270 ' CPU Normal
1280 GOSUB 1350
1290 RETURN
1300 '
1310 ' CPU Attacker
1320 GOSUB 1350
1330 RETURN
1340 '
1350 ' CPU Random
1360 bx=0:by=0:tx=0:ty=0:bl=0
1370 GOSUB 1620:' populate bls1 with all valid moves
1380 IF bl=0 THEN act=0:RETURN:' no valid move found, we should never reach this state normally
1390 r=INT(RND*bl)+1:bx=bls1(r,0):by=bls1(r,1)
1400 ' We found a random valid block next we need to find a valid random target
1410 IF dbgscan=1 THEN SOUND 1,1000,2,15:hx=bx:hy=by:GOSUB 2240:' highlight selected valid block
1420 bl=0
1430 FOR dx=-1 TO 1:FOR dy=-1 TO 1
1440 IF dx=0 AND dy=0 THEN GOTO 1490
1450 nx=bx+dx:ny=by+dy
1460 IF nx<1 OR nx>gw OR ny<1 OR ny>gh THEN 1490
1470 IF grd(nx,ny)<>0 AND grd(nx,ny)<>opp THEN 1490
1480 bl=bl+1:bls1(bl,0)=nx:bls1(bl,1)=ny
1490 NEXT dy
1500 NEXT dx
1510 IF bl=0 THEN act=0:RETURN:' no valid target found, we should never normally reach this state
1520 r=INT(RND*bl)+1:tx=bls1(r,0):ty=bls1(r,1)
1530 IF id=id1 THEN st1(ilt,0)=tx:st1(ilt,1)=ty:st1(isl,0)=bx:st1(isl,1)=by ELSE st2(ilt,0)=tx:st2(ilt,1)=ty:st2(isl,0)=bx:st2(isl,1)=by
1540 IF grd(tx,ty)=0 THEN act=1 ELSE GOSUB 2050'if target is opp, resolve fight
1550 IF act=1 OR act=2 THEN GOSUB 1820
1560 RETURN
1570 '
1580 ' CPU Defender
1590 GOSUB 1350
1600 RETURN
1610 '
1620 ' Populate bls1 with all valid moves
1630 bl=0
1640 minx=st(imn,0):miny=st(imn,1):maxx=st(imx,0):maxy=st(imx,1)
1650 FOR x=minx TO maxx:FOR y=miny TO maxy
1660 IF  dbgscan=1 THEN SOUND 1,1000,2,15:hx=x:hy=y:GOSUB 2240:' highlight grd scanning
1670 IF bl=blmax THEN GOTO 1800:' full list populated, return
1680 IF grd(x,y)<>id THEN GOTO 1780
1690 FOR dx=-1 TO 1:FOR dy=-1 TO 1
1700 IF dx=0 AND dy=0 THEN GOTO 1760
1710 nx=x+dx:ny=y+dy
1720 IF nx<1 OR nx>gw OR ny<1 OR ny>gh THEN GOTO 1760
1730 IF grd(nx,ny)<>0 AND grd(nx,ny)<>opp THEN GOTO 1760
1740 bl=bl+1:' found valid target at nx,ny
1750 bls1(bl,0)=x:bls1(bl,1)=y:GOTO 1780:' valid block found move to next
1760 NEXT dy
1770 NEXT dx
1780 NEXT y
1790 NEXT x
1800 RETURN
1810 '
1820 ' update stats after move, or won fight
1830 IF id=id2 THEN 1940
1840 ' player 1 was active
1850 c1=c1+1:st1(ism,0)=st1(ism,0)+tx:st1(ism,1)=st1(ism,1)+ty:st1(ivg,0)=INT(st1(ism,0)/c1):st1(ivg,1)=INT(st1(ism,1)/c1)
1860 st1(imn,0)=MIN(st1(imn,0),tx):st1(imn,1)=MIN(st1(imn,1),ty):st1(imx,0)=MAX(st1(imx,0),tx):st1(imx,1)=MAX(st1(imx,1),ty)
1870 FOR i=0 TO ial:st(i,0)=st1(i,0):st(i,1)=st1(i,1):NEXT
1880 IF act<>2 THEN RETURN
1890 IF c2-1<1 THEN c2=0:RETURN
1900 c2=c2-1:st2(ism,0)=st2(ism,0)-tx:st2(ism,1)=st2(ism,1)-ty:st2(ivg,0)=INT(st2(ism,0)/c2):st2(ivg,1)=INT(st2(ism,1)/c2)
1910 ' recalculate min max x y if needed due to opp's lost block
1920 IF st2(imn,0)=tx OR st2(imn,1)=ty OR st2(imx,0)=tx OR st2(imx,1)=ty THEN GOSUB 2090:'recalculate min and max x,y
1930 GOTO 2030
1940 ' player 2 was active
1950 c2=c2+1:st2(ism,0)=st2(ism,0)+tx:st2(ism,1)=st2(ism,1)+ty:st2(ivg,0)=INT(st2(ism,0)/c2):st2(ivg,1)=INT(st2(ism,1)/c2)
1960 st2(imn,0)=MIN(st2(imn,0),tx):st2(imn,1)=MIN(st2(imn,1),ty):st2(imx,0)=MAX(st2(imx,0),tx):st2(imx,1)=MAX(st2(imx,1),ty)
1970 FOR i=0 TO ial:st(i,0)=st2(i,0):st(i,1)=st2(i,1):NEXT
1980 IF act<>2 THEN RETURN
1990 IF c1-1<1 THEN c1=0:RETURN
2000 c1=c1-1:st1(ism,0)=st1(ism,0)-tx:st1(ism,1)=st1(ism,1)-ty:st1(ivg,0)=INT(st1(ism,0)/c1):st1(ivg,1)=INT(st1(ism,1)/c1)
2010 ' recalculate min max x y if needed due to opp's lost block
2020 IF st1(imn,0)=tx OR st1(imn,1)=ty OR st1(imx,0)=tx OR st1(imx,1)=ty THEN GOSUB 2090:'recalculate min and max x,y
2030 RETURN
2040 '
2050 ' resolve fg
2060 r=RND:IF r>0.5 THEN act=2 ELSE act=3
2070 RETURN
2080 '
2090 ' recalculate minx maxx miny maxy
2100 IF opp=id1 THEN cminx=st1(imn,0):cmaxx=st1(imx,0):cminy=st1(imn,1):cmaxy=st1(imx,1) ELSE cminx=st2(imn,0):cmaxx=st2(imx,0):cminy=st2(imn,1):cmaxy=st2(imx,1)
2110 imnx=0:imxx=0:imny=0:imxy=0
2120 FOR cx=cminx TO cmaxx:FOR cy=cminy TO cmaxy
2130 IF cx=tx AND cy=ty THEN 2190
2140 IF grd(cx,cy)<>opp THEN GOTO 2190
2150 IF imnx=0 THEN imnx=cx ELSE imnx=MIN(imnx,cx)
2160 IF imxx=0 THEN imxx=cx ELSE imxx=MAX(imxx,cx)
2170 IF imny=0 THEN imny=cy ELSE imny=MIN(imny,cy)
2180 IF imxy=0 THEN imxy=cy ELSE imxy=MAX(imxy,cy)
2190 NEXT cy
2200 NEXT cx
2210 IF opp=id1 THEN st1(imn,0)=imnx:st1(imn,1)=imny:st1(imx,0)=imxx:st1(imx,1)=imxy ELSE st2(imn,0)=imnx:st2(imn,1)=imny:st2(imx,0)=imxx:st2(imx,1)=imxy
2220 RETURN
2230 '
2240 ' highlight
2250 IF hx<1 OR hy<1 OR hx>gw OR hy>gh THEN RETURN
2260 LOCATE ofx+hx,ofy+hy:PEN cpuclr:PRINT hb$
2270 IF grd(hx,hy)=id1 THEN PEN cl1:a$=b1$ ELSE IF grd(hx,hy)=id2 THEN PEN cl2:a$=b2$ ELSE a$=eb$
2280 LOCATE ofx+hx,ofy+hy:PRINT a$
2290 RETURN
2300 '
2310 ' print block counts
2320 PEN cl1:LOCATE cols-3,c1y:PRINT USING "####";c1;
2330 PEN cl2:LOCATE cols-3,c2y:PRINT USING "####";c2;
2340 RETURN
2350 '
2360 ' print centered message
2370 tmp=INT(LEN(ms$)/2):tmpx=MAX(1,hcols-tmp):LOCATE tmpx,hrows:PEN ctx:PRINT ms$:tmp=LEN(ms$)
2380 CALL &BB18:LOCATE tmpx,hrows:PRINT SPC(tmp)
2390 RETURN
2400 '
2410 ' delay routine
2420 IF dly>0 THEN FOR i=0 TO dly:NEXT:RETURN' delay routine
2430 '
2440 ' draw grid border
2450 ' draw top and bottom horizontal
2460 PEN ctx:LOCATE ofx,ofy:PRINT CHR$(150)+STRING$(gw,CHR$(154))+CHR$(156)
2470 LOCATE ofx,ofy+gh+1:PRINT CHR$(147)+STRING$(gw,CHR$(154))+CHR$(153)
2480 ' draw verticals
2490 a$=CHR$(149):FOR i=ofy+1 TO gh+ofy:LOCATE ofx,i:PRINT a$;:LOCATE ofx+gw+1,i:PRINT a$:NEXT
2500 RETURN
2510 '
2520 ' Define grid size
2530 gw=MAX(INT(cols/3),INT(RND*cols)+2):gw=MIN(gw,cols-2):hgw=INT(gw/2):' -2 cols for the vertical grid lines
2540 gh=MAX(INT(rows/3),INT(RND*rows)+2):gh=MIN(gh,rows-5):hgh=INT(gh/2):' -5 rows for 2 cpu rows,1 status line and and 2 grid rows
2550 gwh=gw*gh
2560 ofx=INT((cols-gw)/2):ofy=INT((rows-gh)/2)+1:' locate offsets: ofy+1 row to leave a blank line for the status line
2570 RETURN
2580 '
2590 ' Define starting positions
2600 st1x=INT(gw/2)+1:st1y=1:st2x=INT(gw/2)+1:st2y=gh
2610 id1=1:id2=2:c1=1:c2=1
2620 FOR i=0 TO ial:st1(i,0)=st1x:st1(i,1)=st1y:st2(i,0)=st2x:st2(i,1)=st2y:NEXT
2630 grd(st1x,st1y)=id1:grd(st2x,st2y)=id2
2640 st1(ism,0)=st1x:st1(ism,1)=st1y:st1(ivg,0)=st1x:st1(ivg,1)=st1y:st1(imn,0)=st1x:st1(imn,1)=st1y:st1(imx,0)=st1x:st1(imx,1)=st1y:st1(isl,0)=st1x:st1(isl,1)=st1y
2650 st2(ism,0)=st2x:st2(ism,1)=st2y:st2(ivg,0)=st2x:st2(ivg,1)=st2y:st2(imn,0)=st2x:st2(imn,1)=st2y:st2(imx,0)=st2x:st2(imx,1)=st2y:st2(isl,0)=st2x:st2(isl,1)=st2y
2660 LOCATE  ofx+st1x,ofy+st1y:PEN cl1:PRINT b1$
2670 LOCATE  ofx+st2x,ofy+st2y:PEN cl2:PRINT b2$
2680 p=0.1:sb=INT((gwh/2)*p):' starting blocks formula
2690 ' randomly select starting blocks for reach player
2700 WHILE c1<sb OR c2<sb
2710 ' Player 1 block selection
2720 IF c1<sb THEN
2730 tmpx=INT(RND*gw)+1:tmpy=INT(RND*(gh/2))+1
2740 IF grd(tmpx,tmpy)<>0 THEN 2780
2750 grd(tmpx,tmpy)=id1:c1=c1+1:st1(ism,0)=st1(ism,0)+tmpx:st1(ism,1)=st1(ism,1)+tmpy
2760 st1(ivg,0)=INT(st1(ism,0)/c1):st1(ivg,1)=INT(st1(ism,1)/c1):st1(imn,0)=MIN(st1(imn,0),tmpx):st1(imn,1)=MIN(st1(imn,1),tmpy):st1(imx,0)=MAX(st1(imx,0),tmpx):st1(imx,1)=MAX(st1(imx,1),tmpy)
2770 SOUND 1,MAX(100,1500-(c1+c2)*10),2,10:LOCATE ofx+tmpx,ofy+tmpy:PEN cl1:PRINT b1$
2780 ' Player 2 block selection
2790 IF c2<sb THEN
2800 tmpx=INT(RND*gw)+1:tmpy=INT(RND*(gh/2))+1:tmpy=MIN(gh,INT(RND*(gh/2))+(gh/2)+1)
2810 IF grd(tmpx,tmpy)<>0 THEN 2850
2820 grd(tmpx,tmpy)=id2:c2=c2+1:st2(ism,0)=st2(ism,0)+tmpx:st2(ism,1)=st2(ism,1)+tmpy
2830 st2(ivg,0)=INT(st2(ism,0)/c1):st2(ivg,1)=INT(st2(ism,1)/c1):st2(imn,0)=MIN(st2(imn,0),tmpx):st2(imn,1)=MIN(st2(imn,1),tmpy):st2(imx,0)=MAX(st2(imx,0),tmpx):st2(imx,1)=MAX(st2(imx,1),tmpy)
2840 SOUND 1,MAX(100,1500-(c1+c2)*15),2,10:LOCATE ofx+tmpx,ofy+tmpy:PEN cl2:PRINT b2$
2850 WEND
2860 RETURN
2870 '
2880 ' error handling
2890 ms$="Error with code"+STR$(ERR)+" at line"+STR$(ERL):GOSUB 2360
2900 MODE 2:INK 0,1:INK 1,26:PEN 1:PAPER 0:BORDER 1:LOCATE 1,1:PRINT ms$
2910 IF ERR=5 THEN PRINT"Improper Argument"
2920 END
2930 INK 0,1:INK 1,26:PAPER 0:PEN 1:BORDER 1:MODE 2:END
2940 ' DATA
2950 DATA "start","sum","avg","min","max","sel","last":' sel is last selected position, last is last occupied position
2960 DATA "Normal","Attacker","Random","Defender":' Personality names
2970 DATA "Nrm","Att","Rnd","Def":' Shorthands for Personality names
