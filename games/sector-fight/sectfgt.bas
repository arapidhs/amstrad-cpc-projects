10 ' debug messages and debug grid scanning
20 dbgscan=1
30 RANDOMIZE TIME
40 ON ERROR GOTO 3090
50 ON BREAK GOSUB 3140
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
340 DIM st$(ial):RESTORE 3160:FOR i=0 TO ial:READ st$(i):NEXT
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
580 IF smd=0 THEN RESTORE 3180 ELSE RESTORE 3170
590 FOR i=0 TO psz:READ pn$(i):NEXT
600 '
610 ' Assign personalities based on personality probabilities: pnprb()
620 r=RND:IF r<pnprb(pnrm) THEN pn1=pnrm ELSE IF r<pnprb(patt) THEN pn1=patt ELSE IF r<pnprb(prnd) THEN pn1=prnd ELSE pn1=pdef
630 r=RND:IF r<pnprb(pnrm) THEN pn2=pnrm ELSE IF r<pnprb(patt) THEN pn2=patt ELSE IF r<pnprb(prnd) THEN pn2=prnd ELSE pn2=pdef
640 '
650 ' Wait for key press
660 CLS:IF smd=0 THEN ms$="Press key to start" ELSE ms$="Press any key to start":GOSUB 2570:CLS
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
770 GOSUB 2730:DIM grd(gw,gh):grd(0,0)=-1:' setup grid dimensions
780 GOSUB 2650:' draw grid border
790 GOSUB 2800:' define and draw starting positions and stats
800 blmax=(gw+gh)*2:DIM bls1(blmax,1):blc1=0:DIM bls2(blmax,1):blc1=2:'lists of valid moves for player 1of and 2of, blc1,2 point to the end of fthe list
810 '
820 GOSUB 2520:' print block counts
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
954 '
955 'Process cpu action based on personality
970 act=0:ON pn+1 GOSUB 1400,1440,1480,1680
975 LOCATE clx,cly:PRINT SPC(LEN(mst$));
980 ' act 0 is no move found, 1 is move, 2 fight won, 3 fight loss
1000 IF act=0 THEN THEN GOTO 1270:'no valid move found, end game
1120 ' print move or fg results to screen
1140 IF act=1 THEN hx=tx:hy=ty:GOSUB 2450:SOUND 1,200,20,15:GOSUB 2450:' move highlight, play sound highlight
1150 IF act=2 THEN hx=tx:hy=ty:GOSUB 2450:SOUND 1,142,20,15:SOUND 1,95,20,15:GOSUB 2450:' fight won highlight, play sound highlight
1160 IF act=3 THEN hx=tx:hy=ty:GOSUB 2450:SOUND 1,95,20,15:SOUND 1,125,20,15:GOSUB 2450:' fight lost highlight, play sound highlight
1165 IF act=1 OR act=2 THEN grd(tx,ty)=id
1170 IF grd(tx,ty)=id THEN LOCATE ofx+tx,ofy+ty:PEN cpuclr:PRINT b$;
1190 GOSUB 2520:' print block counts
1200 GOSUB 2620:' delay routine
1220 IF c1+c2>=gwh OR c1=0 OR c2=0 THEN GOTO 1310
1230 IF turn=1 THEN turn=2 ELSE turn=1
1240 trs=1
1250 WEND
1260 '
1270 ' error: no valid move found
1280 IF smd<>0 THEN ms$="Error: no valid move found" ELSE ms$="Err: no move found"
1290 GOSUB 2570:GOTO 1360:' print error message and exit
1300 '
1310 ' end game
1320 IF smd<>0 THEN ms$="Game Over: "+id1$+":"+STR$(c1)+" "+id2$+":"+STR$(c2) ELSE ms$="Game Over:"+MID$(STR$(c1),2)+"/"+MID$(STR$(c2),2)
1330 GOSUB 2570
1340 IF c1>c2 THEN ms$="CPU 1 wins!" ELSE IF c1<c2 THEN ms$="CPU 2 wins!" ELSE ms$="Draw!"
1350 GOSUB 2570
1360 CLS:ms$="Play again? (Y:N)":LOCATE hcols-INT(LEN(ms$)/2),hrows:INPUT"Play Again? (Y:N)",a$
1370 IF UPPER$(a$)="Y" THEN RUN ELSE GOTO 3140
1380 END
1390 '
1400 ' CPU Normal
1410 GOSUB 1480
1420 RETURN
1430 '
1440 ' CPU Attacker
1450 GOSUB 1480
1460 RETURN
1470 '
1480 ' CPU Random
1490 bx=0:by=0:tx=0:ty=0:bl=0
1500 GOSUB 1720:' populate bls1 with all valid moves
1510 IF bl=0 THEN act=0:RETURN:' no valid move found, we should never reach this state normally
1520 r=INT(RND*bl)+1:bx=bls1(r,0):by=bls1(r,1)
1530 ' We found a random valid block next we need to find a valid random target
1540 IF dbgscan=1 THEN SOUND 1,1000,2,15:hx=bx:hy=by:GOSUB 2450:' highlight selected valid block
1550 bl=0
1560 FOR dx=-1 TO 1:FOR dy=-1 TO 1
1570 IF dx=0 AND dy=0 THEN GOTO 1620
1580 nx=bx+dx:ny=by+dy
1590 IF nx<1 OR nx>gw OR ny<1 OR ny>gh THEN 1620
1600 IF grd(nx,ny)<>0 AND grd(nx,ny)<>opp THEN 1620
1610 bl=bl+1:bls1(bl,0)=nx:bls1(bl,1)=ny
1620 NEXT dy
1630 NEXT dx
1640 IF bl=0 THEN act=0:RETURN:' no valid target found, we should never normally reach this state
1650 r=INT(RND*bl)+1:tx=bls1(r,0):ty=bls1(r,1)
1651 IF id=id1 THEN st1(ilt,0)=tx:st1(ilt,1)=ty:st1(isl,0)=bx:st1(isl,1)=by ELSE st2(ilt,0)=tx:st2(ilt,1)=ty:st2(isl,0)=bx:st2(isl,1)=by
1652 IF grd(tx,ty)=0 THEN act=1 ELSE GOSUB 2040'if target is opp, resolve fight
1653 IF act=1 OR act=2 THEN GOSUB 1920
1660 RETURN
1670 '
1680 ' CPU Defender
1690 GOSUB 1480
1700 RETURN
1710 '
1720 ' Populate bls1 with all valid moves
1730 bl=0
1740 minx=st(imn,0):miny=st(imn,1):maxx=st(imx,0):maxy=st(imx,1)
1750 FOR x=minx TO maxx:FOR y=miny TO maxy
1760 IF  dbgscan=1 THEN SOUND 1,1000,2,15:hx=x:hy=y:GOSUB 2450:' highlight grd scanning
1770 IF bl=blmax THEN GOTO 1900:' full list populated, return
1780 IF grd(x,y)<>id THEN GOTO 1880
1790 FOR dx=-1 TO 1:FOR dy=-1 TO 1
1800 IF dx=0 AND dy=0 THEN GOTO 1860
1810 nx=x+dx:ny=y+dy
1820 IF nx<1 OR nx>gw OR ny<1 OR ny>gh THEN GOTO 1860
1830 IF grd(nx,ny)<>0 AND grd(nx,ny)<>opp THEN GOTO 1860
1840 bl=bl+1:' found valid target at nx,ny
1850 bls1(bl,0)=x:bls1(bl,1)=y:GOTO 1880:' valid block found move to next
1860 NEXT dy
1870 NEXT dx
1880 NEXT y
1890 NEXT x
1900 RETURN
1910 '
1920 ' update stats after move, or won fight
1930 IF id=id2 THEN 1979
1935 ' player 1 was active
1940 c1=c1+1:st1(ism,0)=st1(ism,0)+tx:st1(ism,1)=st1(ism,1)+ty:st1(ivg,0)=INT(st1(ism,0)/c1):st1(ivg,1)=INT(st1(ism,1)/c1)
1950 st1(imn,0)=MIN(st1(imn,0),tx):st1(imn,1)=MIN(st1(imn,1),ty):st1(imx,0)=MAX(st1(imx,0),tx):st1(imx,1)=MAX(st1(imx,1),ty)
1970 FOR i=0 TO ial:st(i,0)=st1(i,0):st(i,1)=st1(i,1):NEXT
1971 IF act<>2 THEN RETURN
1974 IF c2-1<1 THEN c2=0:RETURN
1975 c2=c2-1:st2(ism,0)=st2(ism,0)-tx:st2(ism,1)=st2(ism,1)-ty:st2(ivg,0)=INT(st2(ism,0)/c2):st2(ivg,1)=INT(st2(ism,1)/c2)
1976 ' recalculate min max x y if needed due to opp's lost block
1977 IF st2(imn,0)=tx OR st2(imn,1)=ty OR st2(imx,0)=tx OR st2(imx,1)=ty THEN GOSUB 2300:'recalculate min and max x,y
1978 GOTO 2020
1979 ' player 2 was active
1980 c2=c2+1:st2(ism,0)=st2(ism,0)+tx:st2(ism,1)=st2(ism,1)+ty:st2(ivg,0)=INT(st2(ism,0)/c2):st2(ivg,1)=INT(st2(ism,1)/c2)
1990 st2(imn,0)=MIN(st2(imn,0),tx):st2(imn,1)=MIN(st2(imn,1),ty):st2(imx,0)=MAX(st2(imx,0),tx):st2(imx,1)=MAX(st2(imx,1),ty)
2010 FOR i=0 TO ial:st(i,0)=st2(i,0):st(i,1)=st2(i,1):NEXT
2011 IF act<>2 THEN RETURN
2012 IF c1-1<1 THEN c1=0:RETURN
2013 c1=c1-1:st1(ism,0)=st1(ism,0)-tx:st1(ism,1)=st1(ism,1)-ty:st1(ivg,0)=INT(st1(ism,0)/c1):st1(ivg,1)=INT(st1(ism,1)/c1)
2014 ' recalculate min max x y if needed due to opp's lost block
2015 IF st1(imn,0)=tx OR st1(imn,1)=ty OR st1(imx,0)=tx OR st1(imx,1)=ty THEN GOSUB 2300:'recalculate min and max x,y
2020 RETURN
2030 '
2040 ' resolve fg
2050 r=RND:IF r>0.5 THEN act=2 ELSE act=3
2060 RETURN
2070 '
2300 ' recalculate minx maxx miny maxy
2320 IF opp=id1 THEN cminx=st1(imn,0):cmaxx=st1(imx,0):cminy=st1(imn,1):cmaxy=st1(imx,1) ELSE cminx=st2(imn,0):cmaxx=st2(imx,0):cminy=st2(imn,1):cmaxy=st2(imx,1)
2330 imnx=0:imxx=0:imny=0:imxy=0
2340 FOR cx=cminx TO cmaxx:FOR cy=cminy TO cmaxy
2345 IF cx=tx AND cy=ty THEN 2400
2350 IF grd(cx,cy)<>opp THEN GOTO 2400
2360 IF imnx=0 THEN imnx=cx ELSE imnx=MIN(imnx,cx)
2370 IF imxx=0 THEN imxx=cx ELSE imxx=MAX(imxx,cx)
2380 IF imny=0 THEN imny=cy ELSE imny=MIN(imny,cy)
2390 IF imxy=0 THEN imxy=cy ELSE imxy=MAX(imxy,cy)
2400 NEXT cy
2410 NEXT cx
2420 IF opp=id1 THEN st1(imn,0)=imnx:st1(imn,1)=imny:st1(imx,0)=imxx:st1(imx,1)=imxy ELSE st2(imn,0)=imnx:st2(imn,1)=imny:st2(imx,0)=imxx:st2(imx,1)=imxy
2430 RETURN
2440 '
2450 ' highlight
2460 IF hx<1 OR hy<1 OR hx>gw OR hy>gh THEN RETURN
2470 LOCATE ofx+hx,ofy+hy:PEN cpuclr:PRINT hb$
2480 IF grd(hx,hy)=id1 THEN PEN cl1:a$=b1$ ELSE IF grd(hx,hy)=id2 THEN PEN cl2:a$=b2$ ELSE a$=eb$
2490 LOCATE ofx+hx,ofy+hy:PRINT a$
2500 RETURN
2510 '
2520 ' print block counts
2530 PEN cl1:LOCATE cols-3,c1y:PRINT USING "####";c1;
2540 PEN cl2:LOCATE cols-3,c2y:PRINT USING "####";c2;
2550 RETURN
2560 '
2570 ' print centered message
2580 tmp=INT(LEN(ms$)/2):tmpx=MAX(1,hcols-tmp):LOCATE tmpx,hrows:PEN ctx:PRINT ms$:tmp=LEN(ms$)
2590 CALL &BB18:LOCATE tmpx,hrows:PRINT SPC(tmp)
2600 RETURN
2610 '
2620 ' delay routine
2630 IF dly>0 THEN FOR i=0 TO dly:NEXT:RETURN' delay routine
2640 '
2650 ' draw grid border
2660 ' draw top and bottom horizontal
2670 PEN ctx:LOCATE ofx,ofy:PRINT CHR$(150)+STRING$(gw,CHR$(154))+CHR$(156)
2680 LOCATE ofx,ofy+gh+1:PRINT CHR$(147)+STRING$(gw,CHR$(154))+CHR$(153)
2690 ' draw verticals
2700 a$=CHR$(149):FOR i=ofy+1 TO gh+ofy:LOCATE ofx,i:PRINT a$;:LOCATE ofx+gw+1,i:PRINT a$:NEXT
2710 RETURN
2720 '
2730 ' Define grid size
2740 gw=MAX(INT(cols/3),INT(RND*cols)+2):gw=MIN(gw,cols-2):hgw=INT(gw/2):' -2 cols for the vertical grid lines
2750 gh=MAX(INT(rows/3),INT(RND*rows)+2):gh=MIN(gh,rows-5):hgh=INT(gh/2):' -5 rows for 2 cpu rows,1 status line and and 2 grid rows
2760 gwh=gw*gh
2770 ofx=INT((cols-gw)/2):ofy=INT((rows-gh)/2)+1:' locate offsets: ofy+1 row to leave a blank line for the status line
2780 RETURN
2790 '
2800 ' Define starting positions
2810 st1x=INT(gw/2)+1:st1y=1:st2x=INT(gw/2)+1:st2y=gh
2820 id1=1:id2=2:c1=1:c2=1
2830 FOR i=0 TO ial:st1(i,0)=st1x:st1(i,1)=st1y:st2(i,0)=st2x:st2(i,1)=st2y:NEXT
2840 grd(st1x,st1y)=id1:grd(st2x,st2y)=id2
2850 st1(ism,0)=st1x:st1(ism,1)=st1y:st1(ivg,0)=st1x:st1(ivg,1)=st1y:st1(imn,0)=st1x:st1(imn,1)=st1y:st1(imx,0)=st1x:st1(imx,1)=st1y:st1(isl,0)=st1x:st1(isl,1)=st1y
2860 st2(ism,0)=st2x:st2(ism,1)=st2y:st2(ivg,0)=st2x:st2(ivg,1)=st2y:st2(imn,0)=st2x:st2(imn,1)=st2y:st2(imx,0)=st2x:st2(imx,1)=st2y:st2(isl,0)=st2x:st2(isl,1)=st2y
2870 LOCATE  ofx+st1x,ofy+st1y:PEN cl1:PRINT b1$
2880 LOCATE  ofx+st2x,ofy+st2y:PEN cl2:PRINT b2$
2890 p=0.1:sb=INT((gwh/2)*p):' starting blocks formula
2900 ' randomly select starting blocks for reach player
2910 WHILE c1<sb OR c2<sb
2920 ' Player 1 block selection
2930 IF c1<sb THEN
2940 tmpx=INT(RND*gw)+1:tmpy=INT(RND*(gh/2))+1
2950 IF grd(tmpx,tmpy)<>0 THEN 2990
2960 grd(tmpx,tmpy)=id1:c1=c1+1:st1(ism,0)=st1(ism,0)+tmpx:st1(ism,1)=st1(ism,1)+tmpy
2970 st1(ivg,0)=INT(st1(ism,0)/c1):st1(ivg,1)=INT(st1(ism,1)/c1):st1(imn,0)=MIN(st1(imn,0),tmpx):st1(imn,1)=MIN(st1(imn,1),tmpy):st1(imx,0)=MAX(st1(imx,0),tmpx):st1(imx,1)=MAX(st1(imx,1),tmpy)
2980 SOUND 1,MAX(100,1500-(c1+c2)*10),2,10:LOCATE ofx+tmpx,ofy+tmpy:PEN cl1:PRINT b1$
2990 ' Player 2 block selection
3000 IF c2<sb THEN
3010 tmpx=INT(RND*gw)+1:tmpy=INT(RND*(gh/2))+1:tmpy=MIN(gh,INT(RND*(gh/2))+(gh/2)+1)
3020 IF grd(tmpx,tmpy)<>0 THEN 3060
3030 grd(tmpx,tmpy)=id2:c2=c2+1:st2(ism,0)=st2(ism,0)+tmpx:st2(ism,1)=st2(ism,1)+tmpy
3040 st2(ivg,0)=INT(st2(ism,0)/c1):st2(ivg,1)=INT(st2(ism,1)/c1):st2(imn,0)=MIN(st2(imn,0),tmpx):st2(imn,1)=MIN(st2(imn,1),tmpy):st2(imx,0)=MAX(st2(imx,0),tmpx):st2(imx,1)=MAX(st2(imx,1),tmpy)
3050 SOUND 1,MAX(100,1500-(c1+c2)*15),2,10:LOCATE ofx+tmpx,ofy+tmpy:PEN cl2:PRINT b2$
3060 WEND
3070 RETURN
3080 '
3090 ' error handling
3100 ms$="Error with code"+STR$(ERR)+" at line"+STR$(ERL):GOSUB 2570
3110 MODE 2:INK 0,1:INK 1,26:PEN 1:PAPER 0:BORDER 1:LOCATE 1,1:PRINT ms$
3120 IF ERR=5 THEN PRINT"Improper Argument"
3130 END
3140 INK 0,1:INK 1,26:PAPER 0:PEN 1:BORDER 1:MODE 2:END
3150 ' DATA
3160 DATA "start","sum","avg","min","max","sel","last":' sel is last selected position, last is last occupied position
3170 DATA "Normal","Attacker","Random","Defender":' Personality names
3180 DATA "Nrm","Att","Rnd","Def":' Shorthands for Personality names
