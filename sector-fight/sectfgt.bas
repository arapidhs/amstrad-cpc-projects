10 dbg=0:dbgscan=1:' debug messages and debug grid scanning
20 RANDOMIZE TIME
30 ON ERROR GOTO 2820
40 ON BREAK GOSUB 2880
50 MODE 1:INK 0,0:INK 1,26:PAPER 0:PEN 1:BORDER 0
60 '
70 PRINT "Select mode (0-Mode0 1-Mode1 2-Mode2)";
80 a$="":WHILE a$="":a$=INKEY$:WEND
90 IF a$<>"0" AND a$<>"1" AND a$<>"2" THEN a$="1"
100 smd=VAL(a$)
110 cols=20*(2^smd):rows=25:hcols=INT(cols/2):hrows=INT(rows/2)
120 PRINT"Select move delay (1=0.5s 2=1s else=0s)";
130 a$="":WHILE a$="":a$=INKEY$:WEND
140 IF a$="1" THEN dly=500 ELSE IF a$="2" THEN dly=1000 ELSE dly=0
150 '
160 ' Screen initialization and colors - cbg bg color, cl1,cl2 cpu colors, ctx text color
170 IF smd=2 THEN INK 0,0:INK 1,26:INK 2,26:INK 3,26:cbg=0:cl1=1:cl2=1:ctx=1 ELSE INK 0,0:INK 1,2:INK 2,6:INK 3,26:cbg=0:cl1=1:cl2=2:ctx=3
190 PAPER cbg:BORDER cbg:PEN ctx:MODE smd
200 msw=0:ms$="Loading...":GOSUB 2330:' print centered
210 '
220 GOSUB 1190:' Define grid size and starting positions
230 '
240 ' Initialize variables
250 sx=1:sy=2:' location of status line
260 ial=6:ist=0:ism=1:ivg=2:imn=3:imx=4:isl=5:ilt=6
270 DIM st(ial,1):DIM st1(ial,1):DIM st2(ial,1)
280 DIM st$(ial):st$(ist)="start":st$(ism)="sum":st$(ivg)="avg":st$(imn)="min":st$(imx)="max":st$(isl)="sel":st$(ilt)="last"
290 frmin=0.05:frmax=0.1:fravg=(frmin+frmax)/2:oppmin=-0.1:oppmax=-0.05:oppavg=(oppmin+oppmax)/2:empmin=00.1:empmax=-0.03:empavg=(empmin+empmax)/2
300 attthres=0.3:defthres=0.3:probnorm=0.5:probatt=0.5:probrnd=0.5:probdef=0.5:probatt=probatt+probnorm:probrnd=probatt+probrnd:probdef=probrnd+probndef
310 if smd<>0 then ialpn=3:DIM pn$(ialpn):pn$(0)="Normal":pn$(1)="Attacker":pn$(2)="Random":pn$(3)="Defender"
320 if smd=0 then ialpn=3:DIM pn$(ialpn):pn$(0)="Nrm":pn$(1)="Att":pn$(2)="Rnd":pn$(3)="Def"
330 if smd<>2 then b1$=CHR$(207):b2$=CHR$(207) ELSE b1$=CHR$(143):b2$=CHR$(206)
340 eb$=CHR$(32):hb$=CHR$(240)
350 c1=0:c2=0
360 ' Setup highlight blocks
370 SYMBOL 240,0,0,60,60,60,60,0,0
380 ' Assign pnonalities
390 r=RND
400 IF r<probnorm then pn1=0 ELSE IF r<probatt THEN pn1=1 ELSE IF r<probrnd THEN pn1=2 ELSE pn1=3
410 r=RND
420 IF r<probnorm then pn2=0 ELSE IF r<probatt THEN pn2=1 ELSE IF r<probrnd THEN pn2=2 ELSE pn2=3
430 maxpn=MAX(LEN(pn$(pn1)),LEN(pn$(pn2)))
440 ' Initialize grd
450 DIM grd(gw,gh):grd(0,0)=-1:blmax=(gw+gh)*2:DIM bls(blmax,1)
460 id1=1:id2=2:c1=1:c2=1
470 FOR i=0 TO ial:st1(i,0)=start1x:st1(i,1)=start1y:st2(i,0)=start2x:st2(i,1)=start2y:NEXT
480 grd(start1x,start1y)=id1:grd(start2x,start2y)=id2
490 calcid=id1:GOSUB 2140:' calculate min max for x,y
500 calcid=id2:GOSUB 2140
510 ' Wait for keypress
520 CLS:msw=1:IF smd=0 THEN ms$="Press key to start" ELSE ms$="Press any key to start"
530 GOSUB 2330
540 CLS
550 ' print personalities
560 id1$="CPU 1":id2$="CPU 2":algnx=maxpn+MAX(LEN(id1$),LEN(id2$))+4:' aligned x location to print status on cpu rows
570 LOCATE 1,1:PEN cl1:PRINT id1$;": ";pn$(pn1)
580 LOCATE 1,rows:PEN cl2:PRINT id2$;": ";pn$(pn2)
590 GOSUB 2280:' print block counts
600 GOSUB 2470:' draw grd BORDER
610 LOCATE  ofx+start1x,ofy+start1y:PEN cl1:PRINT b1$
620 LOCATE  ofx+start2x,ofy+start2y:PEN cl2:PRINT b2$
630 ' Game loop
640 turn=1:trn=0
650 WHILE c1+c2<gw*gh
660 ' CPU turn
670 trn=trn+1:trs=0
680 IF turn=1 THEN id=id1:opp=id2:cpuclr=cl1:b$=b1$:pn=pn1:FOR i=0 TO ial:st(i,0)=st1(i,0):st(i,1)=st1(i,1):NEXT
690 IF turn=2 THEN id=id2:opp=id1:cpuclr=cl2:b$=b2$:pn=pn2:FOR i=0 TO ial:st(i,0)=st2(i,0):st(i,1)=st2(i,1):NEXT
700 IF dbg=1 THEN GOSUB 2620
710 LOCATE sx,sy:PRINT STRING$(cols," ")
720 prg=ROUND((c1+c2)/(gwh)*100,2)
730 PEN ctx:LOCATE sx,sy:PRINT "Turn";trn;STR$(prg);"%"
740 IF smd<>0 THEN PEN cpuclr:ms$="Thinking.." ELSE ms$=""
750 IF id=id1 THEN ly=1 ELSE ly=rows
760 IF smd<>0 THEN LOCATE algnx,ly:PRINT ms$;
770 ON pn+1 GOSUB 1260,1290,1320,1510
780 ' Process move or fg
790 mv=0:fg=0:IF grd(tx,ty)=0 THEN mv=1 ELSE IF grd(tx,ty)=opp THEN fg=1
800 IF mv=1 THEN GOTO 810 ELSE IF fg=1 THEN GOTO 850 ELSE GOTO 1100
810 GOSUB 1790:' update st after move
820 IF smd<>0 THEN LOCATE algnx,ly:PRINT SPC(LEN(ms$))
830 IF smd<>0 THEN ms$="Moved to"+STR$(tx)+","+STR$(ty) ELSE ms$=""
840 GOTO 920:' print resuts to Screen
850 ' Simple fg resolution
860 GOSUB 1900:' resolve fg
870 GOSUB 1940:' update start after fg
880 IF smd<>0 THEN LOCATE algnx,ly:PRINT SPC(LEN(ms$))
890 IF smd<>0 THEN GOTO 920 ELSE ms$="":GOTO 920
900 GOTO 920:' print results to screen
910 IF wn=1 THEN ms$="Won at"+STR$(tx)+","+STR$(ty) ELSE ms$="Lost at"+STR$(tx)+","+STR$(ty)
920 ' print move or fg results to screen
930 IF mv=1 or wn=1 THEN grd(tx,ty)=id
940 IF mv=1 THEN hx=tx:hy=ty:GOSUB 1730:SOUND 1,200,20,15:GOSUB 1730:' highlight, play sound highlight
950 IF fg=1 AND wn=1 THEN hx=tx:hy=ty:GOSUB 1730:SOUND 1,142,20,15:SOUND 1,95,20,15:GOSUB 1730:' highlight, play sound highlight
960 IF fg=1 AND wn=0 THEN hx=tx:hy=ty:GOSUB 1730:SOUND 1,95,20,15:SOUND 1,125,20,15:GOSUB 1730:' highlight, play sound highlight
970 IF grd(tx,ty)=id THEN LOCATE ofx+tx,ofy+ty:PEN cpuclr:PRINT b$;
980 IF id=1 THEN LOCATE algnx,1 ELSE LOCATE algnx,rows
990 IF LEN(ms$)>0 THEN PEN cpuclr:PRINT ms$
1000 GOSUB 2280:' print block counts
1010 GOSUB 2430:' delay routine
1020 IF id=1 THEN LOCATE algnx,1 ELSE LOCATE algnx,rows:PRINT SPC(LEN(ms$))
1030 IF c1+c2>=gwh OR c1=0 OR c2=0 THEN GOTO 1110
1040 IF turn=1 THEN turn=2 ELSE turn=1
1050 trs=1
1060 IF dbg=1 THEN GOSUB 2620:
1070 WEND
1080 ' error: no valid move found
1090 IF smd<>0 THEN ms$="Error: no valid move found" ELSE ms$="Err: no move found"
1100 msw=1:GOSUB 2330:GOTO 1160:' print error message and exit
1110 ' end game
1120 IF smd<>0 THEN ms$="Game Over: "+id1$+":"+STR$(c1)+" "+id2$+":"+STR$(c2) ELSE ms$="Game Over:"+MID$(STR$(c1),2)+"/"+MID$(STR$(c2),2)
1130 msw=1:GOSUB 2330
1140 msw=1:IF c1>c2 THEN ms$="CPU 1 wins!" ELSE IF c1<c2 THEN ms$="CPU 2 wins!" ELSE ms$="Draw!"
1150 GOSUB 2330
1160 CLS:ms$="Play again? (Y:N)":LOCATE INT(cols/2)-INT(LEN(ms$)/2),INT(rows/2):INPUT"Play Again? (Y:N)",a$
1170 IF UPPER$(a$)="Y" THEN RUN ELSE GOTO 2880
1180 END
1190 ' Define grid size and starting positions
1200 gw=INT(RND*cols)+2:gw=MIN(gw,cols-2):hgw=INT(gw/2):' -2 cols for the vertical grid lines
1210 gh=INT(RND*rows)+2:gh=MIN(gh,rows-5):hgh=INT(gh/2):' -5 rows for 2 cpu rows,1 status line and and 2 grid rows
1220 gwh=gw*gh
1230 ofx=INT((cols-gw)/2):ofy=INT((rows-gh)/2)+1:' locate offsets ofy+1 row to leave a blank line for the status line
1240 start1x=hgw+(gw MOD 2):start1y=1:start2x=hgw+(gw MOD 2):start2y=gh
1250 RETURN
1260 ' CPU Normal
1270 GOSUB 1320
1280 RETURN
1290 ' CPU Attacker
1300 GOSUB 1320
1310 RETURN
1320 ' CPU Random
1330 bx=0:by=0:tx=0:ty=0:bl=0
1340 GOSUB 1540:' populate bls with all valid moves
1350 IF bl=0 THEN GOTO 1500:' no valid move found, we should never reach this state normally
1360 r=INT(RND*bl)+1:bx=bls(r,0):by=bls(r,1)
1370 ' We found a random valid block next we need to find a valid random target
1380 IF dbgscan=1 THEN SOUND 1,1000,2,15:hx=bx:hy=by:GOSUB 1730:' highlight selected valid block
1390 bl=0
1400 FOR dx=-1 TO 1:FOR dy=-1 TO 1
1410 IF dx=0 AND dy=0 THEN GOTO 1460
1420 nx=bx+dx:ny=by+dy
1430 IF nx<1 OR nx>gw OR ny<1 OR ny>gh THEN GOTO 1460
1440 IF grd(nx,ny)<>0 AND grd(nx,ny)<>opp THEN GOTO 1460
1450 bl=bl+1:bls(bl,0)=nx:bls(bl,1)=ny
1460 NEXT dy
1470 NEXT dx
1480 IF bl=0 THEN GOTO 1500:' no valid target found, we should never normally reach this state
1490 r=INT(RND*bl)+1:tx=bls(r,0):ty=bls(r,1)
1500 RETURN
1510 ' CPU Defender
1520 GOSUB 1320
1530 RETURN
1540 ' Populate bls with all valid moves
1550 bl=0
1560 minx=st(imn,0):miny=st(imn,1):maxx=st(imx,0):maxy=st(imx,1)
1570 FOR x=minx TO maxx:FOR y=miny TO maxy
1580 IF  dbgscan=1 THEN SOUND 1,1000,2,15:hx=x:hy=y:GOSUB 1730:' highlight grd scanning
1590 IF bl=blmax THEN GOTO 1720:' full list populated, return
1600 IF grd(x,y)<>id THEN GOTO 1700
1610 FOR dx=-1 TO 1:FOR dy=-1 TO 1
1620 IF dx=0 AND dy=0 THEN GOTO 1680
1630 nx=x+dx:ny=y+dy
1640 IF nx<1 OR nx>gw OR ny<1 OR ny>gh THEN GOTO 1680
1650 IF grd(nx,ny)<>0 AND grd(nx,ny)<>opp THEN GOTO 1680
1660 bl=bl+1:' found valid target at nx,ny
1670 bls(bl,0)=x:bls(bl,1)=y:GOTO 1700:' valid block found move to next
1680 NEXT dy
1690 NEXT dx
1700 NEXT y
1710 NEXT x
1720 RETURN
1730 ' highlight
1740 IF hx<1 OR hy<1 OR hx>gw OR hy>gh THEN GOTO 1780
1750 LOCATE ofx+hx,ofy+hy:PEN cpuclr:PRINT hb$
1760 IF grd(hx,hy)=id1 THEN PEN cl1:c$=b1$ ELSE IF grd(hx,hy)=id2 THEN PEN cl2:c$=b2$ ELSE c$=eb$
1770 LOCATE ofx+hx,ofy+hy:PRINT c$
1780 RETURN
1790 ' update st after move
1800 IF id=id1 THEN 1810 ELSE 1850
1810 c1=c1+1:st1(ism,0)=st1(ism,0)+tx:st1(ism,1)=st1(ism,1)+ty:st1(ivg,0)=INT(st1(ism,0)/c1):st1(ivg,1)=INT(st1(ism,1)/c1)
1820 st1(imn,0)=MIN(st1(imn,0),tx):st1(imn,1)=MIN(st1(imn,1),ty):st1(imx,0)=MAX(st1(imx,0),tx):st1(imx,1)=MAX(st1(imx,1),ty)
1830 st1(ilt,0)=tx:st1(ilt,1)=ty:st1(isl,0)=bx:st1(isl,1)=by
1840 FOR i=0 TO ial:st(i,0)=st1(i,0):st(i,1)=st1(i,1):NEXT:GOTO 1890
1850 c2=c2+1:st2(ism,0)=st2(ism,0)+tx:st2(ism,1)=st2(ism,1)+ty:st2(ivg,0)=INT(st2(ism,0)/c2):st2(ivg,1)=INT(st2(ism,1)/c2)
1860 st2(imn,0)=MIN(st2(imn,0),tx):st2(imn,1)=MIN(st2(imn,1),ty):st2(imx,0)=MAX(st2(imx,0),tx):st2(imx,1)=MAX(st2(imx,1),ty)
1870 st2(ilt,0)=tx:st2(ilt,1)=ty:st2(isl,0)=bx:st2(isl,1)=by
1880 FOR i=0 TO ial:st(i,0)=st2(i,0):st(i,1)=st2(i,1):NEXT:GOTO 1890
1890 RETURN
1900 ' resolve fg
1910 wn=0:rndfg=RND:IF rndfg>0.5 THEN wn=1
1920 IF wn=1 THEN grd(tx,ty)=id
1930 RETURN
1940 ' update st after fg
1950 IF fg=1 THEN IF id=id1 THEN st1(isl,0)=bx:st1(isl,1)=by ELSE st2(isl,0)=bx:st2(isl,1)=by
1960 IF wn=0 THEN GOTO 2130
1970 IF id=id1 THEN GOTO 1980 ELSE GOTO 2060
1980 c1=c1+1:st1(ism,0)=st1(ism,0)+tx:st1(ism,1)=st1(ism,1)+ty:st1(ivg,0)=INT(st1(ism,0)/c1):st1(ivg,1)=INT(st1(ism,1)/c1)
1990 st1(imn,0)=MIN(st1(imn,0),tx):st1(imn,1)=MIN(st1(imn,1),ty):st1(imx,0)=MAX(st1(imx,0),tx):st1(imx,1)=MAX(st1(imx,1),ty)
2000 st1(ilt,0)=tx:st1(ilt,1)=ty:st1(isl,0)=bx:st1(isl,1)=by
2010 FOR i=0 TO ial:st(i,0)=st1(i,0):st(i,1)=st1(i,1):NEXT
2020 c2=c2-1:IF c2>0 THEN st2(ism,0)=st2(ism,0)-tx:st2(ism,1)=st2(ism,1)-ty:st2(ivg,0)=INT(st2(ism,0)/c2):st2(ivg,1)=INT(st2(ism,1)/c2)
2030 ' calculate min max x y if needed
2040 IF st2(imn,0)=tx OR st2(imn,1)=ty OR st2(imx,0)=tx OR st2(imx,1)=ty THEN calcid=id2:GOSUB 2140
2050 GOTO 2130
2060 c2=c2+1:st2(ism,0)=st2(ism,0)+tx:st2(ism,1)=st2(ism,1)+ty:st2(ivg,0)=INT(st2(ism,0)/c2):st2(ivg,1)=INT(st2(ism,1)/c2)
2070 st2(imn,0)=MIN(st2(imn,0),tx):st2(imn,1)=MIN(st2(imn,1),ty):st2(imx,0)=MAX(st2(imx,0),tx):st2(imx,1)=MAX(st2(imx,1),ty)
2080 st2(ilt,0)=tx:st2(ilt,1)=ty:st2(isl,0)=bx:st2(isl,1)=by
2090 FOR i=0 TO ial:st(i,0)=st2(i,0):st(i,1)=st2(i,1):NEXT
2100 c1=c1-1:IF c1>0 THEN st1(ism,0)=st1(ism,0)-tx:st1(ism,1)=st1(ism,1)-ty:st1(ivg,0)=INT(st1(ism,0)/c1):st1(ivg,1)=INT(st1(ism,1)/c1)
2110 ' calculate min max x y if needed
2120 IF st1(imn,0)=tx OR st1(imn,1)=ty OR st1(imx,0)=tx OR st1(imx,1)=ty THEN calcid=id1:GOSUB 2140
2130 RETURN
2140 ' recalculate minx maxx miny maxy
2150 IF calcid<>id1 AND calcid<>id2 THEN GOTO 2270
2160 IF calcid=id1 THEN cminx=st1(imn,0):cmaxx=st1(imx,0):cminy=st1(imn,1):cmaxy=st1(imx,1) ELSE cminx=st2(imn,0):cmaxx=st2(imx,0):cminy=st2(imn,1):cmaxy=st2(imx,1)
2170 imnx=0:imxx=0:imny=0:imxy=0
2180 FOR cx=cminx TO cmaxx:FOR cy=cminy TO cmaxy
2190 IF grd(cx,cy)<>calcid THEN GOTO 2240
2200 IF imnx=0 THEN imnx=cx ELSE imnx=MIN(imnx,cx)
2210 IF imxx=0 THEN imxx=cx ELSE imxx=MAX(imxx,cx)
2220 IF imny=0 THEN imny=cy ELSE imny=MIN(imny,cy)
2230 IF imxy=0 THEN imxy=cy ELSE imxy=MAX(imxy,cy)
2240 NEXT cy
2250 NEXT cx
2260 IF calcid=id1 THEN st1(imn,0)=imnx:st1(imn,1)=imny:st1(imx,0)=imxx:st1(imx,1)=imxy ELSE st2(imn,0)=imnx:st2(imn,1)=imny:st2(imx,0)=imxx:st2(imx,1)=imxy
2270 RETURN
2280 ' print block counts
2290 LOCATE cols-LEN(STR$(c1))-1,1:PEN cl1:PRINT c1;
2300 LOCATE cols-LEN(STR$(c2))-1,rows:PEN cl2:PRINT c2;
2310 RETURN
2320 '
2330 ' print centered message
2340 tmp=INT(LEN(ms$)/2):tmpx=MAX(1,hcols-tmp):LOCATE tmpx,hrows:PEN ctx:PRINT ms$
2350 IF msw=1 THEN GOSUB 2380:LOCATE tmpx,hrows:PRINT SPC(tmp):' wait for key press then clear message
2360 RETURN
2370 '
2380 ' wait routine
2390 CLEAR INPUT
2400 a$=INKEY$:IF a$="" THEN 2400 ELSE CLEAR INPUT
2410 RETURN
2420 '
2430 ' delay routine
2440 IF dly>0 THEN FOR i=0 TO dly:NEXT:' delay routine
2450 RETURN
2460 '
2470 ' draw grd border
2480 PEN ctx
2490 ' draw top horizontal
2500 LOCATE ofx,ofy:PRINT CHR$(150)
2510 LOCATE ofx+1,ofy:PRINT STRING$(gw,CHR$(154))
2520 LOCATE ofx+gw+1,ofy:PRINT CHR$(156)
2530 ' draw bottom horizontal
2540 LOCATE ofx,ofy+gh+1:PRINT CHR$(147)
2550 LOCATE ofx+1,ofy+gh+1:PRINT STRING$(gw,CHR$(154))
2560 LOCATE ofx+gw+1,ofy+gh+1:PRINT CHR$(153)
2570 ' draw verticals
2580 FOR i=ofy+1 TO gh+ofy:LOCATE ofx,i:PRINT CHR$(149):NEXT
2590 FOR i=ofy+1 TO gh+ofy:LOCATE ofx+gw+1,i:PRINT CHR$(149):NEXT
2600 RETURN
2610 '
2620 ' print debug status
2630 PEN ctx:dbgx=1:dbgy=INT(rows/4)-1
2640 ms$="CPU"+STR$(id):IF trs=0 THEN ms$=ms$+" Turn start":GOTO 2650
2650 LOCATE dbgx,dbgy:PRINT ms$
2660 FOR i=0 TO ial:LOCATE dbgx,dbgy+i+1:PRINT st$(i);st(i,0);st(i,1);:NEXT
2670 GOSUB 2380:' wait
2680 dbgxend=cols-dbgx:dbgyend=dbgy+ial+1
2690 IF smd=0 THEN dbgxend=cols
2700 FOR j=dbgy TO dbgyend
2710 LOCATE dbgx,j:IF smd<>0 THEN PRINT SPC(dbgxend) ELSE PRINT STRING$(dbgxend," ");
2720 NEXT
2730 GOSUB 2470:' redraw grd border
2740 FOR i=dbgx TO dbgxend
2750 FOR j=dbgy TO dbgyend
2760 IF i>gw OR j>gh THEN 2790
2770 IF grd(i,j)=id1 THEN PEN cl1:c$=b1$ ELSE IF grd(i,j)=id2 THEN PEN cl2:c$=b2$ ELSE c$=eb$
2780 LOCATE ofx+i,ofy+j:PRINT c$
2790 NEXT:NEXT
2800 RETURN
2810 '
2820 ' error handling
2830 ms$="Error with code"+STR$(ERR)+" at line"+STR$(ERL)
2840 GOSUB 2330:GOSUB 2380:' print centered msg and wait
2850 MODE 2:INK 0,1:INK 1,26:PEN 1:PAPER 0:BORDER 1:LOCATE 1,1:PRINT ms$
2860 IF ERR=5 THEN PRINT"Improper Argument"
2870 END
2880 INK 0,1:INK 1,26:PAPER 0:PEN 1:BORDER 1:MODE 2
























