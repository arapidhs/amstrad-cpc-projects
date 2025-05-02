10 ' debug messages and debug grid scanning
20 dbgscan=1
30 RANDOMIZE TIME
40 ON ERROR GOTO 2740
50 ON BREAK GOSUB 2790
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
320 id1=1:id2=2
330 ial=7:ist=0:ism=1:ivg=2:imn=3:imx=4:isl=5:ilt=6:icn=7
340 DIM st(2,ial,1):DIM st$(ial):RESTORE 2810:FOR i=0 TO ial:READ st$(i):NEXT
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
580 IF smd=0 THEN RESTORE 2830 ELSE RESTORE 2820
590 FOR i=0 TO psz:READ pn$(i):NEXT
600 '
610 ' Assign personalities based on personality probabilities: pnprb()
620 r=RND:IF r<pnprb(pnrm) THEN pn1=pnrm ELSE IF r<pnprb(patt) THEN pn1=patt ELSE IF r<pnprb(prnd) THEN pn1=prnd ELSE pn1=pdef
630 r=RND:IF r<pnprb(pnrm) THEN pn2=pnrm ELSE IF r<pnprb(patt) THEN pn2=patt ELSE IF r<pnprb(prnd) THEN pn2=prnd ELSE pn2=pdef
640 '
650 ' Wait for key press
660 CLS:IF smd=0 THEN ms$="Press key to start" ELSE ms$="Press any key to start":GOSUB 2260:CLS
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
765 LOCATE sx,sy:PAPER ctx:PEN cbg:PRINT STRING$(cols," "):LOCATE sx,sy:PRINT"Initializing...":PAPER cbg:PEN ctx
770 GOSUB 2420:DIM grd(gw,gh):grd(0,0)=-1:' setup grid dimensions
780 GOSUB 2340:' draw grid border
790 GOSUB 2490:' define and draw starting positions and stats
795 'lists of valid moves for player 1 and 2
796 'element 0 of bls1,2 store the counter of valid moves e.g. bls1(0,0) counter of potential valid moves
800 blmax=(gw+gh)*2:DIM bls1(blmax,1):DIM bls2(blmax,1)
804 DIM vm(8,1):' array to store all potential 8 valid moves around a given block, first element at pos 0 stores the count of valid moves e.g.e vm(0,0)=5
805 LOCATE sx,sy:PRINT STRING$(cols," ")
810 '
820 c1=st(id1,icn,0):c2=st(id2,icn,0)
830 GOSUB 2210:' print block counts
840 '
850 ' Game LOOP
860 turn=1:trn=0
870 WHILE c1+c2<gw*gh
880 '
890 ' CPU turn
900 trn=trn+1:trs=0
910 IF turn=1 THEN id=id1:opp=id2:cpuclr=cl1:b$=b1$:pn=pn1:clx=c1x:cly=c1y
920 IF turn=2 THEN id=id2:opp=id1:cpuclr=cl2:b$=b2$:pn=pn2:clx=c2x:cly=c2y
930 LOCATE sx,sy:PRINT STRING$(cols," ")
940 c1=st(id1,icn,0):c2=st(id2,icn,0):prg=ROUND((c1+c2)/(gwh)*100,2)
950 PEN ctx:LOCATE sx,sy:PRINT "Turn";trn;STR$(prg);"%":PEN cpuclr:mst$="...":LOCATE clx,cly:PRINT mst$;
960 '
970 'Process cpu action based on personality
980 act=0:ON pn+1 GOSUB 1290,1330,1370,1600
990 c1=st(id1,icn,0):c2=st(id2,icn,0)
1000 LOCATE clx,cly:PRINT SPC(LEN(mst$));
1010 ' act 0 is no move found, 1 is move, 2 fight won, 3 fight loss
1020 IF act=0 THEN GOTO 1160:'no valid move found, end game
1030 ' print move or fg results to screen
1040 IF act=1 THEN hx=tx:hy=ty:GOSUB 2140:SOUND 1,200,20,15:GOSUB 2140:' move highlight, play sound highlight
1050 IF act=2 THEN hx=tx:hy=ty:GOSUB 2140:SOUND 1,142,20,15:SOUND 1,95,20,15:GOSUB 2140:' fight won highlight, play sound highlight
1060 IF act=3 THEN hx=tx:hy=ty:GOSUB 2140:SOUND 1,95,20,15:SOUND 1,125,20,15:GOSUB 2140:' fight lost highlight, play sound highlight
1070 IF act=1 OR act=2 THEN LOCATE ofx+tx,ofy+ty:PEN cpuclr:PRINT b$;
1090 GOSUB 2210:' print block counts
1100 GOSUB 2310:' delay routine
1110 IF c1+c2>=gwh OR c1=0 OR c2=0 THEN GOTO 1200
1120 IF turn=1 THEN turn=2 ELSE turn=1
1130 trs=1
1140 WEND
1150 '
1160 ' error: no valid move found
1170 IF smd<>0 THEN ms$="Error: no valid move found" ELSE ms$="Err: no move found"
1180 GOSUB 2260:GOTO 1250:' print error message and exit
1190 '
1200 ' end game
1210 IF smd<>0 THEN ms$="Game Over: "+id1$+":"+STR$(c1)+" "+id2$+":"+STR$(c2) ELSE ms$="Game Over:"+MID$(STR$(c1),2)+"/"+MID$(STR$(c2),2)
1220 GOSUB 2260
1230 IF c1>c2 THEN ms$="CPU 1 wins!" ELSE IF c1<c2 THEN ms$="CPU 2 wins!" ELSE ms$="Draw!"
1240 GOSUB 2260
1250 CLS:ms$="Play again? (Y:N)":LOCATE hcols-INT(LEN(ms$)/2),hrows:INPUT"Play Again? (Y:N)",a$
1260 IF UPPER$(a$)="Y" THEN RUN ELSE GOTO 2790
1270 END
1280 '
1290 ' CPU Normal
1300 GOSUB 1370
1310 RETURN
1320 '
1330 ' CPU Attacker
1340 GOSUB 1370
1350 RETURN
1360 '
1370 ' CPU Random
1380 bx=0:by=0:tx=0:ty=0
1390 GOSUB 1640:' populate bls1 with all valid moves
1400 tmp=bls1(0,0):IF tmp<1 THEN act=0:RETURN:' no valid move found, we should never reach this state normally
1410 r=INT(RND*tmp)+1:tmpx=bls1(r,0):tmpy=bls1(r,1)
1420 ' We found a random valid block next we need to find a valid random target
1430 IF dbgscan=1 THEN SOUND 1,1000,2,15:hx=bx:hy=by:GOSUB 2140:' highlight selected valid block
1440 tmpopp=opp:GOSUB 1805:'populate valid moves
1530 tmp=vm(0,0):IF tmp<1 THEN act=0:RETURN:' no valid target found, we should never normally reach this state
1540 r=INT(RND*tmp)+1:tx=vm(r,0):ty=vm(r,1)
1550 bx=tmpx:by=tmpy:st(id,ilt,0)=tx:st(id,ilt,1)=ty:st(id,isl,0)=bx:st(id,isl,1)=by
1560 IF grd(tx,ty)=0 THEN act=1 ELSE GOSUB 1950'if target is opp, resolve fight
1570 IF act=1 OR act=2 THEN grd(tx,ty)=id:GOSUB 1840:' update stats on move or won fight
1580 RETURN
1590 '
1600 ' CPU Defender
1610 GOSUB 1370
1620 RETURN
1630 '
1640 ' Populate bls1 with all valid moves
1650 tmp=0
1660 minx=st(id,imn,0):miny=st(id,imn,1):maxx=st(id,imx,0):maxy=st(id,imx,1)
1670 FOR x=minx TO maxx:FOR y=miny TO maxy
1680 IF dbgscan=1 THEN SOUND 1,1000,2,15:hx=x:hy=y:GOSUB 2140:' highlight grd scanning
1700 IF grd(x,y)<>id THEN GOTO 1800
1710 FOR dx=-1 TO 1:FOR dy=-1 TO 1
1720 IF dx=0 AND dy=0 THEN GOTO 1780
1730 nx=x+dx:ny=y+dy
1740 IF nx<1 OR nx>gw OR ny<1 OR ny>gh THEN GOTO 1780
1750 IF grd(nx,ny)=0 OR grd(nx,ny)=opp THEN tmp=tmp+1 ELSE 1780
1770 bls1(0,0)=tmp:bls1(tmp,0)=x:bls1(tmp,1)=y:GOTO 1800:' valid block found move to next
1780 NEXT:NEXT
1800 NEXT:NEXT
1802 RETURN
1804 '
1805 ' populate valid moves for tmpopp and block at tmpx,tmpy
1806 tmp=0
1807 FOR dx=-1 TO 1:FOR dy=-1 TO 1
1808 IF dx=0 AND dy=0 THEN GOTO 1812
1809 nx=tmpx+dx:ny=tmpy+dy:hx=nx:hy=ny:GOSUB 2140
1810 IF nx<1 OR nx>gw OR ny<1 OR ny>gh THEN 1812
1811 IF grd(nx,ny)=0 OR grd(nx,ny)=tmpopp THEN tmp=tmp+1:vm(tmp,0)=nx:vm(tmp,1)=ny ELSE 1812
1812 NEXT:NEXT
1813 vm(0,0)=tmp
1820 RETURN
1830 '
1840 ' update stats after move, or won fight
1850 tmp=st(id,icn,0)+1:st(id,icn,0)=tmp:st(id,ism,0)=st(id,ism,0)+tx:st(id,ism,1)=st(id,ism,1)+ty:st(id,ivg,0)=INT(st(id,ism,0)/tmp):st(id,ivg,1)=INT(st(id,ism,1)/tmp)
1860 st(id,imn,0)=MIN(st(id,imn,0),tx):st(id,imn,1)=MIN(st(id,imn,1),ty):st(id,imx,0)=MAX(st(id,imx,0),tx):st(id,imx,1)=MAX(st(id,imx,1),ty)
1870 IF act<>2 THEN RETURN
1880 'there was a fight and opp lost a block
1890 IF st(opp,icn,0)-1<1 THEN st(opp,icn,0)=0:RETURN
1900 tmp=st(opp,icn,0)-1:st(opp,ism,0)=st(opp,ism,0)-tx:st(opp,ism,1)=st(opp,ism,1)-ty:st(opp,ivg,0)=INT(st(opp,ism,0)/tmp):st(opp,ivg,1)=INT(st(opp,ism,1)/tmp):st(opp,icn,0)=tmp
1910 GOSUB 1990:' recalculate min max x y if needed due to opp's lost block
1930 RETURN
1940 '
1950 ' resolve fg
1960 r=RND:IF r>0.5 THEN act=2 ELSE act=3
1970 RETURN
1980 '
1990 ' recalculate minx maxx miny maxy
1991 IF st(opp,imn,0)=tx OR st(opp,imn,1)=ty OR st(opp,imx,0)=tx OR st(opp,imx,1)=ty THEN 2000 ELSE RETURN
2000 minx=st(opp,imn,0):maxx=st(opp,imx,0):miny=st(opp,imn,1):maxy=st(opp,imx,1)
2030 FOR i=minx TO maxx:FOR j=miny TO maxy:IF grd(i,j)=opp THEN st(opp,imn,0)=i:GOTO 2040 ELSE NEXT:NEXT
2040 FOR i=maxx TO minx STEP -1:FOR j=miny TO maxy:IF grd(i,j)=opp THEN st(opp,imx,0)=i:GOTO 2050 ELSE NEXT:NEXT
2050 FOR j=miny TO maxy:FOR i=minx TO maxx:IF grd(i,j)=opp THEN st(opp,imn,1)=j:GOTO 2060 ELSE NEXT:NEXT
2060 FOR j=maxy TO miny STEP -1:FOR i=minx TO maxx:IF grd(i,j)=opp THEN st(opp,imx,1)=j:GOTO 2070 ELSE NEXT:NEXT
2070 RETURN
2130 '
2140 ' highlight
2150 IF hx<1 OR hy<1 OR hx>gw OR hy>gh THEN RETURN
2160 LOCATE ofx+hx,ofy+hy:PEN cpuclr:PRINT hb$
2170 IF grd(hx,hy)=id1 THEN PEN cl1:a$=b1$ ELSE IF grd(hx,hy)=id2 THEN PEN cl2:a$=b2$ ELSE a$=eb$
2180 LOCATE ofx+hx,ofy+hy:PRINT a$
2190 RETURN
2200 '
2210 ' print block counts
2220 PEN cl1:LOCATE cols-3,c1y:PRINT USING "####";c1;
2230 PEN cl2:LOCATE cols-3,c2y:PRINT USING "####";c2;
2240 RETURN
2250 '
2260 ' print centered message
2270 tmp=INT(LEN(ms$)/2):tmpx=MAX(1,hcols-tmp):LOCATE tmpx,hrows:PEN ctx:PRINT ms$:tmp=LEN(ms$)
2280 CLEAR INPUT:CALL &BB18:LOCATE tmpx,hrows:PRINT SPC(tmp)
2290 RETURN
2300 '
2310 ' delay routine
2320 IF dly>0 THEN FOR i=0 TO dly:NEXT:RETURN' delay routine
2330 '
2340 ' draw grid border
2350 ' draw top and bottom horizontal
2360 PEN ctx:LOCATE ofx,ofy:PRINT CHR$(150)+STRING$(gw,CHR$(154))+CHR$(156)
2370 LOCATE ofx,ofy+gh+1:PRINT CHR$(147)+STRING$(gw,CHR$(154))+CHR$(153)
2380 ' draw verticals
2390 a$=CHR$(149):FOR i=ofy+1 TO gh+ofy:LOCATE ofx,i:PRINT a$;:LOCATE ofx+gw+1,i:PRINT a$:NEXT
2400 RETURN
2410 '
2420 ' Define grid size
2430 gw=MAX(INT(cols/3),INT(RND*cols)+2):gw=MIN(gw,cols-2):hgw=INT(gw/2):IF gw MOD 2=0 THEN gw=gw-1' Ensure gw is odd, -2 cols for vertical grid lines
2440 gh=MAX(INT(rows/3),INT(RND*rows)+2):gh=MIN(gh,rows-5):hgh=INT(gh/2):IF gh MOD 2=0 THEN gh=gh-1' Ensure gh is odd, -5 rows for 2 cpu rows, 1 status line and 2 grid rows
2450 gwh=gw*gh
2460 ofx=INT((cols-gw)/2):ofy=INT((rows-gh)/2)+1:' locate offsets: ofy+1 row to leave a blank line for the status line
2470 RETURN
2480 '
2490 ' Define starting positions
2500 st1x=INT(gw/2)+1:st1y=1:st2x=INT(gw/2)+1:st2y=gh
2510 FOR i=0 TO ial:st(id1,i,0)=st1x:st(id1,i,1)=st1y:st(id2,i,0)=st2x:st(id2,i,1)=st2y:NEXT
2520 c1=1:st(id1,icn,0)=c1:c2=1:st(id2,icn,0)=c2
2530 grd(st1x,st1y)=id1:grd(st2x,st2y)=id2:
2540 LOCATE  ofx+st1x,ofy+st1y:PEN cl1:PRINT b1$:LOCATE  ofx+st2x,ofy+st2y:PEN cl2:PRINT b2$
2550 p=0.1:sb=MAX(1,INT((gwh/2)*p)):' starting blocks formula
2560 ' randomly select starting blocks for reach player
2570 WHILE c1<sb OR c2<sb
2580 ' Player 1 and 2 block selection
2590 FOR i=id1 TO id2
2600 tmp=st(i,icn,0)
2610 IF tmp>=sb THEN 2690
2625 'id1 has blocks from y=1 to y=gh/2-1, e.g. if gh=11 id1 y ranges from 1-5. Similarly for id2 is from 7-11.
2620 IF i=id1 THEN tmpx=INT(RND*gw)+1:tmpy=INT(RND*INT(gh/2))+1 ELSE tmpx=INT(RND*gw)+1:tmpy=INT(RND*(gh-INT(gh/2)-1))+INT(gh/2)+2
2630 IF grd(tmpx,tmpy)<>0 THEN 2690
2640 grd(tmpx,tmpy)=i:tmp=tmp+1:st(i,icn,0)=tmp:st(i,ism,0)=st(i,ism,0)+tmpx:st(i,ism,1)=st(i,ism,1)+tmpy
2650 st(i,ivg,0)=INT(st(i,ism,0)/tmp):st(i,ivg,1)=INT(st(i,ism,1)/tmp):st(i,imn,0)=MIN(st(i,imn,0),tmpx):st(i,imn,1)=MIN(st(i,imn,1),tmpy)
2660 st(i,imx,0)=MAX(st(i,imx,0),tmpx):st(i,imx,1)=MAX(st(i,imx,1),tmpy)
2670 IF i=id1 THEN cpuclr=cl1:a$=b1$ ELSE cpuclr=cl2:a$=b2$
2680 SOUND 1,1500,2,10:LOCATE ofx+tmpx,ofy+tmpy:PEN cpuclr:PRINT a$
2690 NEXT
2700 c1=st(id1,icn,0):c2=st(id2,icn,0)
2710 WEND
2720 RETURN
2730 '
2740 ' error handling
2750 ms$="Error with code"+STR$(ERR)+" at line"+STR$(ERL):GOSUB 2260
2760 MODE 2:INK 0,1:INK 1,26:PEN 1:PAPER 0:BORDER 1:LOCATE 1,1:PRINT ms$
2770 IF ERR=5 THEN PRINT"Improper Argument"
2780 END
2790 INK 0,1:INK 1,26:PAPER 0:PEN 1:BORDER 1:MODE 2:END
2800 ' DATA
2810 DATA "start","sum","avg","min","max","sel","last","count":' sel is last selected position, last is last occupied position
2820 DATA "Normal","Attacker","Random","Defender":' Personality names
2830 DATA "Nrm","Att","Rnd","Def":' Shorthands for Personality names
