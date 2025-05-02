10 RANDOMIZE TIME
20 ON ERROR GOTO 2760
30 ON BREAK GOSUB 2810
40 MODE 1:INK 0,0:INK 1,26:PAPER 0:PEN 1:BORDER 0
50 '
60 PRINT "Select mode (0-Mode0 1-Mode1 2-Mode2)";
70 a$="":WHILE a$="":a$=INKEY$:WEND
80 IF a$<>"0" AND a$<>"1" AND a$<>"2" THEN a$="1"
90 smd=VAL(a$)
100 cols=20*(2^smd):rows=25:hcols=INT(cols/2):hrows=INT(rows/2)
110 PRINT"Select move delay (1=0.5s 2=1s else=0s)";
120 a$="":WHILE a$="":a$=INKEY$:WEND
130 IF a$="1" THEN dly=500 ELSE IF a$="2" THEN dly=1000 ELSE dly=0
140 '
150 ' Screen initialization and colors
160 ' cbg bg color, cl1,cl2 cpu colors, ctx text color
170 IF smd=2 THEN INK 0,0:INK 1,26:INK 2,26:INK 3,26:cbg=0:cl1=1:cl2=1:ctx=1 ELSE INK 0,0:INK 1,2:INK 2,6:INK 3,26:cbg=0:cl1=1:cl2=2:ctx=3
180 PAPER cbg:BORDER cbg:PEN ctx:MODE smd
190 ms$="Loading...":tmp=INT(LEN(ms$)/2):tmpx=MAX(1,hcols-tmp):LOCATE tmpx,hrows:PEN ctx:PRINT ms$
200 '
210 ' Setup blocks, cpu1 cpu2 empty block, highlight block
220 IF smd=2 THEN b1$=CHR$(143):b2$=CHR$(206) ELSE b1$=CHR$(207):b2$=CHR$(207)
230 SYMBOL 240,0,0,60,60,60,60,0,0
240 eb$=CHR$(32):hb$=CHR$(240)
250 '
260 '
270 ' Initialize cpu/player stats
280 ' position, sum xy, avg xy, min and max xy
290 ' selected pos., last occupied pos.
300 id1=1:id2=2
310 ial=7:ist=0:ism=1:ivg=2:imn=3:imx=4:isl=5:ilt=6:icn=7
320 DIM st(2,ial,1):DIM st$(ial):RESTORE 2830:FOR i=0 TO ial:READ st$(i):NEXT
330 '
340 ' Battle probabilities array - btl():
350 ' 0-2 friendly block min,max,avg
360 ' 3-5 opposing min,max,avg
370 ' 6-8 empty min,max,avg
380 bsz=8:frn=0:frx=1:fra=2:opn=3:opx=4:opa=5:emn=6:emx=7:ema=8
390 DIM btl(bsz)
400 btl(frn)=0.05:btl(frx)=0.1:btl(fra)=(btl(frn)+btl(frx))/2
410 btl(opn)=-0.1:btl(opx)=-0.05:btl(opa)=(btl(opn)+btl(opx))/2
420 btl(emn)=0.01:btl(emx)=-0.03:btl(ema)=(btl(emn)+btl(emx))/2
430 '
440 ' Attacker and defense thresholds
450 attthres=0.3:defthres=0.3
460 '
470 ' Cumulative personality probabilities: pnprb()
480 ' 1 norm: 0.25 prob, 2 att: 0.25, 3 rnd: 0.25, 4 def: 0.25
490 psz=3:pnrm=0:patt=1:prnd=2:pdef=3
500 DIM pnprb(psz):pnprb(pnrm)=0.25:pnprb(patt)=pnprb(pnrm)+0.25:pnprb(prnd)=pnprb(patt)+0.25:pnprb(pdef)=pnprb(prnd)+0.25
510 '
520 ' Personality names pn$()
530 ' Normal, 1 Attacker, 2 Random, 3 Defender
540 psz=3
550 DIM pn$(psz)
560 IF smd=0 THEN RESTORE 2850 ELSE RESTORE 2840
570 FOR i=0 TO psz:READ pn$(i):NEXT
580 '
590 ' Assign personalities based on personality probabilities: pnprb()
600 r=RND:IF r<pnprb(pnrm) THEN pn1=pnrm ELSE IF r<pnprb(patt) THEN pn1=patt ELSE IF r<pnprb(prnd) THEN pn1=prnd ELSE pn1=pdef
610 r=RND:IF r<pnprb(pnrm) THEN pn2=pnrm ELSE IF r<pnprb(patt) THEN pn2=patt ELSE IF r<pnprb(prnd) THEN pn2=prnd ELSE pn2=pdef
620 '
630 ' Wait for key press
640 CLS:IF smd=0 THEN ms$="Press key to start" ELSE ms$="Press any key to start":GOSUB 2270:CLS
650 '
660 ' print personalities
670 id1$="CPU 1":id2$="CPU 2"
680 c1$=id1$+": "+pn$(pn1):c2$=id2$+": "+pn$(pn2)
690 c1x=MAX(LEN(c1$),LEN(c2$))+2:c1y=1:c2x=c1x:c2y=rows' aligned x location to print status on cpu rows
700 sx=1:sy=2:' location of status line
710 LOCATE 1,1:PEN cl1:PRINT c1$
720 LOCATE 1,rows:PEN cl2:PRINT c2$
730 '
740 ' Initialize grid and starting positions
750 LOCATE sx,sy:PAPER ctx:PEN cbg:PRINT STRING$(cols," "):LOCATE sx,sy:PRINT"Initializing...":PAPER cbg:PEN ctx
760 GOSUB 2430:DIM grd(gw,gh):grd(0,0)=-1:' setup grid dimensions
770 GOSUB 2350:' draw grid border
780 GOSUB 2500:' define and draw starting positions and stats
790 '
800 'lists of valid moves for player 1 and 2
810 'element 0 of bls store the counter of valid moves e.g. bls(0,0,0) counter of potential valid moves for player 1 while bls(1,0,0) for player 2
820 blmax=(gw+gh)*2:DIM bls(1,blmax,1)' REM bls(0) stores list for player id1, bls(2) for player id2
830 '
840 DIM vm(8,1):' array to store all potential 8 valid moves around a given block, first element at pos 0 stores the count of valid moves e.g.e vm(0,0)=5
850 '
860 ' Initialize valid block lists for players
870 tmpid=id1:tmpopp=id2:GOSUB 1650
880 tmpid=id2:tmpopp=id1:GOSUB 1650
890 '
900 ' clear initialization message
910 LOCATE sx,sy:PRINT STRING$(cols," ")
920 '
930 c1=st(id1,icn,0):c2=st(id2,icn,0)
940 GOSUB 2220:' print block counts
950 '
960 ' Game LOOP
970 turn=1:trn=0
980 WHILE c1+c2<gw*gh
990 '
1000 ' CPU turn
1010 trn=trn+1:trs=0
1020 IF turn=1 THEN id=id1:opp=id2:cpuclr=cl1:b$=b1$:pn=pn1:clx=c1x:cly=c1y
1030 IF turn=2 THEN id=id2:opp=id1:cpuclr=cl2:b$=b2$:pn=pn2:clx=c2x:cly=c2y
1040 LOCATE sx,sy:PRINT STRING$(cols," ")
1050 c1=st(id1,icn,0):c2=st(id2,icn,0):prg=ROUND((c1+c2)/(gwh)*100,2)
1060 PEN ctx:LOCATE sx,sy:PRINT "Turn";trn;STR$(prg);"%":PEN cpuclr:mst$="...":LOCATE clx,cly:PRINT mst$;
1070 '
1080 'Process cpu action based on personality
1090 act=0:ON pn+1 GOSUB 1400,1440,1480,1610
1100 c1=st(id1,icn,0):c2=st(id2,icn,0)
1110 LOCATE clx,cly:PRINT SPC(LEN(mst$));
1120 ' act 0 is no move found, 1 is move, 2 fight won, 3 fight loss
1130 IF act=0 THEN GOTO 1270:'no valid move found, end game
1140 ' print move or fg results to screen
1150 tmpid=id:'used by highlight routine to decide color
1160 IF act=1 THEN hx=tx:hy=ty:GOSUB 2150:SOUND 1,200,20,15:GOSUB 2150:' move highlight, play sound highlight
1170 IF act=2 THEN hx=tx:hy=ty:GOSUB 2150:SOUND 1,142,20,15:SOUND 1,95,20,15:GOSUB 2150:' fight won highlight, play sound highlight
1180 IF act=3 THEN hx=tx:hy=ty:GOSUB 2150:SOUND 1,95,20,15:SOUND 1,125,20,15:GOSUB 2150:' fight lost highlight, play sound highlight
1190 IF act=1 OR act=2 THEN LOCATE ofx+tx,ofy+ty:PEN cpuclr:PRINT b$;
1200 GOSUB 2220:' print block counts
1210 GOSUB 2320:' delay routine
1220 IF c1+c2>=gwh OR c1=0 OR c2=0 THEN GOTO 1310
1230 IF turn=1 THEN turn=2 ELSE turn=1
1240 trs=1
1250 WEND
1260 '
1270 ' error: no valid move found
1280 IF smd<>0 THEN ms$="Error: no valid move found" ELSE ms$="Err: no move found"
1290 GOSUB 2270:GOTO 1360:' print error message and exit
1300 '
1310 ' end game
1320 IF smd<>0 THEN ms$="Game Over: "+id1$+":"+STR$(c1)+" "+id2$+":"+STR$(c2) ELSE ms$="Game Over:"+MID$(STR$(c1),2)+"/"+MID$(STR$(c2),2)
1330 GOSUB 2270
1340 IF c1>c2 THEN ms$="CPU 1 wins!" ELSE IF c1<c2 THEN ms$="CPU 2 wins!" ELSE ms$="Draw!"
1350 GOSUB 2270
1360 CLS:ms$="Play again? (Y:N)":LOCATE hcols-INT(LEN(ms$)/2),hrows:INPUT"Play Again? (Y:N)",a$
1370 IF UPPER$(a$)="Y" THEN RUN ELSE GOTO 2810
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
1490 ids=id-1:bx=0:by=0:tx=0:ty=0
1500 tmp=bls(ids,0,0):IF tmp<1 THEN act=0:RETURN:' no valid move found, we should never reach this state normally
1510 r=INT(RND*tmp)+1:tmpx=bls(ids,r,0):tmpy=bls(ids,r,1)
1520 ' We found a random valid block next we need to find a valid random target
1530 tmpopp=opp:GOSUB 1810:'populate valid moves
1540 tmp=vm(0,0):IF tmp<1 THEN act=0:RETURN:' no valid target found, we should never normally reach this state
1550 r=INT(RND*tmp)+1:tx=vm(r,0):ty=vm(r,1)
1560 bx=tmpx:by=tmpy:st(id,ilt,0)=tx:st(id,ilt,1)=ty:st(id,isl,0)=bx:st(id,isl,1)=by
1570 IF grd(tx,ty)=0 THEN act=1 ELSE GOSUB 2020:'if target is opp, resolve fight
1580 IF act=1 OR act=2 THEN grd(tx,ty)=id:GOSUB 1920:' update stats on move or won fight
1590 RETURN
1600 '
1610 ' CPU Defender
1620 GOSUB 1480
1630 RETURN
1640 '
1650 ' Populate bls list with all valid moves for id
1660 tmp=0:ids=tmpid-1
1670 minx=st(tmpid,imn,0):miny=st(tmpid,imn,1):maxx=st(tmpid,imx,0):maxy=st(tmpid,imx,1)
1680 FOR x=minx TO maxx:FOR y=miny TO maxy
1690 SOUND 1,1000,2,15:hx=x:hy=y:GOSUB 2150:' highlight grid scanning
1700 IF grd(x,y)<>tmpid THEN GOTO 1780
1710 FOR dx=-1 TO 1:FOR dy=-1 TO 1
1720 IF dx=0 AND dy=0 THEN GOTO 1770
1730 nx=x+dx:ny=y+dy
1740 IF nx<1 OR nx>gw OR ny<1 OR ny>gh THEN GOTO 1770
1750 IF grd(nx,ny)=0 OR grd(nx,ny)=tmpopp THEN tmp=tmp+1 ELSE 1770
1760 bls(ids,0,0)=tmp:bls(ids,tmp,0)=x:bls(ids,tmp,1)=y:GOTO 1780:' valid block found move to next
1770 NEXT:NEXT
1780 NEXT:NEXT
1790 RETURN
1800 '
1810 ' populate valid moves for tmpopp and block at tmpx,tmpy
1820 tmp=0
1830 FOR dx=-1 TO 1:FOR dy=-1 TO 1
1840 IF dx=0 AND dy=0 THEN GOTO 1880
1850 nx=tmpx+dx:ny=tmpy+dy
1860 IF nx<1 OR nx>gw OR ny<1 OR ny>gh THEN 1880
1870 IF grd(nx,ny)=0 OR grd(nx,ny)=tmpopp THEN tmp=tmp+1:vm(tmp,0)=nx:vm(tmp,1)=ny ELSE 1880
1880 NEXT:NEXT
1890 vm(0,0)=tmp
1900 RETURN
1910 '
1920 ' update stats after move, or won fight
1930 tmp=st(id,icn,0)+1:st(id,icn,0)=tmp:st(id,ism,0)=st(id,ism,0)+tx:st(id,ism,1)=st(id,ism,1)+ty:st(id,ivg,0)=INT(st(id,ism,0)/tmp):st(id,ivg,1)=INT(st(id,ism,1)/tmp)
1940 st(id,imn,0)=MIN(st(id,imn,0),tx):st(id,imn,1)=MIN(st(id,imn,1),ty):st(id,imx,0)=MAX(st(id,imx,0),tx):st(id,imx,1)=MAX(st(id,imx,1),ty)
1950 IF act<>2 THEN RETURN
1960 'there was a fight and opp lost a block
1970 IF st(opp,icn,0)-1<1 THEN st(opp,icn,0)=0:RETURN
1980 tmp=st(opp,icn,0)-1:st(opp,ism,0)=st(opp,ism,0)-tx:st(opp,ism,1)=st(opp,ism,1)-ty:st(opp,ivg,0)=INT(st(opp,ism,0)/tmp):st(opp,ivg,1)=INT(st(opp,ism,1)/tmp):st(opp,icn,0)=tmp
1990 GOSUB 2060:' recalculate min max x y if needed due to opp's lost block
2000 RETURN
2010 '
2020 ' resolve fg
2030 r=RND:IF r>0.5 THEN act=2 ELSE act=3
2040 RETURN
2050 '
2060 ' recalculate minx maxx miny maxy
2070 IF st(opp,imn,0)=tx OR st(opp,imn,1)=ty OR st(opp,imx,0)=tx OR st(opp,imx,1)=ty THEN 2080 ELSE RETURN
2080 minx=st(opp,imn,0):maxx=st(opp,imx,0):miny=st(opp,imn,1):maxy=st(opp,imx,1)
2090 FOR i=minx TO maxx:FOR j=miny TO maxy:IF grd(i,j)=opp THEN st(opp,imn,0)=i:GOTO 2100 ELSE NEXT:NEXT
2100 FOR i=maxx TO minx STEP -1:FOR j=miny TO maxy:IF grd(i,j)=opp THEN st(opp,imx,0)=i:GOTO 2110 ELSE NEXT:NEXT
2110 FOR j=miny TO maxy:FOR i=minx TO maxx:IF grd(i,j)=opp THEN st(opp,imn,1)=j:GOTO 2120 ELSE NEXT:NEXT
2120 FOR j=maxy TO miny STEP -1:FOR i=minx TO maxx:IF grd(i,j)=opp THEN st(opp,imx,1)=j:GOTO 2130 ELSE NEXT:NEXT
2130 RETURN
2140 '
2150 ' highlight
2160 IF hx<1 OR hy<1 OR hx>gw OR hy>gh THEN RETURN
2170 LOCATE ofx+hx,ofy+hy:IF tmpid=id1 THEN PEN cl1:PRINT hb$ ELSE PEN cl2:PRINT hb$
2180 IF grd(hx,hy)=id1 THEN PEN cl1:a$=b1$ ELSE IF grd(hx,hy)=id2 THEN PEN cl2:a$=b2$ ELSE a$=eb$
2190 LOCATE ofx+hx,ofy+hy:PRINT a$
2200 RETURN
2210 '
2220 ' print block counts
2230 PEN cl1:LOCATE cols-3,c1y:PRINT USING "####";c1;
2240 PEN cl2:LOCATE cols-3,c2y:PRINT USING "####";c2;
2250 RETURN
2260 '
2270 ' print centered message
2280 tmp=INT(LEN(ms$)/2):tmpx=MAX(1,hcols-tmp):LOCATE tmpx,hrows:PEN ctx:PRINT ms$:tmp=LEN(ms$)
2290 CLEAR INPUT:CALL &BB18:LOCATE tmpx,hrows:PRINT SPC(tmp)
2300 RETURN
2310 '
2320 ' delay routine
2330 IF dly>0 THEN FOR i=0 TO dly:NEXT:RETURN' delay routine
2340 '
2350 ' draw grid border
2360 ' draw top and bottom horizontal
2370 PEN ctx:LOCATE ofx,ofy:PRINT CHR$(150)+STRING$(gw,CHR$(154))+CHR$(156)
2380 LOCATE ofx,ofy+gh+1:PRINT CHR$(147)+STRING$(gw,CHR$(154))+CHR$(153)
2390 ' draw verticals
2400 a$=CHR$(149):FOR i=ofy+1 TO gh+ofy:LOCATE ofx,i:PRINT a$;:LOCATE ofx+gw+1,i:PRINT a$:NEXT
2410 RETURN
2420 '
2430 ' Define grid size
2440 gw=MAX(INT(cols/3),INT(RND*cols)+2):gw=MIN(gw,cols-2):hgw=INT(gw/2):IF gw MOD 2=0 THEN gw=gw-1' Ensure gw is odd, -2 cols for vertical grid lines
2450 gh=MAX(INT(rows/3),INT(RND*rows)+2):gh=MIN(gh,rows-5):hgh=INT(gh/2):IF gh MOD 2=0 THEN gh=gh-1' Ensure gh is odd, -5 rows for 2 cpu rows, 1 status line and 2 grid rows
2460 gwh=gw*gh
2470 ofx=INT((cols-gw)/2):ofy=INT((rows-gh)/2)+1:' locate offsets: ofy+1 row to leave a blank line for the status line
2480 RETURN
2490 '
2500 ' Define starting positions
2510 st1x=INT(gw/2)+1:st1y=1:st2x=INT(gw/2)+1:st2y=gh
2520 FOR i=0 TO ial:st(id1,i,0)=st1x:st(id1,i,1)=st1y:st(id2,i,0)=st2x:st(id2,i,1)=st2y:NEXT
2530 c1=1:st(id1,icn,0)=c1:c2=1:st(id2,icn,0)=c2
2540 grd(st1x,st1y)=id1:grd(st2x,st2y)=id2:
2550 LOCATE  ofx+st1x,ofy+st1y:PEN cl1:PRINT b1$:LOCATE  ofx+st2x,ofy+st2y:PEN cl2:PRINT b2$
2560 p=0.1:sb=MAX(1,INT((gwh/2)*p)):' starting blocks formula
2570 ' randomly select starting blocks for reach player
2580 WHILE c1<sb OR c2<sb
2590 ' Player 1 and 2 block selection
2600 FOR i=id1 TO id2
2610 tmp=st(i,icn,0)
2620 IF tmp>=sb THEN 2710
2640 'id1 has blocks from y=1 to y=gh/2-1, e.g. if gh=11 id1 y ranges from 1-5. Similarly for id2 is from 7-11.
2630 IF i=id1 THEN tmpx=INT(RND*gw)+1:tmpy=INT(RND*INT(gh/2))+1 ELSE tmpx=INT(RND*gw)+1:tmpy=INT(RND*(gh-INT(gh/2)-1))+INT(gh/2)+2
2650 IF grd(tmpx,tmpy)<>0 THEN 2710
2660 grd(tmpx,tmpy)=i:tmp=tmp+1:st(i,icn,0)=tmp:st(i,ism,0)=st(i,ism,0)+tmpx:st(i,ism,1)=st(i,ism,1)+tmpy
2670 st(i,ivg,0)=INT(st(i,ism,0)/tmp):st(i,ivg,1)=INT(st(i,ism,1)/tmp):st(i,imn,0)=MIN(st(i,imn,0),tmpx):st(i,imn,1)=MIN(st(i,imn,1),tmpy)
2680 st(i,imx,0)=MAX(st(i,imx,0),tmpx):st(i,imx,1)=MAX(st(i,imx,1),tmpy)
2690 IF i=id1 THEN cpuclr=cl1:a$=b1$ ELSE cpuclr=cl2:a$=b2$
2700 SOUND 1,1500,2,10:LOCATE ofx+tmpx,ofy+tmpy:PEN cpuclr:PRINT a$
2710 NEXT
2720 c1=st(id1,icn,0):c2=st(id2,icn,0)
2730 WEND
2740 RETURN
2750 '
2760 ' error handling
2770 ms$="Error with code"+STR$(ERR)+" at line"+STR$(ERL):GOSUB 2270
2780 MODE 2:INK 0,1:INK 1,26:PEN 1:PAPER 0:BORDER 1:LOCATE 1,1:PRINT ms$
2790 IF ERR=5 THEN PRINT"Improper Argument"
2800 END
2810 INK 0,1:INK 1,26:PAPER 0:PEN 1:BORDER 1:MODE 2:END
2820 ' DATA
2830 DATA "start","sum","avg","min","max","sel","last","count":' sel is last selected position, last is last occupied position
2840 DATA "Normal","Attacker","Random","Defender":' Personality names
2850 DATA "Nrm","Att","Rnd","Def":' Shorthands for Personality names
