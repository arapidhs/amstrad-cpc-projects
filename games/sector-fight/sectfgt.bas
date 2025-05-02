5 ' Sector Fight
10 RANDOMIZE TIME
20 ON ERROR GOTO 3010
30 ON BREAK GOSUB 3060
40 MODE 1:INK 0,0:INK 1,26:PAPER 0:PEN 1:BORDER 0
50 '
60 CLEAR INPUT:LOCATE 1,1:PRINT "Select mode (0-Mode0 1-Mode1)";
70 a$="":WHILE a$="":a$=INKEY$:WEND
80 IF a$<>"0" AND a$<>"1" THEN a$="1"
90 smd=VAL(a$):cols=20*(2^smd):rows=25:hcols=INT(cols/2):hrows=INT(rows/2)
100 CLEAR INPUT:LOCATE 1,3:PRINT"Auto Pause at End of Turn":PRINT"Default No (Y/N)";
110 a$="":WHILE a$="":a$=INKEY$:WEND
120 IF UPPER$(a$)="Y" THEN ps=1 ELSE ps=0
121 CLEAR INPUT:LOCATE 1,6:PRINT"1 CPU Match (default) 2 vs Human";
122 a$="":WHILE a$="":a$=INKEY$:WEND
123 IF a$<>"1" AND a$<>"2" THEN a$="1"
124 h=VAL(a$):IF h=1 THEN h=0 ELSE h=1:' h 0 means no human
130 '
140 ' Screen initialization and colors
150 ' cbg bg color, cl1,cl2 cpu colors, ctx text color
160 INK 0,0:INK 1,2:INK 2,6:INK 3,26:cbg=0:cl1=1:cl2=2:ctx=3:PAPER cbg:BORDER cbg:PEN ctx:MODE smd
170 ms$="Loading...":tmp=INT(LEN(ms$)/2):tmpx=MAX(1,hcols-tmp):LOCATE tmpx,hrows:PEN ctx:PRINT ms$
180 '
190 ' Setup blocks, cpu1 cpu2 empty block, highlight block
200 b1$=CHR$(207):b2$=CHR$(207):SYMBOL 240,0,0,60,60,60,60,0,0:eb$=CHR$(32):hb$=CHR$(240)
210 '
220 ' Initialize cpu/player stats
230 ' position, sum xy, avg xy, min and max xy
240 ' selected pos., last occupied pos.
250 id1=1:id2=2
260 ial=7:ist=0:ism=1:ivg=2:imn=3:imx=4:isl=5:ilt=6:icn=7
270 DIM st(2,ial,1):DIM st$(ial):RESTORE 3080:FOR i=0 TO ial:READ st$(i):NEXT
280 '
290 ' Battle probabilities array - btl():
300 ' 0-2 friendly block min,max,avg
310 ' 3-5 opposing min,max,avg
320 ' 6-8 empty min,max,avg
330 bsz=8:frn=0:frx=1:fra=2:opn=3:opx=4:opa=5:emn=6:emx=7:ema=8
340 DIM btl(bsz)
350 btl(frn)=0.05:btl(frx)=0.1:btl(fra)=(btl(frn)+btl(frx))/2
360 btl(opn)=-0.1:btl(opx)=-0.05:btl(opa)=(btl(opn)+btl(opx))/2
370 btl(emn)=0.01:btl(emx)=-0.03:btl(ema)=(btl(emn)+btl(emx))/2
380 '
390 ' Attacker and defense thresholds
400 attthres=0.3:defthres=0.3
410 '
420 ' Cumulative personality probabilities: pnprb()
430 ' 1 norm: 0.25 prob, 2 att: 0.25, 3 rnd: 0.25, 4 def: 0.25
440 psz=4:pnrm=0:patt=1:prnd=2:pdef=3:phmn=4
450 DIM pnprb(psz):pnprb(pnrm)=0.25:pnprb(patt)=pnprb(pnrm)+0.25:pnprb(prnd)=pnprb(patt)+0.25:pnprb(pdef)=pnprb(prnd)+0.25
460 '
470 ' Personality names pn$()
480 ' Normal, 1 Attacker, 2 Random, 3 Defender
500 DIM pn$(psz)
510 IF smd=0 THEN RESTORE 3100 ELSE RESTORE 3090
520 FOR i=0 TO psz:READ pn$(i):NEXT
530 '
540 ' Assign personalities based on personality probabilities: pnprb()
550 r=RND:IF r<pnprb(pnrm) THEN pn1=pnrm ELSE IF r<pnprb(patt) THEN pn1=patt ELSE IF r<pnprb(prnd) THEN pn1=prnd ELSE pn1=pdef
560 r=RND:IF r<pnprb(pnrm) THEN pn2=pnrm ELSE IF r<pnprb(patt) THEN pn2=patt ELSE IF r<pnprb(prnd) THEN pn2=prnd ELSE pn2=pdef
565 IF h=1 THEN pn2=phmn
570 '
580 ' Wait for key press
590 CLS:IF smd=0 THEN ms$="Press key to start" ELSE ms$="Press any key to start":GOSUB 2560:CLS
600 '
610 ' print personalities
620 IF h=0 THEN id1$="CPU 1":id2$="CPU 2" ELSE id1$="CPU":id2$="You"
630 c1$=id1$+": "+pn$(pn1):c2$=id2$+": "+pn$(pn2)
640 c1x=MAX(LEN(c1$),LEN(c2$))+2:c1y=1:c2x=c1x:c2y=rows' aligned x location to print status on cpu rows
650 sx=1:sy=2:' location of status line
660 LOCATE 1,1:PEN cl1:PRINT c1$
670 LOCATE 1,rows:PEN cl2:PRINT c2$
680 '
690 ' Initialize grid and starting positions
700 LOCATE sx,sy:PAPER ctx:PEN cbg:PRINT STRING$(cols," "):LOCATE sx,sy:PRINT"Initializing...":PAPER cbg:PEN ctx
710 GOSUB 2680:DIM grd(gw,gh):grd(0,0)=-1:' setup grid dimensions
720 GOSUB 2600:' draw grid border
730 GOSUB 2750:' define and draw starting positions and stats
740 '
750 'lists of valid moves for player 1 and 2
760 'element 0 of bls store the counter of valid moves e.g. bls(0,0,0) counter of potential valid moves for player 1 while bls(1,0,0) for player 2
770 blmax=(gw+gh)*2:DIM bls(1,blmax,1)' REM bls(0) stores list for player id1, bls(2) for player id2
780 '
790 DIM vm(8,1):' array to store all potential 8 valid moves around a given block, first element at pos 0 stores the count of valid moves e.g.e vm(0,0)=5
800 '
810 ' Initialize valid block lists for players
820 tmpid=id1:tmpopp=id2:GOSUB 1790
830 IF h=0 THEN tmpid=id2:tmpopp=id1:GOSUB 1790:'if it is a cpu vs cpu match initialize cpu 2 list has well
840 '
850 ' clear initialization message
860 LOCATE sx,sy:PRINT STRING$(cols," ")
870 '
880 c1=st(id1,icn,0):c2=st(id2,icn,0)
890 GOSUB 2510:' print block counts
900 '
910 ' Game LOOP
920 turn=1:trn=0
930 WHILE c1+c2<gw*gh
940 '
950 ' CPU turn
960 trn=trn+1:trs=0
970 IF turn=1 THEN id=id1:opp=id2:cpuclr=cl1:b$=b1$:pn=pn1:clx=c1x:cly=c1y
980 IF turn=2 THEN id=id2:opp=id1:cpuclr=cl2:b$=b2$:pn=pn2:clx=c2x:cly=c2y
990 LOCATE sx,sy:PRINT STRING$(cols," ")
1000 c1=st(id1,icn,0):c2=st(id2,icn,0):prg=ROUND((c1+c2)/(gwh)*100,2)
1010 PEN ctx:LOCATE sx,sy:PRINT "Turn";:PEN cpuclr:PRINT trn;:PEN ctx:PRINT STR$(prg);"%";
1020 PEN cpuclr:mst$="...":LOCATE clx,cly:PRINT mst$;
1030 '
1040 'Process cpu action based on personality
1050 act=0:ON pn+1 GOSUB 1370,1410,1450,1640,1662
1060 c1=st(id1,icn,0):c2=st(id2,icn,0)
1070 LOCATE clx,cly:PRINT SPC(LEN(mst$));
1080 ' act 0 is no move found, 1 is move, 2 fight won, 3 fight loss
1090 IF act=0 THEN GOTO 1240:'no valid move found, end game
1100 ' print move or fg results to screen
1110 tmpid=id:'used by highlight routine to decide color
1120 IF act=1 THEN hx=tx:hy=ty:GOSUB 2440:SOUND 1,200,20,15:GOSUB 2440:' move highlight, play sound highlight
1130 IF act=2 THEN hx=tx:hy=ty:GOSUB 2440:SOUND 1,142,20,15:SOUND 1,95,20,15:GOSUB 2440:' fight won highlight, play sound highlight
1140 IF act=3 THEN hx=tx:hy=ty:GOSUB 2440:SOUND 1,95,20,15:SOUND 1,125,20,15:GOSUB 2440:' fight lost highlight, play sound highlight
1150 IF act=1 OR act=2 THEN LOCATE ofx+tx,ofy+ty:PEN cpuclr:PRINT b$;
1160 GOSUB 2510:' print block counts
1170 'auto pause
1180 IF ps=1 THEN a$="":FOR i=sx TO cols:LOCATE i,sy:a$=a$+COPYCHR$(#0):NEXT:PAPER cpuclr:PEN cbg:LOCATE sx,sy:PRINT a$:PAPER cbg:PEN cpuclr:CLEAR INPUT:CALL &BB18
1190 IF c1+c2>=gwh OR c1=0 OR c2=0 THEN GOTO 1280
1200 IF turn=1 THEN turn=2 ELSE turn=1
1210 trs=1
1220 WEND
1230 '
1240 ' error: no valid move found
1250 IF smd<>0 THEN ms$="Error: no valid move found" ELSE ms$="Err: no move found"
1260 GOSUB 2560:GOTO 1330:' print error message and exit
1270 '
1280 ' end game
1290 IF smd<>0 THEN ms$="Game Over: "+id1$+":"+STR$(c1)+" "+id2$+":"+STR$(c2) ELSE ms$="Game Over:"+MID$(STR$(c1),2)+"/"+MID$(STR$(c2),2)
1300 GOSUB 2560
1310 IF c1>c2 THEN ms$="CPU 1 wins!" ELSE IF c1<c2 THEN ms$="CPU 2 wins!" ELSE ms$="Draw!"
1320 GOSUB 2560
1330 CLS:ms$="Play again? (Y:N)":LOCATE hcols-INT(LEN(ms$)/2),hrows:INPUT"Play Again? (Y:N)",a$
1340 IF UPPER$(a$)="Y" THEN RUN ELSE GOTO 3060
1350 END
1360 '
1370 ' CPU Normal
1380 GOSUB 1450
1390 RETURN
1400 '
1410 ' CPU Attacker
1420 GOSUB 1450
1430 RETURN
1440 '
1450 ' CPU Random
1460 ids=id-1:bx=0:by=0:tx=0:ty=0:tmpid=id:tmpopp=opp
1470 tmp=bls(ids,0,0):IF tmp<1 THEN act=0:RETURN:' no valid move found, we should never reach this state normally
1480 r=INT(RND*tmp)+1:tmpx=bls(ids,r,0):tmpy=bls(ids,r,1)
1490 'verify that the block is still valid, else refresh the valid block list
1500 tmp=0:GOSUB 2290
1510 IF tmp=0 THEN tmpid=id:GOSUB 1790 ELSE 1550:'if move invalid tmp=0 then refresh list
1520 'retry getting valid block after list has been refreshed
1530 tmp=bls(ids,0,0):IF tmp<1 THEN act=0:RETURN:' no valid move found, we should never reach this state normally
1540 r=INT(RND*tmp)+1:tmpx=bls(ids,r,0):tmpy=bls(ids,r,1)
1550 ' We found a random valid block next we need to find a valid random target
1560 tmpopp=opp:GOSUB 1950:'populate valid moves
1570 tmp=vm(0,0):IF tmp<1 THEN act=0:RETURN:' no valid target found, we should never normally reach this state
1580 r=INT(RND*tmp)+1:tx=vm(r,0):ty=vm(r,1)
1590 bx=tmpx:by=tmpy:st(id,ilt,0)=tx:st(id,ilt,1)=ty:st(id,isl,0)=bx:st(id,isl,1)=by
1600 ' block and target selected, resolve action
1610 GOSUB 1680
1620 RETURN
1630 '
1640 ' CPU Defender
1650 GOSUB 1450
1660 RETURN
1661 '
1662 ' Human
1663 LOCATE 1,1:PRINT"Waiting for human";:CALL &BB18
1669 RETURN
1670 '
1680 ' action handler
1690 IF grd(tx,ty)=0 THEN act=1 ELSE GOSUB 2160:'if target is opp, resolve fight
1700 ' update stats on move or won fight
1710 IF act=1 OR act=2 THEN grd(tx,ty)=id:GOSUB 2060 ELSE RETURN
1720 'check if new occupied block is valid and if yes (tmp=1) add it to the list
1730 ids=id-1:tmp=0:tmpid=id:tmpx=tx:tmpy=ty:GOSUB 2290
1740 IF tmp=1 AND bls(ids,0,0)+1<=blmax THEN tmp=bls(ids,0,0)+1:bls(ids,0,0)=tmp:bls(ids,tmp,0)=tx:bls(ids,tmp,1)=ty
1750 'if opponent non human and if a fight was won at tx,ty then we need to remove opponent's block from his valid list
1760 IF h=0 AND act=2 THEN tmpid=opp:GOSUB 2370
1770 RETURN
1780 '
1790 ' Populate bls list with all valid moves for id
1800 tmp=0:ids=tmpid-1
1810 minx=st(tmpid,imn,0):miny=st(tmpid,imn,1):maxx=st(tmpid,imx,0):maxy=st(tmpid,imx,1)
1820 FOR x=minx TO maxx:FOR y=miny TO maxy
1830 SOUND 1,1000,2,15:hx=x:hy=y:GOSUB 2440:' highlight grid scanning
1840 IF grd(x,y)<>tmpid THEN GOTO 1920
1850 FOR dx=-1 TO 1:FOR dy=-1 TO 1
1860 IF dx=0 AND dy=0 THEN GOTO 1910
1870 nx=x+dx:ny=y+dy
1880 IF nx<1 OR nx>gw OR ny<1 OR ny>gh THEN GOTO 1910
1890 IF grd(nx,ny)=0 OR grd(nx,ny)=tmpopp THEN tmp=tmp+1 ELSE 1910
1900 bls(ids,0,0)=tmp:bls(ids,tmp,0)=x:bls(ids,tmp,1)=y:GOTO 1920:' valid block found move to next
1910 NEXT:NEXT
1920 NEXT:NEXT
1930 RETURN
1940 '
1950 ' populate valid moves for tmpopp and block at tmpx,tmpy
1960 tmp=0
1970 FOR dx=-1 TO 1:FOR dy=-1 TO 1
1980 IF dx=0 AND dy=0 THEN GOTO 2020
1990 nx=tmpx+dx:ny=tmpy+dy
2000 IF nx<1 OR nx>gw OR ny<1 OR ny>gh THEN 2020
2010 IF grd(nx,ny)=0 OR grd(nx,ny)=tmpopp THEN tmp=tmp+1:vm(tmp,0)=nx:vm(tmp,1)=ny ELSE 2020
2020 NEXT:NEXT
2030 vm(0,0)=tmp
2040 RETURN
2050 '
2060 ' update stats after move, or won fight
2070 tmp=st(id,icn,0)+1:st(id,icn,0)=tmp:st(id,ism,0)=st(id,ism,0)+tx:st(id,ism,1)=st(id,ism,1)+ty:st(id,ivg,0)=INT(st(id,ism,0)/tmp):st(id,ivg,1)=INT(st(id,ism,1)/tmp)
2080 st(id,imn,0)=MIN(st(id,imn,0),tx):st(id,imn,1)=MIN(st(id,imn,1),ty):st(id,imx,0)=MAX(st(id,imx,0),tx):st(id,imx,1)=MAX(st(id,imx,1),ty)
2090 IF act<>2 THEN RETURN
2100 'there was a fight and opp lost a block
2110 IF st(opp,icn,0)-1<1 THEN st(opp,icn,0)=0:RETURN
2120 tmp=st(opp,icn,0)-1:st(opp,ism,0)=st(opp,ism,0)-tx:st(opp,ism,1)=st(opp,ism,1)-ty:st(opp,ivg,0)=INT(st(opp,ism,0)/tmp):st(opp,ivg,1)=INT(st(opp,ism,1)/tmp):st(opp,icn,0)=tmp
2130 GOSUB 2200:' recalculate min max x y if needed due to opp's lost block
2140 RETURN
2150 '
2160 ' resolve fg
2170 r=RND:IF r>0.5 THEN act=2 ELSE act=3
2180 RETURN
2190 '
2200 ' recalculate minx maxx miny maxy
2210 IF st(opp,imn,0)=tx OR st(opp,imn,1)=ty OR st(opp,imx,0)=tx OR st(opp,imx,1)=ty THEN 2220 ELSE RETURN
2220 minx=st(opp,imn,0):maxx=st(opp,imx,0):miny=st(opp,imn,1):maxy=st(opp,imx,1)
2230 FOR i=minx TO maxx:FOR j=miny TO maxy:IF grd(i,j)=opp THEN st(opp,imn,0)=i:GOTO 2240 ELSE NEXT:NEXT
2240 FOR i=maxx TO minx STEP -1:FOR j=miny TO maxy:IF grd(i,j)=opp THEN st(opp,imx,0)=i:GOTO 2250 ELSE NEXT:NEXT
2250 FOR j=miny TO maxy:FOR i=minx TO maxx:IF grd(i,j)=opp THEN st(opp,imn,1)=j:GOTO 2260 ELSE NEXT:NEXT
2260 FOR j=maxy TO miny STEP -1:FOR i=minx TO maxx:IF grd(i,j)=opp THEN st(opp,imx,1)=j:GOTO 2270 ELSE NEXT:NEXT
2270 RETURN
2280 '
2290 ' check if block at tmpx tmpy has at least one valid move
2300 tmp=0:IF grd(tmpx,tmpy)<>tmpid THEN RETURN
2310 FOR dx=-1 TO 1:FOR dy=-1 TO 1:IF dx=0 AND dy=0 THEN GOTO 2340
2320 nx=tmpx+dx:ny=tmpy+dy:IF nx<1 OR nx>gw OR ny<1 OR ny>gh THEN 2340
2330 IF grd(nx,ny)=0 OR grd(nx,ny)=tmpopp THEN tmp=1:RETURN ELSE 2340
2340 NEXT:NEXT
2350 RETURN
2360 '
2370 ' remove block from list at tmpx,tmpy
2380 ids=tmpid-1:tmp=bls(ids,0,0):IF tmp<1 THEN RETURN
2390 FOR i=1 TO tmp:IF bls(ids,i,0)=tmpx AND bls(ids,i,1)=tmpy THEN 2400 ELSE NEXT
2400 FOR j=i TO tmp-1:bls(ids,j,0)=bls(ids,j+1,0):bls(ids,j,1)=bls(ids,j+1,1):NEXT
2410 bls(ids,0,0)=tmp-1
2420 RETURN
2430 '
2440 ' highlight
2450 IF hx<1 OR hy<1 OR hx>gw OR hy>gh THEN RETURN
2460 LOCATE ofx+hx,ofy+hy:IF tmpid=id1 THEN PEN cl1:PRINT hb$ ELSE PEN cl2:PRINT hb$
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
2580 CLEAR INPUT:CALL &BB18:LOCATE tmpx,hrows:PRINT SPC(tmp)
2590 RETURN
2600 ' draw grid border
2610 ' draw top and bottom horizontal
2620 PEN ctx:LOCATE ofx,ofy:PRINT CHR$(150)+STRING$(gw,CHR$(154))+CHR$(156)
2630 LOCATE ofx,ofy+gh+1:PRINT CHR$(147)+STRING$(gw,CHR$(154))+CHR$(153)
2640 ' draw verticals
2650 a$=CHR$(149):FOR i=ofy+1 TO gh+ofy:LOCATE ofx,i:PRINT a$;:LOCATE ofx+gw+1,i:PRINT a$:NEXT
2660 RETURN
2670 '
2680 ' Define grid size
2690 gw=MAX(INT(cols/3),INT(RND*cols)+2):gw=MIN(gw,cols-2):hgw=INT(gw/2):IF gw MOD 2=0 THEN gw=gw-1' Ensure gw is odd, -2 cols for vertical grid lines
2700 gh=MAX(INT(rows/3),INT(RND*rows)+2):gh=MIN(gh,rows-5):hgh=INT(gh/2):IF gh MOD 2=0 THEN gh=gh-1' Ensure gh is odd, -5 rows for 2 cpu rows, 1 status line and 2 grid rows
2710 gwh=gw*gh
2720 ofx=INT((cols-gw)/2):ofy=INT((rows-gh)/2)+1:' locate offsets: ofy+1 row to leave a blank line for the status line
2730 RETURN
2740 '
2750 ' Define starting positions
2760 st1x=INT(gw/2)+1:st1y=1:st2x=INT(gw/2)+1:st2y=gh
2770 FOR i=0 TO ial:st(id1,i,0)=st1x:st(id1,i,1)=st1y:st(id2,i,0)=st2x:st(id2,i,1)=st2y:NEXT
2780 c1=1:st(id1,icn,0)=c1:c2=1:st(id2,icn,0)=c2
2790 grd(st1x,st1y)=id1:grd(st2x,st2y)=id2:
2800 LOCATE  ofx+st1x,ofy+st1y:PEN cl1:PRINT b1$:LOCATE  ofx+st2x,ofy+st2y:PEN cl2:PRINT b2$
2810 p=0.1:sb=MAX(1,INT((gwh/2)*p)):' starting blocks formula
2820 ' randomly select starting blocks for reach player
2830 WHILE c1<sb OR c2<sb
2840 ' Player 1 and 2 block selection
2850 FOR i=id1 TO id2
2860 tmp=st(i,icn,0)
2870 IF tmp>=sb THEN 2960
2890 'id1 has blocks from y=1 to y=gh/2-1, e.g. if gh=11 id1 y ranges from 1-5. Similarly for id2 is from 7-11.
2880 IF i=id1 THEN tmpx=INT(RND*gw)+1:tmpy=INT(RND*INT(gh/2))+1 ELSE tmpx=INT(RND*gw)+1:tmpy=INT(RND*(gh-INT(gh/2)-1))+INT(gh/2)+2
2900 IF grd(tmpx,tmpy)<>0 THEN 2960
2910 grd(tmpx,tmpy)=i:tmp=tmp+1:st(i,icn,0)=tmp:st(i,ism,0)=st(i,ism,0)+tmpx:st(i,ism,1)=st(i,ism,1)+tmpy
2920 st(i,ivg,0)=INT(st(i,ism,0)/tmp):st(i,ivg,1)=INT(st(i,ism,1)/tmp):st(i,imn,0)=MIN(st(i,imn,0),tmpx):st(i,imn,1)=MIN(st(i,imn,1),tmpy)
2930 st(i,imx,0)=MAX(st(i,imx,0),tmpx):st(i,imx,1)=MAX(st(i,imx,1),tmpy)
2940 IF i=id1 THEN cpuclr=cl1:a$=b1$ ELSE cpuclr=cl2:a$=b2$
2950 SOUND 1,1500,2,10:LOCATE ofx+tmpx,ofy+tmpy:PEN cpuclr:PRINT a$
2960 NEXT
2970 c1=st(id1,icn,0):c2=st(id2,icn,0)
2980 WEND
2990 RETURN
3000 '
3010 ' error handling
3020 ms$="Error with code"+STR$(ERR)+" at line"+STR$(ERL):GOSUB 2560
3030 MODE 2:INK 0,1:INK 1,26:PEN 1:PAPER 0:BORDER 1:LOCATE 1,1:PRINT ms$
3040 IF ERR=5 THEN PRINT"Improper Argument"
3050 END
3060 INK 0,1:INK 1,26:PAPER 0:PEN 1:BORDER 1:MODE 2:END
3070 ' DATA
3080 DATA "start","sum","avg","min","max","sel","last","count":' sel is last selected position, last is last occupied position
3090 DATA "Normal","Attacker","Random","Defender","Human":' Personality names
3100 DATA "Nrm","Att","Rnd","Def","Hmn":' Shorthands for Personality names
