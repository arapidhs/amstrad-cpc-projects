10 RANDOMIZE TIME
20 ON ERROR GOTO 2950
30 ON BREAK GOSUB 3000
40 MODE 1:INK 0,0:INK 1,26:PAPER 0:PEN 1:BORDER 0
50 '
60 CLEAR INPUT:LOCATE 1,1:PRINT "Select mode (0-Mode0 1-Mode1)";
70 a$="":WHILE a$="":a$=INKEY$:WEND
80 IF a$<>"0" AND a$<>"1" THEN a$="1"
90 smd=VAL(a$):cols=20*(2^smd):rows=25:hcols=INT(cols/2):hrows=INT(rows/2)
110 CLEAR INPUT:LOCATE 1,3:PRINT"Auto Pause at End of Turn":PRINT"Default No (Y/N)";
120 a$="":WHILE a$="":a$=INKEY$:WEND
130 IF UPPER$(a$)="Y" THEN ps=1 ELSE ps=0
140 '
150 ' Screen initialization and colors
160 ' cbg bg color, cl1,cl2 cpu colors, ctx text color
170 INK 0,0:INK 1,2:INK 2,6:INK 3,26:cbg=0:cl1=1:cl2=2:ctx=3:PAPER cbg:BORDER cbg:PEN ctx:MODE smd
190 ms$="Loading...":tmp=INT(LEN(ms$)/2):tmpx=MAX(1,hcols-tmp):LOCATE tmpx,hrows:PEN ctx:PRINT ms$
200 '
210 ' Setup blocks, cpu1 cpu2 empty block, highlight block
220 b1$=CHR$(207):b2$=CHR$(207):SYMBOL 240,0,0,60,60,60,60,0,0:eb$=CHR$(32):hb$=CHR$(240)
250 '
270 ' Initialize cpu/player stats
280 ' position, sum xy, avg xy, min and max xy
290 ' selected pos., last occupied pos.
300 id1=1:id2=2
310 ial=7:ist=0:ism=1:ivg=2:imn=3:imx=4:isl=5:ilt=6:icn=7
320 DIM st(2,ial,1):DIM st$(ial):RESTORE 3020:FOR i=0 TO ial:READ st$(i):NEXT
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
560 IF smd=0 THEN RESTORE 3040 ELSE RESTORE 3030
570 FOR i=0 TO psz:READ pn$(i):NEXT
580 '
590 ' Assign personalities based on personality probabilities: pnprb()
600 r=RND:IF r<pnprb(pnrm) THEN pn1=pnrm ELSE IF r<pnprb(patt) THEN pn1=patt ELSE IF r<pnprb(prnd) THEN pn1=prnd ELSE pn1=pdef
610 r=RND:IF r<pnprb(pnrm) THEN pn2=pnrm ELSE IF r<pnprb(patt) THEN pn2=patt ELSE IF r<pnprb(prnd) THEN pn2=prnd ELSE pn2=pdef
620 '
630 ' Wait for key press
640 CLS:IF smd=0 THEN ms$="Press key to start" ELSE ms$="Press any key to start":GOSUB 2460:CLS
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
760 GOSUB 2620:DIM grd(gw,gh):grd(0,0)=-1:' setup grid dimensions
770 GOSUB 2540:' draw grid border
780 GOSUB 2690:' define and draw starting positions and stats
790 '
800 'lists of valid moves for player 1 and 2
810 'element 0 of bls store the counter of valid moves e.g. bls(0,0,0) counter of potential valid moves for player 1 while bls(1,0,0) for player 2
820 blmax=(gw+gh)*2:DIM bls(1,blmax,1)' REM bls(0) stores list for player id1, bls(2) for player id2
830 '
840 DIM vm(8,1):' array to store all potential 8 valid moves around a given block, first element at pos 0 stores the count of valid moves e.g.e vm(0,0)=5
850 '
860 ' Initialize valid block lists for players
870 tmpid=id1:tmpopp=id2:GOSUB 1760
880 tmpid=id2:tmpopp=id1:GOSUB 1760
890 '
900 ' clear initialization message
910 LOCATE sx,sy:PRINT STRING$(cols," ")
920 '
930 c1=st(id1,icn,0):c2=st(id2,icn,0)
940 GOSUB 2410:' print block counts
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
1060 PEN cpuclr:LOCATE sx,sy:PRINT "Turn";trn;STR$(prg);"%":PEN cpuclr:mst$="...":LOCATE clx,cly:PRINT mst$;
1070 '
1080 'Process cpu action based on personality
1090 act=0:ON pn+1 GOSUB 1400,1440,1480,1720
1100 c1=st(id1,icn,0):c2=st(id2,icn,0)
1110 LOCATE clx,cly:PRINT SPC(LEN(mst$));
1120 ' act 0 is no move found, 1 is move, 2 fight won, 3 fight loss
1130 IF act=0 THEN GOTO 1270:'no valid move found, end game
1140 ' print move or fg results to screen
1150 tmpid=id:'used by highlight routine to decide color
1160 IF act=1 THEN hx=tx:hy=ty:GOSUB 2340:SOUND 1,200,20,15:GOSUB 2340:' move highlight, play sound highlight
1170 IF act=2 THEN hx=tx:hy=ty:GOSUB 2340:SOUND 1,142,20,15:SOUND 1,95,20,15:GOSUB 2340:' fight won highlight, play sound highlight
1180 IF act=3 THEN hx=tx:hy=ty:GOSUB 2340:SOUND 1,95,20,15:SOUND 1,125,20,15:GOSUB 2340:' fight lost highlight, play sound highlight
1190 IF act=1 OR act=2 THEN LOCATE ofx+tx,ofy+ty:PEN cpuclr:PRINT b$;
1200 GOSUB 2410:' print block counts
1201 'auto pause
1210 IF ps=1 THEN a$="":FOR i=sx TO cols:LOCATE i,sy:a$=a$+COPYCHR$(#0):NEXT:PAPER cpuclr:PEN cbg:LOCATE sx,sy:PRINT a$:PAPER cbg:PEN cpuclr:CLEAR INPUT:CALL &BB18
1220 IF c1+c2>=gwh OR c1=0 OR c2=0 THEN GOTO 1310
1230 IF turn=1 THEN turn=2 ELSE turn=1
1240 trs=1
1250 WEND
1260 '
1270 ' error: no valid move found
1280 IF smd<>0 THEN ms$="Error: no valid move found" ELSE ms$="Err: no move found"
1290 GOSUB 2460:GOTO 1360:' print error message and exit
1300 '
1310 ' end game
1320 IF smd<>0 THEN ms$="Game Over: "+id1$+":"+STR$(c1)+" "+id2$+":"+STR$(c2) ELSE ms$="Game Over:"+MID$(STR$(c1),2)+"/"+MID$(STR$(c2),2)
1330 GOSUB 2460
1340 IF c1>c2 THEN ms$="CPU 1 wins!" ELSE IF c1<c2 THEN ms$="CPU 2 wins!" ELSE ms$="Draw!"
1350 GOSUB 2460
1360 CLS:ms$="Play again? (Y:N)":LOCATE hcols-INT(LEN(ms$)/2),hrows:INPUT"Play Again? (Y:N)",a$
1370 IF UPPER$(a$)="Y" THEN RUN ELSE GOTO 3000
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
1490 ids=id-1:bx=0:by=0:tx=0:ty=0:tmpid=id:tmpopp=opp
1500 tmp=bls(ids,0,0):IF tmp<1 THEN act=0:RETURN:' no valid move found, we should never reach this state normally
1510 r=INT(RND*tmp)+1:tmpx=bls(ids,r,0):tmpy=bls(ids,r,1)
1520 'verify that the block is still valid, else refresh the valid block list
1530 tmp=0:GOSUB 2260
1540 IF tmp=0 THEN tmpid=id:GOSUB 1770 ELSE 1580:'if move invalid tmp=0 then refresh list
1550 'retry getting valid block after list has been refreshed
1560 tmp=bls(ids,0,0):IF tmp<1 THEN act=0:RETURN:' no valid move found, we should never reach this state normally
1570 r=INT(RND*tmp)+1:tmpx=bls(ids,r,0):tmpy=bls(ids,r,1)
1580 ' We found a random valid block next we need to find a valid random target
1590 tmpopp=opp:GOSUB 1920:'populate valid moves
1600 tmp=vm(0,0):IF tmp<1 THEN act=0:RETURN:' no valid target found, we should never normally reach this state
1610 r=INT(RND*tmp)+1:tx=vm(r,0):ty=vm(r,1)
1620 bx=tmpx:by=tmpy:st(id,ilt,0)=tx:st(id,ilt,1)=ty:st(id,isl,0)=bx:st(id,isl,1)=by
1630 IF grd(tx,ty)=0 THEN act=1 ELSE GOSUB 2130:'if target is opp, resolve fight
1640 ' update stats on move or won fight
1650 IF act=1 OR act=2 THEN grd(tx,ty)=id:GOSUB 2030 ELSE RETURN
1660 'check if new occupied block is valid and if yes (tmp=1) add it to the list
1670 tmp=0:tmpx=tx:tmpy=ty:GOSUB 2260
1680 IF tmp=1 AND bls(ids,0,0)+1<=blmax THEN tmp=bls(ids,0,0)+1:bls(ids,0,0)=tmp:bls(ids,tmp,0)=tx:bls(ids,tmp,1)=ty
1681' if a fight was won at tx,ty then we need to remove opponent's block from his valid list
1682 IF act=2 THEN tmpid=opp:GOSUB 2322
1700 RETURN
1710 '
1720 ' CPU Defender
1730 GOSUB 1480
1740 RETURN
1750 '
1760 ' Populate bls list with all valid moves for id
1770 tmp=0:ids=tmpid-1
1780 minx=st(tmpid,imn,0):miny=st(tmpid,imn,1):maxx=st(tmpid,imx,0):maxy=st(tmpid,imx,1)
1790 FOR x=minx TO maxx:FOR y=miny TO maxy
1800 SOUND 1,1000,2,15:hx=x:hy=y:GOSUB 2340:' highlight grid scanning
1810 IF grd(x,y)<>tmpid THEN GOTO 1890
1820 FOR dx=-1 TO 1:FOR dy=-1 TO 1
1830 IF dx=0 AND dy=0 THEN GOTO 1880
1840 nx=x+dx:ny=y+dy
1850 IF nx<1 OR nx>gw OR ny<1 OR ny>gh THEN GOTO 1880
1860 IF grd(nx,ny)=0 OR grd(nx,ny)=tmpopp THEN tmp=tmp+1 ELSE 1880
1870 bls(ids,0,0)=tmp:bls(ids,tmp,0)=x:bls(ids,tmp,1)=y:GOTO 1890:' valid block found move to next
1880 NEXT:NEXT
1890 NEXT:NEXT
1900 RETURN
1910 '
1920 ' populate valid moves for tmpopp and block at tmpx,tmpy
1930 tmp=0
1940 FOR dx=-1 TO 1:FOR dy=-1 TO 1
1950 IF dx=0 AND dy=0 THEN GOTO 1990
1960 nx=tmpx+dx:ny=tmpy+dy
1970 IF nx<1 OR nx>gw OR ny<1 OR ny>gh THEN 1990
1980 IF grd(nx,ny)=0 OR grd(nx,ny)=tmpopp THEN tmp=tmp+1:vm(tmp,0)=nx:vm(tmp,1)=ny ELSE 1990
1990 NEXT:NEXT
2000 vm(0,0)=tmp
2010 RETURN
2020 '
2030 ' update stats after move, or won fight
2040 tmp=st(id,icn,0)+1:st(id,icn,0)=tmp:st(id,ism,0)=st(id,ism,0)+tx:st(id,ism,1)=st(id,ism,1)+ty:st(id,ivg,0)=INT(st(id,ism,0)/tmp):st(id,ivg,1)=INT(st(id,ism,1)/tmp)
2050 st(id,imn,0)=MIN(st(id,imn,0),tx):st(id,imn,1)=MIN(st(id,imn,1),ty):st(id,imx,0)=MAX(st(id,imx,0),tx):st(id,imx,1)=MAX(st(id,imx,1),ty)
2060 IF act<>2 THEN RETURN
2070 'there was a fight and opp lost a block
2080 IF st(opp,icn,0)-1<1 THEN st(opp,icn,0)=0:RETURN
2090 tmp=st(opp,icn,0)-1:st(opp,ism,0)=st(opp,ism,0)-tx:st(opp,ism,1)=st(opp,ism,1)-ty:st(opp,ivg,0)=INT(st(opp,ism,0)/tmp):st(opp,ivg,1)=INT(st(opp,ism,1)/tmp):st(opp,icn,0)=tmp
2100 GOSUB 2170:' recalculate min max x y if needed due to opp's lost block
2110 RETURN
2120 '
2130 ' resolve fg
2140 r=RND:IF r>0.5 THEN act=2 ELSE act=3
2150 RETURN
2160 '
2170 ' recalculate minx maxx miny maxy
2180 IF st(opp,imn,0)=tx OR st(opp,imn,1)=ty OR st(opp,imx,0)=tx OR st(opp,imx,1)=ty THEN 2190 ELSE RETURN
2190 minx=st(opp,imn,0):maxx=st(opp,imx,0):miny=st(opp,imn,1):maxy=st(opp,imx,1)
2200 FOR i=minx TO maxx:FOR j=miny TO maxy:IF grd(i,j)=opp THEN st(opp,imn,0)=i:GOTO 2210 ELSE NEXT:NEXT
2210 FOR i=maxx TO minx STEP -1:FOR j=miny TO maxy:IF grd(i,j)=opp THEN st(opp,imx,0)=i:GOTO 2220 ELSE NEXT:NEXT
2220 FOR j=miny TO maxy:FOR i=minx TO maxx:IF grd(i,j)=opp THEN st(opp,imn,1)=j:GOTO 2230 ELSE NEXT:NEXT
2230 FOR j=maxy TO miny STEP -1:FOR i=minx TO maxx:IF grd(i,j)=opp THEN st(opp,imx,1)=j:GOTO 2240 ELSE NEXT:NEXT
2240 RETURN
2250 '
2260 ' check if block at tmpx tmpy has at least one valid move
2270 tmp=0:IF grd(tmpx,tmpy)<>tmpid THEN RETURN
2280 FOR dx=-1 TO 1:FOR dy=-1 TO 1:IF dx=0 AND dy=0 THEN GOTO 2310
2290 nx=tmpx+dx:ny=tmpy+dy:IF nx<1 OR nx>gw OR ny<1 OR ny>gh THEN 2310
2300 IF grd(nx,ny)=0 OR grd(nx,ny)=tmpopp THEN tmp=1:RETURN ELSE 2310
2310 NEXT:NEXT
2320 RETURN
2321 '
2322 ' remove block from list at tmpx,tmpy
2324 ids=tmpid-1:tmp=bls(ids,0,0):IF tmp<1 THEN RETURN
2325 FOR i=1 TO tmp:IF bls(ids,i,0)=tmpx AND bls(ids,i,1)=tmpy THEN 2326 ELSE NEXT
2326 FOR j=i TO tmp-1:bls(ids,j,0)=bls(ids,j+1,0):bls(ids,j,1)=bls(ids,j+1,1):NEXT
2327 bls(ids,0,0)=tmp-1
2329 RETURN
2330 '
2340 ' highlight
2350 IF hx<1 OR hy<1 OR hx>gw OR hy>gh THEN RETURN
2360 LOCATE ofx+hx,ofy+hy:IF tmpid=id1 THEN PEN cl1:PRINT hb$ ELSE PEN cl2:PRINT hb$
2370 IF grd(hx,hy)=id1 THEN PEN cl1:a$=b1$ ELSE IF grd(hx,hy)=id2 THEN PEN cl2:a$=b2$ ELSE a$=eb$
2380 LOCATE ofx+hx,ofy+hy:PRINT a$
2390 RETURN
2400 '
2410 ' print block counts
2420 PEN cl1:LOCATE cols-3,c1y:PRINT USING "####";c1;
2430 PEN cl2:LOCATE cols-3,c2y:PRINT USING "####";c2;
2440 RETURN
2450 '
2460 ' print centered message
2470 tmp=INT(LEN(ms$)/2):tmpx=MAX(1,hcols-tmp):LOCATE tmpx,hrows:PEN ctx:PRINT ms$:tmp=LEN(ms$)
2480 CLEAR INPUT:CALL &BB18:LOCATE tmpx,hrows:PRINT SPC(tmp)
2490 RETURN
2540 ' draw grid border
2550 ' draw top and bottom horizontal
2560 PEN ctx:LOCATE ofx,ofy:PRINT CHR$(150)+STRING$(gw,CHR$(154))+CHR$(156)
2570 LOCATE ofx,ofy+gh+1:PRINT CHR$(147)+STRING$(gw,CHR$(154))+CHR$(153)
2580 ' draw verticals
2590 a$=CHR$(149):FOR i=ofy+1 TO gh+ofy:LOCATE ofx,i:PRINT a$;:LOCATE ofx+gw+1,i:PRINT a$:NEXT
2600 RETURN
2610 '
2620 ' Define grid size
2630 gw=MAX(INT(cols/3),INT(RND*cols)+2):gw=MIN(gw,cols-2):hgw=INT(gw/2):IF gw MOD 2=0 THEN gw=gw-1' Ensure gw is odd, -2 cols for vertical grid lines
2640 gh=MAX(INT(rows/3),INT(RND*rows)+2):gh=MIN(gh,rows-5):hgh=INT(gh/2):IF gh MOD 2=0 THEN gh=gh-1' Ensure gh is odd, -5 rows for 2 cpu rows, 1 status line and 2 grid rows
2650 gwh=gw*gh
2660 ofx=INT((cols-gw)/2):ofy=INT((rows-gh)/2)+1:' locate offsets: ofy+1 row to leave a blank line for the status line
2670 RETURN
2680 '
2690 ' Define starting positions
2700 st1x=INT(gw/2)+1:st1y=1:st2x=INT(gw/2)+1:st2y=gh
2710 FOR i=0 TO ial:st(id1,i,0)=st1x:st(id1,i,1)=st1y:st(id2,i,0)=st2x:st(id2,i,1)=st2y:NEXT
2720 c1=1:st(id1,icn,0)=c1:c2=1:st(id2,icn,0)=c2
2730 grd(st1x,st1y)=id1:grd(st2x,st2y)=id2:
2740 LOCATE  ofx+st1x,ofy+st1y:PEN cl1:PRINT b1$:LOCATE  ofx+st2x,ofy+st2y:PEN cl2:PRINT b2$
2750 p=0.1:sb=MAX(1,INT((gwh/2)*p)):' starting blocks formula
2760 ' randomly select starting blocks for reach player
2770 WHILE c1<sb OR c2<sb
2780 ' Player 1 and 2 block selection
2790 FOR i=id1 TO id2
2800 tmp=st(i,icn,0)
2810 IF tmp>=sb THEN 2900
2830 'id1 has blocks from y=1 to y=gh/2-1, e.g. if gh=11 id1 y ranges from 1-5. Similarly for id2 is from 7-11.
2820 IF i=id1 THEN tmpx=INT(RND*gw)+1:tmpy=INT(RND*INT(gh/2))+1 ELSE tmpx=INT(RND*gw)+1:tmpy=INT(RND*(gh-INT(gh/2)-1))+INT(gh/2)+2
2840 IF grd(tmpx,tmpy)<>0 THEN 2900
2850 grd(tmpx,tmpy)=i:tmp=tmp+1:st(i,icn,0)=tmp:st(i,ism,0)=st(i,ism,0)+tmpx:st(i,ism,1)=st(i,ism,1)+tmpy
2860 st(i,ivg,0)=INT(st(i,ism,0)/tmp):st(i,ivg,1)=INT(st(i,ism,1)/tmp):st(i,imn,0)=MIN(st(i,imn,0),tmpx):st(i,imn,1)=MIN(st(i,imn,1),tmpy)
2870 st(i,imx,0)=MAX(st(i,imx,0),tmpx):st(i,imx,1)=MAX(st(i,imx,1),tmpy)
2880 IF i=id1 THEN cpuclr=cl1:a$=b1$ ELSE cpuclr=cl2:a$=b2$
2890 SOUND 1,1500,2,10:LOCATE ofx+tmpx,ofy+tmpy:PEN cpuclr:PRINT a$
2900 NEXT
2910 c1=st(id1,icn,0):c2=st(id2,icn,0)
2920 WEND
2930 RETURN
2940 '
2950 ' error handling
2960 ms$="Error with code"+STR$(ERR)+" at line"+STR$(ERL):GOSUB 2460
2970 MODE 2:INK 0,1:INK 1,26:PEN 1:PAPER 0:BORDER 1:LOCATE 1,1:PRINT ms$
2980 IF ERR=5 THEN PRINT"Improper Argument"
2990 END
3000 INK 0,1:INK 1,26:PAPER 0:PEN 1:BORDER 1:MODE 2:END
3010 ' DATA
3020 DATA "start","sum","avg","min","max","sel","last","count":' sel is last selected position, last is last occupied position
3030 DATA "Normal","Attacker","Random","Defender":' Personality names
3040 DATA "Nrm","Att","Rnd","Def":' Shorthands for Personality names
