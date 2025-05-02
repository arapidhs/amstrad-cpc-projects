10 ' Sector Fight
20 RANDOMIZE TIME
30 ON ERROR GOTO 3280
40 ON BREAK GOSUB 3330
50 MODE 1:INK 0,0:INK 1,26:PAPER 0:PEN 1:BORDER 0
60 '
70 CLEAR INPUT:LOCATE 1,1:PRINT "Select mode (0-Mode0 1-Mode1)";
80 a$="":WHILE a$="":a$=INKEY$:WEND
90 IF a$<>"0" AND a$<>"1" THEN a$="1"
100 smd=VAL(a$):cols=20*(2^smd):rows=25:hcols=INT(cols/2):hrows=INT(rows/2)
110 CLEAR INPUT:LOCATE 1,3:PRINT"Auto Pause at End of Turn":PRINT"Default No (Y/N)";
120 a$="":WHILE a$="":a$=INKEY$:WEND
130 IF UPPER$(a$)="Y" THEN ps=1 ELSE ps=0
140 CLEAR INPUT:LOCATE 1,6:PRINT"1 CPU Match (default) 2 vs Human";
150 a$="":WHILE a$="":a$=INKEY$:WEND
160 IF a$<>"1" AND a$<>"2" THEN a$="1"
170 h=VAL(a$):IF h=1 THEN h=0 ELSE h=1:' h 0 means no human
180 '
190 ' Screen initialization and colors
200 ' cbg bg color, cl1,cl2 cpu colors, ctx text color
210 INK 0,0:INK 1,2:INK 2,6:INK 3,26:cbg=0:cl1=1:cl2=2:ctx=3:PAPER cbg:BORDER cbg:PEN ctx:MODE smd
220 ms$="Loading...":tmp=INT(LEN(ms$)/2):tmpx=MAX(1,hcols-tmp):LOCATE tmpx,hrows:PEN ctx:PRINT ms$
230 '
240 ' Setup blocks, cpu1 cpu2 empty block, highlight block
250 b1$=CHR$(207):b2$=CHR$(207):SYMBOL 240,0,0,60,60,60,60,0,0:eb$=CHR$(32):hb$=CHR$(240)
260 '
270 ' Initialize cpu/player stats
280 ' position, sum xy, avg xy, min and max xy
290 ' selected pos., last occupied pos.
300 id1=1:id2=2
310 ial=7:ist=0:ism=1:ivg=2:imn=3:imx=4:isl=5:ilt=6:icn=7
320 DIM st(2,ial,1):DIM st$(ial):RESTORE 3350:FOR i=0 TO ial:READ st$(i):NEXT
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
490 psz=4:pnrm=0:patt=1:prnd=2:pdef=3:phmn=4
500 DIM pnprb(psz):pnprb(pnrm)=0.25:pnprb(patt)=pnprb(pnrm)+0.25:pnprb(prnd)=pnprb(patt)+0.25:pnprb(pdef)=pnprb(prnd)+0.25
510 '
520 ' Personality names pn$()
530 ' Normal, 1 Attacker, 2 Random, 3 Defender
540 DIM pn$(psz)
550 IF smd=0 THEN RESTORE 3370 ELSE RESTORE 3360
560 FOR i=0 TO psz:READ pn$(i):NEXT
570 '
580 ' Assign personalities based on personality probabilities: pnprb()
590 r=RND:IF r<pnprb(pnrm) THEN pn1=pnrm ELSE IF r<pnprb(patt) THEN pn1=patt ELSE IF r<pnprb(prnd) THEN pn1=prnd ELSE pn1=pdef
600 r=RND:IF r<pnprb(pnrm) THEN pn2=pnrm ELSE IF r<pnprb(patt) THEN pn2=patt ELSE IF r<pnprb(prnd) THEN pn2=prnd ELSE pn2=pdef
610 IF h=1 THEN pn2=phmn
620 '
630 ' Wait for key press
640 CLS:IF smd=0 THEN ms$="Press key to start" ELSE ms$="Press any key to start":GOSUB 2830:CLS
650 '
660 ' print personalities
670 IF h=0 THEN id1$="CPU 1":id2$="CPU 2" ELSE id1$="CPU":id2$="You"
680 c1$=id1$+": "+pn$(pn1):c2$=id2$+": "+pn$(pn2)
690 c1x=MAX(LEN(c1$),LEN(c2$))+2:c1y=1:c2x=c1x:c2y=rows' aligned x location to print status on cpu rows
700 sx=1:sy=2:' location of status line
710 LOCATE 1,1:PEN cl1:PRINT c1$
720 LOCATE 1,rows:PEN cl2:PRINT c2$
730 '
740 ' Initialize grid and starting positions
750 LOCATE sx,sy:PAPER ctx:PEN cbg:PRINT STRING$(cols," "):LOCATE sx,sy:PRINT"Initializing...":PAPER cbg:PEN ctx
760 GOSUB 2950:DIM grd(gw,gh):grd(0,0)=-1:' setup grid dimensions
770 GOSUB 2870:' draw grid border
780 GOSUB 3020:' define and draw starting positions and stats
790 '
800 'lists of valid moves for player 1 and 2
810 'element 0 of bls store the counter of valid moves e.g. bls(0,0,0) counter of potential valid moves for player 1 while bls(1,0,0) for player 2
820 blmax=(gw+gh)*2:DIM bls(1,blmax,1)' REM bls(0) stores list for player id1, bls(2) for player id2
830 '
840 DIM vm(8,1):' array to store all potential 8 valid moves around a given block, first element at pos 0 stores the count of valid moves e.g.e vm(0,0)=5
850 '
860 ' Initialize valid block lists for players
870 tmpid=id1:tmpopp=id2:GOSUB 1970
880 IF h=0 THEN tmpid=id2:tmpopp=id1:GOSUB 1970:'if it is a cpu vs cpu match initialize cpu 2 list has well
890 '
900 ' clear initialization message
910 LOCATE sx,sy:PRINT STRING$(cols," ")
920 '
930 c1=st(id1,icn,0):c2=st(id2,icn,0)
940 GOSUB 2780:' print block counts
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
1060 PEN ctx:LOCATE sx,sy:PRINT "Turn";:PEN cpuclr:PRINT trn;:PEN ctx:PRINT STR$(prg);"%";
1070 PEN cpuclr:mst$="...":LOCATE clx,cly:PRINT mst$;
1080 '
1090 'Process cpu action based on personality
1100 act=0:ON pn+1 GOSUB 1420,1460,1500,1690,1730
1110 c1=st(id1,icn,0):c2=st(id2,icn,0)
1120 LOCATE clx,cly:PRINT SPC(LEN(mst$));
1130 ' act 0 is no move found, 1 is move, 2 fight won, 3 fight loss
1140 IF act=0 THEN GOTO 1290:'no valid move found, end game
1150 ' print move or fg results to screen
1160 tmpid=id:'used by highlight routine to decide color
1170 IF act=1 THEN hx=tx:hy=ty:GOSUB 2620:SOUND 1,200,20,15:GOSUB 2620:' move highlight, play sound highlight
1180 IF act=2 THEN hx=tx:hy=ty:GOSUB 2620:SOUND 1,142,20,15:SOUND 1,95,20,15:GOSUB 2620:' fight won highlight, play sound highlight
1190 IF act=3 THEN hx=tx:hy=ty:GOSUB 2620:SOUND 1,95,20,15:SOUND 1,125,20,15:GOSUB 2620:' fight lost highlight, play sound highlight
1200 IF act=1 OR act=2 THEN LOCATE ofx+tx,ofy+ty:PEN cpuclr:PRINT b$;
1210 GOSUB 2780:' print block counts
1220 'auto pause
1230 IF ps=1 THEN a$="":FOR i=sx TO cols:LOCATE i,sy:a$=a$+COPYCHR$(#0):NEXT:PAPER cpuclr:PEN cbg:LOCATE sx,sy:PRINT a$:PAPER cbg:PEN cpuclr:CLEAR INPUT:CALL &BB18
1240 IF c1+c2>=gwh OR c1=0 OR c2=0 THEN GOTO 1330
1250 IF turn=1 THEN turn=2 ELSE turn=1
1260 trs=1
1270 WEND
1280 '
1290 ' error: no valid move found
1300 IF smd<>0 THEN ms$="Error: no valid move found" ELSE ms$="Err: no move found"
1310 GOSUB 2830:GOTO 1380:' print error message and exit
1320 '
1330 ' end game
1340 IF smd<>0 THEN ms$="Game Over: "+id1$+":"+STR$(c1)+" "+id2$+":"+STR$(c2) ELSE ms$="Game Over:"+MID$(STR$(c1),2)+"/"+MID$(STR$(c2),2)
1350 GOSUB 2830
1360 IF c1>c2 THEN ms$="CPU 1 wins!" ELSE IF c1<c2 THEN ms$="CPU 2 wins!" ELSE ms$="Draw!"
1370 GOSUB 2830
1380 CLS:ms$="Play again? (Y:N)":LOCATE hcols-INT(LEN(ms$)/2),hrows:INPUT"Play Again? (Y:N)",a$
1390 IF UPPER$(a$)="Y" THEN RUN ELSE GOTO 3330
1400 END
1410 '
1420 ' CPU Normal
1430 GOSUB 1500
1440 RETURN
1450 '
1460 ' CPU Attacker
1470 GOSUB 1500
1480 RETURN
1490 '
1500 ' CPU Random
1510 ids=id-1:bx=0:by=0:tx=0:ty=0:tmpid=id:tmpopp=opp
1520 tmp=bls(ids,0,0):IF tmp<1 THEN act=0:RETURN:' no valid move found, we should never reach this state normally
1530 r=INT(RND*tmp)+1:tmpx=bls(ids,r,0):tmpy=bls(ids,r,1)
1540 'verify that the block is still valid, else refresh the valid block list
1550 tmp=0:GOSUB 2470
1560 IF tmp=0 THEN tmpid=id:GOSUB 1970 ELSE 1600:'if move invalid tmp=0 then refresh list
1570 'retry getting valid block after list has been refreshed
1580 tmp=bls(ids,0,0):IF tmp<1 THEN act=0:RETURN:' no valid move found, we should never reach this state normally
1590 r=INT(RND*tmp)+1:tmpx=bls(ids,r,0):tmpy=bls(ids,r,1)
1600 ' We found a random valid block next we need to find a valid random target
1610 tmpopp=opp:GOSUB 2130:'populate valid moves
1620 tmp=vm(0,0):IF tmp<1 THEN act=0:RETURN:' no valid target found, we should never normally reach this state
1630 r=INT(RND*tmp)+1:tx=vm(r,0):ty=vm(r,1)
1640 bx=tmpx:by=tmpy:st(id,ilt,0)=tx:st(id,ilt,1)=ty:st(id,isl,0)=bx:st(id,isl,1)=by
1650 ' block and target selected, resolve action
1660 GOSUB 1860
1670 RETURN
1680 '
1690 ' CPU Defender
1700 GOSUB 1500
1710 RETURN
1720 '
1730 ' Human
1740 hx=st(id,isl,0):hy=st(id,isl,1):tmpx=hx:tmpy=hy:' Initialize cursor at last selected position
1750 GOSUB 2690:' Highlight cursor
1760 WHILE 1:CLEAR INPUT
1770 a$=INKEY$:IF a$="" THEN GOTO 1770
1780 tmpx=hx:tmpy=hy
1790 IF a$=CHR$(240) THEN hy=hy-1 ELSE IF a$=CHR$(241) THEN hy=hy+1 ELSE IF a$=CHR$(242) THEN hx=hx-1 ELSE IF a$=CHR$(243) THEN hx=hx+1 ELSE IF a$=CHR$(32) THEN 1820 ELSE GOTO 1760
1800 IF hx<1 THEN hx=1 ELSE IF hx>gw THEN hx=gw ELSE IF hy<1 THEN hy=1 ELSE IF hy>gh THEN hy=gh
1810 GOSUB 2690:GOTO 1760:'Highlight cursor and WEND
1820 IF grd(hx,hy)=id THEN SOUND 1,0,2,12,0,1:ENT -1,1,100,1 ELSE SOUND 1,300,10,10:SOUND 1,400,10,10
1830 WEND
1840 RETURN
1850 '
1860 ' action handler
1870 IF grd(tx,ty)=0 THEN act=1 ELSE GOSUB 2340:'if target is opp, resolve fight
1880 ' update stats on move or won fight
1890 IF act=1 OR act=2 THEN grd(tx,ty)=id:GOSUB 2240 ELSE RETURN
1900 'check if new occupied block is valid and if yes (tmp=1) add it to the list
1910 ids=id-1:tmp=0:tmpid=id:tmpx=tx:tmpy=ty:GOSUB 2470
1920 IF tmp=1 AND bls(ids,0,0)+1<=blmax THEN tmp=bls(ids,0,0)+1:bls(ids,0,0)=tmp:bls(ids,tmp,0)=tx:bls(ids,tmp,1)=ty
1930 'if opponent non human and if a fight was won at tx,ty then we need to remove opponent's block from his valid list
1940 IF h=0 AND act=2 THEN tmpid=opp:GOSUB 2550
1950 RETURN
1960 '
1970 ' Populate bls list with all valid moves for id
1980 tmp=0:ids=tmpid-1
1990 minx=st(tmpid,imn,0):miny=st(tmpid,imn,1):maxx=st(tmpid,imx,0):maxy=st(tmpid,imx,1)
2000 FOR x=minx TO maxx:FOR y=miny TO maxy
2010 SOUND 1,1000,2,15:hx=x:hy=y:GOSUB 2620:' highlight grid scanning
2020 IF grd(x,y)<>tmpid THEN GOTO 2100
2030 FOR dx=-1 TO 1:FOR dy=-1 TO 1
2040 IF dx=0 AND dy=0 THEN GOTO 2090
2050 nx=x+dx:ny=y+dy
2060 IF nx<1 OR nx>gw OR ny<1 OR ny>gh THEN GOTO 2090
2070 IF grd(nx,ny)=0 OR grd(nx,ny)=tmpopp THEN tmp=tmp+1 ELSE 2090
2080 bls(ids,0,0)=tmp:bls(ids,tmp,0)=x:bls(ids,tmp,1)=y:GOTO 2100:' valid block found move to next
2090 NEXT:NEXT
2100 NEXT:NEXT
2110 RETURN
2120 '
2130 ' populate valid moves for tmpopp and block at tmpx,tmpy
2140 tmp=0
2150 FOR dx=-1 TO 1:FOR dy=-1 TO 1
2160 IF dx=0 AND dy=0 THEN GOTO 2200
2170 nx=tmpx+dx:ny=tmpy+dy
2180 IF nx<1 OR nx>gw OR ny<1 OR ny>gh THEN 2200
2190 IF grd(nx,ny)=0 OR grd(nx,ny)=tmpopp THEN tmp=tmp+1:vm(tmp,0)=nx:vm(tmp,1)=ny ELSE 2200
2200 NEXT:NEXT
2210 vm(0,0)=tmp
2220 RETURN
2230 '
2240 ' update stats after move, or won fight
2250 tmp=st(id,icn,0)+1:st(id,icn,0)=tmp:st(id,ism,0)=st(id,ism,0)+tx:st(id,ism,1)=st(id,ism,1)+ty:st(id,ivg,0)=INT(st(id,ism,0)/tmp):st(id,ivg,1)=INT(st(id,ism,1)/tmp)
2260 st(id,imn,0)=MIN(st(id,imn,0),tx):st(id,imn,1)=MIN(st(id,imn,1),ty):st(id,imx,0)=MAX(st(id,imx,0),tx):st(id,imx,1)=MAX(st(id,imx,1),ty)
2270 IF act<>2 THEN RETURN
2280 'there was a fight and opp lost a block
2290 IF st(opp,icn,0)-1<1 THEN st(opp,icn,0)=0:RETURN
2300 tmp=st(opp,icn,0)-1:st(opp,ism,0)=st(opp,ism,0)-tx:st(opp,ism,1)=st(opp,ism,1)-ty:st(opp,ivg,0)=INT(st(opp,ism,0)/tmp):st(opp,ivg,1)=INT(st(opp,ism,1)/tmp):st(opp,icn,0)=tmp
2310 GOSUB 2380:' recalculate min max x y if needed due to opp's lost block
2320 RETURN
2330 '
2340 ' resolve fg
2350 r=RND:IF r>0.5 THEN act=2 ELSE act=3
2360 RETURN
2370 '
2380 ' recalculate minx maxx miny maxy
2390 IF st(opp,imn,0)=tx OR st(opp,imn,1)=ty OR st(opp,imx,0)=tx OR st(opp,imx,1)=ty THEN 2400 ELSE RETURN
2400 minx=st(opp,imn,0):maxx=st(opp,imx,0):miny=st(opp,imn,1):maxy=st(opp,imx,1)
2410 FOR i=minx TO maxx:FOR j=miny TO maxy:IF grd(i,j)=opp THEN st(opp,imn,0)=i:GOTO 2420 ELSE NEXT:NEXT
2420 FOR i=maxx TO minx STEP -1:FOR j=miny TO maxy:IF grd(i,j)=opp THEN st(opp,imx,0)=i:GOTO 2430 ELSE NEXT:NEXT
2430 FOR j=miny TO maxy:FOR i=minx TO maxx:IF grd(i,j)=opp THEN st(opp,imn,1)=j:GOTO 2440 ELSE NEXT:NEXT
2440 FOR j=maxy TO miny STEP -1:FOR i=minx TO maxx:IF grd(i,j)=opp THEN st(opp,imx,1)=j:GOTO 2450 ELSE NEXT:NEXT
2450 RETURN
2460 '
2470 ' check if block at tmpx tmpy has at least one valid move
2480 tmp=0:IF grd(tmpx,tmpy)<>tmpid THEN RETURN
2490 FOR dx=-1 TO 1:FOR dy=-1 TO 1:IF dx=0 AND dy=0 THEN GOTO 2520
2500 nx=tmpx+dx:ny=tmpy+dy:IF nx<1 OR nx>gw OR ny<1 OR ny>gh THEN 2520
2510 IF grd(nx,ny)=0 OR grd(nx,ny)=tmpopp THEN tmp=1:RETURN ELSE 2520
2520 NEXT:NEXT
2530 RETURN
2540 '
2550 ' remove block from list at tmpx,tmpy
2560 ids=tmpid-1:tmp=bls(ids,0,0):IF tmp<1 THEN RETURN
2570 FOR i=1 TO tmp:IF bls(ids,i,0)=tmpx AND bls(ids,i,1)=tmpy THEN 2580 ELSE NEXT
2580 FOR j=i TO tmp-1:bls(ids,j,0)=bls(ids,j+1,0):bls(ids,j,1)=bls(ids,j+1,1):NEXT
2590 bls(ids,0,0)=tmp-1
2600 RETURN
2610 '
2620 ' highlight
2630 IF hx<1 OR hy<1 OR hx>gw OR hy>gh THEN RETURN
2640 LOCATE ofx+hx,ofy+hy:IF tmpid=id1 THEN PEN cl1:PRINT hb$ ELSE PEN cl2:PRINT hb$
2650 IF grd(hx,hy)=id1 THEN PEN cl1:a$=b1$ ELSE IF grd(hx,hy)=id2 THEN PEN cl2:a$=b2$ ELSE a$=eb$
2660 LOCATE ofx+hx,ofy+hy:PRINT a$
2670 RETURN
2680 '
2690 'highlight human cursor
2700 'restore previous position
2710 IF grd(tmpx,tmpy)=id1 THEN PEN cl1:tmp$=b1$ ELSE IF grd(tmpx,tmpy)=id2 THEN PEN cl2:tmp$=b2$ ELSE tmp$=eb$
2720 LOCATE ofx+tmpx,ofy+tmpy:PRINT tmp$
2730 PEN ctx:IF grd(hx,hy)=id1 THEN PAPER cl1 ELSE IF grd(hx,hy)=id2 THEN PAPER cl2 ELSE PAPER cbg
2740 LOCATE ofx+hx,ofy+hy:PRINT hb$
2750 PAPER cbg:PEN cpuclr
2760 RETURN
2770 '
2780 ' print block counts
2790 PEN cl1:LOCATE cols-3,c1y:PRINT USING "####";c1;
2800 PEN cl2:LOCATE cols-3,c2y:PRINT USING "####";c2;
2810 RETURN
2820 '
2830 ' print centered message
2840 tmp=INT(LEN(ms$)/2):tmpx=MAX(1,hcols-tmp):LOCATE tmpx,hrows:PEN ctx:PRINT ms$:tmp=LEN(ms$)
2850 CLEAR INPUT:CALL &BB18:LOCATE tmpx,hrows:PRINT SPC(tmp)
2860 RETURN
2870 ' draw grid border
2880 ' draw top and bottom horizontal
2890 PEN ctx:LOCATE ofx,ofy:PRINT CHR$(150)+STRING$(gw,CHR$(154))+CHR$(156)
2900 LOCATE ofx,ofy+gh+1:PRINT CHR$(147)+STRING$(gw,CHR$(154))+CHR$(153)
2910 ' draw verticals
2920 a$=CHR$(149):FOR i=ofy+1 TO gh+ofy:LOCATE ofx,i:PRINT a$;:LOCATE ofx+gw+1,i:PRINT a$:NEXT
2930 RETURN
2940 '
2950 ' Define grid size
2960 gw=MAX(INT(cols/3),INT(RND*cols)+2):gw=MIN(gw,cols-2):hgw=INT(gw/2):IF gw MOD 2=0 THEN gw=gw-1' Ensure gw is odd, -2 cols for vertical grid lines
2970 gh=MAX(INT(rows/3),INT(RND*rows)+2):gh=MIN(gh,rows-5):hgh=INT(gh/2):IF gh MOD 2=0 THEN gh=gh-1' Ensure gh is odd, -5 rows for 2 cpu rows, 1 status line and 2 grid rows
2980 gwh=gw*gh
2990 ofx=INT((cols-gw)/2):ofy=INT((rows-gh)/2)+1:' locate offsets: ofy+1 row to leave a blank line for the status line
3000 RETURN
3010 '
3020 ' Define starting positions
3030 st1x=INT(gw/2)+1:st1y=1:st2x=INT(gw/2)+1:st2y=gh
3040 FOR i=0 TO ial:st(id1,i,0)=st1x:st(id1,i,1)=st1y:st(id2,i,0)=st2x:st(id2,i,1)=st2y:NEXT
3050 c1=1:st(id1,icn,0)=c1:c2=1:st(id2,icn,0)=c2
3060 grd(st1x,st1y)=id1:grd(st2x,st2y)=id2:
3070 LOCATE  ofx+st1x,ofy+st1y:PEN cl1:PRINT b1$:LOCATE  ofx+st2x,ofy+st2y:PEN cl2:PRINT b2$
3080 p=0.1:sb=MAX(1,INT((gwh/2)*p)):' starting blocks formula
3090 ' randomly select starting blocks for reach player
3100 WHILE c1<sb OR c2<sb
3110 ' Player 1 and 2 block selection
3120 FOR i=id1 TO id2
3130 tmp=st(i,icn,0)
3140 IF tmp>=sb THEN 3230
3160 'id1 has blocks from y=1 to y=gh/2-1, e.g. if gh=11 id1 y ranges from 1-5. Similarly for id2 is from 7-11.
3150 IF i=id1 THEN tmpx=INT(RND*gw)+1:tmpy=INT(RND*INT(gh/2))+1 ELSE tmpx=INT(RND*gw)+1:tmpy=INT(RND*(gh-INT(gh/2)-1))+INT(gh/2)+2
3170 IF grd(tmpx,tmpy)<>0 THEN 3230
3180 grd(tmpx,tmpy)=i:tmp=tmp+1:st(i,icn,0)=tmp:st(i,ism,0)=st(i,ism,0)+tmpx:st(i,ism,1)=st(i,ism,1)+tmpy
3190 st(i,ivg,0)=INT(st(i,ism,0)/tmp):st(i,ivg,1)=INT(st(i,ism,1)/tmp):st(i,imn,0)=MIN(st(i,imn,0),tmpx):st(i,imn,1)=MIN(st(i,imn,1),tmpy)
3200 st(i,imx,0)=MAX(st(i,imx,0),tmpx):st(i,imx,1)=MAX(st(i,imx,1),tmpy)
3210 IF i=id1 THEN cpuclr=cl1:a$=b1$ ELSE cpuclr=cl2:a$=b2$
3220 SOUND 1,1500,2,10:LOCATE ofx+tmpx,ofy+tmpy:PEN cpuclr:PRINT a$
3230 NEXT
3240 c1=st(id1,icn,0):c2=st(id2,icn,0)
3250 WEND
3260 RETURN
3270 '
3280 ' error handling
3290 ms$="Error with code"+STR$(ERR)+" at line"+STR$(ERL):GOSUB 2830
3300 MODE 2:INK 0,1:INK 1,26:PEN 1:PAPER 0:BORDER 1:LOCATE 1,1:PRINT ms$
3310 IF ERR=5 THEN PRINT"Improper Argument"
3320 END
3330 INK 0,1:INK 1,26:PAPER 0:PEN 1:BORDER 1:MODE 2:END
3340 ' DATA
3350 DATA "start","sum","avg","min","max","sel","last","count":' sel is last selected position, last is last occupied position
3360 DATA "Normal","Attacker","Random","Defender","Human":' Personality names
3370 DATA "Nrm","Att","Rnd","Def","Hmn":' Shorthands for Personality names
