  #If 0
  =============================================================================
   Program NAME: TBAI_astar.inc
   Author      : Michael Hartlef
   Version     : 1.00
   Description : A* path finding functions
  =============================================================================
  'COPYRIGHT AND PERMISSION NOTICE
  '============================================================================
  Copyright (c) 2009, Michael Hartlef, <mike@fantomgl.com>
   
  All rights reserved.
   
  Permission To use this software Is granted only For the purpose To develop
  thinBasic language modules both For commercial Or non commercial purpose.
  In Any Case the above copyright notice And this permission notice must appear
  In All copies.
   
  THE SOFTWARE Is PROVIDED "AS IS", WITHOUT WARRANTY Of Any KIND, EXPRESS Or
  IMPLIED, INCLUDING BUT Not LIMITED To THE WARRANTIES Of MERCHANTABILITY,
  FITNESS For A PARTICULAR PURPOSE And NONINFRINGEMENT Of THIRD PARTY RIGHTS.
  In NO EVENT SHALL THE AUTHORS Or COPYRIGHT HOLDERS BE LIABLE For Any CLAIM,
  DAMAGES Or OTHER LIABILITY, WHETHER In AN ACTION Of CONTRACT, TORT Or
  OTHERWISE, ARISING From, Out Of Or In CONNECTION With THE SOFTWARE Or THE
  USE Or OTHER DEALINGS In THE SOFTWARE.          
  
  
  =============================================================================
  #EndIf



  #If Not %Def(%TRUE)
    %TRUE  = 1
  #EndIf
  #If Not %Def(%FALSE)
    %FALSE = 0
  #EndIf 


'*---------------------------------------------------------*
'*                         Equates                         *
'*---------------------------------------------------------*

'Declare Function timeGetTime Lib "winmm.dll" Alias "timeGetTime" () As Dword 

                     
'*---------------------------------------------------------*
'*                   User defined types                    *
'*---------------------------------------------------------*

  TYPE tNode
    id              AS LONG
    x               AS SINGLE
    y               AS SINGLE
    z               AS SINGLE
    f               AS DOUBLE
    g               AS DOUBLE
    h               AS DOUBLE
    'cost            AS LONG
    iOpen           AS LONG
    iClose          AS LONG
    parent          AS LONG
    block           AS LONG
    lchild(1 TO 8)  AS LONG
    lcost(1 TO 8)   AS DOUBLE
    childcount      AS LONG
  END TYPE

  TYPE tGrid
    lNodes(1 TO 65535)  AS tNode
    nodeCount           AS LONG
  END TYPE

  
'*---------------------------------------------------------*
'*                      Declarations                       *
'*---------------------------------------------------------*

  GLOBAL lOpenList()    AS LONG
  GLOBAL opencount      AS LONG

  GLOBAL pathnodecount AS LONG
  GLOBAL pathnodes() AS LONG

  '*****************************************************************
  SUB internal_OpenAdd(index AS LONG)
  '*****************************************************************
    opencount = opencount + 1
    lOpenList(opencount) = index
  END SUB


  '*****************************************************************
  SUB internal_OpenRemove(index AS LONG)
  '*****************************************************************
    LOCAL i,j AS LONG
    FOR i = 1 TO opencount
      IF lOpenList(i) = index THEN
        FOR j = i TO opencount - 1
          lOpenList(j) = lOpenList(j+1)
        NEXT
        EXIT FOR
      END IF
    NEXT
    IF opencount > 0 THEN opencount = opencount - 1

  END SUB
  


  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  SUB TBAI_GridDeleteNodes(BYVAL g AS tGrid PTR) EXPORT
'#ELSE
  SUB exec_TBAI_GridDeleteAllNodes()
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL gridID AS EXT
    LOCAL g AS tGrid PTR

    nParsed = thinBasic_ParseXNumbers(1,1,gridID)
    g = gridID
'#ENDIF
    @g.nodecount = 0
  END SUB
  

  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  SUB TBAI_GridDelete(BYVAL g AS tGrid PTR) EXPORT
'#ELSE
  SUB exec_TBAI_GridDelete()
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL gridID AS EXT
    LOCAL g AS tGrid PTR
    nParsed = thinBasic_ParseXNumbers(1,1,gridID)
    g = gridID
'#ENDIF
  '*****************************************************************
    @g.nodecount = 0            
    internal_MemFree(g)
  END SUB
  

  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  FUNCTION TBAI_GridCreate() EXPORT AS DWORD
'#ELSE
  FUNCTION exec_TBAI_GridCreate() AS EXT
  '*****************************************************************
'#ENDIF  
    LOCAL newgrid AS tGrid PTR
    DIM lOpenList(1 TO 65535) AS GLOBAL LONG
    DIM PathNodes(1 TO 65535) AS GLOBAL LONG
    newgrid = internal_Memalloc(SIZEOF(tGrid))   
    @newgrid.nodecount = 0
    FUNCTION = newgrid
  END FUNCTION


  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  FUNCTION TBAI_GridCountNodes(BYVAL g AS tGrid PTR) EXPORT AS LONG
'#ELSE
  FUNCTION exec_TBAI_GridCountNodes() AS EXT
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL gridID AS EXT
    LOCAL g AS tGrid PTR
    nParsed = thinBasic_ParseXNumbers(1,1,gridID)
    g = gridID
'#ENDIF
    FUNCTION = @g.nodecount
  END FUNCTION


  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  FUNCTION TBAI_NodeGetID(BYVAL g AS tGrid PTR, index AS LONG) EXPORT AS LONG
'#ELSE
  FUNCTION exec_TBAI_NodeGetID() AS EXT
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL gridID AS EXT
    LOCAL nodeIndex AS EXT
    LOCAL g AS tGrid PTR
    nParsed = thinBasic_ParseXNumbers(2,2,gridID, nodeIndex)
    g = gridID
'#ENDIF
    FUNCTION = @g.lNodes(nodeIndex).id
  END FUNCTION


  '*****************************************************************
  FUNCTION internal_GridGetNodeIndex(BYVAL g AS tGrid PTR, id AS LONG) AS LONG
  '*****************************************************************
    LOCAL index AS LONG
    LOCAL ret AS LONG
    ret = 0
    FOR index = 1 TO @g.nodecount
      IF  @g.lNodes(index).id = id THEN 
        ret = index
        EXIT FOR
      END IF
    NEXT
    FUNCTION = ret
  END FUNCTION


  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  FUNCTION TBAI_NodeGetIndex(BYVAL g AS tGrid PTR, id AS LONG) EXPORT AS LONG
'#ELSE
  FUNCTION exec_TBAI_NodeGetIndex() AS EXT
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL gridID AS EXT
    LOCAL id AS EXT
    LOCAL lid AS LONG
    LOCAL g AS tGrid PTR
    nParsed = thinBasic_ParseXNumbers(2,2,gridID, id)
    g = gridID
    lid = id
'#ENDIF

    FUNCTION = internal_GridGetNodeIndex(g,lid)
  END FUNCTION


  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  FUNCTION TBAI_GridAddNode(BYVAL g AS tGrid PTR, id AS LONG, x AS LONG, y AS LONG, z AS LONG) EXPORT AS DWORD
'#ELSE
  FUNCTION exec_TBAI_GridAddNode() AS EXT
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL gridID AS EXT
    LOCAL nodeID AS EXT
    LOCAL ex  AS EXT
    LOCAL ey  AS EXT
    LOCAL ez  AS EXT
    LOCAL id, X, Y, Z AS LONG
    LOCAL g AS tGrid PTR
    ez = 0.0
    nParsed = thinBasic_ParseXNumbers(4,5,gridID,NodeID, ex, ey, ez)
    g = gridID
    id = NodeID
    X = ex
    Y = ey
    Z = ez
'#ENDIF
    LOCAL index AS LONG
    
    @g.nodecount = @g.nodecount + 1
    index = @g.nodecount

    @g.lNodes(index).id = id
    @g.lNodes(index).x = x
    @g.lNodes(index).y = y
    @g.lNodes(index).z = z
    @g.lNodes(index).f = 0
    @g.lNodes(index).g = 0
    @g.lNodes(index).h = 0
    @g.lNodes(index).iOpen = %FALSE
    @g.lNodes(index).iClose = %FALSE
    '@g.lNodes(index).cost = 1
    @g.lNodes(index).parent = -1
    @g.lNodes(index).block = %FALSE 
    
    @g.lNodes(index).childcount = 0
        
    FUNCTION = index
  END FUNCTION


  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  SUB TBAI_NodeConnect(BYVAL g AS tGrid PTR, index AS LONG, childindex AS LONG ) EXPORT
'#ELSE
  SUB exec_TBAI_NodeConnect()
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL gridID AS EXT
    LOCAL enodeID1 AS EXT
    LOCAL enodeID2 AS EXT
    LOCAL cost AS DOUBLE
    'LOCAL msg AS STRING
    LOCAL xdiff, ydiff, zdiff AS DOUBLE
    LOCAL id1, id2 AS LONG

    LOCAL index, childindex AS LONG
    LOCAL g AS tGrid PTR
    nParsed = thinBasic_ParseXNumbers(3,3,gridID,eNodeID1, eNodeID2)
    g = gridID
    id1 = eNodeID1
    id2 = eNodeID2
'#ENDIF  
    LOCAL c AS LONG
    index = internal_GridGetNodeIndex( g, id1 )
    childindex = internal_GridGetNodeIndex( g, id2 )

    @g.lNodes(index).childcount = @g.lNodes(index).childcount + 1
    c = @g.lNodes(index).childcount
    @g.lNodes(index).lchild(c) = childindex
    xdiff = @g.lNodes(childindex).x - @g.lNodes(index).x
    ydiff = @g.lNodes(childindex).y - @g.lNodes(index).y
    zdiff = @g.lNodes(childindex).z - @g.lNodes(index).z
    cost = 100*SQR(xdiff^2 + ydiff^2 + zdiff^2)
    @g.lNodes(index).lcost(c) = cost
'msg = "from "+STR$(index)+ " to "+STR$(childindex)+"   cost:"+STR$(cost)
'MSGBOX msg  , %MB_ICONINFORMATION, "Check path"
  END SUB    

    
  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  SUB TBAI_NodeSetBlock(BYVAL g AS tGrid PTR, index AS LONG, block AS LONG) EXPORT
'#ELSE
  SUB exec_TBAI_NodeSetBlock()
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL gridID AS EXT
    LOCAL nodeIndex AS EXT
    LOCAL eblock  AS EXT
    LOCAL g AS tGrid PTR
    LOCAL index, block AS LONG
    'LOCAL msg AS STRING
    nParsed = thinBasic_ParseXNumbers(3,3,gridID,nodeIndex, eblock)
    g = gridID
    index = nodeIndex
    block = eblock
'#ENDIF
'msg = "Block set for node "+STR$(index)+ " to "+STR$(@g.lNodes(index).block) + " = "+str$(eblock)
'MSGBOX msg  , %MB_ICONINFORMATION, "Check path"
    @g.lNodes(index).block = block
  END SUB


  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  FUNCTION TBAI_NodeCountChildren(BYVAL g AS tGrid PTR, index AS LONG) EXPORT AS LONG
'#ELSE
  FUNCTION exec_TBAI_NodeCountChildren() AS EXT
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL nodeIndex AS EXT
    LOCAL gridID AS EXT
    LOCAL index AS LONG
    LOCAL g AS tGrid PTR
    nParsed = thinBasic_ParseXNumbers(2,2,gridID,nodeIndex)
    g = gridID
    index = nodeIndex
'#ENDIF
    FUNCTION = @g.lNodes(index).childcount
  END FUNCTION


  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  FUNCTION TBAI_NodeGetChild(BYVAL g AS tGrid PTR, index AS LONG, childNo AS LONG) EXPORT AS LONG
'#ELSE
  FUNCTION exec_TBAI_NodeGetChild() AS EXT
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL nodeIndex AS EXT
    LOCAL gridID AS EXT
    LOCAL cno    AS EXT
    LOCAL index,childNo AS LONG
    LOCAL g AS tGrid PTR
    nParsed = thinBasic_ParseXNumbers(3,3,gridID,nodeIndex, cno)
    g = gridID
    index = nodeIndex
    childno = cno
'#ENDIF
    FUNCTION = @g.lNodes(index).lchild(cno)
  END FUNCTION


  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  FUNCTION TBAI_NodeGetBlock(BYVAL g AS tGrid PTR, index AS LONG) EXPORT AS LONG
'#ELSE
  FUNCTION exec_TBAI_NodeGetBlock() AS EXT
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL nodeIndex AS EXT
    LOCAL gridID AS EXT
    LOCAL index AS LONG
    LOCAL g AS tGrid PTR
    nParsed = thinBasic_ParseXNumbers(2,2,gridID,nodeIndex)
    g = gridID
    index = nodeIndex
'#ENDIF
    FUNCTION = @g.lNodes(index).block
  END FUNCTION

  '*****************************************************************
  FUNCTION exec_TBAI_NodeFindClosest() AS EXT
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL g AS tGrid PTR
    LOCAL gridID,ex,ey,ez AS EXT
    LOCAL x,y,z,cdist, dist,diffx,diffy,diffz AS SINGLE
    LOCAL i,ci AS LONG
    
    ez = 0.0
    nParsed = thinBasic_ParseXNumbers(3,4, gridID, ex,ey,ez)
    g = gridID
    x = ex
    y = ey
    z = ez
    cdist = 999999.9
    ci = -1
    FOR i = 1 TO @g.nodecount
        diffX = x - @g.lNodes(i).X
        diffY = y - @g.lNodes(i).Y
        diffZ = z - @g.lNodes(i).Z
        dist = SQR(diffX^2 + diffY^2 + diffZ^2)
        IF dist < cdist THEN
        cdist = dist
        ci = i
        END IF    
    NEXT
'#ENDIF  
    FUNCTION = ci
  END FUNCTION

  '*****************************************************************
  SUB exec_TBAI_NodeGetPos()
  '*****************************************************************
    LOCAL nodeIndex AS EXT
    LOCAL gridID AS EXT
    LOCAL index AS LONG
    LOCAL g AS tGrid PTR
    LOCAL lVariablePtr1     AS LONG
    LOCAL lVariableAbsPos1  AS LONG

    LOCAL lVariablePtr2     AS LONG
    LOCAL lVariableAbsPos2  AS LONG

    LOCAL lVariablePtr3     AS LONG
    LOCAL lVariableAbsPos3  AS LONG


    'nParsed = thinBasic_ParseXNumbers(2,2,gridID,nodeIndex)
    IF thinBasic_CheckOpenParens_Mandatory THEN
      thinBasic_ParseNumber gridID
      g = gridID
          IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
          thinBasic_ParseNumber nodeIndex
          index = nodeIndex
          IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
            thinBasic_VariableParse(lVariablePtr1, lVariableAbsPos1)
            IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
              thinBasic_VariableParse(lVariablePtr2, lVariableAbsPos2)
                IF thinBasic_CheckCloseParens_Optional THEN
                    thinBasic_ChangeVariableNumberDirect (lVariablePtr1, lVariableAbsPos1, @g.lNodes(index).x)
                    thinBasic_ChangeVariableNumberDirect (lVariablePtr2, lVariableAbsPos2, @g.lNodes(index).y)
                ELSE
                    IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
                        thinBasic_VariableParse(lVariablePtr3, lVariableAbsPos3)
    
                        thinBasic_ChangeVariableNumberDirect (lVariablePtr1, lVariableAbsPos1, @g.lNodes(index).x)
                        thinBasic_ChangeVariableNumberDirect (lVariablePtr2, lVariableAbsPos2, @g.lNodes(index).y)
                        thinBasic_ChangeVariableNumberDirect (lVariablePtr3, lVariableAbsPos3, @g.lNodes(index).z)
                    END IF
                    thinBasic_CheckCloseParens_Mandatory
                END IF
            END IF
          END IF
      END IF
    END IF
  END SUB

    
                     
  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  FUNCTION TBAI_GridFindPath(BYVAL g AS tGrid PTR, startID AS LONG,endID AS LONG) EXPORT AS DWORD
'#ELSE
  FUNCTION exec_TBAI_GridFindPath() AS EXT
  '*****************************************************************
    REGISTER i AS LONG
    LOCAL c AS LONG
    LOCAL nParsed AS LONG
    LOCAL gridID  AS EXT
    LOCAL nodeID1 AS EXT
    LOCAL nodeID2 AS EXT
    LOCAL g AS tGrid PTR
    LOCAL startID, endID AS LONG

    nParsed = thinBasic_ParseXNumbers(3,3,gridID,NodeID1, nodeID2)
    g = gridID
    startID = NodeID1
    endID = NodeID2
'#ENDIF

    LOCAL item, sitem, eitem, citem, titem, nitem AS LONG
    LOCAL childcount AS LONG
    LOCAL ret AS DWORD
    LOCAL maxf AS LONG 
    LOCAL tempG AS LONG  
    LOCAL h1,h2,h3 AS LONG
    
    LOCAL tmpNode,tmpnode2 AS LONG
    LOCAL childNode AS LONG
    LOCAL startNode AS LONG
    LOCAL EndNode, EndNode2 AS LONG
    
    'LOCAL stime,etime,dtime AS DWORD
    'LOCAL msg AS STRING
'    Local lOpenList As Dword
    LOCAL endnodefound AS LONG
    
    
    'stime = timeGetTime 

    endnodefound=0
    opencount = 0
    
    'search for the start node     
    startNode = internal_GridGetNodeIndex( g, startID )
    
    'search for the end node     
    endNode = internal_GridGetNodeIndex( g, endID )       
    
'msg = "Startnode="+STR$(startnode)+ "    EndNode="+STR$(endnode)
'MSGBOX msg  , %MB_ICONINFORMATION, "Check path"
    
    
    c = @g.nodecount
    FOR i = 1 TO c
      @g.lNodes(i).iOpen = %FALSE
      @g.lNodes(i).iClose = %FALSE
      @g.lNodes(i).g = 0
    NEXT
    'Add the start node ot the open list
    'internal_OpenAdd(startNode)
    @g.lNodes(startNode).parent = 0
    @g.lNodes(startNode).iOpen = %TRUE
    
    
    childcount = @g.lNodes(startNode).childcount                          'msg=""
    FOR i = 1 TO childcount
        childNode = @g.lNodes(startNode).lchild(i)
    
        IF childNode <> endnode THEN
            IF @g.lNodes(childNode).block = %FALSE THEN
'msg = "Visit Child "+STR$(childnode)+ "   block:"+STR$(@g.lNodes(childNode).block)
'MSGBOX msg  , %MB_ICONINFORMATION, "Check path"
        
                '****@g.lNodes(childNode).g = @g.lNodes(childNode).cost + @g.lNodes(startNode).g       
                @g.lNodes(childNode).g = @g.lNodes(startnode).lcost(i) + @g.lNodes(startNode).g       

                '@g.lNodes(childNode).h = Abs(@g.lNodes(childNode).X - @g.lNodes(endnode).X) + Abs(@g.lNodes(childNode).Y - @g.lNodes(endnode).Y)
                h1 = @g.lNodes(childNode).X - @g.lNodes(endnode).X
                h2 = @g.lNodes(childNode).Y - @g.lNodes(endnode).Y
                h3 = @g.lNodes(childNode).Z - @g.lNodes(endnode).Z
                abs_lng(h1)
                abs_lng(h2)
                abs_lng(h3)
                @g.lNodes(childNode).h = h1 + h2 + h3

                @g.lNodes(childNode).F = @g.lNodes(childNode).g + @g.lNodes(childNode).h
                @g.lNodes(childNode).parent = startNode
'msg = "Child "+STR$(childnode)+ "   cost:"+STR$(@g.lNodes(startnode).lcost(i))
'MSGBOX msg  , %MB_ICONINFORMATION, "Check path"
                internal_OpenAdd(childNode)
                @g.lNodes(childNode).iOpen = %TRUE
            END IF
        ELSE
            '****@g.lNodes(childNode).g = @g.lNodes(childNode).cost + @g.lNodes(startnode).g
            @g.lNodes(childNode).g = @g.lNodes(startnode).lcost(i) + @g.lNodes(startnode).g
            @g.lNodes(childNode).F = @g.lNodes(childNode).g + @g.lNodes(childNode).h
            @g.lNodes(childNode).parent = startnode
            'endNode = childnode
            endnodefound = 1
        END IF
    NEXT

'msg = "node="+STR$(startnode)+ "    childnode="+STR$(childNode)+"   opencount="+STR$(opencount)
'MSGBOX msg  , %MB_ICONINFORMATION, "Check path"



    'internal_OpenRemove(startNode)

    @g.lNodes(startNode).iClose = %TRUE

    WHILE ( opencount > 0 AND endnodefound = 0 )
        'Check which Item should be looked at next
        maxf = 999999

        FOR i = 1 TO opencount
            tmpNode = lOpenList(i)
            IF @g.lNodes(tmpNode).F < maxF THEN 
                nitem = lOpenList(i)
                maxf = @g.lNodes(tmpNode).F
            END IF
        NEXT
        
        tmpNode = nItem
        childcount = @g.lNodes(tmpNode).childcount

'msg = "Nextnode="+STR$(tmpNode)+ "    childcount="+STR$(childcount)+ "    opencount="+STR$(opencount)
'MSGBOX msg  , %MB_ICONINFORMATION, "Check path"

        FOR i = 1 TO childcount
            childNode = @g.lNodes(tmpNode).lchild(i)

            IF childNode <> endnode THEN
                IF @g.lNodes(childNode).block = %FALSE THEN
                    'If @g.lNodes(childNode).iOpen = %FALSE Then 
                    '    If @g.lNodes(childNode).iClose = %FALSE Then
                    IF @g.lNodes(childNode).iClose = %FALSE THEN
                        IF @g.lNodes(childNode).iOpen = %FALSE THEN
                            '****@g.lNodes(childNode).g = @g.lNodes(childNode).cost + @g.lNodes(tmpNode).g
                            @g.lNodes(childNode).g = @g.lNodes(tmpNode).lcost(i) + @g.lNodes(tmpNode).g
                            
                            '@g.lNodes(childNode).h = Abs(@g.lNodes(childNode).X - @g.lNodes(endnode).X) + Abs(@g.lNodes(childNode).Y - @g.lNodes(endnode).Y)
                            
                            h1 = @g.lNodes(childNode).X - @g.lNodes(endnode).X
                            h2 = @g.lNodes(childNode).Y - @g.lNodes(endnode).Y
                            h3 = @g.lNodes(childNode).Z - @g.lNodes(endnode).Z
                            abs_lng(h1)
                            abs_lng(h2)
                            abs_lng(h3)
                            @g.lNodes(childNode).h = h1 + h2 + h3
                            
                            @g.lNodes(childNode).F = @g.lNodes(childNode).g + @g.lNodes(childNode).h
                            @g.lNodes(childNode).parent = @g.lNodes(tmpNode).ID

                            internal_OpenAdd(childNode)
                            @g.lNodes(childNode).iOpen = %TRUE
                        'End If
                    ELSE
                        'If @g.lNodes(childNode).iClose = %FALSE Then
                            '****tempG = @g.lNodes(childNode).cost + @g.lNodes(tmpNode).g
                            tempG = @g.lNodes(tmpNode).lcost(i) + @g.lNodes(tmpNode).g
                            IF tempG < @g.lNodes(childNode).G THEN
                                @g.lNodes(childNode).g = tempG
                                @g.lNodes(childNode).F = tempG + @g.lNodes(childNode).h
                                @g.lNodes(childNode).parent = tmpNode
                            END IF    
                        END IF
                    END IF
                END IF 
            ELSE
                '****@g.lNodes(childNode).g = @g.lNodes(childNode).cost + @g.lNodes(tmpNode).g
                @g.lNodes(childNode).g = @g.lNodes(tmpNode).lcost(i) + @g.lNodes(tmpNode).g
                @g.lNodes(childNode).F = @g.lNodes(childNode).g + @g.lNodes(childNode).h
                @g.lNodes(childNode).parent = tmpNode
                'endNode = childnode
                endnodefound = 1
                EXIT FOR
            END IF
        NEXT
        internal_OpenRemove(tmpNode)
        @g.lNodes(tmpNode).iClose = %TRUE
        
    WEND
    'etime = timeGetTime
    tmpNode = endnode
'    msg = STR$(@g.lNodes(tmpNode).ID)
    pathnodecount = 1
    pathnodes(pathnodecount) = @g.lNodes(tmpNode).ID
    WHILE (tmpnode <> startnode)
        pathnodecount = pathnodecount + 1
        tmpNode =  @g.lNodes(tmpNode).parent
'        msg = STR$(@g.lNodes(tmpNode).ID) + " > " +  msg
        pathnodes(pathnodecount) = @g.lNodes(tmpNode).ID
    WEND  
    'etime = timeGetTime 
    'dtime = etime- stime
    'IF dtime = 0 THEN dtime = 1

'msg = "It took " + STR$(dtime) + " milliseconds "+"(FPS:"+STR$(1000/dtime) +")"+" and cost "+ STR$(@g.lNodes(endNode).G) + "." + $CRLF + "The path is:" + $CRLF & msg
'MSGBOX msg, %MB_ICONINFORMATION, "Path found"

    IF @g.lNodes(endNode).g > 0 THEN
'msg = "Add Node to path:" + STR$(pathnodecount)
'MSGBOX msg, %MB_ICONINFORMATION, "Path found"
        'ret = internal_PathCreate(@g.lNodes(pathnodes(pathnodecount)).x,@g.lNodes(pathnodes(pathnodecount)).y,@g.lNodes(pathnodes(pathnodecount)).z,1,1,@g.lNodes(pathnodes(pathnodecount)).ID)
        ret = internal_PathCreate()
        internal_PathAddWP(ret, @g.lNodes(pathnodes(pathnodecount)).x,@g.lNodes(pathnodes(pathnodecount)).y,@g.lNodes(pathnodes(pathnodecount)).z,1,pathnodes(pathnodecount))
        WHILE pathnodecount > 1
            pathnodecount = pathnodecount - 1
'msg = "Add Node to path:" + STR$(pathnodecount)
'MSGBOX msg, %MB_ICONINFORMATION, "Path found"
            internal_PathAddWP(ret, @g.lNodes(pathnodes(pathnodecount)).x,@g.lNodes(pathnodes(pathnodecount)).y,@g.lNodes(pathnodes(pathnodecount)).z,1,pathnodes(pathnodecount))
        WEND 
        FUNCTION = ret
    ELSE
        FUNCTION = %FALSE
    END IF
  END FUNCTION            
