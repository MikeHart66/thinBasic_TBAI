  #IF 0
  =============================================================================
   Program NAME: TBAI_waypoints.inc
   Author      : Michael Hartlef
   Version     : 1.00
   Description : waypoints functions
  =============================================================================
  'COPYRIGHT AND PERMISSION NOTICE
  '============================================================================
  Copyright (c) 2009, Michael Hartlef, <mike@fantomgl.com>

  ALL rights reserved.

  Permission TO use this software IS granted only FOR the purpose TO develop
  thinBasic language modules both FOR commercial OR non commercial purpose.
  IN ANY CASE the above copyright notice AND this permission notice must appear
  IN ALL copies.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT OF THIRD PARTY RIGHTS.
  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
  DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
  OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
  USE OR OTHER DEALINGS IN THE SOFTWARE.


  =============================================================================
  #ENDIF



  #IF NOT %DEF(%TRUE)
    %TRUE  = 1
  #ENDIF
  #IF NOT %DEF(%FALSE)
    %FALSE = 0
  #ENDIF

MACRO DegToRad(dpDegrees) = (dpDegrees*0.0174532925199433##)
MACRO RadToDeg(dpRadians) = (dpRadians*57.29577951308232##)

'*---------------------------------------------------------*
'*                         Equates                         *
'*---------------------------------------------------------*

'----- Marker direction -----
%pmkForward = 1
%pmkBackwards = 2

'----- Marker move mode -----
%pmkStop = 0
%pmkBounce = 1
%pmkLoop = 2


'*---------------------------------------------------------*
'*                   User defined types                    *
'*---------------------------------------------------------*


TYPE tWpNode
    ID            AS LONG
    xpos          AS SINGLE
    ypos          AS SINGLE
    zpos          AS SINGLE
    funcName      AS ASCIIZ * 64
END TYPE


TYPE tWaypoints
    speed         AS SINGLE
    sensivity     AS SINGLE
    'pathAngle     AS SINGLE
    pathAngleD    AS SINGLE
    currdist      AS SINGLE
    'currAngle     AS SINGLE
    currAngleD    AS SINGLE
    destIndex     AS LONG
    sourceIndex   AS LONG
    diffX         AS SINGLE
    diffY         AS SINGLE
    diffZ         AS SINGLE
    distance      AS SINGLE
    nodecount     AS LONG
    lNodes(1 TO 255) AS tWpNode
    direction     AS LONG
    'mode          AS LONG
END TYPE

TYPE tWpMarker
    speed         AS SINGLE
    sensivity     AS SINGLE

    currdist      AS SINGLE
    'currAngle     AS SINGLE
    currAngleD    AS SINGLE
    currXpos      AS SINGLE
    currYpos      AS SINGLE
    currZpos      AS SINGLE
    
    currDistTrav  AS SINGLE

    destIndex     AS LONG
    sourceIndex   AS LONG

    destXpos      AS SINGLE
    destYpos      AS SINGLE
    destZpos      AS SINGLE
    sourceXpos    AS SINGLE
    sourceYpos    AS SINGLE
    sourceZpos    AS SINGLE

    diffX         AS SINGLE
    diffY         AS SINGLE
    diffZ         AS SINGLE

    distance      AS SINGLE
    path          AS DWORD
    direction     AS LONG
    MODE          AS LONG
    subdiv        AS LONG
    currsubdiv    AS LONG
    final         AS LONG
END TYPE


TYPE sPoint
    x   AS SINGLE
    y   AS SINGLE
    z   AS SINGLE
END TYPE 


'*---------------------------------------------------------*
'*                      Declarations                       *
'*---------------------------------------------------------*


  '*****************************************************************
  FUNCTION internal_ATN2( BYVAL y AS EXT, BYVAL x AS EXT) AS EXT
  '*****************************************************************
      LOCAL ax, ay, phi AS EXT
      IF x = 0.0## AND y = 0.0## THEN
        FUNCTION = 0.0##
      ELSE
        ax = ABS(x)
        ay = ABS(y)
    
        IF ax > ay THEN
          phi = RadToDeg( ATN(ay/ax) )
        ELSE
          phi = 90.0## - RadToDeg( ATN(ax/ay) )
        END IF
    
        IF x < 0.0## THEN phi = 180## - phi
        IF y < 0.0## THEN phi = -phi
        FUNCTION = phi
      END IF
  END FUNCTION

  '*****************************************************************
  FUNCTION internal_PathCreate() AS DWORD
  '*****************************************************************
    LOCAL wp AS tWaypoints PTR
    LOCAL index AS LONG
    'LOCAL msg AS STRING
    wp = internal_Memalloc(SIZEOF(tWaypoints))

    'Fill the waypoint structure
    @wp.speed = 1.0
    @wp.sensivity = 0.1
    '@wp.pathangle = 0.0
    @wp.pathangleD = 0.0
    @wp.currdist = 0.0
    '@wp.currAngle = 0.0
    @wp.currAngleD = 0.0
    @wp.sourceIndex = 0
    @wp.destIndex = 0
    @wp.diffX    = 0.0
    @wp.diffY    = 0.0
    @wp.diffz    = 0.0
    @wp.distance = 0.0
    @wp.nodecount = 0

    @wp.direction = 1
    '@wp.MODE = 0



    FUNCTION = wp
  END FUNCTION



  
  
  '*****************************************************************
  FUNCTION internal_MarkerCreate(BYVAL wp AS tWaypoints PTR , dir AS LONG, imode AS LONG, subd AS LONG) AS DWORD
  '*****************************************************************
    LOCAL mk AS tWpMarker PTR
    LOCAL iDest, iSrc AS LONG

    mk = internal_Memalloc(SIZEOF(tWpMarker))
   

    'Fill the waypoint structure
    @mk.speed = 1.0
    @mk.sensivity = 0.1
    @mk.path = wp
    @mk.direction = dir
    @mk.MODE = imode
    @mk.subdiv = subd
    @mk.currsubdiv = 0

    IF dir = %pmkForward THEN
        @mk.sourceIndex = 1
        @mk.destIndex = 2
    ELSE
        @mk.sourceIndex = @wp.nodecount
        @mk.destIndex   = @mk.sourceIndex - 1
    END IF
    iDest = @mk.destIndex
    iSrc  = @mk.sourceIndex
    
    @mk.diffX      = @wp.lNodes(iDest).XPos - @wp.lNodes(iSrc).XPos
    @mk.diffY      = @wp.lNodes(iDest).YPos - @wp.lNodes(iSrc).YPos
    @mk.diffz      = @wp.lNodes(iDest).ZPos - @wp.lNodes(iSrc).ZPos
    @mk.currXPos   = @wp.lNodes(iSrc).XPos
    @mk.currYPos   = @wp.lNodes(iSrc).YPos
    @mk.currZPos   = @wp.lNodes(iSrc).ZPos
    @mk.sourceXPos = @wp.lNodes(iSrc).XPos
    @mk.sourceYPos = @wp.lNodes(iSrc).YPos
    @mk.sourceZPos = @wp.lNodes(iSrc).ZPos
    @mk.destXPos   = @wp.lNodes(idest).XPos
    @mk.destYPos   = @wp.lNodes(idest).YPos
    @mk.destZPos   = @wp.lNodes(idest).ZPos

    @mk.distance = SQR(@mk.diffX^2 + @mk.diffY^2 + @mk.diffz^2)
    @mk.currdist = 0.0
    @mk.currAngleD = internal_ATN2( @mk.diffY, @mk.diffX )+90.0
    '@mk.currAngle = DegToRad(@mk.currAngleD)
    @mk.currDistTrav = 0.0


    @mk.final = %FALSE
    FUNCTION = mk
  END FUNCTION


  '*****************************************************************
  SUB internal_PathAddWP(BYVAL wp AS tWaypoints PTR, xpos AS SINGLE, ypos AS SINGLE, zpos AS SINGLE, typ AS LONG, ID AS LONG)
  '*****************************************************************

    LOCAL np,pl  AS DWORD
    LOCAL index AS LONG
    
    @wp.nodecount = @wp.nodecount + 1
    index = @wp.nodecount
    IF id = 0 THEN
        id = @wp.nodecount
    END IF    
    
    'Set the values for the first node
    @wp.lNodes(index).ID = id
    @wp.lNodes(index).xpos = xpos
    @wp.lNodes(index).ypos = ypos
    @wp.lNodes(index).zpos = zpos
    '@wp.lNodes(index).itype = typ
    @wp.lNodes(index).funcname = ""

    IF @wp.nodecount = 2 THEN
        @wp.destIndex = 2
        @wp.diffX = XPos - @wp.lNodes(1).xPos
        @wp.diffY = YPos - @wp.lNodes(1).yPos
        @wp.diffZ = ZPos - @wp.lNodes(1).zPos
        @wp.distance = SQR(@wp.diffX^2 + @wp.diffY^2 + @wp.diffZ^2)
        
        @wp.currAngleD = internal_ATN2( @wp.diffY, @wp.diffX )+90.0
        '@wp.currAngle = DegToRad(@wp.currAngleD)

    END IF
  END SUB


  '*********************************************************
  SUB internal_PointOnSpline(pn AS sPoint, t AS SINGLE, p0 AS sPoint, p1 AS sPoint, p2 AS sPoint, p3 AS sPoint)
  '*********************************************************
    LOCAL t2,t3 AS SINGLE
    t2 = t * t
    t3 = t2 * t
    pn.x = 0.5 * ( ( 2.0 * p1.x ) + ( -p0.x + p2.x ) * t + ( 2.0 * p0.x - 5.0 * p1.x + 4 * p2.x - p3.x ) * t2 + ( -p0.x + 3.0 * p1.x - 3.0 * p2.x + p3.x ) * t3 )
    pn.y = 0.5 * ( ( 2.0 * p1.y ) + ( -p0.y + p2.y ) * t + ( 2.0 * p0.y - 5.0 * p1.y + 4 * p2.y - p3.y ) * t2 + ( -p0.y + 3.0 * p1.y - 3.0 * p2.y + p3.y ) * t3 )
    pn.z = 0.5 * ( ( 2.0 * p1.z ) + ( -p0.z + p2.z ) * t + ( 2.0 * p0.z - 5.0 * p1.z + 4 * p2.z - p3.z ) * t2 + ( -p0.z + 3.0 * p1.z - 3.0 * p2.z + p3.z ) * t3 )
  END SUB 




  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  FUNCTION TBAI_MarkerCreate(BYVAL mk AS tWpMarker PTR, typ AS LONG, mode AS LONG) EXPORT AS DWORD
'#ELSE
  FUNCTION exec_TBAI_MarkerCreate() AS EXT
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL pwp, edir, emode, esubd AS EXT
    LOCAL dir, lmode, lsubd  AS LONG
    'LOCAL mk AS DWORD
    LOCAL wp AS tWaypoints PTR

    edir = %pmkForward
    emode = %pmkStop
    esubd = 0
    
    nParsed = thinBasic_ParseXNumbers(1,4, pwp, edir, emode, esubd)
    
    wp = pwp
    lmode = emode
    dir = edir
    lsubd = esubd
'#ENDIF
    LOCAL ret AS DWORD
    ret = internal_MarkerCreate(pwp, dir, lmode, lsubd)
    FUNCTION = ret
  END FUNCTION


  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  FUNCTION TBAI_MarkerCurrAngle(BYVAL mk AS tWpMarker PTR) EXPORT AS SINGLE
'#ELSE
  FUNCTION exec_TBAI_MarkerCurrAngle() AS EXT
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL mk AS tWpMarker PTR
    LOCAL pmk AS EXT
    nParsed = thinBasic_ParseXNumbers(1,1, pmk)
    mk = pmk
'#ENDIF  
    FUNCTION = @mk.currAngleD
  END FUNCTION


  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  FUNCTION TBAI_MarkerCurrDist(BYVAL mk AS tWpMarker PTR) EXPORT AS SINGLE
'#ELSE
  FUNCTION exec_TBAI_MarkerCurrDist() AS EXT
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL mk AS tWpMarker PTR
    LOCAL pmk AS EXT
    nParsed = thinBasic_ParseXNumbers(1,1, pmk)
    mk = pmk
'#ENDIF  
      FUNCTION = @mk.currdist
  END FUNCTION


  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  FUNCTION TBAI_MarkerFinished(BYVAL mk AS tWpMarker PTR) EXPORT AS SINGLE
'#ELSE
  FUNCTION exec_TBAI_MarkerFinished() AS EXT
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL mk AS tWpMarker PTR
    LOCAL pmk AS EXT
    nParsed = thinBasic_ParseXNumbers(1,1, pmk)
    mk = pmk
'#ENDIF  
      FUNCTION = @mk.final
  END FUNCTION


  '*****************************************************************
  SUB exec_TBAI_MarkerCurrPos()
  '*****************************************************************
    LOCAL mk AS tWpMarker PTR
    LOCAL pmk AS EXT
    LOCAL lVariablePtr1     AS LONG
    LOCAL lVariableAbsPos1  AS LONG

    LOCAL lVariablePtr2     AS LONG
    LOCAL lVariableAbsPos2  AS LONG

    LOCAL lVariablePtr3     AS LONG
    LOCAL lVariableAbsPos3  AS LONG


    'nParsed = thinBasic_ParseXNumbers(3,4, pwp, x, y, z)
    IF thinBasic_CheckOpenParens_Mandatory THEN
      thinBasic_ParseNumber pmk
      mk = pmk
      IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
        thinBasic_VariableParse(lVariablePtr1, lVariableAbsPos1)
        IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
          thinBasic_VariableParse(lVariablePtr2, lVariableAbsPos2)
            IF thinBasic_CheckCloseParens_Optional THEN
                thinBasic_ChangeVariableNumberDirect (lVariablePtr1, lVariableAbsPos1, @mk.currXpos)
                thinBasic_ChangeVariableNumberDirect (lVariablePtr2, lVariableAbsPos2, @mk.currYpos)
            ELSE
                IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
                    thinBasic_VariableParse(lVariablePtr3, lVariableAbsPos3)

                    thinBasic_ChangeVariableNumberDirect (lVariablePtr1, lVariableAbsPos1, @mk.currXpos)
                    thinBasic_ChangeVariableNumberDirect (lVariablePtr2, lVariableAbsPos2, @mk.currYpos)
                    thinBasic_ChangeVariableNumberDirect (lVariablePtr3, lVariableAbsPos3, @mk.currZpos)
                END IF
                thinBasic_CheckCloseParens_Mandatory
            END IF
        END IF
      END IF
    END IF
  END SUB
  



  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  FUNCTION TBAI_PathCreate(xpos AS SINGLE, ypos AS SINGLE, zpos AS SINGLE, typ AS LONG, mode AS LONG, ID AS LONG) EXPORT AS DWORD
'#ELSE
  FUNCTION exec_TBAI_PathCreate() AS EXT
  '*****************************************************************
    LOCAL nParsed AS LONG
    
'#ENDIF
    LOCAL ret AS DWORD
    'ret = internal_PathCreate(xpos, ypos, zpos, typ, mode, id)
    ret = internal_PathCreate()
    FUNCTION = ret
  END FUNCTION


  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  SUB TBAI_PathAddWP(BYVAL wp AS tWaypoints PTR, xpos AS SINGLE, ypos AS SINGLE, zpos AS SINGLE, typ AS LONG) EXPORT
'#ELSE
  SUB exec_TBAI_PathAddWP()
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL expos, eypos, ezpos, etyp, eID AS EXT
    LOCAL xpos, ypos, zpos AS SINGLE
    LOCAL typ, id  AS LONG
    LOCAL wp AS DWORD
    LOCAL pwp AS EXT
    eid = 0
    etyp = 0
    nParsed = thinBasic_ParseXNumbers(4,5, pwp, expos, eypos, ezpos, eid)
    wp = pwp
    xpos = expos
    ypos = eypos
    zpos = ezpos
    typ = etyp
    id = eid
'#ENDIF
    internal_PathAddWP(wp, xpos, ypos, zpos, typ, ID)
  END SUB


  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  SUB TBAI_PathDelete(BYVAL pwp AS tWaypoints PTR) EXPORT
'#ELSE
  SUB exec_TBAI_PathDelete()
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL wp AS tWaypoints PTR
    LOCAL pwp AS EXT
    nParsed = thinBasic_ParseXNumbers(1,1, pwp)
    wp = pwp
'#ENDIF

    internal_MemFree(wp)
  END SUB


  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  SUB TBAI_MarkerDelete(BYVAL pmk AS tWPMarker PTR) EXPORT
'#ELSE
  SUB exec_TBAI_MarkerDelete()
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL mk AS tWPMarker PTR
    LOCAL pmk AS EXT
    nParsed = thinBasic_ParseXNumbers(1,1, pmk)
    mk = pmk
'#ENDIF

    internal_MemFree(mk)
  END SUB


  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  SUB TBAI_PathDeleteWP(BYVAL wp AS tWaypoints PTR, ID AS LONG) EXPORT
'#ELSE
  SUB exec_TBAI_PathDeleteWP()
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL wp AS tWaypoints PTR
    LOCAL pwp AS EXT
    LOCAL eid AS EXT
    LOCAL i,id, index, c AS LONG
    nParsed = thinBasic_ParseXNumbers(2,2, pwp, eid)
    wp = pwp
    id = eid
'#ENDIF
    c = @wp.nodecount
    
    'Determine the waypoint index
    FOR i = 1 TO c
        IF @wp.lNodes(i).ID = id THEN
            index = i
            EXIT FOR
        END IF
    NEXT

    IF index < c THEN
        FOR i = index TO c - 1
            @wp.lNodes(i).ID       = @wp.lNodes(i+1).ID
            @wp.lNodes(i).xpos     = @wp.lNodes(i+1).xpos
            @wp.lNodes(i).ypos     = @wp.lNodes(i+1).ypos
            @wp.lNodes(i).zpos     = @wp.lNodes(i+1).zpos
            '@wp.lNodes(i).itype    = @wp.lNodes(i+1).itype
            @wp.lNodes(i).funcname = @wp.lNodes(i+1).funcname
        NEXT
    END IF
    
    @wp.nodecount = @wp.nodecount - 1
    c = @wp.nodecount

    IF c >= 2 AND @wp.destindex = 2 THEN
        @wp.diffX = @wp.lNodes(2).XPos - @wp.lNodes(1).XPos
        @wp.diffY = @wp.lNodes(2).YPos - @wp.lNodes(1).YPos
        @wp.diffZ = @wp.lNodes(2).ZPos - @wp.lNodes(1).ZPos
    END IF
    
    IF c = 1 THEN
        @wp.diffX = 0.0
        @wp.diffY = 0.0
        @wp.diffZ = 0.0
        @wp.destindex = 1
    END IF
    
    IF c >= 2 THEN
        @wp.distance = SQR(@wp.diffX^2 + @wp.diffY^2 + @wp.diffZ^2)
        @wp.currAngleD = internal_ATN2( @wp.diffY, @wp.diffX )+90.0
        '@wp.currAngle = DegToRad(@wp.currAngleD)
    ELSE
        @wp.distance = 0.0
        '@wp.currAngle = 0.0
        @wp.currAngleD = 0.0
    END IF
  END SUB


  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  FUNCTION TBAI_PathCurrDist(BYVAL wp AS tWaypoints PTR) EXPORT AS SINGLE
'#ELSE
  FUNCTION exec_TBAI_PathCurrDist() AS EXT
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL wp AS tWaypoints PTR
    LOCAL pwp AS EXT
    nParsed = thinBasic_ParseXNumbers(1,1, pwp)
    wp = pwp
'#ENDIF  
      FUNCTION = @wp.distance
  END FUNCTION







  '*****************************************************************
  SUB exec_TBAI_PathCurrPos()
  '*****************************************************************
    LOCAL wp AS tWaypoints PTR
    LOCAL pwp AS EXT
    LOCAL lVariablePtr1     AS LONG
    LOCAL lVariableAbsPos1  AS LONG

    LOCAL lVariablePtr2     AS LONG
    LOCAL lVariableAbsPos2  AS LONG

    LOCAL lVariablePtr3     AS LONG
    LOCAL lVariableAbsPos3  AS LONG


    'nParsed = thinBasic_ParseXNumbers(3,4, pwp, x, y, z)
    IF thinBasic_CheckOpenParens_Mandatory THEN
      thinBasic_ParseNumber pwp
      wp = pwp
      IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
        thinBasic_VariableParse(lVariablePtr1, lVariableAbsPos1)
        IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
          thinBasic_VariableParse(lVariablePtr2, lVariableAbsPos2)
            IF thinBasic_CheckCloseParens_Optional THEN
                thinBasic_ChangeVariableNumberDirect (lVariablePtr1, lVariableAbsPos1, @wp.lNodes(@wp.destindex).XPos)
                thinBasic_ChangeVariableNumberDirect (lVariablePtr2, lVariableAbsPos2, @wp.lNodes(@wp.destindex).YPos)
            ELSE
                IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
                    thinBasic_VariableParse(lVariablePtr3, lVariableAbsPos3)

                    thinBasic_ChangeVariableNumberDirect (lVariablePtr1, lVariableAbsPos1, @wp.lNodes(@wp.destindex).XPos)
                    thinBasic_ChangeVariableNumberDirect (lVariablePtr2, lVariableAbsPos2, @wp.lNodes(@wp.destindex).YPos)
                    thinBasic_ChangeVariableNumberDirect (lVariablePtr3, lVariableAbsPos3, @wp.lNodes(@wp.destindex).ZPos)
                END IF
                thinBasic_CheckCloseParens_Mandatory
            END IF
        END IF
      END IF
    END IF
  END SUB
  
  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  FUNCTION TBAI_WPGetID(BYVAL wp AS tWaypoints PTR, index AS LONG) EXPORT AS LONG
'#ELSE
  FUNCTION exec_TBAI_WPGetID() AS EXT
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL wp AS tWaypoints PTR
    LOCAL pwp,ei AS EXT
    LOCAL i AS LONG
    nParsed = thinBasic_ParseXNumbers(2,2, pwp, ei)
    wp = pwp
    i = ei
'#ENDIF  
    FUNCTION = @wp.lNodes(i).ID
  END FUNCTION

  '*****************************************************************
  FUNCTION exec_TBAI_WPFindClosest() AS EXT
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL wp AS tWaypoints PTR
    LOCAL pwp,ex,ey,ez AS EXT
    LOCAL x,y,z,cdist, dist,diffx,diffy,diffz AS SINGLE
    LOCAL i,ci AS LONG
    
    ez = 0.0
    nParsed = thinBasic_ParseXNumbers(3,4, pwp, ex,ey,ez)
    wp = pwp
    x = ex
    y = ey
    z = ez
    cdist = 999999.9
    ci = -1
    FOR i = 1 TO @wp.nodecount
        diffX = x - @wp.lNodes(i).XPos
        diffY = y - @wp.lNodes(i).YPos
        diffZ = z - @wp.lNodes(i).ZPos
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
  FUNCTION internal_PathGetWPIndex(BYVAL wp AS tWaypoints PTR, lID AS LONG) AS LONG
  '*****************************************************************
    LOCAL index AS LONG
    LOCAL ret AS LONG
    ret = 0
    FOR index = 1 TO @wp.nodecount
      IF  @wp.lNodes(index).ID = lID THEN 
        ret = index
        EXIT FOR
      END IF
    NEXT
    FUNCTION = ret
  END FUNCTION

  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  FUNCTION TBAI_WPGetIndex(BYVAL g AS tGrid PTR, id AS LONG) EXPORT AS LONG
'#ELSE
  FUNCTION exec_TBAI_WPGetIndex() AS EXT
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL wp AS tWaypoints PTR
    LOCAL pwp, eID AS EXT
    LOCAL lid AS LONG
    nParsed = thinBasic_ParseXNumbers(2,2,pwp, eID)
    wp = pwp
    lid = eID
'#ENDIF

    FUNCTION = internal_PathGetWPIndex(wp,lid)
  END FUNCTION
  
  '*****************************************************************
  SUB exec_TBAI_WPGetPos()
  '*****************************************************************
    LOCAL wp AS tWaypoints PTR
    LOCAL pwp, ei AS EXT
    LOCAL i AS LONG
    LOCAL lVariablePtr1     AS LONG
    LOCAL lVariableAbsPos1  AS LONG

    LOCAL lVariablePtr2     AS LONG
    LOCAL lVariableAbsPos2  AS LONG

    LOCAL lVariablePtr3     AS LONG
    LOCAL lVariableAbsPos3  AS LONG


    'nParsed = thinBasic_ParseXNumbers(3,4, pwp, x, y, z)
    IF thinBasic_CheckOpenParens_Mandatory THEN
      thinBasic_ParseNumber pwp
      wp = pwp
      IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
          thinBasic_ParseNumber ei
          i = ei
          IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
            thinBasic_VariableParse(lVariablePtr1, lVariableAbsPos1)
            IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
              thinBasic_VariableParse(lVariablePtr2, lVariableAbsPos2)
                IF thinBasic_CheckCloseParens_Optional THEN
                    thinBasic_ChangeVariableNumberDirect (lVariablePtr1, lVariableAbsPos1, @wp.lNodes(i).XPos)
                    thinBasic_ChangeVariableNumberDirect (lVariablePtr2, lVariableAbsPos2, @wp.lNodes(i).YPos)
                ELSE
                    IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
                        thinBasic_VariableParse(lVariablePtr3, lVariableAbsPos3)
    
                        thinBasic_ChangeVariableNumberDirect (lVariablePtr1, lVariableAbsPos1, @wp.lNodes(i).XPos)
                        thinBasic_ChangeVariableNumberDirect (lVariablePtr2, lVariableAbsPos2, @wp.lNodes(i).YPos)
                        thinBasic_ChangeVariableNumberDirect (lVariablePtr3, lVariableAbsPos3, @wp.lNodes(i).ZPos)
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
'  FUNCTION TBAI_PathCurrID(BYVAL wp AS tWaypoints PTR) EXPORT AS LONG
'#ELSE
  FUNCTION exec_TBAI_PathCurrID() AS EXT
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL wp AS tWaypoints PTR
    LOCAL pwp AS EXT
    nParsed = thinBasic_ParseXNumbers(1,1, pwp)
    wp = pwp
'#ENDIF  
    FUNCTION = @wp.lNodes(@wp.destindex).ID
  END FUNCTION


  '*****************************************************************
  FUNCTION exec_TBAI_PathCurrIndex() AS EXT
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL wp AS tWaypoints PTR
    LOCAL pwp AS EXT
    nParsed = thinBasic_ParseXNumbers(1,1, pwp)
    wp = pwp
'#ENDIF  
    FUNCTION = @wp.destindex
  END FUNCTION


  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  FUNCTION TBAI_PathCurrAngle(BYVAL wp AS tWaypoints PTR) EXPORT AS SINGLE
'#ELSE
  FUNCTION exec_TBAI_PathCurrAngle() AS EXT
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL wp AS tWaypoints PTR
    LOCAL pwp AS EXT
    nParsed = thinBasic_ParseXNumbers(1,1, pwp)
    wp = pwp
'#ENDIF  
    FUNCTION = @wp.currAngleD
  END FUNCTION



  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  FUNCTION TBAI_PathWPCount(BYVAL wp AS tWaypoints PTR) EXPORT AS LONG
'#ELSE
  FUNCTION exec_TBAI_PathCountWP() AS EXT
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL wp AS tWaypoints PTR
    LOCAL pwp AS EXT
    nParsed = thinBasic_ParseXNumbers(1,1, pwp)
    wp = pwp
'#ENDIF  
    FUNCTION = @wp.NodeCount
  END FUNCTION


  '*****************************************************************
  SUB internal_PathNextWP(BYVAL wp AS tWaypoints PTR)
  '*****************************************************************
    LOCAL iPos  AS LONG
    LOCAL i AS LONG

    IF @wp.direction = 1 THEN
        @wp.sourceindex = @wp.destindex
        IF @wp.destindex < @wp.nodecount THEN
            @wp.destindex = @wp.destindex + @wp.direction
        ELSE
            @wp.destindex = 1
        END IF
    ELSE
        @wp.sourceindex = @wp.destindex
        IF @wp.destindex > 2 THEN
            @wp.destindex = @wp.destindex + @wp.direction
        ELSE
            @wp.destindex = @wp.nodecount
        END IF
    END IF
   
    @wp.diffX = @wp.lNodes(@wp.destindex).XPos - @wp.lNodes(@wp.sourceindex).XPos
    @wp.diffY = @wp.lNodes(@wp.destindex).YPos - @wp.lNodes(@wp.sourceindex).YPos
    @wp.diffZ = @wp.lNodes(@wp.destindex).ZPos - @wp.lNodes(@wp.sourceindex).ZPos
    
    @wp.distance = SQR(@wp.diffX^2 + @wp.diffY^2 + @wp.diffZ^2)

    @wp.currdist = 0.0
    '@wp.currAngle = internal_AngleBearingRadian(@wp.diffX,@wp.diffY)
    '@wp.currAngleD = @wp.currAngle / 0.0174533
    @wp.currAngleD = internal_ATN2( @wp.diffY, @wp.diffX )+90.0
    '@wp.currAngle = DegToRad(@wp.currAngleD)

  END SUB


  '*****************************************************************
  SUB internal_PathNextMK(BYVAL mk AS tWPMarker PTR)
  '*****************************************************************
    LOCAL iPos  AS LONG
    LOCAL i AS LONG
    LOCAL wp AS tWaypoints PTR
    LOCAL iDest, iSrc AS LONG
    'LOCAL msg AS STRING
    
    wp = @mk.PATH
    
    IF @mk.direction = %pmkForward THEN
        'Direction forward
        IF @mk.destindex < @wp.nodecount THEN
            @mk.sourceindex = @mk.destindex
            @mk.destindex = @mk.sourceindex + 1
        ELSE
            SELECT CASE @mk.MODE
                CASE %pmkLoop
                    @mk.sourceindex = 1
                    @mk.destindex = 2
                CASE %pmkBounce
                    @mk.sourceindex = @wp.nodecount
                    @mk.destindex = @mk.sourceindex -1
                    @mk.direction = %pmkBackwards
                CASE ELSE
                    '@mk.sourceindex = @wp.nodecount
                    @mk.final = %TRUE
                    EXIT SUB
            END SELECT
        END IF
    ELSE
        'Direction backward
        IF @mk.destindex > 1 THEN
            @mk.sourceindex = @mk.destindex
            @mk.destindex = @mk.sourceindex -1
        ELSE
            SELECT CASE @mk.MODE
                CASE %pmkLoop
                    @mk.sourceindex = @wp.nodecount
                    @mk.destindex = @mk.sourceindex -1
                CASE %pmkBounce
                    @mk.sourceindex = 1
                    @mk.destindex = @mk.sourceindex +1
                    @mk.direction = %pmkForward
                CASE ELSE
                    @mk.final = %TRUE
                    EXIT SUB
            END SELECT
        END IF
    END IF
    iDest = @mk.destIndex
    iSrc  = @mk.sourceIndex
   
    @mk.diffX = @wp.lNodes(@mk.destindex).XPos - @wp.lNodes(@mk.sourceindex).XPos
    @mk.diffY = @wp.lNodes(@mk.destindex).YPos - @wp.lNodes(@mk.sourceindex).YPos
    @mk.diffZ = @wp.lNodes(@mk.destindex).ZPos - @wp.lNodes(@mk.sourceindex).ZPos
    @mk.currXPos   = @wp.lNodes(iSrc).XPos
    @mk.currYPos   = @wp.lNodes(iSrc).YPos
    @mk.currZPos   = @wp.lNodes(iSrc).ZPos
    @mk.sourceXPos = @wp.lNodes(iSrc).XPos
    @mk.sourceYPos = @wp.lNodes(iSrc).YPos
    @mk.sourceZPos = @wp.lNodes(iSrc).ZPos
    @mk.destXPos   = @wp.lNodes(idest).XPos
    @mk.destYPos   = @wp.lNodes(idest).YPos
    @mk.destZPos   = @wp.lNodes(idest).ZPos
    
    @mk.distance = SQR(@mk.diffX^2 + @mk.diffY^2 + @mk.diffZ^2)

    @mk.currdist = 0.0
    @mk.currAngleD = internal_ATN2( @mk.diffY, @mk.diffX )+90.0
    '@mk.currAngle = DegToRad(@mk.currAngleD)
    
'    msg = "sourceIndex: "+ STR$(@mk.sourceIndex)+ $CRLF
'    msg = msg + "destIndex: "+ STR$(@mk.destIndex)+ $CRLF
'    msg = msg + "srcX: "+ STR$(@wp.lNodes(@mk.sourceIndex).XPos)+ $CRLF
'    msg = msg + "srcY: "+ STR$(@wp.lNodes(@mk.sourceIndex).YPos)+ $CRLF
'    msg = msg + "destX: "+ STR$(@wp.lNodes(@mk.destIndex).XPos)+ $CRLF
'    msg = msg + "destY: "+ STR$(@wp.lNodes(@mk.destIndex).YPos)+ $CRLF
'    msg = msg + "diffX: "+ STR$(@mk.diffX)+ $CRLF
'    msg = msg + "diffY: "+ STR$(@mk.diffY)+ $CRLF
'    msg = msg + "distance: "+ STR$(@mk.distance)+ $CRLF
'    msg = msg + "currDist: "+ STR$(@mk.currDist)+ $CRLF
'    msg = msg + "currXpos: "+ STR$(@mk.currXpos)+ $CRLF
'    msg = msg + "currYpos: "+ STR$(@mk.currYpos)+ $CRLF
'    msg = msg + "angle: "+ STR$(@mk.currangleD)+ $CRLF
    'MSGBOX(msg)

  END SUB


  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  SUB TBAI_PathNextWP(BYVAL wp AS tWaypoints PTR) EXPORT
'#ELSE
  SUB exec_TBAI_PathNextWP()
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL wp AS tWaypoints PTR
    LOCAL pwp AS EXT
    nParsed = thinBasic_ParseXNumbers(1,1, pwp)
    wp = pwp
'#ENDIF  
    internal_PathNextWP(wp)

  END SUB


  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  SUB TBAI_PathSetWPHandler(BYVAL wp AS tWaypoints PTR, ID AS LONG, funcname AS STRING) EXPORT
'#ELSE
  SUB exec_TBAI_WPSetHandler()
  '*****************************************************************
    LOCAL id AS EXT
    LOCAL wp AS tWaypoints PTR
    LOCAL pwp AS EXT
    LOCAL funcname AS STRING

    IF thinBasic_CheckOpenParens THEN
      thinBasic_ParseNumber pwp
      IF thinBasic_CheckComma THEN
        thinBasic_ParseNumber id
        IF thinBasic_CheckComma THEN
          thinBasic_ParseString funcname
          IF thinBasic_CheckCloseParens THEN

            wp = pwp

          END IF
        END IF
      END IF
    END IF

'#ENDIF
    LOCAL index AS LONG
    LOCAL i,c AS LONG
    c = @wp.nodecount
    'Determine the waypoint index
    FOR i = 1 TO c
        IF @wp.lNodes(i).ID = id THEN
            index = i
            EXIT FOR
        END IF
    NEXT

    IF index > 0 THEN
        @wp.lNodes(index).funcName = TRIM$(funcname)
    END IF
  END SUB


  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  FUNCTION TBAI_MarkerUpdate(BYVAL mk AS tMarker PTR, speed AS SINGLE) EXPORT AS SINGLE
'#ELSE
  FUNCTION exec_TBAI_MarkerUpdate() AS EXT
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL speed  AS EXT
    LOCAL mk AS tWPMarker PTR
    LOCAL pmk AS EXT
    LOCAL wp AS tWaypoints PTR

    speed = 0.0
    nParsed = thinBasic_ParseXNumbers(1,2, pmk, speed)
    mk = pmk
    wp = @mk.path

'#ENDIF
    IF @mk.final = %TRUE THEN 
        FUNCTION = 0
        EXIT FUNCTION
    END IF
    LOCAL diffdist AS SINGLE
    LOCAL retext AS EXT

    IF speed = 0.0 THEN speed = @mk.speed
    @mk.currdist = @mk.currdist + speed
    IF @mk.currdist < @mk.distance THEN
        @mk.currXpos = @mk.sourceXpos + @mk.DiffX / @mk.distance * @mk.currdist
        @mk.currYpos = @mk.sourceYpos + @mk.DiffY / @mk.distance * @mk.currdist
        @mk.currZpos = @mk.sourceZpos + @mk.DiffZ / @mk.distance * @mk.currdist
    ELSE
        diffdist = @mk.currdist - @mk.distance
        thinBasic_FunctionSimpleCall(@wp.lNodes(@mk.destIndex).funcName, VARPTR (retExt) )

        internal_PathNextmk(mk)
        IF @mk.final = %FALSE THEN
            @mk.currdist = diffdist
            @mk.currXpos = @mk.sourceXpos + @mk.DiffX / @mk.distance * @mk.currdist
            @mk.currYpos = @mk.sourceYpos + @mk.DiffY / @mk.distance * @mk.currdist
            @mk.currZpos = @mk.sourceZpos + @mk.DiffZ / @mk.distance * @mk.currdist
        END IF
    END IF

    FUNCTION = @mk.currdist
  END FUNCTION


'  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  SUB TBAI_PathTranslate(BYVAL wp AS tWaypoints PTR, xdiff AS SINGLE, ydiff AS SINGLE, Zdiff AS SINGLE) EXPORT
'#ELSE
'  SUB exec_TBAI_PathTranslate()
'  '*****************************************************************
'    LOCAL nParsed AS LONG
'    LOCAL xdiff, ydiff, zdiff AS EXT
'    LOCAL wp AS tWaypoints PTR
'    LOCAL pwp AS EXT
'    nParsed = thinBasic_ParseXNumbers(3,3, pwp, xdiff, ydiff, zdiff)
'    wp = pwp
'#ENDIF
'    LOCAL npo AS LONG
'
'    @wp.currXpos = @wp.currXpos + xdiff
'    @wp.currYpos = @wp.currYpos + ydiff
'    @wp.currZpos = @wp.currZpos + zdiff
'    @wp.sourceXpos = @wp.sourceXpos + xdiff
'    @wp.sourceYpos = @wp.sourceYpos + ydiff
'    @wp.sourceZpos = @wp.sourceZpos + zdiff
'    @wp.destXpos = @wp.destXpos + xdiff
'    @wp.destYpos = @wp.destYpos + ydiff
'    @wp.destZpos = @wp.destZpos + zdiff
'
'    FOR npo = 1 TO @wp.nodecount
'        @wp.lNodes(npo).xpos = @wp.lNodes(npo).xpos + xdiff
'        @wp.lNodes(npo).ypos = @wp.lNodes(npo).ypos + ydiff
'        @wp.lNodes(npo).zpos = @wp.lNodes(npo).zpos + zdiff
'    NEXT
'  END SUB
'
'
'  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  SUB TBAI_PathTurn(BYVAL wp AS tWaypoints PTR, angle AS DOUBLE) EXPORT
'#ELSE
'  SUB exec_TBAI_PathTurn()
'  '*****************************************************************
'    LOCAL nParsed AS LONG
'    LOCAL angle AS EXT
'    LOCAL wp AS tWaypoints PTR
'    LOCAL pwp AS EXT
'    nParsed = thinBasic_ParseXNumbers(2,2, pwp, angle)
'    wp = pwp
'#ENDIF
'  '*****************************************************************
'    LOCAL count AS LONG
'    LOCAL diffx,diffy AS DOUBLE
'    LOCAL rotatedX, rotatedY AS DOUBLE
'    LOCAL oldX, oldY AS DOUBLE
'    LOCAL newX, newY AS LONG
'    LOCAL rotatedXd, rotatedYd AS DOUBLE
'    LOCAL oldXd, oldYd AS DOUBLE
'    LOCAL newXd, newYd AS DOUBLE
'    LOCAL radians,c,s AS DOUBLE
'    LOCAL angleD AS DOUBLE
'
'    LOCAL npo AS LONG
'
'    count = 0
'    IF wp = 0 THEN RETURN
'    'radians = 6.283185308 / (360.0 / angleD)
'    @wp.pathangle = @wp.pathangle + angle
'    @wp.pathangleD = @wp.pathangle / 0.0174533
'    radians = @wp.pathangle
'    c = COS(radians)
'    s = SIN(radians)
'    IF @wp.nodecount <= 1 THEN RETURN
'
'    FOR npo = 1 TO @wp.nodecount
'        count = count + 1
'        IF count = 1 THEN
'            diffx = @wp.lNodes(npo).xpos
'            diffy = @wp.lNodes(npo).ypos
'        ELSE
'            oldx = @wp.lNodes(npo).xpos - diffx
'            oldy = @wp.lNodes(npo).ypos - diffy
'            rotatedX = oldx * c - oldy * s
'            rotatedY = oldy * c + oldx * s
'            newX = rotatedX + diffx
'            newY = rotatedY + diffy
'
'        END IF
'    NEXT
'    'update source position
'    oldx = @wp.sourceXpos - diffx
'    oldy = @wp.sourceYpos - diffy
'    rotatedX = oldx * c - oldy * s
'    rotatedY = oldy * c + oldx * s
'    newX = rotatedX + diffx
'    newY = rotatedY + diffy
'    @wp.sourceXpos = newX
'    @wp.sourceYpos = newY
'
'    'update dest position
'    oldx = @wp.destXpos - diffx
'    oldy = @wp.destYpos - diffy
'    rotatedX = oldx * c - oldy * s
'    rotatedY = oldy * c + oldx * s
'    newX = rotatedX + diffx
'    newY = rotatedY + diffy
'    @wp.destXpos = newX
'    @wp.destYpos = newY
'
'    @wp.diffX = @wp.destXPos - @wp.sourceXpos
'    @wp.diffY = @wp.destYPos - @wp.sourceYpos
'
'    @wp.currXpos = @wp.sourceXpos + @wp.DiffX / @wp.distance * @wp.currdist
'    @wp.currYpos = @wp.sourceYpos + @wp.DiffY / @wp.distance * @wp.currdist
'    @wp.currAngle = internal_AngleBearingRadian(@wp.diffX, @wp.diffY)
'    @wp.currAngleD = @wp.currAngle / 0.0174533
'
'  END SUB
'
'  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  SUB TBAI_PathTurnD(BYVAL wp AS tWaypoints PTR, angleD AS DOUBLE) EXPORT
'#ELSE
'  SUB exec_TBAI_PathTurnD()
'  '*****************************************************************
'    LOCAL nParsed AS LONG
'    LOCAL angleD AS EXT
'    LOCAL wp AS tWaypoints PTR
'    LOCAL pwp AS EXT
'    nParsed = thinBasic_ParseXNumbers(2,2, pwp, angleD)
'    wp = pwp
'#ENDIF
'    LOCAL count AS LONG
'    LOCAL diffx,diffy AS DOUBLE
'    LOCAL rotatedX, rotatedY AS DOUBLE
'    LOCAL oldX, oldY AS DOUBLE
'    LOCAL newX, newY AS LONG
'    LOCAL rotatedXd, rotatedYd AS DOUBLE
'    LOCAL oldXd, oldYd AS DOUBLE
'    LOCAL radians,c,s AS DOUBLE
'    LOCAL angle AS DOUBLE
'
'    LOCAL npo AS LONG
'
'
'    count = 0
'    IF wp = 0 THEN RETURN
'    @wp.pathangleD = @wp.pathangleD + angleD
'    @wp.pathangle = @wp.pathangleD * 0.0174533
'    radians = @wp.pathangle
'
'    c = COS(radians)
'    s = SIN(radians)
'    IF @wp.nodecount <= 1 THEN RETURN
'
'    FOR npo = 1 TO @wp.nodecount
'        count = count + 1
'        IF count = 1 THEN
'            diffx = @wp.lNodes(npo).xpos
'            diffy = @wp.lNodes(npo).ypos
'        ELSE
'            oldx = @wp.lNodes(npo).xpos - diffx
'            oldy = @wp.lNodes(npo).ypos - diffy
'            rotatedX = oldx * c - oldy * s
'            rotatedY = oldy * c + oldx * s
'            newX = rotatedX + diffx
'            newY = rotatedY + diffy
'            IF @wp.lNodes(npo).id = @wp.sourceid THEN
'                @wp.sourcexpos = newX
'                @wp.sourceYpos = newY
'            END IF
'            IF @wp.lNodes(npo).id = @wp.destid THEN
'                @wp.destxpos = newX
'                @wp.destYpos = newY
'            END IF
'        END IF
'    NEXT
'
'
'    @wp.diffX = @wp.destXPos - @wp.sourceXpos
'    @wp.diffY = @wp.destYPos - @wp.sourceYpos
'
'    @wp.currXpos = @wp.sourceXpos + @wp.DiffX / @wp.distance * @wp.currdist
'    @wp.currYpos = @wp.sourceYpos + @wp.DiffY / @wp.distance * @wp.currdist
'    @wp.currAngle = internal_AngleBearingRadian(@wp.diffX, @wp.diffY)
'    @wp.currAngleD = @wp.currAngle / 0.0174533
'
'
'  END SUB
'
'
'  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  SUB TBAI_PathRotate(BYVAL wp AS tWaypoints PTR, angle AS DOUBLE) EXPORT
'#ELSE
'  SUB exec_TBAI_PathRotate()
'  '*****************************************************************
'    LOCAL nParsed AS LONG
'    LOCAL angle AS EXT
'    LOCAL wp AS tWaypoints PTR
'    LOCAL pwp AS EXT
'    nParsed = thinBasic_ParseXNumbers(2,2, pwp, angle)
'    wp = pwp
'#ENDIF
'    LOCAL count AS LONG
'    LOCAL diffx,diffy AS DOUBLE
'    LOCAL rotatedX, rotatedY AS DOUBLE
'    LOCAL oldX, oldY AS DOUBLE
'    LOCAL newX, newY AS LONG
'    LOCAL rotatedXd, rotatedYd AS DOUBLE
'    LOCAL oldXd, oldYd AS DOUBLE
'    LOCAL newXd, newYd AS DOUBLE
'    LOCAL radians,c,s AS DOUBLE
'    LOCAL angleD AS DOUBLE
'
'    LOCAL npo AS LONG
'
'    count = 0
'    'radians = 6.283185308 / (360.0 / angleD)
'    @wp.pathangle = angle
'    @wp.pathangleD = @wp.pathangle / 0.0174533
'    radians = @wp.pathangle
'    c = COS(radians)
'    s = SIN(radians)
'    IF @wp.nodecount <= 1 THEN RETURN
'
'    FOR npo = 1 TO @wp.nodecount
'        count = count + 1
'        IF count = 1 THEN
'            diffx = @wp.lNodes(npo).xpos
'            diffy = @wp.lNodes(npo).ypos
'        ELSE
'            oldx = @wp.lNodes(npo).xpos - diffx
'            oldy = @wp.lNodes(npo).ypos - diffy
'            rotatedX = oldx * c - oldy * s
'            rotatedY = oldy * c + oldx * s
'            newX = rotatedX + diffx
'            newY = rotatedY + diffy
'
'        END IF
'    NEXT
'    'update source position
'    oldx = @wp.sourceXpos - diffx
'    oldy = @wp.sourceYpos - diffy
'    rotatedX = oldx * c - oldy * s
'    rotatedY = oldy * c + oldx * s
'    newX = rotatedX + diffx
'    newY = rotatedY + diffy
'    @wp.sourceXpos = newX
'    @wp.sourceYpos = newY
'
'    'update dest position
'    oldx = @wp.destXpos - diffx
'    oldy = @wp.destYpos - diffy
'    rotatedX = oldx * c - oldy * s
'    rotatedY = oldy * c + oldx * s
'    newX = rotatedX + diffx
'    newY = rotatedY + diffy
'    @wp.destXpos = newX
'    @wp.destYpos = newY
'
'    @wp.diffX = @wp.destXPos - @wp.sourceXpos
'    @wp.diffY = @wp.destYPos - @wp.sourceYpos
'
'    @wp.currXpos = @wp.sourceXpos + @wp.DiffX / @wp.distance * @wp.currdist
'    @wp.currYpos = @wp.sourceYpos + @wp.DiffY / @wp.distance * @wp.currdist
'    @wp.currAngle = internal_AngleBearingRadian(@wp.diffX, @wp.diffY)
'    @wp.currAngleD = @wp.currAngle / 0.0174533
'
'
'  END SUB
'
'  '*****************************************************************
'#IF %WINDOWSDLL = 1 
'  SUB TBAI_PathRotateD(BYVAL wp AS tWaypoints PTR, angleD AS DOUBLE) EXPORT
'#ELSE
'  SUB exec_TBAI_PathRotateD()
'  '*****************************************************************
'    LOCAL nParsed AS LONG
'    LOCAL angleD AS EXT
'    LOCAL wp AS tWaypoints PTR
'    LOCAL pwp AS EXT
'    nParsed = thinBasic_ParseXNumbers(2,2, pwp, angleD)
'    wp = pwp
'#ENDIF
'  '*****************************************************************
'    LOCAL count AS LONG
'    LOCAL diffx,diffy AS DOUBLE
'    LOCAL rotatedX, rotatedY AS DOUBLE
'    LOCAL oldX, oldY AS DOUBLE
'    LOCAL newX, newY AS LONG
'    LOCAL rotatedXd, rotatedYd AS DOUBLE
'    LOCAL oldXd, oldYd AS DOUBLE
'    LOCAL newXd, newYd AS DOUBLE
'    LOCAL radians,c,s AS DOUBLE
'    LOCAL angle AS DOUBLE
'
'    LOCAL npo AS LONG
'
'    count = 0
'    IF wp = 0 THEN RETURN
'    @wp.pathangleD = angleD
'    @wp.pathangle = @wp.pathangleD * 0.0174533
'    radians = @wp.pathangle
'
'    c = COS(radians)
'    s = SIN(radians)
'    IF @wp.nodecount <= 1 THEN RETURN
'
'    FOR npo = 1 TO @wp.nodecount
'        count = count + 1
'        IF count = 1 THEN
'            diffx = @wp.lNodes(npo).xpos
'            diffy = @wp.lNodes(npo).ypos
'        ELSE
'            oldx = @wp.lNodes(npo).xpos - diffx
'            oldy = @wp.lNodes(npo).ypos - diffy
'            rotatedX = oldx * c - oldy * s
'            rotatedY = oldy * c + oldx * s
'            newX = rotatedX + diffx
'            newY = rotatedY + diffy
'            IF @wp.lNodes(npo).id = @wp.sourceid THEN
'                @wp.sourcexpos = newX
'                @wp.sourceYpos = newY
'            END IF
'            IF @wp.lNodes(npo).id = @wp.destid THEN
'                @wp.destxpos = newX
'                @wp.destYpos = newY
'            END IF
'        END IF
'    NEXT
'
'    @wp.diffX = @wp.destXPos - @wp.sourceXpos
'    @wp.diffY = @wp.destYPos - @wp.sourceYpos
'
'    @wp.currXpos = @wp.sourceXpos + @wp.DiffX / @wp.distance * @wp.currdist
'    @wp.currYpos = @wp.sourceYpos + @wp.DiffY / @wp.distance * @wp.currdist
'    @wp.currAngle = internal_AngleBearingRadian(@wp.diffX, @wp.diffY)
'    @wp.currAngleD = @wp.currAngle / 0.0174533
'
'  END SUB
