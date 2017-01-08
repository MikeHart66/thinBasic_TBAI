  #IF 0
  =============================================================================
   Program NAME: TBAI_Lib.bas
   Author      : Michael Hartlef
   Email       : mike@fantomgl.com
   Version     : 1.01 (yet TO be released)
   Description : Dll To manage AI (A* pathfinding, waypoints)
  =============================================================================
  'COPYRIGHT AND PERMISSION NOTICE
  '============================================================================
  Copyright (c) 2009, Michael Hartlef

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

  %WINDOWSDLL = 0         ' -- 0 means for thinBASIC


  #OPTIMIZE SPEED
  #IF %WINDOWSDLL = 1
    #COMPILE DLL "TBAI.dll"
  #ELSE
    #COMPILE DLL "thinbasic_TBAI.dll"
  #ENDIF
  #REGISTER NONE
  '#DIM ALL
  %USEMACROS = 1

  MACRO ABS_LNG(P1)
    MACROTEMP POS
    ! BT P1,31
    ! JNC Pos
    ! NEG P1
    POS:
  END MACRO

  '---Resource file.
  #RESOURCE "TBAI_Lib.PBR"
  #INCLUDE "win32api.inc"

'----------------------------------------------------
'Function declarations  
  FUNCTION internal_MemFree(BYREF pMem AS DWORD) AS LONG
    IF pMem THEN
      FUNCTION = HeapFree( GetProcessHeap(), 0, BYVAL pMem)
      pMem = 0
    END IF
  END FUNCTION

  FUNCTION internal_MemAlloc(BYVAL nSize AS LONG) AS DWORD
    IF nSize > 0 THEN
      FUNCTION = HeapAlloc( GetProcessHeap(), %HEAP_ZERO_MEMORY, nSize)
    END IF
  END FUNCTION


  '---Every used defined thinBasic module must include this file
'#IF %WINDOWSDLL = 1 
'#ELSE
  #INCLUDE "thinCore.inc"
'#ENDIF
  #INCLUDE "TBAI_waypoints.inc"
  #INCLUDE "TBAI_astar.inc"


'----------------------------------------------------
'equates

  '%ID_KEYB  = 101


'*---------------------------------------------------------*
'*                   User defined types                    *
'*---------------------------------------------------------*

'*---------------------------------------------------------*
'*                      Declarations                       *
'*---------------------------------------------------------*

'----------------------------------------------------

  '*****************************************************************
  ' Module functions
  '*****************************************************************


#IF %WINDOWSDLL = 1 
#ELSE
  '----------------------------------------------------------------------------
  FUNCTION LoadLocalSymbols ALIAS "LoadLocalSymbols" (OPTIONAL BYVAL sPath AS STRING) EXPORT AS LONG
  ' This function is automatically called by thinCore whenever this DLL is loaded.
  ' This function MUST be present in every external DLL you want to use
  ' with thinBasic
  ' Use this function to initialize every variable you need and for loading the
  ' new symbol (read Keyword) you have created.
  '----------------------------------------------------------------------------

    '---
    'Add here Initialization code if needed
    '---


    '---
    'Here define/add new thinBasic keywords
    '---

    thinBasic_LoadSymbol "TBAI_GridAddNode",       %thinBasic_ReturnNumber,    CODEPTR(exec_TBAI_GridAddNode),        %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_GridCountNodes",    %thinBasic_ReturnNumber,    CODEPTR(exec_TBAI_GridCountNodes),     %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_GridCreate",        %thinBasic_ReturnNumber,    CODEPTR(exec_TBAI_GridCreate),         %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_GridDelete",        %thinBasic_ReturnNone,      CODEPTR(exec_TBAI_GridDelete),         %thinBasic_ForceOverWrite'    
    thinBasic_LoadSymbol "TBAI_GridDeleteAllNodes",%thinBasic_ReturnNone,      CODEPTR(exec_TBAI_GridDeleteAllNodes), %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_GridFindPath",      %thinBasic_ReturnNumber,    CODEPTR(exec_TBAI_GridFindPath),       %thinBasic_ForceOverWrite

    thinBasic_LoadSymbol "TBAI_NodeConnect",       %thinBasic_ReturnNumber,    CODEPTR(exec_TBAI_NodeConnect),        %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_NodeCountChildren", %thinBasic_ReturnNumber,    CODEPTR(exec_TBAI_NodeCountChildren),  %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_NodeGetBlock",      %thinBasic_ReturnNumber,    CODEPTR(exec_TBAI_NodeGetBlock),       %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_NodeGetChild",      %thinBasic_ReturnNumber,    CODEPTR(exec_TBAI_NodeGetchild),       %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_NodeGetID",         %thinBasic_ReturnNumber,    CODEPTR(exec_TBAI_NodeGetID),          %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_NodeGetIndex",      %thinBasic_ReturnNumber,    CODEPTR(exec_TBAI_NodeGetIndex),       %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_NodeGetPos",        %thinBasic_ReturnNone,      CODEPTR(exec_TBAI_NodeGetPos),         %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_NodeFindClosest",   %thinBasic_ReturnNumber,    CODEPTR(exec_TBAI_NodeFindClosest),    %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_NodeSetBlock",      %thinBasic_ReturnNone,      CODEPTR(exec_TBAI_NodeSetBlock),       %thinBasic_ForceOverWrite

    thinBasic_LoadSymbol "TBAI_PathCountWP",       %thinBasic_ReturnNumber,    CODEPTR(exec_TBAI_PathCountWP),        %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_PathCreate",        %thinBasic_ReturnNumber,    CODEPTR(exec_TBAI_PathCreate),         %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_PathDelete",        %thinBasic_ReturnNone,      CODEPTR(exec_TBAI_PathDelete),         %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_PathAddWP",         %thinBasic_ReturnNone,      CODEPTR(exec_TBAI_PathAddWP),          %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_PathDeleteWP",      %thinBasic_ReturnNone,      CODEPTR(exec_TBAI_PathDeleteWP),       %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_PathCurrDist",      %thinBasic_ReturnNumber,    CODEPTR(exec_TBAI_PathCurrDist),       %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_PathCurrAngle",     %thinBasic_ReturnNumber,    CODEPTR(exec_TBAI_PathCurrAngle),      %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_PathCurrID",        %thinBasic_ReturnNumber,    CODEPTR(exec_TBAI_PathCurrID),         %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_PathCurrIndex",     %thinBasic_ReturnNumber,    CODEPTR(exec_TBAI_PathCurrIndex),      %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_PathCurrPos",       %thinBasic_ReturnNone,      CODEPTR(exec_TBAI_PathCurrPos),        %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_PathNextWP",        %thinBasic_ReturnNone,      CODEPTR(exec_TBAI_PathNextWP),         %thinBasic_ForceOverWrite
    
    thinBasic_LoadSymbol "TBAI_MarkerCreate",      %thinBasic_ReturnNumber,    CODEPTR(exec_TBAI_MarkerCreate),       %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_MarkerCurrAngle",   %thinBasic_ReturnNumber,    CODEPTR(exec_TBAI_MarkerCurrAngle),    %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_MarkerCurrDist",    %thinBasic_ReturnNumber,    CODEPTR(exec_TBAI_MarkerCurrDist),     %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_MarkerCurrPos",     %thinBasic_ReturnNone,      CODEPTR(exec_TBAI_MarkerCurrPos),      %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_MarkerDelete",      %thinBasic_ReturnNone,      CODEPTR(exec_TBAI_MarkerDelete),       %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_MarkerFinished",    %thinBasic_ReturnNumber,    CODEPTR(exec_TBAI_MarkerFinished),     %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_MarkerUpdate",      %thinBasic_ReturnNumber,    CODEPTR(exec_TBAI_MarkerUpdate),       %thinBasic_ForceOverWrite

    thinBasic_LoadSymbol "TBAI_WPGetID",           %thinBasic_ReturnNumber,    CODEPTR(exec_TBAI_WPGetID),            %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_WPGetIndex",        %thinBasic_ReturnNumber,    CODEPTR(exec_TBAI_WPGetIndex),         %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_WPGetPos",          %thinBasic_ReturnNone,      CODEPTR(exec_TBAI_WPGetPos),           %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_WPFindClosest",     %thinBasic_ReturnNumber,    CODEPTR(exec_TBAI_WPFindClosest),      %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBAI_WPSetHandler",      %thinBasic_ReturnNone,      CODEPTR(exec_TBAI_WPSetHandler),       %thinBasic_ForceOverWrite

    '---
    'Here define/add new thinBasic equates
    '---
    thinBasic_AddEquate   "%TBAI_FORWARD"               , "" , %pmkForward
    thinBasic_AddEquate   "%TBAI_BACKWARDS"             , "" , %pmkBackwards
    thinBasic_AddEquate   "%TBAI_STOP"                  , "" , %pmkStop
    thinBasic_AddEquate   "%TBAI_LOOP"                  , "" , %pmkLoop
    thinBasic_AddEquate   "%TBAI_BOUNCE"                , "" , %pmkBounce

  END FUNCTION

  '----------------------------------------------------------------------------
  FUNCTION UnLoadLocalSymbols ALIAS "UnLoadLocalSymbols" () EXPORT AS LONG
  ' This function is automatically called by thinCore whenever this DLL is unloaded.
  ' This function CAN be present but it is not necessary. If present, this function
  ' will be executed by thinBasic core when module will be released.
  ' Use this function to perform uninitialize process, if needed.
  '----------------------------------------------------------------------------
    '---
    'Add here DeInitialization code if needed
    '---
    FUNCTION = 0&
  END FUNCTION

#ENDIF


  FUNCTION LIBMAIN ALIAS "LibMain" (BYVAL hInstance   AS LONG, _
                                    BYVAL fwdReason   AS LONG, _
                                    BYVAL lpvReserved AS LONG) EXPORT AS LONG
    SELECT CASE fwdReason
      CASE %DLL_PROCESS_ATTACH
        FUNCTION = 1
        EXIT FUNCTION
      CASE %DLL_PROCESS_DETACH

        FUNCTION = 1
        EXIT FUNCTION
      CASE %DLL_THREAD_ATTACH

        FUNCTION = 1
        EXIT FUNCTION
      CASE %DLL_THREAD_DETACH

        FUNCTION = 1
        EXIT FUNCTION
    END SELECT

  END FUNCTION
