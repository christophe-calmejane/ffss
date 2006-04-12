// FFSSD_Service
//
// v1.0
// KindMan 20/08/2002
// v1.1
// Christophe Calmejane 21/08/2002

#include <ffss.h>

#define FFSS_REGISTRY_PATH_PROCESSID FFSS_LM_REGISTRY_PATH "Server\\ProcessId"
#define FFSS_REGISTRY_PATH_SERVERDIR FFSS_LM_REGISTRY_PATH "Server\\ServerDirectory"

#define SLEEP_TIME 100
#define MAX_WAIT 50

#include <windows.h>
#include <string.h>
#include <stdio.h>

SERVICE_STATUS          FFSSDaemonStatus; 
SERVICE_STATUS_HANDLE   FFSSDaemonStatusHandle; 

VOID SvcDebugOut(LPSTR String, DWORD Status);
VOID WINAPI FFSSDaemonStart (DWORD argc, char **argv); 
VOID WINAPI FFSSDaemonCtrlHandler (DWORD opcode); 
DWORD FFSSDaemonInitialization (DWORD argc, LPTSTR *argv,DWORD *specificError); 

// main : Starts service

void main()
{ 
    SERVICE_TABLE_ENTRY   DispatchTable[] = { 
        { "FFSSDaemon", FFSSDaemonStart      }, 
        { NULL,              NULL          } 
    }; 

    if (!StartServiceCtrlDispatcher( DispatchTable)) 
    { 
        SvcDebugOut(" [FFSSDaemon] StartServiceCtrlDispatcher error = %d\n", GetLastError()); 
    } 
} 
 
VOID SvcDebugOut(LPSTR String, DWORD Status) 
{ 
    CHAR  Buffer[1024]; 
    if (strlen(String) < 1000) 
    { 
        sprintf(Buffer, String, Status); 
        OutputDebugStringA(Buffer); 
    } 
} 

// Starts daemon

VOID WINAPI FFSSDaemonStart (DWORD argc, char **argv) 
{ 
    DWORD status; 
    DWORD specificError; 
 
    FFSSDaemonStatus.dwServiceType        = SERVICE_WIN32; 
    FFSSDaemonStatus.dwCurrentState       = SERVICE_START_PENDING; 
    FFSSDaemonStatus.dwControlsAccepted   = SERVICE_ACCEPT_STOP; 
    FFSSDaemonStatus.dwWin32ExitCode      = 0; 
    FFSSDaemonStatus.dwServiceSpecificExitCode = 0; 
    FFSSDaemonStatus.dwCheckPoint         = 0; 
    FFSSDaemonStatus.dwWaitHint           = 0; 
 
    FFSSDaemonStatusHandle = RegisterServiceCtrlHandler("FFSSDaemon", FFSSDaemonCtrlHandler); 
 
    if (FFSSDaemonStatusHandle == (SERVICE_STATUS_HANDLE)0) 
    { 
        SvcDebugOut(" [FFSSDaemon] RegisterServiceCtrlHandler failed %d\n", GetLastError()); 
        return; 
    } 
 
    // Initialization code goes here. 
    status = FFSSDaemonInitialization(argc,argv, &specificError); 
 
    // Handle error condition 
    if (status != NO_ERROR) 
    { 
        FFSSDaemonStatus.dwCurrentState       = SERVICE_STOPPED; 
        FFSSDaemonStatus.dwCheckPoint         = 0; 
        FFSSDaemonStatus.dwWaitHint           = 0; 
        FFSSDaemonStatus.dwWin32ExitCode      = status; 
        FFSSDaemonStatus.dwServiceSpecificExitCode = specificError; 
 
        SetServiceStatus (FFSSDaemonStatusHandle, &FFSSDaemonStatus); 
        return; 
    } 
 
    // Initialization complete - report running status. 
    FFSSDaemonStatus.dwCurrentState       = SERVICE_RUNNING; 
    FFSSDaemonStatus.dwCheckPoint         = 0; 
    FFSSDaemonStatus.dwWaitHint           = 0; 
 
    if (!SetServiceStatus (FFSSDaemonStatusHandle, &FFSSDaemonStatus)) 
    { 
        status = GetLastError(); 
        SvcDebugOut(" [FFSSDaemon] SetServiceStatus error %ld\n",status); 
    } 
 
    // This is where the service does its work. 
    SvcDebugOut(" [FFSSDaemon] Returning the Main Thread \n",0); 
 
    return; 
} 
 
// Stub initialization function. 
DWORD FFSSDaemonInitialization(DWORD   argc, LPTSTR  *argv, DWORD *specificError) 
{ 
	STARTUPINFO startupinfo;
	PROCESS_INFORMATION processinfo;
	char buffer[1024];
	char buffer2[1024];

	startupinfo.cb = sizeof(startupinfo);
	startupinfo.cbReserved2 = NULL;
	startupinfo.lpReserved = NULL;
	startupinfo.lpDesktop = NULL;
	startupinfo.lpReserved2 = NULL;
	startupinfo.lpTitle = NULL;	

	// Get path to ffssd
    SU_RB_GetStrValue(FFSS_REGISTRY_PATH_SERVERDIR,buffer,sizeof(buffer),"");
    snprintf(buffer2,sizeof(buffer2),"%s\\ffssd.exe",buffer);

	// Launch ffssd.exe

	if (CreateProcess(NULL, buffer2, NULL, NULL, true, CREATE_NEW_CONSOLE | NORMAL_PRIORITY_CLASS,NULL, buffer, &startupinfo, &processinfo))
	{
		return NO_ERROR;
	}
	else
	{
		*specificError= GetLastError();
		return ERROR_SERVICE_SPECIFIC_ERROR;
	}

	return NO_ERROR;
} 

void StopService()
{
	DWORD status; 

	FFSSDaemonStatus.dwWin32ExitCode = 0; 
	FFSSDaemonStatus.dwCurrentState  = SERVICE_STOPPED; 
	FFSSDaemonStatus.dwCheckPoint    = 0; 
	FFSSDaemonStatus.dwWaitHint      = 0; 
 
	if (!SetServiceStatus (FFSSDaemonStatusHandle,&FFSSDaemonStatus))
	{ 
		status = GetLastError(); 
		SvcDebugOut(" [FFSSDaemon] SetServiceStatus error %ld\n",status); 
	} 
 
	SvcDebugOut(" [FFSSDaemon] Leaving FFSSDaemon \n",0); 
}

VOID WINAPI FFSSDaemonCtrlHandler (DWORD opcode)
{

	DWORD status,res; 
	int pid=0;
	DWORD spid=sizeof(pid);
	HANDLE h;
    int nb=0;

    switch(opcode) 
    { 
        case SERVICE_CONTROL_PAUSE: 
        // Do whatever it takes to pause here. 
            FFSSDaemonStatus.dwCurrentState = SERVICE_PAUSED; 
            break; 
 
        case SERVICE_CONTROL_CONTINUE: 
        // Do whatever it takes to continue here. 
            FFSSDaemonStatus.dwCurrentState = SERVICE_RUNNING; 
            break; 
 
        case SERVICE_CONTROL_STOP: 
        // Do whatever it takes to stop here. 

			// Get FFSSD pid
			// Try to kill it

			SU_RB_GetIntValue(FFSS_REGISTRY_PATH_PROCESSID,&pid,0);
			if(!pid)
			{
				StopService();
				return;
			}
			else
			{

				h = OpenProcess(PROCESS_TERMINATE | PROCESS_QUERY_INFORMATION,false,pid);
				if(h != NULL)
				{
					if (TerminateProcess(h, 0))
					{
                      do
                      {
                        if(GetExitCodeProcess(h,&res) == 0)
                          break;
                        if(res != STILL_ACTIVE)
                        {
						  StopService();
						  return; 
                        }
                        Sleep(100);
                        nb++;
                      } while(nb < MAX_WAIT);
					}
				}

				FFSSDaemonStatus.dwCurrentState = SERVICE_RUNNING;
				break;
			}
	
 
        case SERVICE_CONTROL_INTERROGATE: 
        // Fall through to send current status. 
            break; 
 
        default: 
            SvcDebugOut(" [FFSSDaemon] Unrecognized opcode %ld\n", opcode); 
    } 
 
    // Send current status. 
    if (!SetServiceStatus (FFSSDaemonStatusHandle,  &FFSSDaemonStatus)) 
    { 
        status = GetLastError(); 
        SvcDebugOut(" [FFSSDaemon] SetServiceStatus error %ld\n",status); 
    } 
    return; 
} 
