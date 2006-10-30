#include <ffss.h>

#define FFSS_REGISTRY_PATH_PROCESSID FFSS_LM_REGISTRY_PATH "Server\\ProcessId"
#define SLEEP_TIME 100
#define MAX_WAIT 50

int APIENTRY WinMain(HINSTANCE hInstance,HINSTANCE hPrevInstance,LPSTR lpCmdLine,int nCmdShow)
{
  DWORD ProcessId;
  HANDLE Process;
  DWORD res;
  int nb = 0;

  ProcessId = SU_RB_GetIntValue(FFSS_REGISTRY_PATH_PROCESSID,0);
  if(ProcessId == 0)
    return 0;
  Process = OpenProcess(PROCESS_TERMINATE | PROCESS_QUERY_INFORMATION,false,ProcessId);
  if(Process == NULL)
    return 0;
  if(TerminateProcess(Process,0) == 0)
    return -1;
  do
  {
    if(GetExitCodeProcess(Process,&res) == 0)
      return -2;
    if(res != STILL_ACTIVE)
      break;
    Sleep(100);
    nb++;
  } while(nb < MAX_WAIT);

  Sleep(500);
  return 0;
}
