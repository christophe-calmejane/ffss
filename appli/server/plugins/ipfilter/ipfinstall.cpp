// ipfinstall.cpp : Defines the entry point for the DLL application.
//

#include "stdafx.h"

#include <list>

using namespace std;

#define FILTER_NAME			"IP Filter"
#define FILTER_VERSION		"2.0"
#define FILTER_COPYRIGHT	"(c) MaD - 2002"
#define FILTER_DESCRIPTION	""
#define FILTER_REG_SUBKEY   FSP_BASE_REG_SUBKEY FILTER_NAME
#define FILTER_REG_KEY		FSP_BASE_REG_KEY FILTER_NAME "\\"

#define FFSS_SERVERDIR		FFSS_LM_REGISTRY_PATH "Server\\ServerDirectory"
#define FILTER_GUI_DLL		"ipfgui.dll"

unsigned int g_Language=0;

char *Filter_Description[2] = { "Rules editor for FFSS IP-based filter engine.",
								"Assure la gestion des règles pour le moteur de filtrage IP." };

char *Error_Messages[2] = { "User interface not found",
							"Interface graphique non trouvée"};

/* Plugins Info */
FSP_TInfos g_F_Infos;

/* Plugins Callbacks */
FS_PPlugin g_Pl;

/* ID of the thread */
HANDLE	g_hThread=NULL;

/////////////////////////////////////////////////////////////////////////////
class ChainRegPtr {
public:
	FFSS_FILTER_CHAIN	nChain;
	char*				szKey;

	ChainRegPtr(FFSS_FILTER_CHAIN a, char* b) : nChain(a), szKey(b) {};
};
list<ChainRegPtr*>	g_ChainRegPtrs;
int					g_nRule;

char*	g_ChainKey=NULL;

FFSS_Filter_PApi g_pFAPI;
void *(*g_fnQueryFunc)(int Type,...);

typedef bool (*procGUI)(HWND);
typedef void (*procSetAPIPointer)(FFSS_Filter_PApi);

/*****************************************************************************/
BOOL APIENTRY DllMain( HANDLE hModule, 
                       DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
					 )
{
    return TRUE;
}

///////////////////////////////////////////////////////////////////////////////
extern "C" FS_PLUGIN_EXPORT FSP_PInfos Plugin_QueryInfos(void)
{
	/* Setting plugin info struct (for Plugin_QueryInfos) */
	g_Language=GetFavoriteLanguage();
	g_F_Infos.Name = FILTER_NAME;
	g_F_Infos.Version = FILTER_VERSION;
	g_F_Infos.Copyright = FILTER_COPYRIGHT;
	g_F_Infos.Description = Filter_Description[g_Language];

	return( &g_F_Infos );
}

/////////////////////////////////////////////////////////////////////////////
extern "C" FS_PLUGIN_EXPORT void Plugin_UnInit(void)
{
	SaveChains();

	// There is a thread active
	if( g_hThread!=NULL ) {
		TerminateThread(g_hThread,1);
	}
}

/////////////////////////////////////////////////////////////////////////////
extern "C" FS_PLUGIN_EXPORT FS_PPlugin Plugin_Init(void *Info,void *(*QueryFunc)(int Type,...))
{
	// Place significant initialization here
	g_Language=GetFavoriteLanguage();
	g_fnQueryFunc=QueryFunc;
	g_pFAPI = (FFSS_Filter_PApi)g_fnQueryFunc(FSPQ_GET_FILTER_API);
	if( InstallPlugin()==false ) {
		return(NULL);
	}
	
	/* Load and install rules */
	ReadChains();

	return(g_Pl);	
}

/////////////////////////////////////////////////////////////////////////////
DWORD WINAPI ThreadFunc( LPVOID lpParam )
{
	HINSTANCE			hInstance=NULL;
	procGUI				fnGUI=NULL;
	procSetAPIPointer	fnSetAPIPointer=NULL;
	char				szPathToDLL[_MAX_PATH];
	HWND				hWnd=(HWND)lpParam;

	SU_RB_GetStrValue(FFSS_SERVERDIR,szPathToDLL,sizeof(szPathToDLL),"");
	strcat(szPathToDLL,"\\Plugins\\");
	strcat(szPathToDLL,FILTER_GUI_DLL);

	hInstance = LoadLibrary(szPathToDLL);
	if( hInstance==NULL ) {
		MessageBox((HWND)hWnd,Error_Messages[g_Language],"IP Filter",MB_OK|MB_ICONERROR);
		return(-1);
	} else {
		fnGUI=(procGUI)GetProcAddress(hInstance, "GUI");
		fnSetAPIPointer=(procSetAPIPointer)GetProcAddress(hInstance, "SetAPIPointer");
		
		if( fnGUI==NULL || fnSetAPIPointer==NULL ) {
			MessageBox((HWND)hWnd,Error_Messages[g_Language],"IP Filter",MB_OK|MB_ICONERROR);
			FreeLibrary(hInstance);
			return(-1);
		}

		fnSetAPIPointer(g_pFAPI);
		fnGUI((HWND)hWnd);
		
		FreeLibrary(hInstance);
	}

	g_hThread=0;
	return 0; 
}


/////////////////////////////////////////////////////////////////////////////
extern "C" FS_PLUGIN_EXPORT SU_BOOL Plugin_Configure(void* hWnd)
{
	if( g_hThread!=NULL ) {
		return(true);
	}

	g_hThread = CreateThread( 
        NULL,               // no security attributes 
        0,                  // use default stack size  
        ThreadFunc,         // thread function 
        hWnd,				// argument to thread function 
        0,                  // use default creation flags 
        NULL);		// returns the thread identifier 
 
   // Check the return value for success. 
   if (g_hThread == NULL) {
	   return(false);
   } else {
      CloseHandle( g_hThread );
	  return(true);
   }
}

/////////////////////////////////////////////////////////////////////////////
bool InstallPlugin()
{
	/* Setting all callbacks to NULL */
	g_Pl = (FS_PPlugin) malloc(sizeof(FS_TPlugin));
	if(g_Pl != NULL) {
		memset(g_Pl,0,sizeof(FS_TPlugin));

		/* Setting plugin infos */
		g_Pl->Name = FILTER_NAME;
		g_Pl->Copyright = FILTER_COPYRIGHT;
		g_Pl->Version = FILTER_VERSION;
		g_Pl->size = sizeof(FS_TPlugin);
	}

	return( g_Pl!=NULL && g_pFAPI!=NULL );
}

/////////////////////////////////////////////////////////////////////////////
void ReadChains()
{
	int		nKeyIndex=0;
	DWORD	retCode;
	char	szhKey[MAX_PATH];
	DWORD	dwSize;
	FILETIME ftLastWriteTime;
	HKEY	hKey;
	HKEY	hKeyRules;

	hKey=SU_RB_OpenKeys(FILTER_REG_KEY,KEY_READ);
	if( hKey==NULL ) {
		return;
	}

	// Enumerate keys (each key is a chain)
	retCode=ERROR_SUCCESS;
	while( retCode==ERROR_SUCCESS || retCode==ERROR_MORE_DATA ) {
			
		dwSize=sizeof(szhKey);
		retCode=RegEnumKeyEx( hKey, nKeyIndex, szhKey, &dwSize, 
					NULL,NULL,NULL,&ftLastWriteTime);
		if( retCode==ERROR_SUCCESS || retCode==ERROR_MORE_DATA ) {
			if( RegOpenKeyEx( hKey, szhKey,0,KEY_READ, &hKeyRules )==ERROR_SUCCESS ) {
				ReadRules( hKeyRules );
				RegCloseKey( hKeyRules );
			}
		}
		nKeyIndex++;
	}
	RegCloseKey(hKey);
}

/////////////////////////////////////////////////////////////////////////////
void ReadRules(HKEY hKey)
{
	char	buffer[255];
	char	szRuleName[128];
	unsigned long nBufSize;
	char*	pIP;
	char*	pMask;
	char*	pAction;
	char*	pName;
	int		i=0;
	DWORD	nChainNumber;

	nBufSize=sizeof(nChainNumber);
	if( RegQueryValueEx(hKey,NULL,NULL,NULL,(unsigned char*)&nChainNumber,&nBufSize)==ERROR_SUCCESS ) {
		
		// flush chain and install new rules
		g_pFAPI->ClearChain(nChainNumber);
		
		// set policy
		nBufSize=sizeof(buffer);
		if( RegQueryValueEx(hKey,"Policy",NULL,NULL,(unsigned char*)buffer,&nBufSize)==ERROR_SUCCESS ) {
			g_pFAPI->SetDefaultActionOfChain(nChainNumber,GetActionFromString(buffer));
		}

		// read rules
		sprintf(szRuleName,"Rule%d",i);
		nBufSize=sizeof(buffer);
		while( RegQueryValueEx(hKey,szRuleName,NULL,NULL,(unsigned char*)buffer,&nBufSize)==ERROR_SUCCESS ) {
			// Got a rule, get fields
			pIP=buffer;
			pMask=strchr(pIP,'/');
			*pMask='\0';
			pMask++;
			pAction=strchr(pMask,'/');
			*pAction='\0';
			pAction++;
			pName=strchr(pAction,'/');
			*pName='\0';
			pName++;

			// Add rule
			g_pFAPI->AddRuleToChain_Tail(nChainNumber,pIP,pMask,GetActionFromString(pAction),pName);

			// Try to read next rule
			i++;
			sprintf(szRuleName,"Rule%d",i);
			nBufSize=sizeof(buffer);
		}

	}
}

/////////////////////////////////////////////////////////////////////////////
void SaveChains()
{
	list<ChainRegPtr*>::iterator	it;
	HKEY hKey;

	// Destroy all keys (chains and rules)
	SU_RB_DelKey(FILTER_REG_KEY);

	// Recreate key, store chains and rules
	hKey=SU_RB_CreateKeys(FILTER_REG_KEY);
	if( hKey!=NULL ) {
		// no need to keep key open
		RegCloseKey(hKey);

		g_ChainRegPtrs.clear();
		g_pFAPI->EnumChains(OnListChains);

		for( it=g_ChainRegPtrs.begin(); it!=g_ChainRegPtrs.end(); it++ ) {
			g_ChainKey=(*it)->szKey;
			g_nRule=0;
			g_pFAPI->EnumRulesOfChain((*it)->nChain,OnListRules);
			free(g_ChainKey);
			delete(*it);
		}
	}
}

///////////////////////////////////////////////////////////////////////////////
void OnListChains(FFSS_FILTER_CHAIN Chain,const char Name[],FFSS_FILTER_ACTION Default)
{
	HKEY	hKey;
	char	buffer[255];

	sprintf(buffer,"%s\\%s\\",FILTER_REG_KEY,Name);

	hKey=SU_RB_CreateKeys(buffer);
	if( hKey!=NULL ) {
		// no need to keep key open
		RegCloseKey(hKey);

		// Can't enumerate now, store chain number for later use
		g_ChainRegPtrs.push_back(new ChainRegPtr(Chain,strdup(buffer)));

		// Save Chain ID
		SU_RB_SetIntValue(buffer,(DWORD)Chain);

		// Save Policy
		strcat(buffer,"\\Policy");
		SU_RB_SetStrValue(buffer,GetStringFromAction( Default ));
	}
}

///////////////////////////////////////////////////////////////////////////////
// C callback
void OnListRules(const char IP[],const char Mask[],FFSS_FILTER_ACTION Action,const char Name[])
{
	char	szRule[255];
	char	szRuleName[255];

	sprintf(szRuleName,"%s\\Rule%d",g_ChainKey,g_nRule);
	sprintf(szRule,"%s/%s/%s/%s",IP,Mask,GetStringFromAction(Action),Name);	
	SU_RB_SetStrValue(szRuleName,szRule);	
	g_nRule++;
}

///////////////////////////////////////////////////////////////////////////////
unsigned int GetFavoriteLanguage()
{
	char	szLanguage[4];

	SU_RB_GetStrValue(FFSS_FAV_LANGUAGE,szLanguage,sizeof(szLanguage),"en");
	if( stricmp(szLanguage,"fr")==0 ) {
		return(1);
	}
	return(0);
}
