// stdafx.h : include file for standard system include files,
//  or project specific include files that are used frequently, but
//      are changed infrequently
//

#if !defined(AFX_STDAFX_H__DCC35C34_C0C7_4244_9054_C73E32502B8C__INCLUDED_)
#define AFX_STDAFX_H__DCC35C34_C0C7_4244_9054_C73E32502B8C__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000


// Insert your headers here
#define WIN32_LEAN_AND_MEAN		// Exclude rarely-used stuff from Windows headers

#include <windows.h>

// TODO: reference additional headers your program requires here
#include "../../src/plugin.h"
#undef malloc
//#include "../../src/server.h"

#include "common.h"
#define FFSS_FAV_LANGUAGE	FFSS_LM_REGISTRY_PATH "FavoriteLanguage"

bool InstallPlugin();
void ReadChains();
void ReadRules(HKEY hKey);
void SaveChains();
void OnListChains(FFSS_FILTER_CHAIN Chain,const char Name[],FFSS_FILTER_ACTION Default);
void OnListRules(const char IP[],const char Mask[],FFSS_FILTER_ACTION Action,const char Name[]);
unsigned int GetFavoriteLanguage();
DWORD WINAPI ThreadFunc( LPVOID lpParam );

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_STDAFX_H__DCC35C34_C0C7_4244_9054_C73E32502B8C__INCLUDED_)
