// stdafx.h : include file for standard system include files,
//  or project specific include files that are used frequently, but
//      are changed infrequently
//

#if !defined(AFX_STDAFX_H__7C789B95_47E9_4692_9739_893F5B163EB9__INCLUDED_)
#define AFX_STDAFX_H__7C789B95_47E9_4692_9739_893F5B163EB9__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#define VC_EXTRALEAN		// Exclude rarely-used stuff from Windows headers

#include <afxwin.h>         // MFC core and standard components
#include <afxext.h>         // MFC extensions
#include <afxdtctl.h>		// MFC support for Internet Explorer 4 Common Controls
#ifndef _AFX_NO_AFXCMN_SUPPORT
#include <afxcmn.h>			// MFC support for Windows Common Controls
#endif // _AFX_NO_AFXCMN_SUPPORT
#include <atlbase.h>


#include "StringTable.h"

#include "MT.h"
#ifdef _DEBUG
#define MT_ST_INSTANCE  MT_CSingleton<MT_CStringTable>::Instance()
#else
#define MT_ST_INSTANCE  MT_CSingleton<MT_CResourceStringTable>::Instance()
#endif 
#define MT_ST_LOCAL(id) MT_ST_INSTANCE->GetString(id)

#define FFSS_REG_KEY					HKEY_LOCAL_MACHINE
#define FFSS_REG_SUBKEY					"Software\\FFSS"
#define FFSS_REG_VALUE_USE_PROXY		"Autocheck_Type" // 0 = no conn, 1 = direct, 2 = proxy, 3 = ie
#define FFSS_REG_VALUE_PROXY_HOST		"Autocheck_Proxy_Host"
#define FFSS_REG_VALUE_PROXY_PORT		"Autocheck_Proxy_Port"
#define FFSS_REG_VALUE_PROXY_USER		"Autocheck_Proxy_User"
#define FFSS_REG_VALUE_PROXY_PASSWORD	"Autocheck_Proxy_Password"
#define FFSS_REG_FAVORITE_LANGUAGE		"FavoriteLanguage"

#define FFSS_INET_AGENT	"FFSS Updater (Test Settings)"
#define FFSS_TEST_PAGE	"http://www.ffss.fr.st"


//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_STDAFX_H__7C789B95_47E9_4692_9739_893F5B163EB9__INCLUDED_)
