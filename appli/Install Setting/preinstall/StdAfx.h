// stdafx.h : include file for standard system include files,
//  or project specific include files that are used frequently, but
//      are changed infrequently
//

#if !defined(AFX_STDAFX_H__8920974B_F917_4051_A47A_6435DC09A943__INCLUDED_)
#define AFX_STDAFX_H__8920974B_F917_4051_A47A_6435DC09A943__INCLUDED_

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

#include <afxsock.h>		// MFC socket extensions
#include <atlbase.h>

#define FFSS_REG_KEY				HKEY_LOCAL_MACHINE
#define FFSS_REG_SUBKEY				"Software\\FFSS"
#define FFSS_REG_SERVERKEY			"Software\\FFSS\\Server"
#define FFSS_REG_PLUGINSKEY			"Software\\FFSS\\Server\\Plugins"

#define FFSS_REG_SVR_NAME			"Global_Name"
#define FFSS_REG_SVR_COMMENT		"Global_Comment"
#define FFSS_REG_SVR_MASTER			"Global_Master"
#define FFSS_REG_FAVORITE_LANGUAGE	"FavoriteLanguage"

#include "StringTable.h"

#include "MT.h"
#ifdef _DEBUG
#define MT_ST_INSTANCE  MT_CSingleton<MT_CStringTable>::Instance()
#else
#define MT_ST_INSTANCE  MT_CSingleton<MT_CResourceStringTable>::Instance()
#endif 
#define MT_ST_LOCAL(id) MT_ST_INSTANCE->GetString(id)


//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_STDAFX_H__8920974B_F917_4051_A47A_6435DC09A943__INCLUDED_)
