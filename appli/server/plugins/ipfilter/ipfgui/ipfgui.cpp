/*
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/
// ipfgui.cpp : Defines the initialization routines for the DLL.
//

#include "stdafx.h"
#include "ipfgui.h"
#include "ChainsSheet.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

//
//	Note!
//
//		If this DLL is dynamically linked against the MFC
//		DLLs, any functions exported from this DLL which
//		call into MFC must have the AFX_MANAGE_STATE macro
//		added at the very beginning of the function.
//
//		For example:
//
//		extern "C" BOOL PASCAL EXPORT ExportedFunction()
//		{
//			AFX_MANAGE_STATE(AfxGetStaticModuleState());
//			// normal function body here
//		}
//
//		It is very important that this macro appear in each
//		function, prior to any calls into MFC.  This means that
//		it must appear as the first statement within the
//		function, even before any object variable declarations
//		as their constructors may generate calls into the MFC
//		DLL.
//
//		Please see MFC Technical Notes 33 and 58 for additional
//		details.
//

unsigned int GetFavoriteLanguage();

// Favorite language
unsigned int g_Language=0;

/////////////////////////////////////////////////////////////////////////////
// CIpfguiApp

BEGIN_MESSAGE_MAP(CIpfguiApp, CWinApp)
	//{{AFX_MSG_MAP(CIpfguiApp)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CIpfguiApp construction

CIpfguiApp::CIpfguiApp()
{
	// TODO: add construction code here,
	// Place all significant initialization in InitInstance
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CIpfguiApp object

CIpfguiApp theApp;

///////////////////////////////////////////////////////////////////////////////
FFSS_Filter_PApi g_pFAPI=NULL;


///////////////////////////////////////////////////////////////////////////////
extern "C" FS_PLUGIN_EXPORT bool GUI(HWND hWnd)
{
	CChainsSheet*	pChainsSheet;

	if( g_pFAPI==NULL ) {
		return(false);
	}

	// set string table
	CurST=ST[ GetFavoriteLanguage() ];

	// Edit chains
	pChainsSheet=new CChainsSheet( CurST[ST_WIN_TITLE], CWnd::FromHandle(hWnd) );
	pChainsSheet->DoModal();

	return(true);
}

///////////////////////////////////////////////////////////////////////////////
extern "C" FS_PLUGIN_EXPORT void SetAPIPointer(FFSS_Filter_PApi pFAPI)
{
	g_pFAPI=pFAPI;
}

///////////////////////////////////////////////////////////////////////////////
unsigned int GetFavoriteLanguage()
{
	char	szLanguage[4];
	DWORD	nBufSize;
	CRegKey RegKey;

	if( RegKey.Open(HKEY_LOCAL_MACHINE,"Software\\FFSS")==ERROR_SUCCESS ) {
		nBufSize=sizeof(szLanguage);
		if( RegKey.QueryValue(szLanguage,"FavoriteLanguage",&nBufSize)==ERROR_SUCCESS ) {
			if( stricmp(szLanguage,"fr")==0 ) {
				RegKey.Close();
				return(1);
			}
			// add more language here...
		}
		RegKey.Close();
	}

	return(0);
}