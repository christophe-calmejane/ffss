// PreInstall.h : main header file for the PREINSTALL application
//

#if !defined(AFX_PREINSTALL_H__124206CF_603F_4E55_940C_F8FCDDC1CDEA__INCLUDED_)
#define AFX_PREINSTALL_H__124206CF_603F_4E55_940C_F8FCDDC1CDEA__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"		// main symbols

/////////////////////////////////////////////////////////////////////////////
// CPreInstallApp:
// See PreInstall.cpp for the implementation of this class
//

class CPreInstallApp : public CWinApp
{
public:
	CPreInstallApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPreInstallApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CPreInstallApp)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
protected:
	void InstallStringTables();
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PREINSTALL_H__124206CF_603F_4E55_940C_F8FCDDC1CDEA__INCLUDED_)
