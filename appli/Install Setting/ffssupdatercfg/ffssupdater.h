// ffssupdater.h : main header file for the FFSSUPDATER application
//

#if !defined(AFX_FFSSUPDATER_H__1C15C63E_2AE4_4AD4_B53A_DAEB395FB142__INCLUDED_)
#define AFX_FFSSUPDATER_H__1C15C63E_2AE4_4AD4_B53A_DAEB395FB142__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"		// main symbols

/////////////////////////////////////////////////////////////////////////////
// CFfssupdaterApp:
// See ffssupdater.cpp for the implementation of this class
//

class CFfssupdaterApp : public CWinApp
{
public:
	CFfssupdaterApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CFfssupdaterApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CFfssupdaterApp)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_FFSSUPDATER_H__1C15C63E_2AE4_4AD4_B53A_DAEB395FB142__INCLUDED_)
