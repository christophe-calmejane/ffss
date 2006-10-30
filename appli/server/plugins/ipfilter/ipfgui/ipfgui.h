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
// ipfgui.h : main header file for the IPFGUI DLL
//

#if !defined(AFX_IPFGUI_H__934F5476_5D96_4103_AFCB_0BBACAFA83BA__INCLUDED_)
#define AFX_IPFGUI_H__934F5476_5D96_4103_AFCB_0BBACAFA83BA__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"		// main symbols

/////////////////////////////////////////////////////////////////////////////
// CIpfguiApp
// See ipfgui.cpp for the implementation of this class
//

class CIpfguiApp : public CWinApp
{
public:
	CIpfguiApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CIpfguiApp)
	//}}AFX_VIRTUAL

	//{{AFX_MSG(CIpfguiApp)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_IPFGUI_H__934F5476_5D96_4103_AFCB_0BBACAFA83BA__INCLUDED_)
