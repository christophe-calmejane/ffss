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
#if !defined(AFX_CHAINSSHEET_H__22FD3B8D_665B_4E54_AA8E_0694E038B524__INCLUDED_)
#define AFX_CHAINSSHEET_H__22FD3B8D_665B_4E54_AA8E_0694E038B524__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// ChainsSheet.h : header file
//

#include "ChainPageDlg.h"
#include "FilterRule.h"
#include <list>
using namespace std;

/////////////////////////////////////////////////////////////////////////////
// CChainsSheet

class CChainsSheet : public CPropertySheet
{
	DECLARE_DYNAMIC(CChainsSheet)

// Construction
public:
	CChainsSheet(UINT nIDCaption, CWnd* pParentWnd = NULL, UINT iSelectPage = 0);
	CChainsSheet(LPCTSTR pszCaption, CWnd* pParentWnd = NULL, UINT iSelectPage = 0);

// Attributes
public:
	// list of pages
	list<CChainPageDlg*>	m_pChainPages;

	// Sheet buttons
	CButton					m_btnAddRule;
	CButton					m_btnDelRule;

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CChainsSheet)
	public:
	virtual BOOL OnInitDialog();
	protected:
	virtual BOOL OnCommand(WPARAM wParam, LPARAM lParam);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CChainsSheet();

	// Generated message map functions
protected:
	void Init();
	void AddRuleToPage(  int nIndex, CFilterRule* pRule );
	void DelRuleFromPage(int nIndex, CString strIP, CString strMask, CString strAction);
	void OnOK();
	void OnDelRuleClicked();
	void OnAddRuleClicked();
	//{{AFX_MSG(CChainsSheet)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_CHAINSSHEET_H__22FD3B8D_665B_4E54_AA8E_0694E038B524__INCLUDED_)
