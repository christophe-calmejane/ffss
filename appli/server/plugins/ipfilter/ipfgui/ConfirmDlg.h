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
#if !defined(AFX_CONFIRMDLG_H__04447994_C8BD_4A75_923B_53C659F1CF0C__INCLUDED_)
#define AFX_CONFIRMDLG_H__04447994_C8BD_4A75_923B_53C659F1CF0C__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// ConfirmDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CConfirmDlg dialog

class CConfirmDlg : public CDialog
{
// Construction
public:
	CConfirmDlg(CString& Rule, CWnd* pParent = NULL);   // standard constructor

	CString m_RuleMsg;

// Dialog Data
	//{{AFX_DATA(CConfirmDlg)
	enum { IDD = IDD_CONFIRM };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CConfirmDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	void Localize();

	// Generated message map functions
	//{{AFX_MSG(CConfirmDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnNo();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_CONFIRMDLG_H__04447994_C8BD_4A75_923B_53C659F1CF0C__INCLUDED_)
