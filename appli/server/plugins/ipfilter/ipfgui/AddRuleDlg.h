#if !defined(AFX_ADDRULEDLG_H__EB73D2BE_BABE_4ACD_8724_79DB5FC0BE20__INCLUDED_)
#define AFX_ADDRULEDLG_H__EB73D2BE_BABE_4ACD_8724_79DB5FC0BE20__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// AddRuleDlg.h : header file
//

#include "FilterRule.h"

/////////////////////////////////////////////////////////////////////////////
// CAddRuleDlg dialog

class CAddRuleDlg : public CDialog
{
// Construction
public:
	CAddRuleDlg(CWnd* pParent = NULL);   // standard constructor
	void SetRule( CFilterRule* pRule );

	CFilterRule*	m_pRule;

// Dialog Data
	//{{AFX_DATA(CAddRuleDlg)
	enum { IDD = IDD_ADDRULE_DLG };
	CIPAddressCtrl	c_Netmask;
	CIPAddressCtrl	c_IP;
	CString	m_strRuleName;
	int		m_nAction;

		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CAddRuleDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	void Localize();

	// Generated message map functions
	//{{AFX_MSG(CAddRuleDlg)
		// NOTE: the ClassWizard will add member functions here
	virtual void OnOK();
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_ADDRULEDLG_H__EB73D2BE_BABE_4ACD_8724_79DB5FC0BE20__INCLUDED_)
