#if !defined(AFX_CHAINPAGEDLG_H__0843BB61_AE70_4D06_96AB_F0B0E1BF1B63__INCLUDED_)
#define AFX_CHAINPAGEDLG_H__0843BB61_AE70_4D06_96AB_F0B0E1BF1B63__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// ChainPageDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CChainPageDlg dialog

class CChainPageDlg : public CPropertyPage
{
// Construction
public:
	CChainPageDlg();   // standard constructor

	// Chain global information 
	FFSS_FILTER_CHAIN	m_nChainNumber;
	FFSS_FILTER_ACTION	m_nAction;
	CString				m_strName;

// Dialog Data
	//{{AFX_DATA(CChainPageDlg)
	enum { IDD = IDD_CHAIN_PAGE };
	CListCtrl	m_Rules;
	CString		m_strPolicy;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CChainPageDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	void Localize();

	// Generated message map functions
	//{{AFX_MSG(CChainPageDlg)
	afx_msg void OnMovedown();
	afx_msg void OnMoveup();
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

public:
	// Set rules in Engine
	void SetRules(void);

	// Return the index of the rule identified by IP Mask Action
	int GetRuleIndex( CString strIP, CString strMask, CString strAction );

	// Swap rules in list
	void SwapRules( int nIndex, int nNewIndex );
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_CHAINPAGEDLG_H__0843BB61_AE70_4D06_96AB_F0B0E1BF1B63__INCLUDED_)
