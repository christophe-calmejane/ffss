// ffssupdaterDlg.h : header file
//

#if !defined(AFX_FFSSUPDATERDLG_H__AAAB8D6C_6DAA_4F6E_A31E_80AD843DC79F__INCLUDED_)
#define AFX_FFSSUPDATERDLG_H__AAAB8D6C_6DAA_4F6E_A31E_80AD843DC79F__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/////////////////////////////////////////////////////////////////////////////
// CFfssupdaterDlg dialog

class CFfssupdaterDlg : public CDialog
{
private:
	bool Localize(void);
	void ErrorMessage( CString &strDest, DWORD nError );
	void UpdatePane( void );
	BOOL	m_bUseProxy;
	CRegKey	m_RegKey;

// Construction
public:
	CFfssupdaterDlg(CWnd* pParent = NULL);	// standard constructor

// Dialog Data
	//{{AFX_DATA(CFfssupdaterDlg)
	enum { IDD = IDD_FFSSUPDATER_DIALOG };
	CString	m_strHostname;
	CString	m_strPassword;
	long	m_lPort;
	CString	m_strUser;
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CFfssupdaterDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	HICON m_hIcon;

	// Generated message map functions
	//{{AFX_MSG(CFfssupdaterDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
	afx_msg void OnDirect();
	afx_msg void OnProxy();
	afx_msg void OnButton();
	afx_msg void OnNoConn();
	afx_msg void OnTestSettings();
	afx_msg void OnIe();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

public:
	afx_msg void OnDestroy();
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_FFSSUPDATERDLG_H__AAAB8D6C_6DAA_4F6E_A31E_80AD843DC79F__INCLUDED_)
