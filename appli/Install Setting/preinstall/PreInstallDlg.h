// PreInstallDlg.h : header file
//

#if !defined(AFX_PREINSTALLDLG_H__90634776_DA10_492F_83AB_E62EDA228ADD__INCLUDED_)
#define AFX_PREINSTALLDLG_H__90634776_DA10_492F_83AB_E62EDA228ADD__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/////////////////////////////////////////////////////////////////////////////
// CPreInstallDlg dialog

#include <lm.h>

/* WinNT function */
typedef NET_API_STATUS (__stdcall *procNetShareEnumNT)(LPWSTR,DWORD,LPBYTE*,DWORD,LPDWORD,LPDWORD,LPDWORD);

/* Win9x function */
typedef NET_API_STATUS (__stdcall *procNetShareEnum)(const char FAR *,
													 short,char FAR *,unsigned short,
													 unsigned short FAR *,unsigned short FAR *);

class CPreInstallDlg : public CDialog
{
private:
	void ConvertFromOldFormat( void );
	void AddShare( const char *szShareName, const char *szSharePath, 
				   const char *szShareComment,const char *szSharePasswd,
				   DWORD dwMaxUsers);
	long ImportWinNTShares(void);
	long ImportWin9xShares(void);
	CRegKey				m_RegKey;
	HINSTANCE			m_hinstLib;
	bool				m_bIsWinNT;
	procNetShareEnumNT	m_fnNetShareEnumNT;
	procNetShareEnum	m_fnNetShareEnum;


// Construction
public:
	CPreInstallDlg(CWnd* pParent = NULL);	// standard constructor

// Dialog Data
	//{{AFX_DATA(CPreInstallDlg)
	enum { IDD = IDD_PREINSTALL_DIALOG };
	CString	m_strComment;
	CString	m_strMaster;
	BOOL	m_bImportSamba;
	CString	m_strServer;
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPreInstallDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	HICON m_hIcon;

	// Generated message map functions
	//{{AFX_MSG(CPreInstallDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
	afx_msg void OnOk();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PREINSTALLDLG_H__90634776_DA10_492F_83AB_E62EDA228ADD__INCLUDED_)
