// ConfirmDlg.cpp : implementation file
//

#include "stdafx.h"
#include "ipfgui.h"
#include "ConfirmDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CConfirmDlg dialog


CConfirmDlg::CConfirmDlg(CString& Rule,CWnd* pParent /*=NULL*/)
	: CDialog(CConfirmDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CConfirmDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
	m_RuleMsg=Rule;
}


void CConfirmDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CConfirmDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CConfirmDlg, CDialog)
	//{{AFX_MSG_MAP(CConfirmDlg)
	ON_BN_CLICKED(IDNO, OnNo)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CConfirmDlg message handlers

BOOL CConfirmDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	// TODO: Add extra initialization here
	Localize();
	SetDlgItemText(IDC_RULEMSG,(LPCTSTR)m_RuleMsg);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

///////////////////////////////////////////////////////////////////////////////
void CConfirmDlg::OnNo() 
{
	// TODO: Add your control notification handler code here
	EndDialog(IDNO);
}

///////////////////////////////////////////////////////////////////////////////
void CConfirmDlg::Localize()
{
	SetWindowText(CurST[ST_C_WIN_TITLE]);
	SetDlgItemText(IDC_LDELRULE,CurST[ST_C_REMOVE]);
	SetDlgItemText(IDC_LFROM,CurST[ST_C_FROM]);
	SetDlgItemText(IDOK,CurST[ST_C_ALL]);
	SetDlgItemText(IDNO,CurST[ST_C_CURRENT]);
	SetDlgItemText(IDCANCEL,CurST[ST_C_CANCEL]);
}
