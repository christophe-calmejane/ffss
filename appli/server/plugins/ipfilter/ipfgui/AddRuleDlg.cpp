// AddRuleDlg.cpp : implementation file
//

#include "stdafx.h"
#include "ipfgui.h"
#include "AddRuleDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CAddRuleDlg dialog


CAddRuleDlg::CAddRuleDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CAddRuleDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CAddRuleDlg)
	m_strRuleName = _T("");
	m_nAction = -1;
	//}}AFX_DATA_INIT
	m_pRule=NULL;
}


void CAddRuleDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAddRuleDlg)
	DDX_Control(pDX, IDC_RULE_MASK, c_Netmask);
	DDX_Control(pDX, IDC_RULE_IP, c_IP);
	DDX_Text(pDX, IDC_RULE_NAME, m_strRuleName);
	DDX_CBIndex(pDX, IDC_RULE_ACTION, m_nAction);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CAddRuleDlg, CDialog)
	//{{AFX_MSG_MAP(CAddRuleDlg)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CAddRuleDlg message handlers

void CAddRuleDlg::OnOK() 
{
	// TODO: Add extra validation here
	BYTE b0,b1,b2,b3;

	if( UpdateData()==FALSE )
		return;
	
	if ( m_pRule!=NULL ) {
		m_pRule->m_strName=m_strRuleName;
		m_pRule->m_nAction=m_nAction+1;
		c_IP.GetAddress(b0,b1,b2,b3);
		m_pRule->m_strIP.Format("%d.%d.%d.%d",b0,b1,b2,b3);
		c_Netmask.GetAddress(b0,b1,b2,b3);
		m_pRule->m_strNetMask.Format("%d.%d.%d.%d",b0,b1,b2,b3);
		if( IsDlgButtonChecked(IDC_CHAIN_ALL)==BST_CHECKED ) {
			m_pRule->m_bGlobal=true;
		}
	}

	CDialog::OnOK();
}

///////////////////////////////////////////////////////////////////////////////
void CAddRuleDlg::SetRule(CFilterRule *pRule)
{
	m_pRule=pRule;
}

///////////////////////////////////////////////////////////////////////////////
BOOL CAddRuleDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();

	Localize();

	m_strRuleName=m_pRule->m_strName;
	m_nAction=m_pRule->m_nAction-1;
	c_IP.ClearAddress();
	c_Netmask.ClearAddress();
	CheckRadioButton(IDC_CHAIN_THIS,IDC_CHAIN_ALL,IDC_CHAIN_THIS);

	UpdateData(FALSE);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}


///////////////////////////////////////////////////////////////////////////////
void CAddRuleDlg::Localize()
{
	SetWindowText(CurST[ST_A_WIN_TITLE]);
	SetDlgItemText(IDC_LRULENAME,CurST[ST_A_RULENAME]);
	SetDlgItemText(IDC_LIP,CurST[ST_A_IP]);
	SetDlgItemText(IDC_LNETMASK,CurST[ST_A_MASK]);
	SetDlgItemText(IDC_LACTION,CurST[ST_A_ACTION]);
	SetDlgItemText(IDC_LAPPLY,CurST[ST_A_APPLYTO]);
	SetDlgItemText(IDC_CHAIN_THIS,CurST[ST_A_CURRENT]);
	SetDlgItemText(IDC_CHAIN_ALL,CurST[ST_A_ALL]);
	SetDlgItemText(IDOK,CurST[ST_A_OK]);
	SetDlgItemText(IDCANCEL,CurST[ST_A_CANCEL]);
}
