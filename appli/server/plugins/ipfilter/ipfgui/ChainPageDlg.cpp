// ChainPageDlg.cpp : implementation file
//

#include "stdafx.h"
#include "ipfgui.h"
#include "ChainPageDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern FFSS_Filter_PApi g_pFAPI;
CChainPageDlg*	g_pCurrentPage;

void OnEnumRulesOfChain(const char IP[],const char Mask[],FFSS_FILTER_ACTION Action,const char Name[]);

/////////////////////////////////////////////////////////////////////////////
// CChainPageDlg property page


CChainPageDlg::CChainPageDlg() : CPropertyPage(CChainPageDlg::IDD)
{
	//{{AFX_DATA_INIT(CChainPageDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
	m_nChainNumber=0;
	m_nAction=0;
	m_strName="";
}


void CChainPageDlg::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CChainPageDlg)
	DDX_Control(pDX, IDC_RULES, m_Rules);
	DDX_CBString(pDX, IDC_POLICY, m_strPolicy);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CChainPageDlg, CPropertyPage)
	//{{AFX_MSG_MAP(CChainPageDlg)
	ON_BN_CLICKED(IDC_MOVEDOWN, OnMovedown)
	ON_BN_CLICKED(IDC_MOVEUP, OnMoveup)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CChainPageDlg message handlers

void CChainPageDlg::OnMovedown() 
{
	// TODO: Add your control notification handler code here
	int			nItem=m_Rules.GetItemCount();
	
	while( nItem>=0 ) {

		if( m_Rules.GetItemState(nItem,LVIS_SELECTED)==LVIS_SELECTED ) {

			// Last item selected, can't move down
			if( nItem==m_Rules.GetItemCount()-1 ) {
				return;
			}
			SwapRules(nItem,nItem+1);			
			m_Rules.SetItemState(nItem+1,LVIS_SELECTED,LVIS_SELECTED);
			m_Rules.SetItemState(nItem, 0, LVIS_SELECTED);
		}
		
		nItem--;
	}
}

///////////////////////////////////////////////////////////////////////////////
void CChainPageDlg::OnMoveup() 
{
	// TODO: Add your control notification handler code here
	POSITION	pos = m_Rules.GetFirstSelectedItemPosition();
	int			nItem;
	
	while( pos!=NULL ) {
		nItem=m_Rules.GetNextSelectedItem(pos);

		// First item selected can't move up
		if( nItem==0 ) {
			return;
		}

		SwapRules(nItem,nItem-1);
		m_Rules.SetItemState(nItem-1,LVIS_SELECTED,LVIS_SELECTED);
		m_Rules.SetItemState(nItem, 0, LVIS_SELECTED);
	}	
}

///////////////////////////////////////////////////////////////////////////////
BOOL CChainPageDlg::OnInitDialog() 
{
	CPropertyPage::OnInitDialog();
	
	// TODO: Add extra initialization here
	Localize();
	m_Rules.InsertColumn(0,CurST[ST_R_IP],LVCFMT_LEFT,95);
	m_Rules.InsertColumn(1,CurST[ST_R_MASK],LVCFMT_LEFT,95);
	m_Rules.InsertColumn(2,CurST[ST_R_ACTION],LVCFMT_LEFT,50);
	m_Rules.InsertColumn(3,CurST[ST_R_NAME],LVCFMT_LEFT,124);
	m_Rules.SetExtendedStyle(LVS_EX_FULLROWSELECT);

	/* Set policy */
	m_strPolicy=GetStringFromAction(m_nAction);

	g_pCurrentPage=this;
	g_pFAPI->EnumRulesOfChain(m_nChainNumber,OnEnumRulesOfChain);

	UpdateData(FALSE);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

///////////////////////////////////////////////////////////////////////////////
void CChainPageDlg::SwapRules( int nIndex, int nNewIndex )
{
	CString	strIP;
	CString	strMask;
	CString	strAction;
	CString	strName;

	strIP=m_Rules.GetItemText(nIndex,0);
	strMask=m_Rules.GetItemText(nIndex,1);
	strAction=m_Rules.GetItemText(nIndex,2);
	strName=m_Rules.GetItemText(nIndex,3);

	m_Rules.SetItemText(nIndex,0,(LPCTSTR)m_Rules.GetItemText(nNewIndex,0));
	m_Rules.SetItemText(nIndex,1,(LPCTSTR)m_Rules.GetItemText(nNewIndex,1));
	m_Rules.SetItemText(nIndex,2,(LPCTSTR)m_Rules.GetItemText(nNewIndex,2));
	m_Rules.SetItemText(nIndex,3,(LPCTSTR)m_Rules.GetItemText(nNewIndex,3));

	m_Rules.SetItemText(nNewIndex,0,(LPCTSTR)strIP );
	m_Rules.SetItemText(nNewIndex,1,(LPCTSTR)strMask );
	m_Rules.SetItemText(nNewIndex,2,(LPCTSTR)strAction );
	m_Rules.SetItemText(nNewIndex,3,(LPCTSTR)strName );
}

///////////////////////////////////////////////////////////////////////////////
int CChainPageDlg::GetRuleIndex(CString strIP, CString strMask, CString strAction)
{
	int		nItem;
	int		nItemCount;

	nItemCount=m_Rules.GetItemCount();
	for(nItem=0; nItem<nItemCount; nItem++) {
		if(    ( strIP==m_Rules.GetItemText(nItem,0) )
			&& ( strMask==m_Rules.GetItemText(nItem,1) )
			&& ( strAction==m_Rules.GetItemText(nItem,2) ) ) {
			return(nItem);
		}
	}
	return(-1);
}

///////////////////////////////////////////////////////////////////////////////
void CChainPageDlg::SetRules(void)
{
	int	nItem;
	int nItemCount=m_Rules.GetItemCount();
	CString	strIP;
	CString	strMask;
	CString	strAction;
	CString	strName;

	// Clear current chain
	g_pFAPI->ClearChain(m_nChainNumber);

	// Set default rule
	GetDlgItemText(IDC_POLICY,m_strPolicy);
	g_pFAPI->SetDefaultActionOfChain(m_nChainNumber,GetActionFromString((LPCTSTR)m_strPolicy));

	// Set rules
	for(nItem=0; nItem<nItemCount; nItem++) {
		strIP=m_Rules.GetItemText(nItem,0);
		strMask=m_Rules.GetItemText(nItem,1);
		strAction=m_Rules.GetItemText(nItem,2);
		strName=m_Rules.GetItemText(nItem,3);		

		g_pFAPI->AddRuleToChain_Tail(m_nChainNumber,(LPCTSTR)strIP,(LPCTSTR)strMask,
			GetActionFromString((LPCTSTR)strAction),(LPCTSTR)strName);
	}
}

///////////////////////////////////////////////////////////////////////////////
void OnEnumRulesOfChain(const char IP[],const char Mask[],FFSS_FILTER_ACTION Action,const char Name[])
{
	int	nItem=g_pCurrentPage->m_Rules.GetItemCount();
	
	g_pCurrentPage->m_Rules.InsertItem(nItem,IP);
	g_pCurrentPage->m_Rules.SetItemText(nItem,1,Mask);
	g_pCurrentPage->m_Rules.SetItemText(nItem,2,GetStringFromAction(Action));
	g_pCurrentPage->m_Rules.SetItemText(nItem,3,Name);
}

///////////////////////////////////////////////////////////////////////////////
void CChainPageDlg::Localize()
{
	SetDlgItemText(IDC_LRULES,CurST[ST_RULES]);
	SetDlgItemText(IDC_LDEFAULTRULE,CurST[ST_DEFAULTRULE]);
	SetDlgItemText(IDC_MOVEUP,CurST[ST_MOVEUP]);
	SetDlgItemText(IDC_MOVEDOWN,CurST[ST_MOVEDOWN]);
}
