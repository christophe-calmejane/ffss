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
// ChainsSheet.cpp : implementation file
//

#include "stdafx.h"
#include "ipfgui.h"
#include "ChainsSheet.h"
#include "AddRuleDlg.h"
#include "ConfirmDlg.h"
#include "resource.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

CChainsSheet*	g_pChainsSheet=NULL;
extern FFSS_Filter_PApi g_pFAPI;

void OnEnumChains(FFSS_FILTER_CHAIN Chain,const char Name[],FFSS_FILTER_ACTION Default);

/////////////////////////////////////////////////////////////////////////////
// CChainsSheet

IMPLEMENT_DYNAMIC(CChainsSheet, CPropertySheet)

CChainsSheet::CChainsSheet(UINT nIDCaption, CWnd* pParentWnd, UINT iSelectPage)
	:CPropertySheet(nIDCaption, pParentWnd, iSelectPage)
{
	Init();
}

CChainsSheet::CChainsSheet(LPCTSTR pszCaption, CWnd* pParentWnd, UINT iSelectPage)
	:CPropertySheet(pszCaption, pParentWnd, iSelectPage)
{
	Init();
}

CChainsSheet::~CChainsSheet()
{
	list<CChainPageDlg*>::iterator	it;

	for( it=m_pChainPages.begin(); it!=m_pChainPages.end(); it++ ) {
		delete(*it);
	}
}


BEGIN_MESSAGE_MAP(CChainsSheet, CPropertySheet)
	//{{AFX_MSG_MAP(CChainsSheet)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CChainsSheet message handlers

BOOL CChainsSheet::OnInitDialog()
{
	BOOL bResult = CPropertySheet::OnInitDialog();

	// TODO: Add your specialized code here
	int	i=0;
	TC_ITEM		item;
	CTabCtrl*	pTabCtrl=GetTabControl();
	CWnd*		pWnd=GetDlgItem(IDOK);
	CRect		rectOK;
	CRect		rect;
    int			nOffset = 6;
	list<CChainPageDlg*>::iterator	it;

	SetRedraw(FALSE);

	/* Set Tab labels ***********************/
	for( it=m_pChainPages.begin(); it!=m_pChainPages.end(); it++ ) {
		item.mask = TCIF_TEXT;
		item.pszText = (char*)(LPCTSTR)(*it)->m_strName;
		pTabCtrl->SetItem(i,&item);
		i++;

		// Force OnInitDialog on all pages
		SetActivePage(i);
	}
	SetActivePage(0);	// Activate first page

	/* Add my own buttons *******************/
	pWnd->GetClientRect(&rectOK);
    GetClientRect(&rect);

	/* Align buttons */
    rect.left += nOffset;
    rect.top = rect.bottom-rectOK.Height()-nOffset;
	rect.right = rect.left+rectOK.Width();
	rect.bottom = rect.top+rectOK.Height();

	/* Create them */
    m_btnAddRule.Create(CurST[ST_BTN_ADDRULE],BS_PUSHBUTTON|WS_VISIBLE|WS_TABSTOP|WS_CHILD,rect,this,IDC_BTN_ADDRULE );
	m_btnAddRule.SetFont( GetFont() );
	rect.left=rect.right+nOffset;
	rect.right=rect.left+rectOK.Width();
	m_btnDelRule.Create(CurST[ST_BTN_DELRULE],BS_PUSHBUTTON|WS_VISIBLE|WS_TABSTOP|WS_CHILD,rect,this,IDC_BTN_DELRULE );
	m_btnDelRule.SetFont( GetFont() );

	SetDlgItemText(IDOK,CurST[ST_A_OK]);
	SetDlgItemText(IDCANCEL,CurST[ST_A_CANCEL]);

	/* Show the nice window */
	SetRedraw(TRUE);
	RedrawWindow();

	return bResult;
}

///////////////////////////////////////////////////////////////////////////////
void CChainsSheet::Init()
{
	g_pChainsSheet=this;
	m_psh.dwFlags |= PSH_NOAPPLYNOW;
	m_pChainPages.clear();
	g_pFAPI->EnumChains(OnEnumChains);
	if( m_pChainPages.size()==0 ) {
		MessageBox(CurST[ST_MSG_NOCHAIN],CurST[ST_TITLE],MB_OK|MB_ICONINFORMATION);
	}
}

///////////////////////////////////////////////////////////////////////////////
BOOL CChainsSheet::OnCommand(WPARAM wParam, LPARAM lParam)
{
	// TODO: Add your specialized code here and/or call the base class
    // crack message parameters
    UINT nID = LOWORD(wParam);
    HWND hWndCtrl = (HWND)lParam;
    int nCode = HIWORD(wParam);

	/* Process Add rule & Del rule */
	if( nCode == BN_CLICKED && nID==IDC_BTN_ADDRULE ) {
		OnAddRuleClicked();
		return(TRUE);
	}

	if( nCode == BN_CLICKED && nID==IDC_BTN_DELRULE ) {
		OnDelRuleClicked();
		return(TRUE);
	}

	if( nCode == BN_CLICKED && nID==IDOK ) {
		OnOK();
	}

	return CPropertySheet::OnCommand(wParam, lParam);
}

///////////////////////////////////////////////////////////////////////////////
void CChainsSheet::OnAddRuleClicked()
{
	CAddRuleDlg dlg;
	CString		strIP;
	CString		strNetMask;
	CFilterRule	Rule;
	int			nIndex=0;
	int			nPageCount;

	dlg.SetRule(&Rule);
	if( dlg.DoModal()==IDOK ) {
		if( Rule.m_bGlobal==false ) {
			// only to current chain
			AddRuleToPage(GetActiveIndex(),&Rule);
		} else {
			// add to all chains
			nPageCount=m_pChainPages.size();
			for( nIndex=0; nIndex<nPageCount; nIndex++ ) {
				AddRuleToPage(nIndex,&Rule);
			}
		}
	}
}

///////////////////////////////////////////////////////////////////////////////
void CChainsSheet::OnDelRuleClicked()
{
	CChainPageDlg*		pPage;
	POSITION			pos;
	int					nItem;
	CConfirmDlg*		pConfirmDlg;
	int					nResponse;
	list<CChainPageDlg*>::iterator	it;
	int					nIndex=0;
	CString				strIP;
	CString				strMask;
	CString				strAction;
	CString				strMessage;

	pPage=(CChainPageDlg*)GetActivePage();
	pos=pPage->m_Rules.GetFirstSelectedItemPosition();

	while( pos!=NULL ) {
		nItem = pPage->m_Rules.GetNextSelectedItem(pos);
		strIP=pPage->m_Rules.GetItemText(nItem,0);
		strMask=pPage->m_Rules.GetItemText(nItem,1);
		strAction=pPage->m_Rules.GetItemText(nItem,2);
		strMessage.Format("%s : %s / %s",(LPCTSTR)strAction,(LPCTSTR)strIP,(LPCTSTR)strMask);

		pConfirmDlg=new CConfirmDlg(strMessage);
		nResponse=pConfirmDlg->DoModal();

		switch( nResponse ) {
			case IDOK:
				// remove from all chains
				for( it=m_pChainPages.begin(); it!=m_pChainPages.end(); it++ ) {
					DelRuleFromPage(nIndex++,strIP,strMask,strAction);
				}
				break;
			case IDNO:
				// remove in current chain
				DelRuleFromPage(GetActiveIndex(),strIP,strMask,strAction);
			case IDCANCEL:
				// cancelled
				break;
		}
	}
}

///////////////////////////////////////////////////////////////////////////////
void CChainsSheet::OnOK()
{
	list<CChainPageDlg*>::iterator	it;

	for( it=m_pChainPages.begin(); it!=m_pChainPages.end(); it++ ) {
		(*it)->SetRules();
	}
}


///////////////////////////////////////////////////////////////////////////////
void CChainsSheet::DelRuleFromPage(int nIndex, CString strIP, CString strMask, CString strAction)
{
	CChainPageDlg*	pPage;
	int		nItem;

	pPage=(CChainPageDlg*)GetPage(nIndex);

	nItem=pPage->GetRuleIndex(strIP,strMask,strAction);
	if( nItem!=-1 ) {
		pPage->m_Rules.DeleteItem(nItem);
	}
}

///////////////////////////////////////////////////////////////////////////////
void CChainsSheet::AddRuleToPage(int nIndex, CFilterRule* pRule)
{
	CChainPageDlg*	pPage;
	int		nItem;

	pPage=(CChainPageDlg*)GetPage(nIndex);

	nItem=pPage->m_Rules.GetItemCount();

	pPage->m_Rules.InsertItem(nItem,(LPCTSTR)pRule->m_strIP);
	pPage->m_Rules.SetItemText(nItem,1,(LPCTSTR)pRule->m_strNetMask);
	pPage->m_Rules.SetItemText(nItem,3,(LPCTSTR)pRule->m_strName);
	pPage->m_Rules.SetItemText(nItem,2,GetStringFromAction(pRule->m_nAction));
}

/////////////////////////////////////////////////////////////////////////////
// C callbacks
void OnEnumChains(FFSS_FILTER_CHAIN Chain,const char Name[],FFSS_FILTER_ACTION Default)
{
	CChainPageDlg*	pChainPage;

	if( g_pChainsSheet==NULL ) {
		return;
	}

	pChainPage=new CChainPageDlg();

	if( pChainPage==NULL ) {
		return;
	}

	pChainPage->Construct(IDD_CHAIN_PAGE);

	/* Remove Help button from all pages and sheet */
	pChainPage->m_psp.dwFlags &= ~PSP_HASHELP;
	pChainPage->m_nChainNumber=Chain;
	pChainPage->m_nAction=Default;
	pChainPage->m_strName=Name;

	// Add this page to list
	g_pChainsSheet->m_pChainPages.push_back(pChainPage);

	/* Add to sheet */
	g_pChainsSheet->AddPage(pChainPage);
}
