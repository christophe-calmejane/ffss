// ffssupdaterDlg.cpp : implementation file
//

#include "stdafx.h"
#include "ffssupdater.h"
#include "ffssupdaterDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


#define FFSS_REG_KEY					HKEY_CURRENT_USER
#define FFSS_REG_SUBKEY					"Software\\FFSS"
#define FFSS_REG_VALUE_USE_PROXY		"Autocheck_Useproxy"
#define FFSS_REG_VALUE_PROXY_HOST		"Autocheck_Proxy_Host"
#define FFSS_REG_VALUE_PROXY_PORT		"Autocheck_Proxy_Port"
#define FFSS_REG_VALUE_PROXY_USER		"Autocheck_Proxy_User"
#define FFSS_REG_VALUE_PROXY_PASSWORD	"Autocheck_Proxy_Password"

/////////////////////////////////////////////////////////////////////////////
// CFfssupdaterDlg dialog

CFfssupdaterDlg::CFfssupdaterDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CFfssupdaterDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CFfssupdaterDlg)
	m_strHostname = _T("proxy");
	m_strPassword = _T("");
	m_lPort = 8080;
	m_strUser = _T("");
	//}}AFX_DATA_INIT
	// Note that LoadIcon does not require a subsequent DestroyIcon in Win32
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
	m_bUseProxy=FALSE;
}

void CFfssupdaterDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CFfssupdaterDlg)
	DDX_Text(pDX, IDC_HOSTNAME, m_strHostname);
	DDX_Text(pDX, IDC_PASSWORD, m_strPassword);
	DDX_Text(pDX, IDC_PORT, m_lPort);
	DDX_Text(pDX, IDC_USER, m_strUser);
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CFfssupdaterDlg, CDialog)
	//{{AFX_MSG_MAP(CFfssupdaterDlg)
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	ON_BN_CLICKED(IDC_DIRECT, OnDirect)
	ON_BN_CLICKED(IDC_PROXY, OnProxy)
	ON_BN_CLICKED(ID_BUTTON, OnButton)
	//}}AFX_MSG_MAP
	ON_WM_DESTROY()
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CFfssupdaterDlg message handlers

BOOL CFfssupdaterDlg::OnInitDialog()
{
	CDialog::OnInitDialog();

	// Set the icon for this dialog.  The framework does this automatically
	//  when the application's main window is not a dialog
	SetIcon(m_hIcon, TRUE);			// Set big icon
	SetIcon(m_hIcon, FALSE);		// Set small icon
	
	// TODO: Add extra initialization here
	bool	bUseProxy=false;
	char	szKeyValue[512];
	DWORD	dwKeyValue=0;
	DWORD	dwStringSize=sizeof(szKeyValue);

	/* Create the key */
	if( m_RegKey.Open(FFSS_REG_KEY,FFSS_REG_SUBKEY)==ERROR_SUCCESS ) {

		if( m_RegKey.QueryValue(dwKeyValue,FFSS_REG_VALUE_USE_PROXY)==ERROR_SUCCESS ) {
			bUseProxy=(dwKeyValue!=0);
		}
		if( m_RegKey.QueryValue(dwKeyValue,FFSS_REG_VALUE_PROXY_PORT)==ERROR_SUCCESS ) {
			m_lPort=dwKeyValue;
		}
		if( m_RegKey.QueryValue(szKeyValue,FFSS_REG_VALUE_PROXY_HOST,&dwStringSize)==ERROR_SUCCESS ) {
			m_strHostname=szKeyValue;
		}
		dwStringSize=sizeof(szKeyValue);
		if( m_RegKey.QueryValue(szKeyValue,FFSS_REG_VALUE_PROXY_USER,&dwStringSize)==ERROR_SUCCESS ) {
			m_strUser=szKeyValue;
		}
		dwStringSize=sizeof(szKeyValue);
		if( m_RegKey.QueryValue(szKeyValue,FFSS_REG_VALUE_PROXY_PASSWORD,&dwStringSize)==ERROR_SUCCESS ) {
			m_strPassword=szKeyValue;
		}
	}

	CheckRadioButton(IDC_DIRECT,IDC_PROXY,bUseProxy==false?IDC_DIRECT:IDC_PROXY);
	UpdatePane();
	UpdateData(FALSE);
	
	return TRUE;  // return TRUE  unless you set the focus to a control
}

/*****************************************************************************/
// If you add a minimize button to your dialog, you will need the code below
//  to draw the icon.  For MFC applications using the document/view model,
//  this is automatically done for you by the framework.

void CFfssupdaterDlg::OnPaint() 
{
	if (IsIconic())
	{
		CPaintDC dc(this); // device context for painting

		SendMessage(WM_ICONERASEBKGND, (WPARAM) dc.GetSafeHdc(), 0);

		// Center icon in client rectangle
		int cxIcon = GetSystemMetrics(SM_CXICON);
		int cyIcon = GetSystemMetrics(SM_CYICON);
		CRect rect;
		GetClientRect(&rect);
		int x = (rect.Width() - cxIcon + 1) / 2;
		int y = (rect.Height() - cyIcon + 1) / 2;

		// Draw the icon
		dc.DrawIcon(x, y, m_hIcon);
	}
	else
	{
		CDialog::OnPaint();
	}
}

/*****************************************************************************/
// The system calls this to obtain the cursor to display while the user drags
//  the minimized window.
HCURSOR CFfssupdaterDlg::OnQueryDragIcon()
{
	return (HCURSOR) m_hIcon;
}

/*****************************************************************************/
void CFfssupdaterDlg::OnDirect() 
{
	UpdatePane();
}

/*****************************************************************************/
void CFfssupdaterDlg::OnProxy() 
{
	UpdatePane();
}

/*****************************************************************************/
void CFfssupdaterDlg::UpdatePane()
{
	CWnd*	pWnd;
	BOOL	bState=TRUE;

	if( IsDlgButtonChecked(IDC_DIRECT) ) {
		bState=FALSE;
	}
	pWnd=GetDlgItem(IDC_HOSTNAME);
	pWnd->EnableWindow(bState);
	pWnd=GetDlgItem(IDC_PORT);
	pWnd->EnableWindow(bState);
	pWnd=GetDlgItem(IDC_USER);
	pWnd->EnableWindow(bState);
	pWnd=GetDlgItem(IDC_PASSWORD);
	pWnd->EnableWindow(bState);
	pWnd=GetDlgItem(IDC_LHOSTNAME);
	pWnd->EnableWindow(bState);
	pWnd=GetDlgItem(IDC_LPORT);
	pWnd->EnableWindow(bState);
	pWnd=GetDlgItem(IDC_LUSER);
	pWnd->EnableWindow(bState);
	pWnd=GetDlgItem(IDC_LPASSWORD);
	pWnd->EnableWindow(bState);
}

/*****************************************************************************/
void CFfssupdaterDlg::OnButton() 
{
	// TODO: Add your control notification handler code here
	DWORD	dwKeyValue=0;
	
	/* Error in input fields */
	if( UpdateData(TRUE)==0 ) {
		return;
	}

	/* Try to create it, if it doesn't exist yet */
	m_RegKey.Create(FFSS_REG_KEY,FFSS_REG_SUBKEY);

	if( IsDlgButtonChecked(IDC_DIRECT) ) {
		/* No proxy, but keep existing proxy config (don't remove values) */
		m_RegKey.SetValue(dwKeyValue,FFSS_REG_VALUE_USE_PROXY);
	} else {
		dwKeyValue=1;
		m_RegKey.SetValue(dwKeyValue,FFSS_REG_VALUE_USE_PROXY);
		m_RegKey.SetValue(m_strHostname,FFSS_REG_VALUE_PROXY_HOST);
		dwKeyValue=m_lPort;
		m_RegKey.SetValue(dwKeyValue,FFSS_REG_VALUE_PROXY_PORT);
		m_RegKey.SetValue(m_strUser,FFSS_REG_VALUE_PROXY_USER);
		m_RegKey.SetValue(m_strPassword,FFSS_REG_VALUE_PROXY_PASSWORD);
	}

	EndDialog(IDOK);
}

void CFfssupdaterDlg::OnDestroy()
{
	CDialog::OnDestroy();

	// TODO: Add your message handler code here
	m_RegKey.Close();
}
