// ffssupdaterDlg.cpp : implementation file
//

#include "stdafx.h"
#include "ffssupdater.h"
#include "ffssupdaterDlg.h"
#include <wininet.h>

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

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
	ON_BN_CLICKED(IDC_NOCONN, OnNoConn)
	ON_BN_CLICKED(IDC_TEST_SETTINGS, OnTestSettings)
	ON_BN_CLICKED(IDC_IE, OnIe)
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
	DWORD	dwConnectionType=1;
	char	szKeyValue[512];
	char	Buffer[1024];
	DWORD	dwKeyValue=0;
	DWORD	dwStringSize=sizeof(szKeyValue);
	LPINTERNET_PROXY_INFO lpInternetProxy;
	DWORD	dwBufferLength=sizeof(Buffer);

	/* Localization */
	Localize();

	/* Get global proxy configuration */
	if( InternetQueryOption(NULL,INTERNET_OPTION_PROXY,Buffer,&dwBufferLength)==TRUE ) {
		lpInternetProxy=(LPINTERNET_PROXY_INFO)Buffer;

		/* Not in direct connection  */
		if( lpInternetProxy->dwAccessType!=INTERNET_OPEN_TYPE_DIRECT ) {
			dwConnectionType=2;

			m_strHostname=strtok((char*)lpInternetProxy->lpszProxy,":");
			m_lPort=atol(strtok(NULL,""));

			dwBufferLength=sizeof(Buffer);
			if( InternetQueryOption(NULL,INTERNET_OPTION_PROXY_USERNAME ,Buffer,&dwBufferLength)==TRUE ) {
				m_strUser=Buffer;
			}
			dwBufferLength=sizeof(Buffer);
			if( InternetQueryOption(NULL,INTERNET_OPTION_PROXY_PASSWORD ,Buffer,&dwBufferLength)==TRUE ) {
				m_strPassword=Buffer;
			}
		}
	}

	/* Create the key and override default configuration */
	if( m_RegKey.Open(FFSS_REG_KEY,FFSS_REG_SUBKEY)==ERROR_SUCCESS ) {

		if( m_RegKey.QueryValue(dwKeyValue,FFSS_REG_VALUE_USE_PROXY)==ERROR_SUCCESS ) {
			dwConnectionType=dwKeyValue;
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

	/* Check the box */
	switch( dwConnectionType ) {
		case 0:
			CheckRadioButton(IDC_DIRECT,IDC_IE,IDC_NOCONN);
			break;
		case 1:
			CheckRadioButton(IDC_DIRECT,IDC_IE,IDC_DIRECT);
			break;
		case 2:
			CheckRadioButton(IDC_DIRECT,IDC_IE,IDC_PROXY);
			break;
		case 3:
			CheckRadioButton(IDC_DIRECT,IDC_IE,IDC_IE);
			break;
	}

	/* Enable or disable proxy pane */
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
void CFfssupdaterDlg::OnNoConn() 
{
	UpdatePane();
}

/*****************************************************************************/
void CFfssupdaterDlg::OnIe() 
{
	UpdatePane();
}

/*****************************************************************************/
void CFfssupdaterDlg::UpdatePane()
{
	CWnd*	pWnd;
	BOOL	bState=FALSE;

	if( IsDlgButtonChecked(IDC_PROXY) ) {
		bState=TRUE;
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

	pWnd=GetDlgItem(IDC_TEST_SETTINGS);
	if( IsDlgButtonChecked(IDC_NOCONN) || IsDlgButtonChecked(IDC_IE) ) {
		pWnd->EnableWindow(BST_UNCHECKED);
	} else {
		pWnd->EnableWindow(BST_CHECKED);
	}
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

	if( IsDlgButtonChecked(IDC_NOCONN) ) {
		/* No connection */
		m_RegKey.SetValue(dwKeyValue,FFSS_REG_VALUE_USE_PROXY);
	} else {
		if( IsDlgButtonChecked(IDC_DIRECT) ) {
			/* No proxy, but keep existing proxy config (don't remove values) */
			dwKeyValue=1;
			m_RegKey.SetValue(dwKeyValue,FFSS_REG_VALUE_USE_PROXY);
		} else {
			if( IsDlgButtonChecked(IDC_IE) ) {
				dwKeyValue=3;
				m_RegKey.SetValue(dwKeyValue,FFSS_REG_VALUE_USE_PROXY);
			} else {
				dwKeyValue=2;
				m_RegKey.SetValue(dwKeyValue,FFSS_REG_VALUE_USE_PROXY);
				m_RegKey.SetValue(m_strHostname,FFSS_REG_VALUE_PROXY_HOST);
				dwKeyValue=m_lPort;
				m_RegKey.SetValue(dwKeyValue,FFSS_REG_VALUE_PROXY_PORT);
				m_RegKey.SetValue(m_strUser,FFSS_REG_VALUE_PROXY_USER);
				m_RegKey.SetValue(m_strPassword,FFSS_REG_VALUE_PROXY_PASSWORD);
			}
		}
	}

	EndDialog(IDOK);
}

/*****************************************************************************/
void CFfssupdaterDlg::OnDestroy()
{
	CDialog::OnDestroy();

	// TODO: Add your message handler code here
	m_RegKey.Close();
}

/*****************************************************************************/
void CFfssupdaterDlg::OnTestSettings() 
{
	// TODO: Add your control notification handler code here
	HINTERNET	hInet;
	HINTERNET	hConnection;
	CWnd*		pWnd;
	CString		strMsg;
	CString		strHostname;

	/* Check input fields */
	if( UpdateData(TRUE)==0 ) {
		return;
	}

#pragma message("TODO: Ajouter une fenetre d'état pendant le test")

	pWnd=GetDlgItem(IDC_TEST_SETTINGS);
	pWnd->EnableWindow(FALSE);
	pWnd=GetDlgItem(ID_BUTTON);
	pWnd->EnableWindow(FALSE);

	if( IsDlgButtonChecked(IDC_DIRECT) ) {
		hInet=InternetOpen(FFSS_INET_AGENT,INTERNET_OPEN_TYPE_DIRECT,NULL,
			NULL,0);
	} else {
		strHostname.Format("%s:%ld",m_strHostname,m_lPort);

		hInet=InternetOpen(FFSS_INET_AGENT,INTERNET_OPEN_TYPE_PROXY,
			(LPCTSTR)strHostname,NULL,0);

		if( hInet!=NULL ) {
			if( m_strUser.IsEmpty()!=FALSE ) {
				InternetSetOption(hInet,INTERNET_OPTION_PROXY_USERNAME,
					(void*)(LPCTSTR)m_strUser,m_strUser.GetLength());
			}
			if( m_strPassword.IsEmpty()!=FALSE ) {
				InternetSetOption(hInet,INTERNET_OPTION_PROXY_PASSWORD,
					(void*)(LPCTSTR)m_strPassword,m_strPassword.GetLength());
			}
		}
	}
	
	if( hInet!=NULL ) {
		hConnection=InternetOpenUrl(hInet,FFSS_TEST_PAGE,NULL,0,INTERNET_FLAG_DONT_CACHE,0);

		/* Connection error */
		if( hConnection!=NULL ) {
			MessageBox(MT_ST_LOCAL(ST_CORRECT),MT_ST_LOCAL(ST_DLG_TITLE),
				MB_OK|MB_ICONINFORMATION);
		} else {
			ErrorMessage(strMsg,GetLastError());
			MessageBox((LPCTSTR)strMsg,MT_ST_LOCAL(ST_DLG_TITLE),
				MB_OK|MB_ICONWARNING);
		}
		InternetCloseHandle(hInet);
	}

	pWnd=GetDlgItem(IDC_TEST_SETTINGS);
	pWnd->EnableWindow(TRUE);
	pWnd=GetDlgItem(ID_BUTTON);
	pWnd->EnableWindow(TRUE);
}

/*****************************************************************************/
void CFfssupdaterDlg::ErrorMessage(CString &strDest, DWORD dwError)
{
	switch( dwError ) {
		case ERROR_INTERNET_CANNOT_CONNECT:
			strDest=MT_ST_LOCAL(ST_CANT_CONN);
			break;
		case ERROR_INTERNET_NAME_NOT_RESOLVED:
			strDest=MT_ST_LOCAL(ST_CANT_RESOLVE);
			break;
		default :
			strDest.Format(MT_ST_LOCAL(ST_INET_ERROR),dwError );
			break;
	}
}

/*****************************************************************************/
bool CFfssupdaterDlg::Localize()
{
	SetWindowText(MT_ST_LOCAL(ST_WIN_TITLE));
	SetDlgItemText(IDC_NOCONN,MT_ST_LOCAL(ST_NO_CONN));
	SetDlgItemText(IDC_DIRECT,MT_ST_LOCAL(ST_DIRECT_CONN));
	SetDlgItemText(IDC_PROXY,MT_ST_LOCAL(ST_PROXY_CONN));
	SetDlgItemText(IDC_IE,MT_ST_LOCAL(ST_IE_CONN));
	SetDlgItemText(IDC_LHOSTNAME,MT_ST_LOCAL(ST_LHOSTNAME));
	SetDlgItemText(IDC_LUSER,MT_ST_LOCAL(ST_LUSER));
	SetDlgItemText(IDC_LPASSWORD,MT_ST_LOCAL(ST_LPASSWORD));
	SetDlgItemText(IDC_LPORT,MT_ST_LOCAL(ST_LPORT));
	SetDlgItemText(IDC_TEST_SETTINGS,MT_ST_LOCAL(ST_TEST_SETTINGS));
	SetDlgItemText(IDC_LINFO,MT_ST_LOCAL(ST_LINFO_BAND));
	return(true);
}
