// PreInstallDlg.cpp : implementation file
//

#include "stdafx.h"
#include <fstream.h>

#include "PreInstall.h"
#include "PreInstallDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define SHARE_LIST_SEP "|"
#define SHARE_LIST_OLD_SEP "#"

#define PLUGINS_FILE	"plugins.lst"
#define PLUGINS_PROMPT	"\n    Select a plugin in the list"

#pragma pack(1)
struct share_info_50 {
  char            shi50_netname[LM20_NNLEN+1];
  unsigned char   shi50_type;
  unsigned short  shi50_flags;
  char FAR *      shi50_remark;
  char FAR *      shi50_path;
  char            shi50_rw_password[SHPWLEN+1];
  char            shi50_ro_password[SHPWLEN+1];
};

/////////////////////////////////////////////////////////////////////////////
// CPreInstallDlg dialog

CPreInstallDlg::CPreInstallDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CPreInstallDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CPreInstallDlg)
	m_strComment = _T("");
	m_strMaster = _T("");
	m_bImportSamba = FALSE;
	m_strServer = _T("");
	//}}AFX_DATA_INIT
	// Note that LoadIcon does not require a subsequent DestroyIcon in Win32
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
	m_hinstLib=NULL;
	m_bIsWinNT=false;
	m_fnNetShareEnum=NULL;
	m_fnNetShareEnumNT=NULL;
}

void CPreInstallDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CPreInstallDlg)
	DDX_Control(pDX, IDC_PLUGINS, m_Plugins);
	DDX_Text(pDX, IDC_COMMENT, m_strComment);
	DDX_Text(pDX, IDC_MASTER, m_strMaster);
	DDX_Check(pDX, IDC_SAMBA, m_bImportSamba);
	DDX_Text(pDX, IDC_SERVER, m_strServer);
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CPreInstallDlg, CDialog)
	//{{AFX_MSG_MAP(CPreInstallDlg)
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	ON_BN_CLICKED(ID_BUTTON, OnOk)
	ON_NOTIFY(NM_CLICK, IDC_PLUGINS, OnClickPlugins)
	//}}AFX_MSG_MAP
	ON_WM_DESTROY()
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CPreInstallDlg message handlers

BOOL CPreInstallDlg::OnInitDialog()
{
	CDialog::OnInitDialog();

	// Set the icon for this dialog.  The framework does this automatically
	//  when the application's main window is not a dialog
	SetIcon(m_hIcon, TRUE);			// Set big icon
	SetIcon(m_hIcon, FALSE);		// Set small icon
	
	// TODO: Add extra initialization here
	char			szLocalHostName[512],szName[512],szComment[512],szMaster[512];
	struct hostent*	pHostEnt;
	DWORD			dwStringSize;
	OSVERSIONINFO	Os;

	/* Import function to enum shares */
	Os.dwOSVersionInfoSize=sizeof(OSVERSIONINFO);
	GetVersionEx(&Os);
	if( Os.dwPlatformId != VER_PLATFORM_WIN32_NT ) {
		/* Win9x*/
		m_bIsWinNT=false;
		m_hinstLib = LoadLibrary("SvrApi.dll");
		if( m_hinstLib!=NULL ) {
			m_fnNetShareEnum=(procNetShareEnum)GetProcAddress(m_hinstLib, "NetShareEnum");
		}
	} else {
		/* WinNT*/
		m_bIsWinNT=true;
		m_hinstLib = LoadLibrary("Netapi32.dll");
		if( m_hinstLib!=NULL ) {
			m_fnNetShareEnumNT=(procNetShareEnumNT)GetProcAddress(m_hinstLib, "NetShareEnum");
		}
	}

	m_bImportSamba=TRUE;

	/* Pas de bras, pas de chocolat */
	if( m_hinstLib==NULL
		|| ( m_bIsWinNT==true && m_fnNetShareEnumNT==NULL )
		|| ( m_bIsWinNT==false && m_fnNetShareEnum==NULL ) ) {
		CWnd*	pCwnd=(CWnd*)GetDlgItem(IDC_SAMBA);
		pCwnd->EnableWindow(FALSE);
		m_bImportSamba=FALSE;
	} 

	/* Get local hostname, and set it as default server name */
	gethostname(szLocalHostName,sizeof(szLocalHostName));
	m_strServer=szLocalHostName;

	/* Get ffss master hostname */
	pHostEnt=gethostbyname("ffss");
	if( pHostEnt!=NULL ) {
		m_strMaster=pHostEnt->h_name;
	}

	/* Get values from a previous configuration in registry */
	if( m_RegKey.Open(HKEY_CURRENT_USER,"Software\\FFSS\\Server")==ERROR_SUCCESS ) {
		dwStringSize=sizeof(szName);
		if( m_RegKey.QueryValue(szName,"Global_Name",&dwStringSize)==ERROR_SUCCESS ) {
			m_strServer=szName;
		}
		dwStringSize=sizeof(szComment);
		if( m_RegKey.QueryValue(szComment,"Global_Comment",&dwStringSize)==ERROR_SUCCESS ) {
			m_strComment=szComment;
		}
		dwStringSize=sizeof(szMaster);
		if( m_RegKey.QueryValue(szMaster,"Global_Master",&dwStringSize)==ERROR_SUCCESS ) {
			m_strMaster=szMaster;
		}

		m_RegKey.Close();
	} 

	/*************************************************************************/ 
	SetDlgItemText(IDC_PLUGINDESC,PLUGINS_PROMPT);

	m_Plugins.InsertColumn(0,"Name",LVCFMT_LEFT,160);
	m_Plugins.SetExtendedStyle(LVS_EX_CHECKBOXES|LVS_EX_FULLROWSELECT);
	GetPlugins();

	UpdateData(FALSE);

	return TRUE;  // return TRUE  unless you set the focus to a control
}

// If you add a minimize button to your dialog, you will need the code below
//  to draw the icon.  For MFC applications using the document/view model,
//  this is automatically done for you by the framework.

void CPreInstallDlg::OnPaint() 
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

// The system calls this to obtain the cursor to display while the user drags
//  the minimized window.
HCURSOR CPreInstallDlg::OnQueryDragIcon()
{
	return (HCURSOR) m_hIcon;
}

/*****************************************************************************/
void CPreInstallDlg::OnOk() 
{
	// TODO: Add your control notification handler code here
	CString		strReport;
	long		lSharesAdded;
	int			nItem;
	PluginInfo*	pPI;

	UpdateData(TRUE);

	/*************************************************************************/
	/* Save registry keys                                                    */
	m_RegKey.Create(HKEY_CURRENT_USER,"Software\\FFSS\\Server");

	/* Save server name to registry */
	if( m_strServer.IsEmpty() ) {
		AfxMessageBox("You need to set a name for this server");
		return;
	} else {
		m_RegKey.SetValue((LPCTSTR)m_strServer,"Global_Name");
	}

	/* Save server comment to registry */
	if( m_strComment.IsEmpty()==FALSE ) {
		m_RegKey.SetValue((LPCTSTR)m_strComment,"Global_Comment");
	}

	/* Save master name to registry */
	if( m_strMaster.IsEmpty()==FALSE ) {
		m_RegKey.SetValue((LPCTSTR)m_strMaster,"Global_Master");
	}

	/*************************************************************************/
	/* Import samba shares                                                   */
	if( m_bImportSamba==FALSE ) {
		ConvertFromOldFormat();	/* Convert # => |in share list */
	} else {
		if( m_bIsWinNT==true ) {
			lSharesAdded=ImportWinNTShares();
		} else {
			lSharesAdded=ImportWin9xShares();
		}
		strReport.Format("%d shares have been added",lSharesAdded);
		MessageBox((LPCTSTR)strReport,"Shares added",MB_OK|MB_ICONINFORMATION);
	}
	
	m_RegKey.Close();

	/*************************************************************************/
	/* Do some stuff with plugins                                            */
	m_RegKey.Create(HKEY_CURRENT_USER,"Software\\FFSS\\Server\\Plugins");
	for(nItem=0; nItem<m_Plugins.GetItemCount(); nItem++) {
		pPI=(PluginInfo*)m_Plugins.GetItemData(nItem);
		if( m_Plugins.GetCheck(nItem)==FALSE ) {
			m_RegKey.DeleteValue(pPI->szName);
		} else {
			m_RegKey.SetValue(pPI->szPathToDLL,pPI->szName);
		}
	}
	
	//m_RegKey.Close();

	if( m_hinstLib!=NULL ) {
		FreeLibrary(m_hinstLib); 
	}

	EndDialog(IDOK);	
}

/*****************************************************************************/
#define MAX_ENTRIES 40
long CPreInstallDlg::ImportWin9xShares()
{
	short nLevel = 50;
	struct share_info_50 *pBuffer, *p;
	short cbBuffer;
	unsigned short nEntriesRead = 0;
	unsigned short nTotalEntries = 0;
	unsigned short nTotalCount = 0;
	short nShareProcessed=0;
	int i;
	NET_API_STATUS nStatus;
	char	szShareList[4096];

	strcpy(szShareList,"");
	i=sizeof(struct share_info_50);
	cbBuffer = MAX_ENTRIES * sizeof(struct share_info_50);
	pBuffer = (struct share_info_50*) malloc(cbBuffer);

	/* We don't get a buffer :-/ */
	if( pBuffer==NULL ) {
		return(0);
	}

	do {
		nStatus=m_fnNetShareEnum(NULL,nLevel,(char FAR *)pBuffer,cbBuffer,
			&nEntriesRead,&nTotalEntries);
		if( nStatus==ERROR_SUCCESS || nStatus==ERROR_MORE_DATA ) {
			p=pBuffer;
			for(i=1; i<=nEntriesRead; i++ ) {
				if(p->shi50_type==STYPE_DISKTREE) {
					if( strlen(p->shi50_ro_password)==0 && strlen(p->shi50_rw_password)==0 ) {
						AddShare(p->shi50_netname,p->shi50_path,p->shi50_remark,p->shi50_rw_password,0);
						if( strlen(szShareList)>0 ) {
							strcat(szShareList,SHARE_LIST_SEP);
						}
						strcat(szShareList,p->shi50_netname);
						nShareProcessed++;
					}
				}
				p++;
			}
		}
	} while( nStatus==ERROR_MORE_DATA );
	m_RegKey.SetValue(szShareList,"ShareNames");

	free(pBuffer);
	return(nShareProcessed);
}

/*****************************************************************************/
long CPreInstallDlg::ImportWinNTShares()
{
	NET_API_STATUS res;
	PSHARE_INFO_2 pBuffer, p;
	DWORD	dwShareProcessed=0;
	DWORD	dwSecurityLevel=2;
	DWORD	dwEntriesRead=0;
	DWORD	dwTotalEntries=0;
	DWORD	dwResumeHandle=0, i;
	char	szShareName[20];
	char	szSharePasswd[20];
	char	szSharePath[MAX_PATH];
	char	szShareComment[50];
	DWORD	dwMaxUsers;
	char	szShareList[4096];

	strcpy(szShareList,"");

	do {
		res = m_fnNetShareEnumNT(NULL, dwSecurityLevel, (LPBYTE *) &pBuffer, 
			MAX_PREFERRED_LENGTH, &dwEntriesRead, &dwTotalEntries, &dwResumeHandle);
		if(res == ERROR_SUCCESS || res == ERROR_MORE_DATA) {
			/* Import to ffss*/
			p=pBuffer;
			for( i=1 ;i<=dwEntriesRead; i++ ) {
				/* Get only file Shares */
				if( p->shi2_type==STYPE_DISKTREE ) {
					/* Convert from Unicode to MultiByte string */
					wcstombs( szShareName, (const wchar_t *)p->shi2_netname, sizeof(szShareName) );
					wcstombs( szSharePath, (const wchar_t *)p->shi2_path, sizeof(szSharePath) );
					wcstombs( szShareComment, (const wchar_t *)p->shi2_remark, sizeof(szShareComment) );
					dwMaxUsers=p->shi2_max_uses;
					if( dwMaxUsers==-1 ) {
						dwMaxUsers=0;
					}

					/* Pour le moment, on importe que les partages sans mot de passe */
					if( p->shi2_passwd!=NULL ) {
						wcstombs( szSharePasswd, (const wchar_t *)p->shi2_passwd, sizeof(szSharePasswd) );
					} else {
						AddShare(szShareName,szSharePath,szShareComment,szSharePasswd,dwMaxUsers);
						if( strlen(szShareList)>0 ) {
							strcat(szShareList,SHARE_LIST_SEP);
						}
						strcat(szShareList,szShareName);
						dwShareProcessed++;
					}
				}
				p++;
			}
		}
	} while(res==ERROR_MORE_DATA);
	m_RegKey.SetValue(szShareList,"ShareNames");
	return(dwShareProcessed);
}

/*****************************************************************************/
void CPreInstallDlg::AddShare(const char *szShareName, const char *szSharePath, 
							  const char *szShareComment,const char *szSharePasswd,
							  DWORD dwMaxUsers)
{
	char	szKeyValueName[512];
	DWORD	dwDefault=0;

	_snprintf(szKeyValueName,sizeof(szKeyValueName),"%s_Path",szShareName);
	m_RegKey.SetValue(szSharePath,szKeyValueName);
	_snprintf(szKeyValueName,sizeof(szKeyValueName),"%s_Comment",szShareName);
	m_RegKey.SetValue(szShareComment,szKeyValueName);
	_snprintf(szKeyValueName,sizeof(szKeyValueName),"%s_Users",szShareName);
	m_RegKey.SetValue("",szKeyValueName);
	_snprintf(szKeyValueName,sizeof(szKeyValueName),"%s_MaxConnections",szShareName);
	m_RegKey.SetValue(dwMaxUsers,szKeyValueName);
	_snprintf(szKeyValueName,sizeof(szKeyValueName),"%s_Private",szShareName);
	m_RegKey.SetValue(dwDefault,szKeyValueName);
	_snprintf(szKeyValueName,sizeof(szKeyValueName),"%s_Writeable",szShareName);
	m_RegKey.SetValue(dwDefault,szKeyValueName);
}

/*****************************************************************************/
void CPreInstallDlg::ConvertFromOldFormat()
{
	char	szShareList[4096];
	char	szConvertedShareList[4096];
	char	szKeyValueName[512];
	unsigned long	lStringLength;
	DWORD	dwValue;
	char*	p;

	lStringLength=sizeof(szShareList);
	m_RegKey.QueryValue(szShareList,"ShareNames",&lStringLength);
	strcpy(szConvertedShareList,"");

	if( lStringLength>0 ) {
		p=strtok(szShareList,SHARE_LIST_OLD_SEP);
		
		/* Only one share in list, no # */
		if(  p==NULL && strlen(szConvertedShareList)==0 ) {
			strcpy(szConvertedShareList,szShareList);
		}

		/* Many shares */
		while( p!=NULL ) {
			/* Check if share files exist */
			_snprintf(szKeyValueName,sizeof(szKeyValueName),"%s_Writeable",p);
			if( m_RegKey.QueryValue(dwValue,szKeyValueName)!=ERROR_SUCCESS ) {
				/* New format, do nothing  */
				return;
			} else {
				if( strlen(szConvertedShareList)>0 ) {
					strcat(szConvertedShareList,"|");
				}
				strcat(szConvertedShareList,p);
			}
			p=strtok(NULL,SHARE_LIST_OLD_SEP);
		}
		m_RegKey.SetValue(szConvertedShareList,"ShareNames");
	}
}

/*****************************************************************************/
void CPreInstallDlg::OnDestroy()
{
	int			i;
	PluginInfo*	pPI;

	for(i=0;i<m_Plugins.GetItemCount();i++) {
		pPI=(PluginInfo*)m_Plugins.GetItemData(i);
		delete(pPI);
	}

	m_RegKey.Close();
	
	CDialog::OnDestroy();
}

/*****************************************************************************/
bool CPreInstallDlg::GetPlugins()
{
	WIN32_FIND_DATA	wfd;
	HANDLE			hFind;
	char			szPluginsWildCards[MAX_PATH+1];
	char			szPluginsDirectory[MAX_PATH+1];
	char			szPluginFilename[MAX_PATH+1];
	DWORD			dwStringSize;
	CRegKey			RegKey;
	int				nItem=0;
	PluginInfo*		pPI;
	bool			bKeyExists=false;

	strcpy(szPluginsDirectory,"");

	/* Get server directory */
	if( RegKey.Open(HKEY_CURRENT_USER,"Software\\FFSS\\Server")==ERROR_SUCCESS ) {
		dwStringSize=sizeof(szPluginsDirectory);
		if( RegKey.QueryValue(szPluginsDirectory,"ServerDirectory",&dwStringSize)!=ERROR_SUCCESS ) {
			strcpy(szPluginsDirectory,"");
		}
		RegKey.Close();
	} 

	if( strlen(szPluginsDirectory)==0 ) {
		// error, string is empty
		return(false);
	}

	strcat(szPluginsDirectory,"\\Plugins\\");
	strcpy(szPluginsWildCards,szPluginsDirectory);
	strcat(szPluginsWildCards,"*.dll");

	if( RegKey.Open(HKEY_CURRENT_USER,"Software\\FFSS\\Server\\Plugins")==ERROR_SUCCESS ) {
		bKeyExists=true;
	}

	hFind=FindFirstFile(szPluginsWildCards,&wfd);
	if (hFind == INVALID_HANDLE_VALUE) {
		return(false);
	} else {
		// Process first file
		strcpy(szPluginFilename,szPluginsDirectory);
		strcat(szPluginFilename,wfd.cFileName);
		pPI=GetPluginInfo(szPluginFilename);
		if( pPI!=NULL ) {
			m_Plugins.InsertItem(nItem,pPI->szName);
			m_Plugins.SetItemData(nItem, (DWORD)pPI );
			if( bKeyExists==true ) {
				m_Plugins.SetCheck(nItem, RegValueExists(RegKey, pPI->szName) );
			}
		}

		while( FindNextFile(hFind, &wfd)==TRUE ) {
			nItem++;
			strcpy(szPluginFilename,szPluginsDirectory);
			strcat(szPluginFilename,wfd.cFileName);
			pPI=GetPluginInfo(szPluginFilename);
			if( pPI!=NULL ) {
				m_Plugins.InsertItem(nItem,pPI->szName);
				m_Plugins.SetItemData(nItem, (DWORD)pPI );
				if( bKeyExists==true ) {
					m_Plugins.SetCheck(nItem, RegValueExists(RegKey, pPI->szName) );
				}
			}
		}
		FindClose(hFind);

		if( bKeyExists==true ) {
			RegKey.Close();
		}
	}
	
	return(true);
}

/*****************************************************************************/
bool CPreInstallDlg::RegValueExists(CRegKey& RegKey,const char* szValueName)
{
	char	c;
	DWORD	dwBufferSize=1;
	LONG	lRetCode;

	lRetCode=RegKey.QueryValue(&c,szValueName,&dwBufferSize);

	return( (lRetCode==ERROR_SUCCESS) || (lRetCode==ERROR_MORE_DATA) );
}

/*****************************************************************************/
void CPreInstallDlg::OnClickPlugins(NMHDR* pNMHDR, LRESULT* pResult) 
{
	POSITION	pos = m_Plugins.GetFirstSelectedItemPosition();
	int			nItem;
	PluginInfo*	pPI;

	if( pos==NULL ) {
		SetDlgItemText(IDC_PLUGINDESC,PLUGINS_PROMPT);
	} else {
		nItem = m_Plugins.GetNextSelectedItem(pos);
		pPI=(PluginInfo*)m_Plugins.GetItemData(nItem);
		SetDlgItemText(IDC_PLUGINDESC,pPI->szDescription);
	}

	*pResult = 0;
}

/*****************************************************************************/
PluginInfo* CPreInstallDlg::GetPluginInfo(const char* szPathToDLL)
{
	HINSTANCE				hinstLib;
	procPluginQueryInfos	fnPluginQueryInfos=NULL;
	PluginInfo*				pPI=NULL;
	FSP_PInfos				pInfos;


	hinstLib = LoadLibrary(szPathToDLL);
    if( hinstLib!=NULL ) {
		fnPluginQueryInfos=(procPluginQueryInfos)GetProcAddress(hinstLib, "Plugin_QueryInfos");
		if( fnPluginQueryInfos!=NULL ) {
			pPI=new PluginInfo();
			if( pPI!=NULL ) {
				pInfos=fnPluginQueryInfos();
				if( pInfos!=NULL ) {
					pPI->szName=strdup(pInfos->Name);
					pPI->szCopyright=strdup(pInfos->Copyright);
					pPI->szDescription=strdup(pInfos->Description);
					pPI->szVersion=strdup(pInfos->Version);
					pPI->szPathToDLL=strdup(szPathToDLL);
				}
			}
		}
	}
	FreeLibrary(hinstLib); 

	return(pPI);
}

/*****************************************************************************/
/*****************************************************************************/
PluginInfo::~PluginInfo()
{
	if( szName!=NULL ) {
		free(szName);
	}
	if( szCopyright!=NULL ) {
		free(szCopyright);
	}
	if( szDescription!=NULL ) {
		free(szDescription);
	}
	if( szVersion!=NULL ) {
		free(szVersion);
	}
	if( szPathToDLL!=NULL ) {
		free(szPathToDLL);
	}
}
