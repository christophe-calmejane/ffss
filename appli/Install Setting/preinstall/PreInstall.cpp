// PreInstall.cpp : Defines the class behaviors for the application.
//

#include "stdafx.h"
#include "PreInstall.h"
#include "PreInstallDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

static WCHAR* convertToUnicode(const char* text)
{
	WCHAR* utf16_text = NULL;
	int size_text = MultiByteToWideChar(CP_UTF8, 0, text, -1, utf16_text, 0);
	utf16_text = (WCHAR*)calloc((size_text + 1), sizeof(WCHAR));
	MultiByteToWideChar(CP_UTF8, 0, text, -1, utf16_text, size_text);

	return utf16_text;
}


/////////////////////////////////////////////////////////////////////////////
// CPreInstallApp

BEGIN_MESSAGE_MAP(CPreInstallApp, CWinApp)
	//{{AFX_MSG_MAP(CPreInstallApp)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG
	ON_COMMAND(ID_HELP, CWinApp::OnHelp)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CPreInstallApp construction

CPreInstallApp::CPreInstallApp()
{
	// TODO: add construction code here,
	// Place all significant initialization in InitInstance
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CPreInstallApp object

CPreInstallApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CPreInstallApp initialization

BOOL CPreInstallApp::InitInstance()
{
	if (!AfxSocketInit())
	{
		AfxMessageBox(IDP_SOCKETS_INIT_FAILED);
		return FALSE;
	}

	// Standard initialization
	// If you are not using these features and wish to reduce the size
	//  of your final executable, you should remove from the following
	//  the specific initialization routines you do not need.

#ifdef _AFXDLL
	Enable3dControls();			// Call this when using MFC in a shared DLL
#else
	Enable3dControlsStatic();	// Call this when linking to MFC statically
#endif

	CPreInstallDlg dlg;
	m_pMainWnd = &dlg;

	/* Install string table */
	InstallStringTables();

	dlg.DoModal();

	// Since the dialog has been closed, return FALSE so that we exit the
	//  application, rather than start the application's message pump.
	return FALSE;
}

///////////////////////////////////////////////////////////////////////////////
void CPreInstallApp::InstallStringTables()
{
	CRegKey		RegKey;
	char		buffer[16];
	DWORD		nBufSize;

#ifdef _DEBUG
	//MT_ST_INSTANCE->AddTable("English","en.txt");
	//MT_ST_INSTANCE->AddTable("French","fr.txt");
#else
	MT_ST_INSTANCE->AddTable("English",__argv[0]);
	MT_ST_INSTANCE->AddTable("French",__argv[0]);
#endif /* !_DEBUG */

	if( __argc!=2 ) {
		// Check for the registry
		if(RegKey.Open(FFSS_REG_KEY, convertToUnicode(FFSS_REG_SUBKEY)) != ERROR_SUCCESS)
		{
			//MT_ST_INSTANCE->UseTable(LANGUAGE_ENGLISH);
		} else {
			nBufSize=sizeof(buffer);
			if(RegKey.QueryValue(convertToUnicode(buffer), convertToUnicode(FFSS_REG_FAVORITE_LANGUAGE), &nBufSize) != ERROR_SUCCESS)
			{
				//MT_ST_INSTANCE->UseTable(LANGUAGE_ENGLISH);
			} else {
				if( stricmp(buffer,"en")==0 ) {
					//MT_ST_INSTANCE->UseTable(LANGUAGE_ENGLISH);
				} else {
					if( stricmp(buffer,"fr")==0 ) {
						//MT_ST_INSTANCE->UseTable(LANGUAGE_FRENCH);
					}
				}				
			}
			RegKey.Close();
		}
	} else {
		if( stricmp(__argv[1],"en")==0 ) {
			//MT_ST_INSTANCE->UseTable(LANGUAGE_ENGLISH);
		} else {
			if( stricmp(__argv[1],"fr")==0 ) {
				//MT_ST_INSTANCE->UseTable(LANGUAGE_FRENCH);
			}
		}
	}
}
